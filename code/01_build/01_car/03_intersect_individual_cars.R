# CAR build, stage 03: pairwise CAR-vs-CAR intersections (overlap/conflict areas).
#
# Faithful, reproducible port of the intersection loops of the legacy producer
# `legacy_repo/dropbox_producers/create_muni_year_intersections/C_intersect_individual_cars.R`
# (the `fp$INTERSECT_CARS_ROBUST` + `fp$INTERSECT_CARS_S2` loops and their shared
# `compute_intersections()` workhorse).
#
# For each municipality, self-intersect its CARs to find overlapping property pairs,
# measure each overlap area, and attach both CARs' attributes and registration dates.
# Robust-cleaned shapes are used primarily; the s2-cleaned shapes are a fallback for
# municipalities that have no robust output (the mirror of stage 02's s2/light choice).
#
# Outputs (per-municipality pair-overlap tables, consumed by the stage-03 consolidation):
#   data/intermediate/car/CAROverlap_robust/muni{code}.csv
#   data/intermediate/car/CAROverlap_s2/muni{code}.csv
#
# NOTE: this file covers the intersection computation only; the consolidation into
# the CAR-level files stage 04 reads lives in 03b_consolidate_car_overlaps.R
# (reproducible core: s2 + robust, no v1-v4 archives -- see issues log, issue #15).
#
# Deviations are marked "NOTE(migration):" and catalogued in the issues log.

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(magrittr)
  library(dplyr)
  library(stringi)
  library(here)
})

sf_use_s2(TRUE)

source(here("code", "01_build", "01_car", "_helpers_car_util.R"))
source(here("code", "01_build", "01_car", "_helpers_car_cleaning.R"))

# ---- inputs ----------------------------------------------------------------
in_robust_root      <- here("data", "intermediate", "car", "CleanCARShapes_robust")
in_s2_root          <- here("data", "intermediate", "car", "CleanCARShapes_s2")
in_microdata_update <- here("data", "intermediate", "car", "temas_ambientais_update.csv")

required_inputs <- c(in_robust_root, in_microdata_update)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(paste(
    "Missing required input(s) for CAR stage 03:",
    paste0(" - ", missing_inputs, collapse = "\n"),
    "The CleanCARShapes_robust/s2 shapes come from 01_clean_car_shapes.R and the",
    "microdata from 00_car_registration_years.R.",
    sep = "\n"
  ))
}

# ---- outputs ---------------------------------------------------------------
out_robust_dir <- here("data", "intermediate", "car", "CAROverlap_robust")
out_s2_dir     <- here("data", "intermediate", "car", "CAROverlap_s2")
dir.create(out_robust_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_s2_dir, recursive = TRUE, showWarnings = FALSE)

# ---- updated microdata -----------------------------------------------------
microdata <- fread(in_microdata_update) %>%
  .[!duplicated(registro_car)]

# ---- workhorse: pairwise intersections for one municipality's CARs ----------
# NOTE(migration): the legacy function took a `microdata` argument but then read the
# global `microdata_muni` for the date join (a latent bug -- it worked only because
# the two were the same object at every call site). Here the per-muni microdata is an
# explicit argument that is actually used.
compute_intersections <- function(car, microdata_muni) {
  # self-intersection: every pair of overlapping CARs (incl. each CAR with itself).
  # NOTE(migration): for very dense municipalities the single-call
  # st_intersection(car, car) materializes all pairwise overlaps at once and can
  # exhaust memory (observed >17 GB on dense Rondonia munis). When CHUNK_ROWS is
  # set, the left side is processed in row blocks and results row-bound -- the
  # result rows and their order are identical to the single call (st_intersection
  # orders output by left-hand rows), only peak memory changes.
  chunk_rows <- as.integer(Sys.getenv("CHUNK_ROWS", unset = "0"))
  if (chunk_rows > 0 && nrow(car) > chunk_rows) {
    starts <- seq(1, nrow(car), by = chunk_rows)
    message(sprintf("  chunked self-intersection: %d CARs, %d chunks of %d",
                    nrow(car), length(starts), chunk_rows))
    car_intersection <- do.call(rbind, lapply(seq_along(starts), function(k) {
      j <- starts[k]
      message(sprintf("    chunk %d/%d [rows %d-%d] %s", k, length(starts),
                      j, min(j + chunk_rows - 1, nrow(car)),
                      format(Sys.time(), "%H:%M:%S")))
      block <- st_intersection(car[j:min(j + chunk_rows - 1, nrow(car)), ], car)
      gc()
      block
    }))
  } else {
    car_intersection <- st_intersection(car, car)
  }
  car_intersection <- clean_shape(car_intersection)
  car_intersection <- car_intersection %>% mutate(int_area = st_area(geometry))

  out <- car_intersection %>% as.data.table() %>% copy() %>% .[, geometry := NULL]

  car_date <- microdata_muni[, .(registro_car, data_inscricao)]

  out %>%
    .[, .(COD_IMOVEL, COD_IMOVEL.1, int_area, NUM_AREA, COD_ESTADO,
          NUM_AREA.1, COD_ESTADO.1, SITUACAO, CONDICAO_I, SITUACAO.1, CONDICAO_I.1)] %>%
    merge(x = ., y = car_date, by.x = "COD_IMOVEL.1", by.y = "registro_car",
          all.x = TRUE, all.y = FALSE) %>%
    rename_columns(c("data_inscricao"), c("data_inscricao.1")) %>%
    merge(x = ., y = car_date, by.x = "COD_IMOVEL", by.y = "registro_car",
          all.x = TRUE, all.y = FALSE)
}

# process one municipality: read its cleaned CARs, keep the registered-before-2023
# ones, compute pairwise intersections, write the per-muni CSV. Skip-if-exists.
process_muni <- function(THIS_muni, shp_dir, out_dir) {
  out_file <- file.path(out_dir, paste0("muni", THIS_muni, ".csv"))
  if (file.exists(out_file)) return(invisible(NULL))

  microdata_muni <- microdata %>%
    .[, .(registro_car, codigo_ibge, data_inscricao, situacao_cadastro)] %>%
    copy() %>%
    .[codigo_ibge == THIS_muni] %>%
    .[!duplicated(registro_car)] %>%
    .[year(data_inscricao) < 2023]

  CARS_IN_SAMPLE <- microdata_muni[, unique(registro_car)]
  if (length(CARS_IN_SAMPLE) == 0) return(invisible(NULL))

  message_with_lines(paste0(shp_dir, " (muni ", THIS_muni, ")"))

  # NOTE(migration): legacy does not re-project here -- the cleaned shapes are
  # already EPSG:4674 from stage 01. Kept faithful (no st_transform).
  CARs <- shp_dir %>%
    read_sf() %>%
    .[which(.$COD_IMOVEL %in% CARS_IN_SAMPLE), ]
  if (nrow(CARs) == 0) return(invisible(NULL))

  out <- tryCatch(
    compute_intersections(car = CARs, microdata_muni = microdata_muni),
    error = function(e) {
      message_with_lines(paste0("compute_intersections failed for muni ", THIS_muni,
                                ": ", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(out)) return(invisible(NULL))

  fwrite(out, out_file)
  message_with_lines(paste0("Wrote: ", out_file))
}

# ---- enumerate municipalities per cleaned variant --------------------------
dirs_robust <- list.dirs(in_robust_root, recursive = FALSE) %>%
  data.table(path = .) %>%
  .[, municipio := stri_extract_first_regex(path, "\\d{7}")] %>%
  .[!is.na(municipio)]

dirs_s2 <- if (dir.exists(in_s2_root)) {
  list.dirs(in_s2_root, recursive = FALSE) %>%
    data.table(path = .) %>%
    .[, municipio := stri_extract_first_regex(path, "\\d{7}")] %>%
    .[!is.na(municipio)]
} else {
  data.table(path = character(0), municipio = character(0))
}

# Optional worker-slice argument: a file of 7-digit muni codes (one per line)
# restricts this run to that subset. Used to shard the run across parallel
# containers; results are identical to a serial full run because each muni's
# output is an independent file. No argument = process everything (default).
worker_args <- commandArgs(trailingOnly = TRUE)
if (length(worker_args) >= 1) {
  subset_codes <- readLines(worker_args[1]) %>% trimws() %>% .[. != ""]
  message_with_lines(paste0("Worker slice: ", length(subset_codes), " municipalities from ", worker_args[1]))
  dirs_robust <- dirs_robust[municipio %in% subset_codes]
  dirs_s2 <- dirs_s2[municipio %in% subset_codes]
}

# In worker-slice mode, honour the list file's ordering (lists are sorted
# lightest-first so memory-capped workers defer dense municipalities); the
# default full run keeps the sorted order. Output is order-invariant either way.
muni_order <- if (length(worker_args) >= 1) {
  subset_codes[subset_codes %in% dirs_robust$municipio]
} else {
  sort(unique(dirs_robust$municipio))
}

# ---- Part A(i): robust intersections (primary) -----------------------------
for (THIS_muni in muni_order) {
  shp_dir <- dirs_robust[municipio == THIS_muni, path][[1]]
  process_muni(THIS_muni, shp_dir, out_robust_dir)
  gc()
}

# ---- Part A(ii): s2 intersections (fallback for munis without a robust shape) --
munis_s2_only <- setdiff(dirs_s2$municipio, dirs_robust$municipio)
for (THIS_muni in sort(munis_s2_only)) {
  shp_dir <- dirs_s2[municipio == THIS_muni, path][[1]]
  process_muni(THIS_muni, shp_dir, out_s2_dir)
  gc()
}

message_with_lines("Stage 03 (intersections) complete. Next: 03b_consolidate_car_overlaps.R.")
