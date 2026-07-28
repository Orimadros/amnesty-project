# CAR build, stage 02: municipal CAR-union overlaps with sensitive land.
#
# Faithful, reproducible port of the S2/validated path of the legacy producer
# `legacy_repo/dropbox_producers/create_muni_year_intersections/B_intersect_car_union_sensitive_land.R`
# (the `fp$INTERSECT_SFS_S2_OR_VALIDATED` intersection loop + the
# `consolidate_muni_year_variables_s2()` consolidation from helper_functions.R). This
# is the S2 path because its consolidated output is the file stage 04 (D) consumes.
#
# For each Amazon-biome municipality and each year 2014-2022: take the union of that
# year's CARs (read from the s2-cleaned shapes, with a light-cleaned fallback), and
# compute its overlap areas against indigenous land, conservation units, and public
# forest (types A/B/C), plus municipal-area bookkeeping. Then consolidate every
# per-muni-year record into one expanded municipality x year panel.
#
# Outputs:
#   data/intermediate/car/SicarMuniOverlap_s2/muni{code}_{year}.csv      (per muni-year)
#   data/intermediate/car/sicar_overlap_variables_s2.csv                 (row-bound)
#   data/intermediate/car/sicar_overlap_variables_exapanded_s2.csv       (consumed by stage 04)
#
# Deviations from legacy are marked "NOTE(migration):" and catalogued in
# docs/notes/car_migration_issues.md. Notably:
#   - geobr::read_municipality() (a runtime network download) is replaced with a
#     vendored local municipal-boundaries shapefile (issue #10).
#   - the random-sample + shared-claims-file muni loop is replaced with a
#     deterministic ordered loop with skip-if-exists (issue #4, as in stage 01).
#   - the hardcoded per-municipality special cases (2111300, 1300631, 1507300) are
#     preserved verbatim -- they are hand-tuned geometry fixes, not bugs (issue #11).

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

# clean an st_union() result: coerce to sf, reenforce validity, drop empties, and
# rename the `x` geometry column that st_as_sf() produces. Faithful port of
# helper_functions.R::clean_union_reenforced.
clean_union_reenforced <- function(sf_obj) {
  sf_obj %>%
    st_as_sf() %>%
    st_make_valid(geos_method = "valid_structure", geos_keep_collapsed = FALSE) %>%
    mutate(empty = st_is_empty(x)) %>%
    .[.$empty != TRUE, ] %>%
    select(-c("empty")) %>%
    rename(geometry = x)
}

# ---- inputs ----------------------------------------------------------------
in_s2_root         <- here("data", "intermediate", "car", "CleanCARShapes_s2")
in_light_root      <- here("data", "intermediate", "car", "CleanCARShapes_light")
in_microdata_update <- here("data", "intermediate", "car", "temas_ambientais_update.csv")
in_reg_year_wide   <- here("data", "intermediate", "car", "car_and_reg_year_wide.csv")
in_municipalities  <- here("data", "input", "aux", "municipalities_amazon_biome", "municipalities_amazon_biome.shp")
in_indigenous      <- here("data", "input", "aux", "indigenous_area_amazon_biome", "indigenous_area_amazon_biome.shp")
in_conservation    <- here("data", "input", "aux", "conservation_units_amazon_biome")
in_cnfp_root       <- here("data", "input", "cnfp")
# NOTE(migration): replaces geobr::read_municipality(). Must be a FULL (un-clipped)
# municipal-boundaries layer with a `code_muni` field -- NOT the biome-clipped
# `municipalities_amazon_biome` layer, which would understate muni_area for
# municipalities straddling the biome border. See issue #10.
in_municipal_borders <- here("data", "input", "aux", "municipal_boundaries", "municipal_boundaries.shp")

required_inputs <- c(in_s2_root, in_microdata_update, in_reg_year_wide,
                     in_municipalities, in_indigenous, in_conservation,
                     in_cnfp_root, in_municipal_borders)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(paste(
    "Missing required input(s) for CAR stage 02:",
    paste0(" - ", missing_inputs, collapse = "\n"),
    "The CleanCARShapes_s2/light shapes come from 01_clean_car_shapes.R; the",
    "reg-year files from 00_car_registration_years.R; the sensitive-land and",
    "municipal-boundary layers from data/input/ (see docs/notes/car_migration_issues.md).",
    sep = "\n"
  ))
}

# ---- outputs ---------------------------------------------------------------
out_dir <- here("data", "intermediate", "car")
out_overlap_dir <- file.path(out_dir, "SicarMuniOverlap_s2")
dir.create(out_overlap_dir, recursive = TRUE, showWarnings = FALSE)
out_vars_s2       <- file.path(out_dir, "sicar_overlap_variables_s2.csv")
out_vars_exp_s2   <- file.path(out_dir, "sicar_overlap_variables_exapanded_s2.csv")

# ---- relevant states + forest loader ---------------------------------------
# state code -> sigla map, restricted to the project's states (matches legacy A s1).
brazil_states <- data.table(
  uf_code = c(11:17, 21, 51),
  uf_sigla = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "MT")
)
states <- as.character(brazil_states$uf_code)
names(states) <- brazil_states$uf_sigla

# load the CNFP public-forest shapefile for a state (by 2-digit uf code).
load_forest_data <- function(state_code, states_vector = states) {
  STATE <- names(which(states_vector == state_code))
  file_name <- list.files(path = in_cnfp_root, pattern = "*.shp", recursive = TRUE) %>%
    data.table(file = .) %>%
    .[, state := stri_replace_all_fixed(file, "CNFP 2020 Shapefiles/CNFP_2020_", "")] %>%
    .[, state := stri_replace_all_fixed(state, ".shp", "")] %>%
    .[state == STATE] %>%
    .[, file] %>%
    .[order(.)]
  out <- read_sf(file.path(in_cnfp_root, file_name)) %>%
    st_transform(4674)
  # NOTE(migration): some CNFP 2020 state files (observed: RR) contain broken
  # polygons -- unclosed rings that neither s2 nor GEOS will parse ("Unrecognized
  # geometry type code" under s2; "LinearRing not closed" under GEOS). Close the
  # rings in-place (a no-op for well-formed rings), then repair validity through
  # GEOS (s2 briefly off; GEOS rewrites the WKB so downstream s2 ops parse it).
  # Deterministic. See issues log #22.
  st_geometry(out) <- st_sfc(lapply(st_geometry(out), close_polygon_rings),
                             crs = st_crs(out))
  old_s2 <- sf_use_s2()
  suppressMessages(sf_use_s2(FALSE))
  out <- st_make_valid(out)
  suppressMessages(sf_use_s2(old_s2))
  out
}

# close unclosed polygon rings (first point appended if != last); no-op for
# well-formed geometries. Handles POLYGON and MULTIPOLYGON.
close_polygon_rings <- function(g) {
  close_ring <- function(m) {
    if (nrow(m) >= 3 && !identical(m[1, ], m[nrow(m), ])) m <- rbind(m, m[1, ])
    m
  }
  if (inherits(g, "POLYGON")) {
    st_polygon(lapply(g, close_ring))
  } else if (inherits(g, "MULTIPOLYGON")) {
    st_multipolygon(lapply(g, function(poly) lapply(poly, close_ring)))
  } else {
    g
  }
}

# ---- global sensitive-land layers ------------------------------------------
municipalities_amazon <- read_sf(in_municipalities) %>%
  st_transform(crs = 4674) %>%
  st_make_valid() %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  .[, unique(geocodigo)]

indigenous <- read_sf(in_indigenous) %>%
  st_transform(crs = 4674) %>%
  st_make_valid()

conservation <- read_sf(in_conservation) %>%
  st_transform(crs = 4674) %>%
  st_make_valid()

municipal_borders <- read_sf(in_municipal_borders) %>%
  st_transform(crs = 4674) %>%
  st_make_valid()

microdata <- fread(in_microdata_update)

car_and_reg_year_wide <- fread(in_reg_year_wide) %>%
  .[, municipio := as.numeric(substr(registro_car, 4, 10))]

# ---- enumerate per-muni cleaned-shape directories --------------------------
# Prefer the s2-cleaned shape per municipality; fall back to the light-cleaned shape
# for municipalities where s2 cleaning produced no output (faithful to legacy's
# `these <- munis_light[!munis_light %in% munis_s2]`).
dirs_s2 <- list.dirs(in_s2_root, recursive = TRUE) %>%
  data.table(path = .) %>%
  .[, municipio := stri_extract_first_regex(path, "\\d{7}")] %>%
  .[!is.na(municipio) & municipio %in% municipalities_amazon]

dirs_light <- list.dirs(in_light_root, recursive = TRUE) %>%
  data.table(path = .) %>%
  .[, municipio := stri_extract_first_regex(path, "\\d{7}")] %>%
  .[!is.na(municipio) & municipio %in% municipalities_amazon]

munis_only_light <- setdiff(dirs_light$municipio, dirs_s2$municipio)
property_directories <- rbind(
  dirs_s2,
  dirs_light[municipio %in% munis_only_light]
)[order(municipio)]

munis_to_process <- property_directories[, unique(municipio)] %>% sort()

# Optional worker-slice argument (same sharding pattern as stage 03): a file of
# muni codes restricts and orders this run. Skip-if-exists keeps results
# identical to a serial full run. IMPORTANT: the Part B consolidation below only
# runs in full (no-argument) mode -- worker slices produce per-muni-year files
# only, and a final argument-free run consolidates.
worker_args <- commandArgs(trailingOnly = TRUE)
WORKER_MODE <- length(worker_args) >= 1
if (WORKER_MODE) {
  subset_codes <- readLines(worker_args[1]) %>% trimws() %>% .[. != ""]
  munis_to_process <- subset_codes[subset_codes %in% munis_to_process]
  message_with_lines(paste0("Worker slice: ", length(munis_to_process), " municipalities."))
}
message_with_lines(paste0(length(munis_to_process), " municipalities to process."))

# ---- Part A: per municipality x year overlap areas -------------------------
for (THIS_muni in munis_to_process) {

  PROPERTY_SHP <- property_directories[municipio == THIS_muni, path][[1]]
  PROPERTY_UF <- substr(THIS_muni, 1, 2)

  # Early completion check: 2022 is the last year in the loop, so its file
  # existing proves this municipality finished a prior (sharded) run. Skips the
  # expensive per-state forest processing on resume/consolidation sweeps.
  # (Munis with zero registered CARs by 2022 never write it and are re-checked.)
  if (file.exists(file.path(out_overlap_dir, paste0("muni", THIS_muni, "_2022.csv")))) {
    next
  }

  message_with_lines(paste0("Running municipality: ", THIS_muni))

  # public forests for this state, split by type, unioned
  forests <- load_forest_data(PROPERTY_UF) %>%
    mutate(
      typeA = startsWith(codigo, "FPA") * 1,
      typeB = startsWith(codigo, "FPB") * 1,
      typeC = startsWith(codigo, "FPC") * 1
    ) %>%
    st_make_valid() %>%
    mutate(is_valid = st_is_valid(.))
  forests <- forests[which(forests$is_valid == TRUE), ]

  forestA <- forests[forests$typeA == 1, ] %>% st_union() %>% st_make_valid()
  forestB <- forests[forests$typeB == 1, ] %>% st_union() %>% st_make_valid()
  forestC <- forests[forests$typeC == 1, ] %>% st_union() %>% st_make_valid()
  forestsALL <- forests %>% st_union() %>% st_make_valid()

  # time-invariant forest cross-intersections (computed once per municipality)
  intersect_forestAB  <- st_intersection(forestA, forestB) %>% st_make_valid() %>% st_area() %>% sum() %>% .[1]
  intersect_forestAC  <- st_intersection(forestA, forestC) %>% st_make_valid() %>% st_area() %>% sum() %>% .[1]
  intersect_forestBC  <- st_intersection(forestB, forestC) %>% st_make_valid() %>% st_area() %>% sum() %>% .[1]
  intersect_forestALL <- forestsALL %>% st_area() %>% sum() %>% .[1]

  # municipal border + area
  muni_border <- municipal_borders[municipal_borders$code_muni == as.numeric(THIS_muni), ] %>%
    st_make_valid() %>%
    st_union() %>%
    st_make_valid()
  muni_area <- muni_border %>% st_area() %>% sum() %>% .[1]

  # per-year CAR membership for this municipality (wide -> long)
  car_years_muni <- car_and_reg_year_wide %>% copy() %>%
    .[, FULL := 1] %>%
    .[municipio == as.numeric(THIS_muni)] %>%
    select(-c("municipio")) %>%
    melt.data.table(id.vars = c("registro_car")) %>%
    rename_columns(c("variable"), c("year")) %>%
    .[, year := stri_replace_all_fixed(year, "y", "")]

  for (YEAR in as.character(2014:2022)) {

    CARS_THIS_YEAR <- car_years_muni %>%
      .[year == YEAR & value == 1] %>%
      .[, registro_car]
    if (length(CARS_THIS_YEAR) == 0) next

    out_file <- file.path(out_overlap_dir, paste0("muni", THIS_muni, "_", YEAR, ".csv"))
    if (file.exists(out_file)) next

    message_with_lines(paste0(PROPERTY_SHP, " -- ", YEAR))

    # load this muni's cleaned CARs, keep the ones present this year
    car <- PROPERTY_SHP %>%
      read_sf() %>%
      st_transform(4674) %>%
      .[which(.$COD_IMOVEL %in% CARS_THIS_YEAR), ]

    # union of the year's CARs. NOTE(migration): per-CAR clean_shape() is commented
    # out in legacy here (the input is already cleaned); union then reenforce.
    if (THIS_muni != "2111300") {
      car_union <- car %>% st_union() %>% clean_union_reenforced()
    } else {
      # special case (issue #11): clean each shape first, then union
      car_union <- car %>% clean_shape() %>% st_union()
    }

    car_area <- car_union %>% st_area() %>% .[1]

    # intersection with indigenous land
    car_area_intersect_indi <- car_union %>%
      st_intersection(indigenous) %>% st_area() %>% sum() %>% .[1]

    # intersection with conservation units (special case 1300631 -> NA; issue #11)
    if (THIS_muni %in% c("1300631")) {
      car_area_intersect_conserve <- NA
    } else {
      car_area_intersect_conserve <- car_union %>%
        st_intersection(conservation) %>% st_area() %>% sum() %>% .[1]
    }

    # intersection with forest type A (special case 1507300 uses the 2nd piece; issue #11)
    car_intersect_forestA <- st_intersection(forestA, car_union)
    if (THIS_muni %in% c("1507300")) {
      car_area_intersect_forestA <- car_intersect_forestA %>% .[2] %>%
        clean_union_reenforced() %>% st_make_valid() %>% st_area() %>% sum() %>% .[1]
    } else {
      car_area_intersect_forestA <- car_intersect_forestA %>%
        st_make_valid() %>% st_area() %>% sum() %>% .[1]
    }

    # intersection with forest types B and C
    car_area_intersect_forestB <- st_intersection(forestB, car_union) %>%
      st_make_valid() %>% st_area() %>% sum() %>% .[1]
    car_area_intersect_forestC <- st_intersection(forestC, car_union) %>%
      st_make_valid() %>% st_area() %>% sum() %>% .[1]

    # cancelled CAR union area
    car_cancelled <- car[which(car$SITUACAO == "CA"), ] %>%
      mutate(empty = st_is_empty(geometry), valid = st_is_valid(geometry))
    car_cancelled <- car_cancelled[which(car_cancelled$empty == FALSE), ]
    car_cancelled <- car_cancelled[which(car_cancelled$valid == TRUE), ]
    car_cancelled <- car_cancelled %>% st_union() %>% st_make_valid()
    car_area_cancelled <- car_cancelled %>% st_area() %>% sum() %>% .[1]
    if (length(car_area_cancelled) > 1) car_area_cancelled <- max(car_area_cancelled)

    # not-cancelled CAR union area
    car_notcancelled <- car[which(car$SITUACAO != "CA"), ] %>%
      mutate(empty = st_is_empty(geometry), valid = st_is_valid(geometry))
    car_notcancelled <- car_notcancelled[which(car_notcancelled$empty == FALSE), ]
    car_notcancelled <- car_notcancelled[which(car_notcancelled$valid == TRUE), ]
    car_notcancelled <- car_notcancelled %>% st_union() %>% st_make_valid()
    car_area_notcancelled <- car_notcancelled %>% st_area() %>% .[1]
    if (length(car_area_notcancelled) > 1) car_area_notcancelled <- max(car_area_notcancelled)

    # intersection between cancelled and not-cancelled unions
    car_area_intersections <- st_intersection(car_cancelled, car_notcancelled) %>%
      st_make_valid() %>% st_area() %>% sum() %>% .[1]

    # CAR union area clipped to the municipal border (a data-quality check)
    car_area_in_muni <- st_intersection(muni_border, car_union) %>%
      st_make_valid() %>% st_area() %>% sum() %>% .[1]

    data.table(
      car_union_area = car_area,
      car_union_area_in_muni = car_area_in_muni,
      car_area_intersect_indi = car_area_intersect_indi,
      car_area_intersect_conserve = car_area_intersect_conserve,
      car_area_intersect_forestA = car_area_intersect_forestA,
      car_area_intersect_forestB = car_area_intersect_forestB,
      car_area_intersect_forestC = car_area_intersect_forestC,
      intersect_forestAB = intersect_forestAB,
      intersect_forestAC = intersect_forestAC,
      intersect_forestBC = intersect_forestBC,
      intersect_forestALL = intersect_forestALL,
      car_area_cancelled = car_area_cancelled,
      car_area_notcancelled = car_area_notcancelled,
      car_area_ca_notca_intersections = car_area_intersections,
      muni_area = muni_area,
      year = YEAR,
      municipio = THIS_muni
    ) %>%
      fwrite(out_file)
  }
  gc()
}

if (WORKER_MODE) {
  message_with_lines("Worker slice done (consolidation runs only in full mode).")
  quit(save = "no", status = 0)
}

# ---- Part B: consolidate per-muni-year records -----------------------------
# Faithful port of helper_functions.R::consolidate_muni_year_variables_s2().
message_with_lines("Consolidating SicarMuniOverlap_s2 records.")

csv_files <- list.files(out_overlap_dir, full.names = TRUE)
if (length(csv_files) == 0) stop("No SicarMuniOverlap_s2 records to consolidate.")

sicar_vars <- rbindlist(lapply(csv_files, fread), fill = TRUE)
fwrite(sicar_vars, out_vars_s2)

# restrict to < 2023 and expand to the full municipality x year grid
sicar_vars <- fread(out_vars_s2) %>% .[year < 2023]
sicar_vars <- CJ(unique(sicar_vars$municipio), unique(sicar_vars$year)) %>%
  rename_columns(c("V1", "V2"), c("municipio", "year")) %>%
  merge(sicar_vars, by = c("municipio", "year"), all = TRUE) %>%
  .[, year := as.integer(year)]

# per-muni-year CAR counts from microdata (new and cumulative)
munis_years_exp <- CJ(unique(microdata$codigo_ibge), c(2014:2022)) %>%
  rename_columns(c("V1", "V2"), c("codigo_ibge", "year"))

muni_years_with_zero_cars <- microdata %>% copy() %>%
  .[!duplicated(registro_car)] %>%
  .[, .N, .(year(data_inscricao), codigo_ibge)] %>%
  merge(munis_years_exp, c("codigo_ibge", "year"), all = TRUE) %>%
  setnafill(cols = c("N"), fill = 0) %>%
  .[order(codigo_ibge, year)] %>%
  .[order(year), cumN := cumsum(N), codigo_ibge] %>%
  rename_columns(c("N", "cumN", "codigo_ibge"),
                 c("n_new_CARs_microdata", "n_CARs_microdata", "municipio")) %>%
  .[, year := as.integer(year)]

merge(muni_years_with_zero_cars, sicar_vars, c("municipio", "year"), all = TRUE) %>%
  fwrite(out_vars_exp_s2)

message_with_lines(paste0("Wrote: ", out_vars_exp_s2))
