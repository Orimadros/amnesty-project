# CAR build, stage 03b: consolidate the pairwise CAR overlaps.
#
# Faithful, reproducible port of the consolidation sections of the legacy producer
# `legacy_repo/dropbox_producers/create_muni_year_intersections/C_intersect_individual_cars.R`
# (sections 7.0-7.5, `fp$CONSOLIDATE_VARIABLES_ROBUST`).
#
# Consumes the per-municipality overlap tables from 03_intersect_individual_cars.R and
# produces the CAR-level files stage 04 (D_consolidate) reads:
#   data/intermediate/car/sicar_area_imovel_combined.csv
#   data/intermediate/car/CAR_overlap_variables_final.csv
#   data/intermediate/car/CAR_overlap_variables_reftarget.csv
#   data/intermediate/car/CAR_overlap_variables_conflicts.csv
#   data/intermediate/car/CAR_overlap_variables_reftarget_areas.csv
#
# REPRODUCIBLE-CORE DECISION (issue #15): the legacy `final` additionally unioned in
# archived v1-v4 overlap datasets (`data/processing/archive/CAROverlap_v1../v4/`).
# Those archives have no producer and are absent from the project Dropbox (survey
# 2026-07-14); in the legacy dedup they only gap-filled CAR pairs missing from both
# the s2 and robust runs. Per decision, `final` here is the s2+robust union only.
# Validate the rebuilt muni_year_intersections.csv against the Dropbox snapshot to
# measure any coverage difference.
#
# Deviations are marked "NOTE(migration):" and catalogued in
# docs/notes/car_migration_issues.md (esp. issues #15-#17).

suppressPackageStartupMessages({
  library(data.table)
  library(magrittr)
  library(dplyr)
  library(stringi)
  library(here)
  library(foreign)
})

source(here("code", "01_build", "01_car", "_helpers_car_util.R"))

# ---- inputs ----------------------------------------------------------------
in_shapefiles_root <- here("data", "input", "sicar", "shapefiles")
in_overlap_robust  <- here("data", "intermediate", "car", "CAROverlap_robust")
in_overlap_s2      <- here("data", "intermediate", "car", "CAROverlap_s2")

if (!dir.exists(in_overlap_robust) || length(list.files(in_overlap_robust)) == 0) {
  stop("No CAROverlap_robust outputs found. Run 03_intersect_individual_cars.R first.")
}
if (!dir.exists(in_shapefiles_root)) {
  stop("Raw SICAR shapefiles not found under data/input/sicar/shapefiles.")
}

# ---- outputs ---------------------------------------------------------------
out_dir <- here("data", "intermediate", "car")
out_area_imovel      <- file.path(out_dir, "sicar_area_imovel_combined.csv")
out_final            <- file.path(out_dir, "CAR_overlap_variables_final.csv")
out_reftarget        <- file.path(out_dir, "CAR_overlap_variables_reftarget.csv")
out_conflicts        <- file.path(out_dir, "CAR_overlap_variables_conflicts.csv")
out_reftarget_areas  <- file.path(out_dir, "CAR_overlap_variables_reftarget_areas.csv")

# ---- 7.0: combine every AREA_IMOVEL attribute table -------------------------
# Official per-CAR attributes (NUM_AREA etc.), used to backfill CARs whose robust/s2
# self-intersection is missing.
message_with_lines("Combining AREA_IMOVEL.dbf attribute tables")
area_imovel_dirs <- list.dirs(in_shapefiles_root, recursive = TRUE) %>%
  .[stri_detect_fixed(., "AREA_IMOVEL")] %>%
  .[endsWith(., "AREA_IMOVEL")] %>%
  stri_replace_all_fixed(., "//", "/") %>%
  sort()

sicar_area_imovel_combined <-
  lapply(area_imovel_dirs, function(DIR) {
    foreign::read.dbf(file.path(DIR, "AREA_IMOVEL.dbf")) %>% as.data.table()
  }) %>%
  rbindlist(fill = TRUE)

fwrite(sicar_area_imovel_combined, out_area_imovel)

# ---- 7.1-7.2: row-bind the per-municipality overlap tables ------------------
# NOTE(migration): the legacy loops accumulated with `rbind(car_vars, ...)` where
# `car_vars_s2` / `car_vars_robust` was clearly meant (issue #16) -- as written they
# error or keep only one file. Implemented as the evident intent: bind all files.
read_overlap_dir <- function(dir) {
  files <- list.files(dir, full.names = TRUE, pattern = "\\.csv$") %>% sort()
  lapply(files, function(FILE) {
    fread(FILE) %>%
      .[, `data_inscricao.1` := as.IDate(`data_inscricao.1`)] %>%
      .[, data_inscricao := as.IDate(data_inscricao)]
  }) %>%
    rbindlist(fill = TRUE)
}

car_vars_robust <- read_overlap_dir(in_overlap_robust)
car_vars_s2 <- if (dir.exists(in_overlap_s2) && length(list.files(in_overlap_s2)) > 0) {
  read_overlap_dir(in_overlap_s2)
} else {
  data.table()
}

# ---- 7.3: union s2 + robust, prioritizing s2 ---------------------------------
# cleaning_method labels sort so that s2 beats robust in the dedup below; matches
# legacy ("1_s2_true" < "2_robust"). v1-v4 omitted per issue #15.
if (nrow(car_vars_s2) > 0) {
  car_vars_s2 %<>%
    .[year(data_inscricao.1) < 2023 & year(data_inscricao) < 2023] %>%
    .[, cleaning_method := "1_s2_true"]
}
car_vars_robust %<>%
  .[year(data_inscricao.1) < 2023 & year(data_inscricao) < 2023] %>%
  .[, cleaning_method := "2_robust"]

car_vars <- rbind(car_vars_s2, car_vars_robust, fill = TRUE)

car_vars %>% fwrite(out_final)

# ---- 7.4: reference/target nomenclature --------------------------------------
# Orient every overlapping pair so the REFERENCE CAR is the one registered on or
# after the TARGET's date; dedupe each (reference, target) pair keeping the highest-
# priority cleaning method. Self-intersections are kept.

# reference registered after target
car_vars_cod_after_cod1 <- car_vars %>% copy() %>%
  .[data_inscricao > data_inscricao.1] %>%
  .[, GRP := .GRP, .(COD_IMOVEL, COD_IMOVEL.1)] %>%
  .[order(cleaning_method)] %>%
  .[!duplicated(GRP)] %>% .[, GRP := NULL]

# registered the same day
car_vars_cod_equal_cod1 <- car_vars %>% copy() %>%
  .[data_inscricao == data_inscricao.1] %>%
  .[, GRP := .GRP, .(COD_IMOVEL, COD_IMOVEL.1)] %>%
  .[order(cleaning_method)] %>%
  .[!duplicated(GRP)] %>% .[, GRP := NULL]

# reference registered before target -> swap the pair's roles
car_vars_cod_before_cod1 <- car_vars %>% copy() %>%
  .[data_inscricao < data_inscricao.1] %>%
  .[, GRP := .GRP, .(COD_IMOVEL, COD_IMOVEL.1)] %>%
  .[order(cleaning_method)] %>%
  .[!duplicated(GRP)] %>% .[, GRP := NULL] %>%
  rename_columns(
    c("COD_IMOVEL", "COD_IMOVEL.1", "data_inscricao", "SITUACAO", "SITUACAO.1", "data_inscricao.1"),
    c("COD_IMOVEL.1", "COD_IMOVEL", "data_inscricao.1", "SITUACAO.1", "SITUACAO", "data_inscricao")
  )

car_vars_ordered <- rbind(car_vars_cod_after_cod1, car_vars_cod_equal_cod1) %>%
  rbind(car_vars_cod_before_cod1) %>%
  .[order(cleaning_method)] %>%
  .[, GRP := .GRP, .(COD_IMOVEL, COD_IMOVEL.1)] %>%
  .[!duplicated(GRP)] %>% .[, GRP := NULL]

car_vars_ordered %<>%
  rename_columns(
    c("COD_IMOVEL", "COD_IMOVEL.1", "data_inscricao", "SITUACAO", "SITUACAO.1", "data_inscricao.1"),
    c("carid_reference", "carid_target", "data_inscricao_reference", "SITUACAO_reference", "SITUACAO_target", "data_inscricao_target")
  )

fwrite(car_vars_ordered, out_reftarget)

# conflicts = overlaps between two DIFFERENT CARs
car_vars_ordered %>% .[carid_reference != carid_target] %>%
  fwrite(out_conflicts)

# ---- 7.5: attach own-area to every reference and target ----------------------
# A CAR's own area = its self-intersection area; for CARs with no self-intersection,
# fall back to the official NUM_AREA (hectares -> m2).
own_area <- car_vars_ordered %>%
  copy() %>%
  .[carid_reference == carid_target] %>%
  .[, .(NUM_AREA, carid_reference, int_area, data_inscricao_reference, SITUACAO_reference)] %>%
  .[!duplicated(carid_reference)] %>%
  rename_columns(c("int_area"), c("int_area_ref"))

codes_with_area <- own_area %>% .[, unique(carid_reference)]
codes_all <- car_vars_ordered %>% .[, unique(carid_reference)]
find_info <- codes_all %>% .[!(. %in% codes_with_area)]

own_area_supplement1 <- sicar_area_imovel_combined %>%
  copy() %>%
  .[COD_IMOVEL %in% find_info] %>%
  .[, .(NUM_AREA, COD_IMOVEL, SITUACAO)]

own_area_supplement2 <- car_vars_ordered %>%
  copy() %>%
  .[carid_reference %in% find_info] %>%
  .[!duplicated(carid_reference)] %>%
  .[, .(carid_reference, data_inscricao_reference)]

own_area <- merge(y = own_area_supplement1, x = own_area_supplement2,
                  by.y = "COD_IMOVEL", by.x = "carid_reference", all = TRUE) %>%
  .[, supplemented := TRUE] %>%
  rbind(own_area, ., fill = TRUE) %>%
  .[is.na(int_area_ref), int_area_ref := NUM_AREA * 10000] %>%
  .[, .(carid_reference, int_area_ref)]

# NOTE(migration): legacy wrote `allx=T` (a typo for `all.x`) in both merges, which
# silently fell into `...` and left the default all.x=FALSE -- an inner join that
# would drop pairs whose target id never appears as a reference. Implemented as the
# evident intent, all.x = TRUE (issue #17).
car_vars_ordered_out <-
  merge(car_vars_ordered, own_area,
        by.x = "carid_reference", by.y = "carid_reference",
        all.x = TRUE, all.y = FALSE) %>%
  rename_columns(c("int_area_ref"), c("int_area_reference")) %>%
  merge(., own_area,
        by.x = "carid_target", by.y = "carid_reference",
        all.x = TRUE, all.y = FALSE) %>%
  rename_columns(c("int_area_ref"), c("int_area_target"))

fwrite(car_vars_ordered_out, out_reftarget_areas)

message_with_lines(paste0("Stage 03b complete. Wrote: ", out_final, " ; ",
                          out_reftarget_areas))
