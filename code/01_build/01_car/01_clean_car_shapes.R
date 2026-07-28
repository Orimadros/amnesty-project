# CAR build, stage 01: cleaning of per-municipality CAR shapes.
#
# Faithful, reproducible port of section 3 (`if(fp$CLEAN_CARS)`) of the legacy
# producer `legacy_repo/dropbox_producers/create_muni_year_intersections/A_prepare_initial_data.R`
# (Thiago Alckmin, Oct 2023). This is the producer of "Magic File #1" and its siblings.
#
# For each Amazon-biome municipality: read its raw SICAR property polygons, keep the
# CARs registered before 2023, and write three cleaned variants from a common
# basic-cleaned base (matching legacy):
#   - light  = basic clean (make valid, drop dup geometries + empties; keep invalids)
#   - robust = reenforced clean of the light base (drops still-invalid geometries)
#   - s2     = s2-geometry variant of the light base (can fail on some geometries)
#
# Outputs:
#   data/intermediate/car/CleanCARShapes_light/muni{code}/light{code}.shp
#   data/intermediate/car/CleanCARShapes_robust/muni{code}/robust{code}.shp   (Magic File #1)
#   data/intermediate/car/CleanCARShapes_s2/muni{code}/s2_{code}.shp
# Downstream consumers: robust -> 05_combine_car_biome.R; s2 (with light fallback for
# municipalities where s2 cleaning fails) -> 02_car_union_sensitive_land.R.
#
# Key reproducibility changes vs. legacy (catalogued in docs/notes/car_migration_issues.md):
#   - The robust/s2 saves were buried inside `if(FALSE){...}` (disabled after a
#     one-time run); un-gated here so the files are actually regenerated (issue #3).
#   - The legacy muni loop used random `sample()` selection plus a shared
#     `munis_already_claimed_CAR.csv` file to coordinate parallel workers across
#     machines -- non-deterministic and machine-state-dependent. Replaced with a
#     deterministic ordered loop with skip-if-exists (issue #4).
#   - Only the CAROverlap_invalid_preclean diagnostic side-outputs are dropped; the
#     light/s2 variants ARE produced because stage 02 (B) consumes them (issue #5,
#     corrected).

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
in_shapefiles_root <- here("data", "input", "sicar", "shapefiles")
in_microdata_update <- here("data", "intermediate", "car", "temas_ambientais_update.csv")
in_municipalities  <- here("data", "input", "aux", "municipalities_amazon_biome", "municipalities_amazon_biome.shp")

required_inputs <- c(in_shapefiles_root, in_microdata_update, in_municipalities)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(paste(
    "Missing required input(s) for CAR stage 01:",
    paste0(" - ", missing_inputs, collapse = "\n"),
    "temas_ambientais_update.csv is produced by 00_car_registration_years.R;",
    "the raw SICAR shapefiles come from the amazonLandPrices_project Dropbox",
    "(see docs/notes/car_magic_files_recovery.md).",
    sep = "\n"
  ))
}

# ---- outputs ---------------------------------------------------------------
out_light_root  <- here("data", "intermediate", "car", "CleanCARShapes_light")
out_robust_root <- here("data", "intermediate", "car", "CleanCARShapes_robust")
out_s2_root     <- here("data", "intermediate", "car", "CleanCARShapes_s2")
for (d in c(out_light_root, out_robust_root, out_s2_root)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ---- filter set: Amazon-biome municipalities -------------------------------
municipalities_amazon <- read_sf(in_municipalities) %>%
  st_transform(crs = 4674) %>%
  st_make_valid() %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  .[, unique(geocodigo)]

# ---- updated microdata: which CARs belong to each muni ---------------------
microdata <- fread(in_microdata_update) %>%
  .[!duplicated(registro_car)]

# ---- enumerate the raw AREA_IMOVEL property directories --------------------
# Keep the ".../AREA_IMOVEL" folders, excluding the split ".../AREA_IMOVEL_n"
# variants; restrict to municipalities in the Amazon biome.
# NOTE(migration): legacy handled one municipality (RO / 1100205) whose raw data
# was split into AREA_IMOVEL_1..4 via a manual QUICK_FIX pre-merge step. That
# manual step is not reproduced here; if such split dirs are present they should
# be merged into a single AREA_IMOVEL folder upstream (see issue #6).
property_directories <-
  list.dirs(in_shapefiles_root, recursive = TRUE) %>%
  .[!stri_detect_fixed(., "AREA_IMOVEL_")] %>%
  .[stri_detect_fixed(., "AREA_IMOVEL")] %>%
  data.table(path = .) %>%
  # NOTE(migration): legacy used stri_extract_all_regex here, which returns a LIST
  # column. That was safe in the legacy loop (it only did %in% / stri_detect on it),
  # but this rewrite sorts and == compares the muni code, which errors on a list
  # column. stri_extract_first_regex returns an atomic character vector and is
  # behaviourally identical (each path holds exactly one 7-digit code). See issue #8.
  .[, municipio := stri_extract_first_regex(path, "\\d{7}")] %>%
  .[municipio %in% municipalities_amazon] %>%
  .[order(path)]

# deterministic ordered set of municipalities to process
munis_to_clean <- property_directories[, unique(municipio)] %>% sort()

message_with_lines(paste0(length(munis_to_clean), " municipalities to consider."))

# ---- clean each municipality's CARs ----------------------------------------
for (THIS_muni in munis_to_clean) {

  dir_light  <- file.path(out_light_root,  paste0("muni", THIS_muni))
  dir_robust <- file.path(out_robust_root, paste0("muni", THIS_muni))
  dir_s2     <- file.path(out_s2_root,     paste0("muni", THIS_muni))
  file_light  <- file.path(dir_light,  paste0("light",  THIS_muni, ".shp"))
  file_robust <- file.path(dir_robust, paste0("robust", THIS_muni, ".shp"))
  file_s2     <- file.path(dir_s2,     paste0("s2_",    THIS_muni, ".shp"))

  # skip-if-exists: makes the run resumable and idempotent. `robust` is the
  # completion marker (it is always produced when a muni is processed; s2 may
  # legitimately be absent when s2 cleaning fails for that muni's geometries).
  if (file.exists(file_robust)) next

  PROPERTY_SHP <- property_directories[municipio == THIS_muni, path][[1]]

  # CARs registered (before 2023) in this municipality
  CARS_IN_SAMPLE <- microdata %>%
    .[, .(registro_car, codigo_ibge, data_inscricao, situacao_cadastro)] %>%
    copy() %>%
    .[codigo_ibge == THIS_muni] %>%
    .[!duplicated(registro_car)] %>%
    .[year(data_inscricao) < 2023] %>%
    .[, unique(registro_car)]

  if (length(CARS_IN_SAMPLE) == 0) next

  message_with_lines(PROPERTY_SHP)

  # 1: load property polygons, project to SIRGAS 2000 (EPSG:4674), keep sampled CARs
  car <- PROPERTY_SHP %>%
    read_sf() %>%
    st_transform(4674) %>%
    .[which(.$COD_IMOVEL %in% CARS_IN_SAMPLE), ]

  if (nrow(car) == 0) next

  # 2: basic clean -> the common "light" base (make valid, drop dup geometries +
  #    empties; keep invalids)
  car <- clean_shape_basic(car)
  dir.create(dir_light, recursive = TRUE, showWarnings = FALSE)
  st_write(car, file_light, quiet = TRUE, delete_layer = TRUE)

  # 3: reenforced clean -> the "robust" shapes (drops still-invalid geometries)
  car_robust <- clean_shape_reenforced(car)
  dir.create(dir_robust, recursive = TRUE, showWarnings = FALSE)
  st_write(car_robust, file_robust, quiet = TRUE, delete_layer = TRUE)

  # 4: s2 variant. s2 cleaning can fail on pathological geometries; when it does,
  #    legacy simply produced no s2 output for that muni and downstream (stage 02)
  #    fell back to the light variant. Preserve that: on failure, warn and continue.
  s2_ok <- tryCatch({
    car_s2 <- clean_shape_s2(car)
    dir.create(dir_s2, recursive = TRUE, showWarnings = FALSE)
    st_write(car_s2, file_s2, quiet = TRUE, delete_layer = TRUE)
    TRUE
  }, error = function(e) {
    message_with_lines(paste0("s2 cleaning failed for muni ", THIS_muni,
                              " (downstream falls back to light): ", conditionMessage(e)))
    FALSE
  })

  message_with_lines(paste0("Wrote muni ", THIS_muni,
                            ": light + robust", if (s2_ok) " + s2" else " (s2 skipped)"))
}

message_with_lines("Stage 01 complete: CleanCARShapes_robust written.")
