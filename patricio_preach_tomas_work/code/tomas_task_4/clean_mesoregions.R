# ================================================================
# build_and_patch_meso_clean.R
# ================================================================

# ── 0. Libraries ───────────────────────────────────────────────
library(sf)       # spatial I/O
library(dplyr)    # data wrangling
library(readr)    # CSV I/O
library(stringi)  # accent stripping
library(fs)       # dir_create, file_exists helpers

# ── 1. Paths ──────────────────────────────────────────────────
raw_dir   <- "data/mesoregions_division_2020"
raw_file  <- file.path(raw_dir, "meso_2020.shp")

clean_dir <- "data/clean"
rdata_out <- file.path(clean_dir, "meso_clean.RData")

override_csv <- "data/clean/analysis_vtn/mesoregion_id_overrides.csv"

# ── 2. Read shapefile & basic clean ───────────────────────────
meso_raw <- st_read(raw_file, quiet = TRUE)

meso_clean <- meso_raw %>%
  transmute(
    state       = abbrv_s,
    region_id   = code_ms,
    region_name = name_ms,
    geometry
  )

# ── 3. Apply region-ID overrides (if CSV exists) ──────────────
if (file_exists(override_csv)) {

  normalize_name <- function(x) {
    x |>
      stri_enc_toutf8() |>
      stri_trans_general("NFD; [:Nonspacing Mark:] Remove; NFC") |>
      stri_trans_toupper(locale = "pt") |>
      stri_trim_both() |>
      stri_replace_all_regex("[[:space:]]+", " ")
  }

  overrides <- read_csv(override_csv, show_col_types = FALSE) %>%
    mutate(
      region_id   = as.integer(region_id),
      norm_region = normalize_name(region_name)
    ) %>%
    filter(!is.na(region_id))                 # keep only filled rows

  if (nrow(overrides) == 0) {
    message("ℹ️  Override sheet found but no numeric region_id values to apply.")
  } else {
    n_before <- sum(meso_clean$region_id != 0 & !is.na(meso_clean$region_id))

    meso_clean <- meso_clean %>%
      mutate(norm_region = normalize_name(region_name)) %>%
      left_join(
        overrides %>% select(state, norm_region, region_id_override = region_id),
        by = c("state", "norm_region")
      ) %>%
      mutate(region_id = coalesce(region_id_override, region_id)) %>%
      select(-norm_region, -region_id_override)

    n_after <- sum(meso_clean$region_id != 0 & !is.na(meso_clean$region_id))
    message("✓ Applied ", n_after - n_before, " region_id override(s) from ",
            basename(override_csv))
  }

} else {
  warning("⚠️  Override CSV not found at ", override_csv,
          "; using raw region_id values.")
}

# ── 4. Save meso_clean.RData ──────────────────────────────────
dir_create(clean_dir, recurse = TRUE)
save(meso_clean, file = rdata_out)
cat("✅  meso_clean saved to:\n   ", normalizePath(rdata_out), "\n")
