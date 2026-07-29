# Stage 3 of the Lavoura chain.
#
# Joins BOTH price sources -- NB/VNP (FNP North Brazil) and Lavoura (FNP) -- onto
# each CAR parcel, for all three categories, and reports their divergence.
#
# This is the script `2_empirics.R:2313` consumes
# (<cat>_parcel_nb_lavoura_wide.rds).
#
# Migrated from:
#   legacy_repo/code/patricio_preach_tomas_work/code/
#     Tomas_Lavoura_processing_NB_merge/3.match_lavoura_data.R
#
# Deviations recorded in docs/notes/lavoura_migration_issues.md.

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
library(stringi)
library(stringr)
library(janitor)
library(here)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(path)) {
    stop("Failed to create directory: ", path)
  }
  invisible(path)
}

# Accent-insensitive, case-insensitive region key. NB panels are keyed by region
# NAME (there is no region_id in the FNP North-Brazil workbook), so the join to
# IHS regions has to go through a normalised name.
normalize_name <- function(x) {
  x |>
    stri_enc_toutf8() |>
    stri_trans_general("NFD; [:Nonspacing Mark:] Remove; NFC") |>
    stri_trans_toupper(locale = "pt") |>
    stri_trim_both() |>
    stri_replace_all_regex("[[:space:]]+", " ")
}

CATEGORIES <- c("eligible", "ineligible", "legal")
LAV_YEARS <- 2002:2017

ihs_dir <- here("data", "intermediate", "car", "ihs_breakdown")
lav_master <- here(
  "data", "intermediate", "lavoura", "fnp_lavoura_2002_2017_with_state.csv"
)
nb_pre <- here("data", "clean", "vnp", "city_region_yearly_pt_pre2015.rds")
nb_post <- here("data", "clean", "vnp", "city_region_yearly_pt.rds")

out_dir <- here("data", "intermediate", "lavoura", "parcels_nb_lavoura")

car_files <- setNames(
  file.path(ihs_dir, paste0(CATEGORIES, "_car_IHS.Rdata")), CATEGORIES
)

needed <- c(unlist(car_files), lav_master, nb_pre, nb_post)
absent <- needed[!file.exists(needed)]
if (length(absent) > 0) {
  stop(
    paste(
      "Missing input(s) required by Lavoura step 3:",
      paste0(" - ", absent, collapse = "\n"),
      "Run `make -f analysis.mk vnp` and `make -f analysis.mk lavoura` first.",
      sep = "\n"
    )
  )
}

ensure_dir(out_dir)

# ---- 1) NB/VNP wide panel (both eras stacked) --------------------------------
# The two eras have almost disjoint column sets (see vnp_migration_issues.md
# issue #V4), so bind_rows produces a very wide, very sparse table; collapsing by
# region key then takes the first non-missing value of each price column.
nb_all <- bind_rows(read_rds(nb_pre), read_rds(nb_post)) %>%
  clean_names() %>%
  mutate(nb_key = normalize_name(region_name))

first_non_na <- function(x) {
  v <- x[!is.na(x)]
  if (length(v) > 0) v[[1]] else NA_real_
}

nb_wide <- nb_all %>%
  group_by(nb_key) %>%
  summarise(across(where(is.numeric), first_non_na), .groups = "drop")

nb_price_cols <- setdiff(names(nb_wide), "nb_key")
message(
  "NB panel: ", nrow(nb_wide), " regions x ", length(nb_price_cols),
  " price columns"
)

# ---- 2) Lavoura wide panel ---------------------------------------------------
lav_wide <- read_csv(
  lav_master, na = c("", "NA", "-"), show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(region_id = as.numeric(region_id)) %>%
  pivot_longer(
    matches("^x?\\d{4}$"),
    names_to = "year",
    values_to = "price",
    values_transform = list(price = ~ parse_number(as.character(.x)))
  ) %>%
  mutate(year = as.integer(gsub("^x", "", year))) %>%
  filter(!is.na(price) & price != 0) %>%
  pivot_wider(
    id_cols = region_id,
    names_from = year,
    values_from = price,
    names_glue = "price_{year}_lavoura"
  )

lav_price_cols <- setdiff(names(lav_wide), "region_id")
message(
  "Lavoura panel: ", nrow(lav_wide), " regions x ", length(lav_price_cols),
  " price columns"
)

# ---- 3) Per-category join + divergence report --------------------------------
comparison <- map_dfr(CATEGORIES, function(cat_nm) {
  message("Processing CAR category: ", cat_nm)

  obj_name <- paste0(cat_nm, "_car")
  load(car_files[[cat_nm]])
  parcels_sf <- get(obj_name)

  # VTN 6 joins left = TRUE; legacy step 3 used an inner spatial join. Filter to
  # reproduce that. (Same treatment as Lavoura step 2 -- issue #L3.)
  parcels <- parcels_sf %>%
    sf::st_drop_geometry() %>%
    filter(!is.na(region_id)) %>%
    mutate(
      region_id = as.numeric(region_id),
      nb_key = normalize_name(region_name)
    )
  rm(parcels_sf)

  parcel_all <- parcels %>%
    left_join(nb_wide, by = "nb_key") %>%
    left_join(lav_wide, by = "region_id") %>%
    select(-any_of(c("mun_name", "muni_code", "state_uf", "row_id"))) %>%
    relocate(state, region_id, region_name)

  cat_dir <- file.path(out_dir, paste0(cat_nm, "_parcels_all"))
  ensure_dir(cat_dir)

  out_rds <- file.path(cat_dir, paste0(cat_nm, "_parcel_nb_lavoura_wide.rds"))
  write_rds(parcel_all, out_rds, compress = "gz")

  # Coverage: parcels with no price from EITHER source, in any year.
  price_cols_present <- intersect(
    c(nb_price_cols, lav_price_cols), names(parcel_all)
  )
  any_price <- rowSums(!is.na(as.matrix(
    parcel_all[, price_cols_present, drop = FALSE]
  ))) > 0
  message(
    "  parcels: ", nrow(parcel_all), " | with no price from either source: ",
    sum(!any_price), " (", round(100 * mean(!any_price), 1), "%)"
  )

  # ---- compact long panel (the form the analysis actually wants) -------------
  # One row per (parcel row, year): the mean NB price across that region's land
  # types, alongside the single Lavoura price. Replaces carrying ~1,700 sparse
  # columns into downstream code.
  id_col <- intersect(
    c("COD_IMO", "COD_IMOVEL", "cod_imovel"), names(parcel_all)
  )[1]

  long <- map_dfr(LAV_YEARS, function(yr) {
    nb_yr <- grep(paste0("_", yr, "$"), nb_price_cols, value = TRUE)
    nb_yr <- intersect(nb_yr, names(parcel_all))
    lav_col <- paste0("price_", yr, "_lavoura")

    nb_mean <- if (length(nb_yr) > 0) {
      m <- rowMeans(
        as.matrix(parcel_all[, nb_yr, drop = FALSE]), na.rm = TRUE
      )
      m[is.nan(m)] <- NA_real_
      m
    } else {
      rep(NA_real_, nrow(parcel_all))
    }

    lav_vec <- if (lav_col %in% names(parcel_all)) {
      parcel_all[[lav_col]]
    } else {
      rep(NA_real_, nrow(parcel_all))
    }

    tibble(
      parcel_id = if (!is.na(id_col)) parcel_all[[id_col]] else NA_character_,
      state = parcel_all$state,
      region_id = parcel_all$region_id,
      region_name = parcel_all$region_name,
      year = yr,
      nb_price = nb_mean,
      lavoura_price = lav_vec
    )
  })

  write_csv(long, file.path(cat_dir, paste0(cat_nm, "_parcel_price_panel.csv")))
  message("  wrote ", basename(out_rds), " + compact long panel")

  # ---- divergence summary ---------------------------------------------------
  long %>%
    group_by(year) %>%
    summarise(
      category = cat_nm,
      n_both = sum(!is.na(nb_price) & !is.na(lavoura_price)),
      pct_both = round(100 * mean(!is.na(nb_price) & !is.na(lavoura_price)), 2),
      mean_nb = round(mean(nb_price, na.rm = TRUE), 2),
      mean_lavoura = round(mean(lavoura_price, na.rm = TRUE), 2),
      mean_diff = round(
        mean(nb_price - lavoura_price, na.rm = TRUE), 2
      ),
      .groups = "drop"
    ) %>%
    relocate(category)
})

out_cmp <- file.path(out_dir, "nb_vs_lavoura_by_year.csv")
write_csv(comparison, out_cmp)
message("Wrote: ", out_cmp)

print(as.data.frame(comparison %>% filter(category == "eligible")))
