###############################################################################
#  NB  +  Lavoura  •  parcel-level join & price comparison   (all categories)
###############################################################################
#
#  For each of: eligible / ineligible / legal
#    • joins NB-wide + Lavoura-wide to CAR parcels
#    • prints coverage & mean-difference stats (2002-2017)
#    • saves <cat>_parcel_nb_lavoura_wide.{rds,csv}
###############################################################################

suppressPackageStartupMessages({
  library(sf);      library(dplyr);   library(readr)
  library(stringi); library(janitor)
})

# ── helper ───────────────────────────────────────────────────────────────────
normalize_name <- function(x) {
  x |>
    stri_enc_toutf8() |>
    stri_trans_general("NFD; [:Nonspacing Mark:] Remove; NFC") |>
    stri_trans_toupper(locale = "pt") |>
    stri_trim_both() |>
    stri_replace_all_regex("[[:space:]]+", " ")
}

# ── static look-up layers (loaded once) ──────────────────────────────────────
load("data/clean/IHS_regions_divison.Rdata")        # regions_2015  (sf)
region_crs <- st_crs(regions_2015)

# ── NB WIDE (dedup by nb_key) ────────────────────────────────────────────────
nb_all <- bind_rows(
            read_rds("data/clean/vnp/city_region_yearly_pt_pre2015.rds"),
            read_rds("data/clean/vnp/city_region_yearly_pt.rds")
          ) |>
          janitor::clean_names() |>
          mutate(nb_key = normalize_name(region_name))

nb_wide <- nb_all |>
  group_by(nb_key) |>
  summarise(across(where(is.numeric), ~ first(na.omit(.x))), .groups = "drop")

# ── Lavoura WIDE (2002-2017) ────────────────────────────────────────────────
lav_long <- read_csv(
              "data/output/fnp_lavoura_2002_2017_with_state.csv",
              na = c("", "NA", "-"), show_col_types = FALSE
            ) |>
            janitor::clean_names() |>
            mutate(region_id = as.numeric(region_id)) |>
            pivot_longer(matches("^x?\\d{4}$"),
                         names_to  = "year",
                         values_to = "price",
                         values_transform = list(
                           price = ~ readr::parse_number(as.character(.x))
                         )) |>
            mutate(year = as.integer(gsub("^x", "", year))) |>
            filter(price != 0 & !is.na(price))

lav_wide <- lav_long |>
  pivot_wider(names_from  = year,
              values_from = price,
              names_glue  = "price_{year}_lavoura") |>
  select(region_id, starts_with("price_"))

# ── CAR parcel layers --------------------------------------------------------
load("data/output/eligible_car.Rdata")
load("data/output/ineligible_car.Rdata")
load("data/output/legal_car.Rdata")

car_raw <- list(
  eligible   = eligible_car,
  ineligible = ineligible_car,
  legal     = legal_car
)

# ── output root --------------------------------------------------------------
out_root <- "data/output/parcels_NB_Lavoura"
fs::dir_create(out_root, recurse = TRUE)

# ── iterate over categories --------------------------------------------------
for (cat_nm in names(car_raw)) {

  message("\n══════════  Processing ", cat_nm, " parcels  ══════════")

  parcels <- car_raw[[cat_nm]] |>
    st_transform(region_crs) |>
    st_join(regions_2015 %>% select(region_id, state), left = FALSE) |>
    st_drop_geometry() |>
    left_join(regions_2015 %>% st_drop_geometry() %>%
                select(region_id, state, region_name),
              by = c("region_id", "state")) |>
    mutate(nb_key = normalize_name(region_name)) |>
    relocate(state, region_id, region_name)

  # ── join NB + Lavoura ─────────────────────────────────────────────────────
  parcel_all <- parcels |>
    left_join(nb_wide, by = "nb_key") |>
    left_join(lav_wide, by = "region_id") |>
    select(-any_of(c("mun_name", "muni_code", "state_uf",
                     "nb_key", "row_id")))

  # ── write files ───────────────────────────────────────────────────────────
  cat_dir <- file.path(out_root, paste0(cat_nm, "_parcels_all"))
  fs::dir_create(cat_dir)

  write_rds(parcel_all,
            file.path(cat_dir, paste0(cat_nm, "_parcel_nb_lavoura_wide.rds")))
  write_csv(parcel_all,
            file.path(cat_dir, paste0(cat_nm, "_parcel_nb_lavoura_wide.csv")))

  # ── coverage stats ────────────────────────────────────────────────────────
  nb_cols  <- grep("^preco_.*_\\d{4}$",      names(parcel_all), value = TRUE)
  lav_cols <- grep("^price_\\d{4}_lavoura$", names(parcel_all), value = TRUE)

  no_price <- rowSums(!is.na(parcel_all[, c(nb_cols, lav_cols)])) == 0
  cat("\n▲ CAR parcels with NO price (NB nor Lavoura): ",
      sum(no_price), "/", nrow(parcel_all), " (",
      round(100 * mean(no_price), 2), "%)\n", sep = "")

  # ── mean (NB − Lavoura) 2002-2017 ─────────────────────────────────────────
  yrs_nb   <- unique(stringr::str_extract(nb_cols,  "\\d{4}"))
  yrs_lav  <- unique(stringr::str_extract(lav_cols, "\\d{4}"))
  common_yrs <- sort(intersect(yrs_nb, yrs_lav))          # 2002-2017

  mean_tbl <- lapply(common_yrs, function(yr) {
    nb_cols_yr <- grep(paste0("_", yr, "$"), nb_cols, value = TRUE)
    lav_col    <- paste0("price_", yr, "_lavoura")
    nb_mean <- rowMeans(parcel_all[, nb_cols_yr], na.rm = TRUE)
    nb_mean[is.nan(nb_mean)] <- NA
    lav_vec <- parcel_all[[lav_col]]
    mask    <- !is.na(nb_mean) & !is.na(lav_vec)
    tibble(
      year      = as.integer(yr),
      mean_diff = if (any(mask)) mean(nb_mean[mask] - lav_vec[mask]) else NA_real_,
      pct_area  = round(mean(mask) * 100, 2)
    )
  }) |>
    bind_rows()

  cat("\n▲ Mean (NB − Lavoura) by year (parcel-level averages):\n")
  print(mean_tbl)

  overall_mean <- round(mean(mean_tbl$mean_diff, na.rm = TRUE), 2)
  cat("\n▲ Overall mean difference across all years: ", overall_mean, "\n\n")
}

message("\n✔︎ All categories processed. Files written under:\n  ",
        fs::path_abs(out_root))
