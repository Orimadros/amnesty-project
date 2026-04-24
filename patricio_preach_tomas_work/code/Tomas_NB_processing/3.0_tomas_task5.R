###############################################################################
#  Compare CAR regions to VNP price table (2016-2022) by *name* only
#  – output shows just `region_name` plus the year’s price columns
###############################################################################

library(tidyverse)
library(stringi)
library(fs)
library(readr)

# ── 0. helper: accent-insensitive normaliser ────────────────────────────────
normalize_name <- function(x) {
  x |>
    stri_enc_toutf8() |>
    stri_trans_general("NFD; [:Nonspacing Mark:] Remove; NFC") |>
    stri_trans_toupper(locale = "pt") |>
    stri_trim_both() |>
    stri_replace_all_regex("[[:space:]]+", " ")
}

# ── 1. CAR region list (built earlier) --------------------------------------
load("data/clean/all_car_regions.Rdata")            # gives `all_car_regions`

car_tbl <- all_car_regions |>
  mutate(join_key = normalize_name(region_name)) |>
  distinct(join_key, region_name)

# ── 2. VNP wide table --------------------------------------------------------
vnp_wide <- read_rds("data/clean/vnp/city_region_yearly_pt.rds") |>
  janitor::clean_names() |>
  mutate(join_key = normalize_name(region_name))

# ── 3. Output folders --------------------------------------------------------
out_root <- "data/output/vnp_vs_car_regions"
dir_create(out_root, recurse = TRUE)

years <- 2016:2022

# ── 4. Per-year loop ---------------------------------------------------------
summary_tbl <- map_dfr(years, function(yr) {

  # 4a. columns that belong to this year (e.g. *_2018)
  year_suffix <- paste0("_", yr)
  cols_year   <- names(vnp_wide) |> keep(~ str_ends(.x, year_suffix))

  if (length(cols_year) == 0) {
    warning("No VNP price columns found for year ", yr)
    return(tibble(year = yr, matched = NA_integer_, missing = NA_integer_))
  }

  vnp_year <- vnp_wide |>
    select(join_key, all_of(cols_year))          # <- keep join_key + prices only

  # 4b. joins on normalised name
  car_vnp <- inner_join(car_tbl, vnp_year, by = "join_key") |>
             select(region_name, all_of(cols_year)) |>
             arrange(region_name)

  car_missing <- anti_join(car_tbl, vnp_year, by = "join_key") |>
                 select(region_name) |>
                 arrange(region_name)

  # 4c. write CSVs
  out_dir <- file.path(out_root, yr)
  dir_create(out_dir)

  write_csv(car_vnp,
            file.path(out_dir, sprintf("car_vnp_%d.csv", yr)))
  write_csv(car_missing,
            file.path(out_dir, sprintf("car_missing_%d.csv", yr)))

  # 4d. summary row
  tibble(
    year    = yr,
    matched = nrow(car_vnp),
    missing = nrow(car_missing)
  )
})

print(summary_tbl)
cat("\n📂 Year-by-year CSVs saved in:",
    normalizePath(out_root), "\n")
