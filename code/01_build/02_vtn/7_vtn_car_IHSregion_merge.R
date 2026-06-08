library(dplyr)
library(purrr)
library(readr)
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

load(here("data", "intermediate", "car", "all_car_regions.Rdata"))

car_tbl <- all_car_regions %>%
  select(region_id) %>%
  filter(!is.na(region_id)) %>%
  distinct()

vtn_clean_dir <- here("data", "clean", "vtn_IHS")
out_root <- here("data", "intermediate", "vtn_vs_car_regions")
ensure_dir(out_root)

years <- c(2015, 2016, 2017, 2018, 2019, 2021, 2022)

summary_tbl <- map_dfr(years, function(yr) {
  vtn_path <- file.path(vtn_clean_dir, sprintf("vtn_region_%d.csv", yr))

  if (!file.exists(vtn_path)) {
    warning("Skipping ", yr, ": missing ", vtn_path)
    return(tibble(year = yr, matched = NA_integer_, missing = NA_integer_))
  }

  vtn <- read_csv(vtn_path, show_col_types = FALSE)

  car_vtn <- inner_join(car_tbl, vtn, by = "region_id")
  car_missing <- anti_join(car_tbl, vtn, by = "region_id")

  out_dir <- file.path(out_root, yr)
  ensure_dir(out_dir)

  write_csv(
    car_vtn,
    file.path(out_dir, sprintf("car_vtn_region_%d.csv", yr))
  )
  write_csv(
    car_missing,
    file.path(out_dir, sprintf("car_missing_region_%d.csv", yr))
  )

  tibble(
    year = yr,
    matched = nrow(car_vtn),
    missing = nrow(car_missing)
  )
})

write_csv(summary_tbl, file.path(out_root, "summary.csv"))
print(summary_tbl)
message("Done. Outputs written to: ", out_root)
