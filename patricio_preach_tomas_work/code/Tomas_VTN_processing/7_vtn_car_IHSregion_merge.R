# ============================================================
#  Join CAR-region list with each VTN-region CSV (per year)
#  ── keyed ONLY on region_id (region code) ───────────────────
# ============================================================

library(dplyr)
library(purrr)
library(fs)
library(readr)

# ── 1. CAR regions (unique region_id list) -------------------
load("data/clean/all_car_regions.Rdata")

car_tbl <- all_car_regions %>%          
  select(region_id) %>%                
  distinct()

# ── 2. VTN region CSV paths ---------------------------------
vtn_clean_dir <- "data/clean/vtn_IHS"
out_root      <- "data/output/vtn_vs_car_regions"
dir_create(out_root, recurse = TRUE)

years <- c(2015, 2016, 2017, 2018, 2019, 2021, 2022)

# ── 3. Per-year loop ----------------------------------------
summary_tbl <- map_dfr(years, function(yr) {

  vtn_path <- file.path(vtn_clean_dir, sprintf("vtn_region_%d.csv", yr))

  if (!file.exists(vtn_path)) {
    warning("Skipping ", yr, ": file not found at ", vtn_path)
    return(tibble(year = yr, matched = NA_integer_, missing = NA_integer_))
  }

  # read CSV — make sure the region code column is called region_id
  vtn <- read_csv(vtn_path, show_col_types = FALSE)

  # 3a. matched rows (only by region_id)
  car_vtn <- inner_join(car_tbl, vtn, by = "region_id")

  # 3b. CAR regions absent from this VTN year
  car_missing <- anti_join(car_tbl, vtn, by = "region_id")

  # 3c. write per-year outputs
  out_dir <- file.path(out_root, yr)
  dir_create(out_dir)

  write_csv(car_vtn,
            file.path(out_dir, sprintf("car_vtn_region_%d.csv", yr)))
  write_csv(car_missing,
            file.path(out_dir, sprintf("car_missing_region_%d.csv", yr)))

  # 3d. summary row
  tibble(
    year    = yr,
    matched = nrow(car_vtn),
    missing = nrow(car_missing)
  )
})

print(summary_tbl)
cat("\n📂 Region-level CSVs saved in:",
    normalizePath(out_root), "\n")
