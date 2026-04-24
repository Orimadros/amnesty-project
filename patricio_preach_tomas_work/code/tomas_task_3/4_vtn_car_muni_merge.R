# ============================================================
#  Join CAR municipality list with each cleaned VTN year
#  (keeps only VTN's mun_name column)
# ============================================================

library(dplyr)
library(purrr)
library(fs)
library(readr)

# ── paths ───────────────────────────────────────────────────
car_tbl <- all_car_munis %>%
  rename(state = state_uf) %>%
  mutate(key = NULL) %>% # ensure no stray column
  select(state, muni_code) # ⚠️ drop mun_name here

vtn_clean_dir <- "data/clean/vtn"
out_root <- "data/output/vtn_vs_car"
dir_create(out_root, recurse = TRUE)

years <- c(2015, 2016, 2017, 2018, 2019, 2021, 2022)

summary_tbl <- map_dfr(years, function(yr) {
  vtn_path <- file.path(
    vtn_clean_dir, paste0("vtn_", yr, "_clean.rds")
  )

  if (!file.exists(vtn_path)) {
    warning("Skipping ", yr, ": file not found at ", vtn_path)
    return(tibble(year = yr, matched = NA_integer_, missing = NA_integer_))
  }

  vtn <- readRDS(vtn_path) %>%
    select(state, muni_code, mun_name, everything())

  # intersection
  car_vtn <- inner_join(car_tbl, vtn, by = c("state", "muni_code"))

  # CAR rows missing in VTN
  car_missing <- anti_join(car_tbl, vtn, by = c("state", "muni_code")) %>%
    left_join(all_car_munis %>% rename(state = state_uf),
      by = c("state", "muni_code")
    ) # recover mun_name

  # write
  out_dir <- file.path(out_root, yr)
  dir_create(out_dir)

  write_csv(
    car_vtn,
    file.path(out_dir, paste0("car_vtn", yr, ".csv"))
  )
  write_csv(
    car_missing,
    file.path(out_dir, paste0("car_missing_vtn", yr, ".csv"))
  )

  tibble(
    year = yr,
    matched = nrow(car_vtn),
    missing = nrow(car_missing)
  )
})

print(summary_tbl)
cat("\n📂 Detailed CSVs saved in", normalizePath(out_root), "\n")
