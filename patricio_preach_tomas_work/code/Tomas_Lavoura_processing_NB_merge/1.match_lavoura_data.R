###############################################################################
#  Merge CAR regions with FNP “Lavoura” land-price data (2002-2017)
#  • Master file: keeps every original column + state
#  • Per-year files: treat "-", "" or 0 as NA; missing file includes REGIÃO/TIPO
###############################################################################

library(tidyverse)   # dplyr, purrr, readr, tibble, etc.
library(readxl)      # read_excel()
library(fs)          # dir_create()

# ── 1 ──  CAR region list  ───────────────────────────────────────────────────
load("data/clean/all_car_regions.Rdata")          # gives `all_car_regions`

car_tbl <- all_car_regions %>%
  select(region_id, state) %>%                    # keep code + state
  distinct() %>%
  mutate(region_id = as.numeric(region_id))       # ensure numeric

# ── 2 ──  Read FNP “Lavoura” Excel exactly as is  ────────────────────────────
fp <- "~/Dropbox/amazon_project/data/input/landvalues/vnp/Lavoura_FNP.xlsx"

vnp_full <- read_excel(
               fp,
               skip         = 3,          # start on row 4
               .name_repair = "minimal"   # preserve headers (2002, 2003 …)
             ) %>%
             slice(1:(n() - 2)) %>%       # drop last 2 footer rows
             rename(region_id = `Nº`) %>% # rename code col
             mutate(region_id = as.numeric(region_id)) %>%
             inner_join(car_tbl, by = "region_id") %>%  # add state
             relocate(state, .after = region_id)        # nicer order

# ── 2a ──  Save master file (unchanged values)  ──────────────────────────────
out_master <- "data/output/fnp_lavoura_2002_2017_with_state.csv"
dir_create(dirname(out_master), recurse = TRUE)
write_csv(vnp_full, out_master)

# ── 3 ──  Year-by-year match / miss reports  ────────────────────────────────
years     <- 2002:2017
out_root  <- "data/output/fnp_vs_car_regions"
dir_create(out_root, recurse = TRUE)

summary_tbl <- map_dfr(years, function(yr) {

  col_name  <- as.character(yr)           # e.g. "2008"
  price_raw <- vnp_full[[col_name]]

  # Treat "-", "" or 0 as NA *for the match step only*
  price_clean <- suppressWarnings(as.numeric(price_raw))
  price_clean[price_clean == 0] <- NA

  # Rows that contain a real price for this year
  vnp_year <- vnp_full %>%
              filter(!is.na(price_clean)) %>%
              select(region_id, state,
                     `REGIÃO`, `TIPO DE TERRA`,
                     !!col_name)

  # Matched region_ids (already joined above)
  car_vnp <- vnp_year

  # Region_ids missing a valid price this year
  car_missing <- anti_join(car_tbl, car_vnp, by = "region_id") %>%
                 left_join(vnp_full %>%
                           select(region_id, `REGIÃO`, `TIPO DE TERRA`),
                           by = "region_id") %>%
                 relocate(`REGIÃO`, `TIPO DE TERRA`, .after = state)

  # ── 3a ──  Write CSVs  ────────────────────────────────────────────────────
  out_dir <- file.path(out_root, yr)
  dir_create(out_dir)

  write_csv(car_vnp,
            file.path(out_dir, sprintf("car_fnp_%d.csv", yr)))
  write_csv(car_missing,
            file.path(out_dir, sprintf("car_missing_%d.csv", yr)))

  # ── 3b ──  Summary row  ───────────────────────────────────────────────────
  tibble(
    year    = yr,
    matched = nrow(car_vnp),
    missing = nrow(car_missing)
  )
})

print(summary_tbl)
cat("\n✔︎ Master file saved to:\n   ", normalizePath(out_master),
    "\n📂 Year-by-year CSVs saved in:\n   ", normalizePath(out_root), "\n")
