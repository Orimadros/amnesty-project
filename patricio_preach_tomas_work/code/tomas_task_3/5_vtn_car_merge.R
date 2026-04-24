# ============================================================
#  Join CAR municipality list with each cleaned VTN year
#  (keeps only VTN's mun_name column)
# ============================================================


# Task:
# merge CAR datasets to VTN
# Left join, so wee keep all CAR properties, even if we cant match them all
# Report:
# How many properties we can't match to VTN data (for eligible,ineligible, and ilegal)
# Make sure to deal with accents and similar


# ============================================================
#  Merge CAR parcels with cleaned VTN years
#  ------------------------------------------------------------
#  • Keeps ALL parcels (left join on state + muni_code)
#  • Counts missing **parcels** (not municipalities)
#  • Writes per-year CSVs to data/output/vtn_parcels/<year>/
# ============================================================

library(sf)
library(dplyr)
library(purrr)
library(readr)
library(fs)

# ── folders ─────────────────────────────────────────────────
vtn_clean_dir <- "data/clean/vtn"
out_root      <- "data/output/vtn_parcels"
dir_create(out_root, recurse = TRUE)

years <- c(2015, 2016, 2017, 2018, 2019, 2021, 2022)

# ── 1. Municipality map (keys + CRS) ───────────────────────
load("data/clean/muni_division_2015.Rdata")        # sf object
muni_poly <- muni_division_2015 %>%
  select(state = state_uf, muni_code, mun_name)
muni_crs <- st_crs(muni_poly)

# ── 2. Raw CAR layers ──────────────────────────────────────
load("data/output/eligible_car.Rdata")
load("data/output/ineligible_car.Rdata")
load("data/output/ilegal_car.Rdata")

car_raw <- list(
  eligible   = eligible_car,
  ineligible = ineligible_car,
  ilegal     = ilegal_car
)

# ── 3. Attach municipality keys (no names) ─────────────────
car_layers <- imap(car_raw, function(layer, nm) {

  layer %>%
    st_transform(muni_crs) %>%
    # remove any duplicated key/name columns
    select(-matches("^state$|^muni_code$|^mun[_i]")) %>%
    # spatial join adds only state + muni_code
    st_join(muni_poly %>% select(state, muni_code), left = FALSE) %>%
    st_drop_geometry() %>%
    select(state, muni_code, everything())          # keys first
})

# lookup to add readable names in missing-parcel CSVs
muni_lookup <- muni_poly %>% st_drop_geometry()

# ── 4. Loop over years & categories ─────────────────────────
summary_tbl <- map_dfr(years, function(yr) {

  vtn_file <- file.path(vtn_clean_dir, paste0("vtn_", yr, "_clean.rds"))
  if (!file.exists(vtn_file)) {
    warning("Skipping ", yr, ": VTN file not found.")
    return(tibble(year = yr, category = NA, matched = NA, missing = NA))
  }

  vtn <- readRDS(vtn_file) %>%
         select(state, muni_code, mun_name, everything())  # VTN supplies name

  out_dir <- file.path(out_root, yr)
  dir_create(out_dir)

  map_dfr(names(car_layers), function(cat) {

    parcels <- car_layers[[cat]]

    # full left join: keeps every parcel
    joined <- left_join(parcels, vtn, by = c("state", "muni_code"))
    write_csv(joined,
              file.path(out_dir, sprintf("%s_car_vtn_%d.csv", cat, yr)))

    # PARCEL-level missing list (no distinct())
    miss <- anti_join(parcels, vtn, by = c("state", "muni_code")) %>%
            left_join(muni_lookup, by = c("state", "muni_code"))
    write_csv(miss,
              file.path(out_dir, sprintf("%s_missing_vtn_%d.csv", cat, yr)))

    tibble(year     = yr,
           category = cat,
           matched  = nrow(joined) - nrow(miss),   # parcels with VTN
           missing  = nrow(miss))                  # unmatched parcels
  })
})

print(summary_tbl, n = Inf)   # show all rows
cat("\n📂 CSVs saved under ", normalizePath(out_root), "\n")
