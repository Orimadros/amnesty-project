###############################################################################
#  VTN  •  parcel-level wide table (one row per parcel, all years appended)
#  --------------------------------------------------------------------------
#  • Spatially tags each CAR parcel with region_id                (st_join)
#  • Reads every cleaned VTN-region CSV that exists
#      – renames every non-key column with a “_<year>” suffix
#      – left-joins them to parcels on region_id
#  • Writes 3 files (CSV + RDS) in      data/output/parcels_VTN_wide/
#      eligible_parcels_vtn_wide.{csv,rds}
#      ineligible_parcels_vtn_wide.{csv,rds}
#      legal_parcels_vtn_wide.{csv,rds}
###############################################################################

suppressPackageStartupMessages({
  library(sf);   library(dplyr);  library(purrr);  library(readr);  library(fs)
})

# ── 0 • parameters & paths ──────────────────────────────────────────────────
vtn_dir   <- "data/clean/vtn_IHS"
years     <- c(2015, 2016, 2017, 2018, 2019, 2021, 2022)
out_root  <- "data/output/parcels_VTN_wide"
dir_create(out_root, recurse = TRUE)

# ── 1 • region lookup (keys + CRS) ──────────────────────────────────────────
load("data/clean/IHS_regions_divison.Rdata")            # -> regions_2015 (sf)
region_poly <- regions_2015 %>% select(region_id, state)
region_crs  <- st_crs(region_poly)

region_lookup <- regions_2015 %>%
  st_drop_geometry() %>%
  select(region_id, state, region_name)

# ── 2 • raw CAR parcels ------------------------------------------------------
load("data/output/eligible_car.Rdata")
load("data/output/ineligible_car.Rdata")
load("data/output/legal_car.Rdata")

car_raw <- list(
  eligible   = eligible_car,
  ineligible = ineligible_car,
  legal     = legal_car
)

# ── 3 • spatial tag (adds state, region_id, region_name) --------------------
car_layers <- imap(car_raw, function(layer, nm) {
  layer %>%
    st_transform(region_crs) %>%
    select(-matches("^(state|region_id|mun_name|muni_code|state_uf)$")) %>%
    st_join(region_poly, left = FALSE) %>%      # adds (state, region_id)
    st_drop_geometry() %>%
    left_join(region_lookup, by = c("region_id", "state")) %>%
    relocate(state, region_id, region_name) %>%
    select(state, region_id, region_name, everything())
})

# ── 4 • read all VTN CSVs and build a named list ----------------------------
vtn_tables <- map(years, function(yr) {
  fp <- file.path(vtn_dir, sprintf("vtn_region_%d.csv", yr))
  if (!file.exists(fp)) return(NULL)
  read_csv(fp, show_col_types = FALSE) %>%
    select(-matches("^region_name$")) %>%       # avoid dup column
    distinct(region_id, .keep_all = TRUE) %>%   # guard against accidental dups
    rename_with(~ paste0(.x, "_", yr), -region_id)   # add suffix to every col *except* key
})
names(vtn_tables) <- years
vtn_tables <- compact(vtn_tables)   # drop NULL years (missing CSV)

# ── 5 • build wide table for each parcel layer ------------------------------
walk(names(car_layers), function(cat) {

  tbl <- car_layers[[cat]]

  # iteratively left-join every yearly VTN table
  for (yr in names(vtn_tables)) {
    tbl <- left_join(tbl, vtn_tables[[yr]], by = "region_id")
  }

  # reorder keys first, drop no columns
  tbl <- tbl %>% relocate(state, region_id, region_name)

  # ── 5a • write outputs -----------------------------------------------------
  fn_stub <- file.path(out_root, paste0(cat, "_parcels_vtn_wide"))
  write_rds(tbl,  paste0(fn_stub, ".rds"))
  write_csv(tbl, paste0(fn_stub, ".csv"))

  cat("✔︎  Written: ", basename(fn_stub), ".{rds,csv}\n", sep = "")
})

cat("\n📂  All wide tables saved under ", normalizePath(out_root), "\n")
