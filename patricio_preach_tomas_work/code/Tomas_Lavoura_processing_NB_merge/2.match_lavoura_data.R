###############################################################################
#  Merge CAR parcels with FNP **Lavoura** land-price data  –– 2002-2017 panel
#  --------------------------------------------------------------------------
#  • Keeps ALL parcels (left-join on region_id)
#  • Adds each year’s Lavoura price column
#  • Writes two CSVs per category per year:
#      <cat>_car_lavoura_region_<YYYY>.csv
#      <cat>_missing_lavoura_region_<YYYY>.csv
###############################################################################

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(purrr); library(readr)
  library(fs); library(janitor)
})

lavoura_master <- "data/output/fnp_lavoura_2002_2017_with_state.csv"
out_root       <- "data/output/lavoura_parcels_region"
dir_create(out_root, recurse = TRUE)
years <- 2002:2017

# ── Region polygons & lookup ─────────────────────────────────────────────────
load("data/clean/IHS_regions_divison.Rdata")           # -> regions_2015 (sf)
region_poly   <- regions_2015 %>% select(region_id, state)
region_lookup <- regions_2015 %>% st_drop_geometry() %>%
                   select(region_id, state, region_name)
region_crs    <- st_crs(region_poly)

# ── CAR parcel layers ────────────────────────────────────────────────────────
load("data/output/eligible_car.Rdata")
load("data/output/ineligible_car.Rdata")
load("data/output/legal_car.Rdata")

car_raw <- list(
  eligible   = eligible_car,
  ineligible = ineligible_car,
  legal     = legal_car
)

car_layers <- imap(car_raw, function(layer, nm) {
  layer %>% 
    st_transform(region_crs) %>%                       
    select(-matches("^(state|region_id|mun_name|muni_code|state_uf)$")) %>% 
    st_join(region_poly, left = FALSE) %>% 
    st_drop_geometry() %>% 
    left_join(region_lookup, by = c("region_id", "state")) %>% 
    relocate(state, region_id, region_name)
})

# ── Lavoura wide table ───────────────────────────────────────────────────────
lav_wide <- read_csv(lavoura_master, show_col_types = FALSE) %>% 
  janitor::clean_names() %>% 
  mutate(region_id = as.numeric(region_id))

names(lav_wide) <- sub("^x(\\d{4})$", "\\1", names(lav_wide))   # X2002 → 2002
lav_wide <- lav_wide %>% select(-matches("^region_name$"))       # drop extra name

# Deduplicate region_id rows
dup_ids <- lav_wide %>% count(region_id, name = "n") %>% filter(n > 1)
if (nrow(dup_ids) > 0) {
  warning(nrow(dup_ids), " duplicated region_id rows in Lavoura (keeping first).")
  lav_wide <- lav_wide %>% group_by(region_id) %>% slice(1) %>% ungroup()
}

# ── Per-year × category loop ────────────────────────────────────────────────
summary_tbl <- map_dfr(years, function(yr) {
  
  col_nm <- as.character(yr)
  if (!col_nm %in% names(lav_wide)) {
    warning("Year ", yr, " not present in Lavoura columns.")
    return(tibble(year = yr, category = NA, matched = NA, missing = NA))
  }
  
  price_vec <- suppressWarnings(as.numeric(lav_wide[[col_nm]]))
  price_vec[price_vec == 0] <- NA
  
  lav_year <- lav_wide %>% 
    mutate("{col_nm}" := price_vec) %>% 
    filter(!is.na(.data[[col_nm]])) %>% 
    select(region_id, all_of(col_nm))        # ← state removed to avoid .x/.y
  
  out_dir <- file.path(out_root, yr)
  dir_create(out_dir)
  
  map_dfr(names(car_layers), function(cat) {
    
    parcels <- car_layers[[cat]]
    
    joined <- left_join(parcels, lav_year, by = "region_id") %>% 
              relocate(state, region_id, region_name)
    write_csv(joined,
      file.path(out_dir, sprintf("%s_car_lavoura_region_%d.csv", cat, yr)))
    
    miss <- anti_join(parcels, lav_year, by = "region_id") %>% 
            relocate(state, region_id, region_name)
    write_csv(miss,
      file.path(out_dir, sprintf("%s_missing_lavoura_region_%d.csv", cat, yr)))
    
    tibble(
      year     = yr,
      category = cat,
      matched  = nrow(joined) - nrow(miss),
      missing  = nrow(miss)
    )
  })
})

# ── Console summary ─────────────────────────────────────────────────────────
cat("\n\n═══════════════════════════════════════════════════════════════════════\n")
print(summary_tbl, n = Inf)
cat("\n📂  Parcel-level Lavoura CSVs saved under:\n   ",
    normalizePath(out_root), "\n")
