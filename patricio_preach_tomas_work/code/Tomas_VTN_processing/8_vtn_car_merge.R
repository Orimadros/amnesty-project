# ============================================================
#  Merge CAR parcels with cleaned VTN REGION data (IHS)
#  ------------------------------------------------------------
#  • Keeps ALL parcels (left-join on region_id)
#  • Counts missing parcels per region, per year, per category
#  • Writes per-year CSVs to data/output/vtn_parcels_region/<year>/
# ============================================================

# ── Libraries ───────────────────────────────────────────────
library(sf)
library(dplyr)
library(purrr)
library(readr)
library(fs)

# ── Folder paths ────────────────────────────────────────────
vtn_clean_dir <- "data/clean/vtn_IHS"          # region-level VTN CSVs
out_root      <- "data/output/vtn_parcels_region"
dir_create(out_root, recurse = TRUE)

years <- c(2015, 2016, 2017, 2018, 2019, 2021, 2022)

# ── 1. Region lookup layer (keys + CRS) ─────────────────────
load("data/clean/IHS_regions_divison.Rdata")   # -> regions_2015 (sf)
region_poly <- regions_2015 %>%                # polygons for spatial tag
  select(region_id, state)                     # only keys needed
region_crs  <- st_crs(region_poly)

# Lookup to attach readable names everywhere
region_lookup <- regions_2015 %>% 
  st_drop_geometry() %>% 
  select(region_id, state, region_name)

# ── 2. Raw CAR parcel layers ────────────────────────────────
load("data/output/eligible_car.Rdata")   # -> eligible_car (sf)
load("data/output/ineligible_car.Rdata") # -> ineligible_car (sf)
load("data/output/legal_car.Rdata")     # -> legal_car (sf)

car_raw <- list(
  eligible   = eligible_car,
  ineligible = ineligible_car,
  legal     = legal_car
)

# ── 3. Spatially tag each parcel with region_id  ────────────
car_layers <- imap(car_raw, function(layer, nm) {
  layer %>% 
    st_transform(region_crs) %>%                       
    # drop duplicates & muni columns you no longer need
    select(
      -matches("^(state|region_id|mun_name|muni_code|state_uf)$")
    ) %>%        
    st_join(region_poly, left = FALSE) %>%             # add region_id + state
    st_drop_geometry() %>% 
    # add region_name now so it is always present
    left_join(region_lookup, by = c("region_id", "state")) %>% 
    # reorder: state, region_id, region_name, rest
    relocate(state, region_id, region_name) %>% 
    # put keys first for readability
    select(state, region_id, region_name, everything())
})

# ── 4. Loop over years & categories ─────────────────────────
summary_tbl <- map_dfr(years, function(yr) {
  
  vtn_path <- file.path(vtn_clean_dir, sprintf("vtn_region_%d.csv", yr))
  
  if (!file.exists(vtn_path)) {
    warning("Skipping ", yr, ": VTN region file not found at ", vtn_path)
    return(tibble(year = yr, category = NA, matched = NA, missing = NA))
  }
  
  # Read VTN CSV – must contain region_id
  vtn <- read_csv(vtn_path, show_col_types = FALSE)
  if (!("region_id" %in% names(vtn))) {
    stop("File ", basename(vtn_path), " lacks a region_id column.")
  }
  
  # if VTN itself has a region_name column, drop it to avoid duplicates
  vtn <- vtn %>% select(-matches("^region_name$"))
  
  out_dir <- file.path(out_root, yr)
  dir_create(out_dir)
  
  map_dfr(names(car_layers), function(cat) {
    
    parcels <- car_layers[[cat]]
    
    # 4a. Left-join: keep every parcel
    joined <- left_join(parcels, vtn, by = "region_id") %>% 
              relocate(state, region_id, region_name)  # enforce order again
    write_csv(
      joined,
      file.path(out_dir, sprintf("%s_car_vtn_region_%d.csv", cat, yr))
    )
    
    # 4b. Parcels whose region_id not in this VTN file
    miss <- anti_join(parcels, vtn, by = "region_id") %>% 
            relocate(state, region_id, region_name)
    write_csv(
      miss,
      file.path(out_dir, sprintf("%s_missing_vtn_region_%d.csv", cat, yr))
    )
    
    # 4c. Summary row
    tibble(
      year     = yr,
      category = cat,
      matched  = nrow(joined) - nrow(miss),
      missing  = nrow(miss)
    )
  })
})

print(summary_tbl, n = Inf)
cat("\n📂 Region-level parcel CSVs saved under ",
    normalizePath(out_root), "\n")
