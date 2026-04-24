###############################################################################
#  Merge CAR parcels with cleaned **VNP** (NB) price data  –– 2016-2022 panel
#  --------------------------------------------------------------------------
#  • Keeps ALL parcels (left-join on normalised region *name*)
#  • Adds every land-type price column for the target year
#  • DEBUG: prints duplicate join_keys **only for VNP data**
#  • Writes per-year CSVs to data/output/vnp_parcels_region/<year>/
#    (join_key is NOT included in any output files)
###############################################################################

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(purrr)
  library(readr)
  library(fs)
  library(stringi)
  library(janitor)
})

# ── Helper ───────────────────────────────────────────────────────────────────
normalize_name <- function(x) {
  x |>
    stri_enc_toutf8() |>
    stri_trans_general("NFD; [:Nonspacing Mark:] Remove; NFC") |>
    stri_trans_toupper(locale = "pt") |>
    stri_trim_both() |>
    stri_replace_all_regex("[[:space:]]+", " ")
}

# ── 1. Region lookup layer ──────────────────────────────────────────────────
load("data/clean/IHS_regions_divison.Rdata")       # -> regions_2015 (sf)
region_poly <- regions_2015 %>% select(region_id, state, region_name)
region_crs  <- st_crs(region_poly)

# ── 2. CAR parcel layers (eligible / ineligible / legal) ────────────────────
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
    relocate(state, region_id, region_name) %>%
    mutate(join_key = normalize_name(region_name)) %>%
    select(state, region_id, region_name, join_key, everything())
})

# ── 3. VNP wide table (2016-2022) ────────────────────────────────────────────
vnp_wide <- read_rds("data/clean/vnp/city_region_yearly_pt.rds") %>%
  janitor::clean_names() %>%
  mutate(join_key = normalize_name(region_name))

# ── DEBUG: duplicate keys in VNP table ONLY ──────────────────────────────────
vnp_dups <- vnp_wide %>% count(join_key, name = "n") %>% filter(n > 1)
if (nrow(vnp_dups) > 0) {
  warning(nrow(vnp_dups), " duplicated join_keys found in VNP data. ",
          "Only the first entry will be used in joins.")
  cat("\n── Duplicate keys in VNP data ────────────────────────────────────────\n")
  vnp_dups %>% arrange(desc(n)) %>% as.data.frame() %>% print(row.names = FALSE)
}

# Retain the first occurrence of each duplicated key
vnp_wide <- vnp_wide %>% group_by(join_key) %>% slice(1) %>% ungroup()

# ── 4. Output root & year vector ─────────────────────────────────────────────
out_root <- "data/output/vnp_parcels_region"
dir_create(out_root, recurse = TRUE)

years <- 2016:2022

# ── 5. Per-year × category loop ─────────────────────────────────────────────
summary_tbl <- map_dfr(years, function(yr) {

  year_suffix <- paste0("_", yr)
  cols_year   <- names(vnp_wide) %>% keep(~ stringr::str_ends(.x, year_suffix))

  if (length(cols_year) == 0) {
    warning("No VNP price columns found for year ", yr)
    return(tibble(year = yr, category = NA, matched = NA, missing = NA))
  }

  vnp_year <- vnp_wide %>% select(join_key, all_of(cols_year))

  out_dir <- file.path(out_root, yr)
  dir_create(out_dir)

  map_dfr(names(car_layers), function(cat) {

    parcels <- car_layers[[cat]]

    # 5a. Left-join, then DROP join_key
    joined <- left_join(parcels, vnp_year, by = "join_key") %>%
              select(-join_key)

    write_csv(
      joined,
      file.path(out_dir, sprintf("%s_car_vnp_region_%d.csv", cat, yr))
    )

    # 5b. Anti-join for “missing” list, then DROP join_key
    miss <- anti_join(parcels, vnp_year, by = "join_key") %>%
            select(-join_key)

    write_csv(
      miss,
      file.path(out_dir, sprintf("%s_missing_vnp_region_%d.csv", cat, yr))
    )

    # 5c. Summary counts
    tibble(
      year     = yr,
      category = cat,
      matched  = nrow(joined) - nrow(miss),
      missing  = nrow(miss)
    )
  })
})

# ── 6. Console summary ──────────────────────────────────────────────────────
cat("\n\n═══════════════════════════════════════════════════════════════════════\n")
print(summary_tbl, n = Inf)
cat("\n📂  Parcel-level VNP CSVs saved under:\n   ",
    normalizePath(out_root), "\n")
