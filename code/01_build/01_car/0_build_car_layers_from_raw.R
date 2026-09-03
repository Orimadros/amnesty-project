# Build CAR layers from raw inputs.
#
# This module is intentionally upstream-only:
# - raw inputs from `data/input/**`
# - generated outputs to `data/intermediate/car/**`
#
# Classification rules (explicit and deterministic):
# - `already_treated`: SNCI-certified parcels (area < 100,000 ha) in target states
# - `car_ineligible_cleaned`: CAR parcels with > 1% overlap with control areas
# - `car_eligible_cleaned`: CAR parcels with > 1% overlap with target areas and
#   not ineligible

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(here)
  library(stringr)
  library(stringi)
  library(readr)
  library(fs)
  library(purrr)
})

# NOTE(migration): this scaffold runs under GEOS (planar) geometry, matching the
# legacy 2_empirics workflow. The s2 (spherical) engine rejects a whole class of
# geometries here -- the biome border self-crosses on the sphere (#23), and the
# constructive st_intersection/st_difference steps in the overlap-share scoring
# produce near-degenerate slivers that s2 then refuses to re-parse ("Loop not
# valid: Edge ... degenerate/crosses"). GEOS treats all of these as valid. This is
# a map-classification artifact (eligible/ineligible/already_treated), not the
# validated municipal panel, so planar geometry is both faithful and appropriate.
# See issues log #23/#26.
sf_use_s2(FALSE)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(path)) {
    stop("Failed to create directory: ", path)
  }
  invisible(path)
}

st_erase <- function(x, y) {
  st_difference(x, st_union(st_geometry(y)))
}

# Repair geometries that are GEOS-valid but s2-invalid (rings that self-cross on
# the sphere), which s2 rejects at intersection time ("Loop N not valid: Edge ...
# crosses edge ..."). One-time s2_rebuild(split_crossing_edges) rewrites the WKB.
# Same repair as issues log #23; applied to every raw layer this script intersects.
s2_repair <- function(x) {
  g <- s2::as_s2_geography(st_as_binary(st_geometry(x)), check = FALSE)
  g <- s2::s2_rebuild(g, options = s2::s2_options(split_crossing_edges = TRUE))
  st_geometry(x) <- st_as_sfc(g)
  st_crs(x) <- 4674
  x
}

normalize_text <- function(x) {
  x %>%
    as.character() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    stringr::str_to_upper() %>%
    stringr::str_squish()
}

pick_first_column <- function(tbl, candidates) {
  hit <- candidates[candidates %in% names(tbl)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

calc_overlap_share <- function(car_sf, mask_sf) {
  if (nrow(mask_sf) == 0) {
    return(tibble(car_id = car_sf$car_id, overlap_share = 0))
  }

  # NOTE(migration): st_intersection over all ~801k CARs vs the mask is the scaffold
  # bottleneck (ran >2 h unchunked). The control/target masks are small federal-land
  # subsets, so almost no CAR overlaps them. Pre-filter with the spatial index
  # (st_intersects) and only intersect the few CARs that actually touch the mask --
  # non-touching CARs contribute 0 overlap, which the right_join + coalesce(0) below
  # already yields. Output-equivalent; same optimization as issues log #25.
  touch <- lengths(st_intersects(car_sf, mask_sf)) > 0
  message(sprintf("    overlap pre-filter: %d / %d CARs touch mask", sum(touch), nrow(car_sf)))
  if (!any(touch)) {
    return(tibble(car_id = car_sf$car_id, overlap_share = 0))
  }

  inter <- st_intersection(
    car_sf[touch, ] %>% select(car_id, area_ha),
    mask_sf %>% select(geometry)
  )

  if (nrow(inter) == 0) {
    return(tibble(car_id = car_sf$car_id, overlap_share = 0))
  }

  inter %>%
    mutate(inter_ha = as.numeric(st_area(geometry)) / 10000) %>%
    st_drop_geometry() %>%
    group_by(car_id) %>%
    summarise(inter_ha = sum(inter_ha, na.rm = TRUE), .groups = "drop") %>%
    right_join(car_sf %>% st_drop_geometry() %>% select(car_id, area_ha), by = "car_id") %>%
    mutate(
      inter_ha = coalesce(inter_ha, 0),
      overlap_share = if_else(area_ha > 0, inter_ha / area_ha, 0)
    ) %>%
    select(car_id, overlap_share)
}

ensure_dir(here("data", "intermediate", "car"))

# ---- raw inputs (explicit, no hidden fallbacks) -----------------------------
# NOTE(migration): path fixes 2026-07-15 -- "auxiliary" -> on-disk "aux", and
# car_combined_amazonBiome2.shp is now PRODUCED by 05_combine_car_biome.R into
# data/intermediate/car/ (it is no longer a given input).
raw_amazon_biome <- here("data", "input", "aux", "amazon_biome_border", "amazon_biome_border.shp")
raw_snci <- here("data", "input", "titles", "snci", "snci_certificacoes", "Imvel certificado SNCI Brasil.shp")
raw_car_base <- here("data", "intermediate", "car", "car_combined_amazonBiome2.shp")
raw_cnfp_dir <- here("data", "input", "cnfp", "SHP_2013")

required_paths <- c(raw_amazon_biome, raw_snci, raw_car_base, raw_cnfp_dir)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop(
    paste(
      "Missing required raw input(s) for CAR build:",
      paste0(" - ", missing_paths, collapse = "\n"),
      "Populate these files/directories under data/input before running `make 01_car`.",
      sep = "\n"
    )
  )
}

# ---- output targets ---------------------------------------------------------
out_already_treated <- here("data", "intermediate", "car", "already_treated.shp")
out_eligible <- here("data", "intermediate", "car", "car_eligible_cleaned.shp")
out_ineligible <- here("data", "intermediate", "car", "car_ineligible_cleaned.shp")

# ---- load base geographies --------------------------------------------------
amazon_biome <- st_read(raw_amazon_biome, quiet = TRUE) %>%
  st_transform(4674) %>%
  st_make_valid()

cnfp_files <- dir_ls(raw_cnfp_dir, regexp = "\\.shp$", recurse = TRUE)
if (length(cnfp_files) == 0) {
  stop("No CNFP shapefiles found under: ", raw_cnfp_dir)
}

cnfp <- purrr::map_dfr(cnfp_files, ~ st_read(.x, quiet = TRUE)) %>%
  st_transform(4674) %>%
  st_make_valid() %>%
  st_intersection(amazon_biome)

req_cnfp_cols <- c("governo", "classe", "protecao")
missing_cnfp_cols <- req_cnfp_cols[!req_cnfp_cols %in% names(cnfp)]
if (length(missing_cnfp_cols) > 0) {
  stop(
    "CNFP input is missing required columns: ",
    paste(missing_cnfp_cols, collapse = ", ")
  )
}

cnfp <- cnfp %>%
  mutate(
    governo_norm = normalize_text(governo),
    classe_norm = normalize_text(classe),
    protecao_norm = normalize_text(protecao)
  )

control_areas <- cnfp %>%
  filter(governo_norm == "FEDERAL") %>%
  filter(str_detect(classe_norm, "UC|TI")) %>%
  st_make_valid()

target_areas <- cnfp %>%
  filter(governo_norm == "FEDERAL") %>%
  filter(str_detect(protecao_norm, "SEM DESTINACAO")) %>%
  st_make_valid()

if (nrow(control_areas) > 0 && nrow(target_areas) > 0) {
  target_areas <- st_erase(target_areas, control_areas) %>%
    st_make_valid()
}

# ---- 1) Build already_treated ----------------------------------------------
snci <- st_read(raw_snci, quiet = TRUE) %>%
  st_transform(4674) %>%
  st_make_valid() %>%
  mutate(
    codigo_imo = suppressWarnings(as.numeric(cod_imovel)),
    year = suppressWarnings(as.numeric(str_extract(data_certi, "([0-9]){4}"))),
    area = suppressWarnings(as.numeric(qtd_area_p))
  )

already_treated <- snci %>%
  filter(area < 100000) %>%
  filter(uf_municip %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO", "MT", "MA")) %>%
  st_intersection(amazon_biome) %>%
  st_make_valid()

st_write(already_treated, out_already_treated, delete_layer = TRUE, quiet = TRUE)
message("Wrote: ", out_already_treated)

# ---- 2) Deterministic CAR split --------------------------------------------
# NOTE(migration): raw_car_base is car_combined_amazonBiome2.shp, which
# 05_combine_car_biome.R ALREADY clipped to the biome. The legacy re-intersection
# with amazon_biome here is redundant (a no-op that re-intersects 801k features
# against the ~168k-edge biome polygon -- the same wasted-work trap as #25). Dropped.
car_raw <- st_read(raw_car_base, quiet = TRUE) %>%
  st_transform(4674) %>%
  st_make_valid()

car_id_col <- pick_first_column(car_raw, c("COD_IMOVEL", "COD_IMO", "cod_imovel", "registro_car"))
if (is.na(car_id_col)) {
  stop("Could not identify CAR id column in raw CAR shapefile.")
}

car_area_col <- pick_first_column(car_raw, c("NUM_ARE", "num_area", "area", "AREA_HA"))

car <- car_raw %>%
  mutate(
    car_id = as.character(.data[[car_id_col]]),
    area_ha = if (!is.na(car_area_col)) suppressWarnings(as.numeric(.data[[car_area_col]])) else NA_real_
  ) %>%
  filter(!is.na(car_id) & car_id != "") %>%
  mutate(
    area_ha = if_else(is.na(area_ha) | area_ha <= 0, as.numeric(st_area(geometry)) / 10000, area_ha)
  ) %>%
  group_by(car_id) %>%
  slice(1) %>%
  ungroup() %>%
  st_make_valid()

control_overlap <- calc_overlap_share(car, control_areas) %>%
  rename(control_overlap_share = overlap_share)

target_overlap <- calc_overlap_share(car, target_areas) %>%
  rename(target_overlap_share = overlap_share)

car_scored <- car %>%
  left_join(control_overlap, by = "car_id") %>%
  left_join(target_overlap, by = "car_id") %>%
  mutate(
    control_overlap_share = coalesce(control_overlap_share, 0),
    target_overlap_share = coalesce(target_overlap_share, 0),
    ineligible_flag = control_overlap_share > 0.01,
    eligible_flag = !ineligible_flag & target_overlap_share > 0.01
  )

car_ineligible_cleaned <- car_scored %>%
  filter(ineligible_flag) %>%
  st_make_valid()

car_eligible_cleaned <- car_scored %>%
  filter(eligible_flag) %>%
  st_make_valid()

st_write(car_eligible_cleaned, out_eligible, delete_layer = TRUE, quiet = TRUE)
st_write(car_ineligible_cleaned, out_ineligible, delete_layer = TRUE, quiet = TRUE)

message("Wrote: ", out_eligible)
message("Wrote: ", out_ineligible)
message("CAR split summary:")
message("  total CAR parcels: ", nrow(car_scored))
message("  eligible: ", nrow(car_eligible_cleaned))
message("  ineligible: ", nrow(car_ineligible_cleaned))
