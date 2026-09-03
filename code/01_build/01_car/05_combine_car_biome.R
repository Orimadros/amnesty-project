# CAR build, stage 05: combine cleaned CARs and clip to the Amazon biome.
#
# Faithful-intent, reproducible port of `legacy_repo/code/2_empirics.R:255-307`
# (Pedro Rossi). Combines every per-municipality robust-cleaned CAR shapefile into
# one layer and restricts it to the Amazon biome: CARs in states that lie entirely
# inside the biome (AC, AM, RR, AP) are kept whole; CARs in all other states are
# intersected with the biome border.
#
# Output (consumed by 0_build_car_layers_from_raw.R):
#   data/intermediate/car/car_combined_amazonBiome2.shp
#
# The legacy block was interactive scratch code with hardcoded /Users/... Dropbox
# paths, an 80-core doParallel setup, an off-by-one in its chunking, and a
# double-processing of Para -- see issues log #21 for the full list of what was
# regularized. The geometric operation itself (st_intersection of each non-interior
# state's CARs with the biome border) is preserved exactly.

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(magrittr)
  library(dplyr)
  library(stringi)
  library(here)
})

sf_use_s2(TRUE)

source(here("code", "01_build", "01_car", "_helpers_car_util.R"))

# ---- inputs ----------------------------------------------------------------
in_robust_root <- here("data", "intermediate", "car", "CleanCARShapes_robust")
in_biome       <- here("data", "input", "aux", "amazon_biome_border", "amazon_biome_border.shp")

if (!dir.exists(in_robust_root) || length(list.files(in_robust_root, pattern = "\\.shp$", recursive = TRUE)) == 0) {
  stop("No CleanCARShapes_robust shapefiles found. Run 01_clean_car_shapes.R first.")
}
if (!file.exists(in_biome)) {
  stop("Amazon biome border shapefile not found at data/input/aux/amazon_biome_border/.")
}

# ---- output ----------------------------------------------------------------
out_dir <- here("data", "intermediate", "car")
out_combined_biome <- file.path(out_dir, "car_combined_amazonBiome2.shp")

# ---- biome border ------------------------------------------------------------
amazon_bioma <- read_sf(in_biome) %>%
  st_transform(crs = 4674) %>%
  st_make_valid()
# NOTE(migration): the biome border's ~168k-edge ring self-crosses on the sphere,
# which s2 rejects at st_intersection time ("Loop not valid: Edge crosses edge").
# Repair once with s2_rebuild(split_crossing_edges) -- deterministic, keeps the
# whole clip under s2 as pinned. See issues log #23.
biome_s2 <- s2::as_s2_geography(st_as_binary(st_geometry(amazon_bioma)), check = FALSE)
biome_s2 <- s2::s2_rebuild(biome_s2, options = s2::s2_options(split_crossing_edges = TRUE))
st_geometry(amazon_bioma) <- st_as_sfc(biome_s2)
st_crs(amazon_bioma) <- 4674

# ---- combine the robust-cleaned CARs (deterministic order) -------------------
shp_files <- list.files(in_robust_root, pattern = "\\.shp$",
                        recursive = TRUE, full.names = TRUE) %>% sort()

message_with_lines(paste0("Combining ", length(shp_files), " robust CAR shapefiles."))

car <- lapply(shp_files, read_sf) %>%
  do.call(rbind, .)

# ---- restrict to the Amazon biome --------------------------------------------
# States entirely inside the biome: keep their CARs whole (no clipping needed).
INTERIOR_STATES <- c("AC", "AM", "RR", "AP")

car_interior <- car %>% filter(COD_ESTADO %in% INTERIOR_STATES)
car_border   <- car %>% filter(!COD_ESTADO %in% INTERIOR_STATES)

# NOTE(migration): legacy chunked car_border into 100-row blocks across 80 cores.
# The chunking is kept (st_intersection on the full layer is memory-hungry) but made
# sequential and exact: non-overlapping blocks, no out-of-range tail (issue #21).
biome_cols <- setdiff(names(amazon_bioma), attr(amazon_bioma, "sf_column"))

# NOTE(migration): the naive form intersects EVERY border-state CAR against the
# ~168k-edge biome polygon in 100-row blocks -- thousands of constructive spherical
# intersections against a huge geometry (observed: >36 h, no completion). Replaced
# with a predicate pre-filter that is output-equivalent:
#   * CAR fully covered by the biome -> clipping is a no-op, keep it whole
#   * CAR disjoint from the biome    -> clip yields empty, drop it
#   * CAR crossing the border        -> actually clip (the only expensive case, few)
# Predicates (st_intersects/st_covered_by) use the spatial index and are far cheaper
# than st_intersection. See issues log #25.
message_with_lines("Classifying border-state CARs against the biome (predicate pre-filter)...")
touches <- st_intersects(car_border, amazon_bioma, sparse = FALSE)[, 1]
message_with_lines(paste0("  touching biome: ", sum(touches), " / ", nrow(car_border)))

covered <- rep(FALSE, nrow(car_border))
if (any(touches)) {
  covered[touches] <- st_covered_by(car_border[touches, ], amazon_bioma, sparse = FALSE)[, 1]
}
crossing <- touches & !covered
message_with_lines(paste0("  fully inside (keep whole): ", sum(covered),
                          " | crossing (must clip): ", sum(crossing),
                          " | outside (drop): ", sum(!touches)))

car_whole <- car_border[covered, ]

clipped <- if (any(crossing)) {
  car_cross <- car_border[crossing, ]
  chunk_starts <- seq(1, nrow(car_cross), 100)
  message_with_lines(paste0("Clipping ", nrow(car_cross), " crossing CARs in ",
                            length(chunk_starts), " chunks."))
  do.call(rbind, lapply(seq_along(chunk_starts), function(k) {
    j <- chunk_starts[k]
    message(sprintf("    clip chunk %d/%d [rows %d-%d] %s", k, length(chunk_starts),
                    j, min(j + 99, nrow(car_cross)), format(Sys.time(), "%H:%M:%S")))
    out <- st_intersection(amazon_bioma, car_cross[j:min(j + 99, nrow(car_cross)), ])
    gc()
    out
  })) %>%
    # drop attribute columns inherited from the biome layer (legacy: -id, -bioma)
    select(-any_of(biome_cols))
} else {
  car_border[0, ]
}

ps <- rbind(car_interior, car_whole, clipped)

# legacy deduplicated the clipped rows; applied here to the final layer
ps <- ps[!duplicated(ps), ]

st_write(ps, out_combined_biome, quiet = TRUE, delete_layer = TRUE)

message_with_lines(paste0("Stage 05 complete. Wrote: ", out_combined_biome,
                          " (", nrow(ps), " features)"))
