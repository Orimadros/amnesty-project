# 4_deforestation_stats.R
# ---------------------------------------------------------------------------
# Step 4 of the MapBiomas backbone: aggregate the per-tile transition rasters
# into a biome-level table of forest / deforested / reforested AREA per year.
#
# Migrated from legacy_repo/code/1_mapbiomas.R (table_amazon_biome, lines
# ~772-802). Preserved verbatim: pixel classes (1 forest, 2 deforested, 3
# reforested) and the 30 m-pixel -> hectares factor (x 0.09). Only structure
# changed: here() paths, explicit I/O, testable scope, progress logging.
#
# SCOPE NOTE: legacy also computed zone breakdowns over glebas / control areas /
# CAR (lines ~804-963). Those are deferred: `control_areas2` is a dangling
# legacy object (see PROBLEMS.md) and the CAR zones are blocked by the CAR
# magic-file. This step delivers the unblocked biome-level totals.
#
# Reads  : per-tile transition rasters grid_<base>_<year>.tif
# Writes : one CSV table (year, n_pixels, forest_area_ha, deforested_area_ha,
#          reforested_area_ha)
#
# --- TESTABILITY -----------------------------------------------------------
#   MB_COMBINED_DIR  transition rasters  (default data/intermediate/mapbiomas/transitions_combined)
#   MB_YEARS         years to summarise  (default 1987:2020)
#   MB_STATS_OUT     output CSV path     (default output/tables/mapbiomas_biome_deforestation.csv)
#
# Run full (in container):
#   make docker-run CMD="Rscript code/01_build/04_mapbiomas/4_deforestation_stats.R"
# ---------------------------------------------------------------------------

suppressMessages({
  library(here)
  library(terra)
})

PIXEL_HA <- 0.09   # 30 m x 30 m = 900 m^2 = 0.09 ha (preserved from legacy)

parse_range <- function(s) {
  s <- trimws(s)
  if (grepl(":", s)) { p <- as.numeric(strsplit(s, ":")[[1]]); seq(p[1], p[2]) } else as.numeric(s)
}

combined_dir <- Sys.getenv("MB_COMBINED_DIR", unset = here("data", "intermediate", "mapbiomas", "transitions_combined"))
years    <- parse_range(Sys.getenv("MB_YEARS", unset = "1987:2020"))
stats_out <- Sys.getenv("MB_STATS_OUT", unset = here("output", "tables", "mapbiomas_biome_deforestation.csv"))

log_msg <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))

if (!dir.exists(combined_dir)) stop("Transitions directory not found: ", combined_dir)
dir.create(dirname(stats_out), recursive = TRUE, showWarnings = FALSE)

log_msg("combined_dir = ", combined_dir)
log_msg("years = ", paste(range(years), collapse = ".."))

# Count pixels of a given class across one raster via a frequency table.
class_count <- function(freq_df, value) {
  hit <- freq_df$count[freq_df$value == value]
  if (length(hit)) sum(hit) else 0
}

rows <- list()
for (year in years) {
  tifs <- list.files(combined_dir, pattern = sprintf("_%s\\.tif$", year), full.names = TRUE)
  if (length(tifs) == 0L) { log_msg("year ", year, ": no rasters, skipping"); next }

  n_pixels <- 0; forest <- 0; deforested <- 0; reforested <- 0
  for (tif in tifs) {
    r  <- terra::rast(tif)
    fr <- as.data.frame(terra::freq(r))
    n_pixels   <- n_pixels   + terra::ncell(r)
    forest     <- forest     + class_count(fr, 1)
    deforested <- deforested + class_count(fr, 2)
    reforested <- reforested + class_count(fr, 3)
  }

  rows[[as.character(year)]] <- data.frame(
    year               = year,
    n_pixels           = n_pixels,
    forest_area_ha     = forest     * PIXEL_HA,
    deforested_area_ha = deforested * PIXEL_HA,
    reforested_area_ha = reforested * PIXEL_HA
  )
  log_msg("year ", year, ": ", length(tifs), " tiles | forest ",
          round(forest * PIXEL_HA), " ha, deforested ", round(deforested * PIXEL_HA), " ha")
}

if (length(rows) == 0L) stop("No years produced any statistics.")

out <- do.call(rbind, rows)
write.csv(out, stats_out, row.names = FALSE)
log_msg("DONE. wrote ", nrow(out), " rows -> ", stats_out)
