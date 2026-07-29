# 0_tile_rasters_to_grids.R
# ---------------------------------------------------------------------------
# Step 0 of the MapBiomas backbone: crop each annual land-cover raster to the
# Amazon biome and chop it into 1-degree grid tiles saved as .rds data frames.
#
# Migrated from legacy_repo/code/1_mapbiomas.R (tiling blocks, lines ~47-85 and
# ~308-346). The tiling LOGIC (extents, the 0.00009 nudge, output naming) is
# preserved exactly. Only structure changed: here() paths, explicit I/O, a
# testable scope block, progress logging, and skip-if-exists resumability.
#
# --- TESTABILITY -----------------------------------------------------------
# Every input/output path and the processing scope can be overridden with
# environment variables, so the same script runs on the full 44 GB OR on a
# tiny cropped fixture in seconds. Defaults = full production run.
#
#   MB_INPUT_DIR   dir holding brasil_coverage_<year>.tif   (default data/input/mapbiomas)
#   MB_OUTPUT_DIR  dir to write grid_*.rds into             (default data/intermediate/mapbiomas/grids)
#   MB_BORDER_SHP  Amazon biome border shapefile            (default data/input/aux/...)
#   MB_YEARS       years to process, "a:b" or "a"           (default 1985:2022)
#   MB_H_RANGE     horizontal tile degrees, "a:b"           (default -74:-44)
#   MB_V_RANGE     vertical tile degrees, "a:b"             (default -17:5)
#
# Run full (in container):
#   make docker-run CMD="Rscript code/01_build/04_mapbiomas/0_tile_rasters_to_grids.R"
# ---------------------------------------------------------------------------

suppressMessages({
  library(here)
  library(terra)
  library(sf)
})

# --- config (env-overridable) ----------------------------------------------
parse_range <- function(s) {
  s <- trimws(s)
  if (grepl(":", s)) {
    parts <- as.numeric(strsplit(s, ":")[[1]])
    seq(parts[1], parts[2])
  } else {
    as.numeric(s)
  }
}

input_dir  <- Sys.getenv("MB_INPUT_DIR",  unset = here("data", "input", "mapbiomas"))
output_dir <- Sys.getenv("MB_OUTPUT_DIR", unset = here("data", "intermediate", "mapbiomas", "grids"))
border_shp <- Sys.getenv("MB_BORDER_SHP", unset = here("data", "input", "aux", "amazon_biome_border", "amazon_biome_border.shp"))
years   <- parse_range(Sys.getenv("MB_YEARS",   unset = "1985:2022"))
h_range <- parse_range(Sys.getenv("MB_H_RANGE", unset = "-74:-44"))
v_range <- parse_range(Sys.getenv("MB_V_RANGE", unset = "-17:5"))

# --- fail fast: validate inputs before any heavy work ----------------------
log_msg <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))

if (!file.exists(border_shp)) stop("Biome border shapefile not found: ", border_shp)
if (!dir.exists(input_dir))   stop("Input raster directory not found: ", input_dir)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

log_msg("input_dir  = ", input_dir)
log_msg("output_dir = ", output_dir)
log_msg("years  = ", paste(range(years), collapse = ".."),
        " | h = ", paste(range(h_range), collapse = ".."),
        " | v = ", paste(range(v_range), collapse = ".."))

amazon_border <- sf::read_sf(border_shp)

# --- main loop -------------------------------------------------------------
n_written <- 0L
n_skipped <- 0L
n_empty   <- 0L

for (year in years) {
  tif <- file.path(input_dir, paste0("brasil_coverage_", year, ".tif"))
  if (!file.exists(tif)) { log_msg("SKIP year ", year, " (missing ", basename(tif), ")"); next }

  log_msg("year ", year, ": loading + cropping to biome ...")
  grey <- terra::rast(tif)
  border_proj <- sf::st_transform(amazon_border, terra::crs(grey))
  grey <- terra::crop(grey, border_proj)

  for (h in h_range) {
    t0 <- Sys.time()
    for (v in v_range) {
      vv <- if (v < 0) v + 0.00009 else v - 0.00009           # legacy nudge, preserved
      e  <- terra::ext(h + 0.00009, h + 1, vv, v + 1)

      out_file <- file.path(output_dir, sprintf("grid_%s_%s_%s_%s_%s.rds", h, h + 1, v, v + 1, year))
      if (file.exists(out_file)) { n_skipped <- n_skipped + 1L; next }   # resumable

      tile_df <- as.data.frame(terra::crop(grey, e), xy = TRUE)
      if (nrow(tile_df) == 0L) { n_empty <- n_empty + 1L; next }         # no data in this tile

      saveRDS(tile_df, out_file)
      n_written <- n_written + 1L
    }
    log_msg("  year ", year, " h=", h, " done (", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s)")
  }
}

log_msg("DONE. written=", n_written, " skipped(existing)=", n_skipped, " empty=", n_empty)
