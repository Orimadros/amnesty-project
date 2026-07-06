# 3_compute_transitions.R
# ---------------------------------------------------------------------------
# Step 3 of the MapBiomas backbone (the core): for each grid tile, stack the
# legacy-forest baseline and the per-year cover into one matrix, then apply the
# deforestation/reforestation rule kernel (aux/deforestation_rules.cpp) to
# resolve each pixel's forest -> deforested -> reforested trajectory. Writes one
# raster per year (1=forest, 2=deforested, 3=reforested, 0=other) plus a
# per-tile record of how many times each pixel's cover changed ("noisiness").
#
# Migrated from legacy_repo/code/1_mapbiomas.R (lines ~412-479). Preserved
# verbatim: the column order (legacy first, then years), the seeding step
# (legacy forest -> human in the first year = deforested), the C++ rule call,
# the back-to-back change count, and the 1/2/3/0 value coding.
#
# DELIBERATE ROBUSTNESS CHANGE (flagged): legacy stacked the per-year tiles with
# positional cbind, trusting every tile to have identical pixel order. Here we
# INNER-JOIN on (x, y) instead, so pixels are matched by coordinate. Same result
# when the legacy assumption holds, but correct (not silently misaligned) if a
# tile is missing pixels. No classification/rule logic is changed.
#
# Reads  : legacy grid_<base>_legacy.rds ; cover grid_<base>_<year>_transitions.rds
# Writes : per year grid_<base>_<year>.tif ; per tile grid_<base>_cover.rds
#
# --- TESTABILITY -----------------------------------------------------------
#   MB_LEGACY_DIR     legacy tiles     (default data/intermediate/mapbiomas/legacy)
#   MB_COVER_DIR      per-year cover   (default data/intermediate/mapbiomas/transitions)
#   MB_COMBINED_DIR   output rasters   (default data/intermediate/mapbiomas/transitions_combined)
#   MB_YEARS          years to stack   (default 1987:2020)
#
# Run full (in container):
#   make docker-run CMD="Rscript code/01_build/04_mapbiomas/3_compute_transitions.R"
# ---------------------------------------------------------------------------

suppressMessages({
  library(here)
  library(data.table)
  library(terra)
  library(Rcpp)
})

sourceCpp(here("code", "01_build", "04_mapbiomas", "aux", "deforestation_rules.cpp"))

parse_range <- function(s) {
  s <- trimws(s)
  if (grepl(":", s)) { p <- as.numeric(strsplit(s, ":")[[1]]); seq(p[1], p[2]) } else as.numeric(s)
}

legacy_dir   <- Sys.getenv("MB_LEGACY_DIR",   unset = here("data", "intermediate", "mapbiomas", "legacy"))
cover_dir    <- Sys.getenv("MB_COVER_DIR",     unset = here("data", "intermediate", "mapbiomas", "transitions"))
combined_dir <- Sys.getenv("MB_COMBINED_DIR",  unset = here("data", "intermediate", "mapbiomas", "transitions_combined"))
years <- parse_range(Sys.getenv("MB_YEARS", unset = "1987:2020"))

log_msg <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))

if (!dir.exists(legacy_dir)) stop("Legacy directory not found: ", legacy_dir)
if (!dir.exists(cover_dir))  stop("Cover directory not found: ", cover_dir)
dir.create(combined_dir, recursive = TRUE, showWarnings = FALSE)

# Coerce coordinate keys to character so joins match exactly (matches legacy).
key_char <- function(dt) { dt[, `:=`(x = as.character(x), y = as.character(y))]; dt }

# Tiles that have a legacy baseline AND every requested year's cover.
bases <- sub("^grid_", "", sub("_legacy\\.rds$", "", list.files(legacy_dir, pattern = "_legacy\\.rds$")))
has_all_years <- function(base) all(file.exists(
  file.path(cover_dir, sprintf("grid_%s_%s_transitions.rds", base, years))
))
bases <- Filter(has_all_years, bases)

log_msg("legacy_dir   = ", legacy_dir)
log_msg("cover_dir    = ", cover_dir)
log_msg("combined_dir = ", combined_dir)
log_msg("years = ", paste(range(years), collapse = ".."), " | tiles ready: ", length(bases))
if (length(bases) == 0L) stop("No tiles have both a legacy baseline and all requested years.")

n_done <- 0L
n_skipped <- 0L

for (base in bases) {
  marker <- file.path(combined_dir, sprintf("grid_%s_cover.rds", base))
  if (file.exists(marker)) { n_skipped <- n_skipped + 1L; next }   # resumable
  t0 <- Sys.time()

  # Stack: legacy baseline first, then each year's cover, joined on (x, y).
  dt <- key_char(as.data.table(readRDS(file.path(legacy_dir, sprintf("grid_%s_legacy.rds", base)))))
  setnames(dt, "cover", "cov_legacy")
  for (yr in years) {
    cov <- key_char(as.data.table(readRDS(file.path(cover_dir, sprintf("grid_%s_%s_transitions.rds", base, yr)))))
    setnames(cov, "cover", paste0("cov_", yr))
    dt <- merge(dt, cov, by = c("x", "y"))
  }

  # Build the year-by-year matrix: column 1 = legacy, then years in order.
  cover_cols <- c("cov_legacy", paste0("cov_", years))
  tm <- as.matrix(dt[, ..cover_cols])

  # Back-to-back change count on the RAW sequence (before the rules are applied).
  n_change <- rowSums(tm[, -ncol(tm), drop = FALSE] != tm[, -1, drop = FALSE])

  # Seed the first transition, then apply the rule kernel across each row.
  tm[, 2] <- ifelse(tm[, 1] == "forest" & tm[, 2] == "human", "deforested", tm[, 2])
  tm <- apply_deforestation_rules_cpp(tm)

  # Write one raster per year (1 forest, 2 deforested, 3 reforested, 0 other).
  xs <- as.numeric(dt$x); ys <- as.numeric(dt$y)
  for (idx in seq_along(years)) {
    lab <- tm[, idx + 1L]   # +1: skip the legacy column
    val <- data.table::fifelse(lab == "forest", 1L,
           data.table::fifelse(lab == "deforested", 2L,
           data.table::fifelse(lab == "reforested", 3L, 0L)))
    r <- terra::rast(data.frame(x = xs, y = ys, value = val), type = "xyz", crs = "EPSG:4326")
    terra::writeRaster(r, file.path(combined_dir, sprintf("grid_%s_%s.tif", base, years[idx])),
                       filetype = "GTiff", overwrite = TRUE)
  }

  saveRDS(data.frame(x = dt$x, y = dt$y, n_pixel_change_back_to_back = n_change), marker)
  n_done <- n_done + 1L
  log_msg("  tile ", base, ": ", nrow(dt), " px, ", length(years), " year rasters (",
          round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s)")

  # Release this tile's large objects before the next tile (the per-tile stack
  # is memory-heavy; without this, tiles accumulate and OOM). Matches legacy gc.
  rm(dt, tm, n_change, xs, ys); gc(verbose = FALSE)
}

log_msg("DONE. tiles=", n_done, " skipped(existing)=", n_skipped)
