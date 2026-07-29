# 1_build_legacy_forest.R
# ---------------------------------------------------------------------------
# Step 1 of the MapBiomas backbone: build the "legacy forest" baseline. A pixel
# is legacy forest only if its land cover was forest in BOTH 1985 and 1986.
# This baseline is the reference against which later deforestation/reforestation
# transitions are measured.
#
# Migrated from legacy_repo/code/1_mapbiomas.R (lines ~90-108). The forest-code
# set and the "forest in both years" rule are preserved exactly. Only structure
# changed: here() paths, explicit I/O, testable scope, progress logging,
# skip-if-exists resumability.
#
# Reads  : per-tile grids  grid_<base>_1985.rds and grid_<base>_1986.rds
# Writes : per-tile legacy grid_<base>_legacy.rds  (columns x, y, cover)
#
# --- TESTABILITY -----------------------------------------------------------
#   MB_GRIDS_DIR   dir with grid_*.rds        (default data/intermediate/mapbiomas/grids)
#   MB_LEGACY_DIR  dir to write legacy .rds   (default data/intermediate/mapbiomas/legacy)
#
# Run full (in container):
#   make docker-run CMD="Rscript code/01_build/04_mapbiomas/1_build_legacy_forest.R"
# ---------------------------------------------------------------------------

suppressMessages({
  library(here)
  library(data.table)
})

# MapBiomas land-cover codes counted as "forest" (preserved verbatim from legacy).
FOREST_CODES <- c(1, 3, 4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 13)

grids_dir  <- Sys.getenv("MB_GRIDS_DIR",  unset = here("data", "intermediate", "mapbiomas", "grids"))
legacy_dir <- Sys.getenv("MB_LEGACY_DIR", unset = here("data", "intermediate", "mapbiomas", "legacy"))

log_msg <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))

if (!dir.exists(grids_dir)) stop("Grids directory not found: ", grids_dir)
dir.create(legacy_dir, recursive = TRUE, showWarnings = FALSE)

# Tile "base" = the h_h1_v_v1 part shared by a tile's 1985 and 1986 files.
base_1985 <- sub("^grid_", "", sub("_1985\\.rds$", "", list.files(grids_dir, pattern = "_1985\\.rds$")))
base_1986 <- sub("^grid_", "", sub("_1986\\.rds$", "", list.files(grids_dir, pattern = "_1986\\.rds$")))
bases <- intersect(base_1985, base_1986)

log_msg("grids_dir  = ", grids_dir)
log_msg("legacy_dir = ", legacy_dir)
log_msg("tiles with both 1985 & 1986: ", length(bases))
if (length(bases) == 0L) stop("No tiles have both 1985 and 1986 grids to compare.")

n_written <- 0L
n_skipped <- 0L

for (base in bases) {
  out_file <- file.path(legacy_dir, sprintf("grid_%s_legacy.rds", base))
  if (file.exists(out_file)) { n_skipped <- n_skipped + 1L; next }   # resumable

  a <- as.data.table(readRDS(file.path(grids_dir, sprintf("grid_%s_1985.rds", base))))
  b <- as.data.table(readRDS(file.path(grids_dir, sprintf("grid_%s_1986.rds", base))))

  # Join on pixel coordinates; coerce to character so float keys match exactly
  # (matches legacy behaviour).
  setnames(a, 3, "cov_1985"); setnames(b, 3, "cov_1986")
  a[, `:=`(x = as.character(x), y = as.character(y))]
  b[, `:=`(x = as.character(x), y = as.character(y))]
  res <- a[b, on = .(x, y), nomatch = 0L]

  res[, cover := fifelse(cov_1985 %in% FOREST_CODES & cov_1986 %in% FOREST_CODES,
                         "forest", "not forest")]

  saveRDS(res[, .(x, y, cover)], out_file)
  n_written <- n_written + 1L
  log_msg("  tile ", base, ": ", nrow(res), " px (",
          sum(res$cover == "forest"), " forest)")
}

log_msg("DONE. written=", n_written, " skipped(existing)=", n_skipped)
