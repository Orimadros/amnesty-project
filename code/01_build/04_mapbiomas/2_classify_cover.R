# 2_classify_cover.R
# ---------------------------------------------------------------------------
# Step 2 of the MapBiomas backbone: for each year, label every pixel as
# "forest", "human", or "" (anything else, e.g. water). This per-year cover is
# what the transition step compares across time to detect deforestation and
# reforestation.
#
# Migrated from legacy_repo/code/1_mapbiomas.R (lines ~384-409). The forest and
# human code sets and the classification rule are preserved verbatim. Only
# structure changed: here() paths, explicit I/O, testable scope, progress
# logging, skip-if-exists resumability.
#
# Reads  : per-tile grids  grid_<base>_<year>.rds
# Writes : per-tile cover  grid_<base>_<year>_transitions.rds  (columns x, y, cover)
#
# --- TESTABILITY -----------------------------------------------------------
#   MB_GRIDS_DIR        dir with grid_*.rds        (default data/intermediate/mapbiomas/grids)
#   MB_TRANSITIONS_DIR  dir to write cover .rds     (default data/intermediate/mapbiomas/transitions)
#   MB_YEARS            years to classify, "a:b"    (default 1987:2020)
#
# Run full (in container):
#   make docker-run CMD="Rscript code/01_build/04_mapbiomas/2_classify_cover.R"
# ---------------------------------------------------------------------------

suppressMessages({
  library(here)
  library(data.table)
})

# Land-cover code sets, preserved verbatim from legacy.
FOREST_CODES <- c(1, 3, 4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 13)
HUMAN_CODES  <- c(14, 15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21, 24, 30)

parse_range <- function(s) {
  s <- trimws(s)
  if (grepl(":", s)) { p <- as.numeric(strsplit(s, ":")[[1]]); seq(p[1], p[2]) } else as.numeric(s)
}

grids_dir       <- Sys.getenv("MB_GRIDS_DIR",       unset = here("data", "intermediate", "mapbiomas", "grids"))
transitions_dir <- Sys.getenv("MB_TRANSITIONS_DIR", unset = here("data", "intermediate", "mapbiomas", "transitions"))
years <- parse_range(Sys.getenv("MB_YEARS", unset = "1987:2020"))

log_msg <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))

if (!dir.exists(grids_dir)) stop("Grids directory not found: ", grids_dir)
dir.create(transitions_dir, recursive = TRUE, showWarnings = FALSE)

log_msg("grids_dir       = ", grids_dir)
log_msg("transitions_dir = ", transitions_dir)
log_msg("years = ", paste(range(years), collapse = ".."))

n_written <- 0L
n_skipped <- 0L

for (year in years) {
  files <- list.files(grids_dir, pattern = sprintf("_%s\\.rds$", year))
  if (length(files) == 0L) { log_msg("year ", year, ": no grids, skipping"); next }

  t0 <- Sys.time()
  for (f in files) {
    base <- sub("^grid_", "", sub(sprintf("_%s\\.rds$", year), "", f))
    out_file <- file.path(transitions_dir, sprintf("grid_%s_%s_transitions.rds", base, year))
    if (file.exists(out_file)) { n_skipped <- n_skipped + 1L; next }   # resumable

    a <- as.data.table(readRDS(file.path(grids_dir, f)))
    setnames(a, 3, "code")
    a[, cover := ""]
    a[code %in% FOREST_CODES, cover := "forest"]
    a[code %in% HUMAN_CODES,  cover := "human"]

    saveRDS(a[, .(x, y, cover)], out_file)
    n_written <- n_written + 1L
  }
  log_msg("year ", year, ": ", length(files), " tiles (",
          round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s)")
}

log_msg("DONE. written=", n_written, " skipped(existing)=", n_skipped)
