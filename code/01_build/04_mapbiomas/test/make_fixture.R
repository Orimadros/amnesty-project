# make_fixture.R
# ---------------------------------------------------------------------------
# One-time helper: crop a single full MapBiomas raster down to a tiny
# central-Amazon window so the tiling step can be tested end-to-end in seconds
# instead of on the full ~941 MB file. Output goes under data/input/_test/
# (which is inside the gitignored data/ tree).
#
# Run (in container):
#   make docker-run CMD="Rscript code/01_build/04_mapbiomas/test/make_fixture.R"
# ---------------------------------------------------------------------------

suppressMessages({ library(here); library(terra) })

src <- here("data", "input", "mapbiomas", "brasil_coverage_1985.tif")
if (!file.exists(src)) stop("Source raster not found: ", src)

# Small window inside the Amazon biome (lon -63..-61, lat -6..-4) -> ~4 tiles.
window <- terra::ext(-63, -61, -6, -4)

out_dir <- here("data", "input", "_test", "mapbiomas")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out <- file.path(out_dir, "brasil_coverage_1985.tif")

r <- terra::rast(src)
r <- terra::crop(r, window)
terra::writeRaster(r, out, filetype = "GTiff", overwrite = TRUE)

cat(sprintf("fixture written: %s  (%d x %d cells)\n", out, nrow(r), ncol(r)))
