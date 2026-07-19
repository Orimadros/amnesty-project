# render_demo_map.R — render the Rondonia deforestation demo as a figure.
suppressMessages(library(terra))

D <- "data/intermediate/demo_rondonia/transitions_combined"
out <- "output/figures/rondonia_deforestation_1987_vs_1996.png"
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

# Merge the per-tile rasters for a year, aggregating each tile first (lighter).
get_year <- function(y) {
  fs <- list.files(D, pattern = sprintf("_%d\\.tif$", y), full.names = TRUE)
  rs <- lapply(fs, function(f) terra::aggregate(terra::rast(f), fact = 4, fun = "modal"))
  terra::merge(terra::sprc(rs))
}

r87 <- get_year(1987)
r96 <- get_year(1996)

cols <- c("grey85", "#1a9850", "#d73027", "#4575b4")   # 0 other, 1 forest, 2 deforested, 3 reforested
labs <- c("other", "forest", "deforested", "reforested")
brk  <- c(-0.5, 0.5, 1.5, 2.5, 3.5)

png(out, width = 1700, height = 900, res = 110)
par(mfrow = c(1, 2), mar = c(2, 2, 4, 1), oma = c(3, 0, 3, 0), xpd = NA)
plot(r87, col = cols, breaks = brk, legend = FALSE, axes = FALSE, mar = NA, main = "1987")
plot(r96, col = cols, breaks = brk, legend = FALSE, axes = FALSE, mar = NA, main = "1996")
mtext("Rondonia deforestation, 1987 -> 1996 (migrated MapBiomas pipeline)",
      outer = TRUE, cex = 1.4, font = 2, line = 0.5)
# Shared legend along the bottom, outside the panels.
par(fig = c(0, 1, 0, 1), new = TRUE, mar = c(0, 0, 0, 0))
plot.new()
legend("bottom", legend = labs, fill = cols, horiz = TRUE, bty = "n", cex = 1.1, inset = 0.01)
dev.off()

cat("wrote ", out, "\n")
