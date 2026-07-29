# Stage 1 of the empirics chain: per-parcel deforestation for a given year.
#
# For every CAR parcel in the target areas (the pool the paper's eligible /
# ineligible split is drawn from) and in the never-eligible control areas, count
# deforested pixels against the MapBiomas transition raster for one year.
#
# Raster coding (from code/01_build/04_mapbiomas/3_compute_transitions.R):
#   0 = other/outside, 1 = forest, 2 = deforested, 3 = reforested
# Legacy (2_empirics.R:629-634) computes:
#   deforested_area_hc = count(value == 2) * 0.09      # 30m pixel = 0.09 ha
#   deforestation_rate = count(value == 2) / count(value != 0) * 100
# We store the raw pixel counts and derive both downstream, so the pixel-area
# assumption stays visible and changeable.
#
# Execution: one output file per raster tile, skip-if-exists, so the run is
# resumable and sharded the same way the CAR stages are. Parcels straddling a
# tile boundary get partial counts from each tile; the tiles are exact,
# non-overlapping 1-degree grids, so summing across tiles is exact.
#
# Env:
#   EMP_YEAR   year to process (default 2004)
#   EMP_TILES  optional file with one tile basename per line (worker sharding)

library(terra)
library(sf)
library(dplyr)
library(data.table)
library(here)

sf_use_s2(FALSE) # planar bbox prefiltering only; no constructive geometry here

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) stop("Failed to create directory: ", path)
  invisible(path)
}

YEAR <- as.integer(Sys.getenv("EMP_YEAR", unset = "2004"))

tile_dir <- here("data", "intermediate", "mapbiomas", "transitions_combined")
out_dir <- here("data", "intermediate", "empirics", "defo_tiles", as.character(YEAR))
ensure_dir(out_dir)

car_files <- c(
  target = here("data", "intermediate", "car", "car_eligible_cleaned.shp"),
  never_eligible = here("data", "intermediate", "car", "car_ineligible_cleaned.shp")
)
absent <- car_files[!file.exists(unlist(car_files))]
if (length(absent) > 0) {
  stop("Missing CAR layer(s):\n", paste0(" - ", absent, collapse = "\n"))
}

message("=== parcel deforestation, year ", YEAR, " ===")

# ---- 1) Parcels, transformed ONCE --------------------------------------------
load_layer <- function(path, grp) {
  x <- st_read(path, quiet = TRUE)
  idc <- intersect(c("COD_IMO", "COD_IMOVEL", "cod_imovel"), names(x))[1]
  if (is.na(idc)) stop("No CAR id column in ", path)
  x %>%
    transmute(
      car_id = as.character(.data[[idc]]),
      area_ha = suppressWarnings(as.numeric(.data[["NUM_ARE"]])),
      group = grp
    )
}

parcels <- bind_rows(
  load_layer(car_files[["target"]], "target"),
  load_layer(car_files[["never_eligible"]], "never_eligible")
) %>%
  st_transform(4326)

message("parcels: ", nrow(parcels),
        " (target ", sum(parcels$group == "target"),
        ", never_eligible ", sum(parcels$group == "never_eligible"), ")")

pv <- vect(parcels)

# Per-parcel bounding boxes, vectorised. Subsetting a SpatVector once per feature
# (ext(pv[i, ]) in a loop) is O(n) per call and grinds to a halt at ~177k
# features; geom() returns every vertex tagged with its feature id in one go, so
# the bboxes fall out of a single grouped min/max.
pbb <- as.data.table(geom(pv))[, .(
  xmin = min(x), xmax = max(x),
  ymin = min(y), ymax = max(y)
), by = geom]
setorder(pbb, geom)
pbb[, idx := geom]

# ---- 2) Tiles ----------------------------------------------------------------
tiles <- list.files(tile_dir, pattern = paste0("_", YEAR, "\\.tif$"))
if (length(tiles) == 0) stop("No tiles found for year ", YEAR, " in ", tile_dir)

shard <- Sys.getenv("EMP_TILES", unset = "")
if (nzchar(shard)) {
  keep <- readLines(shard)
  tiles <- tiles[sub("\\.tif$", "", tiles) %in% keep]
  message("worker shard: ", length(tiles), " tiles")
}

message("tiles: ", length(tiles))

# ---- 3) Per-tile extraction --------------------------------------------------
t_start <- Sys.time()
done <- 0L

for (tf in tiles) {
  base <- sub("\\.tif$", "", tf)
  out_f <- file.path(out_dir, paste0(base, ".rds"))
  if (file.exists(out_f)) { done <- done + 1L; next }

  r <- rast(file.path(tile_dir, tf))
  e <- as.vector(ext(r))

  # cheap bbox prefilter before any raster work
  hit <- pbb[xmax >= e[1] & xmin <= e[2] & ymax >= e[3] & ymin <= e[4], idx]

  if (length(hit) == 0) {
    saveRDS(data.table(car_id = character(), defor_px = integer(), valid_px = integer()), out_f)
    done <- done + 1L
    next
  }

  sel <- pv[hit, ]
  ex <- terra::extract(r, sel)
  setDT(ex)
  setnames(ex, 2, "val")

  agg <- ex[, .(
    defor_px = sum(val == 2L, na.rm = TRUE),
    valid_px = sum(val != 0L, na.rm = TRUE)
  ), by = ID]
  agg[, car_id := parcels$car_id[hit[ID]]]
  agg <- agg[defor_px > 0 | valid_px > 0, .(car_id, defor_px, valid_px)]

  saveRDS(agg, out_f)
  done <- done + 1L

  if (done %% 25 == 0) {
    el <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
    message(sprintf("  %d/%d tiles | %.1f min elapsed | ~%.1f min left",
                    done, length(tiles), el,
                    el / done * (length(tiles) - done)))
  }
}

message("all tiles done in ", round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), " min")

# ---- 4) Combine --------------------------------------------------------------
files <- list.files(out_dir, pattern = "\\.rds$", full.names = TRUE)
all <- rbindlist(lapply(files, readRDS), fill = TRUE)

if (nrow(all) == 0) stop("No extraction results produced for year ", YEAR)

# A parcel spanning several tiles contributes one row per tile; sum them.
tot <- all[, .(defor_px = sum(defor_px), valid_px = sum(valid_px)), by = car_id]

meta <- as.data.table(st_drop_geometry(parcels))
out <- merge(meta, tot, by = "car_id", all.x = TRUE)
out[is.na(defor_px), defor_px := 0L]
out[is.na(valid_px), valid_px := 0L]
out[, `:=`(
  year = YEAR,
  deforested_area_ha = defor_px * 0.09,
  deforestation_rate = fifelse(valid_px > 0, defor_px / valid_px * 100, NA_real_)
)]

out_dir2 <- here("data", "intermediate", "empirics")
ensure_dir(out_dir2)
out_f <- file.path(out_dir2, paste0("parcel_defo_", YEAR, ".csv"))
fwrite(out, out_f)

message("Wrote: ", out_f)
message("  parcels with any raster coverage: ", sum(out$valid_px > 0), " / ", nrow(out))
message("  total deforested area (ha): ", format(sum(out$deforested_area_ha), big.mark = ","))
print(out[, .(
  parcels = .N,
  mean_rate = round(mean(deforestation_rate, na.rm = TRUE), 2),
  total_defor_Mha = round(sum(deforested_area_ha) / 1e6, 3)
), by = group])
