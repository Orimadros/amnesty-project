# Stage 8 (diagnostic): TEST F3 from docs/notes/paper_legacy_method_diffs.md.
#
# Legacy cleans the reserve (never-eligible) pool with its OWN algorithm
# (2_empirics.R:1852-1955), not the Appendix C 5-rule one:
#   per municipality, dedupe COD_IMO; for every pair overlapping >10% of i's
#   declared NUM_ARE, erase the intersection from the LARGER parcel (by declared
#   area), sequentially on evolving geometry; parcels are never dropped.
# The per-year panel is then measured on these erased geometries.
#
# Here: apply that algorithm to our in-sample-2019 control parcels (the F2 sample,
# table1_testF2 companion), re-measure rates/areas on the cleaned geometry for
# 2005-2008 (+2014 for totals), and compare against the paper's never-eligible
# column (7,049 / 760 ha / 35.7% / 2.0->2.2 Mha).
#
# Faithfulness notes:
# - pair detection on ORIGINAL geometries, pct threshold vs i's declared NUM_ARE,
#   erasure sequential over the pair list on current state (as legacy's for-loop);
# - empty or failed erasures keep the original geometry (legacy's try/length guards);
# - GEOMETRYCOLLECTIONs get polygons extracted, LINESTRINGs dropped (legacy :1936-55);
# - planar GEOS; areas in EPSG:5880 (legacy used raw st_area; scale is what matters).

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(data.table)
  library(here)
})

sf_use_s2(FALSE)

emp_dir <- here("data", "intermediate", "empirics")
tile_dir <- here("data", "intermediate", "mapbiomas", "transitions_combined")
YEARS <- c(2005:2008, 2014)
CRS_EQ <- 5880

# ---- the F2 never-eligible sample ---------------------------------------------
d19 <- fread(file.path(emp_dir, "parcel_defo_2019.csv"))[, .(car_id, rate_2019 = deforestation_rate)]
elig <- fread(file.path(emp_dir, "parcel_eligibility.csv"))
elig <- merge(elig, d19, by = "car_id", all.x = TRUE)
keep <- elig[class == "never_eligible" & !is.na(rate_2019) & rate_2019 > 10, car_id]
message("in-sample-2019 never-eligible parcels: ", length(keep))

shp <- st_read(here("data", "intermediate", "car", "car_ineligible_cleaned.shp"), quiet = TRUE)
shp$car_id <- as.character(shp$car_id)
shp <- shp[shp$car_id %in% keep, c("car_id", "NUM_ARE")]
shp$NUM_ARE <- suppressWarnings(as.numeric(shp$NUM_ARE))
shp <- st_make_valid(st_transform(shp, 4326))
message("geometries: ", nrow(shp))

st_erase <- function(x, y) suppressWarnings(st_difference(x, st_union(st_geometry(y))))
muni_of <- function(x) sub("^[A-Z]{2}-([0-9]+)-.*$", "\\1", x)
shp$muni <- muni_of(shp$car_id)

# ---- legacy reserve cleaning, per municipality ---------------------------------
cleaned <- vector("list", length(unique(shp$muni)))
mi <- 0L
for (m in sort(unique(shp$muni))) {
  mi <- mi + 1L
  car <- shp[shp$muni == m, ]
  car <- car[!duplicated(car$car_id), ]
  if (nrow(car) > 1) {
    idx <- st_intersects(car, car)
    pr <- list()
    for (i in seq_len(nrow(car))) {
      js <- setdiff(idx[[i]], i)
      if (length(js) == 0) next
      inter <- suppressWarnings(st_intersection(st_geometry(car[i, ]), st_geometry(car[js, ])))
      ha <- as.numeric(st_area(st_transform(st_sfc(inter, crs = 4326), CRS_EQ))) / 1e4
      ok <- which(ha / car$NUM_ARE[i] > 0.1)
      if (length(ok) > 0) {
        pr[[length(pr) + 1L]] <- data.table(
          a = car$car_id[i], b = car$car_id[js[ok]],
          num_a = car$NUM_ARE[i], num_b = car$NUM_ARE[js[ok]])
      }
    }
    pairs <- if (length(pr)) rbindlist(pr) else data.table()
    if (nrow(pairs) > 0) {
      for (r in seq_len(nrow(pairs))) {
        big <- if (pairs$num_a[r] >= pairs$num_b[r]) pairs$a[r] else pairs$b[r]
        sml <- if (pairs$num_a[r] >= pairs$num_b[r]) pairs$b[r] else pairs$a[r]
        gi <- which(car$car_id == big); gj <- which(car$car_id == sml)
        if (length(gi) == 0 || length(gj) == 0) next
        res <- try(st_erase(car[gi, ], car[gj, ]), silent = TRUE)
        if (!inherits(res, "try-error") && nrow(res) > 0 &&
            !all(st_is_empty(res))) {
          st_geometry(car)[gi] <- st_geometry(res)[1]
        }
      }
    }
  }
  gt <- st_geometry_type(car)
  if (any(gt == "GEOMETRYCOLLECTION")) {
    for (k in which(gt == "GEOMETRYCOLLECTION")) {
      ext <- try(st_collection_extract(car[k, ], "POLYGON"), silent = TRUE)
      if (!inherits(ext, "try-error") && nrow(ext) > 0)
        st_geometry(car)[k] <- st_union(st_geometry(ext))
    }
  }
  gt <- st_geometry_type(car)
  car <- car[!gt %in% c("LINESTRING", "MULTILINESTRING", "POINT", "MULTIPOINT"), ]
  cleaned[[mi]] <- car
  if (mi %% 25 == 0) message("  munis done: ", mi)
}
ctl <- do.call(rbind, cleaned)
ctl <- st_make_valid(ctl)
message("cleaned parcels: ", nrow(ctl), " (started ", nrow(shp), ")")
ctl$geom_ha <- as.numeric(st_area(st_transform(ctl, CRS_EQ))) / 1e4

# ---- re-measure on cleaned geometry, per tile ----------------------------------
pv <- vect(ctl)
bb <- data.table(id = seq_len(nrow(ctl)), do.call(rbind, lapply(st_geometry(ctl), function(g) st_bbox(g))))
setnames(bb, c("id", "xmin", "ymin", "xmax", "ymax"))

acc <- CJ(car_id = ctl$car_id, year = YEARS)[, `:=`(defor_px = 0, valid_px = 0)]
setkey(acc, car_id, year)

for (yr in YEARS) {
  tiles <- list.files(tile_dir, pattern = paste0("_", yr, "\\.tif$"), full.names = TRUE)
  message("year ", yr, ": ", length(tiles), " tiles")
  tdone <- 0L
  for (tf in tiles) {
    r <- rast(tf)
    e <- ext(r)
    hit <- bb[xmin < e$xmax & xmax > e$xmin & ymin < e$ymax & ymax > e$ymin, id]
    if (length(hit) == 0) next
    exv <- terra::extract(r, pv[hit, ], touches = FALSE)
    names(exv)[2] <- "v"
    agg <- as.data.table(exv)[, .(d = sum(v == 2, na.rm = TRUE),
                                  n = sum(v != 0 & !is.na(v))), by = ID]
    agg[, car_id := ctl$car_id[hit[ID]]]
    acc[.(agg$car_id, yr), `:=`(defor_px = defor_px + agg$d,
                                valid_px = valid_px + agg$n)]
    tdone <- tdone + 1L
    if (tdone %% 50 == 0) message("  ", tdone, " tiles with hits processed")
  }
}
acc[, rate := fifelse(valid_px > 0, defor_px / valid_px * 100, NA_real_)]
acc[, defor_ha := defor_px * 0.09]
acc[, lf_ha := valid_px * 0.09]

fwrite(acc, file.path(emp_dir, "control_reserve_cleaned_panel.csv"))

# ---- summary vs paper ----------------------------------------------------------
pre <- acc[year <= 2008]
meta <- as.data.table(st_drop_geometry(ctl))
res <- data.table(
  n = nrow(ctl),
  mean_declared_ha = round(mean(meta$NUM_ARE, na.rm = TRUE), 1),
  mean_cleaned_geom_ha = round(mean(meta$geom_ha, na.rm = TRUE), 1),
  mean_lf_ha_pre2009 = round(pre[, mean(lf_ha[valid_px > 0], na.rm = TRUE)], 1),
  rate_mean_of_yearly = round(pre[, .(m = mean(rate, na.rm = TRUE)), by = year][, mean(m)], 1),
  rate_pooled = round(pre[, mean(rate, na.rm = TRUE)], 1),
  defor_Mha_2008 = round(acc[year == 2008, sum(defor_ha, na.rm = TRUE)] / 1e6, 3),
  defor_Mha_2014 = round(acc[year == 2014, sum(defor_ha, na.rm = TRUE)] / 1e6, 3)
)
cat("\n===== TEST F3: legacy reserve cleaning on the F2 never-eligible sample =====\n")
print(as.data.frame(res))
cat("\npaper: N 7,049 | area 760 | rate 35.7 | defo 2.0 -> 2.2 Mha\n")
cat("uncleaned F2 baseline: N 6,856 | declared 1,163.2 | rate 36.7 | 2.329 -> 2.551 Mha\n")
fwrite(res, file.path(emp_dir, "table1_testF3_reserveclean.csv"))
cat("\nWrote: table1_testF3_reserveclean.csv, control_reserve_cleaned_panel.csv\n")
