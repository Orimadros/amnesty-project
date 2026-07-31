# Stage 13: the never-eligible panel measured on legacy's reserve-cleaned geometry,
# for every DiD year (2005-2014).
#
# Supersedes 8_reserve_cleaning_test.R, which only measured 2005-2008 + 2014. The
# DiD needs the full window on ONE geometry basis: mixing cleaned pre-2009 with
# uncleaned post-2009 rates would manufacture a level break exactly at the
# treatment date.
#
# Sample: never-eligible parcels active under the 2019 rule (F2). Cleaning: legacy's
# reserve-only algorithm (F3, see _helpers_reserve_clean.R). The cleaned geometry is
# cached so re-measurement does not repeat the ~10 min cleaning.

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(data.table)
  library(here)
})

sf_use_s2(FALSE)
CRS_EQ <- 5880
YEARS <- 2005:2014

emp_dir <- here("data", "intermediate", "empirics")
tile_dir <- here("data", "intermediate", "mapbiomas", "transitions_combined")
geom_cache <- file.path(emp_dir, "control_cleaned_geometry.gpkg")

source(here("code", "01_build", "06_empirics", "_helpers_reserve_clean.R"))

if (file.exists(geom_cache)) {
  ctl <- st_read(geom_cache, quiet = TRUE)
  message("loaded cached cleaned control geometry: ", nrow(ctl), " parcels")
} else {
  d19 <- fread(file.path(emp_dir, "parcel_defo_2019.csv"))[
    , .(car_id, rate_2019 = deforestation_rate)]
  elig <- merge(fread(file.path(emp_dir, "parcel_eligibility.csv")), d19,
                by = "car_id", all.x = TRUE)
  keep <- elig[class == "never_eligible" & !is.na(rate_2019) & rate_2019 > 10, car_id]
  message("active-2019 never-eligible parcels: ", length(keep))

  shp <- st_read(here("data", "intermediate", "car", "car_ineligible_cleaned.shp"),
                 quiet = TRUE)
  shp$car_id <- as.character(shp$car_id)
  shp <- shp[shp$car_id %in% keep, c("car_id", "NUM_ARE")]
  shp$NUM_ARE <- suppressWarnings(as.numeric(shp$NUM_ARE))
  shp <- st_make_valid(st_transform(shp, 4326))

  ctl <- reserve_clean(shp, crs_eq = CRS_EQ)
  message("cleaned parcels: ", nrow(ctl))
  st_write(ctl, geom_cache, delete_dsn = TRUE, quiet = TRUE)
  message("cached geometry -> ", geom_cache)
}

ctl$geom_ha <- as.numeric(st_area(st_transform(ctl, CRS_EQ))) / 1e4
pv <- vect(ctl)
bb <- as.data.table(do.call(rbind, lapply(st_geometry(ctl), function(g) st_bbox(g))))
setnames(bb, c("xmin", "ymin", "xmax", "ymax"))
bb[, id := .I]

acc <- CJ(car_id = ctl$car_id, year = YEARS)[, `:=`(defor_px = 0, valid_px = 0)]
setkey(acc, car_id, year)

t0 <- Sys.time()
for (yr in YEARS) {
  tiles <- list.files(tile_dir, pattern = paste0("_", yr, "\\.tif$"), full.names = TRUE)
  nhit <- 0L
  for (tf in tiles) {
    r <- rast(tf); e <- ext(r)
    hit <- bb[xmin < e$xmax & xmax > e$xmin & ymin < e$ymax & ymax > e$ymin, id]
    if (length(hit) == 0) next
    exv <- tryCatch(terra::extract(r, pv[hit, ]), error = function(e) NULL)
    if (is.null(exv)) next
    names(exv)[2] <- "v"
    agg <- as.data.table(exv)[, .(d = sum(v == 2, na.rm = TRUE),
                                  n = sum(v != 0 & !is.na(v))), by = ID]
    agg[, car_id := ctl$car_id[hit[ID]]]
    acc[.(agg$car_id, yr), `:=`(defor_px = defor_px + agg$d,
                                valid_px = valid_px + agg$n)]
    nhit <- nhit + 1L
  }
  el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  message(sprintf("year %d done (%d tiles with hits) | %.1f min elapsed", yr, nhit, el))
}

acc[, `:=`(
  rate_legacyforest = fifelse(valid_px > 0, defor_px / valid_px * 100, NA_real_),
  deforested_area_ha = defor_px * 0.09
)]
ga <- data.table(car_id = ctl$car_id, geom_ha = ctl$geom_ha,
                 declared_ha = suppressWarnings(as.numeric(ctl$NUM_ARE)))
acc <- merge(acc, ga, by = "car_id", all.x = TRUE)
acc[, rate_claim := fifelse(declared_ha > 0, deforested_area_ha / declared_ha * 100, NA_real_)]

fwrite(acc, file.path(emp_dir, "control_cleaned_panel_full.csv"))
message("Wrote: control_cleaned_panel_full.csv  (", nrow(acc), " parcel-years, ",
        uniqueN(acc$car_id), " parcels)")
print(acc[, .(mean_rate_lf = round(mean(rate_legacyforest, na.rm = TRUE), 1),
              defo_Mha = round(sum(deforested_area_ha, na.rm = TRUE) / 1e6, 3)), by = year][order(year)])
