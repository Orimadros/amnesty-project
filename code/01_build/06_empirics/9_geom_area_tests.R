# Stage 9 (diagnostic): quantify S2 and S3 from docs/notes/paper_legacy_method_diffs.md.
#
# S2 -- control membership: legacy divides the control-area overlap by GEOMETRIC
#       parcel area (st_area); our build divides by declared NUM_AREA. Same 1%
#       threshold. Using the stored shares (cntrl__ = inter_ha / area_ha), the
#       geometric-denominator share is cntrl__ * area_ha / geom_ha.
#       Limitation: CARs in NEITHER output layer were not scored, so flips into
#       the control pool from completely unscored parcels are invisible here.
#
# S3 -- eligible area cap: legacy uses geometric st_area <= 1500 ha; we use
#       declared area_ha <= 1500. Count the in-sample-2019 parcels that would
#       switch eligibility class under the geometric definition.

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(here)
})
sf_use_s2(FALSE)
CRS_EQ <- 5880
emp_dir <- here("data", "intermediate", "empirics")

geom_ha_of <- function(path) {
  x <- st_read(path, quiet = TRUE)
  x$car_id <- as.character(x$car_id)
  dt <- as.data.table(st_drop_geometry(x))[, .(car_id, area_ha,
    cntrl = as.numeric(cntrl__))]
  dt[, geom_ha := as.numeric(st_area(st_transform(x, CRS_EQ))) / 1e4]
  dt
}

ctl <- geom_ha_of(here("data", "intermediate", "car", "car_ineligible_cleaned.shp"))
tgt <- geom_ha_of(here("data", "intermediate", "car", "car_eligible_cleaned.shp"))

# ---- S2 ------------------------------------------------------------------------
ctl[, share_geom := cntrl * area_ha / geom_ha]
tgt[, share_geom := cntrl * area_ha / geom_ha]
cat("S2: control members whose GEOMETRIC share <= 1% (would LEAVE control):",
    ctl[share_geom <= 0.01, .N], "/", nrow(ctl), "\n")
cat("S2: target members whose GEOMETRIC control share > 1% (would ENTER control):",
    tgt[share_geom > 0.01, .N], "/", nrow(tgt), "\n")

# ---- S3 ------------------------------------------------------------------------
d19 <- fread(file.path(emp_dir, "parcel_defo_2019.csv"))[, .(car_id, rate_2019 = deforestation_rate)]
elig <- fread(file.path(emp_dir, "parcel_eligibility.csv"))
elig <- merge(elig, d19, by = "car_id", all.x = TRUE)
elig[, in2019 := !is.na(rate_2019) & rate_2019 > 10]
ga <- rbind(ctl[, .(car_id, geom_ha)], tgt[, .(car_id, geom_ha)])
e <- merge(elig[in2019 == TRUE], ga, by = "car_id", all.x = TRUE)

e[, small_geom := !is.na(geom_ha) & geom_ha <= 1500 &
    !is.na(defor_ha_2004) & defor_ha_2004 <= 1500]
e[, class_geom := fifelse(class == "never_eligible", "never_eligible",
                   fifelse(occupied_by_2004 & small_geom, "eligible", "ineligible"))]
cat("\nS3: class switches under geometric area cap (in-sample-2019):\n")
print(e[class != class_geom, .N, by = .(class, class_geom)])
cat("\nrate_2008 means by geometric-cap class:\n")
print(e[, .(n = .N, rate2008 = round(mean(rate_2008, na.rm = TRUE), 1),
            mean_area = round(mean(area_ha, na.rm = TRUE), 1)), by = class_geom])

out <- list(
  s2_leave = ctl[share_geom <= 0.01, .N], s2_enter = tgt[share_geom > 0.01, .N],
  s3 = e[class != class_geom, .N, by = .(class, class_geom)])
saveRDS(out, file.path(emp_dir, "geom_area_tests.rds"))
cat("\nWrote: geom_area_tests.rds\n")
