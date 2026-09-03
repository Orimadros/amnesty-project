# Stage 23 (recovered code): rebuild the DETER visible-fraction series.
#
# Port of Multas-AvisosMatchingV3.R:396-611 (the clear-spots half), run on the
# "Clear Spots - Shapefile" folder recovered from Pedro's Dropbox 2026-08-10.
# V3's own export `visible_fraction.csv` survives nowhere, so this rebuild is the
# only path to the provenance of enforcement_clouds.dta's fraction_control /
# fraction_target columns (the 2026-08-07 audit inferred they are plain per-file
# means within year, A/B half-scenes counted separately; verified here).
#
# Faithfulness notes:
#   - sf_use_s2(FALSE) as in V3:25.
#   - muni_control/target areas are st_union'ed in their NATIVE CRS and never
#     transformed (V3:396-401); the clear-spots files are read as-is. V3 has no
#     CRS harmonisation -- if a file's CRS mismatches, legacy st_intersection
#     errors; here mismatches are transformed to the muni CRS with a message
#     (the only deviation, needed to run at all; count reported).
#   - fraction = st_area(intersection) / st_area(union), per FILE id (200505,
#     200605A, 200605B, ...) exactly as V3's loop builds visiblefraction_df.
#
# After the rebuild, the enforcement_clouds.dta comparison: enforcement_* is the
# buffer-0.5 fines_per_warning rounded to 2dp (established 2026-08-07); the
# fraction_* rule is tested here as mean(fraction) by calendar year over file
# rows. Output: visible_fraction_rebuilt.csv + a comparison table.

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(haven)
  library(here)
})
sf_use_s2(FALSE)

emp_dir <- here("data", "intermediate", "empirics")
dd <- here("data", "legacy_dropbox")

mc <- st_read(file.path(dd, "miseEnPlace", "muni_control_areas.gpkg"), quiet = TRUE)
mt <- st_read(file.path(dd, "miseEnPlace", "muni_target_areas.gpkg"), quiet = TRUE)
mcU <- st_union(mc)
mtU <- st_union(mt)
area_control_total <- st_area(mcU)
area_target_total <- st_area(mtU)
message("control area: ", format(area_control_total), " | target area: ",
        format(area_target_total), " | crs: ", st_crs(mc)$input)

cs_dir <- file.path(dd, "fines_robustness", "Clear Spots - Shapefile")
shp <- list.files(cs_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
message(length(shp), " clear-spots shapefiles")

res <- vector("list", length(shp))
n_transformed <- 0L
for (i in seq_along(shp)) {
  id <- sub("^clear_spots", "", tools::file_path_sans_ext(basename(shp[i])))
  x <- suppressWarnings(st_read(shp[i], quiet = TRUE))
  if (is.na(st_crs(x))) st_crs(x) <- st_crs(mc)
  if (st_crs(x) != st_crs(mc)) {
    x <- st_transform(x, st_crs(mc))
    n_transformed <- n_transformed + 1L
  }
  ic <- suppressWarnings(st_intersection(mcU, x))
  ac <- if (length(ic) == 0) 0 else sum(as.numeric(st_area(ic)))
  it <- suppressWarnings(st_intersection(mtU, x))
  at <- if (length(it) == 0) 0 else sum(as.numeric(st_area(it)))
  res[[i]] <- data.table(id = id,
                         fraction_control = ac / as.numeric(area_control_total),
                         fraction_target = at / as.numeric(area_target_total))
  if (i %% 10 == 0) message(i, "/", length(shp), " done")
}
vf <- rbindlist(res)
fwrite(vf, file.path(emp_dir, "visible_fraction_rebuilt.csv"))

# ---- compare to the hand-assembled enforcement_clouds.dta --------------------
vf[, year := as.integer(substr(id, 1, 4))]
by_year <- vf[, .(fraction_control = mean(fraction_control),
                  fraction_target = mean(fraction_target), n_files = .N),
              by = year][order(year)]

ec <- as.data.table(read_dta(file.path(dd, "fines_robustness", "enforcement_clouds.dta")))
cmp <- merge(by_year, ec[, .(year, dta_control = fraction_control,
                             dta_target = fraction_target)], by = "year")
cmp[, `:=`(round_control = round(fraction_control, 2),
           round_target = round(fraction_target, 2))]

cat("\n===== visible fraction: rebuilt per-file yearly means vs enforcement_clouds.dta =====\n")
print(as.data.frame(cmp[, .(year, n_files,
                            rebuilt_ctl = round(fraction_control, 4), dta_ctl = dta_control,
                            match_ctl = round_control == dta_control,
                            rebuilt_tgt = round(fraction_target, 4), dta_tgt = dta_target,
                            match_tgt = round_target == dta_target)]))
cat("\nfiles with transformed CRS (deviation from V3, needed to run): ",
    n_transformed, "\n", sep = "")
cat("Wrote: ", file.path(emp_dir, "visible_fraction_rebuilt.csv"), "\n", sep = "")
