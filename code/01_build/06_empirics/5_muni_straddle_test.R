# Stage 5 (diagnostic): the municipality-boundary straddle test.
#
# Legacy 2_empirics.R:1594-1595 (and the same lines for the eligible files) applies
# two sample filters we do not implement:
#
#   x %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc) %>%
#     filter(row_number(COD_IMO) == 1)          # collapse identical duplicates
#   x %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)   # drop the rest
#
# The second drops any CAR appearing more than once with DIFFERING measured
# values. Legacy's per-year files are stacked per-municipality measurements, so
# the dropped set is (approximately) parcels that appear in more than one
# municipality's file -- parcels straddling a municipality boundary. Our pipeline
# measures each parcel exactly once (summing across raster tiles), so we retain
# them.
#
# Provenance check already done: in OUR SICAR vintage every car_id appears in
# exactly one CleanCARShapes_robust municipality file, and only 6 registro_car in
# all of temas_ambientais.csv carry more than one codigo_ibge. So the legacy
# filter cannot be replicated by provenance here; the population it dropped can
# only be identified geometrically.
#
# This script flags every in-sample parcel whose geometry meaningfully intersects
# more than one municipality, drops them, and recomputes the Table 1 summary.
# The question (checkpoint-20260729b): does the ineligible mean rate move from
# 23.5% toward the paper's 11.4%?
#
# Outputs:
#   data/intermediate/empirics/parcel_muni_straddle.csv   per-parcel flags
#   data/intermediate/empirics/muni_straddle_test.csv     summary, kept vs dropped

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(dplyr)
  library(here)
})

sf_use_s2(FALSE) # planar GEOS, as in the other empirics stages

PIXEL_HA <- 0.09 # one MapBiomas pixel; the smallest overlap that could change a count
CRS_EQ <- 5880   # SIRGAS 2000 / Brazil Polyconic (metres)

emp_dir <- here("data", "intermediate", "empirics")

elig <- fread(file.path(emp_dir, "parcel_eligibility.csv"))
samp <- elig[in_sample == TRUE]
message("in-sample parcels: ", nrow(samp))

# ---- geometries ---------------------------------------------------------------
car_files <- c(
  here("data", "intermediate", "car", "car_eligible_cleaned.shp"),
  here("data", "intermediate", "car", "car_ineligible_cleaned.shp")
)
geo <- do.call(rbind, lapply(car_files, function(f) {
  x <- st_read(f, quiet = TRUE)
  idc <- intersect(c("COD_IMO", "COD_IMOVEL", "cod_imovel"), names(x))[1]
  x <- x[, idc, drop = FALSE]
  names(x)[1] <- "car_id"
  x$car_id <- as.character(x$car_id)
  x
}))
geo <- geo[geo$car_id %in% samp$car_id, ]
message("geometries matched: ", nrow(geo), " / ", nrow(samp))
geo <- st_transform(st_make_valid(geo), CRS_EQ)

# ---- municipalities -----------------------------------------------------------
mun <- st_read(here("data", "input", "aux", "municipal_boundaries",
                    "municipal_boundaries.shp"), quiet = TRUE)
mun <- st_transform(st_make_valid(mun[, "code_muni"]), CRS_EQ)
message("municipalities: ", nrow(mun))

# ---- candidate hits -----------------------------------------------------------
hits <- st_intersects(geo, mun)
n_cand <- lengths(hits)
message("parcels touching >= 2 municipality polygons: ", sum(n_cand >= 2))

# Parcels touching one municipality (or zero) cannot straddle. For the rest,
# measure the actual overlap with each candidate: a shared edge counts as an
# intersects() hit but has no area.
multi <- which(n_cand >= 2)
res <- data.table(car_id = geo$car_id, n_cand = n_cand,
                  n_muni = pmin(n_cand, 1L), second_share = 0)

if (length(multi) > 0) {
  gm <- geo[multi, ]
  pieces <- suppressWarnings(st_intersection(gm, mun))
  pieces$ha <- as.numeric(st_area(pieces)) / 1e4
  pd <- data.table(car_id = pieces$car_id, code_muni = pieces$code_muni, ha = pieces$ha)
  pd <- pd[, .(ha = sum(ha)), by = .(car_id, code_muni)]
  agg <- pd[, {
    o <- sort(ha, decreasing = TRUE)
    .(n_muni = sum(ha > PIXEL_HA),
      second_share = if (.N >= 2) o[2] / sum(o) else 0)
  }, by = car_id]
  res <- merge(res[, .(car_id, n_cand)], agg, by = "car_id", all.x = TRUE)
  res[is.na(n_muni), `:=`(n_muni = pmin(n_cand, 1L), second_share = 0)]
}

res[, straddles := n_muni >= 2]
message("parcels straddling (> ", PIXEL_HA, " ha in >= 2 munis): ", sum(res$straddles))

out <- merge(samp[, .(car_id, class, area_ha, rate_2008, defor_ha_2008, defor_ha_2014)],
             res, by = "car_id", all.x = TRUE)
out[is.na(straddles), `:=`(straddles = FALSE, n_cand = 0L, n_muni = 0L, second_share = 0)]
fwrite(out, file.path(emp_dir, "parcel_muni_straddle.csv"))
message("Wrote: ", file.path(emp_dir, "parcel_muni_straddle.csv"))

# ---- the test -----------------------------------------------------------------
summarise_block <- function(d) {
  d[, .(
    n_properties = .N,
    mean_rate_2008 = round(mean(rate_2008, na.rm = TRUE), 1),
    mean_area_ha = round(mean(area_ha, na.rm = TRUE), 1),
    defor_Mha_2008 = round(sum(defor_ha_2008, na.rm = TRUE) / 1e6, 3),
    defor_Mha_2014 = round(sum(defor_ha_2014, na.rm = TRUE) / 1e6, 3)
  ), by = class]
}

base <- summarise_block(out)
kept <- summarise_block(out[straddles == FALSE])
# sensitivity: only drop parcels with a NON-TRIVIAL second-municipality share
kept_1pct <- summarise_block(out[!(straddles & second_share > 0.01)])

cmp <- merge(base, kept, by = "class", suffixes = c("_all", "_dropstraddle"))
cmp <- merge(cmp, kept_1pct[, .(class, n_properties_drop1pct = n_properties,
                                mean_rate_2008_drop1pct = mean_rate_2008)],
             by = "class")
paper <- data.table(class = c("eligible", "ineligible", "never_eligible"),
                    paper_rate_2008 = c(58.4, 11.4, 35.7),
                    paper_n = c(71171, 15254, 7049))
cmp <- merge(cmp, paper, by = "class")
setorder(cmp, class)

cat("\n================ STRADDLE TEST ================\n")
print(as.data.frame(cmp))

cat("\nper-class straddler counts:\n")
print(out[, .(n = .N, straddlers = sum(straddles),
              straddlers_gt1pct = sum(straddles & second_share > 0.01)), by = class])

fwrite(cmp, file.path(emp_dir, "muni_straddle_test.csv"))
cat("\nWrote: ", file.path(emp_dir, "muni_straddle_test.csv"), "\n", sep = "")
