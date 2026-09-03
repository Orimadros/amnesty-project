# Stage 6 (diagnostic): test two methodological differences vs the paper/legacy,
# found in the 2026-07-30 sweep (docs/notes/never_eligible_method_diff.md):
#
# TEST A -- "average prior to 2009", not the 2008 value.
#   Table 2's notes state the outcome baselines (58.4%, 11.4%) are "measured as the
#   average prior to 2009"; Table 1 carries the same numbers with a "(t<2009)" label.
#   Our table1_comparison benchmarks mean rate_2008 against them. Recompute our means
#   over 2005-2008: (i) mean of the four yearly cross-sectional means (how legacy's
#   clean_car_comp2 is laid out), (ii) pooled parcel-year mean.
#
# TEST B -- occupation-by-2004 as a 2004 LEVEL test, not first-crossing.
#   Legacy 2_empirics.R:1209/1290 filters on deforestation_rate > 10 measured on the
#   2004 raster (a level, net of regrowth). Our stage 2 uses the first year the rate
#   reaches 10% (a crossing). Reclassify with the level test and recompute.
#
# Both tests reuse stage-2 outputs and the parcel_defo CSVs; nothing upstream is
# touched. The remaining lead -- legacy's "ever occupied" filter running on the 2019
# raster (paper says 2014) -- needs a 2019 stage-1 run and is tested separately.

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

emp_dir <- here("data", "intermediate", "empirics")
YEARS_PRE <- 2005:2008

elig <- fread(file.path(emp_dir, "parcel_eligibility.csv"))

# ---- per-parcel erasure-adjusted rates for 2005-2008 ---------------------------
d <- rbindlist(lapply(YEARS_PRE, function(y)
  fread(file.path(emp_dir, sprintf("parcel_defo_%d.csv", y)))))
adj <- fread(file.path(emp_dir, "erasure_adjustment.csv"))
d <- merge(d, adj[, .(car_id, year, er_defo_px, er_valid_px)],
           by = c("car_id", "year"), all.x = TRUE)
d[is.na(er_defo_px), er_defo_px := 0L]
d[is.na(er_valid_px), er_valid_px := 0L]
d[, defor_px := pmax(defor_px - er_defo_px, 0L)]
d[, valid_px := pmax(valid_px - er_valid_px, 0L)]
d[, rate := fifelse(valid_px > 0, defor_px / valid_px * 100, NA_real_)]

samp <- elig[in_sample == TRUE, .(car_id, class, area_ha, rate_2004, rate_2008,
                                  defor_ha_2004, defor_ha_2008, occupied_by_2004, small)]
dd <- merge(d[, .(car_id, year, rate)], samp, by = "car_id")

paper <- data.table(class = c("eligible", "ineligible", "never_eligible"),
                    paper_rate = c(58.4, 11.4, 35.7),
                    paper_n = c(71171, 15254, 7049),
                    paper_area = c(143, 661, 760))

# ---- TEST A --------------------------------------------------------------------
yearly <- dd[, .(m = mean(rate, na.rm = TRUE)), by = .(class, year)]
a <- merge(
  yearly[, .(mean_of_yearly_means = round(mean(m), 1)), by = class],
  dd[, .(pooled_mean = round(mean(rate, na.rm = TRUE), 1)), by = class],
  by = "class")
a <- merge(a, samp[, .(rate2008_only = round(mean(rate_2008, na.rm = TRUE), 1)), by = class],
           by = "class")
a <- merge(a, paper[, .(class, paper_rate)], by = "class")

cat("\n========== TEST A: pre-2009 average vs 2008-only ==========\n")
print(as.data.frame(a))
cat("\nyearly means:\n")
print(dcast(yearly[, .(class, year, m = round(m, 1))], class ~ year, value.var = "m"))

# ---- TEST B --------------------------------------------------------------------
# Legacy level test: rate in 2004 (net of regrowth, erasure-adjusted) > 10, strict.
samp[, occupied_lvl := !is.na(rate_2004) & rate_2004 > 10]
samp[, class_lvl := fifelse(class == "never_eligible", "never_eligible",
                     fifelse(occupied_lvl & small, "eligible", "ineligible"))]

cat("\n========== TEST B: level-2004 occupation vs first-crossing ==========\n")
cat("reclassified parcels:", samp[class != class_lvl, .N], "\n")
print(samp[, .N, by = .(class, class_lvl)][order(class, class_lvl)])

dd2 <- merge(d[, .(car_id, year, rate)], samp[, .(car_id, class_lvl)], by = "car_id")
b <- merge(
  merge(
    dd2[, .(m = mean(rate, na.rm = TRUE)), by = .(class_lvl, year)][
      , .(mean_of_yearly_means = round(mean(m), 1)), by = class_lvl],
    dd2[, .(pooled_mean = round(mean(rate, na.rm = TRUE), 1)), by = class_lvl],
    by = "class_lvl"),
  samp[, .(n = .N, rate2008_only = round(mean(rate_2008, na.rm = TRUE), 1),
           mean_area_ha = round(mean(area_ha, na.rm = TRUE), 1)), by = class_lvl],
  by = "class_lvl")
b <- merge(b, paper, by.x = "class_lvl", by.y = "class")
print(as.data.frame(b))

# ---- area interpretations for the never-eligible 760-vs-1085 gap ---------------
cat("\n========== mean 'area' under different definitions (in-sample) ==========\n")
ar <- samp[, .(
  declared_claim_ha = round(mean(area_ha, na.rm = TRUE), 1),
  legacy_forest_ha_2008 = round(mean(
    fifelse(!is.na(rate_2008) & rate_2008 > 0, defor_ha_2008 / (rate_2008 / 100), NA_real_),
    na.rm = TRUE), 1)
), by = class]
print(as.data.frame(merge(ar, paper[, .(class, paper_area)], by = "class")))

out <- list(testA = a, testB = b, areas = ar)
saveRDS(out, file.path(emp_dir, "table1_method_tests.rds"))
fwrite(a, file.path(emp_dir, "table1_testA_pre2009avg.csv"))
fwrite(b, file.path(emp_dir, "table1_testB_level2004.csv"))
cat("\nWrote: table1_testA_pre2009avg.csv, table1_testB_level2004.csv\n")
