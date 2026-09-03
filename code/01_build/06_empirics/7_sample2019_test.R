# Stage 7 (diagnostic): TEST F2 from docs/notes/paper_legacy_method_diffs.md.
#
# Legacy's "ever occupied" STEP 1 (2_empirics.R:619 glebas, :1818 reserves) keeps
# parcels with deforestation rate > 10% measured on the *2019* legacy-forest raster
# (files misleadingly named active2014_*; the paper's appendix says 2014). Our
# stage 2 uses rate in 2014. Here: recompute the eligibility split's summary with
#   in_sample = rate_2019 > 10   (strict, raw geometry, as legacy's step 1)
# and benchmark the paper's numbers under the F1 correction (pre-2009 averages).
#
# Needs parcel_defo_2019.csv (stage 1 with EMP_YEAR=2019). Classes come from
# parcel_eligibility.csv (stage 2), which classifies ALL parcels; only the sample
# filter changes here. The legacy ineligible lf-area filter is re-applied on the
# new sample exactly as stage 2 does.

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

emp_dir <- here("data", "intermediate", "empirics")
YEARS_PRE <- 2005:2008

f2019 <- file.path(emp_dir, "parcel_defo_2019.csv")
if (!file.exists(f2019)) stop("Run stage 1 with EMP_YEAR=2019 first.")
d19 <- fread(f2019)[, .(car_id, rate_2019 = deforestation_rate)]

elig <- fread(file.path(emp_dir, "parcel_eligibility.csv"))
elig <- merge(elig, d19, by = "car_id", all.x = TRUE)

# Legacy step 1, both pools: strict > 10 on the 2019 level.
elig[, in_sample_2019 := !is.na(rate_2019) & rate_2019 > 10]
# Stage 2's legacy ineligible filter (2_empirics.R:1704), same definition.
drop_inelig <- elig$class == "ineligible" & (is.na(elig$lf_ha) | elig$lf_ha >= 1e5)
elig[drop_inelig == TRUE, in_sample_2019 := FALSE]

cat("in-sample counts, 2014-rule vs 2019-rule:\n")
print(elig[, .(in2014 = sum(in_sample), in2019 = sum(in_sample_2019)), by = class])

# ---- pre-2009 rates on the new sample ------------------------------------------
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

samp <- elig[in_sample_2019 == TRUE]
dd <- merge(d[, .(car_id, year, rate)], samp[, .(car_id, class)], by = "car_id")

paper <- data.table(class = c("eligible", "ineligible", "never_eligible"),
                    paper_rate = c(58.4, 11.4, 35.7),
                    paper_n = c(71171, 15254, 7049),
                    paper_area = c(143, 661, 760))

res <- merge(
  merge(
    dd[, .(m = mean(rate, na.rm = TRUE)), by = .(class, year)][
      , .(mean_of_yearly_means = round(mean(m), 1)), by = class],
    dd[, .(pooled_pre2009 = round(mean(rate, na.rm = TRUE), 1)), by = class],
    by = "class"),
  samp[, .(n = .N,
           rate2008_only = round(mean(rate_2008, na.rm = TRUE), 1),
           mean_area_ha = round(mean(area_ha, na.rm = TRUE), 1),
           defor_Mha_2008 = round(sum(defor_ha_2008, na.rm = TRUE) / 1e6, 3),
           defor_Mha_2014 = round(sum(defor_ha_2014, na.rm = TRUE) / 1e6, 3)), by = class],
  by = "class")
res <- merge(res, paper, by = "class")

cat("\n========== TEST F2: in_sample = rate_2019 > 10, pre-2009 averages ==========\n")
print(as.data.frame(res))

cat("\nyearly means on the 2019-rule sample:\n")
print(dcast(dd[, .(m = round(mean(rate, na.rm = TRUE), 1)), by = .(class, year)],
            class ~ year, value.var = "m"))

fwrite(res, file.path(emp_dir, "table1_testF2_sample2019.csv"))
cat("\nWrote: table1_testF2_sample2019.csv\n")
