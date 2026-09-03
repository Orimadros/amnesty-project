# Stage 10 (diagnostic): apply the conflict algorithm's DROP decisions to the
# Table 1 sample -- the missing consumer found in the 2026-07-31 eligible sweep.
#
# Stage 4 writes parcels_resolved_2014.csv (drops applied) and stage 3 uses it for
# the DiD, but the Table 1 summary in stage 2 never excluded dropped parcels: as of
# checkpoint-20260729b, 23,517 in-sample parcels sat in the counts despite drop
# decisions. Legacy removes them from the per-muni cleaned files BEFORE the
# eligibility tests (car2014_all is built from *_cleaned_* shapefiles), so its
# Table 1 counts them out.
#
# Per F3, legacy never applies the 5-rule algorithm to the reserve pool, so drops
# are applied to the TARGET classes only; the never-eligible column uses the F3
# result (8_reserve_cleaning_test.R).
#
# CAVEAT: stage 4 ran on the 2014-rule in-sample set. The F2 (2019-rule) sample
# adds parcels whose conflicts were never examined, so this is a lower bound on
# drops; re-running stage 4 on the F2 sample is the exact version.

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

emp_dir <- here("data", "intermediate", "empirics")
YEARS_PRE <- 2005:2008

elig <- fread(file.path(emp_dir, "parcel_eligibility.csv"))
d19 <- fread(file.path(emp_dir, "parcel_defo_2019.csv"))[, .(car_id, rate_2019 = deforestation_rate)]
elig <- merge(elig, d19, by = "car_id", all.x = TRUE)
elig[, in2019 := !is.na(rate_2019) & rate_2019 > 10]
drop_inelig <- elig$class == "ineligible" & (is.na(elig$lf_ha) | elig$lf_ha >= 1e5)
elig[drop_inelig == TRUE, in2019 := FALSE]

dec <- fread(file.path(emp_dir, "conflict_decisions_2014.csv"))
dropped <- unique(dec[action %in% c("drop", "drop_random")]$car_id)
message("drop decisions available: ", length(dropped))

samp <- elig[in2019 == TRUE & class != "never_eligible"]
message("F2 target sample before drops: ", nrow(samp))
samp <- samp[!car_id %in% dropped]
message("after drops: ", nrow(samp))

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
dd <- merge(d[, .(car_id, year, rate)], samp[, .(car_id, class)], by = "car_id")

paper <- data.table(class = c("eligible", "ineligible"),
                    paper_rate = c(58.4, 11.4),
                    paper_n = c(71171, 15254),
                    paper_area = c(143, 661),
                    paper_defo08 = c(5.1, 4.1),
                    paper_defo14 = c(5.3, 4.7))

res <- merge(
  merge(
    dd[, .(m = mean(rate, na.rm = TRUE)), by = .(class, year)][
      , .(rate_pre2009 = round(mean(m), 1)), by = class],
    samp[, .(n = .N,
             mean_area_ha = round(mean(area_ha, na.rm = TRUE), 1),
             defor_Mha_2008 = round(sum(defor_ha_2008, na.rm = TRUE) / 1e6, 3),
             defor_Mha_2014 = round(sum(defor_ha_2014, na.rm = TRUE) / 1e6, 3)), by = class],
    by = "class"),
  paper, by = "class")

cat("\n===== F1 + F2 + DROPS APPLIED (target groups) =====\n")
print(as.data.frame(res))
cat("\n(never-eligible under F1+F2+F3: N 6,855 | rate 36.3 | 2.003 -> 2.189 Mha; paper 7,049 | 35.7 | 2.0 -> 2.2)\n")

fwrite(res, file.path(emp_dir, "table1_test_dropsapplied.csv"))
cat("\nWrote: table1_test_dropsapplied.csv\n")
