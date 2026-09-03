# Stage 11 (diagnostic): P1 from the 2026-07-31 re-audit.
#
# Legacy's ineligible sample filter (2_empirics.R:1704) uses `area`, which is
# created ONLY in the 2005 file's block (`defo_2005/(rate_2005/100)`) and never
# recreated in the later joins -- so it is the 2005 legacy-forest area, and any
# ineligible parcel with ZERO deforestation in 2005 (rate NaN -> NA via
# mutate_all) is excluded from the Table 1 rate panel. Our stage 2 port used the
# 2008 values. Recompute the F1+F2+drops comparison with the faithful 2005 basis.

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

# pre-2009 erasure-adjusted rates (also supplies the 2005 lf basis)
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

# legacy's `area`: NA when rate_2005 is NaN (no legacy forest) OR zero defo
lf05 <- d[year == 2005, .(car_id,
  lf05_ha = fifelse(defor_px > 0 & valid_px > 0, valid_px * 0.09, NA_real_))]
elig <- merge(elig, lf05, by = "car_id", all.x = TRUE)
drop_inelig <- elig$class == "ineligible" & (is.na(elig$lf05_ha) | elig$lf05_ha >= 1e5)
message("2005-basis filter drops (in-2019 ineligible): ",
        elig[in2019 == TRUE & drop_inelig, .N],
        "  [2008-basis dropped: ", elig[in2019 == TRUE & class == "ineligible" &
          (is.na(lf_ha) | lf_ha >= 1e5), .N], "]")
elig[drop_inelig == TRUE, in2019 := FALSE]

dec <- fread(file.path(emp_dir, "conflict_decisions_2014.csv"))
dropped <- unique(dec[action %in% c("drop", "drop_random")]$car_id)
samp <- elig[in2019 == TRUE & class != "never_eligible" & !car_id %in% dropped]

dd <- merge(d[, .(car_id, year, rate)], samp[, .(car_id, class)], by = "car_id")
paper <- data.table(class = c("eligible", "ineligible"),
                    paper_rate = c(58.4, 11.4), paper_n = c(71171, 15254),
                    paper_area = c(143, 661),
                    paper_defo08 = c(5.1, 4.1), paper_defo14 = c(5.3, 4.7))
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

cat("\n===== F1 + F2 + drops + 2005-basis lf filter =====\n")
print(as.data.frame(res))
cat("\nyearly means:\n")
print(dcast(dd[, .(m = round(mean(rate, na.rm = TRUE), 1)), by = .(class, year)],
            class ~ year, value.var = "m"))
fwrite(res, file.path(emp_dir, "table1_test_lf2005.csv"))
cat("\nWrote: table1_test_lf2005.csv\n")
