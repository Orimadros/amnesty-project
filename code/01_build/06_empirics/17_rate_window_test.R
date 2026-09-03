# Stage 17 (diagnostic): which averaging window and which SET produce Table 1's rates?
#
# Table 1 reports 58.4 / 11.4 / 35.7; Table 2 calls them "the average prior to 2009".
# On our post-filter panel the ineligible pre-2009 average is 16.7 (+46%) while the
# 2005 value alone is 12.1 (+6%) -- so either the window or the set is wrong.
#
# The set matters because legacy's :1704 filter (`!is.na(area)`, area being the 2005
# legacy-forest area) removes exactly the parcels with ZERO 2005 deforestation. Those
# parcels have a well-defined rate of 0, not NA -- so including them drags the mean
# down. If Table 1's rate row was computed BEFORE that filter and the N row after it
# (or vice versa), the column would mix two subsets, which is the pattern D-C already
# established across tables.
#
# Grid: {post-P1 panel, pre-P1 panel} x {2005 only, 2005-2008 mean-of-yearly,
# 2005-2008 pooled, 2005-2009 mean-of-yearly}, for all three classes. Cleaning
# decisions are held fixed throughout, so only the window and the set vary.

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

emp_dir <- here("data", "intermediate", "empirics")
elig <- fread(file.path(emp_dir, "parcel_eligibility.csv"))

# cleaning survivors (target classes) + the control set stage 2 uses
res_f <- file.path(emp_dir, "parcels_resolved_2004rules.csv")
kept_t <- if (file.exists(res_f)) fread(res_f, select = "car_id")$car_id else NULL
ctl_panel <- file.path(emp_dir, "control_cleaned_panel_full.csv")
kept_c <- if (file.exists(ctl_panel)) unique(fread(ctl_panel, select = "car_id")$car_id) else NULL

# PRE-P1: active-2019 + cleaning survivors, WITHOUT the ineligible legacy-forest filter
elig[, pre_p1 := in_sample_2019 == TRUE]
if (!is.null(kept_t)) elig[class != "never_eligible" & !car_id %in% kept_t, pre_p1 := FALSE]
if (!is.null(kept_c)) elig[class == "never_eligible" & !car_id %in% kept_c, pre_p1 := FALSE]
# POST-P1 is what stage 2 writes as final_sample
message("pre-P1 set:  ", elig[pre_p1 == TRUE, .N],
        "  | post-P1 (final_sample): ", elig[final_sample == TRUE, .N])
print(elig[pre_p1 == TRUE, .(pre_p1 = .N), by = class][
  elig[final_sample == TRUE, .(post_p1 = .N), by = class], on = "class"])

# ---- rates, erasure-adjusted, same basis as stage 2 -----------------------------
YRS <- 2005:2009
d <- rbindlist(lapply(YRS, function(y)
  fread(file.path(emp_dir, sprintf("parcel_defo_%d.csv", y)))))
adj <- fread(file.path(emp_dir, "erasure_adjustment.csv"))
d <- merge(d, adj[, .(car_id, year, er_defo_px, er_valid_px)],
           by = c("car_id", "year"), all.x = TRUE)
d[is.na(er_defo_px), er_defo_px := 0L]
d[is.na(er_valid_px), er_valid_px := 0L]
d[, defor_px := pmax(defor_px - er_defo_px, 0L)]
d[, valid_px := pmax(valid_px - er_valid_px, 0L)]
d[, rate := fifelse(valid_px > 0, defor_px / valid_px * 100, NA_real_)]

# the control group is measured on reserve-cleaned geometry (F3)
if (!is.null(kept_c)) {
  cp <- fread(ctl_panel)[year %in% YRS, .(car_id, year, rate = rate_legacyforest)]
  d <- rbind(d[!car_id %in% cp$car_id, .(car_id, year, rate)], cp)
}

summarise_set <- function(ids, label) {
  dd <- merge(d[car_id %in% ids], elig[, .(car_id, class)], by = "car_id")
  ym <- dd[, .(m = mean(rate, na.rm = TRUE)), by = .(class, year)]
  data.table(
    set = label,
    class = ym[year == 2005]$class,
    yr2005 = round(ym[year == 2005]$m, 1)
  )[, `:=`(
    avg0508 = round(ym[year <= 2008, .(a = mean(m)), by = class][match(class, class)]$a, 1),
    pooled0508 = round(dd[year <= 2008, .(a = mean(rate, na.rm = TRUE)), by = class][match(class, class)]$a, 1),
    avg0509 = round(ym[, .(a = mean(m)), by = class][match(class, class)]$a, 1),
    n = dd[year == 2005, .N, by = class][match(class, class)]$N
  )]
}

out <- rbind(
  summarise_set(elig[final_sample == TRUE, car_id], "post-P1 (stage 2 final_sample)"),
  summarise_set(elig[pre_p1 == TRUE, car_id], "pre-P1 (no :1704 filter)")
)
paper <- data.table(class = c("eligible", "ineligible", "never_eligible"),
                    paper = c(58.4, 11.4, 35.7))
out <- merge(out, paper, by = "class")
setorder(out, class, set)

cat("\n===== RATE BY AVERAGING WINDOW AND SET =====\n")
print(as.data.frame(out))
cat("\nTable 1: eligible 58.4 | ineligible 11.4 | never-eligible 35.7\n")
fwrite(out, file.path(emp_dir, "rate_window_test.csv"))
cat("\nWrote: rate_window_test.csv\n")
