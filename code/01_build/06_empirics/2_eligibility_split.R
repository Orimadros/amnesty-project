# Stage 2 of the empirics chain: build the paper's eligible / ineligible /
# never-eligible split, and compare the resulting summary statistics against
# Table 1 of the working paper.
#
# The paper (section 3.1) defines, conditional on being in a target area
# (undesignated federal land):
#
#   eligible = 1{in target area} x 1{occupation started <= 2004} x 1{area <= 1500 ha}
#
# and ineligible = in a target area but failing either condition. The control
# group ("never-eligible") is squatters in reservations / conservation units.
#
# Operationalised per the paper's appendices (see docs/notes/paper_appendix_specs.md):
#   "occupation began in year t"  ->  FIRST year the deforested share of the parcel's
#                                    legacy forest reaches 10%      (paper 2.3, Appendix C)
#   "ever occupied" sample        ->  deforested share >= 10% in 2014 (Appendix C step 1)
#   "<= 1500 ha"                  ->  deforested area <= 1500 AND area <= 1500
#                                                            (2_empirics.R:1228, 1310)
#
# NOTE: an earlier version of this script used a LEVEL test (rate_2004 > 10) instead of
# the first-crossing test. Those differ under reforestation -- a parcel that reached 15%
# by 1998 and regrew to 8% by 2004 is occupied-since-1998 to the paper but unoccupied to a
# 2004 level test. That misrouted small parcels into ineligible; see issue #E1 in
# docs/notes/paper_appendix_specs.md.
#
# NOTE on group names: our CAR scaffold's car_eligible_cleaned.shp is the paper's
# eligible+ineligible COMBINED (it applies no year/area test), and its
# car_ineligible_cleaned.shp is the paper's never-eligible control. See
# docs/notes/paper_vs_pipeline.md section 2.

library(data.table)
library(here)

OCCUPIED_RATE <- 10 # percent; legacy threshold for "in use"
AREA_CAP <- 1500 # hectares

emp_dir <- here("data", "intermediate", "empirics")

# Every year present on disk. The occupation test needs the full run up to 2004 to
# find the first crossing; 2008/2014 supply the outcomes.
all_files <- list.files(emp_dir, pattern = "^parcel_defo_[0-9]{4}\\.csv$", full.names = TRUE)
have <- sort(as.integer(gsub("\\D", "", basename(all_files))))
needed_outcome <- c(2008, 2014)
if (!all(needed_outcome %in% have)) {
  stop("Missing outcome year(s): ",
       paste(setdiff(needed_outcome, have), collapse = ", "))
}
occ_years <- have[have <= 2004]
if (length(occ_years) < 2) {
  stop("Only ", length(occ_years), " year(s) <= 2004 on disk (",
       paste(occ_years, collapse = ", "), ").\n",
       "The first-crossing occupation test needs the run from 1987; ",
       "see docs/notes/paper_appendix_specs.md issue #E1.")
}
message("occupation years available: ", min(occ_years), "-", max(occ_years),
        " (", length(occ_years), " years)")

d <- rbindlist(lapply(all_files, fread))
message("loaded ", nrow(d), " parcel-year rows")

# ---- occupation: first year the deforested share reaches 10% -------------------
occ <- d[year %in% occ_years & !is.na(deforestation_rate) &
           deforestation_rate >= OCCUPIED_RATE,
         .(occupation_year = min(year)), by = car_id]
message("parcels ever reaching ", OCCUPIED_RATE, "% by ", max(occ_years), ": ", nrow(occ))

w <- dcast(d[year %in% c(2004, needed_outcome)], car_id + group + area_ha ~ year,
           value.var = c("deforested_area_ha", "deforestation_rate"))
setnames(w, gsub("^deforested_area_ha_", "defor_ha_", names(w)))
setnames(w, gsub("^deforestation_rate_", "rate_", names(w)))
w <- merge(w, occ, by = "car_id", all.x = TRUE)

# ---- eligibility --------------------------------------------------------------
w[, occupied_by_2004 := !is.na(occupation_year) & occupation_year <= 2004]
w[, small := !is.na(area_ha) & area_ha <= AREA_CAP &
     !is.na(defor_ha_2004) & defor_ha_2004 <= AREA_CAP]

w[, class := fifelse(
  group == "never_eligible", "never_eligible",
  fifelse(occupied_by_2004 & small, "eligible", "ineligible")
)]

# Appendix C step 1: drop properties with < 10% deforested area in 2014.
w[, in_sample := group == "never_eligible" |
    (!is.na(rate_2014) & rate_2014 >= OCCUPIED_RATE)]

fwrite(w, file.path(emp_dir, "parcel_eligibility.csv"))
message("Wrote: ", file.path(emp_dir, "parcel_eligibility.csv"))

# ---- summary ------------------------------------------------------------------
s <- w[in_sample == TRUE, .(
  n_properties = .N,
  defor_Mha_2008 = round(sum(defor_ha_2008, na.rm = TRUE) / 1e6, 3),
  defor_Mha_2014 = round(sum(defor_ha_2014, na.rm = TRUE) / 1e6, 3),
  mean_rate_2008 = round(mean(rate_2008, na.rm = TRUE), 1),
  mean_defor_ha_2008 = round(mean(defor_ha_2008, na.rm = TRUE), 2),
  mean_area_ha = round(mean(area_ha, na.rm = TRUE), 1)
), by = class]
s[, pct_change_defor := round((defor_Mha_2014 / defor_Mha_2008 - 1) * 100, 1)]
setorder(s, class)

cat("\n================ OUR REBUILD ================\n")
print(as.data.frame(s))

# ---- paper's Table 1 / section 3.2 figures ------------------------------------
paper <- data.table(
  class = c("eligible", "ineligible", "never_eligible"),
  p_n_properties = c(NA, 15000, NA),
  p_defor_Mha_2008 = c(5.10, 4.10, NA),
  p_defor_Mha_2014 = c(5.27, 4.66, NA),
  p_mean_rate_2008 = c(58.4, 11.4, NA),
  p_mean_defor_ha_2008 = c(69.03, 204.3, NA),
  p_pct_change_defor = c(NA, NA, 11.0)
)

cmp <- merge(s, paper, by = "class", all = TRUE)
cat("\n================ VS THE PAPER ================\n")
for (cl in c("eligible", "ineligible", "never_eligible")) {
  r <- cmp[class == cl]
  if (nrow(r) == 0) next
  cat("\n--", cl, "--\n")
  show <- function(lab, ours, theirs, unit = "") {
    if (is.na(theirs)) {
      cat(sprintf("  %-24s ours %10s   paper %10s\n", lab, format(ours), "-"))
    } else {
      d <- if (!is.na(ours) && theirs != 0) sprintf("%+.0f%%", (ours / theirs - 1) * 100) else "n/a"
      cat(sprintf("  %-24s ours %10s   paper %10s   diff %8s\n",
                  lab, format(ours), format(theirs), d))
    }
  }
  show("n properties", r$n_properties, r$p_n_properties)
  show("deforested Mha 2008", r$defor_Mha_2008, r$p_defor_Mha_2008)
  show("deforested Mha 2014", r$defor_Mha_2014, r$p_defor_Mha_2014)
  show("mean rate 2008 (%)", r$mean_rate_2008, r$p_mean_rate_2008)
  show("mean defor ha 2008", r$mean_defor_ha_2008, r$p_mean_defor_ha_2008)
  show("% change 2008->2014", r$pct_change_defor, r$p_pct_change_defor)
}

fwrite(cmp, file.path(emp_dir, "table1_comparison.csv"))
cat("\nWrote: ", file.path(emp_dir, "table1_comparison.csv"), "\n", sep = "")
