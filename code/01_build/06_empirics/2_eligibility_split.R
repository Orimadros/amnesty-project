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

# ---- apply the conflict algorithm's erasure, if it has been computed -----------
# Legacy resolves conflicts BEFORE the eligibility split, so its `area <= 1500` and
# `occupied by 2004` tests see the SHRUNKEN polygons (docs/notes/code_diff_vs_legacy.md
# D1+D2). Re-running this stage after 4b reproduces that order: the first run supplies
# in_sample for resolution, this run classifies on the adjusted values.
adj_f <- file.path(emp_dir, "erasure_adjustment.csv")
if (file.exists(adj_f)) {
  adj <- fread(adj_f)
  n_adj <- uniqueN(adj$car_id)
  d <- merge(d, adj[, .(car_id, year, er_defo_px, er_valid_px, erased_ha)],
             by = c("car_id", "year"), all.x = TRUE)
  d[is.na(er_defo_px), er_defo_px := 0L]
  d[is.na(er_valid_px), er_valid_px := 0L]
  d[, defor_px := pmax(defor_px - er_defo_px, 0L)]
  d[, valid_px := pmax(valid_px - er_valid_px, 0L)]
  d[, deforested_area_ha := defor_px * 0.09]
  d[, rate_raw := deforestation_rate]  # pre-erasure, for the 2014 occupancy filter
  d[, deforestation_rate := fifelse(valid_px > 0, defor_px / valid_px * 100, NA_real_)]
  ea <- unique(adj[!is.na(erased_ha), .(car_id, erased_ha)], by = "car_id")
  d <- merge(d, ea, by = "car_id", all.x = TRUE, suffixes = c("", ".y"))
  d[!is.na(erased_ha) & erased_ha > 0, area_ha := pmax(area_ha - erased_ha, 0)]
  message("ERASURE APPLIED to ", n_adj, " parcels (areas and deforestation reduced)")
} else {
  message("no erasure adjustment on disk -- classifying on unmodified geometry")
}

# ---- occupation: first year the deforested share reaches 10% -------------------
occ <- d[year %in% occ_years & !is.na(deforestation_rate) &
           deforestation_rate >= OCCUPIED_RATE,
         .(occupation_year = min(year)), by = car_id]
message("parcels ever reaching ", OCCUPIED_RATE, "% by ", max(occ_years), ": ", nrow(occ))

if (!"rate_raw" %in% names(d)) d[, rate_raw := deforestation_rate]
w <- dcast(d[year %in% c(2004, needed_outcome)], car_id + group + area_ha ~ year,
           value.var = c("deforested_area_ha", "deforestation_rate", "rate_raw"))
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

# Appendix C step 1: drop properties with < 10% deforested area in 2014. Legacy applies
# this BEFORE conflict resolution (2_empirics.R:636), so it must use the PRE-erasure
# rate; only the eligibility tests above see the shrunken geometry.
# This applies to the CONTROL group too. Table 1's note is explicit: "Until 2008, all
# of these rural parcels illegally occupied public land in the Amazon" -- never-eligible
# parcels are occupied squatters, not every CAR that happens to touch a reserve. An
# earlier version exempted them, which left the control group at 13,025 parcels
# averaging 3,980 ha (paper: 7,049 at 760 ha). Applying the filter brings it to 6,140
# at 1,093 ha.
w[, in_sample := !is.na(rate_raw_2014) & rate_raw_2014 >= OCCUPIED_RATE]

# Legacy 2_empirics.R:1704 applies an extra filter to the ineligible group:
#   inelegible <- inelegible %>% filter(!is.na(area) & area < 100000)
# where `area` is the rate denominator, deforested/(rate/100) -- i.e. legacy forest.
# The <100000 clause is nearly inert; the !is.na clause drops zero-rate parcels.
w[, lf_ha := fifelse(!is.na(rate_2008) & rate_2008 > 0,
                     defor_ha_2008 / (rate_2008 / 100), NA_real_)]
drop_inelig <- w$class == "ineligible" & (is.na(w$lf_ha) | w$lf_ha >= 1e5)
message("legacy ineligible filter drops ", sum(drop_inelig & w$in_sample), " in-sample parcels")
w[drop_inelig == TRUE, in_sample := FALSE]

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
  # Actual Table 1: "CHARACTERISTICS OF OCCUPATIONS IN TARGET AND CONTROL AREAS".
  # Rate is defined in the table note as deforested / claim area, with areas taken
  # from the boundaries submitted by occupants (i.e. declared, not geometric).
  class = c("eligible", "ineligible", "never_eligible"),
  p_n_properties = c(71171, 15254, 7049),
  p_mean_area_ha = c(143, 661, 760),
  p_mean_rate_2008 = c(58.4, 11.4, 35.7),
  p_defor_Mha_2008 = c(5.1, 4.1, 2.0),
  p_defor_Mha_2014 = c(5.3, 4.7, 2.2),
  p_pct_change_defor = c(6.3, 15.6, 11.5)
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
  show("mean area ha", r$mean_area_ha, r$p_mean_area_ha)
  show("% change 2008->2014", r$pct_change_defor, r$p_pct_change_defor)
}

fwrite(cmp, file.path(emp_dir, "table1_comparison.csv"))
cat("\nWrote: ", file.path(emp_dir, "table1_comparison.csv"), "\n", sep = "")
