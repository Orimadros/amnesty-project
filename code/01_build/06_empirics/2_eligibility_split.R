# Stage 2 of the empirics chain: build the paper's eligible / ineligible /
# never-eligible split, and compare the resulting summary statistics against
# Table 1 of the working paper.
#
# As of 2026-07-31 this carries the full replication stack from
# docs/notes/paper_legacy_method_diffs.md. Sample columns written, weakest first:
#   in_sample      the paper's stated rule (occupied by 2014) + the 2008-basis
#                  ineligible filter -- kept because stages 4/4b consume it and the
#                  stage2 -> 4 -> 4b -> stage2 ordering depends on it
#   basis_sample   EMP_SAMPLE_YEAR's rule (default 2019 = as legacy ran it, F2)
#                  + the 2005-basis ineligible filter (P1)
#   final_sample   basis_sample minus the conflict algorithm's drops (N0/N1)
# The Table 1 comparison reports pre-2009 average rates (F1) on final_sample, with
# never-eligible measured on legacy's reserve-cleaned geometry when stage 13 has
# produced that panel (F3).
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

# The "ever occupied" sample filter. The paper's appendices say 2014; the code that
# produced the published tables measures it on the 2019 raster (finding F2, see
# docs/notes/paper_legacy_method_diffs.md). Both flags are computed and written; the
# one the headline comparison uses is EMP_SAMPLE_YEAR (default 2019 = as-executed).
SAMPLE_YEARS <- c(2014, 2017, 2019)
BASIS_YEAR <- as.integer(Sys.getenv("EMP_SAMPLE_YEAR", unset = "2019"))
if (!BASIS_YEAR %in% SAMPLE_YEARS) {
  stop("EMP_SAMPLE_YEAR must be one of ", paste(SAMPLE_YEARS, collapse = " / "))
}
PRE_YEARS <- 2005:2008 # Table 1/2's "average prior to 2009" window (finding F1)

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
  # Take only the per-year pixel adjustments here. `erased_ha` is per-PARCEL and is
  # merged below; pulling it in on (car_id, year) too would leave the year-specific
  # copy in place (the later merge suffixes the parcel-level one to .y), so any year
  # absent from erasure_adjustment.csv -- e.g. the 2019 sample year -- would keep its
  # full area while 1987-2014 shrank. That splits a parcel across two dcast rows and
  # silently blanks its outcome columns.
  d <- merge(d, adj[, .(car_id, year, er_defo_px, er_valid_px)],
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
# Reshape only the outcome years. The sample-year flags are taken straight from the
# long panel below, so a year that the erasure adjustment does not cover can never
# perturb this grouping.
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

# F2: the same filter as legacy actually ran it -- on a post-sample raster, strict >,
# measured on raw (pre-erasure) rates like the 2014 flag above. 2019 is what the code
# reads; 2017 is what its comment says it meant to use ("change to 2017"), so both
# are computed when the data is on disk.
for (sy in setdiff(SAMPLE_YEARS, 2014)) {
  col <- paste0("in_sample_", sy)
  if (sy %in% have) {
    sr <- d[year == sy, .(car_id, r = rate_raw)]
    setnames(sr, "r", paste0("rate_raw_", sy))
    w <- merge(w, unique(sr, by = "car_id"), by = "car_id", all.x = TRUE)
    w[, (col) := !is.na(get(paste0("rate_raw_", sy))) &
        get(paste0("rate_raw_", sy)) > OCCUPIED_RATE]
  } else {
    w[, (col) := NA]
    message("NOTE: no parcel_defo_", sy, ".csv on disk -- ", sy, " basis unavailable")
  }
}

# Legacy 2_empirics.R:1704 applies an extra filter to the ineligible group:
#   inelegible <- inelegible %>% filter(!is.na(area) & area < 100000)
# where `area` is the rate denominator, deforested/(rate/100) -- i.e. legacy forest.
# P1: that column is created ONLY in the 2005 block and survives every later join,
# so it is the 2005 legacy-forest area, and the !is.na clause silently drops every
# parcel with no 2005 deforestation (0/0 -> NaN -> NA). lf_ha (2008) is retained for
# continuity with the earlier comparison.
lf05 <- d[year == 2005, .(car_id,
  lf05_ha = fifelse(defor_px > 0 & valid_px > 0, valid_px * 0.09, NA_real_))]
w <- merge(w, lf05, by = "car_id", all.x = TRUE)
w[, lf_ha := fifelse(!is.na(rate_2008) & rate_2008 > 0,
                     defor_ha_2008 / (rate_2008 / 100), NA_real_)]
drop_inelig <- w$class == "ineligible" & (is.na(w$lf_ha) | w$lf_ha >= 1e5)
message("legacy ineligible filter (2008 basis) drops ",
        sum(drop_inelig & w$in_sample), " in-sample parcels")
w[drop_inelig == TRUE, in_sample := FALSE]

drop_inelig05 <- w$class == "ineligible" & (is.na(w$lf05_ha) | w$lf05_ha >= 1e5)
w[, basis_sample := if (BASIS_YEAR == 2014)
    (!is.na(rate_raw_2014) & rate_raw_2014 >= OCCUPIED_RATE)
  else get(paste0("in_sample_", BASIS_YEAR))]
w[, prefilter_sample := basis_sample] # the active pool before P1 (Table 1 basis)
message("P1 (2005 basis) drops ", sum(drop_inelig05 & w$basis_sample, na.rm = TRUE),
        " ineligible parcels from the ", BASIS_YEAR, " basis sample")
w[drop_inelig05 == TRUE, basis_sample := FALSE]

# N0/N1: the conflict algorithm's DROP decisions must reach the sample. Legacy
# removes dropped parcels from the per-municipality cleaned files BEFORE the
# eligibility tests, so its Table 1 never counts them. Prefer the legacy-faithful
# 2004-rule cleaning (stage 12); fall back to the 2014-rule run (stage 4).
res04 <- file.path(emp_dir, "parcels_resolved_2004rules.csv")
res14 <- file.path(emp_dir, "parcels_resolved_2014.csv")
w[, cleaning_basis := NA_character_]
if (file.exists(res04)) {
  keep_t <- fread(res04, select = "car_id")$car_id
  w[, cleaning_basis := "2004rules"]
} else if (file.exists(res14)) {
  keep_t <- fread(res14, select = "car_id")$car_id
  w[, cleaning_basis := "2014rules"]
} else {
  keep_t <- NULL
  message("NOTE: no conflict-resolution output on disk -- drops NOT applied")
}
# F3: the control pool is cleaned by legacy's reserve-only algorithm, which never
# drops a parcel; stage 13 caches the survivors it could keep as valid geometry.
ctl_panel <- file.path(emp_dir, "control_cleaned_panel_full.csv")
keep_c <- if (file.exists(ctl_panel)) unique(fread(ctl_panel, select = "car_id")$car_id) else NULL

w[, final_sample := basis_sample]
if (!is.null(keep_t)) {
  w[class != "never_eligible" & !car_id %in% keep_t, final_sample := FALSE]
}
if (!is.null(keep_c)) {
  w[class == "never_eligible" & !car_id %in% keep_c, final_sample := FALSE]
}
message("final_sample (", BASIS_YEAR, " basis + P1 + cleaning drops): ",
        sum(w$final_sample, na.rm = TRUE), " parcels")

# Table 1 is reported WITHOUT P1 (stage 17, 2026-08-01). The :1704 filter removes
# exactly the parcels with zero 2005 deforestation, whose rate is a well-defined 0 --
# so dropping them lifts the ineligible mean. On the pre-filter set the paper's own
# stated window ("average prior to 2009") gives 12.0 vs its 11.4 (+5%); on the
# post-filter panel the same window gives 16.7 (+46%). Both of that column's errors
# then point the same way as every other group's, instead of N being the lone
# negative. final_sample (with P1) is retained for the DiD and for anything legacy
# built off its filtered panel.
w[, table1_sample := prefilter_sample]
if (!is.null(keep_t)) {
  w[class != "never_eligible" & !car_id %in% keep_t, table1_sample := FALSE]
}
if (!is.null(keep_c)) {
  w[class == "never_eligible" & !car_id %in% keep_c, table1_sample := FALSE]
}
message("table1_sample (", BASIS_YEAR, " basis + cleaning drops, NO P1): ",
        sum(w$table1_sample, na.rm = TRUE), " parcels")

fwrite(w, file.path(emp_dir, "parcel_eligibility.csv"))
message("Wrote: ", file.path(emp_dir, "parcel_eligibility.csv"))

# ---- summary ------------------------------------------------------------------
# F1: Table 1/2's rate is the average PRIOR TO 2009, not the 2008 value (Table 2's
# notes state this explicitly). The headline column is the mean of the yearly
# cross-sectional means over 2005-2008; 2005-only and 2008-only are printed beside
# it because the paper's ineligible figure tracks the 2005 value most closely.
# F3: never-eligible quantities come from the reserve-cleaned panel when stage 13
# has produced it -- legacy measures that pool on erased geometry.
pre <- d[year %in% PRE_YEARS & car_id %in% w[table1_sample == TRUE, car_id],
         .(car_id, year, rate = deforestation_rate)]
pre <- merge(pre, w[, .(car_id, class)], by = "car_id")
if (!is.null(keep_c) && file.exists(ctl_panel)) {
  cp <- fread(ctl_panel)[year %in% PRE_YEARS & car_id %in% w[table1_sample == TRUE, car_id],
                         .(car_id, year, rate = rate_legacyforest)]
  cp <- merge(cp, w[, .(car_id, class)], by = "car_id")
  pre <- rbind(pre[class != "never_eligible"], cp[class == "never_eligible"])
  message("control rates taken from the reserve-cleaned panel (F3)")
}
rate_tab <- merge(
  pre[, .(m = mean(rate, na.rm = TRUE)), by = .(class, year)][
    , .(mean_rate_pre2009 = round(mean(m), 1)), by = class],
  pre[year == 2005, .(mean_rate_2005 = round(mean(rate, na.rm = TRUE), 1)), by = class],
  by = "class", all = TRUE)

s <- w[table1_sample == TRUE, .(
  n_properties = .N,
  defor_Mha_2008 = round(sum(defor_ha_2008, na.rm = TRUE) / 1e6, 3),
  defor_Mha_2014 = round(sum(defor_ha_2014, na.rm = TRUE) / 1e6, 3),
  mean_rate_2008 = round(mean(rate_2008, na.rm = TRUE), 1),
  mean_defor_ha_2008 = round(mean(defor_ha_2008, na.rm = TRUE), 2),
  mean_area_ha = round(mean(area_ha, na.rm = TRUE), 1)
), by = class]
s <- merge(s, rate_tab, by = "class", all.x = TRUE)

# The control column's TOTALS must come from the cleaned geometry too, or they would
# be measured on a different basis than its rates.
if (!is.null(keep_c) && file.exists(ctl_panel)) {
  cpa <- fread(ctl_panel)[car_id %in% w[table1_sample == TRUE & class == "never_eligible", car_id]]
  s[class == "never_eligible", `:=`(
    defor_Mha_2008 = round(cpa[year == 2008, sum(deforested_area_ha, na.rm = TRUE)] / 1e6, 3),
    defor_Mha_2014 = round(cpa[year == 2014, sum(deforested_area_ha, na.rm = TRUE)] / 1e6, 3),
    mean_defor_ha_2008 = round(cpa[year == 2008, mean(deforested_area_ha, na.rm = TRUE)], 2)
  )]
  message("control totals taken from the reserve-cleaned panel (F3)")
}
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
  show("mean rate pre-2009 (%)", r$mean_rate_pre2009, r$p_mean_rate_2008)
  show("  .. 2005 only", r$mean_rate_2005, r$p_mean_rate_2008)
  show("  .. 2008 only", r$mean_rate_2008, r$p_mean_rate_2008)
  show("mean area ha", r$mean_area_ha, r$p_mean_area_ha)
  show("% change 2008->2014", r$pct_change_defor, r$p_pct_change_defor)
}

fwrite(cmp, file.path(emp_dir, "table1_comparison.csv"))
cat("\nWrote: ", file.path(emp_dir, "table1_comparison.csv"), "\n", sep = "")
