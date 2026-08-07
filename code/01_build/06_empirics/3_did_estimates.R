# Stage 3 of the empirics chain: the paper's two difference-in-differences
# regressions, compared against its reported coefficients.
#
# Paper §3.1, equation (2):
#   y_it = beta * x_i * 1{t >= 2009} + upsilon_i + upsilon_t + eps_it
# with parcel and year fixed effects, standard errors clustered at the state
# level, over the data period 2005-2014.
#
# Two regressions, both against the never-eligible control (§3.1):
#   did1: eligible   vs never-eligible  -> paper reports beta ~ -1.4 p.p.
#   did2: ineligible vs never-eligible  -> paper reports beta = +4.2 p.p.
#
# Outcome. Table 1's note defines the rate as "the share of a property claim's area
# that has been deforested" (claim area as declared), but the code that produced the
# table computes deforested pixels / non-zero pixels -- the LEGACY-FOREST denominator
# -- and that is what our Table 1 reproduction matches. rate_legacyforest is
# therefore the primary outcome here; rate_claim is reported beside it.
#
# rate_claim is also not summarisable as a mean: declared areas include 256 eligible
# parcels under 1 ha, so the ratio has a tail reaching ~10^8 percent (median 50.8,
# p99 102.4, mean 1325). Its beta is still meaningful -- the FE estimator differences
# within parcel -- but read its pre_mean_treated column as junk, not as a baseline.
#
# Sample basis. As of 2026-07-31 this runs on stage 2's `final_sample`, which carries
# the full set of replication findings (docs/notes/paper_legacy_method_diffs.md):
# F2 (the "ever occupied" filter as legacy ran it, on the 2019 raster), P1 (the
# ineligible filter on the 2005 legacy-forest area), N0/N1 (the conflict algorithm's
# drop decisions, evaluated with 2004 deforestation), and F3 (the control pool
# measured on legacy's reserve-cleaned geometry). Set EMP_RESOLVED=0 to estimate
# without the cleaning drops -- the paper claims in §2.3 that results carry either
# way, and that claim is worth testing directly.
#
# Do-file filters (2026-08-07, from the recovered empirics_amazon_final.do -- the
# paper's ACTUAL regression code, data/legacy_dropbox/miseEnPlace/). Three
# undocumented sample filters, on by default here (EMP_DOFILE_FILTERS=0 disables):
#   :25  drop eligible parcels whose MIN pre-2009 rate < 10 (applied globally,
#        before every regression);
#   :26  sa flag: control parcels with 2009 deforested AREA < 5 ha (the do-file
#        misnames the variable defo_rate_2009 -- it is the area). Excluded from
#        the INELIGIBLE regressions only (Table 2's `sa == .`); the eligible
#        regression (Table 1, :47) uses the unflagged control;
#   :65  the ineligible regressions keep MAX pre-2009 rate < 85 -- the `if` clause
#        applies this to BOTH groups, so it filters the control there too.
# NA semantics follow stage 19, validated on their own panels to 0.2%: missing
# min-pre-rate drops an eligible parcel; missing max-pre-rate fails the <85 keep;
# missing 2009 area leaves sa unset (parcel kept).
# Consequence: the final do-file uses NEITHER the zero-2014 drop (D-A) nor
# winsorization (D-B) -- both belong to the superseded did1_new/did2_new export
# path -- so their defaults flipped to OFF on 2026-08-07 (flags remain).
#
# EMP_PANEL=recovered swaps the input for stage 19's rebuild of Pedro's own
# per-year panels (data/legacy_dropbox/output_full/rebuilt_did_panel.rds) and runs
# the IDENTICAL filter + estimation path -- a spec-identical comparison that
# isolates data vintage from specification. Their panels carry no declared area,
# so rate_claim is unavailable there.

library(data.table)
library(here)

# lfe/fixest are not in renv.lock and lfe will not compile on this host (no Fortran
# toolchain), so the two-way FE estimator is implemented directly. See
# _helpers_twfe.R for the method and its self-test.
source(here("code", "01_build", "06_empirics", "_helpers_twfe.R"))

if (!twfe_selftest()) stop("twfe self-test failed -- do not trust these estimates")

DID_YEARS <- 2005:2014
POST_FROM <- 2009

emp_dir <- here("data", "intermediate", "empirics")

PANEL <- Sys.getenv("EMP_PANEL", unset = "ours")

if (PANEL == "recovered") {
  # ---- panel: stage 19's rebuild of Pedro's own per-year panels ----------------
  rp_f <- here("data", "legacy_dropbox", "output_full", "rebuilt_did_panel.rds")
  if (!file.exists(rp_f)) stop("Missing ", rp_f, " -- run 19_rebuild_did_from_recovered.R first.")
  rp <- as.data.table(readRDS(rp_f))
  d <- rp[, .(car_id = COD_IMO, year,
              deforested_area_ha = defo, rate_legacyforest = rate,
              class = fcase(group == "eligible",       "eligible",
                            group == "inelegible",     "ineligible",
                            group == "never eligible", "never_eligible"))]
  # Their panels carry defo + rate only; the declared-area outcome does not exist.
  d[, rate_claim := NA_real_]
  message("EMP_PANEL=recovered: ", uniqueN(d$car_id),
          " parcels from Pedro's rebuilt panel (rate_claim unavailable)")
} else {

elig_f <- file.path(emp_dir, "parcel_eligibility.csv")
if (!file.exists(elig_f)) stop("Missing ", elig_f, " -- run 2_eligibility_split.R first.")

files <- file.path(emp_dir, paste0("parcel_defo_", DID_YEARS, ".csv"))
absent <- files[!file.exists(files)]
if (length(absent) > 0) {
  stop("Missing year panel(s) for the DiD window:\n",
       paste0(" - ", absent, collapse = "\n"))
}

# ---- panel -------------------------------------------------------------------
# Stage 2 now writes `final_sample`: the 2019-basis "ever occupied" filter (F2), the
# 2005-basis ineligible filter (P1), and the conflict algorithm's drop decisions
# (N0/N1). EMP_RESOLVED=0 falls back to `basis_sample` -- the same sample with the
# cleaning drops NOT applied; the paper claims in 2.3 that results carry either way.
ecols <- names(fread(elig_f, nrows = 0))
want <- intersect(c("car_id", "class", "in_sample", "basis_sample", "final_sample"), ecols)
elig <- fread(elig_f, select = want)
use_resolved <- Sys.getenv("EMP_RESOLVED", unset = "1") != "0"
sample_col <- if (use_resolved && "final_sample" %in% want) "final_sample" else
              if ("basis_sample" %in% want) "basis_sample" else "in_sample"
elig <- elig[get(sample_col) == TRUE, .(car_id, class)]
message("sample column: ", sample_col, " -> ", nrow(elig), " parcels")

# L5 / D-C: legacy has a block (2_empirics.R:2701-2718) that measures the DiD control
# on `ccar_clean_inReservas` -- the raw >1%-overlap reserve pool, no occupation
# filter, no cleaning -- rather than Table 1's filtered `control_final`. TESTED
# 2026-08-01 and REJECTED as the path that produced Table 2: with the raw pool the
# eligible coefficient goes to +0.103 (sign flipped, paper -1.412) and ineligible to
# +7.076 (paper +4.204), whereas the filtered control reproduces -1.400 vs -1.412.
# That block sits in the superseded `amazon_working/` vintage (different file
# prefixes), so it is scratch, not the published path. Default OFF; EMP_RAW_CONTROL=1
# reruns the rejected variant.
RAW_CONTROL <- Sys.getenv("EMP_RAW_CONTROL", unset = "0") != "0"
if (RAW_CONTROL) {
  all_ctl <- fread(elig_f, select = c("car_id", "class"))[class == "never_eligible"]
  before <- nrow(elig[class == "never_eligible"])
  elig <- rbind(elig[class != "never_eligible"], all_ctl)
  message("L5: raw control pool (no occupation filter, no cleaning): ",
          before, " -> ", nrow(all_ctl), " parcels")
}

d <- rbindlist(lapply(files, fread))
d <- merge(d, elig, by = "car_id")

# F3: for the TABLE 1 control, legacy measures the reserve-cleaned geometry. Swap in
# that panel across the WHOLE window -- cleaned geometry pre-2009 with uncleaned
# post-2009 would manufacture a break at the treatment date. Skipped under
# EMP_RAW_CONTROL, where the point is precisely that legacy's DiD control is NOT
# cleaned; mixing the two would be incoherent.
ctl_panel <- file.path(emp_dir, "control_cleaned_panel_full.csv")
if (RAW_CONTROL) {
  message("L5: control on RAW uncleaned geometry (F3 panel not applied)")
} else if (file.exists(ctl_panel)) {
  cp <- fread(ctl_panel)[year %in% DID_YEARS &
                         car_id %in% elig[class == "never_eligible", car_id]]
  cp <- cp[, .(car_id, year, deforested_area_ha,
               deforestation_rate = rate_legacyforest,
               area_ha = declared_ha, class = "never_eligible")]
  before <- uniqueN(d[class == "never_eligible", car_id])
  d <- rbind(d[class != "never_eligible"], cp, fill = TRUE)
  message("control outcomes from the reserve-cleaned panel (F3): ",
          before, " -> ", uniqueN(cp$car_id), " parcels")
} else {
  message("NOTE: no reserve-cleaned control panel -- control on uncleaned geometry")
}

d[, rate_claim := fifelse(area_ha > 0, deforested_area_ha / area_ha * 100, NA_real_)]
setnames(d, "deforestation_rate", "rate_legacyforest")

}  # end PANEL == "ours"

# State is the CAR id prefix (e.g. "MT-5101704-0244..."), so no extra join needed.
d[, state := substr(car_id, 1, 2)]
d <- d[grepl("^[A-Z]{2}$", state)]

# ---- do-file sample filters (empirics_amazon_final.do :25/:26/:65) -----------
# Per-parcel filter variables, computed on the assembled panel (so, for the
# control, on the same reserve-cleaned geometry the outcomes use). Semantics in
# the header; the eligible drop applies here, the ineligible-regression filters
# apply inside run_did/baseline for that comparison only.
DOFILE_FILTERS <- Sys.getenv("EMP_DOFILE_FILTERS", unset = "1") != "0"
pre <- d[year < POST_FROM,
  .(rate_min_pre = suppressWarnings(min(rate_legacyforest, na.rm = TRUE)),
    rate_max_pre = suppressWarnings(max(rate_legacyforest, na.rm = TRUE))),
  by = car_id]
pre[!is.finite(rate_min_pre), rate_min_pre := NA]
pre[!is.finite(rate_max_pre), rate_max_pre := NA]
d <- merge(d, pre, by = "car_id", all.x = TRUE)
d <- merge(d, unique(d[year == 2009, .(car_id, defo_area_2009 = deforested_area_ha)],
                     by = "car_id"),
           by = "car_id", all.x = TRUE)
d[, sa := class == "never_eligible" & !is.na(defo_area_2009) & defo_area_2009 < 5]
if (DOFILE_FILTERS) {
  before <- uniqueN(d[class == "eligible", car_id])
  d <- d[!(class == "eligible" & (is.na(rate_min_pre) | rate_min_pre < 10))]
  message(":25 eligible min-pre-rate<10 dropped: ", before, " -> ",
          uniqueN(d[class == "eligible", car_id]), " parcels")
  message(":26 sa-flagged control parcels (2009 defo area < 5 ha, ineligible ",
          "regressions only): ", uniqueN(d[sa == TRUE, car_id]))
}

# D-A (legacy 2_empirics.R:2773/2841): the DiD sample drops parcels with ZERO
# deforested area in 2014, from the control and ineligible groups. Legacy applies it
# to `control` and `spillover` (its name for ineligible) but NOT to `eligible`; that
# asymmetry is reproduced here rather than tidied. Undocumented in the paper.
# DEFAULT OFF since 2026-08-07: the recovered final do-file runs on did.dta, which
# has no zero-2014 drop -- D-A belongs to the superseded did1_new/did2_new export.
if (Sys.getenv("EMP_DROP_ZERO2014", unset = "0") != "0") {
  z <- d[year == 2014 & deforested_area_ha == 0 & class != "eligible", unique(car_id)]
  before <- uniqueN(d$car_id)
  d <- d[!car_id %in% z]
  message("D-A: dropped ", length(z), " zero-2014 parcels (control+ineligible): ",
          before, " -> ", uniqueN(d$car_id), " parcels")
}

d[, post := as.integer(year >= POST_FROM)]

# D-B (legacy :2883/2887): the exported DiD outcome is WINSORIZED at 1/99 within
# variable-year before estimation (`value_w`), written to did1_new/did2_new.dta.
# DEFAULT OFF since 2026-08-07: the recovered final do-file regresses RAW `value`
# from did.dta -- the winsorized export is the superseded path. EMP_WINSOR=1 restores.
WINSOR <- Sys.getenv("EMP_WINSOR", unset = "0") != "0"
if (WINSOR) {
  wz <- function(x) {
    q <- quantile(x, c(0.01, 0.99), na.rm = TRUE, names = FALSE)
    pmin(pmax(x, q[1]), q[2])
  }
  for (v in c("rate_claim", "rate_legacyforest")) {
    d[, (v) := wz(get(v)), by = year]
  }
  message("D-B: outcomes winsorized at 1/99 within year")
}

message("panel: ", nrow(d), " parcel-years | ",
        uniqueN(d$car_id), " parcels | ", uniqueN(d$state), " states")
print(d[, .(parcels = uniqueN(car_id)), by = class])

# ---- estimation --------------------------------------------------------------
# The do-file's regression samples differ by comparison: the eligible regression
# (:47) takes the panel as-is after the :25 drop; the ineligible regressions (:65)
# additionally exclude sa-flagged control parcels and keep max-pre-rate < 85 on
# BOTH groups.
comparison_sample <- function(treated_class) {
  s <- d[class %in% c(treated_class, "never_eligible")]
  if (DOFILE_FILTERS && treated_class == "ineligible") {
    s <- s[sa == FALSE & !is.na(rate_max_pre) & rate_max_pre < 85]
  }
  s
}

run_did <- function(treated_class, outcome) {
  s <- comparison_sample(treated_class)[!is.na(get(outcome))]
  s[, treat := as.integer(class == treated_class)]
  s[, treat_post := treat * post]

  fit <- twfe(s[[outcome]], s$treat_post, s$car_id, s$year, s$state)
  data.table(
    comparison = paste0(treated_class, " vs never_eligible"),
    outcome = outcome,
    beta_pp = round(fit$beta, 3),
    se = round(fit$se, 3),
    t = round(fit$t, 2),
    p = signif(fit$p, 3),
    n_obs = fit$n_obs,
    n_parcels = fit$n_units,
    n_clusters = fit$n_clusters,
    pre_mean_treated = round(
      mean(s[treat == 1 & post == 0, get(outcome)], na.rm = TRUE), 2
    )
  )
}

# rate_claim is undefined on the recovered panel (no declared areas there).
outcomes <- if (PANEL == "recovered") "rate_legacyforest" else
            c("rate_claim", "rate_legacyforest")

res <- rbindlist(lapply(
  outcomes,
  function(o) rbindlist(lapply(c("eligible", "ineligible"), run_did, outcome = o))
))

cat("\n================ DiD ESTIMATES ================\n")
print(as.data.frame(res))

# ---- do-file baselines (Table-1 style) ---------------------------------------
# The do-file's baselines are `sum value if ... & y < 2009` on the filtered
# regression sample: POOLED parcel-year means of the rate (variable == 2) and the
# deforested area (variable == 1) for the treated group. NOTE (2026-08-07): the
# rate line corresponds to Table 1's printed rates (58.4 / 11.4), but the AREA
# line does NOT -- the pooled cumulative-area mean is ~66 ha on Pedro's own panel
# vs the printed 5.1, so Table 1's area rows come from something else (stage 2
# reproduces them to ~2%). pre_defo_area is reported here without a paper target.
base <- rbindlist(lapply(c("eligible", "ineligible"), function(g) {
  t <- comparison_sample(g)[class == g & post == 0]
  data.table(group = g,
             n_parcels = uniqueN(t$car_id),
             pre_rate = round(t[, mean(rate_legacyforest, na.rm = TRUE)], 2),
             pre_defo_area = round(t[, mean(deforested_area_ha, na.rm = TRUE)], 3))
}))
base[, `:=`(paper_n = c(71171, 15254), paper_rate = c(58.4, 11.4))]

cat("\n========== BASELINES ON THE REGRESSION SAMPLES (pooled y<2009) ==========\n")
print(as.data.frame(base))

# ---- vs the paper ------------------------------------------------------------
# The do-file's `value` for variable == 2 is the legacy-forest rate, so the
# comparison runs on rate_legacyforest.
paper <- data.table(
  comparison = c("eligible vs never_eligible", "ineligible vs never_eligible"),
  p_beta_pp = c(-1.412, 4.204),
  p_pre_rate = c(58.4, 11.4)
)

cat("\n================ VS THE PAPER ================\n")
cmp <- merge(res[outcome == "rate_legacyforest"], paper, by = "comparison", all.x = TRUE)
for (i in seq_len(nrow(cmp))) {
  r <- cmp[i]
  cat("\n--", r$comparison, "--\n")
  cat(sprintf("  beta (p.p.)        ours %8.3f   paper %8.3f   %s\n",
              r$beta_pp, r$p_beta_pp,
              if (sign(r$beta_pp) == sign(r$p_beta_pp)) "SIGN MATCHES" else "SIGN DIFFERS"))
  cat(sprintf("  se / p             %8.3f / %s\n", r$se, format(r$p)))
  cat(sprintf("  pre-2009 mean rate ours %8.2f   paper %8.1f\n",
              r$pre_mean_treated, r$p_pre_rate))
}

suffix <- if (PANEL == "recovered") "_recovered" else ""
fwrite(res, file.path(emp_dir, paste0("did_estimates", suffix, ".csv")))
fwrite(base, file.path(emp_dir, paste0("did_baselines", suffix, ".csv")))
# the assembled, filtered panel -- consumed by stage 22 (event studies)
saveRDS(d, file.path(emp_dir, paste0("did_panel", suffix, ".rds")))
cat("\nWrote: ", file.path(emp_dir, paste0("did_estimates", suffix, ".csv")),
    " + ", paste0("did_baselines", suffix, ".csv"),
    " + ", paste0("did_panel", suffix, ".rds"), "\n", sep = "")
