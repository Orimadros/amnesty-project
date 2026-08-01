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

# State is the CAR id prefix (e.g. "MT-5101704-0244..."), so no extra join needed.
d[, state := substr(car_id, 1, 2)]
d <- d[grepl("^[A-Z]{2}$", state)]

d[, rate_claim := fifelse(area_ha > 0, deforested_area_ha / area_ha * 100, NA_real_)]
setnames(d, "deforestation_rate", "rate_legacyforest")

# D-A (legacy 2_empirics.R:2773/2841): the DiD sample drops parcels with ZERO
# deforested area in 2014, from the control and ineligible groups. Legacy applies it
# to `control` and `spillover` (its name for ineligible) but NOT to `eligible`; that
# asymmetry is reproduced here rather than tidied. Undocumented in the paper.
if (Sys.getenv("EMP_DROP_ZERO2014", unset = "1") != "0") {
  z <- d[year == 2014 & deforested_area_ha == 0 & class != "eligible", unique(car_id)]
  before <- uniqueN(d$car_id)
  d <- d[!car_id %in% z]
  message("D-A: dropped ", length(z), " zero-2014 parcels (control+ineligible): ",
          before, " -> ", uniqueN(d$car_id), " parcels")
}

d[, post := as.integer(year >= POST_FROM)]

# D-B (legacy :2883/2887): the exported DiD outcome is WINSORIZED at 1/99 within
# variable-year before estimation (`value_w`), and the regressions themselves ran in
# Stata on that column. The paper never mentions winsorizing. EMP_WINSOR=0 disables.
WINSOR <- Sys.getenv("EMP_WINSOR", unset = "1") != "0"
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
run_did <- function(treated_class, outcome) {
  s <- d[class %in% c(treated_class, "never_eligible") & !is.na(get(outcome))]
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

res <- rbindlist(lapply(
  c("rate_claim", "rate_legacyforest"),
  function(o) rbindlist(lapply(c("eligible", "ineligible"), run_did, outcome = o))
))

cat("\n================ DiD ESTIMATES ================\n")
print(as.data.frame(res))

# ---- vs the paper ------------------------------------------------------------
paper <- data.table(
  comparison = c("eligible vs never_eligible", "ineligible vs never_eligible"),
  p_beta_pp = c(-1.4, 4.2),
  p_pre_rate = c(58.4, 11.4)
)

cat("\n================ VS THE PAPER ================\n")
cmp <- merge(res[outcome == "rate_claim"], paper, by = "comparison", all.x = TRUE)
for (i in seq_len(nrow(cmp))) {
  r <- cmp[i]
  cat("\n--", r$comparison, "--\n")
  cat(sprintf("  beta (p.p.)        ours %8.3f   paper %8.1f   %s\n",
              r$beta_pp, r$p_beta_pp,
              if (sign(r$beta_pp) == sign(r$p_beta_pp)) "SIGN MATCHES" else "SIGN DIFFERS"))
  cat(sprintf("  se / p             %8.3f / %s\n", r$se, format(r$p)))
  cat(sprintf("  pre-2009 mean rate ours %8.2f   paper %8.1f\n",
              r$pre_mean_treated, r$p_pre_rate))
}

fwrite(res, file.path(emp_dir, "did_estimates.csv"))
cat("\nWrote: ", file.path(emp_dir, "did_estimates.csv"), "\n", sep = "")
