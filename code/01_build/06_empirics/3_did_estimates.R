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
# Outcome. Table 1's note defines the deforestation rate as "the share of a
# property claim's area that has been deforested", with claim area as declared by
# the occupant. So the primary outcome is deforested_ha / area_ha * 100. We also
# report the legacy-forest denominator as a robustness check, since Appendix C uses
# that denominator for the occupation test and the two are not interchangeable.
#
# NOTE: the paper states (§2.3) that its results "carry if we used property
# boundaries without any adjustment", i.e. without the spatial conflict-resolution
# stage. This script runs on unresolved boundaries, so LEVELS are known to run high
# (~+39%/+44% on counts, see docs/notes/table1_comparison_findings.md). The point of
# this stage is the SIGN and rough MAGNITUDE of beta, not the levels.

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
elig <- fread(elig_f, select = c("car_id", "class", "in_sample"))
elig <- elig[in_sample == TRUE, .(car_id, class)]

# If the conflict-resolution stage has run, restrict to the parcels it kept. Set
# EMP_RESOLVED=0 to estimate on unresolved boundaries instead (the paper states in
# 2.3 that its results carry either way, so both are worth reporting).
resolved_f <- file.path(emp_dir, "parcels_resolved_2014.csv")
use_resolved <- Sys.getenv("EMP_RESOLVED", unset = "1") != "0" && file.exists(resolved_f)
if (use_resolved) {
  keep_ids <- fread(resolved_f, select = "car_id")$car_id
  before <- nrow(elig)
  elig <- elig[car_id %in% keep_ids]
  message("using CONFLICT-RESOLVED parcels: ", before, " -> ", nrow(elig))
} else {
  message("using UNRESOLVED boundaries: ", nrow(elig), " parcels")
}

d <- rbindlist(lapply(files, fread))
d <- merge(d, elig, by = "car_id")

# State is the CAR id prefix (e.g. "MT-5101704-0244..."), so no extra join needed.
d[, state := substr(car_id, 1, 2)]
d <- d[grepl("^[A-Z]{2}$", state)]

d[, rate_claim := fifelse(area_ha > 0, deforested_area_ha / area_ha * 100, NA_real_)]
setnames(d, "deforestation_rate", "rate_legacyforest")

d[, post := as.integer(year >= POST_FROM)]

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
