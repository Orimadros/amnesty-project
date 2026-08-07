# Stage 22: Figure 3 event studies -- the eventdd-equivalent on our panel.
#
# The recovered empirics_amazon_final.do draws Figure 3 with:
#   eventdd value if variable == 2 & (group == "eligible" | group == "never
#     eligible"), hdfe absorb(COD_IMO i.y) timevar(timeToTreat) cluster(uf)   (:129)
#   eventdd value ... (inelegible | never eligible) & sa == . &
#     defo_rate_value_max < 85, hdfe absorb(COD_IMO i.y) ...                  (:133)
# with timeToTreat = year - 2009 for treated parcels and MISSING for the control
# ("never eligible"), which eventdd folds into the baseline. Equivalent linear
# model: outcome on one dummy per treated relative year (ref: -1, i.e. 2008),
# parcel + year FE, cluster by state. Estimated here with twfe_k()
# (_helpers_twfe.R); coefficient paths written to CSV for plotting.
#
# Input: did_panel.rds written by stage 3 (its default panel carries the do-file
# filters; the :25 eligible drop is already applied there, and this stage applies
# the ineligible-regression filters exactly as stage 3 does).
# EMP_PANEL=recovered reads did_panel_recovered.rds (stage 3 on Pedro's rebuilt
# April-2025 panels) for the vintage comparison.

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})
source(here("code", "01_build", "06_empirics", "_helpers_twfe.R"))
if (!twfe_selftest()) stop("twfe self-test failed -- do not trust these estimates")

POST_FROM <- 2009

emp_dir <- here("data", "intermediate", "empirics")
PANEL <- Sys.getenv("EMP_PANEL", unset = "ours")
suffix <- if (PANEL == "recovered") "_recovered" else ""
panel_f <- file.path(emp_dir, paste0("did_panel", suffix, ".rds"))
if (!file.exists(panel_f)) stop("Missing ", panel_f, " -- run 3_did_estimates.R first",
                                if (PANEL == "recovered") " with EMP_PANEL=recovered")
d <- as.data.table(readRDS(panel_f))

run_event <- function(treated_class) {
  s <- d[class %in% c(treated_class, "never_eligible")]
  if (treated_class == "ineligible") {
    s <- s[sa == FALSE & !is.na(rate_max_pre) & rate_max_pre < 85]
  }
  s <- s[!is.na(rate_legacyforest)]
  s[, ttt := fifelse(class == treated_class, as.integer(year - POST_FROM), NA_integer_)]

  ks <- setdiff(sort(unique(s[!is.na(ttt), ttt])), -1L)   # ref = -1 (2008)
  X <- sapply(ks, function(k) as.integer(!is.na(s$ttt) & s$ttt == k))
  colnames(X) <- paste0("t", ifelse(ks < 0, paste0("m", -ks), paste0("p", ks)))

  fit <- twfe_k(s$rate_legacyforest, X, s$car_id, s$year, s$state)
  out <- cbind(data.table(comparison = paste0(treated_class, " vs never_eligible"),
                          rel_year = ks, year = ks + POST_FROM),
               fit$coefs,
               data.table(n_obs = fit$n_obs, n_parcels = fit$n_units,
                          n_clusters = fit$n_clusters))
  message(treated_class, ": ", fit$n_obs, " obs, ", fit$n_units, " parcels, ",
          fit$n_clusters, " clusters")
  out
}

res <- rbind(run_event("eligible"), run_event("ineligible"))

cat("\n================ EVENT-STUDY COEFFICIENTS (ref: 2008) ================\n")
print(as.data.frame(res[, .(comparison, year, beta = round(beta, 3),
                            se = round(se, 3), p = signif(p, 3))]))

fwrite(res, file.path(emp_dir, paste0("event_study_coefs", suffix, ".csv")))
cat("\nWrote: ", file.path(emp_dir, paste0("event_study_coefs", suffix, ".csv")), "\n",
    sep = "")

# ---- Figure 4: expectation effects by property size band ---------------------
# do-file :175/:177: ineligible vs never-eligible, keep value_max < 95 (max rate
# over ALL years, not the pre-window) and the DECLARED-area band -- both
# conditions sit in the `if` clause, so they filter the control too. The
# do-file's sa / defo_rate_value_max filters are NOT part of this figure.
# Skipped on the recovered panel (their per-year files carry no declared area).
if (PANEL != "recovered") {
  d[, rate_max_all := max(rate_legacyforest, na.rm = TRUE), by = car_id]
  d[!is.finite(rate_max_all), rate_max_all := NA]

  run_fig4 <- function(band_label, band_expr) {
    s <- d[class %in% c("ineligible", "never_eligible") &
             !is.na(rate_max_all) & rate_max_all < 95 &
             eval(band_expr) & !is.na(rate_legacyforest)]
    s[, ttt := fifelse(class == "ineligible", as.integer(year - POST_FROM), NA_integer_)]
    ks <- setdiff(sort(unique(s[!is.na(ttt), ttt])), -1L)
    X <- sapply(ks, function(k) as.integer(!is.na(s$ttt) & s$ttt == k))
    colnames(X) <- paste0("t", ifelse(ks < 0, paste0("m", -ks), paste0("p", ks)))
    fit <- twfe_k(s$rate_legacyforest, X, s$car_id, s$year, s$state)
    cbind(data.table(band = band_label, rel_year = ks, year = ks + POST_FROM),
          fit$coefs,
          data.table(n_obs = fit$n_obs, n_parcels = fit$n_units,
                     n_clusters = fit$n_clusters))
  }

  f4 <- rbind(
    run_fig4("area <= 1500", quote(area_ha <= 1500)),
    run_fig4("1500 < area <= 2500", quote(area_ha > 1500 & area_ha <= 2500))
  )
  cat("\n========== FIGURE 4: ineligible by size band (ref: 2008) ==========\n")
  print(as.data.frame(f4[, .(band, year, beta = round(beta, 3), se = round(se, 3),
                             p = signif(p, 3))]))
  fwrite(f4, file.path(emp_dir, "event_study_fig4.csv"))
  cat("\nWrote: ", file.path(emp_dir, "event_study_fig4.csv"), "\n", sep = "")
}
