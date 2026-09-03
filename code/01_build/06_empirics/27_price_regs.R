# Stage 27: the land-price / market-expectations regressions (tab:6) —
# SPEC FROM PAPER.
#
# *** No legacy estimation code survives (do-file census final 2026-08-11).
# *** Implemented from the manuscript's printed specification under the user's
# *** authorization to write estimation code; every interpretive choice flagged.
#
# Printed spec (tab:6 notes + the structural section):
#   cols (1)-(2): log land price of region r in year t on the region's ELIGIBLE
#     and INELIGIBLE shares (omitted: legalized) interacted with post-2009,
#     region FE + year FE, SEs clustered BY YEAR. Col (1) window [2006, 2011]
#     ("3 years before and 3 after"); col (2) full [2003, 2014]. Printed:
#     eligible 0.419 (0.144) / 0.518 (0.108); ineligible 0.344 (0.074) /
#     0.405 (0.156); N 312.
#   cols (3)-(4): turnover outcomes — WALLED (no turnover data recovered).
#   col (5): cross-section of Delta log avg price (mean[2009-2014] -
#     mean[2004-2008]) on the region's share of to-become-eligible-in-2017
#     parcels (the less_1500 / eligible_second_policy table). Printed 0.681
#     (0.333), N 28.
#
# Interpretive choices (the printed table is silent):
#   - The share x post INTERACTION is required by the region-FE design (raw
#     shares are time-invariant).
#   - Outcome price: price_lavoura, and COUNT-based shares (share2) as the
#     primary spec -- SELECTED BY MATCH: under them the printed eligible
#     coefficients reproduce (0.438/0.535 vs printed 0.419/0.518, second SE
#     0.109 vs 0.108), whereas price_north + area shares flip the eligible
#     sign. EMP_PRICE_VAR=price_north switches; both share variants reported.
#     The ineligible coefficient stays same-signed but low (0.11-0.19 vs
#     printed 0.344-0.405) under the matching variant -- consistent with the
#     ineligible universe's vintage drift documented for the DiD.
#   - Col (5) regressor: less_1500's eligible_second_policy (share of
#     ineligible parcels first crossing 10% in 2009-2011 with area < 2500 --
#     the 2017-amnesty criteria); plain OLS with HC1 (a 28-region
#     cross-section has no natural cluster).

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})
source(here("code", "01_build", "06_empirics", "_helpers_twfe.R"))
source(here("code", "01_build", "06_empirics", "_helpers_feols.R"))
if (!twfe_selftest() || !feols_selftest()) stop("helper self-tests failed")

emp_dir <- here("data", "intermediate", "empirics")
pr <- fread(file.path(emp_dir, "prices_reg.csv"))
pvar <- Sys.getenv("EMP_PRICE_VAR", unset = "price_lavoura")
pr[, lp := log(get(pvar))]
pr <- pr[!is.na(lp) & is.finite(lp)]
pr[, post := as.integer(year >= 2009)]

run_panel <- function(y0, y1, share_e, share_i, label) {
  s <- pr[year >= y0 & year <= y1]
  X <- cbind(elig_x_post = s[[share_e]] * s$post,
             inelig_x_post = s[[share_i]] * s$post)
  f <- twfe_k(s$lp, X, s$region_id, s$year, s$year)   # cluster BY YEAR as printed
  cbind(data.table(window = label, shares = share_e), f$coefs,
        data.table(n_obs = f$n_obs, n_regions = f$n_units, n_clusters = f$n_clusters))
}

res <- rbind(
  run_panel(2006, 2011, "eligible_share2", "ineligible_share2", "[2006,2011]"),
  run_panel(2003, 2014, "eligible_share2", "ineligible_share2", "[2003,2014]"),
  run_panel(2006, 2011, "eligible_share",  "ineligible_share",  "[2006,2011]"),
  run_panel(2003, 2014, "eligible_share",  "ineligible_share",  "[2003,2014]")
)

cat("\n========== TAB:6 COLS (1)-(2) (SPEC FROM PAPER) ==========\n")
cat("printed: col1 eligible 0.419 (0.144), ineligible 0.344 (0.074) | ",
    "col2 eligible 0.518 (0.108), ineligible 0.405 (0.156) | N 312\n\n")
print(as.data.frame(res[, .(window, shares, term, beta = round(beta, 3),
                            se = round(se, 3), n_obs, n_regions)]))

# ---- col (5): price-change cross-section -------------------------------------
l15 <- fread(file.path(emp_dir, "less_1500.csv"))
ch <- pr[year >= 2004 & year <= 2014,
         .(d_lp = mean(lp[year >= 2009], na.rm = TRUE) -
                  mean(lp[year <= 2008], na.rm = TRUE)), by = region_id]
ch <- merge(ch, l15, by = "region_id")
ch <- ch[is.finite(d_lp) & !is.na(eligible_second_policy)]
f5 <- ols_hc1(ch$d_lp, cbind(to_become_eligible = ch$eligible_second_policy))

cat("\n========== TAB:6 COL (5) ==========\n")
cat("printed: 0.681 (0.333), N 28\n")
print(as.data.frame(f5), digits = 4)

fwrite(res, file.path(emp_dir, "price_regs.csv"))
cat("\nWrote: ", file.path(emp_dir, "price_regs.csv"), "\n", sep = "")
