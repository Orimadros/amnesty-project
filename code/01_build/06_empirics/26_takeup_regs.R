# Stage 26: the take-up regressions (tab:25) — SPEC FROM PAPER.
#
# *** No legacy estimation code survives for this exhibit (do-file census final,
# *** 2026-08-11). This stage implements the specification AS PRINTED in the
# *** manuscript (tab:25 notes), under the user's 2026-08-11 authorization to
# *** relax the migrate-only rule FOR ESTIMATION. Anything ambiguous in the
# *** printed spec is resolved conservatively and flagged here.
#
# Printed spec (manuscript notes, tab:25): regress an indicator on
#   - % of deforested area in 2008 (share, given the ~45% turning point in the
#     text: 0.047/(2*0.052)) and its square,
#   - property size bins <=100, (100,500], (500,1000) (>=1000 omitted),
#   - occupation time until 2008 (years since the first year >= 10% deforested,
#     tracked from 1989) and its square;
# cols (1)/(3) no FE, cols (2)/(4) state FE; SEs clustered by state.
# Cols (1)-(2): outcome applies, sample = all eligible (printed N 73,809 -- the
# May vintage; stage 24's default). Cols (3)-(4): outcome receives, sample =
# applicants (printed N 7,507; baseline 42.2%).
#
# Printed coefficients for comparison are hard-coded below.

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})
source(here("code", "01_build", "06_empirics", "_helpers_feols.R"))
if (!feols_selftest()) stop("feols self-test failed")

emp_dir <- here("data", "intermediate", "empirics")
suffix <- Sys.getenv("EMP_TAKEUP_SUFFIX", unset = "")   # "" = May, "_april"
tk <- fread(file.path(emp_dir, paste0("takeup", suffix, ".csv")))
message("takeup", suffix, ": ", nrow(tk), " eligible parcels")

tk[, pct_defo := defo_rate_2008 / 100]
tk[, pct_defo2 := pct_defo^2]
tk[, `:=`(bin_le100 = as.integer(area_do_imovel <= 100),
          bin_100_500 = as.integer(area_do_imovel > 100 & area_do_imovel <= 500),
          bin_500_1000 = as.integer(area_do_imovel > 500 & area_do_imovel < 1000))]
tk[, occ := as.numeric(when_occupied)]
tk[, occ2 := occ^2]
tk[, state := uf]

xvars <- c("pct_defo", "pct_defo2", "bin_le100", "bin_100_500", "bin_500_1000",
           "occ", "occ2")

run_col <- function(df, yvar, fe) {
  s <- df[stats::complete.cases(df[, c(yvar, xvars, "state"), with = FALSE])]
  X <- as.matrix(s[, ..xvars])
  f <- if (fe) fe_ols(s[[yvar]], X, s$state, s$state)
       else    fe_ols(s[[yvar]], X, rep(1L, nrow(s)), s$state)  # constant "FE" = intercept
  cbind(data.table(outcome = yvar, state_fe = fe), f$coefs,
        data.table(n_obs = f$n_obs, n_clusters = f$n_clusters,
                   baseline = mean(s[[yvar]])))
}

applicants <- tk[applies == 1]
res <- rbind(
  run_col(tk, "applies", FALSE),  run_col(tk, "applies", TRUE),
  run_col(applicants, "receives", FALSE), run_col(applicants, "receives", TRUE)
)

paper <- data.table(
  term = rep(xvars, 4),
  col = rep(1:4, each = length(xvars)),
  p_beta = c( 0.047, -0.052, 0.080, 0.062, 0.038, -0.009, 0.0001,
              0.043, -0.057, 0.066, 0.053, 0.032, -0.008, 0.0001,
             -0.171,  0.167, 0.103, -0.054, -0.118, -0.031, 0.001,
             -0.022,  0.070, 0.099, -0.068, -0.117, -0.039, 0.001),
  p_se = c(0.019, 0.018, 0.007, 0.008, 0.010, 0.015, 0.0004,
           0.018, 0.023, 0.014, 0.011, 0.008, 0.150, 0.0001,
           0.193, 0.149, 0.043, 0.058, 0.106, 0.010, 0.0003,
           0.225, 0.163, 0.032, 0.034, 0.090, 0.009, 0.0003)
)
res[, col := rep(1:4, each = length(xvars))]
cmp <- merge(res, paper, by = c("term", "col"))[order(col, match(term, xvars))]

cat("\n========== TAB:25 (SPEC FROM PAPER) vs PRINTED ==========\n")
print(as.data.frame(cmp[, .(col, outcome, state_fe, term,
                            beta = round(beta, 4), se = round(se, 4),
                            p_beta, p_se)]), digits = 4)
cat(sprintf("\nsamples: applies N %d (printed 73,809, baseline 10.2%%; ours %.1f%%)\n",
            res[outcome == "applies" & state_fe == FALSE, n_obs][1],
            100 * res[outcome == "applies" & state_fe == FALSE, baseline][1]))
cat(sprintf("         receives N %d (printed 7,507, baseline 42.2%%; ours %.1f%%)\n",
            res[outcome == "receives" & state_fe == FALSE, n_obs][1],
            100 * res[outcome == "receives" & state_fe == FALSE, baseline][1]))

fwrite(cmp, file.path(emp_dir, paste0("takeup_regs", suffix, ".csv")))
cat("Wrote: ", file.path(emp_dir, paste0("takeup_regs", suffix, ".csv")), "\n", sep = "")
