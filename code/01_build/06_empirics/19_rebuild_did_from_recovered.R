# Stage 19 (recovered data): rebuild legacy's `did.dta` from PEDRO'S OWN per-year
# panels (data/legacy_dropbox/output_full/CAR_{eligible,ineligible,control}_defo_
# 2005..2014.rds) exactly as 2_empirics.R assembles `combined`, then apply the
# sample filters discovered in empirics_amazon_final.do (recovered 2026-08-06):
#
#   line 25: drop eligible parcels with MIN pre-2009 rate < 10
#   line 26: sa = never-eligible parcels with 2009 deforested AREA < 5 (excluded
#            from the ineligible regressions; the variable is misnamed in the code)
#   line 65: ineligible regressions keep MAX pre-2009 rate < 85
#
# and compute the do-file's own baselines (`sum value ... & y < 2009`, i.e. POOLED
# parcel-year means) plus our twfe DiD on their panel. Targets: baselines 58.4 and
# 11.4; Table 2 observations 782,175 and 231,833; betas -1.412 and +4.204.

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})
source(here("code", "01_build", "06_empirics", "_helpers_twfe.R"))

dd <- here("data", "legacy_dropbox", "output_full")
years <- 2005:2014

read_year <- function(prefix, y) {
  x <- as.data.table(readRDS(file.path(dd, sprintf("%s_defo_%d.rds", prefix, y))))
  setnames(x, c("COD_IMO", "defo", "rate"))
  x[, `:=`(defo = as.numeric(defo), rate = as.numeric(rate))]
  x[is.nan(rate), rate := NA]
  x
}

# CORRECTED 2026-08-07 after the line-level audit of 2_empirics.R:1549-2304.
# The did.dta LONG panels (:1569-1585 etc.) apply, per year independently:
#   group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
# i.e. n==1 counted BEFORE any dedup (exact-duplicate rows kill the parcel for
# that year), NO anchoring of the parcel set to 2005, and NO area<1e5 filter on
# any group (2_empirics.R:1704 belongs to the WIDE descriptive table only).
# The previous version of this stage uniqued exact dupes first, anchored to the
# 2005 file, and area-filtered the ineligible -- that changed the ineligible
# side materially (6,988 -> 13,134 parcels); eligible side unaffected.
build_long <- function(prefix) {
  rbindlist(lapply(years, function(y) {
    x <- read_year(prefix, y)
    x[, n := .N, by = COD_IMO]
    x <- x[n == 1][, n := NULL]
    x[, year := y]
    x
  }))
}

message("assembling eligible / inelegible / never eligible panels...")
e <- build_long("CAR_eligible");   e[, group := "eligible"]
i <- build_long("CAR_ineligible"); i[, group := "inelegible"]
c <- build_long("CAR_control");    c[, group := "never eligible"]
combined <- rbind(e, i, c)
message("combined: ", nrow(combined), " parcel-years, ",
        uniqueN(combined$COD_IMO), " parcels")
print(combined[year == 2005, .N, by = group])

# ---- do-file derived variables --------------------------------------------------
pre <- combined[year < 2009,
  .(rate_min_pre = min(rate, na.rm = TRUE), rate_max_pre = max(rate, na.rm = TRUE)),
  by = COD_IMO]
pre[!is.finite(rate_min_pre), rate_min_pre := NA]
pre[!is.finite(rate_max_pre), rate_max_pre := NA]
d09 <- combined[year == 2009, .(COD_IMO, defo_2009 = defo)]
combined <- merge(combined, pre, by = "COD_IMO", all.x = TRUE)
combined <- merge(combined, unique(d09, by = "COD_IMO"), by = "COD_IMO", all.x = TRUE)

# line 25: drop eligible with min pre-2009 rate < 10
combined <- combined[!(group == "eligible" & (is.na(rate_min_pre) | rate_min_pre < 10))]
# line 26: sa flag on the control
combined[, sa := group == "never eligible" & !is.na(defo_2009) & defo_2009 < 5]

# ---- the do-file's own baselines ------------------------------------------------
elig_reg <- combined[group %in% c("eligible", "never eligible")]
inel_reg <- combined[group %in% c("inelegible", "never eligible") & sa == FALSE &
                     !is.na(rate_max_pre) & rate_max_pre < 85]

b1 <- elig_reg[group == "eligible" & year < 2009, mean(rate, na.rm = TRUE)]
b2 <- inel_reg[group == "inelegible" & year < 2009, mean(rate, na.rm = TRUE)]

cat("\n===== FROM PEDRO'S OWN PANELS + THE DO-FILE'S FILTERS =====\n")
cat(sprintf("eligible baseline (sum value, y<2009):    %.2f   (paper 58.4)\n", b1))
cat(sprintf("ineligible baseline (sum value, y<2009):  %.2f   (paper 11.4)\n", b2))
cat(sprintf("eligible reg parcels: %d (+ control %d) | obs %d  (paper implies 78,217 parcels / 782,175 obs)\n",
            uniqueN(elig_reg[group == "eligible", COD_IMO]),
            uniqueN(elig_reg[group == "never eligible", COD_IMO]),
            nrow(elig_reg[!is.na(rate)])))
cat(sprintf("ineligible reg parcels: %d (+ control %d) | obs %d  (paper implies ~16,134 / 231,833 obs)\n",
            uniqueN(inel_reg[group == "inelegible", COD_IMO]),
            uniqueN(inel_reg[group == "never eligible", COD_IMO]),
            nrow(inel_reg[!is.na(rate)])))

# ---- the DiD on their panel -----------------------------------------------------
run <- function(s, treated_group) {
  s <- s[!is.na(rate)]
  s[, `:=`(treat = as.integer(group == treated_group), post = as.integer(year >= 2009))]
  s[, uf := substr(COD_IMO, 1, 2)]
  s <- s[grepl("^[A-Z]{2}$", uf)]
  fit <- twfe(s$rate, s$treat * s$post, s$COD_IMO, s$year, s$uf)
  sprintf("beta %.3f (se %.3f, p %.3g, n_obs %d)", fit$beta, fit$se, fit$p, fit$n_obs)
}
cat("\nDiD on their panel (twfe, cluster uf):\n")
cat("  eligible vs never-eligible:   ", run(elig_reg, "eligible"),
    "  (paper -1.412 se 0.558)\n")
cat("  ineligible vs never-eligible: ", run(inel_reg, "inelegible"),
    "  (paper +4.204 se 0.886)\n")

saveRDS(combined, file.path(dd, "rebuilt_did_panel.rds"))
cat("\nWrote: rebuilt_did_panel.rds\n")
