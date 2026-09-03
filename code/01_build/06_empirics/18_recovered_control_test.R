# Stage 18 (diagnostic, recovered data): rebuild the paper's never-eligible column
# from PEDRO'S OWN FILES -- data/legacy_dropbox/output/CAR_control_defo_2005..2014.rds,
# recovered 2026-08-06 from the amazon_project Dropbox (docs/notes/
# dropbox_survey_20260806.md). These are the exact per-year measurements legacy's
# panel block (2_empirics.R:2092-2126) reads, so every candidate formula for Table
# 1's 7,049 / 760 ha / 35.7% / 2.0 -> 2.2 Mha can be evaluated against the true
# source. No more inference: whatever formula reproduces the numbers IS the
# definition.
#
# Replicates the legacy panel assembly literally: base year 2005, per-year dedup on
# (COD_IMO, rate, defo[, area]), add_count/filter(n == 1), left_joins, NaN -> NA.
# The commented-out `area < 50000` filter (:2130) is evaluated both ways.

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

dd <- here("data", "legacy_dropbox", "output")
years <- 2005:2014
f <- file.path(dd, sprintf("CAR_control_defo_%d.rds", years))
stopifnot(all(file.exists(f)))

panel <- lapply(seq_along(years), function(i) {
  x <- as.data.table(readRDS(f[i]))
  setnames(x, c("COD_IMO", "defo", "rate"))
  x[, `:=`(defo = as.numeric(defo), rate = as.numeric(rate))]
  x[, area := defo / (rate / 100)]
  x[is.nan(rate), rate := NA]
  x[is.nan(area), area := NA]
  # legacy's per-year filters
  x <- unique(x, by = c("COD_IMO", "rate", "defo"))
  x[, n := .N, by = COD_IMO]
  x <- x[n == 1][, n := NULL]
  x[, year := years[i]]
  x
})

message("rows per year: ", paste(sapply(panel, nrow), collapse = " "))

# wide, base 2005 (left_join semantics)
w <- panel[[1]][, .(COD_IMO, defo_2005 = defo, rate_2005 = rate, area = area)]
for (i in 2:length(years)) {
  y <- years[i]
  w <- merge(w, panel[[i]][, .(COD_IMO, defo = defo, rate = rate)],
             by = "COD_IMO", all.x = TRUE)
  setnames(w, c("defo", "rate"), paste0(c("defo_", "rate_"), y))
}
message("panel parcels (base 2005 after n==1): ", nrow(w))

pre <- paste0("rate_", 2005:2008)
w[, rate_pre_mean := rowMeans(.SD, na.rm = TRUE), .SDcols = pre]

candidates <- function(d, label) {
  yearly <- sapply(2005:2008, function(y) mean(d[[paste0("rate_", y)]], na.rm = TRUE))
  pooled <- mean(unlist(d[, ..pre]), na.rm = TRUE)
  data.table(
    set = label,
    n = nrow(d),
    rate_mean_of_yearly = round(mean(yearly), 2),
    rate_pooled_0508 = round(pooled, 2),
    rate_2005_only = round(yearly[1], 2),
    area_mean_lf2005 = round(mean(d$area, na.rm = TRUE), 1),
    defo_Mha_2008 = round(sum(d$defo_2008, na.rm = TRUE) / 1e6, 3),
    defo_Mha_2014 = round(sum(d$defo_2014, na.rm = TRUE) / 1e6, 3),
    pct_change = round((sum(d$defo_2014, na.rm = TRUE) / sum(d$defo_2008, na.rm = TRUE) - 1) * 100, 1)
  )
}

res <- rbind(
  candidates(w, "full panel"),
  candidates(w[!is.na(area) & area < 50000], "with commented area<50000 filter"),
  candidates(w[!is.na(area)], "!is.na(area) only")
)

cat("\n===== THE CONTROL COLUMN FROM PEDRO'S OWN FILES =====\n")
print(as.data.frame(res))
cat("\npaper: N 7,049 | area 760 | rate 35.7 | defo 2.0 -> 2.2 Mha | change 11.5%\n")

# our reproduction, for reference
cat("\nours (F1+F2+F3 basis): N 6,855 | rate 36.3 | 2.003 -> 2.189\n")

# id overlap with OUR never-eligible sample
elig <- fread(here("data", "intermediate", "empirics", "parcel_eligibility.csv"))
ours <- elig[class == "never_eligible" & table1_sample == TRUE, car_id]
theirs <- w$COD_IMO
cat("\nid overlap: theirs ", length(theirs), " | ours ", length(ours),
    " | intersection ", length(intersect(theirs, ours)),
    " | theirs-only ", length(setdiff(theirs, ours)),
    " | ours-only ", length(setdiff(ours, theirs)), "\n", sep = "")

fwrite(res, here("data", "intermediate", "empirics", "recovered_control_test.csv"))
fwrite(data.table(COD_IMO = theirs), file.path(dd, "control_panel_ids.csv"))
cat("\nWrote: recovered_control_test.csv, control_panel_ids.csv\n")
