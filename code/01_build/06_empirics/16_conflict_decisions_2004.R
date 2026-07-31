# Stage 16: the production conflict-resolution decision set, under legacy's
# as-executed semantics and its 2004-based rules.
#
# Why this exists (checkpoint-20260801 plan item 1, difference L4): stage 2 was
# applying an ERASURE computed by stage 4/4b under the 2014-rule decisions while
# taking its DROPS from stage 12's 2004-rule run. A parcel could be shrunk by one
# run's decision and kept by another's. This emits BOTH kinds of decision from a
# single 2004-rule pass, in the schema 4b already consumes
# (car_id, action, other_id), so rates, areas and drops finally share one basis.
#
# Semantics follow the standing rule -- replicate the code, not the paper's
# appendix (docs/notes/paper_legacy_method_diffs.md, and memory code-over-paper-rule):
#   * rules evaluated on 2004 deforestation (N1);
#   * direction rows gated at >10% of i's DECLARED area, i-side only (N4);
#   * rows with a zero-2004-deforestation side are unevaluable and their parcels
#     fall out of the survivor sets (N2);
#   * the final assembly keeps conflicted parcels only via the i-side of surviving
#     rows, so the winner of a fully drop-resolved pair vanishes (N6);
#   * containment-0+0 pairs get a random erase, which our stage 4 no-ops (N3);
#   * require_j_alive is FALSE: legacy filters on `COD_IMO %in% pdfs2` and
#     `id %in% pdfs3`, never on the j side (measured inert, but this is the
#     faithful reading).
#
# Pair measurements are reused from stage 14's cache. Output:
#   conflict_decisions_2004rules.csv   -> feed to 4b via EMP_DECISIONS
#   parcels_resolved_2004rules.csv     -> the kept set (overwrites stage 12's,
#                                         which used require_j_alive = TRUE)

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

SEED <- 20260801
set.seed(SEED)

emp_dir <- here("data", "intermediate", "empirics")
cache_f <- file.path(emp_dir, "conflict_pair_defo_2004.csv")
if (!file.exists(cache_f)) stop("Missing ", cache_f, " -- run 14_conflict_variants.R first.")

elig <- fread(file.path(emp_dir, "parcel_eligibility.csv"))
pool <- elig[class != "never_eligible" & basis_sample == TRUE, .(car_id, class, area_ha)]
d04 <- fread(file.path(emp_dir, "parcel_defo_2004.csv"))[, .(car_id, defo04 = deforested_area_ha)]
pool <- merge(pool, d04, by = "car_id", all.x = TRUE)
pool[is.na(defo04), defo04 := 0]
message("pool: ", nrow(pool))

pr <- fread(here("data", "intermediate", "car", "CAR_overlap_variables_conflicts.csv"),
            select = c("int_area", "carid_reference", "carid_target"))
pr[, `:=`(a = as.character(carid_reference), b = as.character(carid_target))]
pr <- pr[a %in% pool$car_id & b %in% pool$car_id & a != b]
pr[, int_ha := int_area / 1e4]
pr <- pr[, .(int_ha = max(int_ha)), by = .(a = pmin(a, b), b = pmax(a, b))]
pr[, pair := paste(a, b)]

rows <- rbind(pr[, .(i = a, j = b, int_ha, pair)], pr[, .(i = b, j = a, int_ha, pair)])
rows <- merge(rows, pool[, .(i = car_id, area_i = area_ha, defo_i = defo04)], by = "i")
rows <- merge(rows, pool[, .(j = car_id, area_j = area_ha, defo_j = defo04)], by = "j")
rows[, `:=`(pct_i = int_ha / area_i, pct_j = int_ha / area_j)]
rows <- rows[pct_i > 0.1]
rows[, muni := sub("^[A-Z]{2}-([0-9]+)-.*$", "\\1", i)]
rows <- rows[grepl("^[0-9]{6,7}$", muni)]
setorder(rows, muni, i, j)

cache <- fread(cache_f)
rows <- merge(rows, cache, by = "pair", all.x = TRUE)
rows[, evaluable := defo_i > 0 & defo_j > 0 & !is.na(dint)]
rows[, di := fifelse(evaluable, as.integer(dint / defo_i * 100 >= 80), NA_integer_)]
rows[, dj := fifelse(evaluable, as.integer(dint / defo_j * 100 >= 80), NA_integer_)]
rows[, case := fifelse(pct_i >= 0.9 & pct_j >= 0.9, "overlap",
                fifelse(pct_i >= 0.9, "i_in_j",
                fifelse(pct_j >= 0.9, "j_in_i", "overlap")))]
conflicted <- unique(rows$i)
message("direction rows ", nrow(rows), " | evaluable ", sum(rows$evaluable),
        " | conflicted i-parcels ", length(conflicted))

dec <- vector("list", 0)
kept_all <- character(0)

for (m in unique(rows$muni)) {
  rw <- rows[muni == m]
  alive <- unique(c(rw$i, rw$j))
  row_alive <- rep(TRUE, nrow(rw))

  for (r in seq_len(nrow(rw))) {
    if (!rw$evaluable[r]) next
    A <- rw$i[r]; B <- rw$j[r]; di <- rw$di[r]; dj <- rw$dj[r]; cs <- rw$case[r]
    dropA <- dropB <- FALSE
    if (cs == "overlap") {
      if (di == 1 && dj == 0) dropA <- TRUE
      if (di == 0 && dj == 1) dropB <- TRUE
    } else if (cs == "j_in_i") {
      if (di == 1) dropA <- TRUE else if (dj == 1) dropB <- TRUE
    } else {
      if (di == 1 && dj == 0) dropA <- TRUE
      if (dj == 1) dropB <- TRUE
    }
    if (dropA || dropB) {
      who <- if (dropA) A else B
      other <- if (dropA) B else A
      alive <- setdiff(alive, who)
      row_alive[r] <- FALSE
      dec[[length(dec) + 1L]] <- data.table(car_id = who, action = "drop", other_id = other)
    }
  }

  surv <- rw[row_alive & rw$evaluable & rw$i %in% alive] # require_j_alive = FALSE
  deleted <- character(0); done <- character(0); members <- character(0)
  if (nrow(surv) > 0) {
    for (r in seq_len(nrow(surv))) {
      A <- surv$i[r]; B <- surv$j[r]
      if (A %in% deleted || B %in% deleted) next
      cs <- surv$case[r]; di <- surv$di[r]; dj <- surv$dj[r]
      if (surv$pair[r] %in% done) { members <- c(members, A); next }
      done <- c(done, surv$pair[r])
      if (cs == "overlap" && di == 1 && dj == 1) {
        # both sides' deforestation is inside the intersection -> keep one at random
        loser <- if (runif(1) < 0.5) A else B
        deleted <- c(deleted, loser)
        dec[[length(dec) + 1L]] <- data.table(
          car_id = loser, action = "drop_random",
          other_id = if (loser == A) B else A)
        if (loser != A) members <- c(members, A)
      } else {
        # overlap-0+0 and containment-0+0 (N3): keep both, erase the shared area
        # from one side at random
        victim <- if (runif(1) < 0.5) A else B
        dec[[length(dec) + 1L]] <- data.table(
          car_id = victim, action = "erase_intersection_random",
          other_id = if (victim == A) B else A)
        members <- c(members, A)
      }
    }
  }
  kept_all <- c(kept_all, setdiff(unique(members), deleted))
}

decisions <- unique(rbindlist(dec, fill = TRUE))
# a parcel dropped anywhere cannot also be an erase victim
dropped_ids <- unique(decisions[action %in% c("drop", "drop_random")]$car_id)
decisions <- decisions[!(action == "erase_intersection_random" & car_id %in% dropped_ids)]

kept <- unique(c(setdiff(pool$car_id, conflicted), kept_all))
kept <- setdiff(kept, dropped_ids)
res <- pool[car_id %in% kept]

fwrite(decisions, file.path(emp_dir, "conflict_decisions_2004rules.csv"))
fwrite(res[, .(car_id, class)], file.path(emp_dir, "parcels_resolved_2004rules.csv"))

cat("\n=== 2004-RULE DECISIONS (faithful semantics, seed ", SEED, ") ===\n", sep = "")
print(decisions[, .N, by = action][order(-N)])
cat("\nkept: ", nrow(res), " of ", nrow(pool), "\n", sep = "")
print(res[, .N, by = class][order(class)])
cat("\npaper Table 1: eligible 71,171 | ineligible 15,254 (Table 2 implies ~16,134)\n")
cat("Wrote: conflict_decisions_2004rules.csv, parcels_resolved_2004rules.csv\n")
