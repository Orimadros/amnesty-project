# Stage 4 of the empirics chain: the spatial conflict-resolution algorithm.
#
# CAR boundaries are self-declared and unverified, so claims overlap. Before any
# deforestation total is meaningful, each cleared patch must be assigned to exactly
# one claim.
#
# THIS IS THE LEGACY-FAITHFUL IMPLEMENTATION (rewritten 2026-08-01, difference L6).
# It replaces the version that followed Appendix C's prose (see git 1e68c9b), and it
# absorbs the pair caching from stage 14 and the decision emission from stage 16,
# both now retired. Per the standing code-over-paper rule, where legacy's code and
# the paper's appendix disagree the CODE wins -- and every such choice is a switch
# below, so the paper's variant stays runnable.
#
# What legacy actually does (2_empirics.R:734-1160), against Appendix C's list:
#   N1  rules are evaluated on 2004 deforestation, not the sample year
#   N4  a direction row exists per side, gated at >10% of THAT side's declared area;
#       the assembly then filters on the i-side only
#   N2  a row with a zero-deforestation side is unevaluable, and its parcels fall out
#       of every survivor set -- a silent drop with no rule firing
#   N6  the assembly keeps conflicted parcels only through the i-side of surviving
#       rows, so the WINNER of a fully drop-resolved pair vanishes unless another
#       row saves it
#   N3  containment pairs where neither side trips 80% get a random erase; the
#       paper's rule list has no such case
#
# Outputs (4b reads the decisions via EMP_DECISIONS; stage 2 reads the resolved set):
#   conflict_decisions_<basis>.csv   car_id, action, other_id
#   parcels_resolved_<basis>.csv     the kept set
#
# Env:
#   CR_RULE_YEAR         raster year the rules are evaluated on (default 2004)
#   CR_SEED              seed for the two random rules (default 20260801)
#   CR_WINNER_VANISHES   default 1 (legacy); 0 keeps drop-pair winners
#   CR_DROP_UNEVALUABLE  default 1 (legacy); 0 keeps parcels with no evaluable row
#   CR_REQUIRE_J_ALIVE   default 0 (legacy: i-side only); 1 was our earlier reading
#   CR_BASIS             output suffix (default "<CR_RULE_YEAR>rules")

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(data.table)
  library(here)
})

sf_use_s2(FALSE) # planar overlay; matches the legacy 2_empirics workflow (CAR #26)

RULE_YEAR <- as.integer(Sys.getenv("CR_RULE_YEAR", unset = "2004"))
SEED <- as.integer(Sys.getenv("CR_SEED", unset = "20260801"))
WINNER_VANISHES <- Sys.getenv("CR_WINNER_VANISHES", unset = "1") != "0"
DROP_UNEVALUABLE <- Sys.getenv("CR_DROP_UNEVALUABLE", unset = "1") != "0"
REQUIRE_J_ALIVE <- Sys.getenv("CR_REQUIRE_J_ALIVE", unset = "0") != "0"
BASIS <- Sys.getenv("CR_BASIS", unset = paste0(RULE_YEAR, "rules"))

OVERLAP_GATE <- 0.10 # step 2: overlap share of i's own declared area
CONTAIN_CUT <- 0.90  # step 3: containment threshold
DEFO_CUT <- 80       # step 3: percent-of-deforestation threshold

set.seed(SEED)
emp_dir <- here("data", "intermediate", "empirics")
tile_dir <- here("data", "intermediate", "mapbiomas", "transitions_combined")
cache_f <- file.path(emp_dir, paste0("conflict_pair_defo_", RULE_YEAR, ".csv"))

message("basis ", BASIS, " | rule year ", RULE_YEAR, " | seed ", SEED,
        " | winner_vanishes ", WINNER_VANISHES,
        " | drop_unevaluable ", DROP_UNEVALUABLE,
        " | require_j_alive ", REQUIRE_J_ALIVE)

# ---- pool: target-area parcels in the active sample ----------------------------
# O1 (2026-08-01): legacy's cleaning universe is the ACTIVE pool, before the
# ineligible legacy-forest filter -- that filter lives at 2_empirics.R:1704, in the
# panel-building stage, long after the cleaning has run. Our `basis_sample` already
# has it applied, which withheld ~11.9k ineligible parcels from the conflict graph
# and so changed which OTHER parcels got dropped. Default to the pre-filter column;
# CR_POOL_PRE_P1=0 restores the old (incorrect) ordering.
elig <- fread(file.path(emp_dir, "parcel_eligibility.csv"))
pre_p1 <- Sys.getenv("CR_POOL_PRE_P1", unset = "1") != "0"
samp_col <- if (pre_p1 && "in_sample_2019" %in% names(elig)) "in_sample_2019" else
            if ("basis_sample" %in% names(elig)) "basis_sample" else "in_sample"
pool <- elig[class != "never_eligible" & get(samp_col) == TRUE, .(car_id, class, area_ha)]
dr <- fread(file.path(emp_dir, sprintf("parcel_defo_%d.csv", RULE_YEAR)))[
  , .(car_id, defo_rule = deforested_area_ha)]
pool <- merge(pool, dr, by = "car_id", all.x = TRUE)
pool[is.na(defo_rule), defo_rule := 0]
message("pool (", samp_col, ", target classes): ", nrow(pool))

# ---- direction rows ------------------------------------------------------------
pr <- fread(here("data", "intermediate", "car", "CAR_overlap_variables_conflicts.csv"),
            select = c("int_area", "carid_reference", "carid_target"))
pr[, `:=`(a = as.character(carid_reference), b = as.character(carid_target))]
pr <- pr[a %in% pool$car_id & b %in% pool$car_id & a != b]
pr[, int_ha := int_area / 1e4]
pr <- pr[, .(int_ha = max(int_ha)), by = .(a = pmin(a, b), b = pmax(a, b))]
pr[, pair := paste(a, b)]

rows <- rbind(pr[, .(i = a, j = b, int_ha, pair)], pr[, .(i = b, j = a, int_ha, pair)])
rows <- merge(rows, pool[, .(i = car_id, area_i = area_ha, defo_i = defo_rule)], by = "i")
rows <- merge(rows, pool[, .(j = car_id, area_j = area_ha, defo_j = defo_rule)], by = "j")
rows[, `:=`(pct_i = int_ha / area_i, pct_j = int_ha / area_j)]
rows <- rows[pct_i > OVERLAP_GATE]
rows[, muni := sub("^[A-Z]{2}-([0-9]+)-.*$", "\\1", i)]
rows <- rows[grepl("^[0-9]{6,7}$", muni)]
setorder(rows, muni, i, j)
conflicted <- unique(rows$i)
message("direction rows ", nrow(rows), " | pairs ", uniqueN(rows$pair),
        " | conflicted i-parcels ", length(conflicted))

# ---- deforestation inside each pair intersection, cached ------------------------
cache <- if (file.exists(cache_f)) fread(cache_f) else
  data.table(pair = character(), dint = numeric())
todo <- setdiff(unique(rows$pair), cache$pair)
message("pairs to measure: ", length(todo), " (cached ", nrow(cache), ")")

if (length(todo) > 0) {
  geo <- do.call(rbind, lapply(
    c(here("data", "intermediate", "car", "car_eligible_cleaned.shp"),
      here("data", "intermediate", "car", "car_ineligible_cleaned.shp")),
    function(f) {
      x <- st_read(f, quiet = TRUE)
      x <- x[, "car_id", drop = FALSE]
      x$car_id <- as.character(x$car_id)
      x
    }))
  geo <- geo[geo$car_id %in% unique(c(rows$i, rows$j)), ]
  geo <- st_transform(st_make_valid(geo), 4326)
  gi <- setNames(seq_len(nrow(geo)), geo$car_id)
  tiles <- list.files(tile_dir, pattern = paste0("_", RULE_YEAR, "\\.tif$"), full.names = TRUE)
  text <- do.call(rbind, lapply(tiles, function(tf) as.vector(ext(rast(tf)))))

  td <- unique(rows[pair %in% todo, .(pair, i, j)], by = "pair")
  out <- numeric(nrow(td))
  t0 <- Sys.time()
  for (r in seq_len(nrow(td))) {
    out[r] <- tryCatch({
      inter <- suppressWarnings(st_intersection(geo[gi[[td$i[r]]], ], geo[gi[[td$j[r]]], ]))
      if (nrow(inter) == 0 || all(st_is_empty(inter))) 0 else {
        bb <- st_bbox(inter)
        hit <- which(!(text[, 2] < bb["xmin"] | text[, 1] > bb["xmax"] |
                       text[, 4] < bb["ymin"] | text[, 3] > bb["ymax"]))
        tot <- 0
        for (h in hit) {
          ex <- tryCatch(terra::extract(rast(tiles[h]), vect(inter)), error = function(e) NULL)
          if (!is.null(ex)) tot <- tot + sum(ex[[2]] == 2L, na.rm = TRUE)
        }
        tot * 0.09
      }
    }, error = function(e) NA_real_)
    if (r %% 2500 == 0) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      message(sprintf("  measured %d/%d | %.1f min | ~%.0f min left",
                      r, nrow(td), el, el / r * (nrow(td) - r)))
    }
  }
  cache <- rbind(cache, data.table(pair = td$pair, dint = out))
  fwrite(cache, cache_f)
  message("cached ", nrow(cache), " pair measurements")
}

rows <- merge(rows, cache, by = "pair", all.x = TRUE)
rows[, evaluable := defo_i > 0 & defo_j > 0 & !is.na(dint)]
rows[, di := fifelse(evaluable, as.integer(dint / defo_i * 100 >= DEFO_CUT), NA_integer_)]
rows[, dj := fifelse(evaluable, as.integer(dint / defo_j * 100 >= DEFO_CUT), NA_integer_)]
rows[, case := fifelse(pct_i >= CONTAIN_CUT & pct_j >= CONTAIN_CUT, "overlap",
                fifelse(pct_i >= CONTAIN_CUT, "i_in_j",
                fifelse(pct_j >= CONTAIN_CUT, "j_in_i", "overlap")))]
message("evaluable rows ", sum(rows$evaluable), " / ", nrow(rows))

# ---- resolution ----------------------------------------------------------------
dec <- list()
kept_all <- character(0)
t0 <- Sys.time()
munis <- unique(rows$muni)

for (mi in seq_along(munis)) {
  rw <- rows[muni == munis[mi]]
  alive <- unique(c(rw$i, rw$j))
  row_alive <- rep(TRUE, nrow(rw))

  # pass 1: the drop rules, sequential over rows (legacy does not skip a pair whose
  # member already died -- see conflict_resolution_issues.md #C3)
  for (r in seq_len(nrow(rw))) {
    if (!rw$evaluable[r]) next
    A <- rw$i[r]; B <- rw$j[r]; di <- rw$di[r]; dj <- rw$dj[r]; cs <- rw$case[r]
    dropA <- dropB <- FALSE
    if (cs == "overlap") {
      if (di == 1 && dj == 0) dropA <- TRUE
      if (di == 0 && dj == 1) dropB <- TRUE
    } else if (cs == "j_in_i") {          # i contains j
      if (di == 1) dropA <- TRUE else if (dj == 1) dropB <- TRUE
    } else {                               # i_in_j: i contained by j
      if (di == 1 && dj == 0) dropA <- TRUE
      if (dj == 1) dropB <- TRUE
    }
    if (dropA || dropB) {
      who <- if (dropA) A else B
      alive <- setdiff(alive, who)
      row_alive[r] <- FALSE
      dec[[length(dec) + 1L]] <- data.table(
        car_id = who, action = "drop", other_id = if (dropA) B else A)
    }
  }

  # pass 2: the survivors' 0+0 and 1+1 cases
  ok <- row_alive & rw$evaluable & rw$i %in% alive
  if (REQUIRE_J_ALIVE) ok <- ok & rw$j %in% alive
  surv <- rw[ok]

  deleted <- character(0); done <- character(0); members <- character(0)
  if (nrow(surv) > 0) {
    for (r in seq_len(nrow(surv))) {
      A <- surv$i[r]; B <- surv$j[r]
      if (A %in% deleted || B %in% deleted) next
      if (surv$pair[r] %in% done) { members <- c(members, A); next }
      done <- c(done, surv$pair[r])
      if (surv$case[r] == "overlap" && surv$di[r] == 1 && surv$dj[r] == 1) {
        loser <- if (runif(1) < 0.5) A else B
        deleted <- c(deleted, loser)
        dec[[length(dec) + 1L]] <- data.table(
          car_id = loser, action = "drop_random", other_id = if (loser == A) B else A)
        if (loser != A) members <- c(members, A)
      } else {
        victim <- if (runif(1) < 0.5) A else B
        dec[[length(dec) + 1L]] <- data.table(
          car_id = victim, action = "erase_intersection_random",
          other_id = if (victim == A) B else A)
        members <- c(members, A)
      }
    }
  }

  keep_m <- setdiff(unique(members), deleted)
  if (!WINNER_VANISHES) {
    keep_m <- union(keep_m, setdiff(intersect(alive, unique(rw$i)), deleted))
  }
  if (!DROP_UNEVALUABLE) {
    une <- setdiff(unique(rw[evaluable == FALSE]$i), unique(rw[evaluable == TRUE]$i))
    keep_m <- union(keep_m, setdiff(une, deleted))
  }
  kept_all <- c(kept_all, keep_m)

  if (mi %% 50 == 0) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    message(sprintf("  %d/%d munis | %.1f min", mi, length(munis), el))
  }
}

decisions <- unique(rbindlist(dec, fill = TRUE))
dropped_ids <- unique(decisions[action %in% c("drop", "drop_random")]$car_id)
decisions <- decisions[!(action == "erase_intersection_random" & car_id %in% dropped_ids)]

kept <- setdiff(unique(c(setdiff(pool$car_id, conflicted), kept_all)), dropped_ids)
res <- pool[car_id %in% kept]

fwrite(decisions, file.path(emp_dir, paste0("conflict_decisions_", BASIS, ".csv")))
fwrite(res[, .(car_id, class)], file.path(emp_dir, paste0("parcels_resolved_", BASIS, ".csv")))

cat("\n=== CONFLICT RESOLUTION (", BASIS, ") ===\n", sep = "")
print(decisions[, .N, by = action][order(-N)])
cat("\nkept ", nrow(res), " of ", nrow(pool), "\n", sep = "")
print(res[, .N, by = class][order(class)])
cat("\npaper Table 1: eligible 71,171 | ineligible 15,254 (Table 2 implies ~16,134)\n")
cat("Wrote: conflict_decisions_", BASIS, ".csv, parcels_resolved_", BASIS, ".csv\n", sep = "")
