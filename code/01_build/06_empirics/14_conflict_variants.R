# Stage 14: narrow the ineligible-count bracket by separating the EXPENSIVE part of
# the conflict algorithm (measuring 2004 deforestation inside each pair
# intersection) from the CHEAP part (the set logic that decides who survives).
#
# Pass 1 caches the per-pair measurement to disk. Pass 2+ replays the resolution
# under a grid of the semantics we cannot read unambiguously out of legacy, across
# several seeds, so the answer is a labelled range rather than one point estimate.
#
# The three toggles (docs/notes/paper_legacy_method_diffs.md N2/N6 and the 2026-08-01
# re-read):
#   require_j_alive  stage 12 required BOTH sides of a surviving row to be alive.
#                    Legacy's filters are `COD_IMO %in% pdfs2` (i-side only) and
#                    `id %in% pdfs3`; there is no j-side test. FALSE = faithful.
#   winner_vanishes  legacy's final assembly keeps conflicted parcels only if they
#                    appear as COD_IMO in overs/insiders_1/insiders_2. A parcel whose
#                    every row was resolved by a drop it WON is in none of them, so it
#                    disappears. TRUE = as-read.
#   drop_unevaluable rows with a zero-deforestation side (or a GEOMETRYCOLLECTION
#                    intersection) never get drop_i/drop_minus_i set, so they fall out
#                    of all three survivor sets too. TRUE = as-read.
#
# Target window (see the same note): Table 1 says 15,254 ineligible, but Table 2's
# 231,833 observations imply ~16,134, so anything in 15.3k-16.1k is consistent with
# the paper's own numbers.

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(data.table)
  library(here)
})

sf_use_s2(FALSE)
emp_dir <- here("data", "intermediate", "empirics")
tile_dir <- here("data", "intermediate", "mapbiomas", "transitions_combined")
cache_f <- file.path(emp_dir, "conflict_pair_defo_2004.csv")

# ---- pool and direction rows ---------------------------------------------------
elig <- fread(file.path(emp_dir, "parcel_eligibility.csv"))
pool <- elig[class != "never_eligible" & basis_sample == TRUE, .(car_id, class, area_ha)]
d04 <- fread(file.path(emp_dir, "parcel_defo_2004.csv"))[, .(car_id, defo04 = deforested_area_ha)]
pool <- merge(pool, d04, by = "car_id", all.x = TRUE)
pool[is.na(defo04), defo04 := 0]
message("pool (basis_sample target parcels): ", nrow(pool))

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
muni_of <- function(x) sub("^[A-Z]{2}-([0-9]+)-.*$", "\\1", x)
rows[, muni := muni_of(i)]
rows <- rows[grepl("^[0-9]{6,7}$", muni)]
setorder(rows, muni, i, j)
conflicted <- unique(rows$i)
message("direction rows: ", nrow(rows), " | pairs: ", uniqueN(rows$pair),
        " | conflicted i-parcels: ", length(conflicted))

# ---- pass 1: measure each pair intersection once, cache to disk -----------------
need <- unique(rows$pair)
if (file.exists(cache_f)) {
  cache <- fread(cache_f)
  message("pair cache on disk: ", nrow(cache), " pairs")
} else cache <- data.table(pair = character(), dint = numeric())
todo <- setdiff(need, cache$pair)
message("pairs still to measure: ", length(todo))

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

  tiles <- list.files(tile_dir, pattern = "_2004\\.tif$", full.names = TRUE)
  text <- do.call(rbind, lapply(tiles, function(tf) as.vector(ext(rast(tf)))))

  todo_dt <- unique(rows[pair %in% todo, .(pair, i, j)], by = "pair")
  out <- vector("numeric", nrow(todo_dt))
  t0 <- Sys.time()
  for (r in seq_len(nrow(todo_dt))) {
    A <- todo_dt$i[r]; B <- todo_dt$j[r]
    out[r] <- tryCatch({
      inter <- suppressWarnings(st_intersection(geo[gi[[A]], ], geo[gi[[B]], ]))
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
                      r, nrow(todo_dt), el, el / r * (nrow(todo_dt) - r)))
    }
  }
  cache <- rbind(cache, data.table(pair = todo_dt$pair, dint = out))
  fwrite(cache, cache_f)
  message("cached ", nrow(cache), " pair measurements -> ", cache_f)
}

rows <- merge(rows, cache, by = "pair", all.x = TRUE)
rows[, evaluable := defo_i > 0 & defo_j > 0 & !is.na(dint)]
rows[, di := fifelse(evaluable, as.integer(dint / defo_i * 100 >= 80), NA_integer_)]
rows[, dj := fifelse(evaluable, as.integer(dint / defo_j * 100 >= 80), NA_integer_)]
rows[, case := fifelse(pct_i >= 0.9 & pct_j >= 0.9, "overlap",
                fifelse(pct_i >= 0.9, "i_in_j",
                fifelse(pct_j >= 0.9, "j_in_i", "overlap")))]
message("evaluable rows: ", sum(rows$evaluable), " / ", nrow(rows))

# ---- pass 2: replay the set logic under each variant ---------------------------
resolve <- function(require_j_alive, winner_vanishes, drop_unevaluable, seed) {
  set.seed(seed)
  kept <- character(0)
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
        if (dropA) alive <- setdiff(alive, A)
        if (dropB) alive <- setdiff(alive, B)
        row_alive[r] <- FALSE
      }
    }
    ok <- row_alive & rw$evaluable & rw$i %in% alive
    if (require_j_alive) ok <- ok & rw$j %in% alive
    surv <- rw[ok]

    deleted <- character(0); done <- character(0); members <- character(0)
    if (nrow(surv) > 0) {
      for (r in seq_len(nrow(surv))) {
        A <- surv$i[r]; B <- surv$j[r]
        if (A %in% deleted || B %in% deleted) next
        if (surv$case[r] == "overlap" && surv$di[r] == 1 && surv$dj[r] == 1) {
          if (!surv$pair[r] %in% done) {
            done <- c(done, surv$pair[r])
            loser <- if (runif(1) < 0.5) A else B
            deleted <- c(deleted, loser)
            if (loser != A) members <- c(members, A)
          } else members <- c(members, A)
        } else members <- c(members, A)
      }
    }
    keep_m <- setdiff(unique(members), deleted)
    if (!winner_vanishes) {
      keep_m <- union(keep_m, setdiff(intersect(alive, unique(rw$i)), deleted))
    }
    if (!drop_unevaluable) {
      une <- setdiff(unique(rw[evaluable == FALSE]$i), unique(rw[evaluable == TRUE]$i))
      keep_m <- union(keep_m, setdiff(une, deleted))
    }
    kept <- c(kept, keep_m)
  }
  unique(c(setdiff(pool$car_id, conflicted), kept))
}

grid <- CJ(require_j_alive = c(TRUE, FALSE), winner_vanishes = c(TRUE, FALSE),
           drop_unevaluable = c(TRUE, FALSE), seed = c(20260731L, 11L, 909L))
setkey(grid, NULL) # CJ returns a keyed table; rbindlist would try to carry the key
res <- rbindlist(lapply(seq_len(nrow(grid)), function(k) {
  keep <- resolve(grid$require_j_alive[k], grid$winner_vanishes[k],
                  grid$drop_unevaluable[k], grid$seed[k])
  s <- pool[car_id %in% keep, .N, by = class]
  data.table(require_j_alive = grid$require_j_alive[k],
             winner_vanishes = grid$winner_vanishes[k],
             drop_unevaluable = grid$drop_unevaluable[k],
             seed = grid$seed[k],
             eligible = fcoalesce(s[class == "eligible", N][1], 0L),
             ineligible = fcoalesce(s[class == "ineligible", N][1], 0L),
             kept_total = length(keep))
}))

# P1 (2005-basis) already applied upstream via basis_sample, so `ineligible` here is
# directly comparable to Table 1's 15,254 and Table 2's implied ~16,134.
res[, in_window := ineligible >= 15254 & ineligible <= 16134]
setorder(res, require_j_alive, winner_vanishes, drop_unevaluable, seed)

cat("\n===== CONFLICT-SEMANTICS GRID (paper: eligible 71,171 | ineligible 15,254-16,134) =====\n")
print(as.data.frame(res))

cat("\n--- mean across seeds ---\n")
print(as.data.frame(res[, .(eligible = round(mean(eligible)), ineligible = round(mean(ineligible)),
                            any_in_window = any(in_window)),
                        by = .(require_j_alive, winner_vanishes, drop_unevaluable)]))

fwrite(res, file.path(emp_dir, "conflict_variants_grid.csv"))
cat("\nWrote: conflict_variants_grid.csv\n")
