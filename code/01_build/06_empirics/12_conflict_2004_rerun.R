# Stage 12 (diagnostic): the N1 re-run -- legacy-faithful conflict cleaning with
# 2004-based rules, on the 2019-rule active target pool.
#
# Reimplements legacy 2_empirics.R:734-1160 semantics exactly as executed (see
# docs/notes/paper_legacy_method_diffs.md N1-N4 and the 2026-07-31 re-audit):
#   - rules evaluated with 2004 deforestation: numerator = 2004-raster pixels in
#     the pair intersection, denominators = the parcels' 2004 deforested areas
#     measured on RAW geometry (inGleba2 semantics -> our parcel_defo_2004.csv);
#   - direction rows gated on intersect / DECLARED area of i > 10% (N4);
#   - rows with either side's 2004 defo == 0 are unevaluable -> excluded from
#     every survivor set (N2);
#   - drop rules as written (>=80 cutoffs, >=0.9 containment, both-contained ->
#     overlap), sequential over rows, ids of resolved rows removed;
#   - overlap 0+0 -> erase one side (parcel survives), overlap 1+1 -> randomly
#     delete one side, containment 0+0 -> erase (survives) [N3: membership only,
#     the erasure itself is not measured here];
#   - FINAL ASSEMBLY keeps only noConflict parcels plus i-side parcels of
#     surviving evaluable rows -- so the WINNER of a fully drop-resolved pair
#     vanishes too unless it has other surviving rows (N6, legacy as-executed).
# Ordering and random draws are seed-controlled here (legacy was unseeded); the
# match is distribution-level, not row-level.
#
# Output: parcels_resolved_2004rules.csv (kept ids) + a Table 1 recompute on the
# F1+F2+P1 basis. Rates use the existing erasure-adjusted panel (approximation:
# the erasure set under 2004 rules differs; N and composition are the primary
# read-outs).

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(data.table)
  library(here)
})

sf_use_s2(FALSE)
SEED <- 20260731
set.seed(SEED)

emp_dir <- here("data", "intermediate", "empirics")
tile_dir <- here("data", "intermediate", "mapbiomas", "transitions_combined")

# ---- pool: active-2019 target parcels ------------------------------------------
elig <- fread(file.path(emp_dir, "parcel_eligibility.csv"))
d19 <- fread(file.path(emp_dir, "parcel_defo_2019.csv"))[, .(car_id, rate_2019 = deforestation_rate)]
elig <- merge(elig, d19, by = "car_id", all.x = TRUE)
pool <- elig[class != "never_eligible" & !is.na(rate_2019) & rate_2019 > 10,
             .(car_id, class, area_ha)]
d04 <- fread(file.path(emp_dir, "parcel_defo_2004.csv"))[, .(car_id, defo04 = deforested_area_ha)]
pool <- merge(pool, d04, by = "car_id", all.x = TRUE)
pool[is.na(defo04), defo04 := 0]
setkey(pool, car_id)
message("active-2019 target pool: ", nrow(pool))

# ---- direction rows ------------------------------------------------------------
pr <- fread(here("data", "intermediate", "car", "CAR_overlap_variables_conflicts.csv"),
            select = c("int_area", "carid_reference", "carid_target"))
pr[, `:=`(a = as.character(carid_reference), b = as.character(carid_target))]
pr <- pr[a %in% pool$car_id & b %in% pool$car_id & a != b]
pr[, int_ha := int_area / 1e4]
pr[, key := fifelse(a < b, paste(a, b), paste(b, a))]
pr <- pr[, .(int_ha = max(int_ha)), by = .(key, a = pmin(a, b), b = pmax(a, b))]
dir_rows <- rbind(pr[, .(i = a, j = b, int_ha, key)],
                  pr[, .(i = b, j = a, int_ha, key)])
dir_rows <- merge(dir_rows, pool[, .(i = car_id, area_i = area_ha, defo_i = defo04)], by = "i")
dir_rows <- merge(dir_rows, pool[, .(j = car_id, area_j = area_ha, defo_j = defo04)], by = "j")
dir_rows[, pct_i := int_ha / area_i]
dir_rows[, pct_j := int_ha / area_j]
dir_rows <- dir_rows[pct_i > 0.1]           # legacy gate: >10% of i's declared area
muni_of <- function(x) sub("^[A-Z]{2}-([0-9]+)-.*$", "\\1", x)
dir_rows[, muni := muni_of(i)]
dir_rows <- dir_rows[grepl("^[0-9]{6,7}$", muni)]
setorder(dir_rows, muni, i, j)
message("direction rows after gate: ", nrow(dir_rows),
        " | unordered pairs involved: ", uniqueN(dir_rows$key),
        " | i-side parcels: ", uniqueN(dir_rows$i))

# parcels with no gate-passing row as i are noConflict -> kept
conflicted <- unique(dir_rows$i)
kept_noconf <- setdiff(pool$car_id, conflicted)
message("noConflict (kept outright): ", length(kept_noconf))

# ---- geometries + tiles --------------------------------------------------------
need_geo <- unique(c(dir_rows$i, dir_rows$j))
geo <- do.call(rbind, lapply(
  c(here("data", "intermediate", "car", "car_eligible_cleaned.shp"),
    here("data", "intermediate", "car", "car_ineligible_cleaned.shp")),
  function(f) {
    x <- st_read(f, quiet = TRUE)
    x <- x[, "car_id", drop = FALSE]
    x$car_id <- as.character(x$car_id)
    x
  }))
geo <- geo[geo$car_id %in% need_geo, ]
geo <- st_transform(st_make_valid(geo), 4326)
gi <- setNames(seq_len(nrow(geo)), geo$car_id)
message("geometries loaded: ", nrow(geo))

tiles <- list.files(tile_dir, pattern = "_2004\\.tif$", full.names = TRUE)
tile_ext <- do.call(rbind, lapply(tiles, function(tf) as.vector(ext(rast(tf)))))
message("2004 tiles: ", length(tiles))

# defo-in-intersection cache per unordered pair
inter_defo <- new.env()
measure_pair <- function(A, B) {
  k <- if (A < B) paste(A, B) else paste(B, A)
  if (!is.null(inter_defo[[k]])) return(inter_defo[[k]])
  ga <- geo[gi[[A]], ]; gb <- geo[gi[[B]], ]
  val <- tryCatch({
    inter <- suppressWarnings(st_intersection(ga, gb))
    if (nrow(inter) == 0 || all(st_is_empty(inter))) 0 else {
      bb <- st_bbox(inter)
      hit <- which(!(tile_ext[, 2] < bb["xmin"] | tile_ext[, 1] > bb["xmax"] |
                     tile_ext[, 4] < bb["ymin"] | tile_ext[, 3] > bb["ymax"]))
      if (length(hit) == 0) 0 else {
        tot <- 0
        for (h in hit) {
          ex <- tryCatch(terra::extract(rast(tiles[h]), vect(inter)), error = function(e) NULL)
          if (!is.null(ex)) tot <- tot + sum(ex[[2]] == 2L, na.rm = TRUE)
        }
        tot * 0.09
      }
    }
  }, error = function(e) NA_real_)
  inter_defo[[k]] <- val
  val
}

# ---- per-municipality legacy resolution ----------------------------------------
munis <- unique(dir_rows$muni)
kept_all <- character(0)
stats <- data.table()
t0 <- Sys.time()

for (mx in seq_along(munis)) {
  m <- munis[mx]
  rows <- dir_rows[muni == m]
  # evaluate rows
  rows[, evaluable := defo_i > 0 & defo_j > 0]
  rows[, di := NA_integer_]
  rows[, dj := NA_integer_]
  ev <- which(rows$evaluable)
  for (r in ev) {
    dint <- measure_pair(rows$i[r], rows$j[r])
    if (is.na(dint)) { rows$evaluable[r] <- FALSE; next }
    rows$di[r] <- as.integer(dint / rows$defo_i[r] * 100 >= 80)
    rows$dj[r] <- as.integer(dint / rows$defo_j[r] * 100 >= 80)
  }
  rows[, case := fifelse(pct_i >= 0.9 & pct_j >= 0.9, "overlap",
                  fifelse(pct_i >= 0.9, "i_in_j",
                  fifelse(pct_j >= 0.9, "j_in_i", "overlap")))]
  # drop loop (sequential, id order = row order)
  alive <- unique(c(rows$i, rows$j))
  row_alive <- rep(TRUE, nrow(rows))
  for (r in seq_len(nrow(rows))) {
    if (!rows$evaluable[r] || is.na(rows$di[r])) next
    A <- rows$i[r]; B <- rows$j[r]
    di <- rows$di[r]; dj <- rows$dj[r]; cs <- rows$case[r]
    dropA <- dropB <- FALSE
    if (cs == "overlap") {
      if (di == 1 && dj == 0) dropA <- TRUE
      if (di == 0 && dj == 1) dropB <- TRUE
    } else if (cs == "j_in_i") {      # i contains j
      if (di == 1) dropA <- TRUE
      else if (dj == 1) dropB <- TRUE
    } else {                           # i_in_j : i contained by j
      if (di == 1 && dj == 0) dropA <- TRUE
      if (dj == 1) dropB <- TRUE
    }
    if (dropA || dropB) {
      if (dropA) alive <- setdiff(alive, A)
      if (dropB) alive <- setdiff(alive, B)
      row_alive[r] <- FALSE
    }
  }
  # surviving evaluable rows with i still alive
  surv <- rows[row_alive & evaluable & !is.na(di) & i %in% alive & j %in% alive]
  # split cases
  deleted <- character(0)
  done_key <- character(0)
  members <- character(0)
  if (nrow(surv) > 0) {
    for (r in seq_len(nrow(surv))) {
      A <- surv$i[r]; B <- surv$j[r]
      if (A %in% deleted || B %in% deleted) next
      cs <- surv$case[r]; di <- surv$di[r]; dj <- surv$dj[r]
      if (cs == "overlap" && di == 1 && dj == 1) {
        if (!surv$key[r] %in% done_key) {
          done_key <- c(done_key, surv$key[r])
          loser <- if (runif(1) < 0.5) A else B
          deleted <- c(deleted, loser)
          members <- c(members, setdiff(c(A), loser))
        } else members <- c(members, A)
      } else {
        # overlap 0+0 (erase) and containment 0+0 (erase): parcel survives
        members <- c(members, A)
      }
    }
  }
  kept_m <- setdiff(unique(members), deleted)
  kept_all <- c(kept_all, kept_m)
  stats <- rbind(stats, data.table(muni = m, rows = nrow(rows),
                                   unevaluable = sum(!rows$evaluable),
                                   kept = length(kept_m)))
  if (mx %% 25 == 0) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    message(sprintf("  %d/%d munis | %.1f min | ~%.0f min left",
                    mx, length(munis), el, el / mx * (length(munis) - mx)))
  }
}

kept <- unique(c(kept_noconf, kept_all))
res_dt <- pool[car_id %in% kept]
message("FINAL kept: ", nrow(res_dt), " of ", nrow(pool))
fwrite(res_dt[, .(car_id, class)], file.path(emp_dir, "parcels_resolved_2004rules.csv"))

# ---- Table 1 recompute (F1 + F2 + N1 cleaning + P1 filter) ---------------------
YEARS_PRE <- 2005:2008
d <- rbindlist(lapply(YEARS_PRE, function(y)
  fread(file.path(emp_dir, sprintf("parcel_defo_%d.csv", y)))))
adj <- fread(file.path(emp_dir, "erasure_adjustment.csv"))
d <- merge(d, adj[, .(car_id, year, er_defo_px, er_valid_px)],
           by = c("car_id", "year"), all.x = TRUE)
d[is.na(er_defo_px), er_defo_px := 0L]
d[is.na(er_valid_px), er_valid_px := 0L]
d[, defor_px := pmax(defor_px - er_defo_px, 0L)]
d[, valid_px := pmax(valid_px - er_valid_px, 0L)]
d[, rate := fifelse(valid_px > 0, defor_px / valid_px * 100, NA_real_)]

lf05 <- d[year == 2005, .(car_id,
  lf05_ha = fifelse(defor_px > 0 & valid_px > 0, valid_px * 0.09, NA_real_))]
samp <- merge(elig[car_id %in% kept], lf05, by = "car_id", all.x = TRUE)
samp <- samp[!(class == "ineligible" & (is.na(lf05_ha) | lf05_ha >= 1e5))]

dd <- merge(d[, .(car_id, year, rate)], samp[, .(car_id, class)], by = "car_id")
paper <- data.table(class = c("eligible", "ineligible"),
                    paper_rate = c(58.4, 11.4), paper_n = c(71171, 15254),
                    paper_area = c(143, 661),
                    paper_defo08 = c(5.1, 4.1), paper_defo14 = c(5.3, 4.7))
res <- merge(
  merge(
    dd[, .(m = mean(rate, na.rm = TRUE)), by = .(class, year)][
      , .(rate_pre2009 = round(mean(m), 1)), by = class],
    samp[, .(n = .N,
             mean_area_ha = round(mean(area_ha, na.rm = TRUE), 1),
             defor_Mha_2008 = round(sum(defor_ha_2008, na.rm = TRUE) / 1e6, 3),
             defor_Mha_2014 = round(sum(defor_ha_2014, na.rm = TRUE) / 1e6, 3)), by = class],
    by = "class"),
  paper, by = "class")

cat("\n===== N1 RE-RUN: F1 + F2 + legacy-faithful 2004-rule cleaning + P1 =====\n")
print(as.data.frame(res))
cat("\nyearly means:\n")
print(dcast(dd[, .(m = round(mean(rate, na.rm = TRUE), 1)), by = .(class, year)],
            class ~ year, value.var = "m"))
fwrite(res, file.path(emp_dir, "table1_test_N1rerun.csv"))
cat("\nWrote: table1_test_N1rerun.csv, parcels_resolved_2004rules.csv\n")
