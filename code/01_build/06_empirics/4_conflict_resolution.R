# Stage 4 of the empirics chain: the spatial conflict-resolution algorithm.
#
# CAR boundaries are self-declared and unverified, so claims overlap. Before any
# deforestation total is meaningful, each cleared patch must be assigned to exactly
# one claim. This implements Appendix C of the paper:
#
#   1. Drop properties with < 10% deforested area in 2014          [done in stage 2]
#   2. Set aside properties whose overlaps are < 10% of property area
#   3. For each remaining conflicting pair (i, j):
#        - i contains j (overlap > 90%) and > 80% of i's deforestation is in j  -> drop i
#        - i contains j and > 80% of its deforestation is in i                  -> drop j
#        - i intersects j (overlap < 90%) and > 80% of i's deforestation is in
#          the intersection, but not for j                                      -> drop i
#        - > 80% of BOTH deforestation outside the intersection -> keep both, and
#          randomly erase the shared area from one of them
#        - > 80% of BOTH deforestation inside the intersection   -> randomly keep one
#   4. Combine the adjusted properties with those set aside at step 2.
#
# Legacy counterpart: 2_empirics.R lines 731-1170. The rules match; the differences
# are recorded in docs/notes/conflict_resolution_issues.md.
#
# Efficiency: legacy recomputed every pairwise intersection with
# st_intersection(car, car) inside the municipality loop -- the step that made CAR
# stage 03 produce 10-hour municipalities. We already have every pair and its
# intersection AREA from CAR stage 03b, so we only build intersection GEOMETRY for
# the pairs that actually clear the 10% gate (~39k of 139k pairs).
#
# Determinism: two rules draw at random. Legacy used whatever seed R happened to
# have; we set one explicitly, so the output is reproducible but can only match the
# original in distribution, never row-for-row.
#
# Env:
#   CR_YEAR   raster year used to locate deforestation (default 2014)
#   CR_MUNIS  optional file with one municipality code per line (worker sharding)

library(sf)
library(terra)
library(data.table)
library(here)

sf_use_s2(FALSE) # planar overlay; matches the legacy 2_empirics workflow (CAR issue #26)

SEED <- 20260729
OVERLAP_GATE <- 0.10 # step 2: overlaps below this share of own area are set aside
CONTAIN_CUT <- 0.90 # step 3: containment threshold
DEFO_CUT <- 80 # step 3: percent-of-deforestation threshold

CR_YEAR <- as.integer(Sys.getenv("CR_YEAR", unset = "2014"))

ensure_dir <- function(p) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  invisible(p)
}

emp_dir <- here("data", "intermediate", "empirics")
out_dir <- file.path(emp_dir, "conflict_resolution", as.character(CR_YEAR))
ensure_dir(out_dir)

tile_dir <- here("data", "intermediate", "mapbiomas", "transitions_combined")

# ---- inputs ------------------------------------------------------------------
elig <- fread(file.path(emp_dir, "parcel_eligibility.csv"),
              select = c("car_id", "class", "in_sample", "area_ha"))
elig <- elig[in_sample == TRUE]
setkey(elig, car_id)
message("in-sample parcels: ", nrow(elig))

defo <- fread(file.path(emp_dir, paste0("parcel_defo_", CR_YEAR, ".csv")),
              select = c("car_id", "deforested_area_ha"))
setnames(defo, "deforested_area_ha", "defo_ha")
elig <- merge(elig, defo, by = "car_id", all.x = TRUE)

pairs <- fread(here("data", "intermediate", "car", "CAR_overlap_variables_conflicts.csv"),
               select = c("int_area", "carid_reference", "carid_target"))
pairs[, `:=`(a = as.character(carid_reference), b = as.character(carid_target))]
pairs <- pairs[a %in% elig$car_id & b %in% elig$car_id & a != b]
pairs[, int_ha := int_area / 1e4]
pairs <- merge(pairs, elig[, .(a = car_id, area_a = area_ha)], by = "a")
pairs <- merge(pairs, elig[, .(b = car_id, area_b = area_ha)], by = "b")

# Step 2 gate: keep a pair if the overlap exceeds 10% of EITHER side's area, so the
# pair is examined once from whichever side it matters for.
pairs[, `:=`(pct_a = int_ha / area_a, pct_b = int_ha / area_b)]
pairs <- pairs[pct_a > OVERLAP_GATE | pct_b > OVERLAP_GATE]
pairs[, key := fifelse(a < b, paste(a, b), paste(b, a))]
pairs <- unique(pairs, by = "key")
message("conflicting pairs after the 10% gate: ", nrow(pairs))

# Municipality = middle field of the CAR id (e.g. MT-5101704-XXXX).
muni_of <- function(x) sub("^[A-Z]{2}-([0-9]+)-.*$", "\\1", x)
pairs[, muni := muni_of(a)]
pairs <- pairs[grepl("^[0-9]{6,7}$", muni)]

munis <- sort(unique(pairs$muni))
shard <- Sys.getenv("CR_MUNIS", unset = "")
if (nzchar(shard)) {
  munis <- intersect(munis, readLines(shard))
  message("worker shard: ", length(munis), " municipalities")
}
message("municipalities with conflicts: ", length(munis))

# CAR geometries, loaded once.
shp <- rbindlist(list(
  data.table(f = here("data", "intermediate", "car", "car_eligible_cleaned.shp")),
  data.table(f = here("data", "intermediate", "car", "car_ineligible_cleaned.shp"))
))
geo <- do.call(rbind, lapply(shp$f, function(f) {
  x <- st_read(f, quiet = TRUE)
  x <- x[, intersect(c("COD_IMO", "cod_imovel"), names(x))[1], drop = FALSE]
  names(x)[1] <- "car_id"
  x$car_id <- as.character(x$car_id)
  x
}))
geo <- geo[geo$car_id %in% elig$car_id, ]
geo <- st_transform(st_make_valid(geo), 4326)
message("geometries loaded: ", nrow(geo))

defo_lookup <- setNames(elig$defo_ha, elig$car_id)

# ---- per-municipality resolution ---------------------------------------------
set.seed(SEED)
t0 <- Sys.time()
done <- 0L

for (m in munis) {
  out_f <- file.path(out_dir, paste0("muni_", m, ".rds"))
  if (file.exists(out_f)) { done <- done + 1L; next }

  pm <- pairs[muni == m]
  ids <- unique(c(pm$a, pm$b))
  g <- geo[geo$car_id %in% ids, ]
  if (nrow(g) == 0 || nrow(pm) == 0) {
    saveRDS(data.table(car_id = character(), action = character()), out_f)
    done <- done + 1L; next
  }

  # Raster window for this municipality.
  bb <- st_bbox(g)
  tiles <- list.files(tile_dir, pattern = paste0("_", CR_YEAR, "\\.tif$"), full.names = TRUE)
  hit <- Filter(function(tf) {
    e <- as.vector(ext(rast(tf)))
    !(e[2] < bb["xmin"] || e[1] > bb["xmax"] || e[4] < bb["ymin"] || e[3] > bb["ymax"])
  }, tiles)
  if (length(hit) == 0) {
    saveRDS(data.table(car_id = character(), action = character()), out_f)
    done <- done + 1L; next
  }
  rr <- if (length(hit) == 1) rast(hit[[1]]) else do.call(terra::merge, lapply(hit, rast))

  gi <- setNames(seq_len(nrow(g)), g$car_id)
  decisions <- data.table(car_id = character(), action = character())
  dropped <- character(0)

  for (k in seq_len(nrow(pm))) {
    A <- pm$a[k]; B <- pm$b[k]
    if (A %in% dropped || B %in% dropped) next

    ga <- g[gi[[A]], ]; gb <- g[gi[[B]], ]
    inter <- tryCatch(st_intersection(ga, gb), error = function(e) NULL)
    if (is.null(inter) || nrow(inter) == 0 || all(st_is_empty(inter))) next

    dA <- defo_lookup[[A]]; dB <- defo_lookup[[B]]
    if (is.na(dA) || is.na(dB) || dA <= 0 || dB <= 0) next

    ex <- tryCatch(terra::extract(rr, vect(inter)), error = function(e) NULL)
    if (is.null(ex) || nrow(ex) == 0) next
    v <- ex[[2]]
    defo_inter <- sum(v == 2L, na.rm = TRUE) * 0.09

    drop_a <- as.integer(defo_inter / dA * 100 >= DEFO_CUT)
    drop_b <- as.integer(defo_inter / dB * 100 >= DEFO_CUT)

    contained_a <- pm$pct_a[k] >= CONTAIN_CUT # a is (mostly) inside b
    contained_b <- pm$pct_b[k] >= CONTAIN_CUT # b is (mostly) inside a
    case <- if (contained_a && contained_b) "overlap"
            else if (contained_a) "a_in_b"
            else if (contained_b) "b_in_a"
            else "overlap"

    act <- NULL; who <- NULL
    if (case == "overlap") {
      if (drop_a == 1 && drop_b == 0) { who <- A; act <- "drop" }
      else if (drop_a == 0 && drop_b == 1) { who <- B; act <- "drop" }
      else if (drop_a == 1 && drop_b == 1) {
        who <- if (runif(1) < 0.5) A else B; act <- "drop_random"
      } else {
        who <- if (runif(1) < 0.5) A else B; act <- "erase_intersection_random"
      }
    } else if (case == "b_in_a") { # a contains b
      if (drop_a == 1) { who <- A; act <- "drop" }
      else if (drop_b == 1) { who <- B; act <- "drop" }
    } else { # a_in_b : b contains a
      if (drop_b == 1) { who <- B; act <- "drop" }
      else if (drop_a == 1) { who <- A; act <- "drop" }
    }

    if (!is.null(act)) {
      decisions <- rbind(decisions, data.table(car_id = who, action = act))
      if (act %in% c("drop", "drop_random")) dropped <- c(dropped, who)
    }
  }

  saveRDS(unique(decisions), out_f)
  done <- done + 1L
  if (done %% 25 == 0) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
    message(sprintf("  %d/%d munis | %.1f min | ~%.0f min left",
                    done, length(munis), el, el / done * (length(munis) - done)))
  }
}

message("resolution done in ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")

# ---- combine -----------------------------------------------------------------
fs <- list.files(out_dir, pattern = "\\.rds$", full.names = TRUE)
dec <- rbindlist(lapply(fs, readRDS), fill = TRUE)

dropped_ids <- unique(dec[action %in% c("drop", "drop_random")]$car_id)
kept <- elig[!car_id %in% dropped_ids]

fwrite(dec, file.path(emp_dir, paste0("conflict_decisions_", CR_YEAR, ".csv")))
fwrite(kept[, .(car_id, class, area_ha)],
       file.path(emp_dir, paste0("parcels_resolved_", CR_YEAR, ".csv")))

cat("\n=== CONFLICT RESOLUTION SUMMARY ===\n")
print(dec[, .N, by = action][order(-N)])
cat("\nparcels before:", nrow(elig), " dropped:", length(dropped_ids),
    " after:", nrow(kept), "\n")
print(kept[, .(after = .N), by = class][order(class)])
cat("\npaper Table 1: eligible 71,171 | ineligible 15,254 | never_eligible 7,049\n")
