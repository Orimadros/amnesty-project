# Stage 4b: apply the conflict algorithm's "erase the shared area" rule.
#
# Appendix C's fourth rule keeps BOTH parcels when >80% of each side's deforestation
# lies outside the intersection, and randomly erases the shared area from one of them.
# Stage 4 records that decision; this stage measures what is being erased so the
# parcel's area and per-year deforestation can be reduced accordingly.
#
# Why it matters (docs/notes/code_diff_vs_legacy.md, D1+D2): legacy runs conflict
# resolution BEFORE the eligibility split, so its `area <= 1500` and `occupied by 2004`
# tests see the SHRUNKEN polygons. Roughly 10.6% of in-sample parcels are flagged for
# erasure and lose ~70% of their area on average, so this can move parcels across the
# 1,500 ha boundary and change group membership -- not just levels.
#
# Output: one row per (car_id, year) giving the deforested hectares and legacy-forest
# pixels that fall inside the erased region, for stage 2 to subtract.

library(sf)
library(terra)
library(data.table)
library(here)

sf_use_s2(FALSE)

ensure_dir <- function(p) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  invisible(p)
}

emp_dir <- here("data", "intermediate", "empirics")
tile_dir <- here("data", "intermediate", "mapbiomas", "transitions_combined")
# Which decision set to measure. Default is the 2004-rule, legacy-faithful set from
# stage 16; EMP_DECISIONS=conflict_decisions_2014.csv reverts to stage 4's. The
# erased-region cache and the adjustment table are namespaced by the choice, so the
# two bases can coexist without one silently overwriting the other -- the mixing that
# difference L4 was about.
dec_name <- Sys.getenv("EMP_DECISIONS", unset = "conflict_decisions_2004rules.csv")
basis <- sub("^conflict_decisions_", "", sub("\\.csv$", "", dec_name))
out_dir <- file.path(emp_dir, paste0("erasure_", basis))
ensure_dir(out_dir)

dec_f <- file.path(emp_dir, dec_name)
if (!file.exists(dec_f)) {
  stop("Missing ", dec_f, " -- run the matching conflict-resolution stage first.")
}
message("decision basis: ", basis)

dec <- fread(dec_f)
if (!"other_id" %in% names(dec)) {
  stop("conflict_decisions is missing other_id; re-run 4_conflict_resolution.R.")
}
er <- dec[action == "erase_intersection_random" & !is.na(other_id) & other_id != ""]
message("erasure decisions: ", nrow(er), " over ", uniqueN(er$car_id), " parcels")

# ---- geometries ---------------------------------------------------------------
geo <- do.call(rbind, lapply(
  c(here("data", "intermediate", "car", "car_eligible_cleaned.shp"),
    here("data", "intermediate", "car", "car_ineligible_cleaned.shp")),
  function(f) {
    x <- st_read(f, quiet = TRUE)
    idc <- intersect(c("COD_IMO", "cod_imovel"), names(x))[1]
    x <- x[, idc, drop = FALSE]
    names(x)[1] <- "car_id"
    x$car_id <- as.character(x$car_id)
    x
  }
))
geo <- st_transform(st_make_valid(geo), 4326)
gi <- setNames(seq_len(nrow(geo)), geo$car_id)

# ---- erased region per parcel: union of its intersections with the partners ----
targets <- unique(er$car_id)
message("building erased regions for ", length(targets), " parcels")

build_erased <- function(cid) {
  partners <- er[car_id == cid]$other_id
  partners <- partners[partners %in% names(gi)]
  if (length(partners) == 0 || is.na(gi[[cid]])) return(NULL)
  a <- geo[gi[[cid]], ]
  pieces <- lapply(partners, function(pid) {
    tryCatch(st_geometry(st_intersection(a, geo[gi[[pid]], ])), error = function(e) NULL)
  })
  pieces <- Filter(function(z) !is.null(z) && length(z) > 0, pieces)
  if (length(pieces) == 0) return(NULL)
  u <- tryCatch(st_union(do.call(c, pieces)), error = function(e) NULL)
  if (is.null(u) || length(u) == 0 || all(st_is_empty(u))) return(NULL)
  st_sf(car_id = cid, geometry = u)
}

erased <- rbindlist(lapply(targets, function(cid) {
  g <- build_erased(cid)
  if (is.null(g)) NULL else list(car_id = cid, geom = list(st_geometry(g)))
}), fill = TRUE)

erased_sf <- do.call(rbind, lapply(seq_len(nrow(erased)), function(i) {
  st_sf(car_id = erased$car_id[i], geometry = erased$geom[[i]])
}))
erased_sf <- st_make_valid(erased_sf)
erased_sf$erased_ha <- as.numeric(st_area(st_transform(erased_sf, 5880))) / 1e4
message("erased regions built: ", nrow(erased_sf),
        " | mean erased area ", round(mean(erased_sf$erased_ha, na.rm = TRUE), 1), " ha")

st_write(erased_sf, file.path(out_dir, "erased_regions.gpkg"),
         delete_dsn = TRUE, quiet = TRUE)

# ---- deforestation inside the erased regions, per year -------------------------
years <- sort(as.integer(gsub("\\D", "", basename(
  list.files(emp_dir, pattern = "^parcel_defo_[0-9]{4}\\.csv$")))))
message("measuring erased deforestation for years: ",
        min(years), "-", max(years), " (", length(years), ")")

ev <- vect(erased_sf)
bb <- as.data.table(geom(ev))[, .(xmin = min(x), xmax = max(x),
                                  ymin = min(y), ymax = max(y)), by = geom]
setorder(bb, geom)

res <- vector("list", length(years))
for (yi in seq_along(years)) {
  y <- years[yi]
  out_f <- file.path(out_dir, paste0("erased_defo_", y, ".rds"))
  if (file.exists(out_f)) { res[[yi]] <- readRDS(out_f); next }

  acc <- data.table(car_id = character(), defo_px = integer(), valid_px = integer())
  tiles <- list.files(tile_dir, pattern = paste0("_", y, "\\.tif$"), full.names = TRUE)
  for (tf in tiles) {
    r <- rast(tf)
    e <- as.vector(ext(r))
    hit <- bb[xmax >= e[1] & xmin <= e[2] & ymax >= e[3] & ymin <= e[4], geom]
    if (length(hit) == 0) next
    sel <- ev[hit, ]
    ex <- tryCatch(terra::extract(r, sel), error = function(z) NULL)
    if (is.null(ex) || nrow(ex) == 0) next
    setDT(ex); setnames(ex, 2, "val")
    a <- ex[, .(defo_px = sum(val == 2L, na.rm = TRUE),
                valid_px = sum(val != 0L, na.rm = TRUE)), by = ID]
    a[, car_id := erased_sf$car_id[hit[ID]]]
    acc <- rbind(acc, a[, .(car_id, defo_px, valid_px)])
  }
  tot <- acc[, .(er_defo_px = sum(defo_px), er_valid_px = sum(valid_px)), by = car_id]
  tot[, year := y]
  saveRDS(tot, out_f)
  res[[yi]] <- tot
  message("  ", y, ": ", nrow(tot), " parcels with erased deforestation")
}

adj <- rbindlist(res, fill = TRUE)
adj[, er_defo_ha := er_defo_px * 0.09]
adj <- merge(adj, as.data.table(st_drop_geometry(erased_sf))[, .(car_id, erased_ha)],
             by = "car_id", all.x = TRUE)

adj_f <- file.path(emp_dir, paste0("erasure_adjustment_", basis, ".csv"))
fwrite(adj, adj_f)
# Stage 2 reads the un-suffixed name; point it at whichever basis just ran.
fwrite(adj, file.path(emp_dir, "erasure_adjustment.csv"))
message("Wrote: ", adj_f, " (and erasure_adjustment.csv for stage 2)")
cat("\nparcels adjusted:", uniqueN(adj$car_id), "\n")
cat("mean erased area (ha):", round(mean(erased_sf$erased_ha, na.rm = TRUE), 1), "\n")
print(adj[year %in% c(2004, 2008, 2014),
          .(parcels = .N, mean_erased_defo_ha = round(mean(er_defo_ha), 1)), by = year])
