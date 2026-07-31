# Stage 15 (diagnostic): does POOL MEMBERSHIP explain the residual?
#
# The 2026-08-01 semantics grid showed the ineligible gap is NOT in the conflict
# algorithm (its ceiling is 14,488 vs Table 1's 15,254). That points upstream, to
# which CARs enter the target/control pools at all. Two known differences there,
# neither previously measured on the parcels that are in NEITHER of our layers:
#
#   S2 — legacy divides the overlap by the GEOMETRIC parcel area (st_area);
#        our build divides by the DECLARED NUM_AREA. Same 1% threshold.
#        The earlier S2 test could only see parcels already in one of our layers,
#        so flips INTO a pool were invisible. This scores every CAR.
#   P2/D6 — legacy scores target overlap against `glebas_alt` as-is; our build first
#        erases control areas from the target layer. A CAR whose gleba overlap sits
#        mostly inside a gleba-and-control region can therefore be target for legacy
#        and in no pool for us.
#
# Reports pool sizes under all four combinations. Read-only: writes a diagnostic
# CSV, touches nothing the pipeline consumes.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(data.table)
  library(here)
  library(stringr)
  library(stringi)
  library(fs)
})

sf_use_s2(FALSE)
CRS_EQ <- 5880
emp_dir <- here("data", "intermediate", "empirics")

normalize_text <- function(x) {
  stringr::str_squish(stringr::str_to_upper(stringi::stri_trans_general(as.character(x), "Latin-ASCII")))
}

amazon <- st_make_valid(st_transform(
  st_read(here("data", "input", "aux", "amazon_biome_border", "amazon_biome_border.shp"),
          quiet = TRUE), 4674))

cnfp <- purrr::map_dfr(dir_ls(here("data", "input", "cnfp", "SHP_2013"),
                              regexp = "\\.shp$", recurse = TRUE),
                       ~ st_read(.x, quiet = TRUE)) %>%
  st_transform(4674) %>% st_make_valid() %>% st_intersection(amazon) %>%
  mutate(g = normalize_text(governo), cl = normalize_text(classe),
         pr = normalize_text(protecao))

control <- st_make_valid(cnfp %>% filter(g == "FEDERAL", str_detect(cl, "UC|TI")))
target_raw <- st_make_valid(cnfp %>% filter(g == "FEDERAL", str_detect(pr, "SEM DESTINACAO")))
target_erased <- st_make_valid(st_difference(target_raw, st_union(st_geometry(control))))
message("control: ", nrow(control), " | target raw: ", nrow(target_raw),
        " | target erased: ", nrow(target_erased))

car <- st_read(here("data", "intermediate", "car", "car_combined_amazonBiome2.shp"), quiet = TRUE)
idc <- intersect(c("COD_IMOVEL", "COD_IMO", "cod_imovel"), names(car))[1]
arc <- intersect(c("NUM_AREA", "NUM_ARE", "num_area"), names(car))[1]
car <- car %>%
  transmute(car_id = as.character(.data[[idc]]),
            declared_ha = suppressWarnings(as.numeric(.data[[arc]]))) %>%
  filter(!is.na(car_id), car_id != "") %>%
  group_by(car_id) %>% slice(1) %>% ungroup() %>%
  st_transform(4674) %>% st_make_valid()
car$geom_ha <- as.numeric(st_area(st_transform(car, CRS_EQ))) / 1e4
message("CARs scored: ", nrow(car))

overlap_ha <- function(mask, label) {
  touch <- lengths(st_intersects(car, mask)) > 0
  message(label, ": ", sum(touch), " CARs touch the mask")
  if (!any(touch)) return(data.table(car_id = character(), ha = numeric()))
  inter <- suppressWarnings(st_intersection(car[touch, c("car_id")], st_geometry(mask)))
  dt <- data.table(car_id = inter$car_id,
                   ha = as.numeric(st_area(st_transform(inter, CRS_EQ))) / 1e4)
  dt[, .(ha = sum(ha, na.rm = TRUE)), by = car_id]
}

ctl_ha <- overlap_ha(control, "control")
tgt_raw_ha <- overlap_ha(target_raw, "target (legacy, un-erased)")
tgt_er_ha <- overlap_ha(target_erased, "target (ours, erased)")

d <- as.data.table(st_drop_geometry(car))
d <- merge(d, ctl_ha[, .(car_id, ctl = ha)], by = "car_id", all.x = TRUE)
d <- merge(d, tgt_raw_ha[, .(car_id, tgt_raw = ha)], by = "car_id", all.x = TRUE)
d <- merge(d, tgt_er_ha[, .(car_id, tgt_er = ha)], by = "car_id", all.x = TRUE)
for (v in c("ctl", "tgt_raw", "tgt_er")) d[is.na(get(v)), (v) := 0]

pools <- function(denom_col, target_col, label) {
  den <- d[[denom_col]]
  ok <- !is.na(den) & den > 0
  ctl_in <- ok & (d$ctl / den) > 0.01
  tgt_in <- ok & !ctl_in & (d[[target_col]] / den) > 0.01
  data.table(variant = label, denominator = denom_col, target_layer = target_col,
             control_n = sum(ctl_in), target_n = sum(tgt_in))
}

res <- rbindlist(list(
  pools("declared_ha", "tgt_er",  "ours (declared + erased target)"),
  pools("declared_ha", "tgt_raw", "declared + legacy un-erased target"),
  pools("geom_ha",     "tgt_er",  "geometric + erased target"),
  pools("geom_ha",     "tgt_raw", "legacy (geometric + un-erased target)")
))

cat("\n===== POOL MEMBERSHIP UNDER EACH DENOMINATOR / TARGET-LAYER CHOICE =====\n")
print(as.data.frame(res))
cat("\nour built layers on disk: target 164,223 | control 13,025\n")

fwrite(res, file.path(emp_dir, "pool_membership_test.csv"))
fwrite(d, file.path(emp_dir, "pool_membership_shares.csv"))
cat("\nWrote: pool_membership_test.csv, pool_membership_shares.csv\n")
