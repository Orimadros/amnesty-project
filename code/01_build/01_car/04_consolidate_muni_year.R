# CAR build, stage 04: consolidate to the municipality x year panel.
#
# Faithful, reproducible port of `D_consolidate.R` section 8 (`fp$OUTPUT_DATA`) from
# `legacy_repo/dropbox_producers/create_muni_year_intersections/` (Thiago Alckmin,
# Oct 2023). This is the producer of "Magic File #2".
#
# Combines the muni-year CAR-union overlap variables (stage 02) with the pairwise
# CAR-conflict variables (stages 03/03b) into one municipality x year panel.
#
# Outputs:
#   data/intermediate/car/muni_year_intersections.csv                <- MAGIC FILE #2
#   data/intermediate/car/muni_year_intersections_variable_desc.csv
#
# NOTE(migration): legacy also wrote an .xlsx copy via writexl; writexl is not in
# renv.lock and the Excel copy is cosmetic, so the variable-description sheet is
# written as a companion CSV instead (issue #19). Legacy's plotting/documentation
# sections (`fp$GENERATE_DOCUMENTATION`) are diagnostics and are not migrated.
# Deviations catalogued in docs/notes/car_migration_issues.md (issues #18-#20).

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(magrittr)
  library(dplyr)
  library(here)
})

source(here("code", "01_build", "01_car", "_helpers_car_util.R"))

# ---- inputs ----------------------------------------------------------------
in_dir <- here("data", "intermediate", "car")
in_sicar_vars_exp   <- file.path(in_dir, "sicar_overlap_variables_exapanded_s2.csv")
in_reftarget_areas  <- file.path(in_dir, "CAR_overlap_variables_reftarget_areas.csv")
in_final            <- file.path(in_dir, "CAR_overlap_variables_final.csv")
in_municipalities   <- here("data", "input", "aux", "municipalities_amazon_biome", "municipalities_amazon_biome.shp")

required_inputs <- c(in_sicar_vars_exp, in_reftarget_areas, in_final, in_municipalities)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(paste(
    "Missing required input(s) for CAR stage 04:",
    paste0(" - ", missing_inputs, collapse = "\n"),
    "These are produced by stages 02 and 03b.",
    sep = "\n"
  ))
}

out_panel <- file.path(in_dir, "muni_year_intersections.csv")
out_desc  <- file.path(in_dir, "muni_year_intersections_variable_desc.csv")

# ---- Amazon-biome municipality filter set -----------------------------------
municipalities_amazon <- read_sf(in_municipalities) %>%
  st_transform(crs = 4674) %>%
  st_make_valid() %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  .[, unique(geocodigo)]

# ---- 8.1: load the three input tables ---------------------------------------
# muni-year CAR-union overlap variables (stage 02); drop the FULL pseudo-year
sicar_vars_exp <- fread(in_sicar_vars_exp) %>% copy() %>%
  .[, municipio := as.numeric(municipio)] %>%
  .[year != "FULL"] %>%
  .[, year := as.numeric(year)]

# pairwise conflicts in reference/target form with own areas (stage 03b)
car_vars_ordered_out <- fread(in_reftarget_areas) %>%
  .[, muni := substr(carid_reference, 4, 10)]

# NOTE(migration): legacy computed this invariant check and discarded the result;
# surfaced as a warning instead (issue #20).
if (nrow(car_vars_ordered_out[year(data_inscricao_reference) >= year(data_inscricao_target)]) !=
    nrow(car_vars_ordered_out)) {
  warning("reftarget invariant violated: some reference CARs pre-date their targets.")
}

# all pairwise overlaps, both directions (stage 03b)
car_vars <- fread(in_final) %>%
  .[, muni := substr(COD_IMOVEL, 4, 10)]

# ---- 8.2.1: conflict counts by intersection-share bucket ---------------------
# NOTE(migration): the misspelled n_ovarlaps_* names are kept verbatim -- downstream
# legacy code (2_empirics) references them.
conflict_buckets <- car_vars_ordered_out %>% copy() %>%
  # exclude self intersections
  .[carid_target != carid_reference] %>%
  # drop duplicate intersections, preferring the better cleaning method
  .[order(cleaning_method)] %>%
  .[, GRP := .GRP, .(carid_reference, carid_target)] %>%
  .[!duplicated(GRP)] %>%
  # intersection area as share of the reference CAR's own area
  .[, int_perc := 100 * (int_area / int_area_reference)] %>%
  .[, bucket := 33] %>%
  .[int_perc > 33, bucket := 66] %>%
  .[int_perc > 66, bucket := 99] %>%
  .[int_perc == 100, bucket := 100] %>%
  # bucket-muni-year counts, wide on bucket
  .[, year := year(data_inscricao_reference)] %>%
  .[, .N, .(bucket, muni, year)] %>%
  dcast.data.table(formula = muni + year ~ bucket, value.var = "N") %>%
  setnafill(cols = intersect(c("33", "66", "99", "100"), names(.)), fill = 0) %>%
  rename_columns(c("33", "66", "99", "100", "muni"),
                 c("n_ovarlaps_33", "n_ovarlaps_66", "n_ovarlaps_99", "n_ovarlaps_100", "municipio")) %>%
  .[, n_overlaps_sum_buckets := n_ovarlaps_33 + n_ovarlaps_66 + n_ovarlaps_99 + n_ovarlaps_100] %>%
  setnafill(cols = c("n_overlaps_sum_buckets"), fill = 0) %>%
  .[, municipio := as.numeric(municipio)] %>%
  .[, year := as.numeric(year)]

# ---- 8.2.2: unique CARs by muni & year ---------------------------------------
# NOTE(migration): ported verbatim, including the max(year*value) construction over
# the melted year columns. If any CAR in car_vars was registered before 2014, the
# wide table gains a pre-2014 column whose NAs are NOT zero-filled, and
# max(year*value) (no na.rm) poisons actual_year to NA for every CAR -- see issue
# #18 and the validation plan there. Kept faithful pending the snapshot diff.
n_cars <- car_vars %>% copy() %>%
  .[, .(COD_IMOVEL.1, data_inscricao.1)] %>%
  rename_columns(c("COD_IMOVEL.1", "data_inscricao.1"), c("COD_IMOVEL", "data_inscricao")) %>%
  rbind(car_vars[, .(COD_IMOVEL, data_inscricao)], .) %>%
  .[!duplicated(COD_IMOVEL)] %>%
  .[, year := year(data_inscricao)] %>%
  .[, muni := substr(COD_IMOVEL, 4, 10)] %>%
  .[, .(COD_IMOVEL, year, muni)] %>%
  .[, one := 1] %>%
  .[!is.na(year)] %>%
  .[year < 2023] %>%
  dcast.data.table(formula = COD_IMOVEL + muni ~ year, value.var = "one") %>%
  setnafill(x = ., cols = intersect(paste0(c(2014:2022)), names(.)), fill = 0) %>%
  melt.data.table(id.vars = c("COD_IMOVEL", "muni")) %>%
  .[, year := as.integer(as.character(variable))] %>%
  .[, actual_year := max(year * value), COD_IMOVEL] %>%
  .[, indic := 0] %>%
  .[actual_year <= year, indic := 1] %>%
  .[, GRP := .GRP, .(year, COD_IMOVEL)] %>% .[!duplicated(GRP)] %>% .[, GRP := NULL] %>%
  .[, sum(indic), .(muni, year)] %>%
  rename_columns(c("V1", "muni"), c("n_unique_cars", "municipio")) %>%
  .[, municipio := as.numeric(municipio)] %>%
  .[, year := as.numeric(year)]

# ---- 8.2.3: unique conflicting CARs by muni & year ----------------------------
cars_intersection <- car_vars_ordered_out %>% copy() %>%
  .[carid_target != carid_reference] %>%
  .[, .(
    carid_reference,
    carid_target,
    year_reference = year(data_inscricao_reference),
    year_target = year(data_inscricao_target)
  )]

n_unique_conflicting_cars <- NULL
for (YEAR in c(2014:2022)) {
  message_with_lines(YEAR)

  conflicting_cars_this_or_past_years <- cars_intersection %>% copy() %>%
    .[(year_reference <= YEAR & year_target <= YEAR)] %>%
    .[, .(carid_reference, carid_target)]

  tmp <- conflicting_cars_this_or_past_years %>% copy() %>%
    .[, index := 1:.N] %>%
    # one column with every CAR id, regardless of reference/target role
    melt.data.table(id.vars = "index") %>%
    .[, .(unique(value))] %>%
    .[, muni := substr(V1, 4, 10)] %>%
    .[, year := YEAR] %>%
    .[, .N, .(muni, year)]

  n_unique_conflicting_cars <- if (is.null(n_unique_conflicting_cars)) {
    tmp %>% copy()
  } else {
    tmp %>% copy() %>% rbind(n_unique_conflicting_cars, .)
  }
}

n_unique_conflicting_cars %<>%
  rename_columns(c("N", "muni"), c("n_unique_conflicting_cars", "municipio")) %>%
  .[, municipio := as.numeric(municipio)] %>%
  .[, year := as.numeric(year)]

# ---- 8.3: consolidate --------------------------------------------------------
out <-
  merge(conflict_buckets, n_cars, c("year", "municipio"), all = TRUE) %>%
  merge(., n_unique_conflicting_cars, c("year", "municipio"), all = TRUE) %>%
  merge(sicar_vars_exp, c("year", "municipio"), all = TRUE) %>%
  .[!is.na(year)] %>%
  setnafill(cols = c("n_new_CARs_microdata", "n_overlaps_sum_buckets", "n_ovarlaps_33",
                     "n_ovarlaps_66", "n_ovarlaps_99", "n_ovarlaps_100",
                     "n_unique_cars", "n_unique_conflicting_cars"), fill = 0) %>%
  .[year < 2023] %>%
  setnafill(x = ., fill = 0,
            cols = c("car_union_area",
                     "car_area_intersect_indi",
                     "car_area_intersect_conserve",
                     "car_area_intersect_forestA",
                     "car_area_intersect_forestB",
                     "car_area_intersect_forestC",
                     "car_area_cancelled",
                     "car_area_notcancelled",
                     "car_area_ca_notca_intersections",
                     "muni_area",
                     "intersect_forestAB",
                     "intersect_forestAC",
                     "intersect_forestBC",
                     "intersect_forestALL",
                     "car_union_area_in_muni")) %>%
  .[order(municipio, year)]

out %>%
  .[municipio %in% municipalities_amazon] %>%
  fwrite(x = ., file = out_panel)

# ---- variable descriptions (companion CSV; legacy wrote an xlsx sheet) --------
variable_desc <- c(
  'year' = "Year",
  'municipio' = "IBGE 7-digit Municipality",
  'n_ovarlaps_33' = "Number of unique (AB=BA) new overlaps/conflicts which intersect with at most 33% of a reference CAR area.",
  'n_ovarlaps_66' = "Number of unique (AB=BA) new overlaps/conflicts which intersect with (33%, 66%] of a reference CAR area.",
  'n_ovarlaps_99' = "Number of unique (AB=BA) new overlaps/conflicts which intersect with (66%, 100%) of a reference CAR area.",
  'n_ovarlaps_100' = "Number of unique (AB=BA) new overlaps/conflicts which intersect with 100% of a reference CAR area.",
  'n_ovarlaps_sum_buckets' = "Summation of n_ovarlaps variables for all buckets by municipality, year",
  'n_new_CARs_microdata' = "Number of new unique reference CARs present in the municipality this year. Includes CARs with any status (e.g. active, cancelled, etc). Sourced from the micro-data.",
  'n_CARs_microdata' = "Number of unique CARs present in the municipality this year. Includes CARs with any status (e.g. active, cancelled, etc). Sourced from the micro-data.",
  'n_unique_cars' = "Number of unique CARs present in the municipality each year. Includes CARs with any status (e.g. active, cancelled, etc). [Technical note: Should contain the CARs which did not self-intersect.]",
  'n_unique_conflicting_cars' = "Number of unique CARs present in the municipality this year which have at least one conflict by 2022-12-31. Includes CARs with any status (e.g. active, cancelled, etc). [Technical note: Does not contain the CAR self-intersections. CARs which were not successfully self-intersected (<1% of sample) may have conflict which we cannot measure.)]",
  'car_union_area' = "Area (m2) of 'Union of the of all CARs in the municipality' this year.",
  'car_union_area_in_muni' = "Area (m2) of 'Union of the of all CARs in the municipality', intersected with municipal boundires, this year. [Technical note: car_union_area_in_muni should equal car_union_area, unless CARs extend beyond municipal boundaries.]",
  'car_area_intersect_indi' = "Area (m2) of the intersection between the 'Union of the of all CARs in the municipality' and indigenous land this year.",
  'car_area_intersect_conserve' = "Area (m2) of the intersection between the 'Union of the of all CARs in the municipality' and conservation land this year.",
  'car_area_intersect_forestA' = "Area (m2) of the intersection between the 'Union of the of all CARs in the municipality' and forested land (type A) this year.",
  'car_area_intersect_forestB' = "Area (m2) of the intersection between the 'Union of the of all CARs in the municipality' and forested land (type B) this year.",
  'car_area_intersect_forestC' = "Area (m2) of the intersection between the 'Union of the of all CARs in the municipality' and forested land (type C) this year.",
  'intersect_forestALL' = "Area (m2) of the union of forested lands assigned to each municipality, year. [Technical note: Variable created to test for overlaps between forested regions and overlap with municipal boundaries.]",
  'intersect_forestAB' = "Area (m2) of the union of forested lands (type = A and type = B) assigned to each municipality, year. [Technical note: Variable created to test for overlaps between different forest types and overlap with municipal boundaries.]",
  'intersect_forestAC' = "Area (m2) of the union of forested lands (type = A and type = C) assigned to each municipality, year. [Technical note: Variable created to test for overlaps between different forest types and overlap with municipal boundaries.]",
  'intersect_forestBC' = "Area (m2) of the union of forested lands (type = B and type = C) assigned to each municipality, year. [Technical note: Variable created to test for overlaps between different forest types and overlap with municipal boundaries.]",
  'car_area_cancelled' = "Area (m2) of the 'Union of all cancelled CARs in the municipality' this year.",
  'car_area_notcancelled' = "Area (m2) of the 'Union of all non-cancelled CARs in the municipality' this year.",
  'car_area_ca_notca_intersections' = "Area (m2) of the intersection between the 'Union of all non-cancelled CARs in the municipality' with 'Union of all cancelled CARs in the municipality', this year.",
  'muni_area' = "(Computed) Area (m2) of the municipality."
) %>% as.data.table(keep.rownames = TRUE) %>%
  rename_columns(c("rn", "."), c("variable", "desc"))

fwrite(variable_desc, out_desc)

message_with_lines(paste0("Stage 04 complete. Wrote: ", out_panel))
