library(dplyr)
library(purrr)
library(readr)
library(sf)
library(here)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(path)) {
    stop("Failed to create directory: ", path)
  }
  invisible(path)
}

vtn_clean_dir <- here("data", "clean", "vtn_IHS")
out_root <- here("data", "intermediate", "vtn_parcels_region")
ensure_dir(out_root)

years <- c(2015, 2016, 2017, 2018, 2019, 2021, 2022)

load(here("data", "clean", "IHS_regions_divison.Rdata"))
region_lookup <- regions_2015 %>%
  st_drop_geometry() %>%
  select(region_id, state, region_name) %>%
  distinct()

load(here("data", "intermediate", "car", "ihs_breakdown", "eligible_car_IHS.Rdata"))
load(here("data", "intermediate", "car", "ihs_breakdown", "ineligible_car_IHS.Rdata"))
load(here("data", "intermediate", "car", "ihs_breakdown", "legal_car_IHS.Rdata"))

car_raw <- list(
  eligible = eligible_car,
  ineligible = ineligible_car,
  legal = legal_car
)

car_layers <- imap(car_raw, function(layer, nm) {
  layer %>%
    st_drop_geometry() %>%
    filter(!is.na(region_id)) %>%
    left_join(region_lookup, by = "region_id", suffix = c("", "_lookup")) %>%
    mutate(
      state = coalesce(state, state_lookup),
      region_name = coalesce(region_name, region_name_lookup)
    ) %>%
    select(-any_of(c("state_lookup", "region_name_lookup"))) %>%
    distinct()
})

summary_tbl <- map_dfr(years, function(yr) {
  vtn_path <- file.path(vtn_clean_dir, sprintf("vtn_region_%d.csv", yr))

  if (!file.exists(vtn_path)) {
    warning("Skipping ", yr, ": missing ", vtn_path)
    return(tibble(year = yr, category = NA_character_, matched = NA_integer_, missing = NA_integer_))
  }

  vtn <- read_csv(vtn_path, show_col_types = FALSE)
  if (!("region_id" %in% names(vtn))) {
    stop("Missing `region_id` in ", vtn_path)
  }

  out_dir <- file.path(out_root, yr)
  ensure_dir(out_dir)

  map_dfr(names(car_layers), function(cat) {
    parcels <- car_layers[[cat]]

    joined <- left_join(parcels, vtn, by = "region_id")
    miss <- anti_join(parcels, vtn, by = "region_id")

    write_csv(
      joined,
      file.path(out_dir, sprintf("%s_car_vtn_region_%d.csv", cat, yr))
    )
    write_csv(
      miss,
      file.path(out_dir, sprintf("%s_missing_vtn_region_%d.csv", cat, yr))
    )

    tibble(
      year = yr,
      category = cat,
      matched = nrow(joined) - nrow(miss),
      missing = nrow(miss)
    )
  })
})

write_csv(summary_tbl, file.path(out_root, "summary.csv"))
print(summary_tbl, n = Inf)
message("Done. Outputs written to: ", out_root)
