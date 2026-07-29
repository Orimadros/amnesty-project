# Stage 2 of the Lavoura chain.
#
# Attaches the FNP Lavoura price panel (2002-2017) to each CAR parcel, via the
# IHS region the parcel falls in, for all three CAR categories
# (eligible / ineligible / legal).
#
# Migrated from:
#   legacy_repo/code/patricio_preach_tomas_work/code/
#     Tomas_Lavoura_processing_NB_merge/2.match_lavoura_data.R
#
# Deviations from the legacy script are recorded in
# docs/notes/lavoura_migration_issues.md -- in particular the legacy script's
# 96 per-year-per-category CSVs are replaced by one wide panel per category
# plus a coverage summary (issue #L2).

library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)
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

YEARS <- 2002:2017
CATEGORIES <- c("eligible", "ineligible", "legal")

ihs_dir <- here("data", "intermediate", "car", "ihs_breakdown")
in_master <- here(
  "data", "intermediate", "lavoura", "fnp_lavoura_2002_2017_with_state.csv"
)

out_dir <- here("data", "intermediate", "lavoura")
out_coverage <- file.path(out_dir, "lavoura_parcel_coverage_by_year.csv")

car_files <- setNames(
  file.path(ihs_dir, paste0(CATEGORIES, "_car_IHS.Rdata")),
  CATEGORIES
)

missing <- car_files[!file.exists(unlist(car_files))]
if (length(missing) > 0 || !file.exists(in_master)) {
  stop(
    paste(
      "Missing input(s) required by Lavoura step 2:",
      paste0(" - ", c(unname(missing), in_master[!file.exists(in_master)]),
             collapse = "\n"),
      "Run VTN step 6 and Lavoura step 1 first.",
      sep = "\n"
    )
  )
}

ensure_dir(out_dir)

# ---- 1) Lavoura prices, wide -------------------------------------------------
# Non-numeric entries ("-", "") and zeros both mean "no price recorded".
lavoura <- read_csv(in_master, show_col_types = FALSE)

lav_wide <- lavoura %>%
  select(region_id, all_of(as.character(YEARS))) %>%
  mutate(across(
    all_of(as.character(YEARS)),
    ~ {
      v <- suppressWarnings(as.numeric(.x))
      v[!is.na(v) & v == 0] <- NA_real_
      v
    }
  )) %>%
  mutate(region_id = as.numeric(region_id))

dups <- lav_wide %>% count(region_id) %>% filter(n > 1)
if (nrow(dups) > 0) {
  warning(
    nrow(dups), " duplicated region_id row(s) in the Lavoura master; ",
    "keeping the first of each."
  )
  lav_wide <- lav_wide %>% group_by(region_id) %>% slice(1) %>% ungroup()
}

names(lav_wide)[names(lav_wide) != "region_id"] <-
  paste0("price_", as.character(YEARS), "_lavoura")

price_cols <- paste0("price_", as.character(YEARS), "_lavoura")

# ---- 2) Per-category join ----------------------------------------------------
coverage <- map_dfr(CATEGORIES, function(cat_nm) {
  message("Processing CAR category: ", cat_nm)

  obj_name <- paste0(cat_nm, "_car")
  loaded <- load(car_files[[cat_nm]])
  parcels_sf <- get(obj_name)

  n_raw <- nrow(parcels_sf)

  # VTN step 6 joins with left = TRUE, so parcels outside every IHS region
  # carry NA region_id. The legacy script used an inner spatial join, so those
  # parcels were dropped; reproduce that by filtering here.
  parcels <- parcels_sf %>%
    st_drop_geometry() %>%
    filter(!is.na(region_id)) %>%
    mutate(region_id = as.numeric(region_id))

  n_in_region <- nrow(parcels)

  joined <- parcels %>%
    left_join(lav_wide, by = "region_id") %>%
    relocate(state, region_id, region_name)

  out_rds <- file.path(out_dir, paste0(cat_nm, "_parcel_lavoura_wide.rds"))
  out_csv <- file.path(out_dir, paste0(cat_nm, "_parcel_lavoura_wide.csv"))
  write_rds(joined, out_rds)
  write_csv(joined, out_csv)

  message(
    "  parcels: ", n_raw, " -> ", n_in_region, " in an IHS region; wrote ",
    basename(out_rds)
  )

  # Per-year coverage: how many parcels have a Lavoura price that year.
  map_dfr(YEARS, function(yr) {
    col <- paste0("price_", yr, "_lavoura")
    has_price <- !is.na(joined[[col]])
    tibble(
      category = cat_nm,
      year = yr,
      parcels_total = n_in_region,
      parcels_with_price = sum(has_price),
      parcels_without_price = sum(!has_price)
    )
  })
})

write_csv(coverage, out_coverage)
message("Wrote: ", out_coverage)

summary_tbl <- coverage %>%
  group_by(category) %>%
  summarise(
    parcels = first(parcels_total),
    mean_years_priced = round(mean(parcels_with_price / parcels_total) * 16, 2),
    .groups = "drop"
  )
print(as.data.frame(summary_tbl))
