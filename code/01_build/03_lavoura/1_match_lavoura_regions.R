# Stage 1 of the Lavoura chain.
#
# Reads the FNP "Lavoura" bare-land price table (2002-2017, one row per FNP
# region) and restricts it to the IHS regions that actually contain CAR
# parcels, as determined by VTN step 6.
#
# Migrated from:
#   legacy_repo/code/patricio_preach_tomas_work/code/
#     Tomas_Lavoura_processing_NB_merge/1.match_lavoura_data.R
#
# Deviations from the legacy script are recorded in
# docs/notes/lavoura_migration_issues.md.

library(readxl)
library(dplyr)
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

in_regions <- here("data", "intermediate", "car", "all_car_regions.Rdata")
in_lavoura <- here("data", "input", "landvalues", "vnp", "Lavoura_FNP.xlsx")

out_dir <- here("data", "intermediate", "lavoura")
out_master <- file.path(out_dir, "fnp_lavoura_2002_2017_with_state.csv")
out_coverage <- file.path(out_dir, "fnp_lavoura_coverage_by_year.csv")

for (f in c(in_regions, in_lavoura)) {
  if (!file.exists(f)) {
    stop(
      paste(
        "Missing input required by Lavoura step 1:",
        paste0(" - ", f),
        "Run `make 02_vtn_car` (for all_car_regions) and place the FNP",
        "workbook under data/input/landvalues/vnp/ first.",
        sep = "\n"
      )
    )
  }
}

ensure_dir(out_dir)

# ---- 1) CAR region list (from VTN step 6) ----------------------------------
load(in_regions)

car_tbl <- all_car_regions %>%
  select(region_id, state) %>%
  distinct() %>%
  mutate(region_id = as.numeric(region_id))

message("CAR-bearing IHS regions: ", nrow(car_tbl))

# ---- 2) FNP Lavoura workbook ------------------------------------------------
# skip = 3 lands on the header row (Nº | REGIÃO | TIPO DE TERRA | 2002..2017);
# the final two rows of the sheet are footer notes, not data.
lavoura_raw <- read_excel(
  in_lavoura,
  skip = 3,
  .name_repair = "minimal"
) %>%
  slice(1:(n() - 2)) %>%
  rename(region_id = `Nº`) %>%
  mutate(region_id = as.numeric(region_id))

message("FNP Lavoura regions in workbook: ", nrow(lavoura_raw))

lavoura <- lavoura_raw %>%
  inner_join(car_tbl, by = "region_id") %>%
  relocate(state, .after = region_id)

message("Lavoura regions matched to CAR regions: ", nrow(lavoura))

write_csv(lavoura, out_master)
message("Wrote: ", out_master)

# ---- 3) Per-year coverage report -------------------------------------------
# A price is treated as absent when it is 0 or non-numeric ("-", "").
coverage <- map_dfr(YEARS, function(yr) {
  col_name <- as.character(yr)
  price <- suppressWarnings(as.numeric(lavoura[[col_name]]))
  price[price == 0] <- NA_real_

  tibble(
    year = yr,
    regions_with_price = sum(!is.na(price)),
    regions_without_price = sum(is.na(price))
  )
})

write_csv(coverage, out_coverage)
message("Wrote: ", out_coverage)

print(as.data.frame(coverage))
