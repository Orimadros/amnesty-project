# library(sf)


# car_eligible <- st_read("../../data/output/car_eligible_cleaned.shp")

# car_ineligible <- st_read("../../data/output/car_ineligible_cleaned.shp")

# car_ilegal <- st_read("../../data/output/car_ilegal_cleaned.shp")

# Task:
# - Perform a spatial merge of CAR datasets on the municipalities data
# - Do this for any CAR property that intersects any given municipality
# - Find out (for each of eligible, ineligible, and ilegal):
#    - The number of municipalities that contain CAR properties
#    - The names of these municipalities
#    - Are there any overlaps between these?
#
# ====================================================
# Next steps:
# - Sort out ? and accents and re-run
# - Do merge between VTn data set and all_car_munis
# - Report:
#   - How many municipalities we lose per year (i.e in CAR but not VTN)?
#
# - Join CAR data sets with VTN on municipality (CAR -> muni -> VTN)
# - Report:
#   - How many CAR properties have empty VTN data?
#


# ── libraries ───────────────────────────────────────────────
library(sf)
library(glue)
library(dplyr)
library(tidyverse)

# ── paths ───────────────────────────────────────────────────
muni_file <- "data/clean/muni_division_2015.Rdata"
car_dir <- "./../../data/output"

car_files <- list(
  eligible   = "car_eligible_cleaned",
  ineligible = "car_ineligible_cleaned",
  ilegal     = "control_final"
)

out_dir <- "data/output/car_vtn_muni_breakdown" # where we’ll save all CSVs
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ── 1. load the municipalities layer ────────────────────────
load(muni_file) # gives `muni_division_2015`
muni_crs <- st_crs(muni_division_2015)

# ── 2. helper: read & prep a CAR layer ──────────────────────
read_car <- function(layer_name) {
  st_read(
    dsn = car_dir,
    layer = layer_name,
    quiet = TRUE
  ) |>
    st_transform(muni_crs) |>
    st_make_valid()
}

# ── 3. iterate over the three CAR categories ────────────────
muni_lists <- list() # will store the 3 municipality tibbles


for (cat in names(car_files)) {
  message("Processing ", cat, " …")

  car_layer <- read_car(car_files[[cat]])

  # Add muni to all CAR properties
  car_tbl <- car_layer |>
    st_join(muni_division_2015)

  # Save
  name <- glue("{cat}_car")
  assign(name, car_tbl)
  save(list = name, file = file.path(out_dir, paste0(cat, "_car.Rdata")))

  muni_tbl <- car_layer |>
    st_join(muni_division_2015, left = FALSE) |> # intersect join
    st_drop_geometry() |>
    distinct(muni_code, mun_name, state_uf)

  print(muni_tbl |> nrow())

  muni_lists[[cat]] <- muni_tbl # stash for overlaps later

  # save CSV
  write.csv(muni_tbl,
    file = file.path(out_dir, paste0(cat, "_muni.csv")),
    row.names = FALSE
  )
}

# Get set of municipalities in all three groups
all_car_munis <- muni_lists |>
  bind_rows() |>
  distinct()


