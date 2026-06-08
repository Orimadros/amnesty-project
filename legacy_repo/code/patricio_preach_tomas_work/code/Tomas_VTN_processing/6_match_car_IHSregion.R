# ── Libraries ────────────────────────────────────────────────────────
library(sf)
library(tidyverse)
library(glue)

sf_use_s2(FALSE) # keep the s2 engine on

# ── 1. Load the NEW region polygons ──────────────────────────────────
load("data/clean/IHS_regions_divison.Rdata") # object: regions_2015
# If the object is named differently, adjust here:
# regions_2015 <- ihs_regions           # <- example alias if needed

# ── 2. Folder paths ──────────────────────────────────────────────────
car_dir <- "./../../data/output"
out_dir <- "data/output/car_vtn_IHS_breakdown"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ── 3. CAR layer names inside the Geopackage / shapefile -------------
car_files <- list(
  eligible   = "car_eligible_cleaned",
  ineligible = "car_ineligible_cleaned",
  legal     = "already_treated"
)

read_car <- function(layer_name) {
  st_read(dsn = car_dir, layer = layer_name, quiet = TRUE) |>
    st_transform(crs = st_crs(regions_2015)) |> # re-project to match *new* regions
    try(st_make_valid(.), silent = TRUE)
}

# ── 4. Main loop over CAR categories ---------------------------------
region_lists <- list()

for (cat in names(car_files)) {
  message("Processing ", cat, " …")

  # 4a. Load CAR layer
  car_layer <- read_car(car_files[[cat]])

  # 4b. Tag every CAR polygon with its IHS region
  car_tbl <- st_join(car_layer, regions_2015) # ⇦ new join target

  # 4c. Save the full joined layer
  obj_name <- glue("{cat}_car")
  assign(obj_name, car_tbl)
  save(
    list = obj_name,
    file = file.path(out_dir, glue("{cat}_car_IHS.Rdata"))
  )

  # 4d. Table of unique regions touched by this CAR set
  region_tbl <- st_join(car_layer, regions_2015, left = FALSE) |> # ⇦ new join target
    st_drop_geometry() |>
    distinct(state, region_id, region_name)

  print(nrow(region_tbl))
  region_lists[[cat]] <- region_tbl

  write.csv(region_tbl,
    file = file.path(out_dir, glue("{cat}_IHS.csv")),
    row.names = FALSE
  )
}

# ── 5. Union of regions across all CAR groups ------------------------
all_car_regions <- bind_rows(region_lists) |> distinct()
save(all_car_regions, file="all_car_regions.Rdata")
