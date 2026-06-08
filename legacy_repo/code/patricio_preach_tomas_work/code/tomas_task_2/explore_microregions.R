library(sf)
library(tidyverse)

# --- Path to the shapefile ----------------------------------------------------
shape_path <- "./../../data/input/microregions/Immediate_regions/RG2017_rgi.shp"

# --- Read the shapefile -------------------------------------------------------
rg <- read_sf(shape_path)

# 1. column names --------------------------------------------------------------
cat("Column names:\n")
print(names(rg))

# 2. number of rows (Immediate-regions) -------------------------------------
cat("\nNumber of Immediate-regions:", nrow(rg), "\n")

# 3. print *all* rows of the attribute table -----------------------------------
cat("\nAll rows (attributes only):\n")
attr_tbl <- st_drop_geometry(rg)          # remove the geometry column
print(attr_tbl, n = nrow(attr_tbl))       # print every row

car_eligible <- st_read("../../data/output/car_eligible_cleaned.shp")
