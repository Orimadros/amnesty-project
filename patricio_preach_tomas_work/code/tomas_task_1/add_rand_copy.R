# ── 0. packages ─────────────────────────────────────────────
library(sf)        # loads {sf} + GEOS/GDAL/PROJ bindings
library(dplyr)     # mutate(), %>%, n(), …

# ── 1. bring the clean object into RAM ─────────────────────
##  (makes muni_division_2015 available in your workspace)
load("data/clean/muni_division_2015.Rdata")

# ── 2. make an *independent* copy & add a random column ────
set.seed(42)                                        # reproducible
muni_division_2015_rand  <- muni_division_2015 |>   # NEW OBJECT
  mutate(rand_val = runif(n()))                     # n() = rows

# ── 3. save the copy under a new file name ─────────────────
save(muni_division_2015_rand,
     file    = "data/clean/muni_division_2015_rand.Rdata",
     version = 3)          # modern, cross-platform R-data format

message("✅  New object with random column written to data/clean/muni_division_2015_rand.Rdata")
