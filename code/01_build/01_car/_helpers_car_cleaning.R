# CAR geometry-cleaning helpers.
#
# Faithful port of the `clean_shape_*` functions from the legacy producer
# `legacy_repo/dropbox_producers/create_muni_year_intersections/helper_functions.R`
# (Thiago Alckmin, Oct 2023). Behaviour is preserved exactly; the only change is
# removing the runtime `source()` of two scripts from raw.githubusercontent.com
# (they are not used by these functions) so this module has no network dependency.
#
# These functions define the three cleaning passes referenced by the CAR build:
#   - clean_shape_basic       : make valid, drop duplicate geometries, drop empties
#                               (KEEPS still-invalid shapes)
#   - clean_shape_reenforced  : stricter GEOS "valid_structure" validity, then drop
#                               duplicates, empties, AND still-invalid shapes.
#                               THIS is the pass that produces CleanCARShapes_robust.
#   - clean_shape_s2          : s2-geometry variant, delegates to clean_shape_reenforced.
#
# Determinism: all three are pure functions of their input `sf` object. With the
# container's threading pinned to 1 (see Dockerfile) and s2 enabled, they are
# bit-reproducible. Dropping "duplicate geometries" uses base `duplicated()` on the
# geometry list-column, which is order-stable.

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(magrittr)
})

# make valid, drop duplicate geometries, drop empties. INTENTIONALLY keeps
# still-invalid geometries (matches legacy `clean_shape_basic`).
clean_shape_basic <- function(sf_obj) {
  sf_obj %<>%
    st_make_valid() %>%
    .[!duplicated(.$geometry), ] %>%
    mutate(empty = st_is_empty(geometry)) %>%
    mutate(valid = st_is_valid(geometry))

  sf_obj %<>%
    .[which(sf_obj$empty == FALSE), ]

  return(sf_obj)
}

# like clean_shape_basic but ALSO drops still-invalid geometries (default
# st_make_valid method). Used by stage 02's special-case municipality handling.
clean_shape <- function(sf_obj) {
  sf_obj %<>%
    st_make_valid() %>%
    .[!duplicated(.$geometry), ] %>%
    mutate(empty = st_is_empty(geometry)) %>%
    mutate(valid = st_is_valid(geometry))

  sf_obj %<>%
    .[which(sf_obj$empty == FALSE), ]

  sf_obj %<>%
    .[which(sf_obj$valid == TRUE), ]

  return(sf_obj)
}

# stricter "reenforced" cleaning: GEOS valid_structure method, then drop
# duplicates, empties, and still-invalid geometries. Produces the "robust"
# CAR shapes (Magic File #1).
clean_shape_reenforced <- function(sf_obj) {
  sf_obj %<>%
    st_make_valid(., geos_method = "valid_structure",
                  geos_keep_collapsed = FALSE) %>%
    .[!duplicated(.$geometry), ] %>%
    mutate(empty = st_is_empty(geometry)) %>%
    mutate(valid = st_is_valid(geometry))

  # drop empty geometries
  sf_obj %<>%
    .[which(sf_obj$empty == FALSE), ]

  # drop still-invalid geometries
  sf_obj %<>%
    .[which(sf_obj$valid == TRUE), ]

  return(sf_obj)
}

# s2 variant: round-trip through s2 geometry, then reenforced cleaning.
clean_shape_s2 <- function(car) {
  info <- car %>% st_drop_geometry()
  clean <- car %>% st_as_s2() %>% st_as_sf()
  cbind(clean, info) %>% clean_shape_reenforced()
}
