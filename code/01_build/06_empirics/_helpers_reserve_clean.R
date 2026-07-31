# Legacy's reserve-pool cleaning (2_empirics.R:1852-1955), factored out so the
# diagnostic (stage 8) and the production panel (stage 13) share one implementation.
#
# Unlike the Appendix C algorithm used for the gleba pool, this one never drops a
# parcel: for every pair overlapping > 10% of i's DECLARED area, the intersection is
# erased from the LARGER parcel (by declared area), sequentially over the pair list.
# Legacy's rationale, verbatim: "FOR PROPERTIES IN REZ/CONSER THERE'S NO REAL
# DEFINITION OF PROPERTY LINES SO THE CLEANING ONLY REMOVES OVERLAPS TO AVOID
# DOUBLE COUNTING".

reserve_clean <- function(shp, crs_eq = 5880, progress_every = 25L) {
  st_erase <- function(x, y) suppressWarnings(st_difference(x, sf::st_union(sf::st_geometry(y))))
  muni_of <- function(x) sub("^[A-Z]{2}-([0-9]+)-.*$", "\\1", x)
  shp$muni <- muni_of(shp$car_id)

  munis <- sort(unique(shp$muni))
  out <- vector("list", length(munis))

  for (mi in seq_along(munis)) {
    car <- shp[shp$muni == munis[mi], ]
    car <- car[!duplicated(car$car_id), ]

    if (nrow(car) > 1) {
      idx <- sf::st_intersects(car, car)
      pr <- list()
      for (i in seq_len(nrow(car))) {
        js <- setdiff(idx[[i]], i)
        if (length(js) == 0) next
        inter <- suppressWarnings(
          sf::st_intersection(sf::st_geometry(car[i, ]), sf::st_geometry(car[js, ])))
        if (length(inter) == 0) next
        ha <- as.numeric(sf::st_area(sf::st_transform(sf::st_sfc(inter, crs = 4326), crs_eq))) / 1e4
        ok <- which(ha / car$NUM_ARE[i] > 0.1)
        if (length(ok) > 0) {
          pr[[length(pr) + 1L]] <- data.table::data.table(
            a = car$car_id[i], b = car$car_id[js[ok]],
            num_a = car$NUM_ARE[i], num_b = car$NUM_ARE[js[ok]])
        }
      }
      if (length(pr)) {
        pairs <- data.table::rbindlist(pr)
        for (r in seq_len(nrow(pairs))) {
          big <- if (pairs$num_a[r] >= pairs$num_b[r]) pairs$a[r] else pairs$b[r]
          sml <- if (pairs$num_a[r] >= pairs$num_b[r]) pairs$b[r] else pairs$a[r]
          gi <- which(car$car_id == big); gj <- which(car$car_id == sml)
          if (length(gi) == 0 || length(gj) == 0) next
          res <- try(st_erase(car[gi, ], car[gj, ]), silent = TRUE)
          if (!inherits(res, "try-error") && nrow(res) > 0 && !all(sf::st_is_empty(res))) {
            sf::st_geometry(car)[gi] <- sf::st_geometry(res)[1]
          }
        }
      }
    }

    gt <- sf::st_geometry_type(car)
    if (any(gt == "GEOMETRYCOLLECTION")) {
      for (k in which(gt == "GEOMETRYCOLLECTION")) {
        ext <- try(sf::st_collection_extract(car[k, ], "POLYGON"), silent = TRUE)
        if (!inherits(ext, "try-error") && nrow(ext) > 0) {
          sf::st_geometry(car)[k] <- sf::st_union(sf::st_geometry(ext))
        }
      }
    }
    gt <- sf::st_geometry_type(car)
    car <- car[!gt %in% c("LINESTRING", "MULTILINESTRING", "POINT", "MULTIPOINT"), ]

    out[[mi]] <- car
    if (mi %% progress_every == 0) message("  reserve-clean munis: ", mi, "/", length(munis))
  }

  sf::st_make_valid(do.call(rbind, out))
}
