###############################################################################
#  north_top_corr_all.R   –  prints the FNO / North-Brazil top number
#                           for each of the three datasets
###############################################################################

# 1 · North-Brazil state codes  (edit if Pedro’s list differs)
north_states <- c("AC", "AM", "AP", "PA", "RO", "RR", "TO")

# 2 · Region lookup   (drop geometry so sf quirks disappear)
load("data/clean/IHS_regions_divison.Rdata")          # gives regions_2015 (sf)
north_ids <- regions_2015 |>
  sf::st_drop_geometry() |>
  dplyr::filter(state %in% north_states) |>
  dplyr::pull(region_id) |>
  unique() |>
  sort()

# 3 · Helper: average correlation for one dataset --------------------------------
library(purrr)

top_corr <- function(df) {
  mean(
    map_dbl(north_ids, ~ corr_overall(df, region_id = .x)),
    na.rm = TRUE
  )
}

# 4 · Run for each table and print ------------------------------------------------
results <- tibble::tibble(
  dataset = c("eligible", "legal", "ineligible"),
  top_corr = c(
    top_corr(eligible),
    top_corr(legal),
    top_corr(ineligible)
  )
)

print(
  dplyr::mutate(results, top_corr = round(top_corr, 3)),
  width = 60
)

