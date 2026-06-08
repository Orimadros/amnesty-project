###############################################################################
#  region_level_corr.R  – NB vs Lavoura correlations across regions
#  (no parcel matching; works directly on nb_wide and lav_wide)
###############################################################################

suppressPackageStartupMessages({ library(dplyr) })

# 1 ── years that overlap in both data sets
yrs <- 2002:2017

# 2 ── create NB-mean per region & year --------------------------------------
nb_long <- lapply(yrs, function(yr) {
  nb_cols_yr <- grep(paste0("_", yr, "$"), names(nb_wide), value = TRUE)
  tibble::tibble(
    region_id = nb_wide$region_id,
    year      = yr,
    nb_mean   = rowMeans(nb_wide[, nb_cols_yr], na.rm = TRUE)
  )
}) |> dplyr::bind_rows()

# 3 ── reshape Lavoura once ---------------------------------------------------
lav_long <- lav_wide |>
  tidyr::pivot_longer(
    tidyselect::starts_with("price_"),
    names_to  = "year",
    values_to = "lav_price",
    names_pattern = "price_(\\d{4})_lavoura"
  ) |>
  dplyr::mutate(year = as.integer(year))

# 4 ── join & compute correlations -------------------------------------------
yearly_corr <- dplyr::inner_join(nb_long, lav_long,
                                 by = c("region_id", "year")) |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    cor = cor(nb_mean, lav_price, use = "pair"),
    .groups = "drop"
  )

print(yearly_corr, n = 16)

cat("Overall region-level NB ↔ Lavoura correlation:",
    round(mean(yearly_corr$cor, na.rm = TRUE), 3), "\n")
