# ============================================================
# 0. Load packages and region layer
# ------------------------------------------------------------
library(sf)
library(dplyr)
library(ggplot2)

load("data/clean/IHS_regions_divison.Rdata")   # → regions_2015

# ============================================================
# 1. Keep the three region_ids of interest
# ------------------------------------------------------------
sel_ids <- c(122, 123, 130)

regions_sel <- regions_2015 |>
  filter(region_id %in% sel_ids) |>
  mutate(region_label = case_when(
    region_id == 123 ~ "Belém",
    region_id == 130 ~ "Baixo Amazonas",
    region_id == 122 ~ "Ilhas"
  ))

# ============================================================
# 2. Plot Brazil with highlighted & labelled regions
# ------------------------------------------------------------
ggplot() +
  # backdrop: all IHS intermediate regions
  geom_sf(data = regions_2015,
          fill = "grey92",
          colour = "grey70",
          linewidth = 0.1) +
  # highlight the three selected regions
  geom_sf(data  = regions_sel,
          aes(fill = region_label),
          colour   = "black",
          linewidth = 0.3) +
  # text labels placed at polygon centroids
  geom_sf_text(data  = st_centroid(regions_sel),
               aes(label = region_label),
               size = 3) +
  scale_fill_brewer(palette = "Set2",
                    name     = "Region") +
  coord_sf(datum = st_crs(4674)) +   # SIRGAS 2000 CRS
  labs(
    title    = "Belém, Baixo Amazonas & Ilhas – IHS Intermediate Regions (2015)",
    subtitle = "Highlighted against all intermediate regions of Brazil"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    legend.position  = "bottom"
  )

# ============================================================
# 3. Save to file (optional)
# ------------------------------------------------------------
ggsave("plots/ihs_three_regions_no_ids.png", width = 7, height = 8, dpi = 300)
