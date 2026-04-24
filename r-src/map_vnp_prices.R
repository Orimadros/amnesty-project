################################################################################
# Map VNP Land Prices for Amazon Municipalities
# 
# This script creates maps showing average land prices (2016-2022) for Amazon
# municipalities with pastagem data, and generates summary statistics tables.
#
# Output: 
#   - Maps: Municipality boundaries colored by average price
#   - Tables: LaTeX summary statistics for land prices
#   - Data: RDS file with municipality panel data
#
# Data sources:
#   - IHS region boundaries: patricio_preach_tomas_work/data/clean/IHS_regions_divison.Rdata
#   - VNP prices: patricio_preach_tomas_work/data/clean/vnp/city_region_yearly_pt.csv
#   - Municipality boundaries: geobr package
################################################################################

# ── Libraries ─────────────────────────────────────────────────────────────────
library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(geobr)
library(tidyr)
library(scales)
library(kableExtra)
library(stringi)

# ── Paths ─────────────────────────────────────────────────────────────────────
patricio_path <- file.path(
  "patricio_preach_tomas_work"
)

regions_file <- file.path(
  patricio_path,
  "data/clean/IHS_regions_divison.Rdata"
)

prices_file <- file.path(
  patricio_path, 
  "data/clean/vnp/city_region_yearly_pt.csv"
)

output_dir <- "output/figures"

# ── 1. Load IHS Region Boundaries ─────────────────────────────────────────────
message("Loading IHS region boundaries...")
load(regions_file)  # creates object: regions_2015

# ── 2. Load VNP Price Data ────────────────────────────────────────────────────
message("Loading VNP price data (2016-2022)...")
vnp_prices <- read_csv(prices_file, show_col_types = FALSE)

# ── 2b. Create Overlay Map: VNP Municipalities over IHS Regions ──────────────
message("Creating overlay map: VNP municipalities over IHS regions...")

# Load all municipalities
all_muni <- read_municipality(code_muni = 'all', year = 2020)
target_states <- c('PA', 'TO', 'AP', 'AM', 'RO', 'RR', 'AC')
target_muni <- all_muni %>%
  filter(abbrev_state %in% target_states)

# Get VNP regions that have pastagem data
# First, identify which regions have pastagem data
price_cols <- names(vnp_prices)[grepl('^preco_', names(vnp_prices))]

# Extract land types
land_types <- c()
for(col in price_cols) {
  parts <- strsplit(col, '_')[[1]]
  if(length(parts) >= 3) {
    land_type <- parts[2]
    land_types <- c(land_types, land_type)
  }
}
col_to_land_type <- setNames(land_types, price_cols)

# Find regions with pastagem data in 2017 (reference year)
pastagem_cols <- names(col_to_land_type)[col_to_land_type == 'pastagem']
pastagem_2017_cols <- pastagem_cols[grepl('_2017$', pastagem_cols)]
regions_with_pastagem <- vnp_prices %>%
  select(region_name, state, all_of(pastagem_2017_cols)) %>%
  # Check if any 2017 pastagem column has non-NA values
  mutate(has_pastagem_2017 = rowSums(!is.na(select(., all_of(pastagem_2017_cols)))) > 0) %>%
  filter(has_pastagem_2017) %>%
  select(region_name, state) %>%
  distinct() %>%
  arrange(region_name)

vnp_regions <- regions_with_pastagem

# Function to normalize names
normalize_name <- function(x) {
  x %>%
    stringi::stri_trans_general('Latin-ASCII') %>%
    tolower() %>%
    trimws()
}

# Normalize both VNP and municipality names
vnp_regions_norm <- vnp_regions %>%
  mutate(region_name_norm = normalize_name(region_name))

target_muni_norm <- target_muni %>%
  mutate(muni_name_norm = normalize_name(name_muni))

# Match with normalized names
exact_matches <- vnp_regions_norm %>%
  left_join(
    target_muni_norm %>% 
      select(muni_name_norm, name_muni, abbrev_state, code_muni, geom),
    by = c('region_name_norm' = 'muni_name_norm')
  )

# Get the matched municipalities with geometry
matched_muni <- exact_matches %>% 
  filter(!is.na(code_muni)) %>%
  st_as_sf()

# Filter IHS regions to only those in the Amazon/northern states
ihs_regions_filtered <- regions_2015 %>%
  filter(state %in% target_states)

message(sprintf("VNP municipalities with pastagem data in 2017: %d", nrow(matched_muni)))
message(sprintf("IHS regions: %d", nrow(ihs_regions_filtered)))

# Create the overlay map
overlay_map <- ggplot() +
  # IHS regions as background (light gray)
  geom_sf(data = ihs_regions_filtered, 
          fill = 'lightgray', 
          color = 'white', 
          size = 0.5,
          alpha = 0.7) +
  # VNP municipalities on top (colored by state)
  geom_sf(data = matched_muni, 
          aes(fill = abbrev_state), 
          color = 'black', 
          size = 0.3) +
  scale_fill_viridis_d(name = 'State') +
  labs(title = 'VNP Municipalities with Pastagem Data (2017) over IHS Regions',
       subtitle = paste('Municipalities with pastagem data in 2017:', nrow(matched_muni), '| IHS Regions:', nrow(ihs_regions_filtered)),
       caption = 'Gray: IHS regions | Colored: VNP municipalities with pastagem data in 2017') +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 0.5, size = 10),
        legend.position = 'bottom')

# Save overlay map
ggsave(
  file.path(output_dir, "vnp_municipalities_overlay.png"),
  plot = overlay_map,
  width = 14, height = 10, dpi = 300, bg = "white"
)
message("  ✓ vnp_municipalities_overlay.png")

# Print summary
message("Municipalities by state:")
print(table(matched_muni$abbrev_state))
message("IHS regions by state:")
print(table(ihs_regions_filtered$state))

# ── 3. Calculate Average Price Per Region ─────────────────────────────────────
message("Calculating average prices across land types and years...")

# Get all price columns and extract land types
price_cols <- names(vnp_prices)[grepl('^preco_', names(vnp_prices))]

# Extract land types using the same method as get_land_price_moments.R
land_types <- c()
for(col in price_cols) {
  parts <- strsplit(col, '_')[[1]]
  if(length(parts) >= 3) {
    land_type <- parts[2]  # Second part after 'preco'
    land_types <- c(land_types, land_type)
  }
}
col_to_land_type <- setNames(land_types, price_cols)

# Pivot to long format and extract land type
vnp_long <- vnp_prices %>%
  pivot_longer(
    cols = starts_with('preco_'),
    names_to = 'variable',
    values_to = 'price'
  ) %>%
  # Add land type based on column name
  mutate(land_type = col_to_land_type[variable]) %>%
  # Extract year from column name (last 4 digits)
  mutate(year = stringr::str_extract(variable, '[0-9]{4}$')) %>%
  # Remove the original variable column
  select(-variable) %>%
  # Filter out missing prices
  filter(!is.na(price)) %>%
  mutate(name_match = toupper(gsub('[[:space:]]', '', region_name)))

# ── 4. Match VNP Regions to IHS Boundaries ────────────────────────────────────
regions_2015_clean <- regions_2015 %>%
  mutate(name_match = toupper(gsub('[[:space:]]', '', region_name)))

# Function to join prices to regions
join_prices <- function(price_df) {
  regions_2015_clean %>%
    inner_join(price_df, by = 'name_match') %>%
    select(
      state,
      region_id,
      region_name,
      mean_price,
      n_obs,
      geometry
    )
}

# Calculate prices for different land types
# All types
price_summary_all <- vnp_long %>%
  group_by(name_match) %>%
  summarise(mean_price = mean(price, na.rm = TRUE), n_obs = n())

# Pastagem only
price_summary_pastagem <- vnp_long %>%
  filter(land_type == 'pastagem') %>%
  group_by(name_match) %>%
  summarise(mean_price = mean(price, na.rm = TRUE), n_obs = n())

# Mata only
price_summary_mata <- vnp_long %>%
  filter(land_type == 'mata') %>%
  group_by(name_match) %>%
  summarise(mean_price = mean(price, na.rm = TRUE), n_obs = n())

# Terra only
price_summary_terra <- vnp_long %>%
  filter(land_type == 'terra') %>%
  group_by(name_match) %>%
  summarise(mean_price = mean(price, na.rm = TRUE), n_obs = n())

# Pastagem 2020-2022
price_summary_pastagem_2020_2022 <- vnp_long %>%
  filter(land_type == 'pastagem', year %in% c('2020', '2021', '2022')) %>%
  group_by(name_match) %>%
  summarise(mean_price = mean(price, na.rm = TRUE), n_obs = n())

# Join to spatial boundaries
vnp_regions_all <- join_prices(price_summary_all)
vnp_regions_pastagem <- join_prices(price_summary_pastagem)
vnp_regions_mata <- join_prices(price_summary_mata)
vnp_regions_terra <- join_prices(price_summary_terra)
vnp_regions_pastagem_2020_2022 <- join_prices(price_summary_pastagem_2020_2022)

message(sprintf("  Matched to spatial boundaries:"))
message(sprintf("    All types: %d regions", nrow(vnp_regions_all)))
message(sprintf("    Pastagem: %d regions", nrow(vnp_regions_pastagem)))
message(sprintf("    Mata: %d regions", nrow(vnp_regions_mata)))
message(sprintf("    Terra: %d regions", nrow(vnp_regions_terra)))
message(sprintf("    Pastagem 2020-2022: %d regions", nrow(vnp_regions_pastagem_2020_2022)))

# ── 5. Load Context Layers ────────────────────────────────────────────────────
message("Loading Brazil and Amazon boundaries...")
amazon <- read_amazon(year = 2012, simplified = TRUE)
brazil <- read_state(year = 2010, simplified = TRUE)

# ── 6. Create Mapping Function ───────────────────────────────────────────────
create_price_map <- function(regions_df, title_suffix, subtitle_text) {
  # Add price labels
  regions_df <- regions_df %>%
    mutate(
      price_label = case_when(
        mean_price >= 1000 ~ sprintf("%.1fK", mean_price / 1000),
        TRUE ~ sprintf("%.0f", mean_price)
      )
    )
  
  ggplot() +
    # Base layers
    geom_sf(
      data = brazil, 
      fill = 'gray95', 
      color = 'gray70', 
      linewidth = 0.3
    ) +
    geom_sf(
      data = amazon, 
      fill = 'lightgreen', 
      color = 'darkgreen', 
      linewidth = 1.2, 
      alpha = 0.15
    ) +
    # Price-colored regions
    geom_sf(
      data = regions_df,
      aes(fill = mean_price),
      color = 'navy',
      linewidth = 0.6,
      alpha = 0.8
    ) +
    # Price labels
    geom_sf_text(
      data = regions_df,
      aes(label = price_label),
      size = 3.5,
      fontface = 'bold',
      color = 'white',
      check_overlap = FALSE
    ) +
    # Color scale
    scale_fill_gradient(
      name = "Mean Price\n(R$/ha)",
      low = "#FFA500",
      high = "#8B0000",
      labels = label_comma(),
      trans = "log10",
      breaks = c(500, 1000, 2000, 5000, 10000)
    ) +
    # Crop to Amazon
    coord_sf(
      xlim = st_bbox(amazon)[c(1, 3)],
      ylim = st_bbox(amazon)[c(2, 4)]
    ) +
    # Theme
    theme_minimal() +
    labs(
      title = paste0('VNP Land Prices: ', title_suffix),
      subtitle = subtitle_text,
      caption = paste(
        'Source: FNP/VNP via IHS Markit regions',
        '| Numbers show mean price (R$/ha)',
        '| Green boundary = Legal Amazon',
        sep = '\n'
      )
    ) +
    theme(
      plot.title = element_text(size = 14, face = 'bold'),
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(size = 8, hjust = 0, lineheight = 1.2),
      panel.background = element_rect(fill = 'aliceblue'),
      legend.position = 'right'
    )
}

# ── 7. Create All Maps ────────────────────────────────────────────────────────
message("Creating maps...")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# All types
map_all <- create_price_map(
  vnp_regions_all,
  "All Land Types (2016-2022)",
  sprintf("%d regions, average across all land types", nrow(vnp_regions_all))
)

# Pastagem
map_pastagem <- create_price_map(
  vnp_regions_pastagem,
  "Pastagem Only (2016-2022)",
  sprintf("%d regions with pastagem data", nrow(vnp_regions_pastagem))
)

# Mata
map_mata <- create_price_map(
  vnp_regions_mata,
  "Mata Only (2016-2022)",
  sprintf("%d regions with mata (forest) data", nrow(vnp_regions_mata))
)

# Terra
map_terra <- create_price_map(
  vnp_regions_terra,
  "Terra Only (2016-2022)",
  sprintf("%d regions with terra (agricultural land) data", nrow(vnp_regions_terra))
)

# ── 8. Save Maps ──────────────────────────────────────────────────────────────
message("Saving maps...")

ggsave(
  file.path(output_dir, "vnp_price_map_all.png"),
  plot = map_all,
  width = 12, height = 10, dpi = 300, bg = "white"
)
message("  ✓ vnp_price_map_all.png")

ggsave(
  file.path(output_dir, "vnp_price_map_pastagem.png"),
  plot = map_pastagem,
  width = 12, height = 10, dpi = 300, bg = "white"
)
message("  ✓ vnp_price_map_pastagem.png")

ggsave(
  file.path(output_dir, "vnp_price_map_mata.png"),
  plot = map_mata,
  width = 12, height = 10, dpi = 300, bg = "white"
)
message("  ✓ vnp_price_map_mata.png")

ggsave(
  file.path(output_dir, "vnp_price_map_terra.png"),
  plot = map_terra,
  width = 12, height = 10, dpi = 300, bg = "white"
)
message("  ✓ vnp_price_map_terra.png")

# ── 8b. Save Pastagem 2020-2022 Data ──────────────────────────────────────────
processed_vnp_dir <- "data/processed/vnp"
dir.create(processed_vnp_dir, recursive = TRUE, showWarnings = FALSE)

vnp_pastagem_2020_2022_sf <- vnp_regions_pastagem_2020_2022
save(
  vnp_pastagem_2020_2022_sf,
  file = file.path(processed_vnp_dir, "vnp_pastagem_2020_2022.Rdata")
)
message(sprintf("  ✓ Saved sf dataframe: %s", 
                file.path(processed_vnp_dir, "vnp_pastagem_2020_2022.Rdata")))

# ── 9. Print Summary ──────────────────────────────────────────────────────────
cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("SUMMARY: VNP Land Prices by Region and Type\n")
cat("════════════════════════════════════════════════════════════════\n\n")

cat("ALL TYPES:\n")
summary_all <- vnp_regions_all %>%
  st_drop_geometry() %>%
  arrange(desc(mean_price)) %>%
  select(region_id, region_name, mean_price, n_obs)
print(summary_all, n = Inf)

cat("\n\nPASTAGEM:\n")
summary_pastagem <- vnp_regions_pastagem %>%
  st_drop_geometry() %>%
  arrange(desc(mean_price)) %>%
  select(region_id, region_name, mean_price, n_obs)
print(summary_pastagem, n = Inf)

cat("\n\nMATA:\n")
summary_mata <- vnp_regions_mata %>%
  st_drop_geometry() %>%
  arrange(desc(mean_price)) %>%
  select(region_id, region_name, mean_price, n_obs)
print(summary_mata, n = Inf)

cat("\n\nTERRA:\n")
summary_terra <- vnp_regions_terra %>%
  st_drop_geometry() %>%
  arrange(desc(mean_price)) %>%
  select(region_id, region_name, mean_price, n_obs)
print(summary_terra, n = Inf)

cat("\n\nPASTAGEM 2020-2022 (saved to data/processed/vnp/):\n")
summary_pastagem_2020_2022 <- vnp_regions_pastagem_2020_2022 %>%
  st_drop_geometry() %>%
  arrange(desc(mean_price)) %>%
  select(region_id, region_name, mean_price, n_obs)
print(summary_pastagem_2020_2022, n = Inf)

cat("\n")

# ── 10. Save Municipality Panel Data as RDS ───────────────────────────────────
message("Saving municipality panel data...")

# Create the municipality panel data for pastagem
municipality_panel <- vnp_long %>%
  filter(land_type == 'pastagem') %>%
  select(region_name, state, year, price) %>%
  # Add land type for clarity
  mutate(land_type = 'pastagem') %>%
  # Reorder columns
  select(region_name, state, land_type, year, price) %>%
  # Rename price to mean_price for consistency
  rename(mean_price = price) %>%
  # Sort by region and year
  arrange(region_name, year)

# Create output directory for RDS
rds_output_dir <- "data/processed/vnp"
dir.create(rds_output_dir, recursive = TRUE, showWarnings = FALSE)

# Save as RDS
rds_file <- file.path(rds_output_dir, "municipality_pastagem_panel.RDS")
saveRDS(municipality_panel, rds_file)

message(sprintf("Municipality panel data saved to: %s", rds_file))
message(sprintf("Panel dimensions: %d rows × %d columns", nrow(municipality_panel), ncol(municipality_panel)))
message(sprintf("Years covered: %s", paste(sort(unique(municipality_panel$year)), collapse = ", ")))
message(sprintf("Municipalities: %d", length(unique(municipality_panel$region_name))))

# Return list of plot objects (useful if sourcing interactively)
invisible(list(
  overlay = overlay_map,
  all = map_all,
  pastagem = map_pastagem,
  mata = map_mata,
  terra = map_terra
))

