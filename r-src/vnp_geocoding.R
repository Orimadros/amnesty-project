################################################################################
# VNP Municipality Geocoding
# 
# This script takes the municipality panel data and adds geometry information
# by geocoding each row using their region_name.
#
# Input: data/processed/vnp/municipality_pastagem_panel.RDS
# Output: data/processed/vnp/municipality_pastagem_panel_with_geometry.RDS
################################################################################

# ── Libraries ─────────────────────────────────────────────────────────────────
library(sf)
library(dplyr)
library(geobr)
library(stringi)

# ── 1. Load Municipality Panel Data ──────────────────────────────────────────
message("Loading municipality panel data...")
vnp_prices <- readRDS('data/processed/vnp/municipality_pastagem_panel.RDS')

message(sprintf("Loaded panel data: %d rows × %d columns", nrow(vnp_prices), ncol(vnp_prices)))
message(sprintf("Municipalities: %d", length(unique(vnp_prices$region_name))))

# ── 2. Calculate Average Price by Region-Year ────────────────────────────────
message("Calculating average price by region-year...")

# Calculate average price for each region-year combination
vnp_prices <- vnp_prices %>%
  group_by(region_name, state, land_type, year) %>%
  summarise(mean_price = mean(mean_price, na.rm = TRUE), .groups = 'drop')

message(sprintf("Averaged data: %d rows × %d columns", nrow(vnp_prices), ncol(vnp_prices)))

# ── 3. Load Brazilian Municipalities ─────────────────────────────────────────
message("Loading Brazilian municipalities...")
all_muni <- read_municipality(code_muni = 'all', year = 2020)

# Filter to Amazon/northern states
target_states <- c('PA', 'TO', 'AP', 'AM', 'RO', 'RR', 'AC')
target_muni <- all_muni %>%
  filter(abbrev_state %in% target_states)

message(sprintf("Loaded %d municipalities in target states", nrow(target_muni)))

# ── 4. Geocode Each Row ──────────────────────────────────────────────────────
message("Geocoding each row...")

# Function to normalize names
normalize_name <- function(x) {
  x %>%
    stringi::stri_trans_general('Latin-ASCII') %>%
    tolower() %>%
    trimws()
}

# Normalize municipality names
target_muni_norm <- target_muni %>%
  mutate(muni_name_norm = normalize_name(name_muni))

# Add geometry to each row
vnp_prices_with_geometry <- vnp_prices %>%
  mutate(
    region_name_norm = normalize_name(region_name),
    code_muni = NA,
    name_muni = NA,
    abbrev_state = NA,
    geom = NA
  )

# Geocode each row
for(i in 1:nrow(vnp_prices_with_geometry)) {
  region_name_norm <- vnp_prices_with_geometry$region_name_norm[i]
  
  # Find matching municipality
  match_idx <- which(target_muni_norm$muni_name_norm == region_name_norm)
  
  if(length(match_idx) > 0) {
    # Use the first match
    match_idx <- match_idx[1]
    vnp_prices_with_geometry$code_muni[i] <- target_muni_norm$code_muni[match_idx]
    vnp_prices_with_geometry$name_muni[i] <- target_muni_norm$name_muni[match_idx]
    vnp_prices_with_geometry$abbrev_state[i] <- target_muni_norm$abbrev_state[match_idx]
    vnp_prices_with_geometry$geom[i] <- target_muni_norm$geom[match_idx]
  }
}

# Convert to sf object
vnp_prices_with_geometry <- st_as_sf(vnp_prices_with_geometry)

# Remove the temporary normalization column and duplicate geom column
vnp_prices_with_geometry <- vnp_prices_with_geometry %>%
  select(-region_name_norm, -geom)  # Remove both temp column and duplicate geom

message(sprintf("Panel data with geometry: %d rows × %d columns", 
                nrow(vnp_prices_with_geometry), ncol(vnp_prices_with_geometry)))

# ── 5. Save Enhanced Panel Data ──────────────────────────────────────────────
message("Saving panel data with geometry...")

# Create output directory
output_dir <- "data/processed/vnp"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save as RDS
output_file <- file.path(output_dir, "municipality_pastagem_panel_with_geometry.RDS")
saveRDS(vnp_prices_with_geometry, output_file)

message(sprintf("Enhanced panel data saved to: %s", output_file))

# ── 5. Summary Statistics ────────────────────────────────────────────────────
cat("\n")
cat("════════════════════════════════════════════════════════════════\n")
cat("GEOCODING SUMMARY\n")
cat("════════════════════════════════════════════════════════════════\n\n")

cat("Original panel data:\n")
cat(sprintf("  Rows: %d\n", nrow(vnp_prices)))
cat(sprintf("  Columns: %d\n", ncol(vnp_prices)))
cat(sprintf("  Municipalities: %d\n", length(unique(vnp_prices$region_name))))

cat("\nEnhanced panel data with geometry:\n")
cat(sprintf("  Rows: %d\n", nrow(vnp_prices_with_geometry)))
cat(sprintf("  Columns: %d\n", ncol(vnp_prices_with_geometry)))
cat(sprintf("  Municipalities with geometry: %d\n", 
            length(unique(vnp_prices_with_geometry$region_name[!is.na(vnp_prices_with_geometry$code_muni)]))))

cat("\nMunicipalities by state:\n")
print(table(vnp_prices_with_geometry$abbrev_state, useNA = "ifany"))

cat("\nFiles created:\n")
cat(sprintf("  %s\n", output_file))

message("Geocoding completed successfully!")