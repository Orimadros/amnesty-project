library(here)
library(tidyverse)
library(sf)

VNP_PRICES_PATH <- here('patricio_preach_tomas_work/data/clean/vnp/city_region_yearly_pt.csv')

vnp_prices <- read_csv(VNP_PRICES_PATH)

# Get all price columns
price_cols <- names(vnp_prices)[grepl('^preco_', names(vnp_prices))]

# Extract land types using the script's method
land_types <- c()
for(col in price_cols) {
  parts <- strsplit(col, '_')[[1]]
  if(length(parts) >= 3) {
    land_type <- parts[2]  # Second part after 'preco'
    land_types <- c(land_types, land_type)
  }
}
# Create a mapping of column names to land types
col_to_land_type <- setNames(land_types, price_cols)

# Pivot to long format
vnp_long <- vnp_prices %>%
  pivot_longer(
    cols = starts_with('preco_'),
    names_to = 'variable',
    values_to = 'price'
  ) %>%
  # Add land type based on column name
  mutate(land_type = col_to_land_type[variable]) %>%
  # Extract year from column name (last 4 digits)
  mutate(year = str_extract(variable, '[0-9]{4}$')) %>%
  # Remove the original variable column
  select(-variable) %>%
  # Filter out missing prices
  filter(!is.na(price))

# Pivot back to wide format with price columns by year
vnp_wide_by_land_type <- vnp_long %>%
  # Create price column names
  mutate(price_col = paste0('price_', year)) %>%
  # Select only needed columns
  select(region_name, state, land_type, price_col, price) %>%
  # Pivot to wide format
  pivot_wider(
    names_from = price_col,
    values_from = price,
    names_sort = TRUE,
    values_fn = mean
  ) %>%
  # Sort by region and land type
  arrange(region_name, land_type)

# Display results
cat("FINAL DATA STRUCTURE:\n")
cat("Rows:", nrow(vnp_wide_by_land_type), "\n")
cat("Columns:", ncol(vnp_wide_by_land_type), "\n\n")

cat("COLUMN NAMES:\n")
print(names(vnp_wide_by_land_type))

cat("\nSAMPLE DATA (first 10 rows):\n")
print(head(vnp_wide_by_land_type, 10))

cat("\nUNIQUE LAND TYPES:\n")
print(unique(vnp_wide_by_land_type$land_type))

cat("\nUNIQUE REGIONS:\n")
print(unique(vnp_wide_by_land_type$region_name))

# Extract only pastagem data
pastagem_data <- vnp_wide_by_land_type %>%
  filter(land_type == 'pastagem') %>%
  select(-land_type)  # Remove land_type column since we only have pastagem now

# Filter to only regions that have pastagem data in 2017 (reference year)
pastagem_2017_regions <- pastagem_data %>%
  filter(!is.na(price_2017)) %>%
  select(region_name) %>%
  distinct()

pastagem_data <- pastagem_data %>%
  filter(region_name %in% pastagem_2017_regions$region_name)

cat("\n═══════════════════════════════════════════════════════════════\n")
cat("PASTAGEM DATA ONLY\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("Rows:", nrow(pastagem_data), "\n")
cat("Columns:", ncol(pastagem_data), "\n\n")

# Create moments table for each year
price_years <- c('2016', '2017', '2018', '2019', '2020', '2021', '2022')
price_cols <- paste0('price_', price_years)

# Calculate moments for each year
moments_by_year <- data.frame()

for(year in price_years) {
  price_col <- paste0('price_', year)
  
  if(price_col %in% names(pastagem_data)) {
    year_data <- pastagem_data[[price_col]]
    year_data <- year_data[!is.na(year_data)]  # Remove NAs
    
    if(length(year_data) > 0) {
      moments <- data.frame(
        year = year,
        n_obs = length(year_data),
        min = min(year_data, na.rm = TRUE),
        q1 = quantile(year_data, 0.25, na.rm = TRUE),
        median = median(year_data, na.rm = TRUE),
        mean = mean(year_data, na.rm = TRUE),
        q3 = quantile(year_data, 0.75, na.rm = TRUE),
        max = max(year_data, na.rm = TRUE),
        sd = sd(year_data, na.rm = TRUE)
      )
      
      moments_by_year <- rbind(moments_by_year, moments)
    }
  }
}

# Round numeric columns to 2 decimal places
moments_by_year <- moments_by_year %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

cat("MOMENTS BY YEAR:\n")
print(moments_by_year)

cat("\nSUMMARY:\n")
cat("Total years with data:", nrow(moments_by_year), "\n")
cat("Average observations per year:", round(mean(moments_by_year$n_obs), 1), "\n")
cat("Overall price range: R$", min(moments_by_year$min), "- R$", max(moments_by_year$max), "\n")

print(pastagem_data %>% distinct(region_name), n=50)

# ── Generate LaTeX Table ──────────────────────────────────────────────────────
message("Generating LaTeX table...")

# Create output directory
output_dir <- "output/results"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Create the LaTeX table content
latex_content <- paste0(
  "\\begin{center}\n",
  "\\begin{tabular}{@{}lrrrrrrrr@{}}\n",
  "  \\toprule\n",
  "  \\textbf{Year} & \\textbf{N Municipalities} & \\textbf{Min} & \\textbf{Q1} & \\textbf{Mean} & \\textbf{Median} & \\textbf{Q3} & \\textbf{Max} \\\\\n",
  "  \\midrule\n"
)

# Add data rows
for(i in 1:nrow(moments_by_year)) {
  row <- moments_by_year[i, ]
  latex_content <- paste0(latex_content, 
    sprintf("  %s & %d & %s & %s & %s & %s & %s & %s \\\\\n",
      row$year,
      row$n_obs,
      format(round(row$min), big.mark = ",", scientific = FALSE),
      format(round(row$q1), big.mark = ",", scientific = FALSE),
      format(round(row$mean), big.mark = ",", scientific = FALSE),
      format(round(row$median), big.mark = ",", scientific = FALSE),
      format(round(row$q3), big.mark = ",", scientific = FALSE),
      format(round(row$max), big.mark = ",", scientific = FALSE)
    )
  )
}

latex_content <- paste0(latex_content,
  "  \\bottomrule\n",
  "\\end{tabular}\n",
  "\\end{center}\n"
)

# Write to file
table_file <- file.path(output_dir, "pastagem_summary_table.tex")
writeLines(latex_content, table_file)

message(sprintf("LaTeX table written to: %s", table_file))

vnp_prices %>%
  colnames() %>%
  stringr::str_subset("pastagem")
