################################################################################
# Wrapper script to generate VNP price map
# 
# Usage:
#   source("code/analysis/run_vnp_price_map.R")
# 
# Or from terminal:
#   Rscript code/analysis/run_vnp_price_map.R
################################################################################

# Set working directory to project root if running from terminal
if (!interactive()) {
  setwd(here::here())
}

# Source the main mapping script
source("r-src/map_vnp_prices.R")

cat("\n✓ Map generation complete!\n")
cat("  View output at: output/figures/vnp_price_map.pdf\n\n")



