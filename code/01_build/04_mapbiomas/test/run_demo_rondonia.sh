#!/usr/bin/env bash
# run_demo_rondonia.sh
# ---------------------------------------------------------------------------
# Real-data demo: run the MapBiomas backbone (steps 0-4) on core Rondonia
# (the "fishbone" arc of deforestation), 1987-2005. Scoped via the MB_* env
# vars so it exercises the real 44 GB rasters over a small, recognizable
# region that finishes in a few hours. All output lands under the gitignored
# data/intermediate/demo_rondonia/ tree.
#
# Run (in container):
#   make docker-run CMD="bash code/01_build/04_mapbiomas/test/run_demo_rondonia.sh"
# ---------------------------------------------------------------------------
set -euo pipefail

MB="code/01_build/04_mapbiomas"
D="data/intermediate/demo_rondonia"

export MB_INPUT_DIR="data/input/mapbiomas"
export MB_BORDER_SHP="data/input/aux/amazon_biome_border/amazon_biome_border.shp"
export MB_H_RANGE="-65:-61"    # lon tiles covering ~-65..-60
export MB_V_RANGE="-12:-9"     # lat tiles covering ~-12..-8  (core Rondonia, ~20 tiles)

echo "===== step 0: tile 1985-2005 ====="
MB_OUTPUT_DIR="$D/grids" MB_YEARS="1985:2005" Rscript "$MB/0_tile_rasters_to_grids.R"

echo "===== step 1: legacy forest (1985-86) ====="
MB_GRIDS_DIR="$D/grids" MB_LEGACY_DIR="$D/legacy" Rscript "$MB/1_build_legacy_forest.R"

echo "===== step 2: classify cover 1987-2005 ====="
MB_GRIDS_DIR="$D/grids" MB_TRANSITIONS_DIR="$D/cover" MB_YEARS="1987:2005" Rscript "$MB/2_classify_cover.R"

echo "===== step 3: compute transitions 1987-2005 ====="
MB_LEGACY_DIR="$D/legacy" MB_COVER_DIR="$D/cover" MB_COMBINED_DIR="$D/transitions_combined" \
  MB_YEARS="1987:2005" Rscript "$MB/3_compute_transitions.R"

echo "===== step 4: deforestation stats 1987-2005 ====="
MB_COMBINED_DIR="$D/transitions_combined" MB_YEARS="1987:2005" \
  MB_STATS_OUT="$D/deforestation.csv" Rscript "$MB/4_deforestation_stats.R"

echo "===== DEMO DONE ====="
