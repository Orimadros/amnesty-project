#!/usr/bin/env bash
# run_demo_rondonia_step3.sh
# ---------------------------------------------------------------------------
# Memory-safe driver for step 3 of the Rondonia demo: process ONE tile per R
# process (via MB_ONLY_BASE) so the OS reclaims the heavy per-tile memory
# between tiles, then run step 4. Resumable: tiles whose cover.rds marker
# already exists are skipped.
#
# Run (in container):
#   make docker-run CMD="bash code/01_build/04_mapbiomas/test/run_demo_rondonia_step3.sh"
# ---------------------------------------------------------------------------
set -uo pipefail

MB="code/01_build/04_mapbiomas"
D="data/intermediate/demo_rondonia"
YEARS="1987:1996"

for f in "$D"/legacy/grid_*_legacy.rds; do
  base=$(basename "$f" | sed 's/^grid_//; s/_legacy\.rds$//')
  echo "===== tile $base ====="
  MB_ONLY_BASE="$base" MB_LEGACY_DIR="$D/legacy" MB_COVER_DIR="$D/cover" \
    MB_COMBINED_DIR="$D/transitions_combined" MB_YEARS="$YEARS" \
    Rscript "$MB/3_compute_transitions.R"
done

echo "===== step 4: deforestation stats ====="
MB_COMBINED_DIR="$D/transitions_combined" MB_YEARS="$YEARS" \
  MB_STATS_OUT="$D/deforestation.csv" Rscript "$MB/4_deforestation_stats.R"

echo "===== DEMO DONE ====="
