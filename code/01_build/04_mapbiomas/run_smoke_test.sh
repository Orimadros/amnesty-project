#!/usr/bin/env bash
# run_smoke_test.sh
# ---------------------------------------------------------------------------
# Fast end-to-end sanity check of the FULL pipeline wiring before committing to
# the 24-hour run. Exercises the exact same scripts (0 -> 4) via the same
# one-tile-per-process step-3 driver, but scoped to ONE Rondonia tile and a
# handful of years, writing to a THROWAWAY dir so it never touches the real
# production intermediates.
#
# Should finish in a few minutes. If it prints a CSV with sane numbers, the
# wiring, paths, env vars, and container are all good -> clear to launch full.
#
# Run (in container):
#   make docker-run CMD="bash code/01_build/04_mapbiomas/run_smoke_test.sh"
# ---------------------------------------------------------------------------
set -uo pipefail
shopt -s nullglob

MB="code/01_build/04_mapbiomas"
D="data/intermediate/mapbiomas_smoke"     # throwaway scratch, NOT production

# One Rondonia tile: h in [-63,-62], v in [-11,-10]. Grid years must include
# 1985 & 1986 (for the legacy baseline) plus a few transition years.
export MB_H_RANGE="-63"
export MB_V_RANGE="-11"

ts()  { date '+%H:%M:%S'; }
log() { echo "[$(ts)] $*"; }
hr()  { echo "-------------------------------------------------------------------"; }

log "SMOKE TEST starting (throwaway dir: $D)"
rm -rf "$D"; mkdir -p "$D/grids" "$D/legacy" "$D/cover" "$D/transitions_combined"

# --- step 0: grids (1985..1988 so we have the baseline + 2 transition years)
hr; log "step 0: tile rasters"
MB_YEARS="1985:1988" MB_OUTPUT_DIR="$D/grids" \
  Rscript "$MB/0_tile_rasters_to_grids.R"

# --- step 1: legacy baseline
hr; log "step 1: legacy forest"
MB_GRIDS_DIR="$D/grids" MB_LEGACY_DIR="$D/legacy" \
  Rscript "$MB/1_build_legacy_forest.R"

# --- step 2: per-year cover (1987..1988)
hr; log "step 2: classify cover"
MB_GRIDS_DIR="$D/grids" MB_TRANSITIONS_DIR="$D/cover" MB_YEARS="1987:1988" \
  Rscript "$MB/2_classify_cover.R"

# --- step 3: transitions, one tile per process
hr; log "step 3: transitions (one tile per process)"
n=0
for f in "$D"/legacy/grid_*_legacy.rds; do
  base=$(basename "$f" | sed 's/^grid_//; s/_legacy\.rds$//')
  log "  tile $base"
  MB_ONLY_BASE="$base" MB_LEGACY_DIR="$D/legacy" MB_COVER_DIR="$D/cover" \
    MB_COMBINED_DIR="$D/transitions_combined" MB_YEARS="1987:1988" \
    Rscript "$MB/3_compute_transitions.R"
  n=$(( n + 1 ))
done
if [ "$n" -eq 0 ]; then
  log "ERROR: no legacy tiles produced -- smoke test FAILED."; exit 1
fi

# --- step 4: stats
hr; log "step 4: deforestation stats"
MB_COMBINED_DIR="$D/transitions_combined" MB_YEARS="1987:1988" \
  MB_STATS_OUT="$D/smoke_deforestation.csv" \
  Rscript "$MB/4_deforestation_stats.R"

hr; log "SMOKE TEST result CSV:"
cat "$D/smoke_deforestation.csv"
hr; log "SMOKE TEST DONE. If the numbers above look sane, you're clear for the full run."
log "(cleanup: rm -rf $D)"
