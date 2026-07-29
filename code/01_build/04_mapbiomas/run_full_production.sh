#!/usr/bin/env bash
# run_full_production.sh
# ---------------------------------------------------------------------------
# FULL-SCALE, memory-safe driver for the MapBiomas backbone (steps 0 -> 4),
# meant for a long unattended run. Runs entirely INSIDE the container.
#
#   step 0  tile rasters to grids        (all years, script default 1985:2022)
#   step 1  legacy-forest baseline       (needs the 1985 & 1986 grids)
#   step 2  per-year forest/human cover  (script default 1987:2020)
#   step 3  transitions  -- ONE TILE PER R PROCESS via MB_ONLY_BASE, so the OS
#           fully reclaims the heavy per-tile memory between tiles (robust to
#           OOM even at full biome scale).
#   step 4  biome-level deforestation stats CSV.
#
# PRODUCTION DEFAULTS: this driver does NOT set MB_YEARS / MB_H_RANGE /
# MB_V_RANGE. Every script uses its own built-in production default, which is
# the whole point -- the ranges live in the scripts, not here, so they can't
# drift. (In particular step 0 keeps 1985:2022 so the legacy baseline has its
# 1985 & 1986 grids.) The ONLY override is MB_ONLY_BASE in the step-3 loop.
#
# RESUMABLE: every step skips work whose output already exists, so if this dies
# partway you can simply re-run it and it picks up where it stopped.
#
# Launch (from the host, keeping the Mac awake + logging):
#   caffeinate -dimsu nohup make docker-run \
#     CMD="bash code/01_build/04_mapbiomas/run_full_production.sh" \
#     > run_full.log 2>&1 &
# ---------------------------------------------------------------------------
set -uo pipefail
shopt -s nullglob

MB="code/01_build/04_mapbiomas"
LEGACY_DIR="data/intermediate/mapbiomas/legacy"

ts()  { date '+%Y-%m-%d %H:%M:%S'; }
log() { echo "[$(ts)] $*"; }
hr()  { echo "==================================================================="; }

run_step() {   # run_step "label" Rscript ...
  local label="$1"; shift
  hr; log "BEGIN $label"; hr
  local t0=$SECONDS
  "$@"
  local rc=$?
  local dt=$(( SECONDS - t0 ))
  if [ $rc -ne 0 ]; then
    log "FAILED $label (exit $rc) after ${dt}s -- aborting."
    exit $rc
  fi
  log "END $label (${dt}s)"
}

hr; log "MapBiomas FULL production run starting"
log "working dir: $(pwd)"
log "disk before:"; df -h . | sed 's/^/    /'
hr

# --- steps 0 -> 2 (batch; light on memory) ---------------------------------
# Step 0 tiles 1985:2020 only: 1985 & 1986 feed the legacy baseline and
# 1987:2020 feed the transitions. Years 2021-2022 are never read downstream
# (transitions stop at 2020), so tiling them would be wasted time + disk.
run_step "step 0 : tile rasters to grids"  env MB_YEARS="1985:2020" Rscript "$MB/0_tile_rasters_to_grids.R"
run_step "step 1 : legacy-forest baseline"     Rscript "$MB/1_build_legacy_forest.R"
run_step "step 2 : per-year forest/human cover" Rscript "$MB/2_classify_cover.R"

# --- step 3 : ONE TILE PER PROCESS -----------------------------------------
# Each tile is a fresh Rscript (MB_ONLY_BASE) so the OS reclaims the heavy
# per-tile memory between tiles. MB_STEP3_JOBS controls how many tiles run
# concurrently (default 1 = the original sequential behavior). Tiles are
# resumable: any tile whose grid_<base>_cover.rds marker exists is skipped, so
# re-running -- or an OOM-killed worker -- just retries the unfinished tiles.
JOBS="${MB_STEP3_JOBS:-1}"
COMBINED_DIR="data/intermediate/mapbiomas/transitions_combined"
hr; log "BEGIN step 3 : transitions (one tile per R process, $JOBS concurrent)"; hr

tiles=( "$LEGACY_DIR"/grid_*_legacy.rds )
n_total=${#tiles[@]}
if [ "$n_total" -eq 0 ]; then
  log "ERROR: no legacy tiles found in $LEGACY_DIR -- did step 1 run? Aborting."
  exit 1
fi
log "tiles to process: $n_total"

# Warm the shared C++ kernel cache ONCE (single process) before fanning out, so
# workers reuse one compiled .so instead of racing to first-compile it.
first_base=$(basename "${tiles[0]}" | sed 's/^grid_//; s/_legacy\.rds$//')
log "precompiling C++ kernel (warming shared cache) ..."
MB_ONLY_BASE="$first_base" Rscript "$MB/3_compute_transitions.R" >/dev/null 2>&1 || true

t3_start=$SECONDS
# Feed every base into xargs, one fresh Rscript per tile, $JOBS at a time.
# The script itself skips tiles whose marker already exists.
for f in "${tiles[@]}"; do
  basename "$f" | sed 's/^grid_//; s/_legacy\.rds$//'
done | xargs -P "$JOBS" -I{} sh -c 'MB_ONLY_BASE="$1" Rscript "'"$MB"'/3_compute_transitions.R"' _ {}

# Tally by counting completion markers (robust across parallel workers).
n_done=$(ls "$COMBINED_DIR"/grid_*_cover.rds 2>/dev/null | wc -l | tr -d ' ')
n_fail=$(( n_total - n_done ))
log "step 3 done in $(( SECONDS - t3_start ))s : completed=$n_done / $n_total (missing=$n_fail)"
if [ "$n_fail" -gt 0 ]; then
  log "NOTE: $n_fail tiles have no marker yet -- re-run to retry them."
fi

# --- step 4 : biome-level stats --------------------------------------------
run_step "step 4 : biome-level deforestation stats" Rscript "$MB/4_deforestation_stats.R"

hr
log "ALL DONE. failed_tiles=$n_fail"
log "output CSV -> output/tables/mapbiomas_biome_deforestation.csv"
log "disk after:"; df -h . | sed 's/^/    /'
hr

# Non-zero exit if any tile failed, so the launcher/log makes it obvious.
if [ "$n_fail" -gt 0 ]; then exit 2; fi
