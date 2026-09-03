#!/usr/bin/env bash
# run_step3_only.sh
# ---------------------------------------------------------------------------
# Resume driver for step 3 (transitions) + step 4 (biome-masked stats) ONLY.
#
# Why this exists: the `grids/` intermediate (step-0 output, ~36 GB) was deleted
# to free disk space. Steps 0-2 are already complete (their outputs `legacy/` and
# `transitions/` remain), and step 3 reads ONLY those two dirs -- it never touches
# `grids/`. So we resume by running step 3 directly instead of the full driver
# (run_full_production.sh), which would try to REGENERATE grids at step 0.
#
# Resumable: step 3 skips any tile whose grid_<base>_cover.rds marker exists.
# Memory: MB_STEP3_JOBS controls concurrency; default 1 (sequential) is the only
# OOM-safe setting at the 19.5 GiB Docker limit -- interior tiles peak ~18-19 GiB.
#
# Runs INSIDE the container:
#   make docker-run CMD="bash code/01_build/04_mapbiomas/run_step3_only.sh"
# ---------------------------------------------------------------------------
set -uo pipefail
shopt -s nullglob

MB="code/01_build/04_mapbiomas"
LEGACY_DIR="data/intermediate/mapbiomas/legacy"
COMBINED_DIR="data/intermediate/mapbiomas/transitions_combined"
JOBS="${MB_STEP3_JOBS:-1}"

ts()  { date '+%Y-%m-%d %H:%M:%S'; }
log() { echo "[$(ts)] $*"; }
hr()  { echo "==================================================================="; }

hr; log "STEP 3+4 resume (grids deleted; steps 0-2 skipped, their outputs reused)"
log "working dir: $(pwd)"; log "disk:"; df -h . | sed 's/^/    /'; hr

tiles=( "$LEGACY_DIR"/grid_*_legacy.rds )
n_total=${#tiles[@]}
if [ "$n_total" -eq 0 ]; then
  log "ERROR: no legacy tiles in $LEGACY_DIR -- aborting."; exit 1
fi
n_have=$(ls "$COMBINED_DIR"/grid_*_cover.rds 2>/dev/null | wc -l | tr -d ' ')
log "legacy tiles: $n_total | already done: $n_have | remaining: $((n_total - n_have)) | concurrency: $JOBS"

# Warm the shared C++ kernel cache ONCE before fanning out.
first_base=$(basename "${tiles[0]}" | sed 's/^grid_//; s/_legacy\.rds$//')
log "precompiling C++ kernel (warming shared cache) ..."
MB_ONLY_BASE="$first_base" Rscript "$MB/3_compute_transitions.R" >/dev/null 2>&1 || true

hr; log "BEGIN step 3 : transitions (one tile per R process, $JOBS concurrent)"; hr
t0=$SECONDS
for f in "${tiles[@]}"; do
  basename "$f" | sed 's/^grid_//; s/_legacy\.rds$//'
done | xargs -P "$JOBS" -I{} sh -c 'MB_ONLY_BASE="$1" Rscript "'"$MB"'/3_compute_transitions.R"' _ {}
n_done=$(ls "$COMBINED_DIR"/grid_*_cover.rds 2>/dev/null | wc -l | tr -d ' ')
log "step 3 done in $(( SECONDS - t0 ))s : markers=$n_done / $n_total (missing=$(( n_total - n_done )))"

hr; log "BEGIN step 4 : biome-masked deforestation stats"; hr
Rscript "$MB/4_deforestation_stats.R"
rc4=$?
log "step 4 exit=$rc4"

hr; log "disk after:"; df -h . | sed 's/^/    /'; hr

# Non-zero if any tile is still missing OR step 4 failed, so the launcher does
# not mark the run complete.
if [ "$n_done" -lt "$n_total" ]; then log "INCOMPLETE: $((n_total - n_done)) tiles missing"; exit 2; fi
exit "$rc4"
