#!/usr/bin/env bash
# auto_resume.sh
# ---------------------------------------------------------------------------
# Single, self-healing entry point for the full MapBiomas production run.
# Used BOTH for the manual launch and by the LaunchAgent after a reboot.
#
# It is idempotent and safe to fire repeatedly:
#   * if the run already finished  -> exits immediately (completion marker)
#   * if a run is already going     -> exits immediately (live lock file)
#   * otherwise                     -> starts Docker, waits for the daemon,
#                                       then runs the resumable pipeline under
#                                       caffeinate. On clean success it writes
#                                       the completion marker so it never runs
#                                       again.
#
# The pipeline itself (run_full_production.sh) is resumable via skip-if-exists,
# so a reboot at worst loses the single in-flight tile.
#
# Manual launch (gives you your terminal back, logs to run_full.log):
#   nohup "/Users/kristopherjenner/Local projects/Scheinkman/Amnesty/amnesty-project/code/01_build/04_mapbiomas/auto_resume.sh" >> \
#     "/Users/kristopherjenner/Local projects/Scheinkman/Amnesty/amnesty-project/run_full.log" 2>&1 &
# ---------------------------------------------------------------------------
set -uo pipefail

# launchd hands scripts a minimal PATH (/usr/bin:/bin:/usr/sbin:/sbin) that does
# NOT include /usr/local/bin, where the `docker` CLI lives. Set an explicit PATH
# so this works identically whether launched by launchd (reboot) or by hand.
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

PROJ="/Users/kristopherjenner/Local projects/Scheinkman/Amnesty/amnesty-project"
# MB_STEP3_JOBS: number of tiles processed concurrently in step 3. Set here so it
# crosses into the container (env vars on the host don't). EMPIRICAL (2026-07-13,
# 19.5 GiB Docker limit): the big interior tiles (~8.2M px) peak ~9-10 GiB each,
# so even 2 concurrent OOM-kill each other on the interior cluster (kills at 4,
# 3, AND 2; stuck in a kill-retry loop at 2). At 19.5 GiB only 1 (sequential) is
# reliable. To parallelize, raise the Docker RAM limit: ~28 GiB -> 2 workers,
# ~40 GiB -> 3. Bump this number in step with the RAM.
# STEP-3-ONLY resume: grids/ was deleted to free disk space, so we must NOT run
# run_full_production.sh (its step 0 would regenerate grids, ~36 GB + hours).
# run_step3_only.sh resumes step 3 + masked step 4 directly from the surviving
# legacy/ and transitions/ dirs. JOBS=1 is the only OOM-safe setting at 19.5 GiB.
DRIVER="bash -c 'MB_STEP3_JOBS=1 bash code/01_build/04_mapbiomas/run_step3_only.sh'"
DONE_MARKER="$PROJ/data/intermediate/mapbiomas/.full_complete"
LOCK="$PROJ/data/intermediate/mapbiomas/.run.lock"

cd "$PROJ" || { echo "[auto_resume] cannot cd to project"; exit 1; }
mkdir -p "$(dirname "$DONE_MARKER")"

stamp() { date '+%Y-%m-%d %H:%M:%S'; }

# 1) Already finished? Nothing to do.
if [ -f "$DONE_MARKER" ]; then
  echo "[$(stamp)] [auto_resume] completion marker present -- nothing to do."
  exit 0
fi

# 2) A run already in progress (live PID in lock)? Don't double-launch.
if [ -f "$LOCK" ] && kill -0 "$(cat "$LOCK" 2>/dev/null)" 2>/dev/null; then
  echo "[$(stamp)] [auto_resume] a run is already active (pid $(cat "$LOCK")) -- exiting."
  exit 0
fi

# 3) Make sure Docker Desktop is up (it may not auto-start after a reboot).
echo "[$(stamp)] [auto_resume] ensuring Docker is running ..."
open -a Docker 2>/dev/null || true
until docker info >/dev/null 2>&1; do sleep 5; done
echo "[$(stamp)] [auto_resume] Docker daemon is up."

# 4) Take the lock and run the resumable pipeline, keeping the Mac awake.
echo "$$" > "$LOCK"
echo "[$(stamp)] [auto_resume] launching pipeline (pid $$) ..."
caffeinate -dimsu make docker-run CMD="$DRIVER"
rc=$?
rm -f "$LOCK"

if [ "$rc" -eq 0 ]; then
  touch "$DONE_MARKER"
  echo "[$(stamp)] [auto_resume] pipeline finished cleanly -- marked complete."
else
  echo "[$(stamp)] [auto_resume] pipeline exited $rc (not complete) -- will retry on next login."
fi
exit "$rc"
