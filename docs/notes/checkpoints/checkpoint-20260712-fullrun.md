# Checkpoint 20260712 — MapBiomas full production run (in progress)

Handoff so a new chat can continue. Point the new session here:
"read docs/notes/checkpoints/checkpoint-20260712-fullrun.md and continue."

## TL;DR — where we are RIGHT NOW (2026-07-12 ~19:11 EDT)

The full-scale MapBiomas backbone run (all years, full Amazon biome) is **running and
healthy**, currently in **step 3**, tile **~5 / 713 done**. Steps 0, 1, 2 are complete.
It is going to take ~3 days as-is. We were **about to parallelize step 3 to finish it
by tomorrow** — that is the pending action.

- Branch: `mapbiomas-migration`
- Run is launched via a launchd auto-resume agent (survives reboots) — see below.
- Disk: **41 GB free**, projected step-3 output only ~10 GB. Space is FINE.
- Docker: 12 CPUs, **15.6 GiB RAM** (user set the slider to 16 GB earlier).

## The immediate decision (pending user)

Step 3 runs one tile at a time using **1 of 12 cores + ~4.5 GB RAM per tile** (11 cores
idle). Plan to speed it up (~3 days -> ~18-24 h, "done by tomorrow"):

1. **User bumps Docker RAM to ~20 GB + swap ~4 GB** (Docker Desktop -> Settings ->
   Resources; this restarts Docker and stops the current step 3 — fine, it's resumable).
2. **Relaunch step 3 in PARALLEL**, 3-4 tiles concurrently (RAM-limited, NOT core-limited:
   interior tiles need ~7 GB each, machine has 26 GB). **Precompile the C++ kernel once**
   before launching parallel workers (else 713 processes each recompile, AND concurrent
   `sourceCpp` first-compiles race/clobber). Then run step 4.
3. **Monitor `docker stats`** and tune worker count (drop to 3 if memory tight — avoid another
   low-RAM reboot; push to 4 if headroom).

Resumable: relaunching skips tiles whose `grid_<base>_cover.rds` marker already exists.

### Rejected speedups (don't re-propose)
- **Skip zero-forest tiles**: UNSAFE. 197/713 tiles have 0 *legacy* (1985-86) forest, but they
  can still contain forest that appeared and was cleared after 1986, so dropping them would
  corrupt deforestation totals.
- **Rewriting the (x,y) character-key merge** to use less memory: touches the exact
  pixel-matching logic preserved verbatim from legacy — too risky mid-run.

## Progress detail (all timestamps in log are container/UTC; subtract 4h for EDT)

- **Step 0** (tile rasters -> grids): DONE. `written=1086 skipped=24582`. ~1 h (mostly skips).
  Trimmed to years **1985:2020** (2021-22 are unused downstream) via `env MB_YEARS` in the driver.
- **Step 1** (legacy 1985&86 forest baseline): DONE. 713 tiles. ~6 h. legacy/ dir = ~70 GB.
- **Step 2** (per-year forest/human cover 1987:2020): DONE. 34 years x 713 tiles. ~14 h.
  transitions/ (cover) dir ~49 GB.
- **Step 3** (transitions, one R process per tile): IN PROGRESS, ~5/713.
  ~366 s/tile so far. Writes 34 compressed GTiffs/tile to transitions_combined/.
- **Step 4** (biome deforestation stats CSV): not started.

## KEY FIX applied this session — GTiff compression (verify it stays!)

`code/01_build/04_mapbiomas/3_compute_transitions.R` `writeRaster` was UNCOMPRESSED — at
713 tiles x 34 years that's hundreds of GB and would have crashed the disk. Added (lossless,
values unchanged):
```r
gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=6"), datatype = "INT1U"
```
VERIFIED WORKING: tifs are ~250-580 KB each (~14 MB per tile of 34), vs ~8-14 MB each
uncompressed. Total step-3 output projected ~10 GB.

## How the run is orchestrated (files created this session, all UNCOMMITTED)

- `code/01_build/04_mapbiomas/run_full_production.sh` — the driver: step 0 -> 1 -> 2 ->
  (per-tile loop) step 3 -> 4, at production defaults. Step 0 pinned to MB_YEARS=1985:2020.
  **Do NOT edit while it's running** (bash reads it live). Editing the not-yet-running
  `3_*.R` / `4_*.R` is safe (fresh Rscript per tile reads them when reached).
- `code/01_build/04_mapbiomas/auto_resume.sh` — self-healing entry point: brings Docker up,
  takes a lock (`data/intermediate/mapbiomas/.run.lock`), runs the driver under `caffeinate`,
  writes `data/intermediate/mapbiomas/.full_complete` on clean success. Has explicit
  `PATH=/usr/local/bin:...` (launchd's minimal PATH omits /usr/local/bin where `docker` lives —
  this bit us once).
- `~/Library/LaunchAgents/com.amnesty.mapbiomas-resume.plist` — LaunchAgent, RunAtLoad, fires
  auto_resume.sh at each login (so a reboot resumes the moment the user logs in; FileVault is
  ON + auto-login OFF, so it can't resume fully headless before login). Loaded via
  `launchctl load -w`. **To disarm when the whole project is done:**
  `launchctl unload ~/Library/LaunchAgents/com.amnesty.mapbiomas-resume.plist && rm ~/Library/LaunchAgents/com.amnesty.mapbiomas-resume.plist`

### To parallelize (implementation sketch for next session)
Stop current run, temporarily `launchctl unload` the agent (so it doesn't relaunch the
sequential driver), precompile the kernel once, then something like:
```bash
# inside container, from project root:
ls data/intermediate/mapbiomas/legacy/grid_*_legacy.rds \
 | sed 's#.*/grid_##; s#_legacy.rds##' \
 | xargs -P 4 -I{} sh -c 'MB_ONLY_BASE={} Rscript code/01_build/04_mapbiomas/3_compute_transitions.R'
# then: Rscript code/01_build/04_mapbiomas/4_deforestation_stats.R
```
Wrap the whole thing the same way (a new parallel driver invoked by auto_resume.sh so reboot
safety is preserved), and re-arm the agent. Watch `docker stats` for memory.

## Monitoring cheat-sheet
```bash
cd "/Users/kristopherjenner/Local projects/Scheinkman/Amnesty/amnesty-project"
tail -f run_full.log
ls data/intermediate/mapbiomas/transitions_combined/*_cover.rds | wc -l   # step-3 tiles done /713
df -h .                                                                    # disk
docker stats --no-stream                                                   # per-worker memory
pgrep -lf "docker run"                                                     # alive?
```

## Environment facts / gotchas (still true)
- All R runs in Docker: `make docker-run CMD="..."`. Image `amazon-amnesty:dev` built.
- Host: Apple M4 Pro, **26 GB** RAM, macOS. FileVault ON, auto-login OFF.
- **Auto macOS updates were the cause of a mid-run reboot** — user disabled them (Software
  Update -> Automatic Updates OFF). Keep them off until the run finishes. Pending updates
  (Tahoe 26, Sequoia 15.7.7) must NOT be installed until done.
- Log timestamps are container UTC (EDT = UTC - 4).
- Reproducibility contract intact: only structural + the lossless compression change.

## Other pre-existing context (from prior checkpoint-20260706.md)
- This is "Trilha A" (raw-only MapBiomas backbone), independent of the CAR magic-file blocker.
- Deferred in step 4: zone breakdowns (glebas / control areas / CAR). `control_areas2` is a
  dangling legacy object; CAR zones blocked by missing `CleanCARShapes_robust/*.shp` (need from
  Pedro/Thiago). See PROBLEMS.md.
- A learning/worksheet PDF was produced this session at
  `output/learning/mapbiomas_learning_packet.pdf` (for the user to study the pipeline).
- PR for the branch still to be opened in the browser (gh CLI not installed):
  https://github.com/Orimadros/amnesty-project/compare/main...mapbiomas-migration?expand=1
