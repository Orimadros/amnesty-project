# Checkpoint 20260706 — MapBiomas backbone migration

Handoff note so a new chat session can continue this work. Point the new session
here: "read docs/notes/checkpoints/checkpoint-20260706.md and continue."

## Where we are

Branch: `mapbiomas-migration` (pushed to origin, 8 commits ahead of main).
PR: being opened manually via
https://github.com/Orimadros/amnesty-project/compare/main...mapbiomas-migration?expand=1
(gh CLI is NOT installed on this machine; no API token — PRs must be opened in the browser.)

Migrated the raw-only MapBiomas backbone out of the 1,528-line monolith
`legacy_repo/code/1_mapbiomas.R` into 5 modular, tested scripts under
`code/01_build/04_mapbiomas/`, wired into `analysis.mk`. This is the unblocked
"Trilha A" work (independent of the CAR magic-file blocker).

## Done (committed + tested)

- `0_tile_rasters_to_grids.R` — crop rasters to biome + tile into grids
- `1_build_legacy_forest.R` — 1985-86 legacy-forest baseline
- `2_classify_cover.R` — per-year forest/human classification
- `3_compute_transitions.R` — stack years + C++ kernel -> transition rasters
- `4_deforestation_stats.R` — biome-level area-by-year table
- `aux/deforestation_rules.cpp` — transition kernel (verbatim from legacy)
- `test_transitions.R` — kernel unit test (3/3)
- Wired into `analysis.mk` as `make -f analysis.mk mapbiomas` (steps 0->4, stamps). `all` stays VTN-only.

All steps validated in-container on a tiny cropped fixture (test/make_fixture.R),
and on a real Rondonia demo (1987-1996): deforestation grew ~13x; figure at
`output/figures/rondonia_deforestation_1987_vs_1996.png`.

## Key facts / gotchas

- **Runtime rule**: all R runs in Docker via `make docker-run CMD="..."`. Image
  `amazon-amnesty:dev` is built. Never run spatial R on host.
- **Memory (important)**: step 3 is memory-bound. Stacking ~20 years x ~14M-pixel
  tiles OOM-kills the 7.75 GiB Docker host. Fixed two ways: gc() between tiles, and
  `MB_ONLY_BASE` single-tile-per-process mode. Full-biome / all-38-year runs need
  the per-tile-process approach (see test/run_demo_rondonia_step3.sh) or more RAM.
- **Testability**: every script reads `MB_*` env vars (dirs, `MB_YEARS`,
  `MB_H_RANGE`, `MB_V_RANGE`) to scope runs; defaults = full production.
- **Data**: real inputs organized under `data/input/` (gitignored). MapBiomas
  tifs in `data/input/mapbiomas/`, aux layers in `data/input/aux/`.
- **Demo helpers kept LOCAL (uncommitted, by user request)**:
  `test/run_demo_rondonia.sh`, `test/run_demo_rondonia_step3.sh`,
  `test/render_demo_map.R`, and the figure.

## Open / next steps

1. (User doing now) Open the PR in the browser.
2. Presentation to Leo tomorrow 10am (2026-07-07) — map + stats + migration story ready.
3. Deferred in step 4: zone breakdowns (glebas / control areas / CAR). `control_areas2`
   is a dangling legacy object; CAR zones blocked by the magic-file.
4. Full-scale production run (all 38 years, full biome) — needs the memory strategy;
   not yet run.
5. Still-blocking magic file remains `CleanCARShapes_robust/*.shp` (no in-repo producer)
   — needed from Pedro/Thiago to unblock the CAR branch. See PROBLEMS.md.
