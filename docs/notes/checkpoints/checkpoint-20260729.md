# Checkpoint 20260729 — branches merged, VTN region chain live, Lavoura 1-2 migrated

Handoff so a new chat can continue. Point the new session here:
"read docs/notes/checkpoints/checkpoint-20260729.md and continue."

## TL;DR — where we are RIGHT NOW (2026-07-29)

- **Everything is on `integration/car-mapbiomas`**, checked out in the MAIN checkout.
  It merges the CAR migration and the MapBiomas backbone. Tip: `bb9d5a7`.
- **The whole price chain now runs end to end**, all idempotent:
  `make -f analysis.mk vnp` then `make -f analysis.mk lavoura`.
- **VTN 0 -> 5 -> 6 now run** for the first time. Step 6 had a latent crash; fixed.
- **Nothing is pushed.** `main` is protected (see below).
- **Next piece of work: `2_empirics`.** Its price inputs are now satisfied — Lavoura
  step 3 produces `<cat>_parcel_nb_lavoura_wide.rds`, which `2_empirics.R:2313` reads,
  and the MapBiomas rasters it needs are on disk.

### Added later the same day (2026-07-29)

- **NB/VNP track migrated** (`code/01_build/05_vnp/`, `make -f analysis.mk vnp`):
  legacy `Tomas_NB_processing/1.0` and `2.0`. Legacy `3.0`-`6.0` are diagnostics that
  nothing reads and that Lavoura step 3 subsumes — deliberately not migrated
  (`docs/notes/vnp_migration_issues.md` issue #V7).
- **Lavoura step 3 migrated** (`3_join_nb_lavoura_parcels.R`). The Lavoura chain is now
  complete: 1 -> 2 -> 3.
- **Two findings worth knowing before using these prices:**
  - *NB has an era break* (#V4). The pre-2015 and 2016+ sheets use different land-type
    conventions (106 free-text types vs 11 normalised ones), so a series crossing
    2015/2016 has a composition break. Rio Branco goes from `mata` ~125 R$/ha to
    `pastagem formada` ~6,000 R$/ha.
  - *NB sits systematically below Lavoura* (#L13), -656 R$/ha in 2002 widening to
    -5,489 in 2017. That is composition, not a bug: Lavoura is cropland only, NB
    averages all land types including forest. Do NOT treat them as interchangeable.
- **Coverage payoff** (#L14): Lavoura alone leaves 43,695 of 165,940 eligible parcels
  unpriced in 2017; NB + Lavoura together leave only 2,459 (1.5%) with no price at all.

## Branch layout — read this before touching git

`main` is at `b959f21` and contains **neither** major migration. Both forked from it:

```
b959f21 main
 ├── CAR line ........ ca5f3f9   (the real CAR migration)
 └── mapbiomas-migration 6987a39
        └── merged together as -> integration/car-mapbiomas -> c21b0da  <-- WORK HERE
```

- **`car-migration` is STALE at `6c2db5e` and does NOT contain `ca5f3f9`.** Merging it
  expecting the CAR pipeline gets you docs and no code. Delete or fast-forward it.
- **`main` is protected.** `.github/CODEOWNERS` + the `leo-only` ruleset require Code
  Owner approval from @Orimadros on every PR. Do NOT self-merge or push to main; open
  a PR and let Leo review.
- The merge itself was verified: `analysis.mk` was the ONLY genuinely merged file
  (every script byte-identical to its parent branch), no duplicate make recipes, both
  `01_car` and `04_mapbiomas` chains resolve in correct dependency order.

## Environment facts / gotchas (all still true)

- **Work in the MAIN checkout**, `/Users/kristopherjenner/Local projects/Scheinkman/Amnesty/amnesty-project`.
  `data/` (~130 GB) is gitignored and exists ONLY there, so Docker bind-mounts and all
  runs must happen there. Worktrees have no `data/`.
- **`build/stamps/*.stamp` are COMMITTED TO GIT and dated 2026-05-05.** They come from
  the original snapshot commit, NOT from any local run — four VTN stamps existed while
  `data/clean/` did not exist at all. **Never read stamp presence as proof a step ran.**
  Check for the actual output files. (Recommended cleanup, not yet done: gitignore
  `build/stamps/` and untrack the four.)
- **Exit codes get masked.** `make ... | tail`, and backgrounded commands ending in
  `tail`, return TAIL's status. A VTN-6 run that died was reported by the harness as
  "exit code 0". **Always** redirect to a log and append `echo "EXIT=$?" >> log`, then
  grep for it. This trap is also recorded in the CAR notes; it has now cost time twice.
- MapBiomas rasters **survived**: `data/intermediate/mapbiomas/transitions_combined`
  holds 75 GB / 24,955 `.tif`. `2_empirics` is NOT blocked on regenerating them,
  despite what the deletion guidance in the MapBiomas notes might imply.
- Disk: ~113 GB free of 460 GB.

## What ran this session

| Step | Outcome |
|---|---|
| VTN-0 municipal mesh | 5,570 municipalities -> `data/clean/muni_division_2015.Rdata` |
| VTN-5 IHS regions | 133 regions. Warning "some municipalities did not match" is BENIGN: only 2 unmatched dissolved groups, both RJ/SP, nowhere near the Amazon |
| VTN-6 CAR x region | 29 CAR-bearing regions -> `ihs_breakdown/` + `all_car_regions.Rdata` |
| Lavoura 1 | 29/29 regions matched to the FNP workbook, 21 priced |
| Lavoura 2 | 3 category panels + coverage summary |

Verified: distinct parcel counts reconcile EXACTLY with the CAR scaffold layers
(164,223 eligible / 13,025 ineligible). 2017 prices R$350-19,500/ha, median R$8,000,
no zeros or negatives. `make -f analysis.mk lavoura` re-run is a no-op.

## Bug fixed this session

`code/01_build/02_vtn/6_match_car_IHSregion.R:29` did
`car_layers[!file.exists(car_layers)]` where `car_layers` is a `list()`.
`file.exists()` needs a character vector and errors with `invalid 'file' argument`, so
VTN 6 died before doing any work. It had been migrated months ago but never executed,
so this was undetected. Fixed to `file.exists(unlist(car_layers))`.

Side effect: this settles the "is `all_car_regions` a magic file?" question. It is not —
VTN 6 produces it.

## Inputs vendored 2026-07-29

Placed under `data/input/`:

```
muni_division_2015/BRMUE250GC_SIR.{shp,dbf,shx,prj,cpg}
landvalues/ihs_markit/IHS Markit S&P Jun23.xlsx
landvalues/vnp/Lavoura_FNP.xlsx
landvalues/vnp/vnp_2002_2017.xlsx                  <- NB track
landvalues/vnp/Land Price_North Brazil_FNP.xlsx    <- NB track
```

- `muni_division_2015` is a **public IBGE download**, not project data, and is NOT in
  the Dropbox share: `geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2015/Brasil/BR/br_municipios.zip`
  (101 MB, Malha Municipal Digital 2015, 1:250,000, SIRGAS 2000).
- The Dropbox share that DOES hold the rest is `amazonLandPrices_project`, under
  `data/raw/landvalues/{ihs_markit,vnp,vtn}/`. Note the legacy code referenced a
  DIFFERENT Dropbox root (`amazon_project`), which is why some paths don't line up.

**Still missing from disk:** the VTN *price* tables `data/raw/landvalues/vtn/vtn_YYYY.rds`
(Dropbox has them). These block VTN steps 7-8 only. Note there is deliberately no 2020:
`VTN_YEARS := 2015 2016 2017 2018 2019 2021 2022`, matching both the Dropbox contents
and the manual fix-sheets. Not a gap.

## Lavoura specifics

Full issue log: `docs/notes/lavoura_migration_issues.md` (10 issues). The two that
matter downstream:

- **#L4 region-straddling parcels are DUPLICATED** by the spatial join — one row per
  (parcel, region), each with that region's price. Faithful to legacy, but **aggregate
  on distinct parcel id** or counts inflate. eligible 165,940 rows / 164,223 distinct.
- **#L7 only 21 of 29 CAR-bearing regions appear in the FNP survey at all** — the same
  21 every year, so it is a source coverage limit, not intermittent gaps. This is why
  mean priced-years lands at ~11-13 of 16.

Deviation worth knowing: legacy step 2 wrote 96 per-year-per-category CSVs that
re-serialise the whole parcel table 16x and that **nothing downstream reads**. Replaced
with one wide panel per category (`price_YYYY_lavoura` columns, matching what step 3
builds) plus a coverage summary — ~140 MB instead of several GB.

## Next session: `2_empirics`

DONE this session (see the "Added later" block above): the NB/VNP track and Lavoura
step 3. `2_empirics` is now the next piece, and both classes of input it needs exist:

- price side: `data/intermediate/lavoura/parcels_nb_lavoura/<cat>_parcels_all/<cat>_parcel_nb_lavoura_wide.rds`
  (read at `legacy_repo/code/2_empirics.R:2313`)
- deforestation side: `data/intermediate/mapbiomas/transitions_combined/` (75 GB, present)
- CAR side: the scaffold layers + `car_combined_amazonBiome2.shp`

Approach: **decompose into numbered stages like the CAR chain — do NOT port it as one
script.** It is 3,571 lines mixing pipeline code with exploratory blocks, hardcoded
personal paths, and dead objects. Its already-migrated pieces (the biome combine at
lines 255-307 -> CAR stage 05; the eligible/ineligible/already_treated classification ->
the CAR scaffold) should be skipped, not re-ported. Expect roughly: deforestation
scoring per CAR, the spatial conflict-resolution algorithm, control-area construction,
then the regressions.

Note the legacy 2_empirics reads the wide RDS and immediately derives per-year means via
`rowMeans` over `preco_*_<year>` columns. The compact
`<cat>_parcel_price_panel.csv` written by Lavoura step 3 already is that derivation, so
new stages should prefer it over re-deriving from the 1,700-column wide table.

## Still open / not done

- **Nothing pushed.** Needs a PR into protected `main` with @Orimadros review.
- `build/stamps/` should be gitignored and the 4 tracked stamps untracked.
- VTN steps 7-8 (need the price tables).
- Lavoura step 3 (needs NB).
- `2_empirics`, `3_policy1`, `x_aggregate_infractions` (Pedro track).
- `PROBLEMS.md` sections 2 (non-idempotent VTN correction step) and 3 (manual
  fix-sheets, working as designed) are unchanged and still accurate.
