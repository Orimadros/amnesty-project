# Survey of Pedro's two Dropbox shares (2026-08-06)

User-provided links, browsed anonymously (16-item listing cap per folder applies;
full enumeration of large folders needs a Dropbox login or the folder-zip download).

- **Dropbox 1 = `amazon_project`** — the root behind every
  `/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/...` path in
  `2_empirics.R`. Link key `yne3cokw83dmcgjgrkh5h`, rlkey `0vfttwvcmmnqw8l71r99g05n4`.
- **Dropbox 2 = `amazonLandPrices_project`** — the second root (raw CAR, CNFP,
  price work). Link key `tspefm9npkgrm6wwjfp4d`, rlkey `1huqo05d1hqy30plcj2jkv5ow`.

## HEADLINE FINDS (all in Dropbox 1)

### 1. `fines_robustness/` — the ENTIRE missing Table 3 block

- **`fines_robustness_sutva.do`** (1.5 KB) — the Stata specification for Table 3.
  Fetched in full; now at `legacy_repo/fines_robustness/`. Key content:
  `areg prior_fine after [propensity_move] if gleba_first_year > 2005,
  a(MUNICIPIO|UF) cluster(MUNICIPIO)`, event-year dummies y2006-y2014, `cloud` and
  `cloud_adj` covariate variants, and the enforcement-intensity regressions on
  `enforcement_clouds.dta`. Inputs: `reg1_n.dta`, `enforcement_clouds.dta`.
- **`Multas-AvisosMatchingV1/V2/V3.R`** (36-45 KB each, June 2025) — the
  fines-to-DETER-warnings spatio-temporal matching. V3 writes
  `fines_per_warning_ratios.csv` and the cloud visible-fraction series. Fetched;
  now at `legacy_repo/fines_robustness/`.
- Data sitting beside them: `enforcement_clouds.dta`, `fines_per_warning_ratios.csv`,
  `combined_warnings.*`, `deter_modis_agregadosanuais_2004_2017/`,
  `Dados DETER - Shell/`, `Clear Spots - Shapefile/`, `brazilian_legal_amazon/`.

### 2. `data/output/` — the ACTUAL RUN'S OUTPUTS (April-July 2025)

First 16 items visible: `already_treated.*` (July 2025),
**`CAR_control_defo_2005..2014.rds`** (~540 KB each — the exact never-eligible
panel files legacy's Table 1 block reads), `car_eligible_cleaned.dbf/prj` (20.6 MB,
Apr 2025). Alphabetically later (unlisted, behind the 16-item cap) should be
`car_ineligible_cleaned.*`, `CAR_eligible/ineligible[_uncleaned]_defo_*.rds`,
`control_final.shp`, `did1_new.dta`, `did2_new.dta`, `prices_reg.dta`, `takeup.dta`.

### 3. `miseEnPlace/` — final shapefiles + the fines microdata

- `car_eligible_cleaned.shp/.dbf/.shx` (May 20 2025): **.shx size 576.73 KB
  implies ~72,079 features — the paper's 71,171 eligible.** In legacy code
  car2004 (the ELIGIBLE group) is what gets written to `car_eligible_cleaned.shp`.
- `car_ineligible_cleaned.*` (May 21 2025): .shx 159.64 KB implies ~19,942
  features — the ineligible group pre-panel-filters.
- **`autos_infracao_df.rds`** (56 MB) + `auto_infracao.html` (590 MB) — the IBAMA
  fines microdata.

### 4. `data/` root — per-year measured panels from 1989

`CAR_eligible_defo_1989..2001.rds` visible (5.7-6.1 MB each; presumably through
2014 behind the cap) — pre-2005 years our replication never had.

### 5. `data/intermediate/` — per-municipality intermediates (May 2025)

`active2004_inGleba_cleaned_CAR_<muni>.*` etc. — the actual per-muni cleaned files,
i.e. the post-conflict-resolution geometry as it really came out, including the
unseeded random draws. This is the ground truth our seeded reimplementation can only
match in distribution.

## Verified non-finds

- Dropbox `code/2_empirics.R` (232.7 KiB, ~11 months old) is **byte-identical** to
  `legacy_repo/code/2_empirics.R` — no newer version with table code exists.
- `code/working/treatmentGroups_generate.R` (64.7 KB, Feb 2024) matches the repo's.
- Dropbox 2 `code/` = archive/create_muni_year_intersections/final (0_mapbiomas.R)/
  infractions (aggregate_infractions.R)/output/prep — nothing empirics-new visible.
- No further `.do` files surfaced in any listed folder besides
  `fines_robustness_sutva.do` (the takeup/prices/DiD do-files were presumably run
  interactively or live in unlisted portions — see download plan).

## Unexplored (16-item cap or not yet visited)

`patricio/`, `docs/data_creation/`, `docs/programa/`, `docs/RegistroRural/`,
`data/input/`, tails of `data/`, `data/output/`, `data/intermediate/`,
`miseEnPlace/` (post-"C"), Dropbox 2's `data/`, `output/`, `subprojects/`,
`archive/`, `prep/`, `subprojects/BH replication.zip` (96.9 MB),
`subprojects/permutations/`.

## Recommended downloads (require confirmation; sizes as listed)

1. `data/output/` folder zip — the run's actual group memberships and panels;
   would settle every count question parcel-by-parcel. Contains 20 MB+ shapefiles;
   zip likely 200-500 MB.
2. `miseEnPlace/car_eligible_cleaned.*` + `car_ineligible_cleaned.*` (~58 MB) —
   the May 2025 final group shapefiles.
3. `fines_robustness/` data companions (`enforcement_clouds.dta`,
   `fines_per_warning_ratios.csv`, `combined_warnings.gpkg`, DETER folders) —
   small-to-medium; unblocks running the SUTVA do-file end to end.
4. `miseEnPlace/autos_infracao_df.rds` (56 MB) — the fines microdata.
5. `data/` per-year `CAR_*_defo_YYYY.rds` (~6 MB × N) — exact panel comparison.
