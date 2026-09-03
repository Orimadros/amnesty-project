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
   `fines_per_warning_ratios.csv`, `combined_warnings.gpkg` (NOTE 2026-08-07: file is an EMPTY GeoPackage skeleton — no feature tables), DETER folders) —
   small-to-medium; unblocks running the SUTVA do-file end to end.
4. `miseEnPlace/autos_infracao_df.rds` (56 MB) — the fines microdata.
5. `data/` per-year `CAR_*_defo_YYYY.rds` (~6 MB × N) — exact panel comparison.

---

# EXTRACTION RESULTS (2026-08-06, same day)

## Recovered and stored locally (`data/legacy_dropbox/`, ~330 MB)

- **`empirics_amazon_final.do`** (miseEnPlace) — THE paper's regression code:
  reghdfe with a(COD_IMO i.y | i.uf##i.y | i.codigo_ibge##i.y), cluster(uf); the
  eventdd calls behind Figures 3-4; the invaded-area regression; and three
  UNDOCUMENTED sample filters: eligible keeps MIN pre-2009 rate >= 10 (:25),
  ineligible keeps MAX pre-2009 rate < 85 (:65), control drops 2009 deforested
  area < 5 ha (:26). Baselines are pooled parcel-year `sum value` on the
  regression samples. The winsorized column (value_w) exists but the final
  do-file uses RAW value — D-B does not apply to the published run.
- **`multas_RegsFE.R`** (+ multas.R/updated/test) — the Policy-Jump table:
  feols(enters ~ policy | COD_MUNICIPIO) with the exact column variants.
- `fines_robustness_sutva.do` + Multas-AvisosMatchingV1-3.R (already committed),
  now with their data: enforcement_clouds.dta, fines_per_warning_ratios.csv,
  combined_warnings.gpkg, autos_infracao_df.rds (56 MB).
- Full `data/output` (167 MB zip): per-year panels for ALL THREE groups
  2005-2014, control_final.shp, both cleaned shapefiles, already_treated.
  NOT present: did*.dta / takeup.dta / prices_reg.dta (written to ~/Documents).
- miseEnPlace May-2025 group shapefiles + muni_target/control_areas.gpkg.

## What the recovered data proves

1. **The eligible count is SOLVED.** Rebuilding `did.dta` from their own panels
   (stage 19) and applying the do-file's :25 filter: **71,044 parcels / 775,480
   obs vs the paper's 71,171 / 782,175 (0.2% / 0.9%)**. The missing ingredient
   was an undocumented min-rate filter, not our pipeline.
2. **The pipeline was a moving target.** Their own eligible file is 81,406
   records in April 2025 and 73,809 in May 2025, vs 71,171 printed; successive
   runs overlap only 82% with each other. Our 77,300 sits inside their own
   drift.
3. **Parcel-level agreement is high where universes overlap**: 90.1% of their
   May eligible are our eligible; of the 7,326 remainder, 5,570 are not in our
   CAR universe at all (SICAR vintage) and only 250 are genuinely classified
   differently.
4. **Even their own files do not reproduce Table 1's control column** under the
   script's assembly (N 6,542, rate 30.3, totals 1.24/1.38 vs 7,049 / 35.7 /
   2.0 / 2.2) — control_final.shp itself has 6,542 features. Table 1's control
   numbers come from a vintage not present in the share.
5. On their panel with the do-file filters, our estimator gives eligible -0.344
   and ineligible +3.377 vs the paper's -1.412 / +4.204 — the April-2025 panel
   is not the printed run's vintage, consistent with (2).

## Immediately actionable next steps

- Adopt the :25/:26/:65 filters in stage 3 and re-estimate on OUR panel.
- Port multas_RegsFE.R + the SUTVA do-file (data now in hand) for Table 3 and
  the Policy-Jump table.
- Ask Pedro only for what remains truly absent: ~/Documents/did.dta,
  takeup.dta + DadosTerraLegal.csv, prices_reg.dta + vtn_YYYY.rds, and the
  vintage that printed Table 1's control column.
