# Checkpoint 20260806 — empirics replication after the Dropbox recovery

Handoff for a fresh session. Point the new session here:
"read docs/notes/checkpoints/checkpoint-20260806.md and continue."

Supersedes `checkpoint-20260801.md`. Branch **`integration/car-mapbiomas`** in the
MAIN checkout (`/Users/kristopherjenner/Local projects/Scheinkman/Amnesty/
amnesty-project` — worktrees have no `data/`). Nothing pushed; do NOT suggest
opening a PR or emailing Pedro.

## Standing rules

1. **Code over paper**: when legacy code and the paper disagree, replicate the
   code; keep the paper's variant behind a flag. Exception found: when the code
   has TWO paths, the tiebreaker is which reproduces the published numbers.
2. All R runs inside Docker: `make docker-run CMD="Rscript <path>"`.
3. Never re-litigate items in the closed register of
   `docs/notes/paper_legacy_method_diffs.md` (bottom sections).

## THE BREAKTHROUGH (2026-08-06): Pedro's Dropboxes recovered

Two shares surveyed and mined (`docs/notes/dropbox_survey_20260806.md`; ~330 MB
now in `data/legacy_dropbox/`, gitignored; scripts committed under
`legacy_repo/fines_robustness/` and `data/legacy_dropbox/miseEnPlace/*.do|R`):

- **`empirics_amazon_final.do`** — the paper's actual regression code
  (`data/legacy_dropbox/miseEnPlace/`). reghdfe, cluster(uf), FE variants
  a(COD_IMO i.y | i.uf##i.y | i.codigo_ibge##i.y); eventdd for Figs 3-4; and
  THREE undocumented sample filters:
  * eligible: keep MIN pre-2009 rate >= 10 (do-file :25)
  * ineligible: keep MAX pre-2009 rate < 85 (:65)
  * control: drop parcels with 2009 deforested area < 5 ha (:26; variable
    misnamed defo_rate_2009 — it is the AREA)
  Baselines = pooled parcel-year means on the filtered samples. Winsorized
  column exists but the final do-file uses RAW value.
- **`fines_robustness_sutva.do`** + `Multas-AvisosMatchingV1-3.R` (Table 3) and
  **`multas_RegsFE.R`** (Policy-Jump table) — with their data:
  `enforcement_clouds.dta`, `autos_infracao_df.rds` (IBAMA fines),
  `combined_warnings.gpkg`, `fines_per_warning_ratios.csv`.
- **Their per-year panels for all three groups 2005-2014**
  (`data/legacy_dropbox/output_full/CAR_{eligible,ineligible,control}_defo_*.rds`)
  + `control_final.shp` + both cleaned group shapefiles (April vintage), plus
  the May-2025 group shapefiles in `data/legacy_dropbox/miseEnPlace/`.

## What stage 19 proved with their own data

- Rebuilding did.dta from their panels + the :25 filter gives **eligible 71,044
  parcels / 775,480 obs vs the paper's 71,171 / 782,175 (0.2%/0.9%) — the
  eligible count question is CLOSED**.
- Their pipeline drifted: eligible file = 81,406 (Apr 2025), 73,809 (May 2025),
  71,171 printed. Our 77,300 is inside their own drift. Parcel-level agreement
  where universes overlap: 90%+; only 250 true classification disagreements;
  5,570 of their eligible parcels are absent from our CAR universe (SICAR
  vintage).
- Their own files do NOT reproduce Table 1's control column (6,542 / rate 30.3 /
  1.24-1.38 Mha vs printed 7,049 / 35.7 / 2.0-2.2). control_final.shp = 6,542
  features. That vintage is not in the share — only remaining unsourced number.
- On their April panel with the do-file filters, our twfe gives -0.344 / +3.377
  vs printed -1.412 / +4.204 → the printed run used a different (earlier)
  vintage. Not our estimator's fault.

## Current state of OUR pipeline (before adopting the do-file filters)

Stage 2 basis: 2019 sample year (F2), `table1_sample` = active+cleaning drops,
no P1 for Table 1; `final_sample` (with P1) for the DiD. Faithful stage 4
(2004-rule, N1/N2/N4/N6 semantics, switches documented in its header).

| ours | paper | error |
|---|---|---|
| eligible 77,300 / 4.988 / 5.172 / rate 53.8 | 71,171 / 5.1 / 5.3 / 58.4 | +9% / -2% / -2% / -8% |
| ineligible 18,225 / 3.678 / 4.164 / rate 12.0 (2005-08 avg) / area 673 | 15,254 / 4.1 / 4.7 / 11.4 / 661 | +19% / -10% / -11% / +5% / +2% |
| never-elig 6,855 / 2.003 / 2.189 / rate 36.3 | 7,049 / 2.0 / 2.2 / 35.7 | -3% / 0% / -1% / +2% |
| DiD (filtered control): -1.426 / +5.485 | -1.412 / +4.204 | |

## NEXT STEPS, in order

1. **Adopt the do-file's three filters in stage 3** (and report Table-1-style
   baselines with them): drop eligible min-pre-rate<10; keep ineligible
   max-pre-rate<85; drop control 2009-defo-area<5ha. Remove the D-A zero-2014
   drop and D-B winsorization defaults (final do-file uses neither — D-A/D-B
   belong to the superseded did1_new/did2_new path). Re-estimate; expect the
   eligible baseline to rise toward 58.4 and ineligible toward 11.4.
2. **Estimate on THEIR rebuilt panel too** (`output_full/rebuilt_did_panel.rds`
   already saved by stage 19) — spec-identical comparison, isolates
   data-vintage from spec.
3. **Port `multas_RegsFE.R` + `fines_robustness_sutva.do`** (data in hand) →
   Table 3 + Policy-Jump. Note: needs haven/foreign for .dta (check renv.lock;
   package dance if absent).
4. **Figure 3 event studies**: implement eventdd-equivalent (their absorb =
   COD_IMO + i.y, cluster uf) on our panel.
5. Optional cleanups: retire stages 5-12/14-19 diagnostics into docs; fold
   `table1_sample` reporting into a single clean output.
6. Ask-list for Pedro is now only: ~/Documents/did.dta, takeup.dta +
   DadosTerraLegal.csv, prices_reg.dta + vtn_YYYY.rds, and whichever vintage
   printed Table 1's control column.

## Key files

- `docs/notes/paper_legacy_method_diffs.md` — full findings trail (F/N/P/S/D/L/O
  series + closed register).
- `docs/notes/dropbox_survey_20260806.md` — Dropbox map + extraction results.
- `docs/notes/missing_for_replication.md` — exhibit-by-exhibit status.
- `code/01_build/06_empirics/` — stages; 18/19 read `data/legacy_dropbox/`.
- Table 1 comparison: run stage 2; DiD: stage 3 (EMP_* switches in headers).
