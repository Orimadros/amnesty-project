# Checkpoint 20260807 — do-file filters adopted; SUTVA + Policy-Jump + Fig 3 ported

Handoff for a fresh session. Point the new session here:
"read docs/notes/checkpoints/checkpoint-20260807.md and continue."

Supersedes `checkpoint-20260806.md`. Branch **`integration/car-mapbiomas`** in the
MAIN checkout (worktrees have no `data/`). Nothing pushed; do NOT suggest opening
a PR or emailing Pedro.

## Standing rules (unchanged)

1. **Code over paper**; two code paths -> the one that reproduces the published
   numbers. 2. All R inside Docker (`make docker-run CMD=...`). 3. Never
   re-litigate the closed register in `docs/notes/paper_legacy_method_diffs.md`.

## Done today (2026-08-07) — checkpoint-20260806 steps 1-4

1. **Stage 3 adopts the do-file's three filters by default** (:25 eligible
   min-pre-rate>=10 global; :26 control 2009-defo-area<5ha, ineligible regs only;
   :65 max-pre-rate<85 on BOTH groups of the ineligible regs). D-A (zero-2014)
   and D-B (winsorize) defaults flipped OFF (superseded did1_new/did2_new path).
   `EMP_PANEL=recovered` runs the identical code path on stage 19's rebuild of
   Pedro's April-2025 panels. Stage 3 now also writes `did_panel[_recovered].rds`.
   Results (rate_legacyforest): ours **-1.544 (0.684)** / **+4.413 (1.220)** vs
   paper -1.412 (0.558) / +4.204 (0.886); their own Apr-2025 panel gives -0.344 /
   +3.377 -> we are now CLOSER to print than their surviving data. Baselines:
   eligible 56.21 (print 58.4, was 53.8 pre-filters); ineligible 15.42 — and
   **15.27 on their own panel**, so the printed 11.4 is vintage-specific
   (checkpoint-20260806's expectation there is REFUTED). Also: Table 1's
   "deforested area" rows are NOT the do-file's `sum value if variable==1`
   (pooled mean ~66 ha on both panels vs printed 5.1); stage 2 covers those rows.
2. **Stage 21 `21_policy_jump.R`** (port of recovered multas_RegsFE.R) —
   Policy-Jump table vs print: Deforestation/Arson EXACT (-0.0685, se 0.0051,
   N 421,968); model 2 N exact (104,757), coef +0.0134 vs printed +0.016 (same
   sample, small definitional/vintage gap — only open cell); models 1/3 within
   2% N, coefs match to rounding. + year-FE coefficient paths CSV.
3. **Stage 22 `22_event_studies.R`** (Figures 3 AND 4) — new `twfe_k()`
   multi-regressor two-way FE in `_helpers_twfe.R`. Our panel: eligible flat
   pre-trends, post declining to -3.23 by 2014; ineligible +5.70 by 2014. Their
   panel: same shapes attenuated (-2.28 / +4.40), pre-trends less flat than ours.
   PUBLISHED Fig 3 is the two RATE panels only (area panels commented out in the
   tex as well as the do-file), so Fig 3 is fully covered. Fig 4 (ineligible by
   declared-area band, value_max<95 over ALL years, band+cap filter both groups,
   do-file :175/:177) added to stage 22 -> `event_study_fig4.csv`.
4. **Stage 20 `20_fines_sutva.R`** (SUTVA exhibit, tab:3) — rebuilds `reg1_n`
   (NOT in the share) from autos_infracao_df.rds + CNFP SHP_2013 + biome border,
   then runs the do-file's areg specs + enforcement_clouds part. RESULT: tab:3
   reproduces EXACTLY — N 5,655 exact, every coefficient/SE/baseline to printed
   rounding, all 8 event coefficients identical, cols 7-8 exact. Exhibit CLOSED
   (see method-diffs evening section).
5. New helper `_helpers_feols.R`: one-way-FE OLS (areg-convention dof incl.
   nested-FE rule, CR1) + HC1 OLS; self-tested against lm().

Docs updated: `paper_legacy_method_diffs.md` (two dated 2026-08-07 sections),
`missing_for_replication.md` (rows 4, 7, Fig 3).

## NEXT STEPS, in order

1. ~~Stage 20 output~~ DONE same day: tab:3 exact (see above).
2. ~~Fig 3/4 remaining panels~~ DONE same day: published Fig 3/Fig 4 are RATE
   panels only (area panels commented out in tex + do-file); Fig 4 added to
   stage 22 and reproduces the conditional pattern (below-1500 +6.28 by 2014,
   1500-2500 flat).
3. ~~Model-2 policy-jump coefficient~~ CLOSED same day: year-based policy flips
   the sign (-0.0129), so the print used our definition; residual is fines-
   extract vintage (N matches exactly).
4. Optional cleanups: retire stages 5-12/14-19 diagnostics into docs; fold
   `table1_sample` reporting into one clean output.
5. Ask-list for Pedro (unchanged + one): ~/Documents/did.dta, takeup.dta +
   DadosTerraLegal.csv, prices_reg.dta + vtn_YYYY.rds, the Table-1-control
   vintage, and ~/Documents/reg1_n.dta (to diff against our rebuild).

## Evening addendum: the version map + a stage-19 correction

Line-by-line audit of all legacy regression code (three parallel passes) →
**docs/notes/regression_version_map.md** (the reference doc). Big items:
did.dta located (2_empirics.R:2304, interactive-only export); THREE DiD
generations mapped; **stage 19 assembly corrected** (no dup-rescue / no 2005
anchor / no area filter) — ineligible on their panel is now 13,134 parcels /
baseline 9.03 / beta +5.396 (morning's 15.27 / +3.377 RETRACTED as assembly
artifacts); Table-1-vs-DiD dedup split found (wide dedups, long doesn't);
Policy-Jump provenance = multas_RegsFE.R only; enforcement_clouds.dta is
hand-assembled (buffer-0.5, 2dp) with an A/B half-scene visibility artifact
straddling 2009; combined_warnings.gpkg is EMPTY. New next steps: tab:2
interacted-FE columns (needs codigo_ibge); fold the dedup split into stage 2/3
docs.

## Key files

- `docs/notes/paper_legacy_method_diffs.md` — findings trail + closed register.
- `code/01_build/06_empirics/` — stages; 18-22 read `data/legacy_dropbox/`.
- Printed exhibits: `docs/papers/manuscript_20260423.tex` (tab:policy_jump
  ~:1363, tab:3 ~:2450, Fig 3 spec in the recovered do-file :129/:133).
