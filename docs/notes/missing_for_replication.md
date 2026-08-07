# What the paper reports that our pipeline does NOT produce

Compiled 2026-08-01 from `docs/papers/manuscript_20260423.tex` (every `\caption`),
cross-checked against `legacy_repo` (all 64 R files) and `data/input/`.

Status key: **DONE** = we reproduce it · **PARTIAL** = some columns/panels only ·
**CODE MISSING** = legacy has no script for it · **DATA MISSING** = inputs absent.

## Main tables

| # | Exhibit | Status | What exists in legacy | What is missing |
|---|---|---|---|---|
| 1 | **Characteristics of Occupations** (Table 1) | **DONE** | `2_empirics.R` builds the groups; the table itself was typed by hand | — |
| 2 | **2009 Amnesty Take-Up and Eligibility** | **CODE + DATA MISSING** | `3_policy1.R` links Terra Legal applications to CAR and exports `takeup.dta` | The regressions (ran in Stata). Input `DadosTerraLegal.csv` is not in `data/input/` |
| 3 | **2009 Amnesty Eligibility and Expectation Effects** (Table 2) | **PARTIAL** | DiD panels exported as `did1_new.dta` / `did2_new.dta` | We estimate cols (1) and (4) — property+year FE. Cols (2)(3)(5)(6) use **year×state** and **year×municipality** FE, not implemented. Their Stata `.do` file is absent |
| 4 | **Assessing the Response from Land-Grabbers in Control Areas** (Table 3, SUTVA) | **DONE 2026-08-07 — EXACT** | Stage 20 (`20_fines_sutva.R`) rebuilds `reg1_n` from scratch and reproduces EVERY printed cell of tab:3 to rounding (N 5,655 exact; all 8 event coefficients identical; baselines 0.16%/0.32%; cols 7-8 exact) | — |
| 5 | **Moral Hazard Estimates Using Land Prices** (Table 6) | **CODE + DATA MISSING** | `2_empirics.R:2576` exports `prices_reg.dta`; the VTN/VNP/IHS cleaning chain exists | The regressions (Stata). Our VTN steps 7-8 are blocked on the `vtn_YYYY.rds` price tables |
| 6 | **Treatment Effects of Forgiveness Expectation** | **CODE MISSING** | — | Whole exhibit |
| 7 | **Policy-Jump Estimates (t≥2009)** | **DONE 2026-08-07** | Stage 21 (`21_policy_jump.R`) ports the recovered `multas_RegsFE.R` | Deforestation/Arson column EXACT (coef/se/N); model 2 N exact, coef 0.0134 vs printed 0.016; models 1/3 within 2% — see method-diffs 2026-08-07 |

## Main figures

| Figure | Status | Missing |
|---|---|---|
| Fig 1 Deforestation Evolution | CODE MISSING | Aggregation off the MapBiomas transitions (we have the rasters) |
| Fig 2 Eligibility Assignment (map) | mostly reproducible | Map code exists in `2_empirics.R` (tmap/ggplot); not ported |
| **Fig 3 Eligibility and Expectation Effects (event study, 4 panels)** | **RATE PANELS DONE 2026-08-07** | Stage 22 (`22_event_studies.R`) estimates the eventdd-equivalent rate paths (ref 2008) on our panel and the recovered April-2025 panel; `event_study_coefs[_recovered].csv`. Deforested-AREA panels (log value, variable==1) not yet ported |
| Fig 4 Conditional Effects of Forgiveness Expectation | **DONE 2026-08-07** | Stage 22 (`event_study_fig4.csv`): below-1500 band ramps to +6.28 by 2014, 1500-2500 band flat/negative — the printed conditional pattern (published panels are the two rate panels; area panels commented out in the tex) |
| Fig 10 Anticipating Another Amnesty (invaded property areas) | CODE MISSING | Distributional analysis of claim sizes over time |
| Fig applications — Amnesty Take-Up | DATA MISSING | Terra Legal applications |
| Fig fine — Spatial Distribution of Fines | DATA MISSING | Geolocated IBAMA fines |

## Appendix

Appendix B's CAR-intersection tables (`tab:number_of_cars`, probability of being born
into conflict, joint distribution of reference/target intersections) and the
cancelled-CAR and municipal-panel maps are mostly **derivable** from our CAR stage
03b/04 outputs, but no code builds them. Section **3.3.3 is an empty placeholder** in
the draft, so the claim that results carry without spatial cleaning is unverified
there (though the DiD vintage never applied cleaning at all — see
`paper_legacy_method_diffs.md`).

## Inputs we do not have

1. `DadosTerraLegal.csv` — Terra Legal applications (take-up table + figure)
2. Geolocated **IBAMA environmental fines** with entity tax IDs (Table 3, Fig fine, policy-jump)
3. **DETER** deforestation warnings (Table 3)
4. **Cloud-coverage** series used for the adjusted enforcement intensity (Table 3)
5. `vtn_YYYY.rds` land-price tables (Table 6; blocks VTN steps 7-8)
6. `temas_ambientais_update.csv` — legacy's *updated* SICAR microdata (we hold only `temas_ambientais.csv`; the `_update` vintage drives their duplicate-drop filters)
7. **CNFP 2020** shapefiles and `i3geomap_glebas_federais.shp` — the layers behind the DiD vintage (we hold CNFP SHP_2013 only)
8. The **Stata `.do` files** for every regression — no `.do` or `.dta` exists anywhere in the repo
9. The `amazon_working/*.rds` intermediates the DiD blocks read

## The short ask-list

Highest value first, i.e. what unblocks the most:

1. **The Stata do-files.** Every regression in the paper ran in Stata on `.dta`
   exports; none of that is committed. Without them the specifications (FE
   structure, clustering, weights, winsorising) are guesses.
2. **The fines / DETER / cloud data.** Blocks Table 3, the policy-jump table and a
   figure — the largest single unreplicated block.
3. **Terra Legal applications.** Blocks the take-up table and figure.
4. **`vtn_YYYY.rds`.** Blocks Table 6 and our own VTN steps 7-8.
5. **`temas_ambientais_update.csv` and the CNFP 2020 / i3geomap layers.** Would let
   us reconstruct the DiD vintage's pool exactly and probably close the residual
   count gaps documented in `paper_legacy_method_diffs.md`.
6. **Event-study code** for Figure 3 (or confirmation it was Stata `eventdd`/manual).
