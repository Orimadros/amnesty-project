# The version map: every generation of Pedro's regression code vs the paper

Compiled 2026-08-07 from a line-by-line audit of all regression-bearing legacy
code (2_empirics.R 3,571 lines; treatmentGroups_generate.R 1,122; the four
multas*.R; Multas-AvisosMatchingV1-3; both recovered do-files). Written because
multiple co-existing versions produce different numbers and it was unclear which
produced the paper. Line references are to the legacy files as on disk.

## 0. Executive summary

- The DiD has **three generations**; only Gen 3 (`did.dta` + the recovered
  `empirics_amazon_final.do`) is the paper's. Gens 1-2 (`did1/did2[_new].dta`)
  are superseded — their distinctive features (zero-2014 drop D-A, winsorization
  D-B) do NOT appear in the published path.
- The fines work has **two distinct methodologies**, not versions of one
  pipeline: municipality-level matching (multas.R → test_Guilherme → RegsFE,
  which printed the Policy-Jump table) and point-in-polygon on CNFP 2013
  (multas_updated.R + fines_robustness_sutva.do, which printed the SUTVA table).
- The DETER matching V1→V2 changed **nothing numerically** (and neither exports
  anything); V3 is the only operative version. `enforcement_clouds.dta` was
  **hand-assembled** outside any script.
- Every Stata-facing export in 2_empirics.R sits below lines that ERROR on a
  clean run (`did.dta` at :2304 is unreachable by `source()`), so all printed
  runs came from interactive sessions — a structural reason vintages drift.

## 1. Exhibit concordance (names shift across drafts)

| recovered do-file header | current tex label | printed anchors |
|---|---|---|
| "TABLE 1" (:47-61) | **tab:2** "Eligibility and Expectation Effects" cols 1-3 (older drafts: "Table 3") | -1.412 (0.558) / -0.844 / -0.829; baseline 58.4; obs 782,175 |
| "TABLE 2" (:65-78) | tab:2 cols 4-6 | +4.204 (0.886) / 4.047 / 3.744; baseline 11.4; obs 231,833 |
| "FIGURE 3" (:129/:133) | fig:3 (two RATE panels; area panels commented out in tex too) | — |
| "FIGURE 4" (:175-181) | fig:4 (two RATE panels by size band) | — |
| multas_RegsFE.R :396-441 | tab:policy_jump | -0.051 / +0.016 / -0.0081 / -0.0685 |
| fines_robustness_sutva.do | tab:3 "Response from Land-Grabbers" | γ≈0.002, N 5,655; cols 7-8: 0.033 / 0.031 |
| 2_empirics.R :2163ff "less_1500", :2576 prices_reg | tab:6 land prices | blocked on data |

## 2. DiD lineage — three generations

| | Gen 1 | Gen 2 | **Gen 3 (paper)** |
|---|---|---|---|
| Code | treatmentGroups_generate.R:307-476 | 2_empirics.R:2726-2888 | 2_empirics.R:1549-2304 → **`did.dta`** (:2304) + empirics_amazon_final.do |
| Exports | did1.dta / did2.dta | did1_new.dta / did2_new.dta | did.dta |
| ID / group encoding | COD_IMOVEL, treatment 0/1 | COD_IMOVEL, treatment 0/1 | **COD_IMO, group strings** ("eligible"/"inelegible"/"never eligible" — note misspelling) |
| Dropbox root | amazon_working + ~/Documents | amazon_working (prefixes CAR_notEligible_, car2004_, CAR_inReservas_) | **amazon_project** (CAR_eligible_, CAR_ineligible_, CAR_control_) |
| Gleba-overlap rule | ANY overlap > 0 (:174) | same upstream | **> 1%** (:497), UC/TI >1% removed first (:395) |
| Eligibility | deforested ≤1500 only | same | ≤1500 deforested AND polygon ≤1500 ha |
| Occupation raster | 2004 | 2004 | **2019** for "active2014" then 2004 (two-stage) |
| Conflict cleaning | none | none | full algorithm :746-1155 (random tie-breaks, NO set.seed → irreproducible) |
| Zero-2014 drop (D-A) | control+spillover | control+spillover | **none** |
| Winsorize (D-B) | no | **yes** (value_w) | **no** (:2163 commented out) |
| Per-year dedup | none | none | add_count()/n==1, long panels |
| Extra covariates | none | uf, ibge, areas | + cancelled, when_occupied, defo_rate_2009 (misnamed: it's an AREA), min/max pre rates, dummy_reach_1500, … |

Fingerprint: `cancelled` + `when_occupied` exist ONLY in did.dta — proof the
recovered do-file (which uses both) ran on Gen 3.

**did.dta construction details that matter for reproduction** (audit findings):

1. The LONG (DiD) panels apply per-year `add_count()/filter(n==1)` with NO prior
   exact-duplicate dedup, NO 2005 anchoring of the parcel set, and NO area<1e5
   filter (2_empirics.R:1704's filter belongs to the WIDE descriptive table
   only). Stage 19's original rebuild got all three wrong; **corrected
   2026-08-07** — consequences in §5.
2. The WIDE (Table 1) panels dedup differently (unique-rows first, then n==1),
   so **Table 1 and the DiD run on structurally different samples from the same
   rds files** — beyond the vintage issue.
3. `did.dta` is written at :2304, but :2282-2297 reference undefined objects and
   error → the export is only reachable interactively. Same pattern for
   prices_reg.dta (undefined inputs at :2572-2574).
4. `when_occupied` uses rate >= 10; the sample-selection occupation test uses
   strict > 10 — inconsistent thresholds inside one generation.
5. An "uncleaned" DiD universe exists fully built but its rbind is commented out
   (:2159); it is NOT drop-in (its eligible long panel lacks the `area` column →
   variable codes would shift in Stata).
6. `enters_after_policy` has a character-comparison bug ("Inf" > "2008" is
   lexically TRUE → never-deforested parcels coded as entrants); unused by the
   published regressions but present in did.dta.

## 3. Fines lineage — two methodologies

Chronology (established by fossil evidence — commented paths, missing objects):

```
multas.R (Mac, origin)          parses auto_infracao.html → autos_infracao_df.rds;
                                writes muni_control/target gpkgs; enters loop 2004:2014, <= y
   ↓ port to Windows
multas_test_Guilherme.R         fills in TI/UC shapefiles; glebas1 still undefined (dead end);
                                year >= 2004
   ↓ + glebas + fixest
multas_RegsFE.R  ★POLICY-JUMP   year >= 2002; loop 2005:2014 with <= y-1 (definition change!);
                                4 outcomes, 4 event-study + 4 policy LPMs (feols, muni FE)
   ↓ methodological rewrite
multas_updated.R ★reg1_n.dta    CNFP 2013 point-in-polygon (st_within), target erased of
                                control overlap, NO year filter; prior_fine outcome;
                                → fines_robustness_sutva.do (areg)  ★SUTVA table
```

Key facts:

- **Policy-Jump (printed) = multas_RegsFE.R only.** Earlier versions would give
  different numbers: contemporaneous control-fine rule (`<= y` vs `<= y-1`),
  different base-year filters (none / 2004 / 2002), no [2005,2025] window.
- **RegsFE and updated are different estimands**: municipality-exposure `enters`
  vs point-in-polygon `prior_fine`; municipalities can be in BOTH target and
  control sets in RegsFE, while updated erases overlap. Not comparable columns.
- Load-bearing quirks (all replicated by our stages 20-21, which is why they
  reproduce print): `min_year` ≡ `year` for the f_not/f_yes rows (grouped by
  (CPF, year), min over a constant) — the multas_final panel mixes two meanings
  of min_year in one column; model 3's `multas_control_all` never gets the
  [2005,2025] window (2002-04 rows retained); model 3's policy keyed on
  target_min_year, not control_min_year; reg1_n keeps CPF only because grouped
  select() re-adds the grouping column (otherwise the cloud loop yields all-NA).
- The audit flags an rbind column mismatch (f_not/f_yes would carry a re-added
  `year` under grouped-select semantics) that would ERROR in modern dplyr; the
  printed table exists and our ungrouped port matches it to ~exact, so the
  executed environment behaved like the ungrouped reading.
- x_aggregate_infractions.R is an unrelated lineage (SICAFI municipal counts,
  Alckmin 2024, all code inside if(FALSE)) — only link: it documents the IBAMA
  URL whose HTML dump multas.R parses.

## 4. DETER matching lineage (V1→V2→V3) and enforcement_clouds.dta

- V1/V2/V3 lines 1-385 are **byte-identical** (verified): DETER 2005-14 +
  fines, 3857 projection, buffers 0.5/1/2 ha, fine within [warning, +6 months],
  st_contains for fines, st_intersects for area tags, counts by warning-year.
- V1 (month-availability cloud adjustment) is **degenerate** — its multiplier is
  files-present/12, identical for control and target. V2 = geometry hygiene
  only, numerically nil, likely never ran (`st_geometry_names` isn't an sf
  function). **Neither V1 nor V2 exports anything.** V3 deletes the cloud
  pipeline, writes fines_per_warning_ratios.csv + visible_fraction.csv from
  precomputed "Clear Spots" shapefiles (produced by no surviving script).
- **enforcement_clouds.dta is hand-assembled**: enforcement_control/target =
  buffer-0.5 fines_per_warning rounded to 2 dp (verified exactly, all 10 years
  × both arms; buffers 1 and 2 ruled out). fraction_control/target match a
  plain mean over per-FILE rows — **A/B half-scenes averaged as separate
  observations**, mechanically halving measured visibility in 2007-2010.
- Consequences for the printed tab:3 cols 7-8: (i) the visibility artifact
  straddles the 2009 cutoff (depresses 2007-2010), so the `after` coefficient on
  adjusted enforcement partly reflects DETER scene-delivery format, not
  enforcement; (ii) 2-dp rounding puts up to ±17% relative error on the pre-2009
  levels (2005 target rounds to exactly 0). Our stage 20 consumes the .dta as
  given, so it reproduces print exactly — these are caveats about the exhibit,
  not the replication.
- **combined_warnings.gpkg (recovered share) is an EMPTY GeoPackage skeleton**
  (65,536 bytes, zero feature tables) — correct the survey's implication that
  the DETER data is in hand. Re-running V3 needs the DETER shapefiles + Clear
  Spots folder, neither on disk. 1,335 target warnings have unparseable dates in
  the shipped ratios (an `NA` year row survives, hand-edited, in the csv).

## 5. Which data vintage printed which number (and our rebuild status)

April-2025 per-year panels (the only surviving vintage) under the do-file's own
spec, EXACT long-panel assembly (stage 19 as corrected 2026-08-07):

| | their Apr-2025 panel | printed | our pipeline |
|---|---|---|---|
| eligible beta | -0.344 (0.449) | -1.412 (0.558) | **-1.544 (0.684)** |
| ineligible beta | +5.396 (1.774) | +4.204 (0.886) | +4.413 (1.220) |
| eligible baseline / N | 55.12 / 775,480 | 58.4 / 782,175 | 56.21 / 795,175 |
| ineligible baseline / N | 9.03 / 169,987 | 11.4 / 231,833 | 15.42 / 163,850 |

- CORRECTION to the 2026-08-07 morning note: with the faithful assembly their
  panel gives ineligible baseline **9.03** and beta **+5.40** (the earlier
  15.27 / +3.38 were artifacts of stage 19's then-incorrect assembly: exact-dup
  rescue + 2005 anchor + area<1e5). The printed 11.4 is BRACKETED by assembly
  variants on their own data; still not reproduced by any — vintage remains the
  explanation, but the direction claim ("their panel gives ~15") is retracted.
- Eligible side is insensitive to the assembly fix (71,044 parcels either way).
- The printed ineligible obs (231,833 → ~23,183 parcels) EXCEEDS Table 1's
  15,254 + 7,049 — consistent with the do-file running on a larger unfiltered
  did.dta ineligible set than Table 1's (the wide/long dedup split in §2 plus
  vintage), i.e. tab:2 cols 4-6 and Table 1 do not describe the same sample
  (D-C, now with a mechanism).
- Fixed-assembly recovered event studies: eligible 2014 -2.28 (unchanged);
  ineligible 2014 +8.45 (was +4.40 under the old assembly) vs ours +5.70.

## 6. Version → printed exhibit master table

| printed exhibit | producing version | our replication status |
|---|---|---|
| tab:2 cols 1/4 | did.dta (Gen 3) + do-file :47/:65 | betas/signs match (-1.544/+4.413); exact N blocked on did.dta vintage |
| tab:2 cols 2-3/5-6 | same, uf##year / ibge##year absorbs | stage 3 time_fe variants: -0.893/-0.951 vs print -0.844/-0.829; +4.683/+4.250 vs +4.047/+3.744 — halving pattern reproduces |
| fig:3 | do-file :129/:133 | stage 22, shapes match |
| fig:4 | do-file :175/:177 (value_max<95 = rate reading, verified) | stage 22, pattern matches |
| tab:policy_jump | multas_RegsFE.R :396-441 | stage 21: col 4 EXACT; col 2 N exact; cols 1/3 within 2% |
| tab:3 SUTVA | multas_updated.R + sutva.do | stage 20: EVERY cell to printed rounding, N exact |
| tab:1 | 2_empirics.R wide tables (different dedup than DiD!) | stage 2 (rates/areas ~2%); control col = missing vintage |
| tab:6, takeup, figs 1/10 | prices/takeup exports (broken inputs) | blocked on data |

## 7. Open items after this audit

1. did.dta itself (the printed vintage) — still the #1 ask (with reg1_n.dta as
   a nice-to-have diff target; we rebuild it exactly).
2. ~~tab:2's interacted-FE columns~~ DONE same day (stage 3 time_fe variants;
   codigo_ibge = CAR-id second segment).
3. The wide-vs-long dedup split means our Table-1-vs-DiD comparisons should not
   expect the same parcel counts even within one vintage — fold into stage 2/3
   docs.
4. The enforcement fraction defect (§4) — worth a referee-note; not our bug.
