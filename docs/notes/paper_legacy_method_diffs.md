# Paper vs legacy code vs our pipeline: the 2026-07-30 methodological sweep

Triggered by the never-eligible discrepancies (ours 6,140 / 1,085 ha / 43.5% vs the
paper's 7,049 / 760 / 35.7%). Full re-read of the manuscript
(`docs/papers/manuscript_20260423.tex`) and a line-by-line trace of
`legacy_repo/code/2_empirics.R` for all three groups. Two of the differences found
here are large enough to change the Table 1 comparison qualitatively; one is already
confirmed quantitatively.

## THE HEADLINE FINDINGS

### F1 (CONFIRMED) — Table 1/2/3 rates are pre-2009 AVERAGES, not 2008 values

Table 2's notes: *"The outcome baseline is measured as the average prior to 2009 for
treated properties."* Table 1's area and rate rows carry a "(ha, t<2009)" label, and
Table 1's 58.4/11.4 equal Table 2's baselines exactly. Legacy's `clean_car_comp2`
(2_empirics.R:1707-2156) lays out yearly means 2005-2014 per group — the table
numbers are drawn from the pre-2009 columns, not from 2008.

Our `table1_comparison` benchmarked **mean rate_2008** against these numbers. Wrong
benchmark. Recomputed on our data (`6_table1_method_tests.R`, run 2026-07-30):

| class | 2008-only (old comparison) | mean over 2005-2008 | paper |
|---|---|---|---|
| eligible | 56.6 | **53.9** | 58.4 |
| ineligible | 23.5 | **16.8** | 11.4 |
| never-eligible | 43.5 | **40.9** | 35.7 |

Yearly means: ineligible 10.6 (2005), 14.5, 18.5, 23.5 (2008) — the 2005 value alone
is 10.6, essentially the paper's 11.4. Never-eligible: 37.9 → 43.5. This single
correction removes ~60% of the ineligible gap and ~35% of the never-eligible gap.
It also dissolves most of the "Table 1 does not reconcile with itself" argument in
`code_diff_vs_legacy.md` — the rate is a different-period average from the totals,
so the ratio check there was comparing across periods.

### F2 (TEST RUNNING) — legacy's "ever occupied" filter runs on the 2019 raster, the paper says 2014

The paper (Appendix 1 outline: "ever occupied (up to **2014**)"; Appendix 2 step 1:
"Drop properties with <10% of deforested area in **2014**") states the sample filter
we implemented (`rate_raw_2014 >= 10`). Legacy's code does something else: STEP 1
for both the gleba pool (2_empirics.R:619) and the reserve pool (:1818) measures the
rate on the **2019** `plot_legacy_forest` raster (with a comment "change to 2017"),
strict `> 10`. The output files are *named* `active2014_*`, which is presumably how
the 2014 language got into the draft.

Consequence: legacy's samples include parcels first occupied 2015-2019. Those have
near-zero rates in 2005-2008, diluting every pre-2009 group mean, most strongly in
the groups where late entry is possible (ineligible by construction = not occupied
by 2004; never-eligible has no occupation-date restriction) and hardly at all for
eligible (must be occupied by 2004 anyway). This matches the residual pattern after
F1 exactly: eligible needs to move UP (+4.5pp), ineligible DOWN (−5.4pp),
never-eligible DOWN (−5.2pp) — and adding late entrants moves only the latter two,
downward, while raising both Ns toward the paper's (never-eligible 6,140 → 7,049,
DiD ineligible-panel obs 231,833 vs our count).

Being measured now: stage 1 run for `EMP_YEAR=2019` (tiles exist through 2020), then
re-derive `in_sample = rate_2019 > 10` and recompute. Per the standing rule: measure,
don't trust the direction argument.

### F3 (NOT YET IMPLEMENTED) — the control group gets a DIFFERENT cleaning algorithm

Legacy 2_empirics.R:1852-1858, comment verbatim:

```
#-------- FOR PROPERTIES IN REZ/CONSER THERE'S NO REAL DEFINITION OF PROPERTY LINES --#
#-------- SO THE CLEANING ONLY REMOVES OVERLAPS TO AVOID DOUBLE COUNTING -------------#
```

The reserve pool is NOT cleaned with the Appendix C 5-rule algorithm. Instead, per
municipality: dedupe `COD_IMO`; for every pair overlapping > 10% of i's declared
`NUM_ARE`, **erase the intersection from the LARGER parcel** (smaller stays whole);
no raster logic, no random draws, **no parcel is ever dropped**. The per-year
measurements (`CAR_control_defo_YYYY.rds`) then run on these erased geometries.

Our pipeline applies the same 5-rule conflict resolution (stage 4) to all three
groups — including never-eligible, where legacy never does. The 5-rule algorithm
DROPS parcels (lowering our N: 6,140 vs 7,049) and does not shrink large reserve
parcels the way repeated erase-from-larger does (their measured area falls, and with
it the legacy-forest denominator). This plausibly bears on all three never-eligible
gaps (N, mean area 1,085 vs 760, rate).

Also: the ineligible `area < 100000` filter (2_empirics.R:1704) has a never-eligible
analogue `area < 50000` at :2130 that is **commented out** — correctly not
implemented by us.

## SECONDARY DIFFERENCES (found, smaller or already bounded)

- **S1 — occupation-by-2004 is a LEVEL test in legacy, not first-crossing.** Legacy
  :1209/:1290 filters `rate > 10` measured on the 2004 raster (net of regrowth) over
  the cleaned geometry. Our stage 2 uses the first-crossing test (which follows the
  paper's §2.3 "when occupation first began" language — another paper-vs-code
  contradiction, cf. issue #E1). Tested (`6_table1_method_tests.R` TEST B): moves
  3,892 parcels eligible→ineligible; pooled pre-2009 ineligible mean 16.8→16.2;
  small.
- **S2 — control membership denominator.** Legacy: overlap share = intersect area /
  `st_area(geometry)` (GEOMETRIC). Ours (`0_build_car_layers_from_raw.R`):
  / declared `NUM_AREA` (fallback geometric). Same 1% threshold, same control-first
  precedence, same CNFP federal UC|TI layer. Membership can differ where declared
  and geometric areas diverge. Unquantified.
- **S3 — eligible area cap uses GEOMETRIC area in legacy** (`st_area()/1e4 <= 1500`,
  :1231/:1315) plus 2004-deforested ≤ 1500; ours uses declared `area_ha`. An earlier
  declared-vs-geometric test found ~12 parcels, but it should be rechecked against
  this exact pair of definitions once F2 lands.
- **S4 — strict `>` vs our `>=`** in both the 10% filters. Trivial but free to align.
- **S5 — legacy's combined CAR layer double-processed Pará** (issues log #21) and
  built per-municipality files via a `temas_ambientais_update` join whose duplicates
  are inert in our vintage (see the 2026-07-30 RESULT in `code_diff_vs_legacy.md`).
  Not reproducible on our side; noted as legacy-data noise.
- **S6 — Table 1 "Property Area (ha, t<2009)"** may itself be a pre-2009 average,
  and legacy's `area` variable in the panels is the legacy-forest denominator
  (`deforested/(rate/100)`), not the claim area — while Table 1's note says areas
  come "directly from boundaries submitted by land occupants". Our checks: declared
  claim mean 1,085 / legacy-forest-2008 mean 1,049 for never-eligible vs paper 760 —
  neither closes the gap; F2+F3 composition changes are the open candidates.

## What the paper's LIVE text says about the control group (complete list)

1. §3.1 (:965): control = "land-grabbers in indigenous reservations and conservation
   areas, both federal land" — matches CNFP `governo==FEDERAL & classe UC|TI`.
2. Table 1 note: "Never eligible properties are located in control areas (indigenous
   reservations and conservation areas). Until 2008, all of these rural parcels
   illegally occupied public land" — occupied-squatter framing backs applying the
   in-sample filter to the control (as we do).
3. Table 2 note: control units in BOTH regressions are the never-eligible parcels;
   baselines are pre-2009 averages (F1).
4. §3.3.3 (`sec:9`) "Alternative definitions and tests" remains an empty placeholder
   — the "results carry without the spatial cleaning" claim is still unverified.
5. The "Introduction - DRY RUN" section (:795-841) is inside `\iffalse...\fi` (dead):
   it defines "pure control" as parcels OUTSIDE glebas with occupation up to 2009 and
   ineligible as deforestation starting 2005-2009. Contradicts the live text; treat
   only as a fossil of an earlier design, but note the 2005-2009 window idea echoes
   F2's late-entrant issue.

## Legacy control-group construction, end to end (for the record)

```
fpnd (CNFP) -> control_areas = FEDERAL & (UC|TI), ∩ biome                     (:73-74)
car_amazon (combined, biome-clipped, Pará double-processed, dedup COD_IMOVEL)  (:310,355)
overlap = Σ st_area(∩ control_areas) / st_area(parcel) ; >1% -> ccar_clean_updated (:358-395)
  [precedence: control first; remainder with >1% gleba overlap -> ccar_dirty]  (:417-497)
join temas_ambientais_update (codigo_ibge)                                     (:1814)
STEP 1 per muni: rate on 2019 raster > 10  -> active2014_inReserva_CAR_<muni>  (:1818-1845)
reserve cleaning per muni: pairwise, erase ∩ from larger, none dropped         (:1858-1955)
combine, dedup -> control_final (st_write control_final.shp)                   (:1968-1977)
per-year 2005-2014 extract on cleaned geometry -> CAR_control_defo_YYYY.rds    (:2066-2086)
panel: left_joins from 2005 file, dedup + n==1 filters (inert for us)          (:2092-2126)
NO area filter (the <50000 one is commented out)                               (:2130)
Table 1 column: yearly means/sums 2005-2014 -> pre-2009 figures                (:2132-2156)
```

## Ranked next actions

1. **Finish F2**: when `parcel_defo_2019.csv` lands, recompute the split with
   `in_sample = rate_2019 > 10` (raw, pre-erasure) and pre-2009 average rates; also
   try 2017 if 2019 overshoots (the "change to 2017" comment).
2. **Update the stage-2 comparison** to benchmark pre-2009 averages (F1) instead of
   rate_2008, whatever F2 shows.
3. **Implement F3** (reserve-specific cleaning) if never-eligible N/area still
   disagree after F2 — it is the only difference that touches N and mean area
   directly for the control group.
4. Re-test S1-S4 jointly on top of F1+F2 rather than one at a time; interactions
   with the sample change are likely.

---

# RESULT (2026-07-30, same day): F2 CONFIRMED

`parcel_defo_2019.csv` measured (stage 1, EMP_YEAR=2019) and the split re-sampled
with legacy's actual rule (`7_sample2019_test.R`: rate_2019 > 10, strict, raw
geometry, ineligible lf-filter re-applied):

| class | old comparison (2008-only, 2014 sample) | F1 only | **F1+F2** | paper |
|---|---|---|---|---|
| eligible rate | 56.6 | 53.9 | 53.2 | 58.4 |
| ineligible rate | 23.5 | 16.8 | **14.6** | 11.4 |
| never-eligible rate | 43.5 | 40.9 | **36.7** | **35.7** |
| never-eligible N | 6,140 | — | **6,856** | **7,049** |
| ineligible mean area | 682.6 | — | **659.5** | **661** |

The never-eligible column is now essentially reproduced (rate within 1pp, N within
2.7%, mean area excepted). The ineligible mean area matches to 0.2%. The ineligible
rate retains a ~3pp gap (14.6 vs 11.4; sample-composition candidates: S1 level test,
count surplus — our N 22,892 vs their 15,254). The eligible rate sits ~5pp BELOW the
paper under F1+F2 (53.2 vs 58.4), the mirror image of the count surplus (100,185 vs
71,171 parcels: our extra parcels are plausibly lower-rate).

Still open, ranked:
1. **Never-eligible mean area** 1,163 vs 760 — F3 (reserve-specific cleaning) and/or
   S6 (what "Property Area (t<2009)" averages) remain the candidates.
2. **Ineligible residual rate gap** (14.6 → 11.4) and both count surpluses — likely
   linked; the "change to 2017" comment is NOT the answer (2017 would remove
   late entrants and push the rate back UP).
3. Eligible rate 53.2 vs 58.4 — composition of the surplus parcels.

Decision needed before promoting any of this into stage 2: whether the pipeline's
in-sample rule should follow the paper's text (2014) or the legacy code that
produced the tables (2019). Keep both flags in the data; report both.
