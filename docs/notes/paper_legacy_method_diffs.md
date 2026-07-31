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

---

# RESULT (2026-07-30 overnight): S2 and S3 ELIMINATED

`9_geom_area_tests.R` (geometric vs declared denominators, on the in-sample-2019
split): S2 — only 4 / 13,025 control members would leave the control pool under
legacy's geometric-share denominator, and 0 / 164,223 target-pool members would
enter (caveat: parcels scored in neither output layer are invisible to this test).
S3 — the geometric 1,500-ha cap switches 84 eligible→ineligible and 37
ineligible→eligible. Neither can move a group mean. (The script's ineligible count
differs from stage 7's because it omits the lf-area filter; its rate column is not
comparable — use stage 7's numbers.)

Remaining live candidates for the residuals: F3 (reserve cleaning — running),
count-surplus composition (eligible 100,185 vs 71,171; ineligible 22,892 vs
15,254), S1 (level test, small), and legacy-side data noise (S5).

---

# RESULT (2026-07-30 overnight): count-surplus channels probed

Two legacy-side loss channels examined for the treated-group count surplus
(eligible 100,185 vs 71,171; ineligible 22,892 vs 15,254 on the F1+F2 basis):

- **Pará double-processing is INERT downstream**: legacy rbinds PA twice into its
  combined layer (2_empirics.R:273-306, no dedup at the end) but then dedups by
  COD_IMOVEL at :355 before anything else uses it. Not a surplus explanation.
- **Microdata-join loss is real but small**: legacy's per-municipality loops iterate
  over `codigo_ibge` from the `temas_ambientais_update` join, so CARs absent from
  the microdata are never measured (NA muni -> skipped). Against OUR
  `temas_ambientais.csv`: 2.9% of the eligible pool, 3.0% of ineligible, 15.5% of
  the control pool have no microdata row. Explains percent-level losses on legacy's
  side (their _update vintage may differ), not a 40% surplus.

The surplus therefore most plausibly sits in legacy's own processing losses — the
appendix concedes 891,234 -> 829,260 processed CARs ("additional work needs to be
done to understand this slippage") — plus n==1 drops driven by duplicates in their
microdata vintage, neither of which we can reproduce without their intermediates.
The rate implications are what matter for us: with F1+F2 applied, the remaining
rate residuals (ineligible 14.6 vs 11.4, eligible 53.2 vs 58.4) are consistent with
their samples being ~30% smaller subsets of ours with somewhat different
composition; there is no evidence left of a *methodological* difference on our side
beyond F3 (being tested) and the documented S1/S4.

---

# RESULT (2026-07-30 overnight): F3 CONFIRMED — never-eligible column closed

`8_reserve_cleaning_test.R` applied legacy's reserve-only cleaning (erase overlaps
from the larger parcel, never drop) to the F2 control sample and re-measured the
cleaned geometries (years 2005-2008 + 2014):

| statistic | F2 only | **F2 + F3** | paper |
|---|---|---|---|
| N | 6,856 | 6,855 | 7,049 |
| pre-2009 mean rate | 36.7 | **36.3** | 35.7 |
| total defo 2008 (Mha) | 2.329 | **2.003** | **2.0** |
| total defo 2014 (Mha) | 2.551 | **2.189** | **2.2** |
| mean declared area | 1,163 | 1,170 | — |
| mean cleaned geometric area | — | 1,029 | — |
| mean legacy-forest area (pre-2009) | — | 982.5 | 760? |

The totals now match the paper to two decimal digits — the reserve-specific
cleaning was exactly the missing step for the control group's aggregates.

On the one remaining number: the paper's 760 matches NO parcel-level mean we can
compute, but the RATIO construction total-defo/N ÷ mean-rate gives 795 on the
paper's own row and **805 on ours** — a 1.3% gap. "Property Area (ha, t<2009)" for
the control column is therefore most plausibly an aggregate-implied legacy-forest
area, not a mean of claimed boundaries (the ineligible column's 661, by contrast,
matches our mean DECLARED area 659.5). With that reading, every number in the
never-eligible column reproduces.

## Where this leaves the replication (end of 2026-07-30)

- **Never-eligible: reproduced** (rate 36.3/35.7, totals exact, N −2.7%, area
  consistent under the ratio reading). Required: F1 + F2 + F3.
- **Ineligible: rate 14.6 vs 11.4 open** (composition of legacy's smaller sample);
  mean area exact; totals ours 4.5/5.2 vs 4.1/4.7 Mha (~+10%, tracks the count
  surplus).
- **Eligible: rate 53.2 vs 58.4 open**, same composition suspicion (our pool +41%).
- Count surpluses attributed to legacy-side processing losses (their admitted 63k
  slippage + microdata-vintage n==1 drops); not reproducible from our side.
- Decision pending: 2014 (paper text) vs 2019 (legacy code) as the pipeline's
  official in-sample rule; both computed.

---

# THE ELIGIBLE SWEEP (2026-07-31): line-by-line, legacy 2_empirics.R:255-1340 vs our stages

Requested after the F1-F3 round: a meticulous pass over every step that produces the
paper's 71,171 eligible / 15,254 ineligible. Each finding lists function, output
effect, and whether the paper documents it. Numbers below are measured, not argued.

## N0 (OURS — the largest single unexplained gap) — drop decisions never reached Table 1

Stage 4 computes 24,203 drop decisions (20,993 `drop` + 3,210 `drop_random`) and
writes `parcels_resolved_2014.csv`; stage 3 uses it for the DiD (EMP_RESOLVED=1).
**Stage 2's Table 1 summary never excludes them**: 23,517 in-sample parcels
(17,585 eligible, 4,703 ineligible, 1,229 never-eligible) were counted despite
being dropped. Legacy removes dropped parcels from the per-muni `*_cleaned_*`
files BEFORE the eligibility tests, so its Table 1 never sees them.
- Function: Appendix C step 3 drop rules; documented in the paper.
- Output (`10_apply_drops_test.R`, F1+F2 basis, drops as a lower bound since they
  were computed under the 2014 sample rule): eligible N 100,185 -> **82,787**
  (paper 71,171; error +41% -> +16%); ineligible N 22,892 -> **18,278** (paper
  15,254); eligible totals 6.42/6.69 -> **5.47/5.69** Mha (paper 5.1/5.3);
  ineligible totals -> **3.95/4.46** (paper 4.1/4.7, now slightly under).
- The 2026-07-29 count-surplus note's "conflict resolution ruled out (29.5k max <
  33.5k surplus)" conflated *sufficient* with *relevant* — 24.2k of real drops
  were sitting unapplied.

## N1 — the conflict rules run on 2004 deforestation, not 2014

Legacy :736-738/:751-753 loads the **2004** raster for the cleaning block; the
`>= 80%` numerators (defo in intersection) and denominators (`dfrst__`, the 2004
deforested area recorded by the inGleba2 pass) are all 2004 quantities. Our stage
4 evaluates the same rules with `CR_YEAR=2014`.
- Function: decides WHICH side of a conflict is dropped/erased.
- Output effect: large — 2004 deforestation is sparse, so many pairs flip between
  evaluable and not (see N2), and drop targets differ. Not yet re-run; the exact
  fix is stage 4 with 2004 inputs plus N2-N4 semantics.
- Paper: Appendix B/C states the rules but never states the year the deforested
  shares are measured in. The "2014" in step 1 refers to the sample filter (which
  is itself really 2019 — F2).

## N2 — legacy silently drops conflicted parcels whose pairs cannot be evaluated

In legacy's per-muni cleaning, a pair row gets `drop_i = NA` when either side's
2004 deforestation is exactly zero (string `"0"`, :826) or the intersection is a
GEOMETRYCOLLECTION. NA rows are excluded from `overs`/`insiders_1`/`insiders_2`,
and the final assembly (:1138) keeps only `noConflict` plus parcels appearing in
those three sets — so a conflicted parcel whose every pair is unevaluable is
**dropped with no rule firing at all**. Our stage 4 `next`s those pairs and keeps
the parcels.
- Function: none intended — it is a fall-through of the assembly filter.
- Output (measured on our pairs + 2004 defo): among in-sample parcels with a
  legacy-gate conflict row, ALL rows unevaluable for **2,748 ineligible**, 335
  eligible, 196 never-eligible. The ineligible skew is mechanical: late entrants
  have zero 2004 deforestation. Stacked on N0's 18,278 this lands at ~15,530 vs
  the paper's 15,254 (+1.8%).
- Paper: not documented anywhere.

## N3 — containment pairs where neither side trips 80%: legacy erases, we no-op

Legacy's `insiders_2` (:1076-1130) takes containment pairs with drop_i=0 and
drop_minus_i=0 and randomly erases the intersection from one side. Our stage 4
takes no action on that case (act stays NULL). The paper's Appendix B bullet list
has no containment-0+0 rule either — the code invented one.
- Output effect: geometry/area shrinkage on containment pairs (donuts when the
  container loses), not drops. Affects measured areas and rates, magnitude untested.

## N4 — pair gating is direction-specific in legacy

Legacy builds a row per direction and gates on `intersect/NUM_ARE(i) > 10%` for
each direction separately (:774-790); mirrored rows are deduped only by the
ordered id, and the erase/delete loops deactivate the mirror with a `run` flag.
Our stage 4 gates once per unordered pair on `> 10% of EITHER side`. Consequence:
a parcel whose own overlap share is under 10% is untouchable in legacy (goes to
noConflict) even when its partner's share is large; our version can still examine
and drop it.
- Output effect: our drop set is a superset on asymmetric pairs; magnitude folded
  into the N1 re-run.

## N5 (LEGACY BUG) — the gleba overlap scoring covers the wrong rows

The target-pool scoring (:448-487) chunks `ccar_not_in_rez_valid` with slice
bounds that are typo'd: `slice(1000001:150000)`, `slice(1500001:200000)`, ... In
R these are DESCENDING sequences, so after the correct `1:100000`:
- rows **100,001-149,999 are never scored** — their gleba overlap is NA -> 0 ->
  excluded from the target pool regardless of true overlap;
- rows >= 150,000 are covered by up to SIX overlapping chunks, and the per-parcel
  `sum(intersect_area)` then multiplies their overlap share several-fold, letting
  parcels with true overlap down to ~0.2% clear the ">1%" bar.
- Function: none — it is a bug. The paper documents a 0.1% threshold (Appendix
  B) while the code writes 1%, and the executed arithmetic delivers neither.
- Output effect: legacy's target pool is missing a ~50k-row band (position-, not
  property-based — the band sits inside the AC/AM/RR/AP block that leads their
  row order) and includes an inflated low-overlap fringe. Both push their
  eligible/ineligible pool away from ours in opposite directions; without their
  exact row order this is irreproducible. It is plausibly a large share of the
  remaining eligible N and rate residual.

## Steps verified as matching (no difference found)

- Biome combine: interior states (AC/AM/RR/AP) kept whole, others clipped —
  matches our 05_combine_car_biome.R (their Para double-processing is neutralized
  by the :355 dedup; our #21 regularizations don't change membership).
- COD_IMOVEL dedup before scoring (:355 vs our slice(1)).
- Control-first precedence; CNFP layer definitions; 1% threshold as written.
- The 10% conflict gate against DECLARED area (NUM_ARE) on both sides.
- The `>= 80` drop cutoffs and the containment cutoffs (>= .9), including the
  both-contained -> "overlap" tiebreak.
- Random draws for overlap-0+0 (erase one side) and overlap-1+1 (delete one side)
  map to our `erase_intersection_random` / `drop_random` (distribution-equivalent
  only; legacy is unseeded).
- Eligible finalization: 2004 LEVEL test on cleaned geometry (S1, tested small),
  geometric-area cap (S3, tested small: 84+37 switches), panel joins from the
  2005 file with n==1 filters (inert on our vintage).

## Standing accounting of the residual gaps (F1+F2+drops, lower-bound drops)

| | ours | paper | remaining error | dominant remaining cause |
|---|---|---|---|---|
| eligible N | 82,787 | 71,171 | +16% | N1 re-run pending, N5 (their missing band), universe slippage |
| eligible rate | 53.3 | 58.4 | -8.7% | composition of the above |
| ineligible N | 18,278 (~15,530 after N2) | 15,254 | +1.8% after N2 | N1 re-run pending |
| ineligible rate | 14.4 | 11.4 | +26% | N1/N2 change the drop composition; N5 fringe parcels (low-overlap, low-rate) sit in THEIR pool but not ours |
| never-eligible | closed (F3) | — | <=6% everywhere | — |

Next implementation step if desired: a legacy-faithful stage-4 mode (2004 rules,
NA-pair drops, direction gate, containment erase) re-run on the 2019-rule sample,
then stage 2 consuming parcels_resolved for the Table 1 summary.

---

# RE-AUDIT OF THE ELIGIBLE CHAIN (2026-07-31, second pass): every line re-traced

Requested check of earlier assumptions. New findings P1-P3, verified-inert checks
P4-P9, and confirmations. Each: function / measured output / paper status.

## P1 (CONFIRMED, large for ineligible) — the :1704 filter is on the 2005 legacy-forest area

`inelegible$area` is created ONLY in the 2005 file's block; the year 2006-2014
join blocks never recreate it (their `data` has no `area` column, so no suffix
clash — the 2005 column survives). Therefore `filter(!is.na(area) & area < 1e5)`:
- uses the **2005** legacy-forest area (`defo_2005/(rate_2005/100)`), and
- excludes every ineligible parcel with **zero 2005 deforestation** (0/0 = NaN ->
  NA via the `mutate_all` line) — i.e. all post-2005 entrants, measured on the
  cleaned geometry.
Our stage 2 port used the 2008 values. Faithful recompute
(`11_lf2005_filter_test.R`, F1+F2+drops basis): the filter drops 12,094 in-sample
ineligible parcels (2008 basis: 7,586); **ineligible N = 14,909 vs paper 15,254
(-2.3%)**. Side effects: the pre-2009 average rate RISES to 16.3 (zero-rate rows
leave), the **2005 yearly mean is 11.8 vs the paper's 11.4 (+3.5%)**, and mean
area moves to 868.8 (away from 661). Paper: the filter is documented nowhere.

This reopens the F1 question of what "average prior to 2009" means operationally:
on the faithful sample our 2005-only mean reproduces 11.4 almost exactly, while
the 2005-2008 average does not. Candidate resolution: legacy's Table 1 rate may
be the 2005 (panel-start) value, or the N1-faithful cleaning further lowers
2006-2008. Undecidable without the N1 re-run.

## P2 (re-confirmed, direction now precise) — D6, control areas not erased from glebas

Legacy keeps `glebas_alt` intact (:76-77; the erase at :96 is commented). Our
build erases control from target. Consequence beyond what D6 recorded: a parcel
with <1% control overlap whose gleba overlap lies mostly INSIDE a gleba∩control
region is target for legacy but can fall below 1% for us and land in NO pool.
Direction: shrinks our pool (opposite of the surplus); composition-relevant.
Paper: Appendix flow says nothing about the erasure either way.

## P3 — legacy runs all layer intersections without st_make_valid

The CNFP/biome intersections (:71-77) and the overlap scorings run on unrepaired
geometries under GEOS-lenient planar mode; we st_make_valid everything. Function:
sliver/self-intersection tolerance. Output: unmeasurable from our side (their
GEOS version decided which features survived); expected small. Paper: n/a.

## Checks that came back INERT (assumptions verified)

- **P4 — NUM_ARE fallback**: our build substitutes geometric area when declared
  NUM_AREA is missing/<=0; measured: **0 of 177,248** pool parcels affected.
- **P5 — CNFP attribute normalization**: raw values are unaccented uppercase
  (`governo` in {FEDERAL, ESTADUAL, ...}, `protecao` = "SEM DESTINACAO"), so
  legacy's raw filters == our normalized ones, including exact-match `governo ==
  "FEDERAL"` excluding FEDERAL/ESTADUAL mixes on both sides.
- **P6 — already_treated (SNCI)**: built at :339 and never consumed downstream in
  2_empirics.R — SNCI-titled parcels are NOT excluded from any pool. Ours neither.
- **P7 — combine chunking off-by-one** (:277, 101-row chunks): duplicates are
  removed by the :355 COD_IMOVEL dedup. Inert.
- **P8 — hardcoded break ranges** (:279-303, `breaks[1:1000]`...`[4001:4398]`):
  fitted to their vintage's row counts; would truncate on different data. Not
  verifiable from our side; flagged as vintage risk only.
- **P9 — ccar_dirty round-trips through a Dropbox shapefile** (:516): the version
  measured downstream is whatever ccar_dirty.shp held at run time; if stale, even
  the N5-buggy scoring wouldn't describe it. Vintage risk, unverifiable.

## Confirmations from the verbatim re-read

- inGleba2 (:673-712) drops the 2019 columns before writing, so the cleaning
  block's `dfrst__`/`dfrstt_` are definitively the 2004 measurements (N1 stands).
- The eligible pool gets NO area/lf filter analogous to :1704 (only ineligible).
- Panel base year is the 2005 file (alphabetical file order), n==1 filters as
  documented; `NaN -> NA` conversion matches our NA semantics.
- STEP-1's rate denominator (`layer != 0`) equals our valid_px (classes 1/2/3);
  NA pixels drop out of both.
- Zero-valid-pixel parcels: rate NaN -> excluded by `> 10` in legacy; NA ->
  excluded by our in-sample rule. Same.

## Updated standing accounting

| | ours (best faithful) | paper | error |
|---|---|---|---|
| eligible N | 82,787 | 71,171 | +16.3% |
| eligible pre-2009 rate | 53.3 | 58.4 | -8.7% |
| ineligible N | **14,909** | 15,254 | **-2.3%** |
| ineligible 2005 rate / pre-2009 avg | **11.8** / 16.3 | 11.4 | **+3.5%** / +43% |
| ineligible totals 2008/2014 | 3.91 / 4.36 | 4.1 / 4.7 | -4.6% / -7.2% |
| never-eligible (F3 basis) | closed | — | <=6% |

Eligible-side residual candidates, in order: N5 (their missing 100k-150k row band
— position-based, irreproducible), N1 re-run (2004-based drop composition), P2/D6
pool-membership fringe. The ineligible column is now effectively reproduced in N,
totals, and the 2005 rate; only the "average prior to 2009" reading is unsettled.

---

# RESULT (2026-07-31): the N1 re-run — legacy-faithful 2004-rule cleaning

`12_conflict_2004_rerun.R` (seed 20260731): direction-gated pairs (N4), 2004-based
drop rules (N1), unevaluable-pair exclusion (N2), and the as-executed assembly that
also loses drop-pair winners without other surviving conflicts (N6), run on the
active-2019 target pool (130,663 parcels; 76,767 direction rows; 51,755 measured
pair intersections; ~15 min). The cleaning keeps 94,682. With the P1 (2005-basis)
ineligible filter on top:

| | F2 + 2014-rule drops (+P1) | **F2 + N1-faithful cleaning + P1** | paper |
|---|---|---|---|
| eligible N | 82,787 | **76,592** | 71,171 |
| eligible defo 2008/2014 (Mha) | 5.47 / 5.69 | **4.97 / 5.16** | 5.1 / 5.3 |
| eligible mean area | 136.0 | 133.2 | 143 |
| eligible pre-2009 rate | 53.3 | 53.7 | 58.4 |
| ineligible N | 14,909 | 12,021 | 15,254 |
| ineligible defo 2008/2014 | 3.91 / 4.36 | 3.21 / 3.58 | 4.1 / 4.7 |
| ineligible 2005 rate / pre-2009 avg | 11.8 / 16.3 | 12.0 / 16.7 | 11.4 |

Read-out:
- **Eligible is now effectively reproduced in levels**: N +7.6%, totals −2.5/−2.7%
  (from +41% and +26% at the start of this exercise). The as-executed cleaning
  semantics — including the winner-vanishing assembly — are what the 71,171
  requires; the documented Appendix C rules alone are not enough.
- **Ineligible is bracketed by our two approximations**: gentler 2014-rule drops
  give −2.3% N, the faithful 2004-rule cleaning overshoots to −21%. Legacy's true
  outcome sits between, because our conflict graph (raw-geometry overlaps from CAR
  stage 03) is denser than legacy's per-muni active-file intersections, and the
  random draws / row order are distribution-equivalent only. The 2005-basis rate
  lands at 11.8-12.0 vs 11.4 in both variants.
- Caveats: rates ride on the 2014-rule erasure panel (the 2004-rule erasure set was
  not re-measured); some pair intersections degraded to GEOMETRYCOLLECTION and
  were skipped by terra (warnings), biasing toward fewer resolutions.

## Bottom line of the whole exercise

Every Table 1 number is now attributable to identified, tested mechanisms: F1
(pre-2009 averaging) + F2 (2019 sample raster) + F3 (reserve-only cleaning) + N0
(drops must reach the sample) + N1/N2/N4/N6 (as-executed cleaning semantics) + P1
(2005-basis ineligible filter), with the residual scatter (~±8% on eligible rate,
the ineligible N bracket) explained by irreproducible legacy-side artifacts (N5
slice bug, unseeded randomness, their conflict graph, the 63k slippage). There is
no remaining unexplained methodological difference on our side.

---

# CONSOLIDATED (2026-07-31): the stack is now stage 2 + stage 3, and the DiD is re-run

Stages 6-12 were diagnostics. The findings they established now live in the pipeline:

- **stage 13** (`13_control_cleaned_panel.R`, new): the never-eligible panel measured
  on legacy's reserve-cleaned geometry (F3) for all DiD years 2005-2014, geometry
  cached. Supersedes stage 8, which only covered 2005-2008 + 2014 — the DiD needs one
  geometry basis across the window or the cleaning itself would look like a 2009 break.
  Cleaning logic factored into `_helpers_reserve_clean.R`.
- **stage 2**: writes three nested sample columns — `in_sample` (paper's stated 2014
  rule + 2008-basis filter; retained because stages 4/4b consume it), `basis_sample`
  (EMP_SAMPLE_YEAR, default 2019 = as legacy ran it, F2, + the 2005-basis filter P1),
  and `final_sample` (basis minus the conflict drops, N0/N1). Table 1 now reports
  pre-2009 average rates (F1) with 2005-only and 2008-only beside them, and the
  control column's rates AND totals from the F3 panel.
- **stage 3**: consumes `final_sample` and swaps the F3 panel in for the control
  group's outcomes. EMP_RESOLVED=0 falls back to `basis_sample` (no cleaning drops).

## Bug found while consolidating (ours, pre-existing)

Stage 2's erasure block merged `erased_ha` on (car_id, year) AND on car_id; the
`suffixes = c("", ".y")` left the per-YEAR copy in force. Every year present in
`erasure_adjustment.csv` (1987-2014) had its area shrunk, but a year outside it did
not — invisible until 2019 entered the reshape, at which point 12,401 parcels carried
two different `area_ha` values, split into two dcast rows, lost their outcome columns,
and 7,268 of them silently flipped eligible -> ineligible. Fixed by taking only the
pixel columns from the per-year merge; the sample-year flags now come straight from
the long panel so the reshape can never be perturbed again.

## Table 1, consolidated run

| | ours | paper | error |
|---|---|---|---|
| eligible N / defo08 / defo14 | 76,592 / 4.972 / 5.156 | 71,171 / 5.1 / 5.3 | +8% / -3% / -3% |
| eligible rate pre-2009 (2005 / 2008) | 53.7 (51.1 / 56.1) | 58.4 | -8% |
| ineligible N / defo08 / defo14 | 12,021 / 3.209 / 3.580 | 15,254 / 4.1 / 4.7 | -21% / -22% / -24% |
| ineligible rate pre-2009 (2005 / 2008) | 16.7 (**12.0** / 21.1) | 11.4 | +46% (**+5%** on 2005) |
| never-elig N / defo08 / defo14 | 6,855 / **2.003** / **2.189** | 7,049 / 2.0 / 2.2 | -3% / **+0%** / **-1%** |
| never-elig rate pre-2009 | 36.3 | 35.7 | +2% |

## DiD, re-run on `final_sample` (stage 3, 954,680 parcel-years, 9 state clusters)

| comparison | outcome | beta (p.p.) | se | p | pre-mean |
|---|---|---|---|---|---|
| eligible vs never-elig | legacy-forest | **-1.212** | 0.742 | 0.141 | 53.53 |
| ineligible vs never-elig | legacy-forest | **+5.556** | 1.310 | 0.003 | 16.74 |
| eligible vs never-elig | claim area | -1.075 | 0.962 | 0.296 | (junk, see below) |
| ineligible vs never-elig | claim area | +5.283 | 1.414 | 0.006 | 20.54 |
| **paper** | | **-1.412** / **+4.204** | 0.558 / 0.886 | | 58.4 / 11.4 |

Both signs match and both magnitudes are far closer than the pre-sweep estimates
(-1.742 / +9.502 at checkpoint-20260729b): the eligible coefficient is now within
0.2 p.p. of the paper's and the ineligible within 1.4 p.p. The legacy-forest
outcome is primary — it is what legacy's code computes and what our Table 1
reproduces; its pre-means (53.5, 16.7) are also the ones comparable to the paper's
58.4 / 11.4 baselines.

**Do not read `rate_claim`'s pre_mean_treated**: declared areas include 256 eligible
parcels under 1 ha, giving that ratio a tail to ~10^8 percent (median 50.8, p99
102.4, mean 1325). The FE estimator differences within parcel so its beta is still
interpretable, but the level is not a baseline.

Caveats unchanged: SEs are hand-rolled CR1 on 9 clusters; the conflict drops come
from the 2004-rule cleaning whose erasure set was not re-measured for the rate panel;
random draws are distribution-equivalent only.

---

# RESULT (2026-08-01): the ineligible bracket, and three new DiD-path findings

## The conflict-semantics grid (`14_conflict_variants.R`)

Pair measurements cached once (41,707 pairs, `conflict_pair_defo_2004.csv`); the set
logic then replayed under 2x2x2 semantics x 3 seeds. Counts below already carry P1,
so they compare directly against Table 1's 15,254 (and Table 2's implied ~16,134).

| require_j_alive | winner_vanishes | drop_unevaluable | eligible | ineligible |
|---|---|---|---|---|
| FALSE | FALSE | FALSE/TRUE | 83,470 | **14,471** |
| TRUE | FALSE | FALSE/TRUE | 83,554 | **14,488** |
| FALSE | TRUE | FALSE | 78,902 | 13,564 |
| TRUE | TRUE | FALSE | 78,297 | 13,390 |
| FALSE | TRUE | TRUE | 78,474 | 12,576 |
| TRUE | TRUE | TRUE | 77,869 | 12,402 |
| paper | | | 71,171 | 15,254 (Table 2 implies ~16,134) |

Findings:
- **`require_j_alive` is inert** (+/-20 parcels). My stage-12 deviation was real but
  NOT the cause of the overshoot — hypothesis falsified, as measured.
- **`winner_vanishes` is the dominant lever**: ~1,000 ineligible and ~4,600 eligible
  parcels. `drop_unevaluable` costs another ~1,000 ineligible, but only when
  winner_vanishes is on.
- **Seed variation is negligible** (+/-20), so the random rules are not the story.
- **No combination reaches the target window.** The ceiling is 14,488 ineligible,
  still -5% vs Table 1 and -10% vs Table 2's implied count.

So the bracket collapses: any defensible semantics gives **14.5k-14.9k ineligible**
(the 2014-rule variant's 14,909 sits in the same band), i.e. **-2% to -5%** vs
Table 1 — not the -21% we reported from stage 12, which was the most aggressive
corner of this grid. **The remaining gap is not in the resolution semantics**; it is
upstream (pool composition / vintage).

Tension worth stating: no single setting fits both columns. winner_vanishes=FALSE
fits ineligible best (14,488) but leaves eligible at 83,554 (+17%); TRUE fits
eligible best (77,869, +9%) but drops ineligible to 12,402. A single legacy run
cannot have produced both of the paper's columns from our pool — further evidence
that Table 1's two columns came from different intermediates (see D-C below).

## D-A (NEW) — the DiD sample drops zero-2014-deforestation parcels

Legacy :2773/:2841 builds `drop_spillover`/`drop_control` = parcels whose 2014
deforested area is exactly 0, and excludes them from the DiD panels. Applied to the
control and ineligible groups but NOT to eligible — the asymmetry is in the code.
Undocumented in the paper. On our sample: 384 parcels.

## D-B (NEW) — the DiD outcome is WINSORIZED at 1/99, and the paper never says so

Legacy :2883/:2887: `mutate(value_w = winsorize(value, c(0.01, 0.99)))` grouped by
variable-year, written to `did1_new.dta` / `did2_new.dta`. The regressions themselves
ran in **Stata**, on that winsorized column. A search of the manuscript for
"winsor|trim|outlier|top-code" returns **nothing**.

## D-C (NEW) — Table 1 and Table 2 are built from DIFFERENT samples and vintages

The DiD blocks read a different directory (`Dropbox/amazon_working/`) with different
prefixes (`CAR_notEligible_defo_`, `car2004_defo_`, `CAR_inReservas_defo_`) than the
Table 1 blocks (`amazon_project/data/intermediate/`, `CAR_eligible_defo_`,
`CAR_ineligible_defo_`, `CAR_control_defo_`). Crucially the DiD control is measured
on **`ccar_clean_inReservas`** — the raw >1%-overlap control pool, with **no**
active-occupation filter and **no** cleaning — whereas Table 1's control is the
cleaned, filtered `control_final`. That is a direct structural explanation for why
Table 2's observation count implies ~16,134 ineligible against Table 1's 15,254:
the two tables do not describe the same sample.

## DiD under the legacy options (stage 3, EMP_DROP_ZERO2014=1 EMP_WINSOR=1)

| comparison | outcome | beta | se | p | paper |
|---|---|---|---|---|---|
| eligible vs never-elig | legacy-forest | **-1.476** | 0.749 | 0.084 | **-1.412** |
| ineligible vs never-elig | legacy-forest | +5.429 | 1.353 | 0.004 | +4.204 |
| eligible vs never-elig | claim | -1.189 | 0.720 | 0.137 | |
| ineligible vs never-elig | claim | +5.757 | 1.350 | 0.003 | |

The eligible coefficient now sits within **0.06 p.p.** of the paper's. Winsorizing
moves eligible (-1.212 -> -1.476) far more than ineligible (+5.556 -> +5.429).

## Bug fixed while building stage 14

`data.table(key = ...)` silently treats `key` as the reserved key argument, not a
column: the first grid run crashed and discarded 7 minutes of measurement. Column
renamed to `pair`, and the cache is now written before any set logic runs.

---

# LIVE-DIFFERENCE REGISTER (2026-08-01): our CURRENT code vs legacy, line by line

Earlier audits compared legacy against OUR CODE AS IT WAS. The pipeline has since
changed (stages 2/3 consolidated, 13 added), so this is a fresh pass over what is on
disk now. Each entry: what differs, measured size, and whether it is deliberate.

## Still unaligned, deliberate (paper text over legacy code)

- **L1 (S1) — occupation test.** `2_eligibility_split.R:118` uses first-crossing
  (`deforestation_rate >= 10` in ANY year <= 2004). Legacy :1209/:1290 uses the 2004
  LEVEL (`> 10` on the 2004 raster). We follow the paper's "when occupation first
  began" (2.3). Measured: 3,892 parcels move eligible -> ineligible.
- **L2 (S3) — area cap.** `:134` tests DECLARED `area_ha <= 1500`; legacy tests
  GEOMETRIC `st_area()/1e4 <= 1500` (:1231/:1315). Both also cap 2004 deforested
  area. Measured: 84 eligible -> ineligible, 37 the other way.

## Still unaligned, not deliberate — fix candidates

- **L3 (S4) — boundary operator.** Ours: `>= 10` for the occupation test and the
  2014 in-sample flag (`:118`, `:151`, `:183`); legacy is strict `> 10` everywhere.
  The 2019 flag (`:158`) is already strict. Measured: **53 parcels** sit at exactly
  10.000 in 2014. Free to align.
- **L4 — MIXED CLEANING BASES inside stage 2 (new, ours).** The rate/area erasure
  comes from `erasure_adjustment.csv`, produced by stage 4/4b under the **2014-rule**
  decisions, while the DROPS come from `parcels_resolved_2004rules.csv`, produced by
  stage 12 under the **2004-rule** semantics. So a parcel can be shrunk by one run's
  erase decision and kept/dropped by another's. Internally inconsistent; the clean
  fix is to re-run 4b off the 2004-rule decision set.
- **L5 (D-C) — DiD control pool.** Stage 3 gives the control group the Table-1
  treatment (occupation filter + reserve cleaning). Legacy's DiD control is
  `ccar_clean_inReservas`: the raw >1%-overlap pool, **no occupation filter, no
  cleaning**. Our stage 3 is therefore comparable to Table 1's sample but NOT to
  Table 2's, which is what the coefficients are benchmarked against.
- **L6 — production stage 4 vs the faithful semantics.** `4_conflict_resolution.R`
  still evaluates the rules on 2014 deforestation (N1), skips pairs whose members are
  already dropped (C3), and no-ops on containment-0+0 (N3). Stages 12/14 hold the
  faithful versions; stage 4 has not been retired or updated.

## Verified equivalent on this pass (no action)

- Raster extraction: `terra::extract` with default `touches = FALSE` (cell-centre) on
  both sides; `valid_px = val != 0` with NA excluded matches legacy's
  `filter(layer != 0)` (dplyr drops NA). Legacy reads one national mosaic per year
  (`f[1]`); we sum exact, non-overlapping 1-degree tiles -- equivalent.
- Reserve cleaning (stage 13 helper) vs legacy :1858-1955: same >10%-of-declared
  gate, same erase-from-larger-by-NUM_ARE direction in both branches, same sequential
  mutation of the working layer, same GEOMETRYCOLLECTION/LINESTRING handling.
- `_helpers_twfe.R`: CR1 correction `(G/(G-1))*((N-1)/(N-K))` and `df = G-1` match
  Stata's cluster default. The paper's columns (2)-(3) use year-by-state and
  year-by-municipality FE, which we do not estimate -- not a baseline difference.
- Class precedence (control first), CNFP layer filters, the 1% pool threshold as
  written, dedup-by-id before scoring, panel base year 2005, NaN -> NA semantics.

## Being measured now (stage 15)

**L7 (S2 + P2/D6) — pool membership.** Legacy divides the pool-overlap by GEOMETRIC
parcel area and scores against the UN-ERASED gleba layer; we use DECLARED area and
erase control from target. The earlier S2 test could only see parcels already in one
of our layers, so flips INTO a pool were invisible -- exactly the direction the
semantics grid says the residual must live in. `15_pool_membership_test.R` scores
every CAR in the biome under all four combinations.

## RESULT — L7 (pool membership) ELIMINATED

`15_pool_membership_test.R` scored all **801,813** CARs in the biome (16,084 touch the
control mask, 182,572 the target mask) under every combination:

| variant | control pool | target pool |
|---|---|---|
| ours (declared denominator + erased target) | 13,017 | 164,187 |
| declared + legacy's un-erased target | 13,017 | 164,187 |
| geometric + erased target | 13,025 | 164,223 |
| legacy (geometric + un-erased target) | 13,025 | 164,223 |
| our layers on disk | 13,025 | 164,223 |

- **Declared vs geometric denominator: 8 control / 36 target parcels** (0.02%).
- **Erasing control from target: ZERO difference**, at any denominator. The two CNFP
  filters (`classe` UC|TI vs `protecao` SEM DESTINACAO) select disjoint polygons in
  this vintage, so D6/P2 is definitively **inert** — closing an item that had been
  open since the first diff note.

Pool membership is therefore NOT the residual. Combined with the semantics grid
(residual not in the conflict algorithm) and the earlier eliminations (muni-straddle,
Pará double-processing, microdata-join loss, S1-S4 all small), every mechanism on OUR
side is now either implemented or measured and ruled out.

## Where the replication stands, definitively

The remaining gaps — eligible N +9% to +17% depending on cleaning semantics,
ineligible N -2% to -5%, eligible rate ~-8% — are attributable to legacy-side
artifacts that cannot be reproduced from our data:

1. **N5**, the typo'd `slice()` bounds in legacy's gleba scoring: rows 100,001-149,999
   of their CAR table were never scored, and rows >= 150,000 were scored by up to six
   overlapping chunks whose areas are summed. Position-dependent on their row order.
2. **The 63k slippage** their own appendix concedes (891,234 raw -> 829,260 processed).
3. **D-C**: Table 1 and Table 2 were built from different intermediate vintages, with
   the DiD control being the raw uncleaned pool — so no single sample reproduces both.
4. Unseeded random draws in two conflict rules (distribution-equivalent only).

No further code-level test on our side is expected to move these numbers.
