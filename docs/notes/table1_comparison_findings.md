# Table 1 comparison — first rebuild vs the paper

Date: 2026-07-29
Inputs: `data/intermediate/empirics/parcel_defo_{2004,2008,2014}.csv` (all 713 raster
tiles per year), `parcel_eligibility.csv`, `table1_comparison.csv`.
Produced by `code/01_build/06_empirics/{1_parcel_deforestation,2_eligibility_split}.R`.

## Verdict

**The per-parcel measurement reproduces. The levels do not, and the gap is entirely in
parcel counts, not in measurement.** This is the expected signature of the deferred
spatial conflict-resolution stage.

## Our rebuild

| group | N | defor Mha 2008 | defor Mha 2014 | mean rate 2008 | mean defor ha 2008 | mean area ha | change |
|---|---|---|---|---|---|---|---|
| eligible | 98,547 | 6.534 | 6.763 | 56.3% | 66.30 | 136.4 | +3.5% |
| ineligible | 25,790 | 5.085 | 5.846 | 20.4% | 197.16 | 592.3 | +15.0% |
| never-eligible | 13,025 | 2.645 | 2.915 | 21.3% | 203.10 | 3,980.2 | +10.2% |

Sample: 137,362 of 177,248 parcels are in-sample; ~40k drop out as never occupied.
Raster coverage was 177,04x / 177,248 (99.9%) in every year.

## Versus the paper

**Per-property quantities — all within 4%:**

| quantity | ours | paper | diff |
|---|---|---|---|
| eligible mean defor ha 2008 | 66.30 | 69.03 | −4% |
| eligible mean rate 2008 | 56.3 | 58.4 | −4% |
| ineligible mean defor ha 2008 | 197.16 | 204.3 | −3% |
| never-eligible % change | 10.2 | 11.0 | −7% |

**Totals and counts — 24-72% high:**

| quantity | ours | paper | diff |
|---|---|---|---|
| eligible total Mha 2008 | 6.534 | 5.10 | +28% |
| eligible total Mha 2014 | 6.763 | 5.27 | +28% |
| ineligible total Mha 2008 | 5.085 | 4.10 | +24% |
| ineligible total Mha 2014 | 5.846 | 4.66 | +25% |
| ineligible N | 25,790 | ~15,000 | +72% |

## Decomposition: the error is in N, not in the measurement

`total = N x mean`. The means match, so the surplus must be in N. Backing out the
paper's implied counts from its own totals and means:

| group | our N | implied paper N | paper's stated N |
|---|---|---|---|
| eligible | 98,547 | ~73,900 | not stated |
| ineligible | 25,790 | ~20,100 | ~15,000 |

We carry roughly **25-30% surplus parcels**, each measured correctly.

## Why: duplicate claims, not over-measurement

CAR registrations are self-declared and unchecked, so multiple claims cover the same
land. Measured on our target pool:

- **85.9%** of parcels (141,038 of 164,223) sit in at least one overlap
- **185,816** unique overlapping pairs
- overlapping land totals **11.8 Mha** (int_area is m²; median pair 1.3 ha)

The conflict-resolution algorithm (`2_empirics.R:731-1170`) exists to assign each cleared
patch to exactly one claim. Until it runs, duplicate claims inflate N and therefore the
totals. Note this refines the earlier framing in the session: it is surplus *claims*,
each measured correctly, not inflated area per parcel.

**Ruled out:** parcels only clipping the target area. Legacy uses
`ccar_dirty <- ccar_not_in_rez %>% filter(overlap*100 > 1)` (line 497) and keeps whole
parcels — identical to our scaffold's >1% rule. Affects both sides equally.

**Not fixed by conflict resolution:** we assume a flat 0.09 ha/pixel, but at −10°
latitude a 0.000269495° pixel is ≈0.0885 ha, so we run ~1.5% high throughout. Cheap to
correct if it matters.

## The one real anomaly — ineligible composition

`ineligible mean rate 2008: ours 20.4% vs paper 11.4% (+79%)` is the only figure outside
7%, and it is not explained by the count surplus.

Composition looks wrong: our ineligible parcels average **592 ha**; the paper's implied
average is **~1,792 ha** (204.3 ha deforested / 11.4%). So we classify too many *small*
parcels as ineligible.

Likely mechanism: we operationalise "occupation started ≤2004" as `rate_2004 > 10`
(legacy's threshold, `2_empirics.R:636,1209,1290`). A small parcel that was occupied but
only lightly cleared by 2004 fails that test and falls into ineligible, when it probably
belongs in eligible. That pulls the ineligible mean area down and its mean rate up —
exactly the observed direction.

The paper states only "occupation started up to 2004" and does not give the operational
cutoff, so **this needs confirmation from the authors** rather than a guess. Candidate
alternatives: a lower rate threshold, any non-zero clearing, or a first-clearing-year
test derived from the transition rasters rather than a level test.

## Next steps

1. **Port the conflict-resolution stage.** Not blocked — parcel geometries, rasters, and
   the precomputed pairwise overlaps (`CAR_overlap_variables_conflicts.csv`, 1.25M pairs
   from CAR stage 03b) are all on disk. Legacy recomputed overlaps inside the loop; we can
   feed it the precomputed pairs instead, which should remove the most expensive part of
   what made CAR stage 03 slow. Estimate 1-2 days porting + 1-3 days compute, written
   per-municipality with skip-if-exists.
   **Reproducibility caveat:** rule 3 keeps one claim *at random* when only the shared
   area is cleared. That needs a fixed seed, and the original run used whatever seed R
   had — so those cases can only match in distribution, never bit-identically.
2. **Resolve the occupancy threshold** with the authors (see anomaly above).
3. Re-run stage 2 and re-compare; then the DiD assembly.

---

# Second pass (2026-07-29, after the 1987-2004 occupation fix)

Re-ran with the first-crossing occupation test (18 years, 1987-2004) and the
Appendix C step-1 sample rule. 112,904 parcels ever reach 10% by 2004.

| quantity | pass 1 | pass 2 | paper | pass-2 diff |
|---|---|---|---|---|
| eligible N | 98,547 | 98,941 | — | — |
| eligible total Mha 2008 | 6.534 | 6.537 | 5.10 | +28% |
| eligible mean rate 2008 | 56.3 | 56.4 | 58.4 | −3% |
| eligible mean defor ha | 66.30 | 66.06 | 69.03 | −4% |
| **ineligible N** | 25,790 | **21,923** | ~15,000 | +46% (was +72%) |
| ineligible total Mha 2008 | 5.085 | 5.009 | 4.10 | +22% |
| **ineligible mean rate 2008** | 20.4 | **20.4** | 11.4 | **+79% (unchanged)** |
| ineligible mean defor ha | 197.16 | 228.48 | 204.3 | +12% (was −3%) |
| ineligible mean area ha | 592.3 | 674.3 | — | — |
| never-eligible % change | 10.2 | 10.2 | 11.0 | −7% |

**What the fix achieved:** the ineligible count gap halved (+72% -> +46%) and mean
area rose (592 -> 674 ha). Eligible-side agreement held (rate −3%, mean defor −4%).

**What it did not:** the mean-rate anomaly is *completely unchanged* at 20.4% vs 11.4%.
My prediction that the occupancy fix would "largely fix" the composition anomaly was
wrong — the reclassified parcels had near-average rates, so the mean did not move. Mean
deforested area also overshot (−3% -> +12%).

## Three hypotheses tested and eliminated

1. **Occupation level-vs-first-crossing (issue #E1).** Real difference, correctly fixed,
   but it moves counts, not the rate.
2. **Rate denominator.** Paper §3.2 defines its outcome as `deforested/total area` while
   Appendix C's occupation test uses `deforested/legacy forest`. Computing both:
   ineligible 20.4% (legacy forest) vs 20.0% (total area). **Not the cause.**
3. **Declared vs geometric area.** Legacy uses `st_area(car)/10000` (line 1227); we used
   the declared `NUM_ARE`. Measured: mean 343.8 vs 342.8 ha, median 66.3 vs 66.4, and
   4,759 vs 4,895 parcels over 1,500 ha. Reclassifying on geometric area moves ineligible
   from 21,923 to 21,935 and its rate from 20.4 to 20.5. **Not the cause.**

## Leading remaining candidate: the gleba threshold

Appendix B step 2 defines a "dirty" CAR as **>0.1%** of its area inside a federal gleba.
Our scaffold uses **>1%** (see `paper_appendix_specs.md` section 4).

Mechanism: a 0.1% threshold admits parcels that only marginally clip a gleba. Large
parcels are far more likely to clip a boundary at a low share, so the looser threshold
should pull in many more big, lightly-cleared properties — which would raise the
ineligible group's mean area and lower its mean rate, the exact direction needed.

Caveat on the arithmetic: the paper's "implied mean area" of ~1,792 ha (204.3 / 0.114)
assumes its rate is a ratio of means. If Table 1 reports a mean of ratios, that implied
figure is not valid, so the composition cannot be pinned down from two summary statistics
alone. Do not treat ~1,792 ha as established.

Testing this requires re-running the CAR scaffold's overlap scoring at 0.1% — the
expensive step, though already optimised (CAR issue #26).

## Question for the authors

Two things only they can settle:
1. **Is the gleba threshold 0.1% (Appendix B) or 1% (the legacy code)?** A 10x difference
   in target-pool membership.
2. **What exactly is Table 1's "deforestation rate"** — mean of per-parcel ratios, or
   aggregate deforested/aggregate area? And over which denominator (legacy forest or total
   property area)? This determines whether a 20.4% vs 11.4% gap is even comparable.

---

# Third pass (2026-07-29) — the ACTUAL Table 1, and a bug in our sample rule

## The real Table 1 (found on p.32)

Earlier passes compared against figures scraped from §3.2 prose plus an *inferred* mean
area. The actual table is:

**TABLE 1: CHARACTERISTICS OF OCCUPATIONS IN TARGET AND CONTROL AREAS**

| | Eligible | Ineligible | Never eligible |
|---|---|---|---|
| # Properties | 71,171 | 15,254 | 7,049 |
| Property Area (ha, t<2009) | 143 | 661 | 760 |
| Property Deforestation Rate (t<2009) | 58.4% | 11.4% | 35.7% |
| Δ Property Deforested Area | 6.3% | 15.6% | 11.5% |
| Total Deforested Area 2008 (Mha) | 5.1 | 4.1 | 2.0 |
| Total Deforested Area 2014 (Mha) | 5.3 | 4.7 | 2.2 |

**RETRACTION:** the earlier "implied ineligible mean area ~1,792 ha" was wrong, as flagged
at the time. The true figure is **661 ha** and ours is **674 ha (+2%)**. The pass-1/2
diagnosis "we classify too many small parcels as ineligible" was therefore **incorrect** —
our ineligible area was right all along.

## Rate definition — settled by the table note

> "We define deforestation rates as the share of a property claim's area that has been
> deforested. Property areas are calculated directly from boundaries submitted by land
> occupants."

So: **deforested / claim area**, area **as declared** by the occupant. Neither of the two
guesses in pass 2 was right: not legacy forest, and declared area (not geometric) is
correct — which vindicates the original `NUM_ARE` choice.

## Bug found and fixed: the control group escaped the occupancy filter

The Table 1 note states *"Until 2008, all of these rural parcels illegally occupied public
land in the Amazon"* — never-eligible parcels are **occupied squatters**, not every CAR
that happens to touch a reserve. Our `in_sample` exempted `never_eligible` from the
>=10%-in-2014 filter. Fixed.

| never-eligible | before fix | after fix | paper |
|---|---|---|---|
| N | 13,025 | **6,140** | 7,049 (−13%) |
| mean area ha | 3,980 | **1,093** | 760 (+44%) |
| mean rate 2008 | 21.3% | **43.4%** | 35.7% (+22%) |
| Mha 2008 | 2.65 | **2.29** | 2.0 (+14%) |
| Mha 2014 | 2.91 | **2.48** | 2.2 (+13%) |

Count error went from +85% to −13%.

## Current standing vs Table 1

| quantity | ours | paper | diff |
|---|---|---|---|
| eligible N | 98,941 | 71,171 | +39% |
| eligible mean area | 134.9 | 143 | −6% |
| eligible mean rate | 56.4 | 58.4 | −3% |
| eligible Mha 2008 / 2014 | 6.54 / 6.83 | 5.1 / 5.3 | +28% / +29% |
| eligible Δ | 4.5% | 6.3% | −29% |
| ineligible N | 21,923 | 15,254 | +44% |
| **ineligible mean area** | 674.3 | 661 | **+2%** |
| ineligible mean rate | 20.4 | 11.4 | +79% |
| ineligible Mha 2008 / 2014 | 5.01 / 5.74 | 4.1 / 4.7 | +22% / +22% |
| ineligible Δ | 14.6% | 15.6% | −6% |
| never-elig N | 6,140 | 7,049 | −13% |
| never-elig mean area | 1,093 | 760 | +44% |
| never-elig mean rate | 43.4 | 35.7 | +22% |
| never-elig Mha 2008 / 2014 | 2.29 / 2.48 | 2.0 / 2.2 | +14% / +13% |
| never-elig Δ | 8.5% | 11.5% | −26% |

Areas and rates are close for eligible (−6%, −3%) and the change-over-time figures are
within 6% for ineligible. Counts remain +39%/+44% for the treated groups — the
conflict-resolution surplus, since Table 1 is a POST-resolution table.

## The ineligible rate: possibly an inconsistency in the paper

Table 1's own ineligible column does not reconcile with itself under a ratio-of-means
reading: 4.1 Mha / 15,254 = **268.8 ha** mean deforested area, against a stated mean claim
area of 661 ha, giving **40.7%** — not the stated 11.4%. The same check on the other two
columns is coherent (eligible 71.7/143 = 50.1% vs 58.4% stated; never-eligible
283.7/760 = 37.3% vs 35.7% stated).

A mean-of-ratios far below the ratio-of-means is *possible* for the ineligible group —
it contains many post-2004 arrivals with little clearing alongside a few very large,
heavily cleared parcels — so 11.4% is not necessarily an error. But the gap is a factor of
3.6, versus ~1.2 and ~1.05 for the other groups, which makes the ineligible column the
odd one out. **Worth putting to the authors.** Our own figures show the same qualitative
pattern but milder: mean-of-ratios 20.4% against ratio-of-means 33.9%.

## Would the 0.1% gleba threshold help? Probably not — and it argues our 1% is right

A 0.1% threshold is **looser** than our 1%, so it admits MORE parcels into the target
pool. Our treated counts are already +39%/+44%, so it moves the wrong way. It would also
pull in mostly large parcels that merely clip a gleba boundary, raising the target pool's
mean area — but our eligible (−6%) and ineligible (+2%) areas already match, so we do not
need them.
Given the number of other placeholders and inconsistencies in this draft, Appendix B's
"0.1%" is plausibly a typo for the 1% the legacy code implements. Still worth confirming,
but the evidence now points to 1% being correct.
Note the asymmetry: for the CONTROL side, a **stricter** threshold would help — our
never-eligible mean area is +44%, consistent with huge parcels that only marginally clip a
reserve being counted whole.
