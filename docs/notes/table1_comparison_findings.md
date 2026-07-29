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
