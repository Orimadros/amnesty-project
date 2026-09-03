# Specs recovered from the paper's appendices

Date: 2026-07-29
Source: `amnesty_wp.pdf`, Appendix B (Data and Variable Definitions) and
Appendix C (Spatial Resolution Algorithm).

These appendices are the authoritative spec for the two stages still to be ported.
They are more precise than the legacy code comments — prefer them.

## 1. Occupation: the definition, and how our first attempt differed

**Paper (§2.3):**
> "the first year when the share of deforested area in a rural parcel reaches 10% is
> assigned as the beginning of human occupation. We use 10% since for very small areas a
> lower share could be affected by noise in satellite readings, though marginally changing
> our threshold has little effect on our point estimates."

**Paper (Appendix C):**
> "We define a parcel as occupied in year t if at least 10% of its **legacy forest** has
> been deforested."

So occupation start = `min{ year : deforested_share(year) >= 10% }`, with the denominator
being *legacy forest*.

**What we got right:** the 10% threshold, and the denominator. Our `valid_px`
(raster value != 0) IS the legacy forest — MapBiomas step 3 codes 0 for pixels that were
never legacy forest, so value in {1,2,3} is exactly the legacy-forest set.

**What we got wrong (issue #E1):** `2_eligibility_split.R` used a *level* test,
`rate_2004 > 10`, rather than a *first-crossing* test. The two agree whenever clearing
only accumulates, but diverge under reforestation. A parcel that reached 15% by 1998 and
then regrew to 8% by 2004 is:
  - paper: occupied since 1998 -> **eligible**
  - our level test: 8% in 2004 -> **ineligible**
The paper reports 8 Mha reforested against 52 Mha deforested (~15% of cleared land
regrew at some point), so this is not a rare edge case. It biases in exactly the observed
direction: small parcels misrouted into ineligible, pulling its mean area down and its
mean rate up (see `table1_comparison_findings.md`).

**Fix:** extract every year 1987-2004 and take the first year the share reaches 10%.

**Subtlety worth recording:** raster value 3 = reforested, which implies the pixel *was*
deforested earlier. So `count(value in {2,3})` at 2004 is an alternative "ever deforested
by 2004" measure, computable from a single year. It is not identical to the
first-crossing-over-years test (the reforestation rule requires 2 consecutive deforested
years then 3 forest years). We implement the first-crossing test, since that is what the
paper's text specifies, but the single-year variant is a cheap cross-check if wanted.

**Second, smaller difference (issue #E2):** Appendix C step 1 defines the ever-occupied
sample as *"Drop properties with < 10% of deforested area in 2014"* — a level test at
2014. Our `in_sample` used `occupied_2014 | occupied_2004`, which retains parcels the
paper drops.

## 2. Spatial conflict resolution — the full spec (Appendix C)

```
1. Drop properties with < 10% of deforested area in 2014
2. Set aside properties without any overlaps with other properties (< 10% of property area)
3. Apply the following spatial conflict resolution algorithm:
   - if i contains j (overlap > 90%) and > 80% of i's deforested area is in j        -> drop i
   - if i contains j and > 80% of its deforested area is in i                        -> drop j
   - if i intersects j (overlap < 90%) and > 80% of i's deforested area is in the
     intersection, but not for j                                                     -> drop i
   - if i intersects j and > 80% of BOTH deforested areas are outside the
     intersection -> keep both, and randomly remove the intersection area from i or j
   - if i intersects j and > 80% of BOTH deforested areas are in the intersection
                                                                    -> randomly keep i or j
4. Combine adjusted properties with those set aside in step 2.
```

Two details this pins down that the legacy comments did not:
- **"No overlap" means overlaps totalling < 10% of property area**, not zero overlap.
  Those parcels bypass the algorithm entirely.
- Containment is **overlap > 90%**, and the deforested-area tests are all at **80%**.

Reproducibility note: two of the five rules involve a random choice. They need a fixed
seed; the original run used whatever seed R had, so those cases can only ever match in
distribution, not bit-identically.

## 3. Conflict resolution may not gate the results (§2.3)

> "Though implied magnitudes change, all our results carry if we used property boundaries
> without any adjustment (see Section 3.3.3)."

The authors state the DiD conclusions survive without the adjustment — only magnitudes
move. So the regressions can be reached before paying for that stage, using the
unresolved boundaries, with levels known to run high.

## 4. Definitional discrepancy to confirm with the authors

Appendix B step 2:
> "CARs with more than **0.1%** of their area inside a federal gleba are 'dirty', the
> others 'clean'"

Our scaffold uses **1%** (`target_overlap_share > 0.01`), and legacy
`2_empirics.R:497` reads `filter(overlap*100 > 1)` — which is >1% if `overlap` is a
fraction, or >0.01% if it is already a percentage. There is a 10x ambiguity here. Note
the direction: a looser 0.1% threshold admits MORE parcels, so it does not explain our
current surplus — but it does change group membership.

## 5. Cross-checks that validate our rebuild

| quantity | paper | ours | note |
|---|---|---|---|
| CARs in the Amazon biome | 892,670 | 892,630 | 40 apart — exactly the difference the CAR migration already documented vs the Dropbox snapshot |
| CARs intersecting >=1 other | 86% | 85.9% | match |
| unique CAR conflicts | 1,380,510 | 1,246,945 | ~10% low, consistent with dropping the unreproducible v1-v4 archives (CAR issue #15) |
| cancelled CARs | 32,553 (3.5%) | — | not yet checked |

Also from Appendix B, useful for validating the conflict stage: conditional on having at
least one intersection, a CAR overlaps **3.6 times** on average, with each intersection
about **22.73%** of its area.
