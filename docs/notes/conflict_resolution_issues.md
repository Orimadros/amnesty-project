# Conflict-resolution port: deviations and open questions

Date: 2026-07-29
Implements: `code/01_build/06_empirics/4_conflict_resolution.R`
Spec: paper Appendix C. Legacy counterpart: `2_empirics.R:731-1170`.

The legacy code and Appendix C describe the same five rules; I verified them line by
line. Differences below are implementation, not logic — except #C4, which is a genuine
choice.

### #C1 — pairs come from CAR stage 03b, not recomputed (OPTIMIZATION)
Legacy ran `st_intersection(car, car)` for every parcel in a municipality inside the
loop — the pattern that made CAR stage 03 produce 10-hour municipalities. We already
have every pair and its intersection AREA in `CAR_overlap_variables_conflicts.csv`, so
we build intersection GEOMETRY only for pairs that clear the 10% gate: **46,950 of
139,612 pairs**. Same pairs, a third of the geometry work.

### #C2 — the 10% gate is applied symmetrically (DEVIATION, deliberate)
Legacy filtered `pct_intersect_i > .1`, i.e. the overlap as a share of **i's** area
only, relying on the all-pairs intersection producing both (i,j) and (j,i) so each
direction got its own test. We deduplicate pairs, so the gate is
`pct_a > 10% OR pct_b > 10%` — a pair is examined if the overlap is material to
*either* side. Equivalent in coverage to legacy's two-directional test, without
processing each pair twice.

### #C3 — dropped parcels are honoured within a municipality (DEVIATION, fixes a latent bug)
Legacy accumulated drop decisions into `pdfs2`/`pdfs3` vectors and applied them after
looping every pair, so a parcel already slated for removal could still act as the
surviving side of a later pair. We skip any pair whose members are already dropped.
This makes the result order-dependent in principle; pairs are processed in a stable
sorted order so it stays deterministic.

### #C4 — deforestation is located with the 2014 raster, not 2004 (DEVIATION — CHECK)
Legacy loads a **2004** raster at line 739 and uses it for every `defo_inter`
computation. Appendix C step 1 defines the sample on **2014** deforestation, and the
rules speak of "its deforested area" without a year. Using 2014 measures where a
parcel's clearing actually sits at the end of the study window, which is what the
ownership question is really asking. Set `CR_YEAR=2004` to reproduce the legacy choice.
**This is the deviation most likely to move results and is worth confirming with the
authors.**

### #C5 — randomness is seeded (DEVIATION, necessary)
Two rules draw at random. Legacy used whatever seed R happened to have, so its output
was never reproducible even to itself. We fix `set.seed(20260729)`. Consequence: our
result is reproducible, but for the affected pairs it can only match the original in
distribution, never row-for-row.

### #C6 — "erase intersection" is recorded, not yet applied (INCOMPLETE)
The 0+0 overlap rule keeps both parcels and erases the shared area from one at random.
We record that decision (`erase_intersection_random`) but do not currently rewrite the
geometry, because the downstream consumers (`parcels_resolved_*.csv`, the DiD) key on
parcel identity rather than shape. Erasing would change the loser's deforested area and
so its rate. **This must be implemented before the resolved geometries are used for
anything area-based.** Tracked as the next step.

### #C7 — planar overlay (INHERITED)
Runs under `sf_use_s2(FALSE)`, matching the legacy 2_empirics workflow and CAR issue
#26, which found s2 rejects the near-degenerate slivers constructive overlay produces
here.

## Sizing note

Only **29,564 of 127,004 in-sample parcels (23.3%)** have an overlap exceeding 10% of
their own area, so 97,440 are set aside untouched by step 2. The algorithm therefore
cannot drop more than ~29.5k parcels, which is BELOW our 33,530 surplus versus the
paper — see `count_surplus_analysis.md`. Expect this stage to reduce the gap
substantially but not to close it.
