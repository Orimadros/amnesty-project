# Systematic diff: our pipeline vs the legacy code, from the divergence point

Date: 2026-07-29
Method: traced the ineligible group — where the residual discrepancy lives — backwards
through `2_empirics.R` and compared each step against our stages 1-4.

Six differences found. Three are new this pass, and two of them are strong candidates.

---

## D1 — ORDER OF OPERATIONS (NEW, and the strongest candidate)

**Legacy sequence:**
```
ccar_dirty (in glebas)
  -> active2014 filter (rate > 10 in 2014)          [line 636]
  -> CONFLICT RESOLUTION -> car2014_all             [lines 731-1170]
  -> active2004 (rate > 10 in 2004) + area<=1500 + defo<=1500 -> car2004 = ELIGIBLE  [1228,1290,1310]
  -> car_inelegible = car2014_all MINUS car2004      [line 1370]
```

**Our sequence:**
```
target pool -> occupancy filter -> ELIGIBILITY SPLIT -> conflict resolution
```

**Legacy resolves conflicts BEFORE classifying; we classify first and resolve after.**

This matters because resolution *modifies geometry*: the 0+0 rule erases the shared area
from one side. Legacy's `area <= 1500` and `active2004` tests therefore run on **shrunken**
polygons, while ours run on full ones. A parcel just over 1,500 ha that loses its overlap
becomes eligible in legacy and stays ineligible for us.

Combined with D2, this is the most plausible remaining explanation for the composition gap.

## D2 — THE ERASURE IS NOT APPLIED (issue #C6, now quantified — NEW)

Our stage 4 *records* `erase_intersection_random` decisions but does not rewrite geometry.
Measured:

- **10,914 in-sample parcels** flagged for erasure — 10.6% of the resolved sample
- mean overlap to erase: **438.5 ha** against a mean legacy-forest area of **626.2 ha**
- i.e. those parcels should lose **~70% of their area**, and we leave them whole

Legacy's own comment at line 1855 is explicit: *"SO THE CLEANING ONLY REMOVES OVERLAPS TO
AVOID DOUBLE COUNTING."*

Combined with D1, erasure feeds the `<=1500` test, so it changes *classification*, not just
levels. **This is the single largest un-implemented piece of the legacy pipeline.**

## D3 — A MISSING FILTER ON THE INELIGIBLE GROUP (NEW, tested)

`2_empirics.R:1704`:
```r
inelegible <- inelegible %>% filter(!is.na(area) & area < 100000)
```
We have no equivalent. Tested on our resolved set:

| | ours now | + legacy filter | paper |
|---|---|---|---|
| ineligible N | 16,571 | **14,422** | 15,254 |
| mean rate 2008 | 20.76% | 23.84% | 11.4% |

The `area < 100000` clause is nearly inert (only **7** parcels), but `!is.na(area)` drops
**2,142** parcels whose rate is zero. Net: the count fit improves from **+9% to −5%**, but
the rate moves *away* from the paper. So D3 is real and should be implemented for
fidelity, but it is **not** the cause of the rate gap — it worsens it.

## D4 — INELIGIBLE IS DEFINED BY SUBTRACTION, NOT BY PREDICATE

Legacy: `car_inelegible <- car2014_all %>% filter(!COD_IMO %in% car2004$COD_IMO)`.
Ours: a predicate, `!(occupied_by_2004 & small)`.

Logically equivalent *given identical inputs* — but the inputs are not identical because of
D1/D2. Worth restructuring to subtraction so the two cannot drift apart.

## D5 — LEGACY'S `area` IS THE LEGACY-FOREST AREA, NOT THE CLAIM AREA (NEW)

Legacy derives it by inverting the rate:
```r
control$area <- as.numeric(control$deforested_area_hc)/(as.numeric(control$deforestation_rate)/100)
```
So `area` = deforested / (rate/100) = **the rate's denominator**. This independently
confirms the denominator is legacy forest, not declared claim area — settling a question
that Table 1's note ("share of a property claim's area") had muddied.

It also means **Table 1's "Property Area (ha)" row may be legacy-forest area**, not
declared area. Both of ours are close, so this does not discriminate:

| ineligible mean area | ours | paper |
|---|---|---|
| legacy-forest area | 626.2 | 661 (−5%) |
| declared claim area | 674.3 | 661 (+2%) |

## D6 — WE ERASE CONTROL AREAS FROM TARGET AREAS; LEGACY DOES NOT

Our scaffold line 185 runs `st_erase(target_areas, control_areas)`. Legacy has exactly
that, **commented out** (lines 113-114). Shrinks our target pool, so it points away from
our surplus — but it is a genuine infidelity.

---

## Answered along the way

**Does conflict resolution apply to the control group? YES.** Legacy runs a separate
cleaning loop over `active2014_inReserva_CAR_*` producing `active2014_inReserva_cleaned_CAR*`
(lines 1841, 1955). Our applying it to control is correct — that question is closed.

## New hypotheses, ranked

1. **D1 + D2 together — resolution before classification, with erasure applied.** ~10.6% of
   parcels should shrink by ~70%, and that happens *before* the `<=1500` test. This can move
   parcels between eligible and ineligible, which is exactly the kind of compositional change
   the rate gap needs. **Untested — requires implementing #C6 and reordering the stages.**
2. **D3 — the missing ineligible filter.** Tested: fixes the count (+9% → −5%), worsens the
   rate. Implement for fidelity; not the answer.
3. **D5 — Table 1's area row may be legacy forest.** Does not discriminate on our data.

## What would falsify hypothesis 1

Implement erasure, move resolution ahead of the split, re-run. If the ineligible mean rate
does not fall from ~20.8% toward 11.4%, then no code difference we have found explains the
gap, and the remaining explanation is in Table 1's own arithmetic — which already fails to
reconcile (4.1 Mha / 15,254 over 661 ha implies ~40%, not 11.4%).

---

# RESULT (2026-07-29): hypothesis D1+D2 is FALSIFIED

Implemented all three differences and re-ran:
- **D2**: stage 4 now records the erasure partner; new **stage 4b** rebuilds each erased
  region and measures its deforestation for all 28 years; stage 2 subtracts it.
- **D1**: stage 2 applies that adjustment BEFORE the `<=1500 ha` and occupied-by-2004
  tests, reproducing legacy's order. `in_sample` correctly still uses the PRE-erasure
  2014 rate, since legacy's active2014 filter runs before cleaning.
- **D3**: legacy's `filter(!is.na(area) & area < 100000)` on the ineligible group.

## First: a correction to the D2 sizing

I previously estimated the erasure at "438.5 ha, ~70% of parcel area". **That was wrong
by an order of magnitude.** It summed *every* overlap for a flagged parcel, including
pairs that were never erased. Measured properly, from the union of only the erasure
partners:

**mean erased area = 43.5 ha**, against a mean parcel area of ~626 ha — about **7%**,
not 70%. Mean erased deforestation: 13.2 ha (2004), 17.8 (2008), 20.5 (2014).

## What changed

| ineligible | before | after D1+D2+D3 | paper |
|---|---|---|---|
| N | 21,923 | 19,113 | 15,254 (+25%, was +44%) |
| **mean rate 2008** | 20.4% | **23.5%** | 11.4% (**+106%, was +79%**) |
| deforested Mha 2008 | 5.009 | **4.444** | 4.1 (**+8%**, was +22%) |
| deforested Mha 2014 | 5.741 | 5.048 | 4.7 (+7%, was +22%) |
| mean area ha | 674.3 | 682.6 | 661 (+3%) |

| DiD (legacy-forest rate) | before | after | paper |
|---|---|---|---|
| eligible beta | −1.708 | −1.742 | −1.412 |
| **ineligible beta** | +9.788 | **+9.502** | **+4.204** |

## Verdict

**The totals improved substantially** — ineligible deforested area went from +22% to
**+8%**, and mean area is within 3%. **The rate got worse**, from +79% to +106%, and the
coefficient barely moved (+9.79 → +9.50 against a target of +4.20).

This is the falsification I pre-registered: *"if the ineligible mean rate does not fall
from ~20.8% toward 11.4%, then no code difference we have found explains the gap."*
It did not fall. It rose.

**No code difference between our pipeline and the legacy code explains the ineligible
rate gap.** Every structural candidate has now been implemented or eliminated:

| candidate | outcome |
|---|---|
| occupation level vs first-crossing | fixed, moved counts not rates |
| rate denominator | tested, 20.4 vs 20.0 — no effect |
| declared vs geometric area | tested, moves 12 parcels |
| control-group sample filter | fixed, large improvement to control |
| conflict resolution | implemented, fixed counts not coefficients |
| assignment precedence | matches legacy (Appendix B is wrong) |
| gleba threshold | matches legacy (Appendix B is wrong) |
| CNFP vintage / layer definitions | match exactly |
| erasure + ordering (D1+D2) | implemented, rate moved AWAY |
| ineligible area filter (D3) | implemented, rate moved AWAY |

## What remains

Table 1's ineligible column does not reconcile with itself. With the paper's own
figures — 4.1 Mha over 15,254 parcels of mean claim area 661 ha — the implied rate is
**~40%**, not the stated **11.4%**. The eligible and never-eligible columns are both
internally coherent under the same check (50.1 vs 58.4; 37.3 vs 35.7).

Our post-D1/D2/D3 ineligible deforested area (4.444 Mha) and mean area (682.6 ha) now
both sit within 8% of the paper's. It is specifically and only the **rate** that
disagrees — which is the one number in that column that cannot be derived from the
others.

**Recommendation:** stop searching the code. Ask the authors how Table 1's 11.4% and
Table 3's 11.4% outcome baseline were computed, because our reconstruction agrees with
every other number in that column.
