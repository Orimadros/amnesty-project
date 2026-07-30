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
