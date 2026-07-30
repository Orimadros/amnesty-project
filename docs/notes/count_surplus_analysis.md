# Why do we have 33,530 more parcels than the paper?

Date: 2026-07-29
Status: open. Conflict resolution has been RULED OUT as a sufficient explanation.

| group | ours | paper | diff |
|---|---|---|---|
| eligible | 98,941 | 71,171 | +27,770 |
| ineligible | 21,923 | 15,254 | +6,669 |
| never-eligible | 6,140 | 7,049 | **−909** |
| **total** | **127,004** | **93,474** | **+33,530** |

Note the asymmetry: the two TREATED groups are ~+40% while the CONTROL group is
**−13%**. Any explanation has to produce that pattern, not just "too many parcels".

## Ruled out #1: conflict resolution cannot close this

Appendix C step 2 sets aside "properties without any overlaps with other properties
(< 10% of property area)" — the algorithm only ever touches parcels whose overlap
exceeds 10% of their OWN area. Measured on our in-sample set:

| overlap threshold | pairs | parcels | share of sample |
|---|---|---|---|
| > 0% | 139,605 | 71,007 | 55.9% |
| > 1% | 71,129 | 46,309 | 36.5% |
| > 5% | 47,656 | 34,096 | 26.8% |
| **> 10%** | **39,465** | **29,564** | **23.3%** |

So **97,440 parcels are set aside untouched**, and the algorithm can drop at most
29,564 — and realistically far fewer, since each resolved pair drops one side, some
parcels appear in several pairs, and the "0+0 overlap" rule keeps BOTH parcels
(it only erases the shared area).

**29,564 maximum < 33,530 surplus.** Conflict resolution cannot be the whole story,
and probably accounts for at most half of it. This overturns the working assumption
carried through the previous three passes.

It also revises the earlier "85.9% of parcels are in an overlap" framing: true, but
only 23.3% are in an overlap big enough for the algorithm to act on.

## Ruled out #2: cancelled / suspended CARs

The paper discusses cancelled CARs at length (Appendix B, §A.1) so it was plausible
they are excluded. Status counts in our in-sample set (SITUACA):

| status | eligible layer | control layer | total | share |
|---|---|---|---|---|
| AT (active) | 65,762 | 1,695 | 67,457 | 53.1% |
| PE (pending) | 50,149 | 3,328 | 53,477 | 42.1% |
| CA (cancelled) | 3,875 | 905 | 4,780 | 3.8% |
| SU (suspended) | 1,078 | 212 | 1,290 | 1.0% |

Dropping cancelled AND suspended removes only **6,070** — well short of 33,530, and
it would push the control group further below the paper's count.

## Still open — leading candidates

**(a) Group-assignment PRECEDENCE differs.** Appendix B's recipe is ordered:
```
2. CARs with more than 0.1% of their area inside a federal gleba are "dirty" [treated]
3. For CARs OUTSIDE federal glebas, [...] inside reservation or indigenous areas [control]
```
So gleba membership is tested FIRST and wins; only parcels outside federal glebas can
become control. Our scaffold inverts this — `ineligible_flag = control_overlap > 1%`
is evaluated first and `eligible_flag = !ineligible_flag & target_overlap > 1%`, so a
parcel overlapping BOTH a gleba and a reserve is assigned to the CONTROL group where
the paper would call it treated.
Direction check: fixing this moves parcels control -> treated, which would make our
control smaller still and our treated larger. That is the WRONG direction for the
totals, but it is a genuine structural difference and should be corrected regardless.

**(b) The state-gleba step is missing entirely.** Appendix B step 3 requires control
parcels to be outside federal glebas AND **not in state glebas**. Our scaffold has no
state-gleba layer at all — we only use CNFP federal land. Parcels sitting in state
glebas should be excluded from the control group (and are not "pure control" per step
4). This could plausibly explain the control-group composition problems (our mean area
was 5.2x too large before the occupancy fix, and is still +44%).

**(c) The 0.1% vs 1% gleba threshold.** Looser admits MORE parcels, so it worsens the
treated surplus. Still unresolved, but it cannot explain a surplus.

**(d) Something upstream in the target pool.** 164,223 parcels carry >1% target
overlap before any occupancy filter. If the paper's target pool is materially smaller,
the difference originates in the CAR scaffold, not in the empirics stages.

## Next diagnostic

Test (a) and (b): rebuild the group assignment with gleba-first precedence, and check
whether a state-gleba layer exists in `data/input/` (terrabrasilis or CNFP). That is
cheap relative to re-running the scaffold's overlap scoring.
