# Exhaustive sweep of the paper's tables and appendices

Date: 2026-07-29
Scope: all 5 main tables, all 9 figures and their notes, Appendices A-D, §3.3.x.

Several open questions are answered, including the top one — and the answer is that
**the authors cannot explain it either.**

## 1. ANSWERED — what reduces 892,670 CARs to 829,260

Appendix B, **footnote 15**:

> "Of the 559 municipalities in the Amazon biome, 557 municipalities have been fully
> processed. Within these processed municipalities, we observe **891,234** unique CARs
> in the raw data-files and successfully process **829,260** CARs. **Additional work
> needs to be done to understand this slippage.**"

So the ~62,000-CAR gap (7%) is an **acknowledged, unexplained loss in the authors' own
pipeline**. This was our #1 question for Pedro; the paper pre-emptively answers it with
"we don't know".

Implications:
- A meaningful part of our parcel surplus is not our error — our pipeline processes
  essentially every CAR (892,630 at stage 00), theirs silently loses 7%.
- Their processed universe is 829,260 against our ~892,630, a ratio of 0.93. Our
  post-resolution total is +10% over their Table 1 — the same order as this slippage.
- They processed **557 of 559** municipalities; our CAR chain did **558**.

## 2. NEW — Table 3 gives the actual coefficients, standard errors, and N

I had been comparing against prose ("approximately 2%", "an estimated β of 1
percentage point", "4.2"). The real table:

| | (1) Property+Year | (2) Property+Year×State | (3) Property+Year×Muni |
|---|---|---|---|
| Amnesty Eligibility | **−1.412** (0.558) | −0.844 (0.325) | −0.829 (0.304) |
| Amnesty Expectation | **4.204** (0.886) | 4.047 (0.815) | 3.744 (0.662) |
| Outcome baseline | 58.4% | | 11.4% |
| Estimated effect | −2.4% / 36.9% | −1.4% / 35.5% | −1.4% / 32.8% |
| Observations | 782,175 (elig) | | 231,833 (inelig) |

Column (1) is exactly our specification. **Our unresolved estimate was −1.406 against
their −1.412 — a 0.4% difference.** The resolved estimate (−1.708) is further away.

Our standard errors run ~1.7-2x theirs (0.928 vs 0.558; 1.750 vs 0.886), which is
consistent with the hand-rolled CR1 dof correction being more conservative than
`felm`'s, and with our larger samples.

Observation counts reconcile with Table 1: 782,175 / 10 years = 78,218 parcels ≈
eligible 71,171 + never-eligible 7,049 = 78,220. So the regressions do run on Table 1's
samples over a balanced 10-year panel.

Against our samples:
| regression | ours unresolved | ours resolved | paper |
|---|---|---|---|
| eligible | 1,050,810 | 863,120 (+10%) | 782,175 |
| ineligible | 280,630 | 214,790 (**−7%**) | 231,833 |

After resolution our ineligible regression sample is **below** the paper's.

**New robustness target:** columns (2)/(3) and (5)/(6) add state-by-year and
municipality-by-year fixed effects. The eligibility effect roughly halves
(−1.412 → −0.83) while the expectation effect barely moves (4.204 → 3.744). We can run
both — a sharper test than the baseline alone.

## 3. ANSWERED — cancelled CARs are NOT excluded

Appendix B section C: *"Since CAR cancellations are tough to enforce and have been
observed to always have at least one conflicting land claim with another CAR, **we
include cancelled CARs into our calculations**."* Confirms our choice; closes that
candidate for good.

## 4. Appendix B section C describes the pipeline we already migrated

Its variable list — `car_union_area`, `car_area_intersect_indi`,
`car_area_intersect_conserve`, `car_area_intersect_forest{A,B,C}`,
`car_area_cancelled`, `car_area_notcancelled`, `year`, `municipio` — is exactly the
output of our CAR stage 04 (`muni_year_intersections.csv`). Independent confirmation
that the CAR migration targeted the right artefact.

## 5. §3.3.3 is an unwritten placeholder

> "Short paragraph detailing robustness (different occupation threshold, using CARs
> without the spatial cleaning etc) from table in Appendix."

So the §2.3 claim that *"all our results carry if we used property boundaries without
any adjustment (see Section 3.3.3)"* points at text that does not exist, and the
supporting appendix table is absent. **That claim is currently unverified in the
draft** — worth knowing, since we relied on it to justify estimating before conflict
resolution.

## 6. Figure 4 note — a definitional detail not stated elsewhere

> "Deforestation rates are adjusted for forest regrowth (reforestation) or abandonment."

Our measure counts raster value 2 over legacy forest, and MapBiomas step 3 already
codes a cleared-then-regrown pixel as 3 (reforested), not 2 — so our rate is arguably
already regrowth-adjusted by construction. Flagged rather than actioned: confirming it
would need reforested-pixel counts, which stage 1 does not currently store.

## 7. Figure 3 note confirms our area definitions

> "Only undesignated federal land was targeted [...] State-owned land, forestland
> allocated to indigenous reservations, conservation units, or land designated to any
> other use were excluded [...] Federal land allocated to reservations and conservation
> as the control areas."

Matches our scaffold exactly (target = FEDERAL & SEM DESTINACAO; control = FEDERAL &
UC|TI). Also note it cites "the algorithm described in Appendix B" when the algorithm
is in Appendix C — another cross-reference slip.

## Remaining open

Only one substantive question is left for the authors:

**What exactly is Table 1's 11.4% ineligible baseline?** It still does not reconcile
with its own row (4.1 Mha / 15,254 parcels over a 661 ha mean claim area implies ~40%),
the other two columns are internally coherent, and it drives the entire remaining
coefficient gap. Our post-resolution ineligible pre-2009 mean is 15.22% on the
legacy-forest denominator — closer to 11.4% than the 20-21% figures quoted earlier in
this investigation, because those were 2008 claim-area means rather than the regression
baseline.

Everything else we had queued has now been answered by the paper itself.
