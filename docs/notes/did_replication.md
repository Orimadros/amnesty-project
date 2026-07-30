# DiD replication — first estimates vs the paper

Date: 2026-07-29
Produced by `code/01_build/06_empirics/3_did_estimates.R` on the 2005-2014 window
(1,270,040 parcel-years, 127,004 parcels, 9 states).

## Headline: the paper's central result reproduces

Paper §3.1 eq. (2): `y_it = beta * x_i * 1{t>=2009} + upsilon_i + upsilon_t + eps_it`,
parcel and year FEs, clustered at state level.

| comparison | outcome | our beta (p.p.) | se | p | paper |
|---|---|---|---|---|---|
| eligible vs never-eligible | legacy-forest rate | **−1.406** | 0.928 | 0.168 | **−1.4** |
| ineligible vs never-eligible | legacy-forest rate | **+10.361** | 1.728 | 0.0003 | **+4.2** |
| eligible vs never-eligible | claim-area rate | −1.736 | 1.218 | 0.192 | −1.4 |
| ineligible vs never-eligible | claim-area rate | +17.724 | 4.832 | 0.006 | +4.2 |

**The eligible coefficient is essentially exact: −1.406 against the paper's −1.4.** In
relative terms too: ours is −1.406 on a pre-2009 mean of 53.7% = −2.6%; the paper's is
−1.4 on 58.4% = −2.4%.

**The qualitative finding — the paper's actual contribution — holds.** A small decrease in
deforestation among the forgiven, and a much larger increase among those excluded who
plausibly anticipated a future amnesty. Signs match on both regressions and the asymmetry
is large in the same direction.

**The ineligible magnitude is ~2.5x too big** (+10.4 vs +4.2). Consistent with the
unresolved-boundary surplus: our ineligible group is +44% on count and its mean rate runs
20.4% vs the paper's 11.4% (see `table1_comparison_findings.md`). Both inflate a
level-scaled coefficient.

## Which denominator? The evidence says legacy forest, not claim area

Table 1's note says the rate is "the share of a property claim's area that has been
deforested". Taken literally that means deforested / declared area. But that outcome is
badly behaved on our data:

- pre-2009 mean for the eligible group is **1,036%** — impossible as a share. Declared
  `NUM_ARE` is frequently far smaller than the polygon's actual deforested extent, so the
  ratio explodes.
- uncapped, those outliers dominate: the ineligible coefficient inflates to +17.7.

The legacy-forest denominator behaves sensibly and matches the paper better on BOTH the
level and the coefficient:

| | legacy forest | claim area | paper |
|---|---|---|---|
| eligible pre-2009 mean rate | 53.7% | 1,036% | 58.4% |
| eligible beta | −1.406 | −1.736 | −1.4 |

So either the authors compute claim area from geometry rather than the declared field, or
their rate really is over legacy forest. Given Appendix C defines the *occupation* test
over legacy forest, the latter is plausible. **We use legacy forest as primary and flag
the discrepancy with the table note.**

## Estimator note

`lfe` and `fixest` are not in renv.lock, and `lfe` will not build on this host — it needs
a Fortran toolchain (`/opt/gfortran`) that is absent, so the CLAUDE.md package dance fails
at link time. Rather than extend the reproducibility contract or install a system
toolchain, the two-way FE estimator is implemented directly in
`_helpers_twfe.R` using base R + data.table (both pinned): Frisch-Waugh-Lovell with
alternating-projection demeaning, and CR1 cluster-robust SEs.

It ships with a self-test that recovers a known coefficient from synthetic data with
strong unit and time effects (true 2.500, estimated 2.463) and the script aborts if the
test fails. Point estimates are exact; the SE degrees-of-freedom correction is standard
CR1 and may differ slightly from `felm`'s.

## Caveats

- Runs on **unresolved boundaries**. The paper states (§2.3) results "carry if we used
  property boundaries without any adjustment", which is exactly what this tests — and they
  do.
- The eligible coefficient is **not statistically significant** at conventional levels
  (p = 0.17, 9 state clusters). The paper does not report its se, so we cannot tell whether
  this differs from theirs. With only 9 clusters, cluster-robust inference is fragile
  regardless.
- Levels remain high pending conflict resolution.

## Next

1. Conflict resolution should shrink the ineligible coefficient toward +4.2 — the single
   sharpest test of that stage.
2. Ask the authors which denominator Table 1 uses, given the claim-area rate is degenerate
   on declared areas.

---

# Second pass (2026-07-29) — after conflict resolution

Conflict resolution ran (24,121 parcels dropped) and the DiD was re-estimated on the
resolved set. **It fixed the counts and barely moved the coefficients.**

## Counts: large improvement

| group | unresolved | resolved | paper | resolved diff |
|---|---|---|---|---|
| eligible | 98,941 | 81,404 | 71,171 | +14% (was +39%) |
| ineligible | 21,923 | 16,571 | 15,254 | **+9%** (was +44%) |
| never-eligible | 6,140 | 4,908 | 7,049 | **−30%** (was −13%) |
| total | 127,004 | 102,883 | 93,474 | +10% (was +36%) |

The stage removed ~72% of the surplus — more than the "at most half" estimated in
`count_surplus_analysis.md`, though it still does not close the gap, as predicted.

## Coefficients: essentially unchanged

| comparison | unresolved | resolved | paper |
|---|---|---|---|
| eligible (legacy-forest rate) | −1.406 | **−1.708** | −1.4 |
| ineligible (legacy-forest rate) | +10.361 | **+9.788** | +4.2 |
| eligible (claim-area rate) | −1.736 | −2.153 | −1.4 |
| ineligible (claim-area rate) | +17.724 | +11.865 | +4.2 |

**The prediction that conflict resolution would pull the ineligible coefficient toward
+4.2 was wrong.** It moved from +10.4 to +9.8 — a 6% change against a gap of 2.5x. The
eligible coefficient moved slightly AWAY from the paper (−1.41 to −1.71).

## What this establishes

The parcel surplus and the coefficient gap are **independent problems**. Removing 24,121
duplicate claims corrected the counts without materially changing the estimated effects,
which means the remaining coefficient discrepancy is not a sample-size artefact.

The driver is the persistent **ineligible rate level**: ours is 21.6% pre-2009 against the
paper's 11.4%, essentially unchanged by resolution (was 20.4%). A treated group whose
outcome runs ~2x the paper's will produce a proportionally larger coefficient on the same
underlying response. Closing the coefficient gap therefore requires explaining the rate
gap — which remains the single unresolved discrepancy, now surviving:

- the occupation-definition fix (first-crossing vs level test)
- the denominator question (legacy forest vs claim area)
- declared vs geometric area
- the control-group sample-filter bug
- and now conflict resolution

## New concern: the control group

Never-eligible went from −13% to **−30%** against the paper. Conflict resolution dropped
1,232 control parcels, and the control group is now the worst-fitting of the three. Since
both regressions are estimated against this group, its composition directly scales both
coefficients. Worth checking whether the algorithm should be applied to control parcels at
all, or only within the treated pool — the paper does not say.

## Still true

Both signs match, and the qualitative asymmetry — a small decrease among the forgiven, a
much larger increase among the excluded — holds under every specification tried.
