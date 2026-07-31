# Checkpoint 20260801 — empirics replication: state and forward plan

Supersedes `checkpoint-20260729b.md` (whose "one untested lead" is closed).
Full evidence trail: `docs/notes/paper_legacy_method_diffs.md`.

## Standing rule (user, 2026-08-01)

**Use the paper as a guide, but when in doubt replicate what the CODE did.** The
published results came from `legacy_repo/code/2_empirics.R`; the draft may be stale.
Keep the paper's variant behind a flag and report both where it matters.

This resolves the deferred sample question: **2019 basis is the default** (already
`EMP_SAMPLE_YEAR=2019`).

## Where Table 1 stands (stage 2, consolidated)

| | ours | paper | error |
|---|---|---|---|
| eligible N / defo08 / defo14 | 76,592 / 4.972 / 5.156 | 71,171 / 5.1 / 5.3 | +8% / -3% / -3% |
| eligible rate pre-2009 | 53.7 | 58.4 | -8% |
| ineligible N / defo08 / defo14 | 12,021 / 3.209 / 3.580 | 15,254 / 4.1 / 4.7 | -21% / -22% / -24% |
| ineligible rate 2005 / pre-2009 | 12.0 / 16.7 | 11.4 | +5% / +46% |
| never-elig N / defo08 / defo14 | 6,855 / 2.003 / 2.189 | 7,049 / 2.0 / 2.2 | -3% / +0% / -1% |
| never-elig rate pre-2009 | 36.3 | 35.7 | +2% |

Note the ineligible N of 12,021 is the most aggressive corner of the semantics grid;
the defensible band is 14.5k-14.9k (-2% to -5%). Switching stage 2 to a
`winner_vanishes=FALSE` decision set is a one-line change (see plan item 2).

## DiD (stage 3, with the legacy options on)

| comparison | outcome | ours | paper |
|---|---|---|---|
| eligible vs never-elig | legacy-forest | **-1.476** (se 0.749) | -1.412 (se 0.558) |
| ineligible vs never-elig | legacy-forest | +5.429 (se 1.353) | +4.204 (se 0.886) |

## What is CLOSED (do not re-litigate)

Found and implemented: F1 pre-2009 averaging - F2 2019 sample raster - F3
reserve-only control cleaning - N0 drops must reach the sample - N1/N2/N4/N6
as-executed conflict semantics - P1 2005-basis ineligible filter - D-A zero-2014
drop - D-B winsorized DiD outcome.

Measured and ELIMINATED: muni-straddle - Pará double-processing - microdata-join
loss - S2 declared-vs-geometric denominators (8/36 parcels) - D6/P2 target erasure
(zero effect; CNFP filters are disjoint) - pool membership generally (all 801,813
CARs scored) - conflict-semantics grid (ceiling 14,488 ineligible; seeds ±20;
require_j_alive inert).

Irreproducible legacy-side artifacts that own the residual: **N5** (typo'd `slice()`
bounds leave rows 100,001-149,999 unscored and multiply-count rows ≥150,000), the
**63k slippage** their appendix concedes, **D-C** (Table 1 and Table 2 built from
different vintages; the DiD control is the raw uncleaned pool), unseeded randomness.

## Forward plan, ranked

1. **L4 — unmix the cleaning bases (ours, internal inconsistency).** Stage 2 applies
   erasure from the 2014-rule run (`erasure_adjustment.csv`, stage 4/4b) but drops
   from the 2004-rule run (stage 12). Re-run 4b off the 2004-rule decision set so
   rates, areas and drops share one basis. Nothing else is trustworthy until this is
   consistent.
2. **Adopt the code's conflict semantics end-to-end and retire stage 4 (L6).** Fold
   stage 12/14's resolution into the production stage with the semantics switchable;
   pick the decision set by the code reading, and report the 14.5k-14.9k band.
3. **L5 — give the DiD the code's control pool.** Legacy's DiD control is the raw
   >1%-overlap reserve pool (no occupation filter, no cleaning). Under the standing
   rule this is what stage 3 should use for the Table 2 comparison; keep the current
   Table-1-style sample behind a flag. This is the last known reason our coefficients
   are only approximately benchmark-comparable.
4. **L1/L2/L3 — switch to the code's tests.** Occupation = 2004 LEVEL `> 10` (not
   first-crossing); area cap on GEOMETRIC area; strict `>` at every 10% threshold.
   Measured sizes: 3,892 / 121 / 53 parcels. All three currently follow the paper.
5. **Extend beyond Table 1-2.** Tables 3-6 (SUTVA/fines, land prices, moral hazard)
   have never been replicated. Table 3 needs the fines/DETER/cloud data; Tables 4-6
   need the VTN/lavoura/VNP price track, which is blocked on the `vtn_YYYY.rds`
   price tables (see the 2026-07-29 checkpoint).
6. **Optional — N5 sensitivity.** We cannot reproduce their row order, but we can
   bound the effect by scoring a random 100k-150k band out of our pool and
   re-running, to show how much of the eligible surplus it could account for.
