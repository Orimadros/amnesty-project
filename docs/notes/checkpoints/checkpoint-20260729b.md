# Checkpoint 20260729b — empirics replication: where it stands, what to do next

> **RESOLVED 2026-07-30.** The "DO THIS FIRST" test was run (`code/01_build/06_empirics/5_muni_straddle_test.R`): dropping the 7,704 in-sample parcels that geometrically straddle a municipality boundary moves the ineligible mean rate 23.5% -> 23.0% (23.2% under a 1%-of-area threshold). It does NOT fall toward 11.4%. Per the protocol below, the search is closed: the remaining explanation is the paper's own arithmetic. See the 2026-07-30 RESULT section of `docs/notes/code_diff_vs_legacy.md`.


Handoff so a new chat can continue. Point the new session here:
"read docs/notes/checkpoints/checkpoint-20260729b.md and continue."

## TL;DR

- Branch **`integration/car-mapbiomas`**, tip `1e68c9b`, checked out in the MAIN checkout.
  **Nothing is pushed.** `main` is protected (needs @Orimadros review).
- The paper's **DiD result reproduces**. Eligible coefficient −1.742 vs the paper's
  −1.412; both signs correct; the qualitative asymmetry holds under every specification.
- **One number does not reproduce**: the ineligible group's mean deforestation rate,
  ours 23.5% vs the paper's 11.4%. It drives the ineligible coefficient (+9.5 vs +4.2).
- **The immediate next task is a single, well-defined test** — see "DO THIS FIRST".

## DO THIS FIRST — the one untested lead

Legacy `2_empirics.R:1594-1595` applies two sample filters we do NOT implement:

```r
inelegible %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc) %>%
  filter(row_number(COD_IMO) == 1) %>% ungroup()
inelegible %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)   # <- this one
```

The second **drops any CAR appearing more than once with differing measured values** —
i.e. parcels straddling a municipality boundary, which legacy measures once per
municipality file. We instead sum across raster tiles and keep one combined value, so we
retain those parcels.

A third, smaller one: legacy builds the wide panel by `left_join` starting from the
**2005** file, so 2005 silently defines the sample.

**Test:** identify parcels whose geometry crosses a municipality boundary, drop them,
recompute the ineligible mean rate. If it falls toward 11.4%, that is the answer. If not,
the remaining explanation is in the paper's own arithmetic (below) and the search should
stop.

Do NOT reason about the direction first — two prior direction predictions were wrong.
Measure it.

## Why the gap probably is not ours

Table 1's ineligible column does not reconcile with itself. Apply "total deforested ÷
(N × mean area)" to each column:

| group | stated rate | implied by its own totals | ratio |
|---|---|---|---|
| eligible | 58.4% | 50.1% | 1.17 |
| never-eligible | 35.7% | 37.3% | 0.96 |
| **ineligible** | **11.4%** | **40.7%** | **0.28** |

Our equivalents: eligible 1.16 (essentially identical to their 1.17), ineligible 0.69.
So we reproduce their *internal relationship* for the eligible group but not for the
ineligible one — and the ineligible column is the only one where their own five numbers
disagree.

Everything else in that column now matches: deforested area +8%, mean area +3%, change
2008→2014 −13%.

## Current numbers (tip 1e68c9b)

| | ours | paper |
|---|---|---|
| eligible N / area / rate | 98,779 / 132.7 ha / 56.6% | 71,171 / 143 / 58.4% |
| ineligible N / area / rate | 19,113 / 682.6 ha / **23.5%** | 15,254 / 661 / **11.4%** |
| never-elig N / area / rate | 6,140 / 1,085 ha / 43.5% | 7,049 / 760 / 35.7% |
| DiD eligible β | −1.742 (se 1.075) | −1.412 (se 0.558) |
| DiD ineligible β | **+9.502** (se 1.657) | **+4.204** (se 0.886) |

## What has been built (all committed, all runs via `make docker-run`)

```
code/01_build/06_empirics/
  1_parcel_deforestation.R   per-parcel deforestation per year, resumable per raster tile
  2_eligibility_split.R      eligible/ineligible/never-eligible + Table 1 comparison
  3_did_estimates.R          the paper's two DiD regressions + comparison
  4_conflict_resolution.R    Appendix C's 5-rule algorithm, per municipality
  4b_apply_erasure.R         measures the erased regions so stage 2 can subtract them
  _helpers_twfe.R            two-way FE estimator + self-test (lfe will not build here)
```

Data on disk: `data/intermediate/empirics/` — 28 years of `parcel_defo_YYYY.csv`
(1987-2014), `parcel_eligibility.csv`, `parcels_resolved_2014.csv`,
`conflict_decisions_2014.csv`, `erasure_adjustment.csv`, `did_estimates.csv`,
`table1_comparison.csv`.

Re-run order after any change: stage 2 → stage 4 → stage 4b → stage 2 again → stage 3.
(Stage 2 runs twice by design: the first pass supplies `in_sample` for resolution, the
second classifies on the erasure-adjusted values, reproducing legacy's ordering.)

## Candidates already implemented or eliminated — do not re-litigate

occupation level-vs-first-crossing (fixed) · rate denominator (no effect) · declared vs
geometric area (12 parcels) · control-group sample filter (fixed, big improvement) ·
conflict resolution (fixed counts, not coefficients) · assignment precedence (matches
legacy; Appendix B is wrong) · gleba threshold 1% not 0.1% (matches legacy; Appendix B is
wrong) · CNFP vintage and layer definitions (match exactly) · erasure + ordering
(implemented, rate moved away) · ineligible area filter line 1704 (implemented, rate
moved away) · cancelled CARs (paper explicitly includes them).

Full detail: `docs/notes/code_diff_vs_legacy.md`.

## Questions for the authors

1. **How were Table 1's 11.4% and Table 3's matching 11.4% baseline computed?** Every
   other number in that column reconstructs; only this one does not, and it does not
   follow from its own row. It is also the denominator behind the headline "37% increase".
2. Appendix B's stated **0.1%** gleba threshold and its gleba-first assignment ordering
   both contradict the code that produced Table 1 (which uses 1% and assigns control
   first). Worth flagging as draft errors.
3. Appendix B footnote 15 already concedes an unexplained **892,670 → 829,260** slippage;
   no action needed, but it explains part of our count surplus.

## Known gaps / caveats

- **§3.3.3 is an unwritten placeholder**, so the paper's claim that results "carry
  without the spatial cleaning" is unverified in the draft. We relied on it.
- Two conflict-resolution rules draw at **random**; we seed them, legacy did not. Those
  cases can only ever match in distribution.
- Our SEs run ~2x the paper's (hand-rolled CR1 vs `felm`, and larger samples). With 9
  state clusters, neither set is worth much.
- Pixel area is a flat 0.09 ha; at −10° latitude it is ~0.0885 ha, so we run ~1.5% high.
- Stage 4's per-municipality tile scan reopens all 713 rasters each iteration — hoist it
  if that stage is re-run often.

## Other open items unrelated to the empirics

- Nothing pushed; needs a PR into protected `main`.
- `build/stamps/` should be gitignored; the 4 tracked stamps date from 2026-05-05 and are
  NOT evidence of local runs.
- Lavoura step 3 is done; VTN 7-8 still need the `vtn_YYYY.rds` price tables.
