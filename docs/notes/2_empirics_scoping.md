# `2_empirics.R` scoping pass

Date: 2026-07-29
Status: scoping only. No code migrated yet.

Purpose: turn `legacy_repo/code/2_empirics.R` (3,571 lines) into a numbered stage list
with dependencies, so scope can be cut with information rather than guessed at.

> **PARTLY SUPERSEDED — read `docs/notes/paper_vs_pipeline.md` first.**
> After reading the paper (`amnesty_wp.pdf`), two things below are wrong:
> 1. **"Second finding" is misdiagnosed.** It guesses `spillover` is the mis-wired loop.
>    It is **`control`**. `spillover` IS the paper's ineligible group, so its
>    `CAR_notEligible_defo_` read is correct; `control` should read
>    `CAR_inReservas_defo_`.
> 2. **The stage-4 "already migrated" note is too optimistic.** Our
>    `car_eligible_cleaned.shp` is the paper's eligible + ineligible *combined*; the
>    eligible/ineligible split still has to be built, and its occupied-by-2004 condition
>    needs the deforestation rasters. Stage 5 is on the critical path.
> The stage map, the magic-file finding, and the estimates remain valid.

## HEADLINE FINDING — the DiD tail is disconnected from the pipeline above it

This is the thing to resolve before any code is written.

The script's terminal outputs are `did1_new.dta` and `did2_new.dta` (lines 2862, 2872),
the DiD panels. They are assembled at lines 2850-2890 from three objects: `control`,
`eligible`, `spillover`. Those three are read from
`~/Library/CloudStorage/Dropbox/amazon_working/` with these patterns:

| object | source pattern (line) | producer in repo? |
|---|---|---|
| `eligible` | `car2004_defo_*` (2783) | **NONE** — 1 mention in the whole legacy tree, the read itself |
| `control` | `CAR_notEligible_defo_*` (2727) | **NONE** — 2 mentions, both reads |
| `spillover` | `CAR_notEligible_defo_*` (2748) | **NONE** — same pattern as `control`, see below |

Meanwhile the upstream pipeline (lines 1370-1480, 2069-2081) writes five per-year files
into `data/intermediate/` that **nothing in the repo ever reads**:

```
CAR_eligible_defo_<year>.rds
CAR_eligible_uncleaned_defo_<year>.rds
CAR_ineligible_defo_<year>.rds
CAR_ineligible_uncleaned_defo_<year>.rds
CAR_control_defo_<year>.rds
```

So the script computes five families of deforestation panels, then builds its
headline results from three *differently named* files in a *different directory* that
nothing produces. This is the same "magic file" pattern the CAR migration existed to
remove, sitting directly under the paper's main outputs.

**Most likely reading** (needs confirmation, do not assume): `amazon_working/` holds an
older generation of the same artifacts under earlier names, i.e.
`car2004_defo_` ≈ `CAR_eligible_defo_` and `CAR_notEligible_defo_` ≈
`CAR_ineligible_defo_`. This is plausible because the upstream code derives
`car_eligible_cleaned.shp` from an object literally called `car2004` (line 1331), so
`car2004_defo_` reads naturally as "deforestation for the 2004-active eligible set".
If that holds, the DiD tail can be rewired onto the pipeline's own outputs and becomes
reproducible.

**DECISION REQUIRED before stage 6 (the DiD assembly) can be written:**
- (A) Rewire onto the pipeline's own `CAR_*_defo_<year>.rds`, documenting the name
  mapping. Fully reproducible. Recommended, but it changes which rows enter the
  regressions, so the DTA files will not be bit-identical to the historical ones.
- (B) Obtain `amazon_working/` as a vendored snapshot and treat it as a given input.
  Preserves the exact published numbers, imports non-reproducible state.
This mirrors CAR issue #15 exactly, and was resolved there by taking (A) and
quantifying the difference against the published snapshot.

## SECOND FINDING — `control` and `spillover` are built from the same files

Lines 2727 and 2748 apply the **identical** filter `str_detect(file_name,
"CAR_notEligible_defo_")`. `control` is then reshaped wide (via repeated `left_join`)
and `spillover` long (via `melt`), and each gets its own drop filter, but they descend
from the same source files.

`did2` (line 2867) is therefore `rbind(control, spillover)` with `treatment = 0` and
`treatment = 1` respectively — i.e. **the same population compared against itself**,
differing only in reshaping and the `drop_*` filter. Almost certainly a copy-paste bug:
`spillover` should read a different pattern (`CAR_inReservas_defo_` is the obvious
candidate — it is written at line 2718 and read nowhere).

Flagging, not fixing. If `did2` backs a published result, that result needs review
independently of this migration.

## Stage map

Confidence: **[H]** verified by reading the code this pass; **[M]** inferred from I/O
structure; **[L]** needs a closer read before committing to it.

| # | Stage | Lines | Status | Compute |
|---|---|---|---|---|
| — | Load biome / glebas / states; build control + target areas | 49-250 | **[H]** partly done — the CAR scaffold already builds control/target masks | light |
| — | Combine CAR shapes + clip to biome | 253-307 | **[H] ALREADY MIGRATED** → CAR stage 05 | — |
| — | `already_treated` (SNCI, ≤2009) | 324-341 | **[H] ALREADY MIGRATED** → CAR scaffold | — |
| 1 | `ccar_clean` / `ccar_dirty`: CARs inside vs outside federal glebas | 347-514 | **[M]** needed | moderate |
| 2 | Active-2014 CAR selection + per-muni defo attach | 600-728 | **[M]** needed | **heavy** — per-muni × raster, per-parcel R loop |
| 3 | **Spatial conflict-resolution algorithm** | 731-1170 | **[H]** needed | **heaviest** — nested per-CAR loops over `insiders_1/2`, `overs` |
| 4 | Active-2004 cleaned/uncleaned → `car_eligible_{cleaned,uncleaned}.shp` | 1180-1332 | **[H]** `car_eligible_cleaned` already produced by the CAR scaffold — check equivalence | moderate |
| 5 | Per-year deforestation panels → `CAR_*_defo_<year>.rds` | 1370-1480, 2069-2081 | **[H]** needed — this is what the DiD should consume | **heavy** — 17 years × 4 populations |
| 6 | Control set: CARs in reservas → `control_final.shp` | 1800-1980 | **[M]** needed | heavy |
| 7 | Wide/long panel assembly from the `.rds` files | 1550-1700, 2093-2130 | **[M]** needed | light |
| 8 | **DiD assembly → `did1_new.dta`, `did2_new.dta`** | 2726-2890 | **[H]** needed, **blocked on the decision above** | light |
| — | NB/Lavoura price merge → `did.dta`, `less_1500.dta` | 2300-2330 | **[H] INPUT ALREADY SATISFIED** by Lavoura step 3 | light |
| — | `prices_reg.dta` | 2560-2580 | **[L]** probably needed if price regressions are in the paper | light |
| — | Maps, plots, `tmap_save`, `ggsave` to `~/Documents/` | 143-214, 1500-1545, 2040-2046, 2690-2700, 3500-3540 | **[H] EXPLORATORY** — drop unless a specific figure is wanted | light |
| — | `occupationCAR/` blocks, `sigef`, `~/Downloads/municipios.csv` | 2600-2740, 2900-3571 | **[L] likely DEAD** — older generation, personal paths, superseded | — |

## What this means for the estimate

The load-bearing path to the DiD outputs is stages **1 → 2 → 3 → 4 → 5 → 6 → 7 → 8**.
Stage 3 (conflict resolution) and stage 5 (per-year defo panels) dominate compute; they
are the analogues of CAR stage 03, which on this machine produced 10-hour
single-municipality runs and needed OOM rescue.

Roughly half the file — the maps, the `occupationCAR` blocks, the `sigef` section, the
post-2900 figure code — is exploratory or superseded and does not feed the DTA outputs.

**Revised estimate for the DiD-only path:** ~4-6 stages of real porting work rather than
8+, so **around 1 week** rather than two, plus **2-4 days of compute**. Adding the
figures and `3_policy1.R` pushes it back toward the original two-week figure.

Every stage from 2 onward must be written **per-municipality with skip-if-exists**, so it
can be sharded across containers and resumed after an OOM. On a 24 GB / 12-core M4 Pro
the binding constraint is RAM, not cores: measured CAR runs needed ~14 GB per heavy
worker, so realistically 1-2 concurrent workers.

## Recommended order

1. **Resolve the magic-file decision above (A or B).** Everything terminal depends on it.
2. Stage 4 equivalence check first — if the CAR scaffold's `car_eligible_cleaned` already
   matches what stage 4 would produce, stages 1-4 may collapse substantially. This is the
   cheapest possible test and could remove the two heaviest stages from scope.
3. Then stage 5 (defo panels), 6 (control), 7 (panels), 8 (DiD).
4. Figures last, individually, only if a specific one is wanted.

Step 2 is worth doing before anything else: it is a few hours of work and could cut days.
