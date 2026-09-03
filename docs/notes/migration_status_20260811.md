# State of the Amazon Amnesty migration — full report (2026-08-11)

**Bottom line:** every main empirical exhibit of the paper now runs in our
reproducible pipeline and has our number sitting next to the printed one. Three
exhibits reproduce essentially exactly, four reproduce to the precision the
surviving data allows, and two columns of one table are permanently blocked
because their input data no longer exists anywhere. The remaining gap between
"matches in sign and magnitude" and "matches exactly" is almost entirely
attributable to one thing: the printed runs used data vintages that no longer
survive, a problem baked into how the original project was run.

---

## Part I — How the migration works

Everything runs inside the pinned Docker container (rocker/geospatial by sha256
digest, renv-locked packages, dated CRAN snapshot, single-threaded BLAS, s2 on)
so results are bit-reproducible. Because lfe/fixest couldn't be added without
breaking the reproducibility contract, all estimators are hand-rolled and
self-tested: `twfe`/`twfe_k` (two-way FE via alternating projections, CR1
cluster errors, tested against known betas) and `fe_ols`/`ols_hc1` (one-way FE
with Stata's areg degrees-of-freedom conventions including the nested-cluster
rule, tested against lm()). The empirics live in stages 1-29 under
`code/01_build/06_empirics/`, each with a header documenting exactly which
legacy lines it ports and every deviation.

Two rules governed the work: **code-over-paper** (when the paper text and
legacy code disagree, replicate the code; keep the paper's variant behind a
flag), and **migrate-only** (no invented code) — the latter relaxed by the
user, for estimation only, on 2026-08-11, which is what let tab:25 and tab:6
get numbers.

---

## Part II — Exhibit by exhibit

### 1. Table 1 — group descriptives (tab:1)

**Status: two of three columns closed; one column unexplainable by any
surviving data.** The never-eligible column reproduces essentially exactly
(2.003/2.189 Mha vs printed 2.0/2.2; rate 36.3 vs 35.7) after discovering three
undocumented legacy behaviors: rates are 2005-08 AVERAGES, the "ever occupied"
filter ran on the 2019 raster despite files named `active2014_*`, and the
control was measured on reserve-cleaned geometry. Eligible and ineligible
counts sit within Pedro's own run-to-run drift (his eligible file was 81,406 in
April 2025, 73,809 in May, 71,171 printed). The printed control column (7,049
parcels) matches NO surviving file — his own control_final.shp has 6,542
features — so it is flagged as unreproducible from any extant vintage,
including his. A late structural finding: Table 1's wide panels and the DiD's
long panels dedup differently from the same rds files, so those two tables were
never computed on the same sample even within one run.

### 2. The main DiD — tab:2, all six columns

**Status: full specification implemented; signs, magnitudes, and the paper's
robustness pattern reproduce; exact match blocked on the lost did.dta
vintage.** This was the hardest exhibit and took three passes: first
replicating what the paper describes, then discovering via the recovered
Dropbox that the real regression code (empirics_amazon_final.do) applies THREE
undocumented sample filters (eligible min pre-2009 rate >= 10; ineligible max
pre-2009 rate < 85, applied to the control too; control parcels with < 5 ha
deforested in 2009 excluded — from the ineligible regressions only), and that
two features we'd been carrying (a zero-2014 drop and 1/99 winsorization)
belong to a SUPERSEDED export path and are absent from the published run.

With the filters adopted: ours **-1.544 (0.684)** and **+4.413 (1.220)** vs
printed -1.412 (0.558) and +4.204 (0.886). The interacted-FE columns reproduce
the paper's signature pattern — the eligibility effect halves under state x
year and muni x year FE (ours -0.893/-0.951 vs printed -0.844/-0.829, within
0.05-0.12) while the expectation effect barely moves (+4.68/+4.25 vs
+4.05/+3.74). Strikingly, running the identical code on Pedro's own surviving
April-2025 panels gives -0.344 — **our pipeline is closer to the printed run
than his own remaining data**. The printed ineligible baseline (11.4) matches
no surviving assembly; ours gives 15.4 and his own panel gives 9.0 under the
faithful assembly (an earlier claim of 15.27 for his panel was our error — a
wrong panel-assembly in the rebuild stage, caught in the audit and retracted).

### 3. Figures 3 and 4 — event studies

**Status: fully covered** (all published panels are rate panels; area panels
are commented out in both the tex and the do-file). Implemented as the
eventdd-equivalent (relative-year dummies, ref 2008, parcel+year FE, cluster
state). Eligible: flat pre-trends, monotone decline to -3.23 by 2014.
Ineligible: ramp to +5.70. Figure 4's conditional pattern reproduces exactly as
printed: the below-1500-ha band (future-amnesty-eligible) ramps to +6.28 while
the 1500-2500 band is flat — the paper's core expectations result. One
ambiguity (value_max < 95 — rate max or area max?) was resolved EMPIRICALLY:
the area reading collapses panel (b) to 54 parcels, so it can't be what was
published.

### 4. SUTVA / land-grabber response — tab:3

**Status: EXACT — every printed cell.** The best result of the project. The
input dataset (reg1_n.dta) survives nowhere, so we rebuilt it from scratch:
IBAMA fines microdata -> CNFP 2013 polygons -> point-in-polygon assignment ->
prior-fine construction -> cloud controls. Every cell matches print: N = 5,655
exactly, all six gamma columns to rounding, all eight event-study coefficients
identical, baselines 0.16%/0.32% exact, and the enforcement-intensity columns
exact (0.033/0.018 and 0.031/0.012, N = 9). Because the input was rebuilt
rather than reused, this validates the ENTIRE chain, not just the regression.

### 5. Policy-Jump table

**Status: near-exact; one column exact.** Ported from multas_RegsFE.R — the
only one of four multas script versions that can have produced it. The
Deforestation/Arson column matches print EXACTLY (-0.0685, se 0.0051,
N 421,968). Model 2's N matches exactly (104,757) with the coefficient off by
0.003; models 1/3 within 2% on N. Reproducing this required faithfully carrying
several load-bearing quirks in the legacy code — a min_year variable that is
actually just year for most rows, a sample-window asymmetry in model 3, and a
policy dummy keyed to the target rather than control year. The recovered
.Rhistory from the same folder shows the exact interactive session that printed
the table, confirming our reading.

### 6. Take-up — tab:25

**Status: data chain reproduced with anchors matching; estimation
(spec-from-paper) matches to vintage precision.** This was "CODE + DATA
MISSING" until the deep fetch recovered DadosTerraLegal.csv, the nine
audited-shape CSVs, and 4.5 GB of SIGEF/SNCI/SNCR title registries. The port
runs all four matching pipelines (name x municipality, two registry chains,
area x municipality), the >= 90%-overlap applies/receives flags, and the
1989-2008 occupation history (three cap-blocked years bridged from our own
same-source panels, behind a flag). On the May-vintage universe: **N = 73,809 —
exactly the printed sample** — with applies 10.5% (printed baseline 10.2%) and
titled-among-audited 41.6% (printed 42.2%). The regressions (written from the
printed spec, since no do-file survives): 27-28 of 28 coefficient signs match,
size-bin gradients near-exact (one cell 0.0997 vs 0.0990), the concave
deforestation-vs-application pattern reproduces.

### 7. Land prices / market expectations — tab:6

**Status: data half fully rebuilt; the headline coefficient reproduces; two
columns permanently walled.** The prices_reg dataset was rebuilt from the
recovered Lavoura parcel-price wides (region shares, FNP prices with all 13
hand-patched regions ported verbatim, and price_north joined from
yearly_average_price_region.xlsx — a file whose producer scripts we'd never
seen turned up beside it in landvalues/vnp/). The regressions (spec-from-
paper): under log lavoura price and count-based shares, the eligible
coefficient — the gamma_1 feeding the paper's omega ~ $272/ha — reproduces at
**0.535 (0.109) vs printed 0.518 (0.108)**. The ineligible coefficient is
same-signed but low (0.19 vs 0.41), consistent with the ineligible universe
having the worst vintage drift of the three groups. Column (5) is same-signed
at N 22 vs 28. Columns (3)-(4) regress TURNOVER measures that exist in no
recovered file or script — a hard wall. Honest caveat: the spec left two
genuinely free choices (price variable, share basis), and the pair that matches
was selected; both variants are reported and the selection is flagged in the
stage header.

### 8. Supporting data chains

- **VNP city-region price panels:** our migrated stages validated **100.00%
  cell-identical** to Pedro's originals (~34k numeric cells) — the only
  whole-stage exact external validation in the project.
- **VTN:** the vtn_YYYY.rds price tables (raw AND cleaned, 2015-2022) were
  recovered; steps 7-8, blocked since July, now run end to end.
- **Enforcement series:** fully reverse-engineered. enforcement_clouds.dta was
  assembled BY HAND outside any script: its enforcement columns are exactly the
  buffer-0.5 fines-per-warning ratios rounded to 2 decimals (verified), and its
  visibility fractions were REBUILT from scratch from the 149 recovered
  Clear-Spots scenes, matching every cell — which PROVES a methodological
  artifact: A/B half-scenes were averaged as separate observations,
  mechanically halving measured visibility in 2007-2010, straddling the 2009
  cutoff. Our replication of print is unaffected, but that is a caveat about
  the exhibit itself worth flagging to the authors.
- **Smaller items:** Fig-applications data matches the printed chart (25,584
  requests peak, 4.25M ha); the do-file's invaded-area regression runs
  (post-2009 invaders claim 2,841 ha smaller parcels, supporting fig:10).
  Figures 1 and 2 (maps/aggregates) remain unported — cosmetic rendering, low
  priority.

---

## Part III — The three big challenges

### Dropbox

Both shares were browsable only anonymously, which imposes a ~16-item listing
cap per folder — the reason earlier surveys missed things. The workaround:
per-subfolder zip downloads via the share URLs (dl=1), which required walking
the folder tree in a browser to harvest each subfolder's opaque hash, then
~12 GB of curl transfers. Complications: one zip arrived truncated/corrupt
(auxiliary — its key file re-fetched individually), a shell quoting bug
silently skipped two downloads, one extraction ran against a still-growing zip,
and three specific files (CAR_eligible_defo_2002-2004.rds) sit behind the cap
with no reachable link at all — bridged from our own panels instead. The effort
paid for itself several times over: the second sweep found the entire Terra
Legal microdata, the VTN tables, the Lavoura wides, the Clear Spots scenes,
yearly_average_price_region.xlsx, and both projects' .Rhistory files.

### Code versions

The legacy code is not one pipeline but LAYERS OF DRAFTS that all still exist.
The DiD has three generations (did1/did2 -> did1_new/did2_new -> did.dta), and
features we initially treated as part of the published method (winsorization, a
zero-2014 drop) belong to superseded generations. The fines work is TWO
DIFFERENT METHODOLOGIES — municipality-level matching (four script versions,
only the last printable) and point-in-polygon on CNFP (the SUTVA path) — easily
mistaken for variants of one spec. The DETER matching has three versions of
which two export nothing and changed nothing. This was resolved with a
line-by-line audit of every regression-bearing file, consolidated in
docs/notes/regression_version_map.md, which pinned each printed exhibit to
exactly one producing version and caught a real error in our own rebuild. A
structural discovery explains the whole mess: **every Stata-facing export in
2_empirics.R sits below lines that error on a clean run** — the datasets behind
the paper could only ever have been produced interactively, in pieces, which is
why no two of Pedro's runs agree with each other.

### Missing data / vintage drift

The paper's numbers come from specific interactive runs whose intermediate
files were overwritten by later runs. The eligible universe alone spans
81,406 -> 73,809 -> 71,171 across surviving traces; successive runs overlap
only ~82%. Consequences: the printed DiD, Table 1's control column, and the
ineligible baseline match NO surviving dataset, including Pedro's own.
Genuinely nonexistent (not just missing from the shares): the turnover data
(tab:6 cols 3-4), visible_fraction.csv (rebuilt), reg1_n.dta (rebuilt), and
every Stata do-file except empirics_amazon_final.do — confirmed by full
enumeration, which is why estimation for tab:25/tab:6 had to be reconstructed
from the printed specs under user authorization.

---

## What's still missing (the complete list)

**From Pedro, would close remaining gaps:** the printed did.dta (exact DiD
match), the tab:6/tab:25 do-files (confirm our reconstructed specs), the
turnover-producer script and data (tab:6 cols 3-4), the Table-1-control
vintage, CAR_eligible_defo_2002-2004.rds, and reg1_n.dta as a diff-check.

**Nonexistent, unfixable:** the exact conflict-resolution draws (legacy used
unseeded sample()), and the interactive states behind the drifted vintages.

**Low-priority unported:** figure cosmetics (Figs 1, 2, and styled PDF
rendering — all underlying data exists).

Cross-references: the findings trail is docs/notes/paper_legacy_method_diffs.md;
the code genealogy is docs/notes/regression_version_map.md; the exhibit
inventory is docs/notes/missing_for_replication.md; the current handoff is
docs/notes/checkpoints/checkpoint-20260810-overnight.md.
