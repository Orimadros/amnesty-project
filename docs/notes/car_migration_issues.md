# CAR magic-file migration: issues, bugs, and deviations log

Date started: 2026-07-14
Status: in progress. Stages 00-01 drafted (untested — no raw data on disk yet).

Companion to `docs/notes/car_magic_files_recovery.md` (the chain map). This file
records every place where the migrated code in `code/01_build/01_car/` deviates from
the legacy producer, plus bugs/irregularities found in the legacy code while porting.

Legacy source, vendored verbatim at:
`legacy_repo/dropbox_producers/create_muni_year_intersections/`
(downloaded 2026-07-14 from the amazonLandPrices_project Dropbox).

## Canonical path (confirmed from code)

The orchestrator `create_SicarMuniOverlap_variables.R` sources, in order,
`A_prepare_initial_data.R` -> `B_intersect_car_union_sensitive_land.R` ->
`C_intersect_individual_cars.R` -> `D_consolidate.R`, each gated by an `fp$SECTION_*`
flag. `D_consolidate.R` (not `D_consolidate2.R`) is canonical: it is the one the
orchestrator sources, it uses the `fp$`-namespaced flags, and it carries later typo
fixes and an extra computed column. The `_manual_clean`, `_workingmem`, `D2_recompute_areas`,
and `D_consolidate2` files are side branches and are NOT migrated.

Correction to an earlier note: the CAR cleaning is in **`A_prepare_initial_data.R`
section 3** (`if(fp$CLEAN_CARS)`), not "Section C". The `fp$CLEAN_CARS` flag merely
sits near the Section-C flags in the orchestrator's config list.

## Deviations in migrated stages 00-01

### Issue #1 — latent variable bug in the reg-year panel (FIXED in migration)
`A_prepare_initial_data.R:371` builds the panel from an object named `microdata`,
but that section only ever defines `microdata_inc`; `microdata` is undefined there.
In the final canonical run this section was toggled off (`CARDIDS_AND_YEARS <- FALSE`,
commented "NO NEED TO RUN AGAIN"), so the bug was likely never hit as-is.
For a rebuild-from-raw this code must run. The rest of the pipeline defines
`microdata <- fread("temas_ambientais_update.csv")` (the 2023-excluded update), so
`00_car_registration_years.R` reads that file back into `microdata` before building
the panel. This is the clearly-intended object and keeps the panel consistent with
what stage 01 consumes.

### Issue #2 — dead code: `car_and_reg_year_tmp_nas` (DROPPED)
`A_prepare_initial_data.R:386-390` computes a `yNA` NA-handling table that is then
`rm()`ed without being used by the wide `dcast`. Dropped in the migration.

### Issue #3 — robust save disabled behind `if(FALSE)` (UN-GATED)
In `A_prepare_initial_data.R` section 3, the `clean_shape_reenforced()` ->
`CleanCARShapes_robust` write (and the light + s2 writes) sit inside an `if(FALSE){...}`
block (lines 709-775), disabled after the one-time production run. Only
`clean_shape_basic()` ran unconditionally. `01_clean_car_shapes.R` un-gates the robust
pass so Magic File #1 is actually regenerated.

### Issue #4 — non-deterministic muni loop (REPLACED)
The legacy cleaning loop selected municipalities with random `sample()` and coordinated
multiple parallel worker machines through a shared `munis_already_claimed_CAR.csv`
"claim" file. Output is per-muni and skip-if-exists, so order does not change results,
but the random/claims machinery is machine-state-dependent. Replaced with a
deterministic ordered `for` loop over sorted municipality codes, keeping skip-if-exists.

### Issue #5 — side-outputs (CORRECTED: light + s2 are REQUIRED, not dropped)
Initially I dropped the "light" and "s2" cleaned variants from stage 01 believing
only `robust` was consumed downstream. **That was wrong.** Reading stage 02 (the B
script) showed its intersection loop reads CAR geometries from `CleanCARShapes_s2/`,
falling back to `CleanCARShapes_light/` for municipalities where the s2 variant is
absent (B lines 1194-1216: it builds `property_directories_s2` and appends light dirs
for `these <- munis_light[!munis_light %in% munis_s2]`). So all three variants are
load-bearing:
  - `robust` -> 05_combine_car_biome.R (the `2_empirics` combine)
  - `s2` (with `light` fallback) -> 02_car_union_sensitive_land.R (B)
Stage 01 now writes all three from a common basic-cleaned base, matching legacy. The
s2 write is wrapped in tryCatch so that, as in legacy, a municipality whose s2
cleaning fails simply has no s2 output and B falls back to its light variant.
Only the truly-unused `CAROverlap_invalid_preclean/...` invalid-geometry diagnostics
remain dropped.

### Issue #10 — geobr network dependency in B (must be replaced; DECISION NEEDED)
`B_intersect_car_union_sensitive_land.R:1184` loads municipal borders via
`geobr::read_municipality()`, which downloads boundary data from IBGE at runtime.
That is a network call and violates the repo's reproducibility contract (PROJ network
is off; all inputs must be vendored). The migrated stage 02 must read a local
municipal-boundaries shapefile instead. Candidate: the terrabrasil
`data/input/aux/municipalities_amazon_biome` layer already used by stages 00-01,
mapping its `geocodigo` to the `code_muni` field B expects -- pending confirmation
that its municipality coverage and codes match what `read_municipality()` returned.

### Issue #6 — manual QUICK_FIX pre-merge for RO/1100205 (NOT reproduced)
`A_prepare_initial_data.R:6-19` (`if(fp$QUICK_FIX)`) manually merges
`AREA_IMOVEL_1..4` into a single `AREA_IMOVEL` folder for municipality 1100205 (RO).
This is a one-off manual data-prep step. `01_clean_car_shapes.R` reads the merged
`AREA_IMOVEL` folder and excludes split `AREA_IMOVEL_n` dirs. If split dirs are found
for any municipality once the raw data lands, they must be merged upstream. TODO:
decide whether to fold this merge into a small pre-step or document it as a manual
input fix.

### Issue #7 — removed runtime GitHub `source()` (VENDORED)
`helper_functions.R:33-34` sourced `helper_functions_simple.R` and
`helper_functions_brazil.R` from `raw.githubusercontent.com/Thiago-Alckmin/...` at
runtime. Those files are vendored alongside the producers, and the two utilities
actually used (`message_with_lines`, `rename_columns`) are ported into
`_helpers_car_util.R`. No runtime network dependency remains.

### Issue #8 — list-column crash from the deterministic loop refactor (FIXED)
Found during self-review. Legacy built the muni-code column with
`stri_extract_all_regex(path, "\\d{7}")`, which returns a **list column**. That was
safe in the legacy loop (only `%in%` / `stri_detect` were used on it), but the
reproducible rewrite sorts the municipality codes (`sort(unique(municipio))`) and
compares them with `==`. `sort()` on a list column errors ("cannot xtfrm data
frames"), so stage 01 would have crashed before cleaning anything. Verified in the
container. Fixed by switching to `stri_extract_first_regex` (atomic character
vector); behaviour is identical because each path contains exactly one 7-digit code.

### Issue #9 — duplicate-key merge in the date fill (PRESERVED, flagged)
`A_prepare_initial_data.R:337-348` binds the two scraped date tables
(`cancelled_car_dates.csv` + `202310_report_data_registro_car.csv`) into
`cancelled_cars_with_dates` **without de-duplicating `registro_car`**, then
`merge(..., all = TRUE)` into the microdata. If a CAR id appears in both scrapes (or
twice within one), the merge cartesian-expands and duplicates that CAR's microdata
row. Preserved as-is to stay faithful, but flagged: once the scraped CSVs are on disk,
check for duplicate `registro_car` across them and decide whether a
`unique(registro_car)` guard is warranted. Note the fread round-trip reads
`data_inscricao` back as IDate (verified), so the `year()` filters downstream are fine.

### Issue #11 — hardcoded per-municipality special cases in B (PRESERVED)
The B s2 loop contains municipality-specific geometry handling that is preserved
verbatim in stage 02, because these are hand-tuned fixes for pathological geometries,
not bugs:
  - `2111300`: union is built as `clean_shape() %>% st_union()` instead of
    `st_union() %>% clean_union_reenforced()`.
  - `1300631`: `car_area_intersect_conserve` is set to `NA` (the conservation
    intersection failed for this muni).
  - `1507300`: the forest-A and forest-ALL intersections take the 2nd geometry piece
    (`.[2]`) and reenforce-clean it before measuring area.
Flag: these are fragile and tied to a specific data snapshot. When stage 02 is first
run on real data, verify each still behaves as intended; if the underlying geometry
issues no longer reproduce, the special cases may be removable.

### Issue #12 — CNFP vintage mismatch (NOTED)
Stage 02 loads public forests from the "CNFP 2020 Shapefiles" set (per legacy B),
whereas the existing scaffold `0_build_car_layers_from_raw.R` reads CNFP `SHP_2013`.
Both live under `data/input/cnfp/`. Confirm which CNFP vintage each stage should use
when the data lands; they are different snapshots and not interchangeable.

### Issue #13 — `valid_structure` GEOS method ignored under s2 (ASSUMPTION, verify vs snapshot)
The legacy cleaning functions (`clean_shape_reenforced`, `clean_union_reenforced`) call
`st_make_valid(geos_method = "valid_structure", geos_keep_collapsed = FALSE)`. Under the
repo's mandated `sf_use_s2(TRUE)` with geodetic coordinates (EPSG:4674), sf **ignores
those GEOS arguments** and validates via s2 instead (confirmed by the runtime warning).
Consequences:
  - `clean_shape_reenforced` effectively reduces to s2 `st_make_valid` + drop
    duplicates/empties/invalids -- i.e. it converges with the plain `clean_shape`.
  - This is only bit-faithful to the legacy snapshot if that snapshot was produced with
    s2 enabled (modern sf default). If the snapshot needs planar `valid_structure`
    geometry, reproducing it would require toggling s2 off around these calls -- which
    conflicts with the repo's "s2 explicitly enabled" reproducibility contract.
Action: when the raw data + a snapshot of `CleanCARShapes_robust` are available, compare
regenerated vs snapshot geometry; resolve the s2-vs-GEOS tension explicitly if they differ.

### Issue #14 — CAR shapefiles assumed to carry `SITUACAO` (ASSUMPTION)
Stage 02's cancelled/not-cancelled split reads `car$SITUACAO` from the cleaned CAR
shapes. This assumes the raw SICAR `AREA_IMOVEL` shapefiles carry a `SITUACAO` field
(they typically do) and that stage 01's cleaning preserves it (it does -- cleaning only
touches geometry). If the field is absent, `car$SITUACAO` is NULL and the cancelled/
not-cancelled areas silently become empty (0/NA) rather than erroring. Verify the field
is present when the data lands. Faithful to legacy, which makes the same assumption.

### Issue #15 — CAR_overlap_variables_final.csv depends on non-reproducible v1-v4 archives (RESOLVED 2026-07-14: reproducible core only)
DECISION (user-approved): `final` = s2 + robust union only; the v1-v4 augmentation is
dropped. Implemented in `03b_consolidate_car_overlaps.R`. Validation plan: rebuild
`muni_year_intersections.csv` and diff against the Dropbox snapshot
(`data/cleaned/municipal_level/muni_year_intersections.csv`) to quantify any coverage
difference attributable to the missing v1-v4 gap-filling. Original analysis follows.
The legacy consolidation in `C_intersect_individual_cars.R:1278-1293` builds the file
stage 04 reads (`CAR_overlap_variables_final.csv`) by unioning the reproducible `FINAL`
(s2 + robust intersections) with archived overlap datasets `v1/v2/v3/v4` read from
`data/processing/archive/CAROverlap_v1../v4/`. Those archives are outputs of older
cleaning runs with **no producer in this pipeline** -- i.e. the same "magic file"
hazard this whole migration exists to remove. Consequences:
  - A fully reproducible-from-raw build can only reconstruct `FINAL` (s2 + robust);
    it cannot regenerate v1-v4.
  - Omitting v1-v4 yields a CAR-conflict set with potentially lower coverage than the
    `muni_year_intersections.csv` that currently exists, so results may shift.
Decision required before writing the stage-03 consolidation:
  (A) Reproducible core only: `final := FINAL` (s2 + robust), drop the v1-v4
      augmentation. Fully reproducible; document the coverage difference. (Recommended,
      matches the project's reproducibility mandate.)
  (B) Treat the v1-v4 `CAROverlap` archives as vendored "given" inputs (if still
      available in the Dropbox) to preserve exact coverage, importing non-reproducible
      historical state.
`03_intersect_individual_cars.R` (the reproducible intersection computation) is written
and does not depend on this decision; only the consolidation into the D-consumed files
does.

**Dropbox survey (2026-07-14, via the full-project share link).** The orchestrator sets
`dir_wd <- "/data/research/Thiago/Amazon/"` -- the whole A-D chain ran on Thiago's
research server; the Dropbox holds only a partial copy of its outputs. Findings:

Present in the Dropbox (usable as snapshots / inputs):
  - `data/processing/CleanCARShapes_{light,robust,s2}/` -- Magic File #1 snapshots.
  - `data/cleaned/municipal_level/muni_year_intersections.csv` (+ .xlsx) -- Magic
    File #2 snapshot (0.96 MB). Gold standard for validating our rebuild.
  - `data/cleaned/sicar/car_combined.{shp,dbf,prj,shx}` (~1.7 GB) -- the combined CAR
    layer (a derived snapshot; resolves the stage-00 provenance question).
  - `data/raw/sicar/` (shapefiles, microdata, validate_registry) -- raw inputs.
  - Older generations: `data/processing/CAROverlap/` (per-muni `muni{code}_2023.csv`),
    `SicarMuniOverlap/`, `SicarMuniOverlap_REPLICATION/`.

NOT in the Dropbox (referenced by C/D; lived only on the server):
  - `data/processing/archive/CAROverlap_v1..v4/` -- the v1-v4 archives. No `archive/`
    under data/processing; the top-level `archive/` holds only slurm logs.
  - `data/processing/CAROverlap_robust/`, `CAROverlap_s2/` -- C's per-muni outputs.
  - `data/processing/SicarMuniOverlap_s2/` -- B's s2 per-muni-year outputs.
  - The consolidated `data/cleaned/` CSVs D reads (`CAR_overlap_variables_final.csv`,
    `_reftarget_areas.csv`, `sicar_overlap_variables_exapanded_s2.csv`,
    `sicar_area_imovel_combined.csv`) -- `data/cleaned/car_level/` is empty.

Assessment: unlike Magic File #1 (whose producer was recovered), the missing
intermediates are REGENERABLE from what the Dropbox does hold -- stages 02/03 rebuild
SicarMuniOverlap_s2 and CAROverlap_robust/s2 from the cleaned shapes + raw data. The
only truly unrecoverable pieces are the v1-v4 archives: producers superseded, data
absent. Their role in D's input is lowest-priority gap-filling -- the reftarget dedup
keeps `1_s2_true` first, then `2_robust`, and v1-v4 only contribute CAR pairs absent
from both. And since the final muni_year_intersections.csv exists as a snapshot, the
impact of dropping v1-v4 is empirically measurable: rebuild without them, diff against
the snapshot, and document any gap.

### Issue #16 — rbind typos in C's consolidation loops (FOUND; relevant when porting consolidation)
The s2/robust consolidation loops in `C:914-945` accumulate with `rbind(car_vars, ...)`
where they clearly mean `rbind(car_vars_s2, ...)` / `rbind(car_vars_robust, ...)`, and
the robust loop then fwrites `car_vars_robust` (only ever the first file). As written
these either error on the first iteration (undefined `car_vars`) or drop all-but-one
file. When the stage-03 consolidation is written, implement the evident intent
(row-bind all per-muni CSVs) and record the fix here.
FIXED in `03b_consolidate_car_overlaps.R`: `read_overlap_dir()` binds every per-muni
CSV with `rbindlist(fill = TRUE)` over a sorted file list.

### Issue #17 — `allx=T` merge typo in C's reftarget_areas step (FIXED in migration)
`C_intersect_individual_cars.R:1075` and `:1080` pass `allx=T` (typo for `all.x`) to
`merge()`. The misspelled argument falls into `...` and is ignored, so the legacy
merges ran with the default `all.x=FALSE` -- an inner join that silently drops any
overlap pair whose target CAR id never appears as a reference id. Implemented as the
evident intent (`all.x = TRUE, all.y = FALSE`) in `03b_consolidate_car_overlaps.R`.
Caveat for validation: if the legacy inner join did drop rows, our reftarget_areas
may contain MORE pairs than the historical file; check row counts when diffing
against snapshots.

### Issue #18 — NA-poisoning in D's n_unique_cars when pre-2014 registrations exist (CONFIRMED in container; PORTED FAITHFULLY)
`D_consolidate.R` section 8.2.2 casts CARs wide on registration year, zero-fills ONLY
columns 2014-2022, then computes `actual_year := max(year*value)` per CAR without
`na.rm`. If ANY CAR in the overlap data was registered before 2014, the wide table
gains a pre-2014 column whose NAs survive the fill, `max()` returns NA for every CAR
registered 2014+, and those CARs drop out of `n_unique_cars` entirely (only the
pre-2014 CARs count). Demonstrated in the pinned container with synthetic data: adding
one 2013 registration changed n_unique_cars(2018) from 2 to 1.
Ported VERBATIM in `04_consolidate_muni_year.R` because faithfulness is safe either
way: if the legacy input contained pre-2014 dates, the published snapshot embeds these
same poisoned values and the faithful port reproduces them; if not, no poisoning
occurs. Validation directive: when diffing the rebuilt panel against the Dropbox
snapshot, check `min(year(data_inscricao))` in CAR_overlap_variables_final.csv --
if < 2014, inspect n_unique_cars in the snapshot for implausibly low values before
concluding anything about our port.

### Issue #19 — writexl dependency dropped (DEVIATION)
Legacy D wrote `muni_year_intersections.xlsx` (data + variable-description sheet) via
`writexl`, which is not in renv.lock. Rather than extend the reproducibility contract
for a cosmetic Excel copy, stage 04 writes the variable descriptions to
`muni_year_intersections_variable_desc.csv` alongside the main CSV. If an xlsx is
wanted, do the package-adding dance for writexl and add the write-back.

### Issue #20 — discarded invariant check surfaced as warning (DEVIATION)
`D_consolidate.R:23` computed "all reference CARs registered on/after their targets"
and silently discarded the result. Stage 04 evaluates the same invariant and issues a
`warning()` if it fails.

### Issue #21 — 2_empirics combine was interactive scratch code (REGULARIZED)
`legacy_repo/code/2_empirics.R:255-307` (the car_combined_amazonBiome2 producer) was
interactive scratch: hardcoded `/Users/pedrotremacoldirossi/...` Dropbox paths,
`registerDoParallel(cores=80)`, chunk indexing `j:(j+100)` (101 rows -- every block's
last row duplicates the next block's first, and the final block runs past the end of
the table), Para processed twice (car_2 = "not AC/AM/RR/AP" includes PA, yet PA was
also run separately as car_3 and both were rbind-ed), and a dedup computed at line 292
then discarded by the rebuild at line 304. `05_combine_car_biome.R` implements the
evident intent: combine all robust shapes (sorted, deterministic), keep
AC/AM/RR/AP CARs whole, clip every other state's CARs to the biome border in exact
non-overlapping sequential chunks, drop the biome layer's attribute columns, dedup
once at the end. The geometric operation (st_intersection with the biome border) is
unchanged. Note: legacy's `select(-id, -bioma)` is generalized to dropping all
attribute columns of the biome layer. Sequential execution replaces the 80-core
foreach (the container pins threading to 1 for determinism anyway).

### Issue #22 — CNFP 2020 RR shapefile has unclosed polygon rings (FIXED in stage 02)
`data/input/cnfp/CNFP 2020 Shapefiles/CNFP_2020_RR.shp` contains polygons whose rings
do not close (first point != last point). s2 refuses them ("Unrecognized geometry
type code" WKB parse errors) and GEOS refuses conversion ("Points of LinearRing do
not form a closed linestring"), so `st_make_valid()` failed either way and stage-02
workers died deterministically on the first Roraima municipality. Legacy presumably
ran with s2 off and an older GEOS that auto-closed rings. Fix in
`load_forest_data()`: close rings in R (no-op for well-formed geometries), then
st_make_valid through GEOS with s2 briefly off (GEOS rewrites the WKB so all
downstream s2 ops parse it). Verified: RR forest union area ~1.74e11 m^2. Applies to
all states defensively; only RR exhibited the corruption.

### Issue #23 — Amazon biome border ring self-crosses on the sphere (FIXED in stage 05)
The biome border polygon (~168k edges) is GEOS-valid but s2-invalid ("Loop 114 is not
valid: Edge 168410 crosses edge 168415") -- planar-valid rings can still cross when
edges become geodesics. st_intersection under s2 failed at WKB conversion. Fix:
one-time `s2_rebuild(split_crossing_edges = TRUE)` on the biome geometry after load;
the whole biome clip then runs under s2 as pinned. Deterministic. Legacy presumably
clipped under planar GEOS and never hit this.

### Issue #24 — "FULL" pseudo-year not computed in stage 02 (DEVIATION, output-equivalent)
Legacy B's year loop ran 2014-2022 PLUS a "FULL" pseudo-year (union over all CARs
regardless of registration year). The stage-02 port loops 2014-2022 only -- the FULL
pass was dropped (noticed during the final consolidation sweep). Output-equivalent
for the final panel: D_consolidate filters `year != "FULL"` immediately, so FULL rows
never reach muni_year_intersections.csv; omitting them saves ~11% of stage-02
compute. The consolidated sicar_overlap_variables_s2.csv therefore lacks FULL rows
that the legacy intermediate had -- only relevant if someone later wants the FULL
diagnostics. Also added an early muni-completion check (muni's _2022.csv exists →
skip) so resume/consolidation sweeps don't redo per-state forest processing.

### Issue #25 — stage 05 biome clip: predicate pre-filter replaces blanket clipping (OPTIMIZATION, output-equivalent)
The ported clip intersected EVERY border-state CAR against the ~168k-edge biome
polygon in 100-row blocks. With ~hundreds of thousands of border-state CARs that is
thousands of constructive spherical intersections against a huge geometry: the run
exceeded 36 h at 100% CPU with no completion and no progress visibility.
Replaced with a spatial-predicate pre-filter that is output-equivalent:
  - CAR fully covered by the biome -> clipping is a no-op, keep whole
  - CAR disjoint from the biome    -> clip yields empty, drop
  - CAR crossing the border        -> actually clip (only expensive case, few)
Predicates use the spatial index and are far cheaper than st_intersection.
Equivalence verified empirically on a 300-CAR straddling sample: identical row
count (300 vs 300), identical COD_IMOVEL set, total area agreeing to 4.2e-12
relative. Per-chunk progress logging added to the remaining clip loop.
Caveat: row ORDER within the border-state group may differ from the blanket-clip
version (whole-kept rows precede clipped rows); the set of features and their
geometries are the same, and a dedup follows. Immaterial for a map layer.

### Issue #26 — scaffold 0_build_car_layers: GEOS + two #25-style optimizations (FIXED)
Running the eligible/ineligible/already_treated scaffold surfaced three problems, all
resolved:
  1. s2 rejects a whole class of geometries here (biome self-crossing #23; and the
     constructive st_intersection/st_difference in overlap scoring produce
     near-degenerate slivers s2 refuses to re-parse). FIX: run the scaffold under
     GEOS/planar (`sf_use_s2(FALSE)`), matching the legacy 2_empirics workflow. This
     is a map-classification artifact, not the validated panel, so planar is faithful.
  2. Redundant re-clip: car_raw (= car_combined_amazonBiome2, already biome-clipped
     by stage 05) was re-intersected with the biome -- 801k features vs the 168k-edge
     polygon, a multi-hour no-op. Dropped.
  3. calc_overlap_share intersected all 801k CARs against the control/target masks.
     Those masks are small federal-land subsets (only 16,084 and 182,572 CARs touch
     them). FIX: st_intersects pre-filter, intersect only touchers (non-touchers get
     0 via the existing right_join + coalesce). Output-equivalent (#25 pattern).
After these, the scaffold completes: already_treated (7,468), car_eligible_cleaned
(164,223), car_ineligible_cleaned (13,025 features) -- all geometry-valid.

## ============================================================
## CAR MIGRATION COMPLETE (2026-07-24)
## ============================================================
Full chain regenerated from raw, containerized, network-free, reproducible:
  00 registration years -> 01 robust cleaning (Magic File #1) -> {02 sensitive-land
  overlaps, 03 pairwise conflicts -> 03b consolidation} -> 04 muni-year panel
  (Magic File #2) -> 05 biome combine -> 0_build scaffold (eligible/ineligible/
  already_treated). Wired in analysis.mk (`make 01_car`). VALIDATION: rebuilt
  muni_year_intersections.csv matches the published snapshot to research grade
  (5,022 rows, structure identical, muni-year Jaccard 1.0, r=0.99+ on all key
  variables; conflicts ~1.5% below snapshot, the expected effect of dropping the
  unreproducible v1-v4 archives, #15). 26 issues/bugs/deviations documented above.
  Both magic files are now regenerable artifacts. VTN steps 6-8 (which consume the
  scaffold outputs) are unblocked.

## Open provenance questions (need confirmation when raw data lands)

- `car_combined.dbf` (read in stage 00): RESOLVED (2026-07-14 Dropbox survey). It is a
  derived combine -- the full `car_combined.{shp,dbf,prj,shx}` (~1.7 GB) sits in the
  Dropbox at `data/cleaned/sicar/`. Treat as a documented snapshot input; note the
  chicken-and-egg wrinkle that stage 00 reads the combined attribute table while the
  combine itself descends from the cleaned shapes (legacy had the same circularity).
- The two scraped date CSVs (`cancelled_car_dates.csv`,
  `202310_report_data_registro_car.csv`): manual Python-scrape outputs with no
  producer script. Documented as given manual inputs (like the VTN fix-sheets in
  PROBLEMS.md section 3). Their scraper is not in scope for migration.
- Path convention mismatch: the existing scaffold `0_build_car_layers_from_raw.R`
  references `data/input/auxiliary/...`, but the on-disk data uses `data/input/aux/...`.
  The new stages follow the on-disk `aux/` layout. This mismatch in the existing
  scaffold should be reconciled separately.

## Migration status

Done (drafted, parse-checked in container, not yet run):
- `00_car_registration_years.R` <- `A_prepare_initial_data.R` s1-2
- `01_clean_car_shapes.R`        <- `A_prepare_initial_data.R` s3 (robust + s2 + light)
- `02_car_union_sensitive_land.R` <- `B_...R` S2 intersect loop + `consolidate_muni_year_variables_s2()`

Done (migration COMPLETE as of 2026-07-15):
- `03_intersect_individual_cars.R`   <- `C_...R` INTERSECT loops (robust + s2).
- `03b_consolidate_car_overlaps.R`   <- `C_...R` sections 7.0-7.5 (reproducible core,
  issues #15-#17 applied). Produces the two CAR-level files D reads.
- `04_consolidate_muni_year.R`       <- `D_consolidate.R` section 8 (issues #18-#20).
  Produces Magic File #2 (`muni_year_intersections.csv`).
- `05_combine_car_biome.R`           <- `2_empirics.R:255-307` (issue #21). Produces
  `car_combined_amazonBiome2.shp`, the input the existing scaffold
  `0_build_car_layers_from_raw.R` was blocked on.
- `analysis.mk`: full CAR chain wired with stamps (car00..car05); `make 01_car` runs
  00 -> 01 -> {02, 03 -> 03b, 05} -> 04 -> scaffold in dependency order.

Every stage parses in the pinned container, and the fragile transforms were verified
behaviourally on synthetic fixtures in-container (stages 03b, 04, 05).

## Test-run findings (2026-07-19, autonomous overnight session)

- **Stage 00 RAN and VALIDATED** (~5 min in container). Outputs: temas_ambientais_update.csv
  (242 MB, 892,630 rows) + car_and_reg_year_wide.csv (54 MB, 891,961 CARs, y2013-y2022).
  vs. Dropbox snapshot: 40 rows / 10 CAR ids difference (0.001%), and the snapshot has the
  pre-rename `cancelled` column with NO situacao_cadastro CA-fill -- the snapshot predates
  the final legacy code revision that we ported. Our output matches the final code.
- **Issue #18 EMPIRICALLY MOOT:** min(data_inscricao year) = 2014 in the real microdata;
  zero pre-2014 registrations, so the NA-poisoning path never triggers. n_unique_cars is
  safe as ported. (y2013 panel column exists and is all-zero, as in legacy.)
- Input downloads: all 9 states complete except PA (in progress); RR muni 1400704 absent
  from Dropbox source itself.

### Execution-hardening changes made during the first real run (2026-07-19 night)
All reproducibility-neutral (per-muni outputs are independent, skip-if-exists):
- **Worker sharding**: stages 02 and 03 accept an optional muni-list file argument to
  split the run across parallel containers; list order is honoured (lightest-first for
  memory-capped workers). No argument = original full-run behaviour; in stage 02,
  worker mode skips the Part B consolidation (a final full-mode run consolidates).
- **Chunked self-intersection** (stage 03): env CHUNK_ROWS blocks the left side of
  st_intersection(car, car) to bound peak memory on dense municipalities; the result
  rows and order are identical to the single call. Needed because dense Rondonia
  munis exhausted memory (>17 GB observed for muni 1100049; 1100130 OOM'd even with
  ~19 GB free). CHUNK_ROWS is an execution knob, not a data parameter.
- **municipal_boundaries input**: sourced as geobr 2010 simplified municipality
  layers (the geobr::read_municipality() default the legacy called at runtime),
  9 Amazonia Legal states, combined to data/input/aux/municipal_boundaries/
  municipal_boundaries.shp (code_muni + geometry only, to survive shapefile field
  abbreviation). One-time download; pipeline itself stays network-free.
- **Ops lesson**: `docker run ... | tail` masks the container exit code (tail's 0
  wins) -- an OOM-killed stage looked "completed". All runs now log to files and
  record the true exit code.

### Ops note: chunked-intersection memory has TWO regimes (learned mid-run)
CHUNK_ROWS bounds the *per-chunk* st_intersection peak (~5 GB for the densest munis at
CHUNK_ROWS=400). BUT `do.call(rbind, lapply(chunks))` accumulates every chunk's result
in a list and the final rbind + clean_shape (st_make_valid over the whole assembled
result) spikes ABOVE the per-chunk footprint. So a giant that runs steadily at ~5 GB
for 10 h can jump to ~10-12 GB at the very end. Practical rule for parallel giants:
give each a ~14 GB memory cap (not 10 GB) and keep enough host headroom that a giant's
end-of-muni spike can't collide with another giant's spike or push total actual RAM
past ~22 GB on this 24 GB machine. When a giant is within ~4 chunks of finishing,
don't launch extra lanes until it has written its CSV. (docker update --memory can
raise a running container's cap without restart — used to rescue two 10 h giants that
were about to OOM at assembly.)

## VALIDATION VERDICT (2026-07-23): REPRODUCTION SUCCESSFUL

Rebuilt muni_year_intersections.csv vs the Dropbox snapshot:
- **Structure/coverage: identical.** 5,022 rows both; 26 columns both; 558
  municipalities both; muni-year key Jaccard = 1.000 (perfect coverage).
- **Microdata-derived counts: exact.** n_CARs_microdata and n_new_CARs_microdata:
  correlation 1.0000, 100% exact match.
- **Geometry-derived variables: near-identical.** n_unique_cars r=0.9996 (issue #18
  empirically moot); n_unique_conflicting_cars r=0.9992; n_ovarlaps_33/66/99
  r=0.99-1.00; car_union_area r=0.9915; car_area_intersect_indi r=0.9999;
  conserve r=0.9989; forestA r=0.9990; muni_area r=0.9960. Median relative
  differences ~0 throughout; means within ~1%.
- **Issue #15 effect quantified:** our conflict counts run ~1.5% BELOW snapshot
  (mean n_unique_conflicting_cars 768 vs 780) — the expected signature of dropping
  the unreproducible v1-v4 archive gap-filler. Direction and size consistent.
- **Only weak spot:** n_ovarlaps_100 (the exact-100%-overlap bucket) r=0.37 but
  95.3% exact matches — it counts extremely rare events (mean 0.05-0.07 per
  muni-year) gated on exact float equality (int_perc == 100), so tiny geometry-
  engine differences flip a handful of cells; correlation is noise-dominated at
  these counts. Documented, not concerning.

Bottom line: the fully-from-raw, containerized, network-free rebuild reproduces the
published municipal panel to research-grade fidelity, with every deviation explained
and catalogued (issues #1-#24). Both magic files are now regenerable artifacts.

## Runbook: first test run (when raw data lands)

1. Populate inputs (paths expected by the scripts):
   - `data/input/sicar/shapefiles/` + `data/input/sicar/microdata/temas_ambientais.csv`
     <- Dropbox `data/raw/sicar/`
   - `data/input/sicar/car_combined.dbf` <- Dropbox `data/cleaned/sicar/car_combined.dbf`
   - `data/input/manual/car/{cancelled_car_dates.csv,202310_report_data_registro_car.csv}`
     <- Dropbox `data/processing/missing_car_dates/`
   - `data/input/aux/{municipalities_amazon_biome,amazon_biome_border,indigenous_area_amazon_biome,conservation_units_amazon_biome,cnfp}/`
     <- Dropbox `data/raw/terrabrasil/` + `data/raw/cnfp/`
2. `make 01_car` (host) -- runs the chain in the container via analysis.mk stamps.
3. Validate: diff the rebuilt `data/intermediate/car/muni_year_intersections.csv`
   against the Dropbox snapshot (`data/cleaned/municipal_level/muni_year_intersections.csv`),
   checking in this order: (a) muni-year coverage, (b) n_unique_cars for issue #18
   poisoning, (c) conflict counts for issue #15 (missing v1-v4) and #17 (allx typo)
   effects. Optionally also spot-check `CleanCARShapes_robust` against the Dropbox
   snapshot of the same folder for a few municipalities.

None of the migrated stages have been executed yet: the raw SICAR data is not on
disk (deferred to conserve laptop storage while MapBiomas runs). Stages are written
to be "ready to test" once inputs land.
