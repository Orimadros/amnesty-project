# Problems: Non-Raw Inputs Not Generated In-Repo

Date: 2026-05-04

This file flags inputs that are:
1. used as upstream inputs by scripts in this repo,
2. not "raw" in the strict provenance sense (already transformed/cleaned/derived),
3. and not generated anywhere by code in this repo at the same path.

## 1) Canonical Price Pipeline (high priority)

### A) Pre-serialized VTN inputs (`.rds`)
- **Paths expected by code**
  - `data/input/landvalues/vtn/vtn_2015.rds`
  - `data/input/landvalues/vtn/vtn_2016.rds`
  - `data/input/landvalues/vtn/vtn_2017.rds`
  - `data/input/landvalues/vtn/vtn_2018.rds`
  - `data/input/landvalues/vtn/vtn_2019.rds`
  - `data/input/landvalues/vtn/vtn_2021.rds`
  - `data/input/landvalues/vtn/vtn_2022.rds`
  - (directory form) `./../../data/input/landvalues/vtn`
- **Read in**
  - `code/patricio_preach_tomas_work/code/Tomas_VTN_processing/1_pre_clean_vtn_2019-22.R`
  - `code/patricio_preach_tomas_work/code/Tomas_VTN_processing/2_clean_vtn_data.R`
  - (also older/deprecated readers in `preach/` and `tomas_task_3/`)
- **Why flagged**
  - `.rds` is an already serialized R object format (not raw source format).
  - No script writes these files under `data/input/landvalues/vtn/`.

### B) CAR layers consumed by VTN step 6 are "cleaned/derived" and external to canonical chain
- **Path form expected by code**
  - `../../data/output` as datasource folder, layers:
  - `car_eligible_cleaned`
  - `car_ineligible_cleaned`
  - `already_treated`
- **Read in**
  - `code/patricio_preach_tomas_work/code/Tomas_VTN_processing/6_match_car_IHSregion.R`
- **Why flagged**
  - Layer names include cleaned/treated semantics (derived artifacts).
  - They are not produced by the canonical Tomas chain.
  - They are produced only in separate legacy scripts with absolute personal paths (not reproducible in current chain).

## 2) Additional Legacy-Branch Derived Inputs (not generated at same input path)

### C) Transition rasters used as inputs in empirics/policy
- **Paths expected by code**
  - `/Users/.../data/input/transitions_combined/*.tif`
  - `/Volumes/ElementsMain/transitions_combined/*.tif`
- **Read in**
  - `code/2_empirics.R`
  - `code/3_policy1.R`
- **Why flagged**
  - `transitions_combined` is a derived product (not raw source).
  - No script writes to `data/input/transitions_combined/` paths.
  - Some scripts write to different absolute processing paths (`data/processing/transitions_combined`), causing path inconsistency.

### D) "updated" SICAR microdata input
- **Path expected by code**
  - `/Users/.../data/input/sicar/microdata/temas_ambientais_update.csv`
- **Read in**
  - `code/2_empirics.R`
  - `code/3_policy1.R`
- **Why flagged**
  - File naming indicates post-processed update artifact (`*_update.csv`), not raw acquisition format.
  - No script writes this file at the input path used by those scripts.

## 3) Clarification (not flagged here)

- `code/patricio_preach_tomas_work/data/muni_division_2015/BRMUE250GC_SIR.*` is **not generated in-repo**, but it is a standard shapefile source snapshot (IBGE geometry source format), so it is treated as source input rather than derived intermediate.

## 4) Inputs Read From `output/` Paths (Pipeline Smell / Coupling Risk)

Using one script's `output` as another script's input is normal in a DAG, but in this repo it is often undocumented, path-inconsistent, or mixed with absolute personal paths. This section flags all such cases explicitly.

### A) Canonical Tomas chain: output-as-input edges (expected but tightly coupled)

- `code/patricio_preach_tomas_work/code/Tomas_VTN_processing/8_vtn_car_merge.R`
  - reads: `data/output/{eligible,ineligible,legal}_car.Rdata`
- `code/patricio_preach_tomas_work/code/Tomas_VTN_processing/9_joining_vtn.R`
  - reads: `data/output/{eligible,ineligible,legal}_car.Rdata`
- `code/patricio_preach_tomas_work/code/Tomas_NB_processing/5.0_tomas_task5.R`
  - reads: `data/output/{eligible,ineligible,legal}_car.Rdata`
- `code/patricio_preach_tomas_work/code/Tomas_NB_processing/6.0_tomas_task5.R`
  - reads: `data/output/{eligible,ineligible,legal}_car.Rdata`
- `code/patricio_preach_tomas_work/code/Tomas_Lavoura_processing_NB_merge/2.match_lavoura_data.R`
  - reads: `data/output/{eligible,ineligible,legal}_car.Rdata`
  - reads: `data/output/fnp_lavoura_2002_2017_with_state.csv`
- `code/patricio_preach_tomas_work/code/Tomas_Lavoura_processing_NB_merge/3.match_lavoura_data.R`
  - reads: `data/output/{eligible,ineligible,legal}_car.Rdata`
  - reads: `data/output/fnp_lavoura_2002_2017_with_state.csv`
- `code/patricio_preach_tomas_work/code/Tomas_Lavoura_processing_NB_merge/corr_by_area.R` (diagnostic)
  - reads: `data/output/parcels_NB_Lavoura/*_parcel_nb_lavoura_wide.rds`

Why this is a problem for migration:
- `data/output/` is serving as both "final outputs" and "intermediate inputs", so stage boundaries are ambiguous.
- clean repo should separate these into explicit `intermediate/` vs `final/` (or `build/`) locations with declared producer scripts.

### B) Broken/inconsistent output-as-input in canonical VTN step 6

- `code/patricio_preach_tomas_work/code/Tomas_VTN_processing/6_match_car_IHSregion.R`
  - sets `car_dir <- "./../../data/output"`
  - then tries to read layers `car_eligible_cleaned`, `car_ineligible_cleaned`, `already_treated` via `st_read(dsn=car_dir, layer=...)`

Why flagged:
- this expects GIS layers in `data/output`, but current repo snapshot stores CAR inputs mainly as `.Rdata` objects (`eligible_car.Rdata`, etc.).
- causes runtime failure (`try-error` fed into `st_join`) unless those GIS layers exist externally.

### C) Cross-pipeline absolute-path output input

- `code/2_empirics.R`
  - reads:
    - `~/Dropbox (Personal)/amazon_project/code/patricio_preach_tomas_work/data/output/parcels_NB_Lavoura/legal_parcels_all/legal_parcel_nb_lavoura_wide.rds`
    - `~/Dropbox (Personal)/amazon_project/code/patricio_preach_tomas_work/data/output/parcels_NB_Lavoura/eligible_parcels_all/eligible_parcel_nb_lavoura_wide.rds`
    - `~/Dropbox (Personal)/amazon_project/code/patricio_preach_tomas_work/data/output/parcels_NB_Lavoura/ineligible_parcels_all/ineligible_parcel_nb_lavoura_wide.rds`

Why flagged:
- hard-coded absolute home-directory output path prevents reproducible runs on another machine.
- this is a key integration edge (Tomas pipeline -> Pedro empirics), so it must be made relative and explicit in the clean DAG.

### D) Deprecated branches with output-as-input patterns

- `code/patricio_preach_tomas_work/code/tomas_task_2/explore_microregions.R`
  - reads: `../../data/output/car_eligible_cleaned.shp`
- `code/patricio_preach_tomas_work/code/tomas_task_3/3_explore_muni_car.R`
  - reads from `car_dir <- "./../../data/output"` (legacy shapefile-layer assumptions)
- `code/patricio_preach_tomas_work/code/tomas_task_3/5_vtn_car_merge.R`
  - reads: `data/output/{eligible,ineligible,ilegal}_car.Rdata`

Why flagged:
- these are legacy/parallel drafts and reinforce inconsistent conventions for what `data/output` contains.

## 5) Script-Level Cycle / Hidden Bootstrap State

There is a script-level cycle in the current legacy graph:

1. `code/2_empirics.R` (early/mid sections) writes CAR-derived artifacts (`car_eligible_cleaned`, `car_ineligible_cleaned`, `already_treated`, `control_final`) to absolute paths outside the canonical Tomas tree.
2. `code/patricio_preach_tomas_work/code/Tomas_VTN_processing/6_match_car_IHSregion.R` expects those CAR artifacts as inputs from `../../data/output` (layer-based `st_read`).
3. Downstream Tomas scripts produce `data/output/parcels_NB_Lavoura/*_parcel_nb_lavoura_wide.rds`.
4. `code/2_empirics.R` (later section) reads those `*_parcel_nb_lavoura_wide.rds`.

Why this is critical:
- this creates a practical `2_empirics.R <-> Tomas pipeline` loop at the script level.
- from a clean checkout, the pipeline is not reproducible unless precomputed outputs already exist in the filesystem.
- current runs rely on hidden bootstrap state (leftover files) rather than a fully acyclic, from-scratch DAG.

Migration implication:
- split `2_empirics.R` into at least two explicit stages:
  - an upstream CAR-geometry preparation stage (if still needed), and
  - a downstream final empirics stage.
- then make step 6 consume those declared stage outputs via stable relative paths (or remove step 6 dependency if `.Rdata` CAR objects are canonical).

## 6) Non-Idempotent Scripts (Re-run Hazards)

### A) `Tomas_VTN_processing/3_unmatched_vtn_correction.R` — 2015 row-delta hard-stop

- **Path in legacy:** `code/patricio_preach_tomas_work/code/Tomas_VTN_processing/3_unmatched_vtn_correction.R`
- **Path in new repo (verbatim port):** `code/01_build/02_vtn/3_unmatched_vtn_correction.R`

**Behavior**
- For each year, the script computes `row_diff = nrow(vtn_new) - nrow(vtn_orig)` and asserts:
  - `year == "2015"`: `row_diff` must be exactly `-1` (the EXTERIOR / `state == "EX"` row removal).
  - other years: `row_diff` must be exactly `0`.
- Both checks `stop()` on mismatch.

**Why flagged**
- A re-run on already-patched outputs removes 0 rows from the 2015 file (the EXTERIOR row was already deleted on the first run), so `row_diff == 0`, which fails the `-1` assertion and crashes the pipeline.
- The same fragile pattern applies to the 2018 ACEGUA state-label fix (`RR → RS`): it `message()`s "already correct" but the row-diff invariant for non-2015 years is `0`, which is consistent — the ACEGUA fix is incidentally idempotent. The 2015 EXTERIOR fix is not.

**Suggested fix (NOT applied yet — flag only)**
- Replace the delta-based assertion with an *invariant* check:
  - 2015: assert `nrow(vtn_new %>% filter(state == "EX" & mun_name == "EXTERIOR")) == 0` after the filter, regardless of how many rows were removed.
  - All years: assert that no row remains with `is.na(muni_code) & !is.na(key)` once we've joined the fix table (or whatever the actual semantic invariant of the patch is).
- This survives re-runs cleanly without losing the safety check.

**Status:** documented here; do not modify the script in this round. Fix to be planned in a later session, alongside any other idempotency cleanup of the VTN chain.
