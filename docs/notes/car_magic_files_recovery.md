# CAR magic files: discovery, chain map, and migration plan

Date: 2026-07-06
Status: discovery write-up + high-level migration plan (no code migrated yet)

## TL;DR

The two "magic files" that block the CAR side of the pipeline —
`CleanCARShapes_robust/*.shp` and `muni_year_intersections.csv` — were flagged in
`PROBLEMS.md` as having **no producer script in the repo**. They were never
unproducible: their producer code lives in the **original `amazonLandPrices_project`
Dropbox**, in `code/create_muni_year_intersections/`, and was simply never committed
to git. This note maps that producer chain — every input, intermediate, and final
output, in plain terms — and sketches how we migrate it into the repo.

Where the code was found (Dropbox):
`amazonLandPrices_project/code/create_muni_year_intersections/`
(scripts dated Sep 2024 — newer than the `legacy_repo/` copies).

---

## Vocabulary (plain-language)

- **CAR** (*Cadastro Ambiental Rural*): Brazil's mandatory environmental registry of
  rural properties. Each entry is a **self-declared** property boundary — not a legal
  title. Owners register to declare how much of their land is forest/degraded.
- **SICAR**: the government database/system that holds the CAR entries.
- **Shapefile**: a bundle of files describing map shapes — polygons defined by
  coordinates (`.shp`) plus an attribute table (`.dbf`). One CAR = one polygon + its
  fields (ID, status, declared area).
- **"Clean" a shapefile**: fix geometric defects (self-intersections, duplicate
  vertices, empty shapes) so spatial math (areas, intersections) runs correctly.
- **"Magic file"**: a file the pipeline needs but the repo cannot explain — no
  producer script, no documented source. Dangerous because the project looks runnable
  while secretly depending on someone's old machine state.

---

## The big picture

There is **one upstream cleaning step** that both magic-file chains depend on, then
the chains fan out:

```
                 raw SICAR property shapefiles (self-declared boundaries)
                              |
                              v
         [ A_prepare_initial_data.R  §3  +  helper_functions.R ]
              clean each municipality's CAR polygons
                              |
             +----------------+------------------+
             v                                   v
   CleanCARShapes_robust/*.shp          (overlap variables per CAR)
   (MAGIC FILE #1)                               |
             |                                   v
             v                        [ B_ , C_ , D_consolidate.R ]
   car_combined.shp                    muni_year_intersections.csv
             |                          (MAGIC FILE #2)
             v
   car_combined_amazonBiome2.shp
   (feeds car_eligible / car_ineligible -> VTN 6-8, Lavoura, empirics)
```

---

## Chain 1 — `CleanCARShapes_robust` (the CAR-cleaning step)

**Producer:** `A_prepare_initial_data.R`, section 3 (`if(fp$CLEAN_CARS)`), using the
`clean_shape_*` functions defined in `helper_functions.R`. **Confirmed** by reading
both files.

### Inputs (raw / given)
- `data/raw/sicar/shapefiles/{UF}/SHAPE_{muni}/AREA_IMOVEL/` — the **raw CAR property
  polygons**, one folder per municipality. This is the untouched download from SICAR.
- `data/cleaned/sicar/car_combined.dbf` — the **attribute table** of all CARs (IDs,
  status `SITUACAO`, declared area). Used to list which CAR IDs exist and which were
  cancelled.
- `data/raw/sicar/microdata/temas_ambientais.csv` — **registration microdata**: per-CAR
  dates and status. Used to know when each CAR was registered.

### Steps (how it's performed)
1. **Build a registration-year panel** (section 2). Combine the CAR list with the
   microdata; fill in missing/cancelled registration dates (some were scraped
   separately with Python). Output: `temas_ambientais_update.csv` and
   `car_and_reg_year_wide.csv` — a table saying, for each CAR, which years it existed.
2. **Clean each municipality's CAR polygons** (section 3). Loop over municipalities;
   read the raw property shapefile; run three cleaning passes (from
   `helper_functions.R`):
   - `clean_shape_basic()` — make geometries valid, drop empties, **keep** duplicates.
   - `clean_shape_reenforced()` — make valid with the stricter GEOS `valid_structure`
     method, drop duplicates, empties, **and** still-invalid shapes. **This is the one
     that produces `CleanCARShapes_robust`.**
   - `clean_shape_s2()` — an s2-geometry variant.
   A skip-if-exists framework tracks which municipalities are already done, so the run
   is resumable.

### Intermediate / final outputs
- `data/processing/CleanCARShapes_light/muni{code}/light{code}.shp`
- `data/processing/CleanCARShapes_robust/muni{code}/robust{code}.shp`  ← **MAGIC FILE #1**
- `data/processing/CleanCARShapes_s2/muni{code}/s2_{code}.shp`

### Downstream of the magic file (in legacy `2_empirics.R`)
- Combine all per-municipality `robust*.shp` into one `car_combined.shp`.
- Intersect `car_combined` with the Amazon biome border → `car_combined_amazonBiome2.shp`.
- That feeds `car_eligible_cleaned.shp` / `car_all_cleaned.shp`, which the VTN 6–8,
  Lavoura, and empirics steps consume.

---

## Chain 2 — `muni_year_intersections.csv` (the overlap consolidation)

**Producer:** the ordered `A_ -> B_ -> C_ -> D_consolidate.R` scripts in the same
folder. `D_consolidate.R` writing the final CSV is **confirmed** by reading it; the
B/C roles are inferred from their names and the data files D reads.

### Steps (how it's performed)
1. `A_prepare_initial_data.R` — prepare raw data and clean the CARs (Chain 1 above).
2. `B_intersect_car_union_sensitive_land.R` — intersect the (unioned) CAR area of each
   municipality with **sensitive land**: indigenous territories, conservation units,
   and public forest. Produces per-CAR overlap-area variables. *(role inferred from name)*
3. `C_intersect_individual_cars.R` — intersect **individual CARs with each other** to
   measure overlap/conflict between properties. *(role inferred from name)*
4. `D_consolidate.R` — read the cleaned overlap tables and roll everything up to the
   **municipality × year** level, then write the final file. **Confirmed** line:
   ```r
   fwrite(x = ., file = paste0(dir_wd, "data/cleaned/municipal_level/muni_year_intersections.csv"))
   ```

### Intermediate inputs D reads (produced by A/B/C)
- `data/cleaned/sicar_overlap_variables_exapanded_s2.csv` — municipal CAR-vs-sensitive-land
  overlap variables (expanded to all muni-years).
- `data/cleaned/CAR_overlap_variables_reftarget_areas.csv` — CAR-to-CAR conflict areas
  (reference vs target CAR).
- `data/cleaned/CAR_overlap_variables_final.csv` — all CAR overlap variables.

### Final output
- `data/cleaned/municipal_level/muni_year_intersections.csv` (and `.xlsx`)  ← **MAGIC FILE #2**
  A municipality × year panel of CAR area and overlap variables. Consumed by
  `legacy_repo/code/x_aggregate_infractions.R`.

---

## How we migrate this (general plan)

The goal (per the repo's rules): make both chains reproducible from raw inputs +
documented manual corrections, runnable in Docker via one `make` command, with no
hardcoded personal paths and no runtime network calls.

1. **Vendor the code.** Copy into `code/01_build/01_car/` (as its "stage 0"):
   `A_prepare_initial_data.R`, `B_...R`, `C_...R`, `D_consolidate.R`,
   `helper_functions.R`, and the two helper scripts currently `source()`d from GitHub
   (`helper_functions_simple.R`, `helper_functions_brazil.R`) — saved as **local files**,
   not fetched at runtime.
2. **Vendor the raw data.** Bring the raw SICAR shapefiles, `temas_ambientais.csv`, and
   `car_combined.dbf` from the Dropbox `data/` folder into the repo's `data/input/`
   (gitignored). Document them as source snapshots.
3. **Normalize for reproducibility.** Replace hardcoded `dir_wd`/Dropbox paths with
   `here()`; remove the network `source()` calls; un-gate the `if(FALSE){ ... }` block
   in `A_prepare` §3 that currently disables the robust/s2 saving; split the monoliths
   into single-job scripts.
4. **Wire into the DAG.** Add a CAR-cleaning stage to `analysis.mk` upstream of the
   existing `0_build_car_layers_from_raw.R`, with stamps.
5. **Test small, then scale.** Run one state/municipality first (fast feedback), verify
   the cleaned shapes and the consolidated CSV look right, then run the full set.
6. **Validate.** Compare regenerated `CleanCARShapes_robust` and
   `muni_year_intersections.csv` against the existing snapshot files.

---

## Caveats / open items

- **Gated code**: the robust/s2 saving in `A_prepare` §3 sits inside `if(FALSE){...}`
  (toggled off after the one-time run). Un-gating is required to regenerate.
- **External network dependency**: `helper_functions.R` `source()`s two scripts from
  `raw.githubusercontent.com/Thiago-Alckmin/...` at runtime — must be vendored locally
  for reproducibility.
- **Raw data volume**: the raw SICAR shapefiles are large; plan storage under the
  gitignored `data/input/`.
- **B/C exact outputs unconfirmed**: roles of `B_` and `C_` are inferred from names and
  the files `D_consolidate.R` reads; confirm by reading them before migrating.
- **Manual/scraped pieces**: some cancelled-CAR dates came from a separate Python scrape
  (`cancelled_car_dates.csv`, `202310_report_data_registro_car.csv`) — treat as
  documented manual inputs.
