# Dropbox review — Pedro's `amazonLandPrices_project` (2026-07-12)

Review of the shared Dropbox (`amazonLandPrices_project`, shared by "Pedro T"). Read
in-browser (Dropbox renders full text previews of `.R`/`.cpp` files, so no download was
needed). Goal: report what it contains, whether it holds a *newer, better-structured*
MapBiomas script that would make our migration unnecessary, and how it fits our work.

## Bottom line

- **No.** The only MapBiomas script in the Dropbox is `code/final/0_mapbiomas.R`, and it is
  the **legacy monolith** — the exact source our migration was based on. It is *less*
  structured than what we built, not more. **Our migration was necessary and is the cleaner
  version. Do not redo it.**
- The "newer / better-written" work the user sensed is **real, but on the CAR / magic-file
  side**, not MapBiomas: `code/create_muni_year_intersections/` is a clean, modular A→B→C→D
  refactor of the magic-file pipeline.
- Two concrete payoffs for us: (1) `0_mapbiomas.R`'s second half is the authoritative source
  for the **step-4 zone deforestation stats** we deferred; (2) the modular CAR pipeline is
  the authoritative source for the **magic file (`SicarMuniOverlap`)** and the CAR-cleaning
  logic tied to our current blocker.

## What the Dropbox contains

Full RA repo. Top level: `.Rproj.user/`, `archive/`, `code/`, `data/`, `docs/`, `graphs/`,
`lit/`, `misc/`, `output/`, `subprojects/`, plus loose files (a commented copy of the
magic-file script, some PDFs).

Relevant code:

| Path | What it is |
|---|---|
| `code/final/0_mapbiomas.R` | **Legacy MapBiomas monolith** — grids → legacy forest → per-year cover → transitions (C++) → plots → **deforestation zone stats**. Source of our migration. |
| `code/final/aux/deforestation_rules.cpp` | C++ transition kernel. Same file we already copied into our `aux/`. |
| `code/create_muni_year_intersections/` | **Modular magic-file pipeline** (see below). |
| `code/prep/create_SicarMuniOverlap_variables.R` | Older monolithic magic-file script. |
| `code/prep/vtnReceitaFederal.R` | Land-price (VTN / Receita Federal) prep. |
| `code/prep/scrape_sicar_incomplete.R`, `checking_tse.R` | SICAR scraping, TSE checks. |
| `code/infractions/aggregate_infractions.R` | IBAMA infractions aggregation. |
| `code/final/`, `code/output/` | Final analysis + output builders. |
| `subprojects/BH replication/`, `subprojects/permutations/` | Side analyses. |
| `code/archive/` | `trash.R`, `trash2.R` — ignore. |

## `code/final/0_mapbiomas.R` vs. our migration

It is the same algorithm we migrated, confirmed line-by-line on the parts that matter:

- **Same 713-grid tiling**: horizontal −74:−44, vertical −17:5, the `+/-0.00009` edge nudge.
- **Same legacy-forest definition**: forest = MapBiomas classes
  `{1,3,4,5,6,49,10,11,12,32,29,50,13}` in *both* 1985 and 1986.
- **Same per-year cover**: forest classes above; human =
  `{14,15,18,19,39,20,40,62,41,36,46,47,35,48,9,21,24,30}`.
- **Same (x,y)→character-key `data.table` merge** to align pixels across years.
- **Same C++ transition rules** (`deforestation_rules.cpp`): forest(1)/deforested(2)/
  reforested(3), "forest→human ⇒ deforested", sticky states, plus the
  `n_pixel_change_back_to_back` noise measure.

Where it is **worse** than our version (i.e., why the migration was worth doing):

- One ~1000-line script doing everything; no step separation.
- **Hardcoded absolute paths** (`/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-…`)
  throughout — not portable, not reproducible off his machine.
- **Manually chunked merges** (`for k in 2:153`, `154:250`, …) to dodge memory limits —
  brittle and hand-tuned.
- No containerization, no resumability, uncompressed raster writes.

Our `code/01_build/04_mapbiomas/` (steps 0–4, Dockerized, resumable, parameterized,
DEFLATE-compressed output) is strictly the more structured implementation. **Nothing to
port back for the core transition pipeline.** Best use of this file for us: as a **reference
oracle** to validate our outputs, and for the step-4 logic below.

## The genuinely newer work: `code/create_muni_year_intersections/`

A modular refactor of the magic-file (`create_SicarMuniOverlap_variables.R`) pipeline:

```
A_prepare_initial_data.R
B_intersect_car_union_sensitive_land.R   <- CAR cleaning + intersections (the core)
C_intersect_individual_cars.R
D_consolidate.R / D_consolidate2.R / D2_recompute_areas.R
helper_functions.R
verify_cars_within_municipality.R
create_SicarMuniOverlap_variables{_manual_clean,_workingmem,}.R   (variants)
```

`B_…R` (read in full) loads SICAR property shapefiles per municipality, cleans CAR geometry
(`st_make_valid`, validity filtering), intersects with forest / sensitive-land layers, and
computes areas per **municipality × year**, writing to `data/processing/SicarMuniOverlap/`.
Notable improvements over the monolith:

- **Relative paths** (`data/raw/sicar/…`) — portable.
- A **claim-based, resumable, parallel** "update-as-you-go" framework (`munis_already_claimed.csv`,
  skip already-completed munis) — multiple workers can run safely.
- Shared `helper_functions.R` (e.g. `load_forrest_data`, logging helpers).

This is the "better-written, newer" code the user remembered. It is the CAR/magic-file
side of the project, unrelated to the MapBiomas raster pipeline.

## How this fits what we've already done

1. **MapBiomas migration** — validated. Keep it; use `0_mapbiomas.R` only as a correctness
   reference. No rework.
2. **Deferred step-4 zone breakdowns** (glebas / control areas / CAR) — the second half of
   `0_mapbiomas.R` (the `#DEFORESTATION STATS` section) is the authoritative source. It
   builds: states ∩ biome; federal glebas (`i3geomap_glebas_federais`) with a
   `remove_overlaps_optimized()` routine; FPND (public undesignated forests, `cnfp/SHP_2013`);
   indigenous + conservation → `control_areas`; `glebas_net = glebas − indigenous − conservation`.
   This is exactly the zone logic our step 4 stubs out. It also explains the
   `control_areas2` "dangling legacy object" noted in PROBLEMS.md — it comes straight from
   this script.
3. **CAR magic-file blocker** (`CleanCARShapes_robust/*.shp` missing) — `create_muni_year_intersections/`
   (esp. `B_…R` and `helper_functions.R`) is where cleaned CAR shapes / `SicarMuniOverlap`
   are produced. Reading it may let us regenerate the cleaned CAR inputs ourselves rather
   than waiting on Pedro/Thiago. Worth a focused read before escalating the blocker.

## Suggested next steps

- Do **not** re-migrate MapBiomas.
- When we pick step 4 back up, port the zone-stats logic from `0_mapbiomas.R`'s
  `#DEFORESTATION STATS` section into `4_deforestation_stats.R` (containerized, relative
  paths, using our raw-data layout).
- Read `create_muni_year_intersections/{helper_functions.R, A_, B_, C_, D_}` end-to-end to
  (a) understand the magic file and (b) see whether we can regenerate `CleanCARShapes_robust`
  and unblock the CAR side.
