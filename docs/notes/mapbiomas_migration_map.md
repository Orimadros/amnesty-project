# MapBiomas migration map (`1_mapbiomas.R`)

Date: 2026-06-25
Status: planning document (Route C — no code changes yet)
Source: `legacy_repo/code/1_mapbiomas.R` (1,528 lines, monolithic)

This is a structural map of the legacy MapBiomas script, produced before migrating it
into `code/01_build/04_mapbiomas/`. The goal is to separate the **canonical pipeline
backbone** from **exploratory plotting** and **scratch/dead code**, and to propose a
modular script breakdown.

Migration rule (from CLAUDE.md): do NOT change substantive logic. Only structure,
paths (`here::here()`), explicit I/O, and DAG wiring.

---

## What the script produces (the point of it all)

The star output is **`transitions_combined/`** — a per-pixel, per-year record of forest
state across the Amazon biome, coded as:

- `1` = forest
- `2` = deforested
- `3` = reforested
- `0` = never-forest / human cover since the 1985–86 baseline

Everything in the backbone exists to build that. Downstream, the script also computes
aggregate deforestation tables and (in a blocked section) overlays CAR property parcels.

---

## Raw inputs (not in repo — live on Pedro's Dropbox today)

| Input | What it is |
|---|---|
| `brasil_coverage_<year>.tif` (1985–2022) | MapBiomas annual land-cover rasters |
| `amazon_biome_border.shp` | Outline of the Amazon biome |
| `BR_UF_2021.shp` | Brazilian state boundaries (for stats) |
| `glebas_federais/*.shp` | Federal public land parcels (for control areas) |
| `cnfp/*.shp` | Public-forest shapefiles (control areas) |
| `deforestation_rules.cpp` | C++ helper implementing the transition logic |
| `car_eligible_cleaned.shp`, `car_all_cleaned.shp` | CAR parcels — **BLOCKED dependency** |
| `mapbiomas_amazon_official.xlsx` | External benchmark table (validation only) |

---

## Section-by-section breakdown

Legend: **[BACKBONE]** = core pipeline, must migrate · **[PLOT]** = exploratory,
archive/separate · **[CAR]** = blocked by the magic-file problem · **[SCRATCH]** =
dead/duplicate re-run code, do not migrate.

| Lines | Classification | What it does |
|---|---|---|
| 1–46 | setup | Load ~40 libraries; load first raster + biome border |
| 47–85 | **[BACKBONE]** | Tile the 1985 & 1986 rasters into grid `.rds` files |
| 87–111 | **[BACKBONE]** | Build **legacy forest** baseline (pixel was forest in both 1985 & 1986) |
| 113–211 | **[BACKBONE]** | Merge legacy grid tiles → one `legacy_forest.tif` (chunked merges) |
| 213–305 | **[PLOT]** | Crop/mask legacy forest to biome and merge for a figure |
| 308–346 | **[BACKBONE]** | Tile the 1987–2020 rasters into grid `.rds` files |
| 347–409 | **[BACKBONE]** | Classify each pixel as `forest` / `human` per year |
| 412–479 | **[BACKBONE] ★** | Compute year-by-year transitions via the C++ helper → write `transitions_combined/` |
| 482–581 | **[PLOT]** | Crop/mask/merge transitions rasters for figures |
| 582–759 | support | Load aux layers (states, glebas, CNFP forests); overlap-erase helpers for control areas |
| 760–895 | stats | Build deforestation tables: biome total, glebas, control areas, net |
| 898–963 | **[CAR]** | Overlay `car_eligible` / `car_ineligible` on transitions → per-group deforestation stats — **BLOCKED** |
| 969–999 | stats/PLOT | Compare to `mapbiomas_amazon_official.xlsx` benchmark + comparison plot |
| 1000–1300 | **[PLOT]** | Assorted maps/figures (`ggplot`, `ggsave`, `tikz`) |
| 1300–1528 | **[SCRATCH]** | Duplicate re-run blocks pointing at `~/Downloads` and `/Volumes/ElementsMain` (second machine / external drive). Not canonical. |

Observed scale of the mess: 27 `writeRaster`, 10 `saveRDS`, 6 `ggplot`, 3 `ggsave`,
14 references to `~/Downloads`, 5 to `/Volumes` — every path hardcoded to one machine.

---

## Proposed module breakdown (target: `code/01_build/04_mapbiomas/`)

The backbone collapses to **~4 focused scripts**, plus the C++ helper, plus a quarantine
bucket for plots and a deferred CAR script.

| New script | Replaces (legacy lines) | Job |
|---|---|---|
| `0_tile_rasters_to_grids.R` | 47–85, 308–346 | Crop each yearly raster to the biome and tile into grids (1985–2020 in one parameterized loop) |
| `1_build_legacy_forest.R` | 87–211 | Build the 1985–86 legacy-forest baseline and merge to one layer |
| `2_classify_cover.R` | 347–409 | Classify every pixel `forest`/`human` per year |
| `3_compute_transitions.R` ★ | 412–479 | Apply `deforestation_rules.cpp` → write `transitions_combined/` |
| `4_deforestation_stats.R` | 760–895, 969–999 | Aggregate deforestation tables + benchmark comparison |
| `aux/deforestation_rules.cpp` | (external) | The C++ transition logic, migrated verbatim |
| `5_car_deforestation_stats.R` | 898–963 | **DEFERRED** — CAR overlay; blocked by the magic file |
| (archive, not in DAG) | 213–305, 482–581, 1000–1300 | Exploratory plots — move to a `plots/` or `archive/` area, not the pipeline |
| (delete) | 1300–1528 | Scratch re-run blocks — do not migrate |

So: **4 backbone scripts + 1 C++ helper to run end-to-end now**, 1 CAR script parked
until the magic file resolves, and the plotting/scratch code separated out.

---

## Open questions to resolve before coding

1. **Are the raw inputs available to us?** The `brasil_coverage_*.tif` files and
   `amazon_biome_border.shp` are not in the repo. Without them we can refactor the code
   but cannot run/validate it. (MapBiomas rasters are public downloads.)
2. **Grid bounds and CRS are hardcoded** (`-74:-44` horizontal, `-17:5` vertical,
   `EPSG:4326`). Keep as-is (logic) but lift into named constants.
3. **The C++ helper path** is hardcoded to a Dropbox `code/final/aux/` location; it must
   be migrated into the repo and sourced via `here::here()`.
4. **Validation target:** do we have any reference `transitions_combined` output to
   diff against, or do we validate structurally (pixel counts, value distributions)?
