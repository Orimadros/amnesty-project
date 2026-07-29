# NB/VNP migration: issues, bugs, and deviations log

Date started: 2026-07-29
Status: producers (legacy 1.0, 2.0) migrated and RUN. Reporting scripts 3.0-6.0 not
migrated -- see issue #V7.

Companion to `docs/notes/car_migration_issues.md` and
`docs/notes/lavoura_migration_issues.md`, same convention.

Legacy source:
`legacy_repo/code/patricio_preach_tomas_work/code/Tomas_NB_processing/{1.0..6.0}_tomas_task5.R`

## What this chain is

"NB" / "VNP" is the FNP *North Brazil* commercial land-price survey, a second price
source alongside Lavoura. The workbook
`data/input/landvalues/vnp/Land Price_North Brazil_FNP.xlsx` has two sheets covering
two eras, and the two producer scripts turn each into a wide
`(state, region_name) x preco_<land_type>_<year>` panel.

```
Land Price_North Brazil_FNP.xlsx
  ├── sheet "FNP até 2015"        -> 2_city_region_panel_pre2015.R -> city_region_yearly_pt_pre2015.{csv,rds}
  └── sheet "FNP 2016 em diante"  -> 1_city_region_panel_2016on.R  -> city_region_yearly_pt.{csv,rds}
```

Both land in `data/clean/vnp/`, the exact paths Lavoura step 3 expects.

## Issues

### Issue #V1 — runtime `install.packages()` removed (VENDORED)
Both legacy scripts open with an `installed.packages()` check that calls
`install.packages(..., repos = "https://cloud.r-project.org")` at runtime. That is a
network call, it writes to the container's ephemeral layer, and `CLAUDE.md` forbids it
outright. Removed; all four packages (`readxl`, `tidyverse`, `janitor`, `fs`) are
already in `renv.lock`.

### Issue #V2 — hardcoded personal Dropbox path (NORMALIZED)
Legacy read from
`/Users/carolinamelloneetlin/Dropbox/amazon_project/data/input/landvalues/vnp/Land_Price_North_Brazil_FNP.xlsx`.
Replaced with `here("data","input","landvalues","vnp", ...)`. Note the legacy filename
uses underscores (`Land_Price_North_Brazil_FNP.xlsx`) but the file as delivered from the
`amazonLandPrices_project` Dropbox is `Land Price_North Brazil_FNP.xlsx` (spaces).
The migrated code uses the real name.

### Issue #V3 — the two scripts were duplicates (DE-DUPLICATED)
`1.0` and `2.0` are the same 110-line body differing only in `sheet` and `out_stub`.
The shared body now lives once in `_helpers_vnp.R::build_city_region_panel()`, with two
thin callers. Behaviour is unchanged.

### Issue #V4 — the two sheets use INCOMPATIBLE land-type conventions (MAJOR, PRESERVED)
This is the most important finding of the port and it is a property of the source data,
not of the code.

| sheet | rows | regions | distinct land types | price cols | filled |
|---|---|---|---|---|---|
| FNP até 2015 | 121 | 18 | **106** | 1,590 | 6.3% |
| FNP 2016 em diante | 166 | 66 | **11** | 77 | 16.6% |

The pre-2015 sheet bakes a qualifier into the land-type string itself -- `Cerrado`,
`Cerrado (Cerejeiras)`, `Cerrado (Humaitá)`, `Mata (Apuí)`, `Mata (Rio Branco)`, ... --
so 106 distinct strings each become their own `preco_<land>_<year>` column, giving a very
wide, very sparse table. The 2016+ sheet instead normalises land type to 11 clean
categories (`Cerrado`, `Floresta Amazonica`, `Pastagem Formada`, ...) and moves the
qualifier into separate `yield` / `detail` columns that exist only on that sheet.

**Consequence:** the two panels are NOT column-compatible, and any series spanning the
2015/2016 boundary has a *composition* break, not just a price change. Concretely, for
Rio Branco the only pre-2015 series is `preco_mata_(rio_branco)_*` (standing forest,
~125 R$/ha in 2001) while the 2016+ series is `preco_pastagem_formada_*` (established
pasture, ~6,000 R$/ha in 2016). Most of that ~40x gap is land type and inflation, not a
price shock.

Ported faithfully (no attempt to reconcile the two conventions), because reconciling
would change published numbers and is an analytical decision, not a migration one.
**Directive:** before using NB prices across 2015/2016, either restrict to one era, or
harmonise land types explicitly (e.g. strip the parenthetical qualifier in the pre-2015
sheet and re-aggregate) and document the choice.

Note that Lavoura step 3 partially masks this: it takes `rowMeans` over all
`preco_*_<year>` columns per year, so the fragmentation averages out within a year --
but the *set* of land types being averaged still differs across the boundary.

### Issue #V5 — `NaN` from all-NA groups normalised to `NA` (DEVIATION, safe)
`mean(x, na.rm = TRUE)` returns `NaN` when every value in the group is NA, and legacy
wrote those `NaN`s straight into the panel. `NaN` is not caught by `is.na()` in some
downstream idioms and prints confusingly. The migrated helper converts `NaN -> NA_real_`
after the yearly aggregation. This only affects cells that carried no information either
way.

### Issue #V6 — year extraction verified against the real headers (NO CHANGE NEEDED)
The legacy two-step extraction (prefer a 4-digit `19xx|20xx`, else `2000 + last two
digits`) looked fragile against headers like `jan_fev_010` and `nov_dez_015`. Checked
empirically across both sheets: **123 price columns, 0 unparsed, every year in
2001-2022 and in plausible range.** The 3-digit tails resolve correctly through the
fallback branch. Ported unchanged.
Incidental source facts: 2018 has only 4 bimonthly columns (no jan/feb, no jul/aug) and
2022 uses a different naming scheme (`jan_mar_22`, `abril_jul_22`, `jul_setr_22`,
`out_dez_22`). Both parse fine.

### Issue #V7 — reporting scripts 3.0-6.0 NOT migrated (SCOPED OUT, deliberate)
Legacy `3.0`/`4.0` are region-level match reports and `5.0`/`6.0` are parcel-level joins,
for the 2016+ and pre-2015 panels respectively. They are diagnostics: nothing downstream
reads their outputs, and they replicate for NB exactly what Lavoura steps 1-2 already do
for Lavoura -- including the same per-year CSV explosion that was replaced in Lavoura
issue #L2. `5.0`/`6.0` in particular are subsumed by Lavoura step 3, which joins BOTH
price sources to CAR parcels in one pass and IS consumed downstream
(`2_empirics.R:2313`).
Not migrated. If the per-era diagnostics are ever wanted, build them from the panels
plus `all_car_regions`, following the Lavoura step 1 coverage-report pattern.

### Issue #V8 — NB covers 17 of 29 CAR-bearing regions (DATA GAP)
Matching normalised `region_name` against the 29 IHS regions that contain CAR parcels:
**17 match, 12 do not.** The 12 misses are every Mato Grosso region (Alta Floresta,
Aripuanã, Barra do Garças, Cáceres, Cuiabá, Pontes e Lacerda, Sinop, Tangará da Serra,
Vila Rica) plus Maranhão (Bacabal, Imperatriz, São Luís) -- consistent with a survey
titled "North Brazil", which does not cover the centre-west or north-east.
For comparison, Lavoura covers 21 of 29 (issue #L7). The two sources are complementary,
which is presumably why legacy step 3 merges them.
Also: 15 of the 18 pre-2015 regions reappear in the 2016+ sheet; `Baixo Amazonas`,
`Belém` and `Ilhas` are pre-2015 only.

### Issue #V9 — non-syntactic column names in the pre-2015 panel (ACCEPTED)
Because land types carry parentheses, the pre-2015 panel has column names like
`preco_mata_(rio_branco)_2001`. These are legal tibble names, survive the csv/rds round
trip, and still match Lavoura step 3's `^preco_.*_\d{4}$` selector. Left as-is to keep
the column names traceable to the source land-type strings; anything consuming them
should use backticks or `.data[[...]]`.

### Issue #V10 — directory numbered 05_vnp, not 04_vnp (DEVIATION)
`PROBLEMS.md` §4 proposed `code/01_build/04_vnp/`, but `04_mapbiomas` now occupies that
prefix. Used `code/01_build/05_vnp/` to avoid two modules sharing a number. The numbers
are labels, not execution order (MapBiomas is independent; VNP feeds Lavoura step 3).

## Migration status

Done (written, run, outputs verified):
- `_helpers_vnp.R` (shared panel builder)
- `1_city_region_panel_2016on.R`  <- `1.0_tomas_task5.R`  -> 66 regions x 77 cols, 2016-2022
- `2_city_region_panel_pre2015.R` <- `2.0_tomas_task5.R`  -> 18 regions x 1,590 cols, 2001-2015
- `analysis.mk`: `vnp` target wired with stamps (`vnp01`, `vnp02`)

Not migrated: legacy `3.0`-`6.0` (issue #V7).

Unblocked by this: **Lavoura step 3** (`3.match_lavoura_data.R`), which needs exactly
the two `.rds` files now produced.
