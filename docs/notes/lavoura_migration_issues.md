# Lavoura migration: issues, bugs, and deviations log

Date started: 2026-07-29
Status: steps 1-2 migrated and RUN successfully. Step 3 blocked on the NB/VNP track.

Companion to `docs/notes/car_migration_issues.md`, same convention: every place
where the migrated code in `code/01_build/03_lavoura/` deviates from the legacy
producer, plus bugs found in the legacy code while porting.

Legacy source:
`legacy_repo/code/patricio_preach_tomas_work/code/Tomas_Lavoura_processing_NB_merge/`
(`1.match_lavoura_data.R`, `2.match_lavoura_data.R`, `3.match_lavoura_data.R`).

## Canonical chain

```
VTN 0 -> VTN 5 -> VTN 6 ------> lavoura 1 -> lavoura 2   [-> lavoura 3, blocked]
(muni    (IHS     (CAR x region  (FNP prices  (attach prices
 mesh)    regions) join)          x regions)   to parcels)
```

Inputs that are documented "given" manual files (no producer, like the VTN
fix-sheets): `data/input/landvalues/vnp/Lavoura_FNP.xlsx` (FNP commercial land
price survey) and `data/input/landvalues/ihs_markit/IHS Markit S&P Jun23.xlsx`.
`data/input/muni_division_2015/` is a public IBGE download (Malha Municipal
Digital 2015, 1:250,000, SIRGAS 2000, layer `BRMUE250GC_SIR`), retrieved from
`geoftp.ibge.gov.br/.../malhas_municipais/municipio_2015/Brasil/BR/br_municipios.zip`.

## Issues

### Issue #L1 — `all_car_regions` is NOT a magic file (RESOLVED)
Legacy step 1 opens with `load("data/clean/all_car_regions.Rdata")` and no
producer is visible in the lavoura folder, which reads like another magic file.
It is not: `all_car_regions.Rdata` is an output of the already-migrated VTN step 6
(`code/01_build/02_vtn/6_match_car_IHSregion.R`), written to
`data/intermediate/car/all_car_regions.Rdata`. The migrated step 1 reads it from
there. No new provenance gap.

### Issue #L2 — 96 per-year-per-category CSVs replaced by a wide panel (DEVIATION)
Legacy step 2 writes, for each of 16 years x 3 categories, both a `*_car_lavoura_region_YYYY.csv`
(the FULL parcel table plus that one year's price column) and a
`*_missing_lavoura_region_YYYY.csv`. That re-serialises the entire parcel table
16 times per category -- ~96 files and several GB of near-duplicate data -- and
**nothing downstream consumes them**: step 3 reads step 1's master CSV, not these.
They are diagnostics.
Replaced with, per category, one `*_parcel_lavoura_wide.{rds,csv}` carrying all 16
price columns (`price_YYYY_lavoura`), plus a single
`lavoura_parcel_coverage_by_year.csv` giving matched/missing parcel counts per
year per category. Strictly information-preserving relative to the 96 files, and
the wide column naming matches what legacy step 3 constructs, so step 3 can consume
it directly when the NB track lands.

### Issue #L3 — redundant spatial join removed (OPTIMIZATION, semantics preserved)
Legacy step 2 loads `{eligible,ineligible,legal}_car.Rdata` -- which are VTN step 6
outputs that ALREADY carry `region_id`/`region_name`/`state` from a spatial join --
then drops those columns and redoes `st_join(region_poly, left = FALSE)`. The
migrated step 2 consumes VTN 6's join directly instead of repeating it.
One semantic detail preserved deliberately: VTN 6 joins with `left = TRUE` (parcels
outside every IHS region survive with `NA` region), whereas legacy step 2 used
`left = FALSE` (an inner join, dropping them). The migrated step 2 reproduces the
inner-join semantics by filtering `!is.na(region_id)`. Empirically this drops
exactly 1 parcel (in `legal`); the other two categories are fully covered.

### Issue #L4 — region-straddling parcels are duplicated (PRESERVED, quantified)
`st_join` emits one row per (parcel, intersecting region) pair, so a CAR parcel
crossing an IHS region border appears more than once, with a different region's
price attached to each copy. Legacy never deduplicated, and neither does the port
(faithful), but the effect is real and must be handled by any downstream
aggregation. Measured on the first successful run:

| category   | rows    | distinct parcels | dup rows | parcels in >1 region | max regions |
|------------|---------|------------------|----------|----------------------|-------------|
| eligible   | 165,940 | 164,223          | 1,717    | 1,710                | 3           |
| ineligible | 13,326  | 13,025           | 301      | 297                  | 3           |
| legal      | 7,796   | 7,311            | 485      | 431                  | 8           |

The distinct-parcel counts for `eligible` (164,223) and `ineligible` (13,025)
reconcile exactly with the CAR scaffold layer sizes, confirming the fan-out is the
only source of the extra rows. For `legal`, the scaffold writes 7,468 features but
only 7,311 distinct `cod_imovel`: the SNCI source itself repeats some ids, which is
upstream of this chain.
**Directive:** aggregate on distinct parcel id (or pick one region per parcel by a
documented rule) before computing any parcel-count or mean-price statistic.

### Issue #L5 — price sentinel handling (PRESERVED)
Legacy treats `0`, `"-"` and `""` as "no price recorded" (via
`as.numeric()` coercion plus an explicit `== 0` test) and this is applied only for
matching/reporting, never written back into the master CSV. Ported verbatim: the
master CSV keeps the workbook values as-is, and the NA-coercion happens in the
coverage report (step 1) and in the wide panel (step 2).

### Issue #L6 — footer rows in the FNP workbook (VERIFIED against the real file)
Legacy does `slice(1:(n() - 2))` to drop two footer rows after `skip = 3`.
Confirmed correct against the delivered workbook: sheet `Lavoura`, `skip = 3` lands
on the header (`Nº | REGIÃO | TIPO DE TERRA | 2002..2017`) and yields 135 rows, of
which the last 2 are footer notes -> 133 FNP regions. That 133 exactly matches the
133 distinct `region_id` values in `regions_2015` from VTN step 5, a useful
cross-check that the two sources share a region numbering.

### Issue #L7 — 8 of 29 CAR-bearing regions have no Lavoura price at all (DATA GAP)
VTN 6 finds 29 IHS regions containing CAR parcels. All 29 are present in the FNP
workbook, but only 21 carry a price, and the same 21 in every year 2002-2017 --
i.e. 8 regions are absent from the FNP survey entirely rather than intermittently.
Not a code defect; it is a coverage limit of the FNP source in the Amazon. It is
why mean per-parcel priced-year counts land near 11-13 of 16 rather than 16
(`lavoura_parcel_coverage_by_year.csv`).

### Issue #L8 — duplicate `region_id` guard (PRESERVED)
Legacy step 2 warns and keeps the first row when a `region_id` repeats in the
Lavoura table. Preserved. Note the master CSV can legitimately gain duplicate
`region_id` rows upstream: step 1 inner-joins the workbook against
`all_car_regions`, which is `distinct(state, region_id, region_name)`, so a region
spanning two states contributes two rows. Did not trigger on the current data.

### Issue #L9 — `file.exists()` on a list crashed VTN step 6 (FIXED, found here)
Not a lavoura bug, but discovered while running this chain for the first time.
`6_match_car_IHSregion.R:29` did `car_layers[!file.exists(car_layers)]` where
`car_layers` is a `list()`. `file.exists()` requires a character vector and errors
with `invalid 'file' argument`, so VTN 6 died before doing any work. Since VTN 6
had been migrated but never executed, this had gone unnoticed. Fixed to
`file.exists(unlist(car_layers))`. Ops note: the failure was initially masked
because `make ... | tail` returns tail's exit status -- the same trap recorded in
the CAR notes. All runs here log to a file and append an explicit `EXIT=$?`.

### Issue #L10 — step 3 was blocked on the NB/VNP track (RESOLVED 2026-07-29)
`3.match_lavoura_data.R` joins NB (VNP) prices alongside Lavoura and needs
`data/clean/vnp/city_region_yearly_pt{,_pre2015}.rds`. Those are now produced by
`code/01_build/05_vnp/` (see `docs/notes/vnp_migration_issues.md`), so step 3 is
migrated and run as `3_join_nb_lavoura_parcels.R`.

### Issue #L11 — `first(na.omit(x))` on an all-NA group (HARDENED)
Legacy step 3 collapses the stacked NB panels with
`summarise(across(where(is.numeric), ~ first(na.omit(.x))))`. When a price column is
entirely NA within a region key, `na.omit()` returns a zero-length vector and
`dplyr::first()` on that either errors or returns an implicit NA depending on version.
Given the NB panel is 94% empty (issue #V4), all-NA groups are common, not exotic.
Replaced with an explicit helper that returns `NA_real_` for the empty case. Same
result where legacy worked, no crash where it didn't.

### Issue #L12 — wide table written as RDS only, plus a compact long panel (DEVIATION)
Legacy step 3 wrote both `.rds` and `.csv` of the joined table. After the NB join that
table is ~165,940 rows x ~1,700 mostly-empty columns for the eligible category alone;
as CSV that is roughly 10 GB of `,NA,`. The `.rds` is kept (gzip-compressed, 233 MB for
eligible -- it is what `2_empirics.R:2313` reads, so the faithful wide form survives)
and the CSV of the wide table is dropped.
Added instead `<cat>_parcel_price_panel.csv`: one row per (parcel, year) with the mean
NB price across that region's land types and the single Lavoura price. This is the form
`2_empirics` actually derives anyway (it does `rowMeans` over `preco_*_<year>` columns),
so it is both smaller and closer to what downstream code wants.

### Issue #L13 — NB prices sit systematically BELOW Lavoura (COMPOSITION, not a defect)
The rebuilt comparison shows a consistent, widening gap (eligible category, mean
NB - Lavoura): -656 R$/ha in 2002 growing to -5,489 in 2017.
This is expected and is NOT evidence of a merge bug. Lavoura is specifically *cropland*
(`Terra agrícola de grãos`, etc.), the most valuable class, while the NB per-year figure
averages across every land type recorded for the region -- including standing forest
(`mata`) and `cerrado`, which are much cheaper. The two series measure different baskets.
**Directive:** do not treat NB and Lavoura as interchangeable price series, and do not
"correct" the gap. If a like-for-like comparison is wanted, restrict the NB average to
comparable land types before differencing.

### Issue #L14 — the two sources are genuinely complementary (COVERAGE)
Lavoura alone leaves 43,695 of 165,940 eligible parcels unpriced in 2017 (it covers 21
of 29 CAR-bearing regions). NB covers a different 17 of 29. Together, only **2,459
parcels (1.5%)** have no price from either source in any year (ineligible 4.6%, legal
2.7%). This is the main practical payoff of migrating the NB track.
Note the era break from issue #V4 is visible here too: the share of parcels carrying
BOTH prices drops from 69.5% (2002-2015) to 57.9% (2016-2017).

## Migration status

Done (written, run, outputs verified):
- `1_match_lavoura_regions.R` <- `1.match_lavoura_data.R`
- `2_match_lavoura_parcels.R` <- `2.match_lavoura_data.R`
- `analysis.mk`: `lavoura` target wired with stamps (`lavoura01`, `lavoura02`),
  depending on the VTN-6 stamp.

Blocked:
- step 3 (NB + Lavoura parcel comparison) -- see issue #L10.
