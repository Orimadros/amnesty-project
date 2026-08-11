# Checkpoint 2026-08-10/11 (overnight) — Dropbox deep fetch + second-half migration

Handoff: "read docs/notes/checkpoints/checkpoint-20260810-overnight.md and
continue." Supersedes checkpoint-20260807.md for CURRENT WORK; that file still
documents the DiD/SUTVA/Policy-Jump state. Branch integration/car-mapbiomas.
Standing rules unchanged (CLAUDE.md + code-over-paper + no PR/email suggestions).
User authorized: blanket downloads from Pedro's two shares (any size), migrate
only recovered code, port price side to the data walls.

## Recovered tonight (data/legacy_dropbox/, gitignored; fetch2/ holds raw zips)

- **input_terralegal/**: DadosTerraLegal.csv + 9 parcelageo_*AS_WKT.csv — the
  ENTIRE takeup input set (was top of the Pedro ask-list).
- **pptw_data_clean/**: vtn_2015..2022_clean.rds (the vtn_YYYY.rds ask-list
  item!), vtn_region_2015-2022.csv, city_region_yearly_pt[_pre2015] (their VNP
  panels — validate ours against them), IHS_regions_divison.Rdata,
  all_car_regions.Rdata.
- **pptw_data_output/parcels_NB_Lavoura/**: all four parcel_nb_lavoura_wide.rds
  (legal/eligible/ineligible/ilegal, July-2025 vintage) — tab:6 inputs. The full
  4GB zip (683 files: car_vtn/vnp_IHS breakdowns, fnp_vs_car_regions by year)
  sits in fetch2/pptw_data_output.zip, only the wides extracted so far.
- **fines_robustness/Clear Spots - Shapefile/**: all 149 monthly scenes.
- **data_root/**: CAR_eligible_defo_1989..2001.rds (2002-2004 blocked by the
  anonymous listing cap; bridged from our panels in stage 24).
- .Rhistory files (fines = V1/V2 cloud session; miseEnPlace = the RegsFE
  Policy-Jump interactive run — resolves the rbind question: it ran),
  docs/ (16MB, documentation incl. an API key file — do not commit), working/,
  permutations skeleton, mise_archive (TestingCodeLiu.R + SICAFI csvs),
  alp_code/ (Dropbox 2 code tree incl. prep/vtnReceitaFederal.R).
- input_titles.zip (2GB+, SIGEF/SNCI/SNCR) downloading; miseEnPlace_full.zip
  queued behind it (needed: Munic_Micro_Meso_Region_Codes.csv,
  temas_ambientais_update.csv, and the LAST possible hiding place of any
  prices/takeup do-file); input_landvalues.zip + input_microregions.zip retrying;
  input_auxiliary.zip TRUNCATED/corrupt (uf_uf_id.xlsx must be re-fetched
  individually — needed by stage 24).

## New stages (committed)

- **23_visible_fraction.R** — V3:396-611 port on Clear Spots; rebuilds
  visible_fraction (surviving nowhere) and tests the per-file yearly-mean rule
  against enforcement_clouds.dta. RUNNING at checkpoint time
  (build/logs/stage23.log).
- **24_takeup.R** — full 3_policy1.R port (pipelines 0-3, applies/receives at
  >=90% overlap, 1989-2008 occupation history with EMP_BRIDGE_0204). NOT YET
  RUN — waits on titles + miseEnPlace_full + uf_uf_id.xlsx.
- **25_prices_reg.R** — 2_empirics.R:2306-2580 port. RAN: 29 regions x
  2002-2017, 418 priced region-years, shares5 + less_1500 written. Walls:
  yearly_average_price_region + turnover producers missing from every recovered
  script (price_north = price_lavoura per the :2573 NA-fallback); tab:6 Stata
  do-file unrecovered -> estimation stops at the dataset.

## Remaining tonight (in order)

1. Collect stage 23 result -> document the fraction-rule verdict in
   paper_legacy_method_diffs.md (A/B half-scene artifact confirmation).
2. When downloads land: extract titles + miseEnPlace_full; fetch uf_uf_id.xlsx
   individually (browse data/input/auxiliary listing for its href); hunt
   miseEnPlace_full for *.do / crosswalk / temas_update; run stage 24 (takeup);
   compare applies/receives to the paper's ~10% / 42% anchors.
3. Validate our vnp01/vnp02 panels against their city_region_yearly files; wire
   vtn_YYYY_clean.rds into VTN steps 7-8 (separate stage or config pointing at
   data/legacy_dropbox/pptw_data_clean/vtn/).
4. Optional: extract rest of pptw_data_output.zip (car_vtn_IHS breakdowns may
   BE the yearly_average_price_region precursor — check before declaring the
   wall final).
5. Update missing_for_replication rows (takeup, Fig applications), the version
   map, this checkpoint; final commit.

## Ask-list for Pedro after tonight (shrunk)

did.dta (printed vintage), the tab:6 + takeup (tab:25) Stata do-files, the
yearly_average_price_region / turnover producer script, reg1_n.dta (diff
target), CAR_eligible_defo_2002-2004.rds (cap-blocked), Table-1-control vintage.
