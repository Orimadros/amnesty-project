# Stage 1 of the NB/VNP chain: FNP North-Brazil land prices, 2016 onward.
#
# Produces data/clean/vnp/city_region_yearly_pt.{csv,rds} -- one row per
# (state, region_name), one column per (land type x year).
#
# Migrated from:
#   legacy_repo/code/patricio_preach_tomas_work/code/
#     Tomas_NB_processing/1.0_tomas_task5.R
#
# Deviations recorded in docs/notes/vnp_migration_issues.md.

source(here::here("code", "01_build", "05_vnp", "_helpers_vnp.R"))

build_city_region_panel(
  sheet = "FNP 2016 em diante",
  out_stub = file.path(VNP_OUT_DIR, "city_region_yearly_pt")
)
