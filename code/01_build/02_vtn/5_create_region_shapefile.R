# ============================================================
# 0. Libraries
# ------------------------------------------------------------
library(readxl) # read Excel sheets
library(dplyr) # wrangle data
library(sf) # spatial objects & unions
library(janitor) # clean_names()  (install if you haven’t)
library(here)

# ============================================================
# 1. Read the region–municipality lookup table
# ------------------------------------------------------------
# ── 1. Read intermediate-region lookup ────────────────────────────────────────
load(here("data", "clean", "muni_division_2015.Rdata"))
muni_2015 <- muni_division_2015

file_path <- here("data", "input", "landvalues", "ihs_markit", "IHS Markit S&P Jun23.xlsx")

muni_region_cw <- read_excel(file_path, sheet = "T14", skip = 5) |>
  slice(1:(n() - 5)) |>
  clean_names() |>
  rename(
    state       = uf,
    region_name = regiao,
    region_id   = id_regiao,
    muni_code   = cod_mun_ibge,
    muni_name   = municipio
  ) |>
  mutate(muni_code = as.integer(muni_code)) # <- now part of the chain


# ============================================================
# 2. Load the municipal geometries
# ------------------------------------------------------------


muni_2015 <- muni_2015 |>
  mutate(muni_code = as.integer(muni_code)) |>
  dplyr::rename(state = state_uf)

# ── 2. Join region info onto each municipality ───────────────
muni_joined <- muni_2015 |>
  dplyr::left_join(
    muni_region_cw |>
      dplyr::select(muni_code, region_id, region_name),
    by = "muni_code"
  )

# sanity check (optional)
if (any(is.na(muni_joined$region_id))) {
  warning("Some municipalities did not match a region_id.")
}

# ── 3. Dissolve municipalities into regions ──────────────────
regions_2015 <- muni_joined |>
  sf::st_make_valid() |>
  dplyr::group_by(state, region_id, region_name) |>
  dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
  sf::st_make_valid()

# ── 4. Save result ───────────────────────────────────────────
out_file <- here("data", "clean", "IHS_regions_divison.Rdata")
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
save(regions_2015, file = out_file)
