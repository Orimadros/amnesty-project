library(readxl)
library(dplyr)
library(janitor)
library(tidyr)
library(glue)
library(stringi)
library(fs)
library(tidyverse)

# Tasks for Tomas:
# - Make this work for 2019 onwards
# - sort out missing municodes in VTN_clean (we should have no NA, NA at the end)
#
# If time allows:
# - Find a shapefile dataset for intermediate regions
#   and spatial-join this to CAR data
#



library(readxl)
library(dplyr)
library(janitor)
library(tidyr)
library(glue)
library(stringi)
library(fs)
library(tidyverse)

# ── 1. Read intermediate-region lookup ────────────────────────────────────────
file_path <- "../../data/input/landvalues/ihs_markit/IHS Markit S&P Jun23.xlsx"

muni_region_cw <- read_excel(file_path, sheet = "T14", skip = 5) |>
  slice(1:(n() - 5)) |>
  rename(
    state       = UF,
    region_name = Região,
    region_id   = `ID Região`,
    muni_code   = `Cód. Mun. IBGE`,
    muni_name   = Município
  )

# ── 2. Collect cleaned VTN files ──────────────────────────────────────────────
vtn_files <- dir_ls("data/clean/vtn/")
stopifnot(length(vtn_files) > 0)

# ── 3. Process each year ──────────────────────────────────────────────────────
for (path in vtn_files) {
  yr <- stringr::str_extract(path, "\\d{4}")
  message("🔧  Processing VTN ", yr, " …")

  vtn_df <- readRDS(path) |>
    left_join(muni_region_cw, by = c("state", "muni_code"))

  # diagnostics ---------------------------------------------------------------
  n_regions <- vtn_df$region_name |>
    unique() |>
    length()
  n_unmatched <- vtn_df |>
    filter(is.na(region_name)) |>
    nrow()
  message("    • regions found   : ", n_regions)
  message("    • unmatched rows  : ", n_unmatched)

  # ── 4. Region-level averages (ONLY receita_* columns) ───────────────────────
  df <- vtn_df |>
    group_by(region_id, region_name) |>
    summarise(
      across(
        starts_with("receita"),
        ~ mean(.x, na.rm = TRUE),
        .names = "{.col}"
      ),
      .groups = "drop"
    )

  # ── 5. Write CSV -----------------------------------------------------------
  out_path <- glue("data/clean/vtn_IHS/vtn_region_{yr}.csv")
  write_csv(df, out_path)
  message("    ↳ wrote ", out_path, "\n")
}

message("✅  Region-level CSVs saved in data/clean/vtn_meso")
