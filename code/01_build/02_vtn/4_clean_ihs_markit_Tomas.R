# nolint

library(readxl)
library(dplyr)
library(janitor)
library(tidyr)
library(glue)
library(stringi)
library(fs)
library(readr)
library(here)

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(path)) {
    stop("Failed to create directory: ", path)
  }
  invisible(path)
}

# ── Paths ────────────────────────────────────────────────────────────────────
file_path <- here("data", "input", "landvalues", "ihs_markit", "IHS Markit S&P Jun23.xlsx")
vtn_clean_dir <- here("data", "clean", "vtn")
vtn_ihs_dir <- here("data", "clean", "vtn_IHS")
ensure_dir(vtn_ihs_dir)

# ── 1. Read intermediate-region lookup ──────────────────────────────────────
muni_region_cw <- read_excel(file_path, sheet = "T14", skip = 5) |>
  slice(1:(n() - 5)) |>
  clean_names() |>
  rename(
    state       = uf,
    region_name = regiao,
    region_id   = id_regiao,
    muni_code   = cod_mun_ibge,
    muni_name   = municipio
  )

# ── 2. Collect cleaned VTN files ────────────────────────────────────────────
vtn_files <- dir_ls(vtn_clean_dir, regexp = "vtn_\\d{4}_clean\\.rds$")
stopifnot(length(vtn_files) > 0)

# ── 3. Process each year ────────────────────────────────────────────────────
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

  # ── 4. Region-level averages (ONLY receita_* columns) ───────────────────
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

  # ── 5. Write CSV ----------------------------------------------------------
  out_path <- file.path(vtn_ihs_dir, glue("vtn_region_{yr}.csv"))
  write_csv(df, out_path)
  message("    ↳ wrote ", out_path, "\n")
}

message("✅  Region-level CSVs saved in ", vtn_ihs_dir)
