# Shared helper for the NB/VNP (FNP North-Brazil land price) chain.
#
# Legacy scripts 1.0 and 2.0 of Tomas_NB_processing are byte-for-byte the same
# logic applied to two sheets of the same workbook, differing only in the sheet
# name and the output stub. That duplicated body lives here once.
#
# See docs/notes/vnp_migration_issues.md for the deviation log.

library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(janitor)
library(here)

VNP_WORKBOOK <- here(
  "data", "input", "landvalues", "vnp", "Land Price_North Brazil_FNP.xlsx"
)

VNP_OUT_DIR <- here("data", "clean", "vnp")

VNP_NA_TOKENS <- c("", "NA", "NaN", "NAN", "Invalid Num", "Invalid Number")

ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(path)) {
    stop("Failed to create directory: ", path)
  }
  invisible(path)
}

# Build the wide (state x region_name) x preco_<land>_<year> panel from one
# sheet of the FNP workbook, and write it as .csv + .rds under out_stub.
#
# The workbook stores prices in bimonthly columns to the right of `reference`
# (e.g. nov_dec_01, jan_fev_010, jan_mar_22); everything left of `reference` is
# metadata. Prices are averaged to a yearly figure per land type.
build_city_region_panel <- function(sheet, out_stub, workbook = VNP_WORKBOOK) {
  if (!file.exists(workbook)) {
    stop(
      paste(
        "Missing FNP workbook required by the VNP chain:",
        paste0(" - ", workbook),
        "Place it under data/input/landvalues/vnp/ first.",
        sep = "\n"
      )
    )
  }

  raw <- read_excel(
    workbook,
    sheet = sheet,
    .name_repair = "unique",
    na = VNP_NA_TOKENS
  ) %>%
    clean_names() %>%
    select(-land_type_english) %>%
    rename(
      land_type = land_type_portuguese,
      region_name = city_region
    ) %>%
    mutate(
      land_type = str_to_title(land_type),
      land_clean = tolower(str_replace_all(land_type, "\\s+", "_"))
    ) %>%
    relocate(land_clean, .before = reference)

  # Price columns are everything to the right of `reference`. The 2016+ sheet
  # carries extra `yield`/`detail` metadata columns, but they sit to the LEFT of
  # `reference`, so this rule excludes them without special-casing.
  ref_idx <- match("reference", names(raw))
  if (is.na(ref_idx)) {
    stop("Column 'reference' not found in sheet '", sheet, "' -- check headers.")
  }
  price_cols <- names(raw)[(ref_idx + 1):ncol(raw)]

  raw <- raw %>%
    mutate(across(all_of(price_cols), ~ parse_number(as.character(.))))

  long <- raw %>%
    pivot_longer(
      all_of(price_cols),
      names_to = "period_raw",
      values_to = "preco_brl_ha"
    ) %>%
    mutate(
      # Prefer a real 4-digit year; fall back to the trailing 2 digits. The
      # fallback is what handles the pre-2015 sheet's 3-digit tails
      # (jan_fev_010 -> 2010, nov_dez_015 -> 2015).
      ano = as.integer(str_extract(period_raw, "(?<!\\d)(19|20)\\d{2}(?!\\d)")),
      ano = if_else(
        is.na(ano),
        2000L + as.integer(str_extract(period_raw, "\\d{2}$")),
        ano
      )
    )

  no_year <- long %>% filter(is.na(ano))
  if (nrow(no_year) > 0) {
    message("Headers in '", sheet, "' that did NOT yield a year:")
    print(no_year %>% count(period_raw, sort = TRUE), n = Inf)
    long <- long %>% filter(!is.na(ano))
  } else {
    message("Sheet '", sheet, "': every price header produced a valid year.")
  }

  yearly <- long %>%
    group_by(region_name, state, land_clean, ano) %>%
    summarise(preco_brl_ha = mean(preco_brl_ha, na.rm = TRUE), .groups = "drop")

  # mean() of an all-NA group returns NaN; normalise so downstream is.na() works.
  yearly <- yearly %>%
    mutate(preco_brl_ha = if_else(is.nan(preco_brl_ha), NA_real_, preco_brl_ha))

  yearly_wide <- yearly %>%
    mutate(col_stub = paste0("preco_", land_clean, "_", ano)) %>%
    select(region_name, state, col_stub, preco_brl_ha) %>%
    pivot_wider(
      id_cols = c(region_name, state),
      names_from = col_stub,
      values_from = preco_brl_ha
    ) %>%
    arrange(region_name)

  ensure_dir(dirname(out_stub))
  write_csv(yearly_wide, paste0(out_stub, ".csv"))
  write_rds(yearly_wide, paste0(out_stub, ".rds"))

  message(
    "Sheet '", sheet, "': ", nrow(yearly_wide), " (state, region) rows x ",
    ncol(yearly_wide) - 2, " price columns, years ",
    min(yearly$ano), "-", max(yearly$ano)
  )
  message("Wrote: ", out_stub, ".{csv,rds}")

  invisible(yearly_wide)
}
