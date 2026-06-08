# nolint

###############################################################################
#  Clean VTN files (2015-2022)
#  • 2015-2018: read from raw folder
#  • 2019-2022: read from pre-clean folder
#  • mun_name is normalised (no accents) before any joins
###############################################################################

library(dplyr)
library(stringi)
library(purrr)
library(fs)      # dir_ls()
library(sf)

# ── 0. paths ────────────────────────────────────────────────────────────────
vtn_raw_dir       <- "./../../data/input/landvalues/vtn"   # 2015-2018
vtn_preclean_dir  <- "data/preclean"                       # 2019-2022
vtn_clean_dir     <- "data/clean/vtn"                      # output
muni_rdata        <- "data/clean/muni_division_2015.Rdata"

# ── 1. helper: strip accents, trim, collapse spaces ─────────────────────────
normalize_name <- function(x) {
  x |>
    stri_enc_toutf8() |>
    stri_trans_general("NFD; [:Nonspacing Mark:] Remove; NFC") |>
    stri_trans_toupper(locale = "pt") |>
    stri_trim_both() |>
    stri_replace_all_regex("[[:space:]]+", " ")
}

# ── 2. lookup table (state + key → muni_code) ───────────────────────────────
load(muni_rdata)  # gives muni_division_2015
muni_lookup <- muni_division_2015 |>
  st_drop_geometry() |>
  rename(
    state         = state_uf,
    mun_name_ibge = mun_name
  ) |>
  mutate(key = normalize_name(mun_name_ibge)) |>
  select(state, key, muni_code)

# ── 3. collect VTN file paths (2015-2022) ───────────────────────────────────
vtn_files <- c(
  dir_ls(                         # raw originals
    vtn_raw_dir,
    regexp  = "vtn_20(15|16|17|18)\\.rds$",
    recurse = TRUE
  ),
  dir_ls(                         # pre-cleaned
    vtn_preclean_dir,
    regexp  = "vtn_20(19|20|21|22)\\.rds$",
    recurse = TRUE
  )
)
stopifnot(length(vtn_files) > 0)

# ── 4. make sure output folder exists ───────────────────────────────────────
dir_create(vtn_clean_dir, recurse = TRUE)

# ── 5. cleaner for a single file ────────────────────────────────────────────
clean_one_vtn <- function(path) {
  year <- stringr::str_extract(path, "\\d{4}")
  message("🧹 Cleaning VTN ", year, " …")

  vtn <- readRDS(path) |>
    mutate(
      mun_name = normalize_name(mun_name),  # ← accent-free visible column
      key      = mun_name                   # key for join (already normalised)
    )

  # ── PATCH: 2015-2018 — drop `mapa_` cols and rename `receita_*` ───────────
  if (year %in% c("2015", "2016", "2017", "2018")) {
    vtn <- vtn |>
      select(-starts_with("mapa_"))
  }
  # ──────────────────────────────────────────────────────────────────────────

  vtn <- vtn |>
    left_join(muni_lookup, by = c("state", "key")) |>
    relocate(state, muni_code, mun_name, year)

  # warn & save rows still without muni_code
  n_miss <- sum(is.na(vtn$muni_code))
  if (n_miss) {
    message("   ⚠️  ", n_miss, " rows still missing muni_code (saved for review)")
    dir_create("data/clean/analysis_vtn", recurse = TRUE)
    saveRDS(
      vtn |> filter(is.na(muni_code)) |> select(state, mun_name),
      file = file.path("data/clean/analysis_vtn", paste0("unmatched_", year, ".rds"))
    )
  }

  # write cleaned file
  saveRDS(
    vtn |> select(-key),                       # drop helper
    file = file.path(vtn_clean_dir,
                     paste0("vtn_", year, "_clean.rds")),
    compress = "xz"
  )

  vtn |> select(-key)                          # return to stack
}

# ── 6. iterate over all files & stack ───────────────────────────────────────
vtn_all_clean <- map_dfr(vtn_files, clean_one_vtn)

message(
  "✅  Done: cleaned VTN files in ", normalizePath(vtn_clean_dir),
  "\n   Stacked tibble available as `vtn_all_clean`."
)
