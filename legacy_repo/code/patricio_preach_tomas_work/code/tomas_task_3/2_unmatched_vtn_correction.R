# ============================================================================
# 3_unmatched_vtn_correction.R  
#  • Fills in missing muni_code values from the correction sheets
#  • Fixes one mis-labelled state in 2018 (ACEGUA: RR → RS)
#  • Deletes the bogus EX / EXTERIOR row in 2015
#  • Asserts row-count invariants so you know if anything unexpected happens
# ============================================================================

library(dplyr)
library(readr)
library(fs)
library(stringi)
library(purrr)
library(stringr)
library(glue)

# ── 0. PATHS ────────────────────────────────────────────────────────────────
vtn_clean_dir    <- "data/clean/vtn"
vtn_cleaning_dir <- "data/clean/analysis_vtn"   # adjust if your folder name differs

# ── 1. READ CORRECTION SHEETS ──────────────────────────────────────────────
fix_sheets <- dir_ls(
  vtn_cleaning_dir,
  regexp  = "unmatched_\\d{4}_TO_FIX\\.csv$",
  recurse = FALSE
)

fix_tbl <- map_dfr(
  fix_sheets,
  read_csv,
  col_types = cols(
    state     = col_character(),
    muni_code = col_integer(),
    mun_name  = col_character()
  )
)

# ── 2. NORMALISE NAMES + DEDUPLICATE CORRECTIONS ───────────────────────────
normalize_name <- function(x) {
  x |>
    stri_enc_toutf8() |>
    stri_trans_general("NFD; [:Nonspacing Mark:] Remove; NFC") |>
    stri_trans_toupper(locale = "pt") |>
    stri_trim_both() |>
    stri_replace_all_regex("[[:space:]]+", " ")
}

fix_tbl <- fix_tbl %>%
  mutate(key = normalize_name(mun_name)) %>%
  filter(!is.na(key)) %>%                    # avoid NA-to-NA matches
  distinct(state, key, .keep_all = TRUE)     # one row per state+key

# ── 3. PATCH EACH CLEANED FILE ─────────────────────────────────────────────
clean_files <- dir_ls(
  vtn_clean_dir,
  regexp  = "vtn_\\d{4}_clean\\.rds$",
  recurse = FALSE
)

for (f in clean_files) {
  year     <- str_extract(f, "\\d{4}")
  vtn_orig <- readRDS(f)

  # 3a. **2018 ONLY**: fix ACEGUA state label (RR → RS) ---------------------
  if (year == "2018") {
    n_fixed <- sum(vtn_orig$mun_name == "ACEGUA" & vtn_orig$state == "RR")
    if (n_fixed > 0) {
      vtn_orig <- vtn_orig %>%
        mutate(state = if_else(mun_name == "ACEGUA" & state == "RR", "RS", state))
      message("🔧  Corrected ", n_fixed,
              " row(s) in 2018: ACEGUA state changed RR → RS")
    } else {
      message("ℹ️  No ACEGUA rows with state RR found in 2018 (already correct)")
    }
  }

  # 3b. Fill missing muni_code values --------------------------------------
  vtn_new <- vtn_orig %>%
    mutate(key = normalize_name(mun_name)) %>%
    left_join(fix_tbl %>% select(state, key, muni_code_fix = muni_code),
              by = c("state", "key")) %>%
    mutate(
      muni_code = if_else(is.na(muni_code), muni_code_fix, muni_code)
    ) %>%
    select(-key, -muni_code_fix)

  # 3c. **2015 ONLY**: remove bogus EX / EXTERIOR row -----------------------
  if (year == "2015") {
    before  <- nrow(vtn_new)
    vtn_new <- vtn_new %>%
      filter(!(state == "EX" & mun_name == "EXTERIOR"))
    message("🗑️  Removed ",
            before - nrow(vtn_new),
            " bogus row (state == 'EX' & mun_name == 'EXTERIOR') from 2015")
  }

  # 3d. Sanity checks -------------------------------------------------------
  row_diff <- nrow(vtn_new) - nrow(vtn_orig)

  if (year == "2015") {
    if (row_diff != -1) {
      stop(glue("Row count for {basename(f)} changed by {row_diff} but should be -1."))
    }
  } else {
    if (row_diff != 0) {
      stop(glue("Row count changed for {basename(f)}: ",
                "{nrow(vtn_orig)} → {nrow(vtn_new)}"))
    }
  }

  filled <- sum(is.na(vtn_orig$muni_code)) - sum(is.na(vtn_new$muni_code))

  # 3e. Save patched file ---------------------------------------------------
  saveRDS(vtn_new, f, compress = "xz")
  message("✅  Patched ", basename(f), " (filled ", filled, " muni_codes)")
}
