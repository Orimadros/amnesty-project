# nolint

###############################################################################
#  0_pre_clean_vtn_2019-22.R
#  • Reads 2019, 2021, 2022 raw VTN .rds files
#  • Cleans names, converts numerics, applies SP-2022 duplicate patch
#  • Ensures `unico` is the **last column** in the output tibble
###############################################################################

library(dplyr)
library(stringr)
library(readr)
library(fs)
library(stringi)
library(purrr)

# ---------- paths ---------------------------------------
vtn_raw_dir      <- "./../../data/input/landvalues/vtn"
vtn_preclean_dir <- "data/preclean"
dir_create(vtn_preclean_dir, recurse = TRUE)

# ---------- helpers -------------------------------------
numify <- function(x) {
  parse_number(as.character(x),
               locale = locale(decimal_mark = ".", grouping_mark = ""))
}

clean_mun_name <- function(tbl) {
  tbl %>%
    # 1. accent-stripped copy for pattern matching
    mutate(.noacc = stri_trans_general(
      mun_name, "NFD; [:Nonspacing Mark:] Remove; NFC"
    )) %>%
    # 2. strip trailing “S/INFORMACAO” tokens
    mutate(
      .noacc = str_remove(
        .noacc,
        regex("\\s+(S/INFORMAC[ÃA]O\\b\\s*)+$", ignore_case = TRUE)
      ),
      .noacc = str_squish(.noacc),
      mun_name = .noacc
    ) %>%
    select(-.noacc) %>%
    # 3. drop rows that start with “NOTAS:”
    filter(!str_starts(mun_name, regex("^NOTAS[: ]", ignore_case = TRUE)))
}

# ---------- pre-clean one tibble ------------------------
preclean_one <- function(tbl, year) {
  if (year == 2022) {
    tbl <- tbl %>% slice(-c(2061, 2067))
  }

  tbl %>%


  clean_mun_name() %>%
    mutate(
      unico = if (!"unico" %in% names(.))
        NA_character_ else as.character(unico)
    ) %>%
    # unico moved to the **end** of the column order
    select(state, mun_name, lavoura_boa:preservation, year, unico) %>%
    mutate(
      unico  = numify(unico),
      across(lavoura_boa:preservation, numify),
      year   = as.integer(year)
    ) %>%

    relocate(state, mun_name, year) %>%   # keeps required leading columns
    # ---------- NEW: add receita_ prefix to numeric cols -------------------
    rename_with(
      ~ ifelse(.x %in% c("state", "mun_name", "year"), .x,
               paste0("receita_", .x))
    )
}

# ---------- run for 2019, 2021, 2022 --------------------
raw_files <- dir_ls(
  vtn_raw_dir,
  regexp = "vtn_(2019|2021|2022)\\.rds$"
)
stopifnot(length(raw_files) > 0)

walk(raw_files, function(path) {
  yr <- str_extract(path, "\\d{4}")
  message("🔧  pre-cleaning ", yr, " …")
  cleaned <- readRDS(path) %>% preclean_one(yr)

  saveRDS(
    cleaned,
    file = file.path(vtn_preclean_dir, paste0("vtn_", yr, ".rds")),
    compress = "xz"
  )
  message("     ↳ wrote vtn_", yr, ".rds")
})

message("✅  Pre-clean finished.")

# ---------- sanity check (stub count) -------------------
years_done <- raw_files %>% basename() %>% str_extract("\\d{4}") %>% unique()

for (yr in years_done) {
  f <- file.path(vtn_preclean_dir, paste0("vtn_", yr, ".rds"))
  n_stub <- readRDS(f) %>%
            filter(str_detect(mun_name, "INFORMAC")) %>%
            nrow()
  message("Year ", yr, ": rows with stub = ", n_stub)
}
