###############################################################################
#  City-Region yearly averages • FNP land-price dataset  (Portuguese land types)
#  ---------------------------------------------------------------------------
#  Sheet          :  "FNP até 2015"      (covers 2001-2015)
#  Output files   :  data/clean/vnp/city_region_yearly_pt_pre2015.{csv,rds}
###############################################################################

# ── 1.  Packages ────────────────────────────────────────────────────────────
pkgs <- c("readxl", "tidyverse", "janitor", "fs")
new  <- pkgs[!pkgs %in% installed.packages()[ , "Package"]]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")

library(readxl)
library(tidyverse)
library(janitor)
library(fs)

# ── 2.  Paths ───────────────────────────────────────────────────────────────
raw_fp   <- "/Users/carolinamelloneetlin/Dropbox/amazon_project/data/input/landvalues/vnp/Land_Price_North_Brazil_FNP.xlsx"
sheet    <- "FNP até 2015"                           # ← pre-2016 panel
out_dir  <- "data/clean/vnp"
dir_create(out_dir, recurse = TRUE)
out_stub <- file.path(out_dir, "city_region_yearly_pt_pre2015")

# ── 3.  Read & tidy base table ──────────────────────────────────────────────
raw <- read_excel(
          raw_fp,
          sheet        = sheet,
          .name_repair = "unique",
          na           = c("", "NA", "NaN", "NAN", "Invalid Num", "Invalid Number")
       ) %>%
  clean_names() %>%
  select(-land_type_english) %>%                    # keep Portuguese label only
  rename(
    land_type   = land_type_portuguese,
    region_name = city_region                       # standardise naming
  ) %>%
  mutate(
    land_type  = str_to_title(land_type),                          # “Pastagem Plantada”
    land_clean = str_replace_all(land_type, "\\s+", "_") %>%       # "Pastagem_Plantada"
                 tolower()                                         # "pastagem_plantada"
  ) %>%
  relocate(land_clean, .before = reference)         # keep out of price_cols

# ── 4.  Identify price columns (all after `reference`) ──────────────────────
ref_idx <- match("reference", names(raw))
if (is.na(ref_idx))
  stop("Column 'reference' not found — check header spelling.")

price_cols <- names(raw)[ (ref_idx + 1) : ncol(raw) ]

# ── 5.  Force-coerce those columns to numeric (text → NA) ───────────────────
raw <- raw %>%
  mutate(across(all_of(price_cols),
                ~ readr::parse_number(as.character(.))))

# ── 6.  Melt to long format ─────────────────────────────────────────────────
long <- raw %>%
  pivot_longer(all_of(price_cols),
               names_to  = "period_raw",
               values_to = "preco_brl_ha")

# ── 7.  Robust year extraction (expects 2001-2015) ──────────────────────────
long <- long %>%
  mutate(
    ano = str_extract(period_raw, "(?<!\\d)(19|20)\\d{2}(?!\\d)") %>% as.integer(),
    ano = if_else(
            is.na(ano),
            2000 + as.integer(str_extract(period_raw, "\\d{2}$")),
            ano
          )
  )

# ── 7-b. Diagnostic: headers with no year ───────────────────────────────────
no_year <- long %>% filter(is.na(ano))
if (nrow(no_year) > 0) {
  cat("\n⚠️  Headers that did NOT yield a year (pre-2015 sheet):\n")
  no_year %>%
    count(period_raw, sort = TRUE) %>%
    print(n = Inf)
} else {
  cat("\n✅  Every header produced a valid year (pre-2015 sheet).\n")
}
long <- long %>% filter(!is.na(ano))   # keep only rows with a parsed year

# ── 8.  Yearly mean per state × region_name × land-type ─────────────────────
yearly <- long %>%
  group_by(region_name, state, land_clean, ano) %>%   # ← region_fnp dropped
  summarise(preco_brl_ha = mean(preco_brl_ha, na.rm = TRUE),
            .groups = "drop")

# ── 9.  Pivot wide (one row per region) ─────────────────────────────────────
yearly_wide <- yearly %>%
  mutate(col_stub = paste0("preco_", land_clean, "_", ano)) %>%
  select(region_name, state, col_stub, preco_brl_ha) %>%
  pivot_wider(
    id_cols     = c(region_name, state),          # rows keyed only by these two
    names_from  = col_stub,
    values_from = preco_brl_ha
  ) %>%
  arrange(region_name)

# ── 10.  Save outputs ───────────────────────────────────────────────────────
write_csv(yearly_wide, paste0(out_stub, ".csv"))
write_rds(yearly_wide,  paste0(out_stub, ".rds"))

cat(
  "\n✅  One row per (state, region_name) for 2001-2015 written to:\n   ",
  paste0(out_stub, ".csv\n   "),
  paste0(out_stub, ".rds\n")
)
