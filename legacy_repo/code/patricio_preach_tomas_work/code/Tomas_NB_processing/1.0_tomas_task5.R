###############################################################################
#  City–Region yearly averages • FNP land-price dataset (Portuguese land types)
#  ---------------------------------------------------------------------------
#  • Reads sheet “FNP 2016 em diante”
#  • Forces every post-reference column to numeric (bad tokens → NA)
#  • Keeps `land_clean` left of `reference`, so it isn’t melted away
#  • Robustly extracts the year; drops rows with no year
#  • Shows a diagnostic table of any un-parsed headers
#  • Outputs one row per (city_region, state, region_fnp) with columns
#       preco_<land_clean>_<year>
###############################################################################

# ── 1. Libraries ────────────────────────────────────────────────────────────
pkgs <- c("readxl", "tidyverse", "janitor", "fs")
new  <- pkgs[!pkgs %in% installed.packages()[ , "Package"]]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")

library(readxl)
library(tidyverse)
library(janitor)
library(fs)

# ── 2. Paths ────────────────────────────────────────────────────────────────
raw_fp  <- "/Users/carolinamelloneetlin/Dropbox/amazon_project/data/input/landvalues/vnp/Land_Price_North_Brazil_FNP.xlsx"
sheet   <- "FNP 2016 em diante"

out_dir  <- "data/clean/vnp"
dir_create(out_dir, recurse = TRUE)
out_stub <- file.path(out_dir, "city_region_yearly_pt")

# ── 3. Read & tidy ──────────────────────────────────────────────────────────
raw <- read_excel(
          raw_fp,
          sheet        = sheet,
          .name_repair = "unique",
          na           = c("", "NA", "NaN", "NAN", "Invalid Num", "Invalid Number")
       ) %>%
  clean_names() %>%
  select(-land_type_english) %>%
  rename(
    land_type   = land_type_portuguese,
    region_name = city_region          # ⬅ standardise naming now
  ) %>%
  mutate(
    land_type  = str_to_title(land_type),                     # “Pastagem Plantada”
    land_clean = str_replace_all(land_type, "\\s+", "_") %>%  # "Pastagem_Plantada"
                 tolower()                                    # "pastagem_plantada"
  ) %>%
  relocate(land_clean, .before = reference)                   # keep out of price columns

# ── 4. Identify price columns (all after `reference`) ───────────────────────
ref_idx    <- match("reference", names(raw))
if (is.na(ref_idx))
  stop("Column 'reference' not found — check header spelling.")

price_cols <- names(raw)[ (ref_idx + 1) : ncol(raw) ]

# ── 5. Force-coerce price cols to numeric ───────────────────────────────────
raw <- raw %>%
  mutate(across(all_of(price_cols),
                ~ readr::parse_number(as.character(.))))

# ── 6. Melt to long ─────────────────────────────────────────────────────────
long <- raw %>%
  pivot_longer(all_of(price_cols),
               names_to  = "period_raw",
               values_to = "preco_brl_ha")

# ── 7. Robust year extraction ───────────────────────────────────────────────
long <- long %>%
  mutate(
    ano = str_extract(period_raw, "(?<!\\d)(19|20)\\d{2}(?!\\d)") %>% as.integer(),
    ano = if_else(
            is.na(ano),
            2000 + as.integer(str_extract(period_raw, "\\d{2}$")),
            ano
          )
  )

# ── 7-b. Diagnostic: show headers with no year ──────────────────────────────
no_year <- long %>% filter(is.na(ano))
if (nrow(no_year) > 0) {
  cat("\n⚠️  Headers that did NOT yield a year:\n")
  no_year %>%
    count(period_raw, sort = TRUE) %>%
    print(n = Inf)
} else {
  cat("\n✅  Every header produced a valid year.\n")
}

# Drop rows with missing year
long <- long %>% filter(!is.na(ano))

# ── 8. Yearly mean per (state × region_name × land_type) ────────────────────
yearly <- long %>%
  group_by(region_name, state, land_clean, ano) %>%
  summarise(preco_brl_ha = mean(preco_brl_ha, na.rm = TRUE),
            .groups = "drop")

# ── 9. Pivot wide ───────────────────────────────────────────────────────────
yearly_wide <- yearly %>%
  mutate(col_stub = paste0("preco_", land_clean, "_", ano)) %>%
  select(region_name, state, col_stub, preco_brl_ha) %>%
  pivot_wider(
    id_cols     = c(region_name, state),  # ⬅ only these two define a row
    names_from  = col_stub,
    values_from = preco_brl_ha
  ) %>%
  arrange(region_name)

# ── 10. Save outputs ────────────────────────────────────────────────────────
write_csv(yearly_wide, paste0(out_stub, ".csv"))
write_rds(yearly_wide,  paste0(out_stub, ".rds"))

cat(
  "\n✅  One row per (state, region_name) written to:\n",
  paste0(out_stub, ".csv\n"),
  paste0(out_stub, ".rds\n")
)
