# =============================================
# PROJECT:  Spatial analysis starter script
# DATA:     Brazilian municipalities (IBGE)
# =============================================

library(tidyverse)
library(stringi)
library(sf)

RAW_DIR  <- "data/muni_division_2015"
LAYER    <- "BRMUE250GC_SIR"
CLEAN_FN <- "data/clean/muni_division_2015.Rdata"
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)

# 1. read raw shapefile ---------------------------------------------------
raw_muni <- st_read(dsn = RAW_DIR, layer = LAYER, quiet = TRUE)

# 2. basic column hygiene -------------------------------------------------
raw_muni <- raw_muni %>%
  rename(
    muni_code = CD_GEOCMU,
    mun_name  = NM_MUNICIP
  )

# 3. add state UF code ----------------------------------------------------
state_key <- c(
  "11"="RO","12"="AC","13"="AM","14"="RR","15"="PA","16"="AP","17"="TO",
  "21"="MA","22"="PI","23"="CE","24"="RN","25"="PB","26"="PE","27"="AL","28"="SE","29"="BA",
  "31"="MG","32"="ES","33"="RJ","35"="SP","41"="PR","42"="SC","43"="RS",
  "50"="MS","51"="MT","52"="GO","53"="DF"
)

raw_muni <- raw_muni %>%
  mutate(state_uf = state_key[str_sub(muni_code, 1, 2)])

# 4. type conversions + encoding fixes -----------------------------------
raw_muni <- raw_muni %>%
  mutate(
    muni_code = as.numeric(muni_code),
    mun_name  = mun_name %>%
                  stri_enc_toutf8() %>%
                  stri_trans_general("NFD; [:Nonspacing Mark:] Remove; NFC") %>%
                  stri_trans_toupper(locale = "pt") %>%
                  stri_trim_both() %>%
                  stri_replace_all_regex("[[:space:]]+", " ")
  )

# ---------- PATCH: fix wrong code for MG ---------------------------------
raw_muni <- raw_muni %>%
  mutate(
    muni_code = if_else(
      muni_code == 3100539 & state_uf == "MG",
      3150539,
      muni_code
    )
  )
# ------------------------------------------------------------------------

# 5. re-project to Brazil Polyconic (EPSG 5880) ---------------------------
raw_muni <- st_transform(raw_muni, 5880)

# 6. drop water-body polygons --------------------------------------------
raw_muni <- raw_muni %>% filter(!muni_code %in% c(4300001, 4300002))

# 7. geometry validity check ---------------------------------------------
raw_muni <- st_make_valid(raw_muni)

# 9. save -----------------------------------------------------------------
muni_division_2015 <- raw_muni
save(muni_division_2015, file = CLEAN_FN)
message("✅ Clean shapefile saved to: ", CLEAN_FN)
