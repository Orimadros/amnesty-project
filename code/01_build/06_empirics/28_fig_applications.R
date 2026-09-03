# Stage 28 (recovered code): the Fig-applications aggregates (3_policy1.R:70-110).
# Ports the DATA behind the two Terra Legal bar charts (requests per year,
# 2009-2018; requested hectares per year, 2009-2015) -- the ggplot cosmetics are
# not ported. Uses the same terra_legal cleaning as stage 24 (:55-64).

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
  library(stringi)
  library(readr)
  library(here)
})
dd <- here("data", "legacy_dropbox")
emp_dir <- here("data", "intermediate", "empirics")

terra_legal <- read_delim(file.path(dd, "input_terralegal", "DadosTerraLegal.csv"),
                          delim = ";", escape_double = FALSE,
                          locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE,
                          show_col_types = FALSE)
terra_legal$municipio <- tolower(stri_trans_general(terra_legal$municipio, "Latin-ASCII"))
terra_legal$nome_requerente <- str_squish(stri_trans_general(terra_legal$nome_requerente, "Latin-ASCII"))
terra_legal$ha_m <- gsub("\\..*", "", as.character(terra_legal$area_do_imovel))
terra_legal <- terra_legal %>% group_by(nome_requerente, municipio, area_do_imovel) %>%
  filter(row_number(nome_requerente) == 1)
terra_legal <- terra_legal %>% filter(numero_processo != "00000.000000/0000-00")
terra_legal <- terra_legal %>% group_by(numero_processo) %>% add_count() %>%
  filter(n == 1) %>% ungroup() %>% dplyr::select(-n)
terra_legal$year_request <- str_extract(gsub(".*\\/", "", terra_legal$numero_processo),
                                        "^([0-9])([0-9])([0-9])([0-9])")

# panel (a): requests per year, 2009-2018 (:70)
req <- terra_legal %>% group_by(year_request) %>% count() %>%
  filter(as.numeric(year_request) > 2008 & as.numeric(year_request) < 2019)
# panel (b): requested hectares per year, 2009-2015 (:91)
ha <- terra_legal %>% group_by(year_request) %>%
  summarise(total_ha = sum(as.numeric(ha_m), na.rm = TRUE), .groups = "drop") %>%
  filter(as.numeric(year_request) > 2008 & as.numeric(year_request) < 2016)

cat("\n===== Fig applications: requests per year =====\n")
print(as.data.frame(req))
cat("\n===== Fig applications: requested hectares per year =====\n")
print(as.data.frame(ha))

fwrite(as.data.table(req), file.path(emp_dir, "fig_applications_requests.csv"))
fwrite(as.data.table(ha), file.path(emp_dir, "fig_applications_hectares.csv"))
cat("\nWrote: fig_applications_{requests,hectares}.csv\n")
