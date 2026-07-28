# CAR build, stage 00: registration-year panel.
#
# Faithful, reproducible port of sections 1-2 of the legacy producer
# `legacy_repo/dropbox_producers/create_muni_year_intersections/A_prepare_initial_data.R`
# (Thiago Alckmin, Oct 2023).
#
# Purpose: from the raw CAR microdata + attribute table + the two Python-scraped
# cancelled-date tables, build (a) an updated microdata file with missing/cancelled
# registration dates filled in, and (b) a wide registro_car x year panel indicating,
# for each CAR, the years in which it was registered.
#
# Outputs (both consumed by 01_clean_car_shapes.R):
#   data/intermediate/car/temas_ambientais_update.csv
#   data/intermediate/car/car_and_reg_year_wide.csv
#
# Deviations from the legacy script are marked "NOTE(migration):" inline and
# catalogued in docs/notes/car_migration_issues.md.

suppressPackageStartupMessages({
  library(sf)
  library(data.table)
  library(magrittr)
  library(dplyr)
  library(stringi)
  library(here)
  library(foreign)
})

source(here("code", "01_build", "01_car", "_helpers_car_util.R"))

# ---- inputs (explicit; no hidden fallbacks) --------------------------------
# NOTE(migration): legacy used hardcoded Dropbox/server paths and a `data/raw`,
# `data/cleaned` split. Here all given inputs live under data/input/ and all
# generated artifacts under data/intermediate/, per this repo's conventions.
in_microdata      <- here("data", "input", "sicar", "microdata", "temas_ambientais.csv")
in_car_combined   <- here("data", "input", "sicar", "car_combined.dbf")
in_municipalities <- here("data", "input", "aux", "municipalities_amazon_biome", "municipalities_amazon_biome.shp")
# Python-scraped cancelled-CAR registration dates (manual "given" inputs; no
# producer script -- documented like the VTN fix-sheets in PROBLEMS.md section 3).
in_cancelled_dates <- here("data", "input", "manual", "car", "cancelled_car_dates.csv")
in_report_dates    <- here("data", "input", "manual", "car", "202310_report_data_registro_car.csv")

required_inputs <- c(in_microdata, in_car_combined, in_municipalities,
                     in_cancelled_dates, in_report_dates)
missing_inputs <- required_inputs[!file.exists(required_inputs)]
if (length(missing_inputs) > 0) {
  stop(paste(
    "Missing required input(s) for CAR stage 00:",
    paste0(" - ", missing_inputs, collapse = "\n"),
    "Populate these under data/input/ before running. The SICAR raw data and the",
    "two scraped cancelled-date CSVs come from the amazonLandPrices_project Dropbox;",
    "see docs/notes/car_magic_files_recovery.md for provenance.",
    sep = "\n"
  ))
}

# ---- outputs ---------------------------------------------------------------
out_dir <- here("data", "intermediate", "car")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_microdata_update <- file.path(out_dir, "temas_ambientais_update.csv")
out_reg_year_wide    <- file.path(out_dir, "car_and_reg_year_wide.csv")
out_scrape_list      <- file.path(out_dir, "missing_car_dates", "car_ids_muni.csv")
dir.create(dirname(out_scrape_list), recursive = TRUE, showWarnings = FALSE)

# ---- municipalities in the Amazon biome (filter set) -----------------------
municipalities_amazon <- read_sf(in_municipalities) %>%
  st_transform(crs = 4674) %>%
  st_make_valid() %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  .[, unique(geocodigo)]

# ---- 2.0: registration microdata, restricted to Amazon municipalities ------
microdata_inc <-
  fread(in_microdata) %>%
  .[, uf := as.numeric(substr(codigo_ibge, 1, 2))] %>%
  .[codigo_ibge %in% municipalities_amazon] %>%
  .[, `:=`(
    data_inscricao                   = as.IDate(data_inscricao),
    data_alteracao_condicao_cadastro = as.IDate(data_alteracao_condicao_cadastro),
    data_ultima_retificacao          = as.IDate(data_ultima_retificacao)
  )]
gc()

# ---- 2.1: list every CAR id + cancellation status from the attribute table -
all_cars <- foreign::read.dbf(in_car_combined) %>%
  data.table() %>%
  .[, municipio := as.numeric(substr(COD_IMOVEL, 4, 10))] %>%
  .[municipio %in% municipalities_amazon] %>%
  .[!duplicated(COD_IMOVEL)]

# separate cancelled (SITUACAO == "CA") from the rest, then re-bind the ids
cancelled_cars     <- all_cars %>% copy() %>% .[SITUACAO == "CA"] %>% .[!duplicated(COD_IMOVEL)]
not_cancelled_cars <- all_cars %>% copy() %>% .[SITUACAO != "CA"] %>% .[!duplicated(COD_IMOVEL)]
CAR_IDS <- rbind(cancelled_cars, not_cancelled_cars) %>% .[, COD_IMOVEL]

# CARs that already carry a registration date vs. those that don't
CAR_IDS_WITH_DATES <- microdata_inc %>%
  .[registro_car %in% CAR_IDS] %>%
  .[!is.na(data_inscricao)] %>%
  .[, registro_car]
CAR_IDS_WITHOUT_DATES <- CAR_IDS %>% .[!. %in% CAR_IDS_WITH_DATES]

# 2.2: record the date-less ids (this is the list that was fed to the Python
# scraper). Kept as a build artifact for provenance; the scrape has already been
# run and its results are the two manual inputs read below.
CAR_IDS_WITHOUT_DATES %>%
  data.table() %>%
  .[, municipio := as.numeric(substr(`.`, 4, 10))] %>%
  .[municipio %in% municipalities_amazon] %>%
  fwrite(out_scrape_list)

rm(all_cars); gc()

# ---- 2.2: merge in the scraped cancelled-CAR dates -------------------------
trial_one <- fread(in_cancelled_dates) %>%
  .[, .(date, registro_car)] %>%
  rename_columns(c("date"), c("data_inscricao_can"))

trial_two <- fread(in_report_dates) %>%
  .[, .(data_registro, car)] %>%
  rename_columns(c("data_registro", "car"), c("data_inscricao_can", "registro_car"))

# both scrapes provide dates as dd/mm/yyyy strings -> parse to ISO IDate
cancelled_cars_with_dates <- rbind(trial_one, trial_two) %>%
  .[, dd   := substr(data_inscricao_can, 1, 2)] %>%
  .[, mm   := substr(data_inscricao_can, 4, 5)] %>%
  .[, yyyy := substr(data_inscricao_can, 7, 10)] %>%
  .[, data_inscricao_can := as.IDate(paste0(yyyy, "-", mm, "-", dd))] %>%
  .[, .(registro_car, data_inscricao_can)]

# ---- 2.3: fill missing dates, mark cancellations, write updated microdata ---
microdata_inc %<>% copy() %>%
  merge(x = ., y = cancelled_cars_with_dates, by = "registro_car", all = TRUE) %>%
  .[is.na(data_inscricao), cancelled := TRUE] %>%
  .[, data_inscricao := as.IDate(data_inscricao)] %>%
  .[is.na(data_inscricao), data_inscricao := data_inscricao_can] %>%
  .[, data_inscricao_can := NULL] %>%
  .[is.na(codigo_ibge), codigo_ibge := substr(registro_car, 4, 10)] %>%
  .[, uf := substr(codigo_ibge, 1, 2)] %>%
  .[!is.na(cancelled), situacao_cadastro := "CA"] %>%
  rename_columns(c("cancelled"), c("cancelled_indic")) %>%
  .[, cancelled_indic := !is.na(cancelled_indic)]

# PURPOSEFULLY EXCLUDE CARs registered in 2023 (matches legacy)
microdata_inc %>%
  .[year(data_inscricao) < 2023] %>%
  fwrite(out_microdata_update)

# ---- 2.4: build the wide registro_car x year registration panel ------------
# NOTE(migration): the legacy line built this panel from an object named
# `microdata`, which is never defined in this section (only `microdata_inc` is) --
# a latent bug (see docs/notes/car_migration_issues.md, issue #1). The rest of the
# legacy pipeline defines `microdata` as `fread(temas_ambientais_update.csv)` (the
# 2023-excluded update), so we read that back here, which is the clearly-intended
# object and keeps the panel consistent with what stage 01 consumes.
microdata <- fread(out_microdata_update)

years_dt <- data.table(year = c(2013:2022), merge_index = rep(1, 10))

car_and_reg_year_tmp <- microdata %>% copy() %>%
  .[, year_car_registration := year(data_inscricao)] %>%
  .[, .N, .(year_car_registration, registro_car)] %>%
  .[, .(year_car_registration, registro_car)] %>%
  .[, merge_index := 1] %>%
  merge(years_dt, "merge_index", all = TRUE, allow.cartesian = TRUE) %>%
  .[, year_car_reg_indic := (year_car_registration <= year) * 1] %>%
  .[, .(registro_car, year_car_reg_indic, year)] %>%
  .[, year := paste0("y", year)]

# NOTE(migration): the legacy `car_and_reg_year_tmp_nas` object (yNA handling) was
# computed and then removed without being used by the wide cast below; dropped as
# dead code (issue #2).
car_and_reg_year_wide <- car_and_reg_year_tmp %>% copy() %>%
  .[, GRP := .GRP, .(registro_car, year)] %>% .[!duplicated(GRP)] %>% .[, GRP := NULL] %>%
  setnafill(fill = 0, cols = c("year_car_reg_indic")) %>%
  dcast(registro_car ~ year, value.var = "year_car_reg_indic") %>%
  .[order(y2013, y2014, y2015, y2016, y2017, y2018, y2019, y2020, y2021, y2022)]

fwrite(car_and_reg_year_wide, out_reg_year_wide)

message_with_lines(paste0("Wrote: ", out_microdata_update))
message_with_lines(paste0("Wrote: ", out_reg_year_wide))
