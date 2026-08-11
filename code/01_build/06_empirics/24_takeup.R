# Stage 24 (recovered code + data): the 2009 Amnesty take-up build -- takeup.dta.
#
# Port of legacy_repo/code/3_policy1.R, runnable end to end for the first time
# after the 2026-08-10 Dropbox fetch recovered its inputs:
#   data/legacy_dropbox/input_terralegal/DadosTerraLegal.csv        (applications)
#   data/legacy_dropbox/input_terralegal/shapes/parcelageo_*AS_WKT.csv (audited shapes)
#   data/legacy_dropbox/input_titles/{sncr,sigef,snci}              (title registries)
#   data/legacy_dropbox/input_auxiliary/uf_uf_id.xlsx
#   data/legacy_dropbox/miseEnPlace_full/Munic_Micro_Meso_Region_Codes.csv
#   data/legacy_dropbox/output_full/car_eligible_cleaned.shp        (April vintage --
#     the file 3_policy1.R:305 reads from data/intermediate/)
#   data/legacy_dropbox/{output_full,data_root}/CAR_eligible_defo_YYYY.rds
#
# What it builds (3_policy1.R line refs):
#   :55-64   terra_legal cleaning (dedup by requester/muni/area; drop the
#            all-zero processo; unique processo via add_count/n==1; year_request)
#   :117-139 tlp_shapes: audited property shapes, natureza Particular + Serfal
#   :143-171 pipeline 0: merge on (nome_requerente, municipio) -> WKT polygons
#   :183-249 pipelines 1-2: SNCR (first_name, ha_m2, municipio) -> codigo_imo ->
#            SIGEF / SNCI polygons
#   :269-301 pipeline 3 (area_do_imovel, municipio) + assembly of terra_legal_shp
#   :304-339 applies: eligible parcels >= 90% covered by an application polygon
#   :458-477 receives: same with status_processo == "Titulado"
#   :342-455 the eligible wide/long panels 1989-2008 + 2005-2014, when_occupied
#            (years-since-occupation as of 2008, rate >= 10 rule), defo_rate_2008
#   :479     write takeup.dta equivalent -> data/intermediate/empirics/takeup.csv
#
# Deviations, all documented:
#   - Plots (:66-110 ggsave, tm_shape blocks) and the CAFIR pipeline 4 (:486+,
#     which sits AFTER the takeup export and feeds nothing) are not ported.
#   - CAR_eligible_defo_2002/2003/2004.rds are behind Dropbox's anonymous listing
#     cap; those three years of the occupation history are BRIDGED from our own
#     parcel_defo_{2002,2003,2004}.csv restricted to the eligible id set (same
#     MapBiomas source; EMP_BRIDGE_0204=0 drops the bridge and lets
#     when_occupied skip 2002-2004 as legacy would with missing files).
#   - The takeup regressions themselves (tab:25) ran in Stata; no do-file was
#     recovered, so this stage stops at the dataset (per project rules: migrate
#     only code we have).

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
  library(stringi)
  library(readr)
  library(readxl)
  library(sf)
  library(here)
})
sf_use_s2(FALSE)

dd <- here("data", "legacy_dropbox")
emp_dir <- here("data", "intermediate", "empirics")

# ---- terra_legal applications (:55-64) ---------------------------------------
terra_legal <- read_delim(file.path(dd, "input_terralegal", "DadosTerraLegal.csv"),
                          delim = ";", escape_double = FALSE,
                          locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE,
                          show_col_types = FALSE)
terra_legal$municipio <- tolower(stri_trans_general(terra_legal$municipio, "Latin-ASCII"))
terra_legal$nome_requerente <- str_squish(stri_trans_general(terra_legal$nome_requerente, "Latin-ASCII"))
terra_legal$first_name <- tolower(stri_trans_general(gsub(" .*", "", terra_legal$nome_requerente), "Latin-ASCII"))
terra_legal$ha_m <- gsub("\\..*", "", as.character(terra_legal$area_do_imovel))
terra_legal$ha_m2 <- str_extract(as.character(terra_legal$area_do_imovel), "([0-9]+)[\\.]*([0-9])")
terra_legal <- terra_legal %>% group_by(nome_requerente, municipio, area_do_imovel) %>%
  filter(row_number(nome_requerente) == 1)
terra_legal <- terra_legal %>% filter(numero_processo != "00000.000000/0000-00")
terra_legal <- terra_legal %>% group_by(numero_processo) %>% add_count() %>%
  filter(n == 1) %>% ungroup() %>% dplyr::select(-n)
terra_legal$year_request <- str_extract(gsub(".*\\/", "", terra_legal$numero_processo),
                                        "^([0-9])([0-9])([0-9])([0-9])")
message("terra_legal applications: ", nrow(terra_legal))

amazon_bioma <- read_sf(here("data", "input", "aux", "amazon_biome_border",
                             "amazon_biome_border.shp"))

# ---- tlp_shapes (:117-139) ---------------------------------------------------
shp_files <- list.files(file.path(dd, "input_terralegal", "shapes"),
                        pattern = "*.csv", recursive = TRUE)
tlp_shapes <- data.frame()
for (k in shp_files) {
  d <- read_csv(file.path(dd, "input_terralegal", "shapes", k),
                col_types = cols(codigo_imovel = col_character()))
  tlp_shapes <- bind_rows(tlp_shapes, data.frame(d))
  message("read ", k)
}
uf_uf_id <- read_excel(file.path(dd, "input_auxiliary", "uf_uf_id.xlsx"))
muni_crosswalk <- read_csv(file.path(dd, "miseEnPlace_full",
                                     "Munic_Micro_Meso_Region_Codes.csv"),
                           locale = locale(encoding = "ISO-8859-1"),
                           show_col_types = FALSE)

tlp_shapes <- tlp_shapes %>% left_join(uf_uf_id, "uf_id")
tlp_shapes$nome_requerente <- str_squish(stri_trans_general(tlp_shapes$detentor_nome, "Latin-ASCII"))
tlp_shapes$ha_m <- gsub("\\..*", "", as.character(tlp_shapes$area_ha))
muni_crosswalk$municipio_id <- muni_crosswalk$munic_code
tlp_shapes <- tlp_shapes %>% left_join(muni_crosswalk, "municipio_id")
tlp_shapes$municipio <- tolower(stri_trans_general(tlp_shapes$munic, "Latin-ASCII"))
colnames(tlp_shapes)[1] <- "geometry"
tlp_shapes <- tlp_shapes %>% filter(natureza == "Particular") %>%
  filter(orgao_publico == "Serfal") %>% filter(!is.na(nome_requerente))
message("tlp_shapes (Particular/Serfal): ", nrow(tlp_shapes))

# ---- pipeline 0: name x municipality (:143-154, :171) ------------------------
terra_legal_shp <- terra_legal %>%
  merge(tlp_shapes, c("nome_requerente", "municipio"))
terra_legal_shp <- st_as_sf(terra_legal_shp, wkt = "geometry", crs = st_crs(amazon_bioma))
terra_legal_shp <- st_intersection(terra_legal_shp, amazon_bioma)
terra_legal_shp <- terra_legal_shp %>% group_by(nome_requerente, numero_processo, art) %>%
  slice_max(area_ha, with_ties = FALSE) %>% ungroup()
terra_legal <- terra_legal %>% filter(!numero_processo %in% terra_legal_shp$numero_processo)
message("pipeline 0 matched: ", nrow(terra_legal_shp), " | unmatched left: ", nrow(terra_legal))

# ---- pipeline 1: SNCR -> SIGEF (:183-219) ------------------------------------
sncr_files <- list.files(file.path(dd, "input_titles", "sncr"),
                         pattern = "*.csv", recursive = TRUE)
# all columns as character: the state files type-guess differently (legacy's
# base rbind coerced silently), and the downstream parsing treats AREA TOTAL as
# the raw string anyway
sncr <- data.frame()
for (k in sncr_files) {
  d <- read_delim(file.path(dd, "input_titles", "sncr", k), delim = ";",
                  escape_double = FALSE, locale = locale(), trim_ws = TRUE,
                  col_types = cols(.default = "c"))
  sncr <- bind_rows(sncr, d)
}
message("sncr rows: ", nrow(sncr))
sncr$area_ha <- as.numeric(str_replace(str_remove_all(sncr$`ÁREA TOTAL`, "\\."), ",", "."))
sncr$ha_m <- gsub("\\..*", "", as.character(sncr$area_ha))
sncr$municipio <- tolower(stri_trans_general(sncr$`MUNICÍPIO`, "Latin-ASCII"))
sncr$first_name <- tolower(stri_trans_general(gsub(" .*", "", sncr$TITULAR), "Latin-ASCII"))
sncr <- sncr %>% filter(!str_detect(`ÁREA TOTAL`, "^\\,"))
sncr$ha_m2 <- str_extract(str_replace(str_remove_all(sncr$`ÁREA TOTAL`, "\\."), ",", "."),
                          "([0-9]+)\\.([0-9])")
sncr$ha_m2 <- ifelse(str_detect(sncr$ha_m2, "\\.0"),
                     gsub("\\..*", "", as.character(sncr$ha_m2)), sncr$ha_m2)

terra_legal_sncr <- terra_legal %>% merge(sncr, c("first_name", "ha_m2", "municipio")) %>%
  arrange(numero_processo)
terra_legal_sncr$codigo_imo <- terra_legal_sncr$`CÓDIGO DO IMOVEL`
terra_legal <- terra_legal %>% filter(!numero_processo %in% terra_legal_sncr$numero_processo)

sigef <- read_sf(file.path(dd, "input_titles", "sigef", "sigef_br", "Sigef Brasil.shp")) %>%
  st_transform(crs = 4674) %>% st_make_valid()
sigef$codigo_imo <- as.numeric(sigef$codigo_imo)
terra_legal_sncr_sigef <- terra_legal_sncr %>% merge(sigef, "codigo_imo")
terra_legal_sncr <- terra_legal_sncr %>%
  filter(!numero_processo %in% terra_legal_sncr_sigef$numero_processo)
terra_legal_sncr_sigef <- st_as_sf(terra_legal_sncr_sigef)
message("pipeline 1 (SNCR->SIGEF) matched: ", nrow(terra_legal_sncr_sigef))

# ---- pipeline 2: SNCR -> SNCI (:239-251) -------------------------------------
snci <- read_sf(file.path(dd, "input_titles", "snci", "snci_certificacoes",
                          "Imvel certificado SNCI Brasil.shp")) %>%
  st_transform(crs = 4674) %>% st_make_valid()
snci$codigo_imo <- as.numeric(snci$cod_imovel)
terra_legal_sncr_snci <- terra_legal_sncr %>% merge(snci, "codigo_imo")
terra_legal_sncr <- terra_legal_sncr %>%
  filter(!numero_processo %in% terra_legal_sncr_snci$numero_processo)
terra_legal_sncr_snci <- st_as_sf(terra_legal_sncr_snci)
message("pipeline 2 (SNCR->SNCI) matched: ", nrow(terra_legal_sncr_snci))

# ---- pipeline 3: area x municipality on the unmatched shapes (:269-295) ------
tlp_shapes_unmatched <- tlp_shapes %>% filter(!id %in% terra_legal_shp$id)
tlp_shapes_unmatched$area_do_imovel <- as.character(tlp_shapes_unmatched$area_ha)
terra_legal$area_do_imovel <- as.character(terra_legal$area_do_imovel)
terra_legal_shp2 <- terra_legal %>% merge(tlp_shapes_unmatched, c("area_do_imovel", "municipio"))
terra_legal_shp2 <- st_as_sf(terra_legal_shp2, wkt = "geometry", crs = st_crs(amazon_bioma))
terra_legal <- terra_legal %>% filter(!numero_processo %in% terra_legal_shp2$numero_processo)
message("pipeline 3 matched: ", nrow(terra_legal_shp2), " | final unmatched: ", nrow(terra_legal))

# ---- assembly (:299-301): polygon set + status, geometry-safe ----------------
pick <- function(x, extra = character()) {
  keep <- intersect(c("numero_processo", "nome_requerente", "status_processo",
                      "codigo_imo", "codigo_imovel", extra), names(x))
  y <- x[, keep]
  st_geometry(y) <- st_geometry(x)
  y
}
tls <- pick(terra_legal_shp)
p1 <- pick(terra_legal_sncr_sigef) %>% filter(!codigo_imo %in% tls$codigo_imo)
seen <- c(tls$codigo_imo, p1$codigo_imo)
p2 <- pick(terra_legal_sncr_snci) %>% filter(!codigo_imo %in% seen)
p3 <- pick(terra_legal_shp2) %>% filter(!codigo_imovel %in% seen)
common <- Reduce(intersect, list(names(tls), names(p1), names(p2), names(p3)))
applications_shp <- rbind(tls[, common], p1[, common], p2[, common], p3[, common])
message("combined application polygons: ", nrow(applications_shp))

# ---- applies / receives (:304-339, :458-477) ---------------------------------
car_eligible <- read_sf(file.path(dd, "output_full", "car_eligible_cleaned.shp"))

overlap_flag <- function(dataset_A, dataset_B, threshold = 0.9) {
  dataset_B <- st_transform(dataset_B, st_crs(dataset_A))
  idx <- st_intersects(dataset_A, dataset_B)
  vapply(seq_along(idx), function(i) {
    b <- idx[[i]]
    if (length(b) == 0) return(FALSE)
    inter <- suppressWarnings(st_intersection(dataset_A[i, ], dataset_B[b, ]))
    if (nrow(inter) == 0) return(FALSE)
    as.numeric(sum(st_area(inter)) / st_area(dataset_A[i, ])) >= threshold
  }, logical(1))
}

message("computing applies (>=90% overlap with any application)...")
applies_flag <- overlap_flag(car_eligible, applications_shp)
message("applies parcels: ", sum(applies_flag))
message("computing receives (>=90% overlap with a Titulado application)...")
receives_flag <- overlap_flag(car_eligible,
                              applications_shp %>% filter(status_processo == "Titulado"))
message("receives parcels: ", sum(receives_flag))

flags <- data.table(COD_IMO = car_eligible$COD_IMO,
                    applies = as.integer(applies_flag),
                    receives = as.integer(receives_flag))

# ---- occupation history 1989-2008 + covariates (:342-455) --------------------
read_year_rds <- function(f) {
  x <- as.data.table(readRDS(f))
  setnames(x, c("COD_IMO", "deforested_area_hc", "deforestation_rate"))
  x[, `:=`(deforested_area_hc = as.numeric(deforested_area_hc),
           deforestation_rate = as.numeric(deforestation_rate))]
  x[is.nan(deforestation_rate), deforestation_rate := NA]
  x[, n := .N, by = COD_IMO]
  x <- x[n == 1][, n := NULL]
  x
}
files <- c(
  setNames(file.path(dd, "data_root", sprintf("CAR_eligible_defo_%d.rds", 1989:2001)),
           1989:2001),
  setNames(file.path(dd, "output_full", sprintf("CAR_eligible_defo_%d.rds", 2005:2008)),
           2005:2008)
)
panel <- rbindlist(lapply(names(files), function(y) {
  x <- read_year_rds(files[[y]]); x[, year := as.integer(y)]; x
}))

# bridge 2002-2004 from our own measured panels (same MapBiomas source), eligible ids
if (Sys.getenv("EMP_BRIDGE_0204", unset = "1") != "0") {
  ids <- unique(car_eligible$COD_IMO)
  br <- rbindlist(lapply(2002:2004, function(y) {
    x <- fread(file.path(emp_dir, sprintf("parcel_defo_%d.csv", y)),
               select = c("car_id", "deforested_area_ha", "deforestation_rate", "year"))
    x <- x[car_id %in% ids,
           .(COD_IMO = car_id, deforested_area_hc = deforested_area_ha,
             deforestation_rate, year)]
    x
  }))
  panel <- rbind(panel, br)
  message("2002-2004 bridged from our panels: ", nrow(br), " parcel-years")
}

# when_occupied (:448-450): first year with rate >= 10, expressed as 2008 - year
occ <- panel[deforestation_rate >= 10,
             .(first_occ = min(year)), by = COD_IMO]
flags <- merge(flags, occ, by = "COD_IMO", all.x = TRUE)
flags[, when_occupied := fifelse(is.na(first_occ), NA_integer_, 2008L - first_occ)]
flags[, first_occ := NULL]

# defo_rate_2008 (:451-452)
r08 <- panel[year == 2008, .(COD_IMO, defo_rate_2008 = deforestation_rate)]
flags <- merge(flags, unique(r08, by = "COD_IMO"), by = "COD_IMO", all.x = TRUE)

# area covariates from the SICAR microdata (:443-446)
# prefer the _update microdata (has `cancelled`); fall back to our local
# temas_ambientais.csv (same fields minus cancelled), selecting what exists
tem_f <- file.path(dd, "miseEnPlace_full", "temas_ambientais_update.csv")
if (!file.exists(tem_f)) tem_f <- here("data", "input", "sicar", "microdata",
                                       "temas_ambientais.csv")
if (file.exists(tem_f)) {
  want <- c("registro_car", "uf", "codigo_ibge", "area_do_imovel",
            "area_rural_consolidada", "cancelled")
  avail <- intersect(want, names(fread(tem_f, nrows = 0)))
  tem <- fread(tem_f, select = avail)
  tem <- unique(tem, by = "registro_car")
  setnames(tem, "registro_car", "COD_IMO")
  flags <- merge(flags, tem, by = "COD_IMO", all.x = TRUE)
  message("covariates joined from ", basename(tem_f), ": ",
          paste(setdiff(avail, "registro_car"), collapse = ", "))
} else {
  message("NOTE: temas microdata not found -- area covariates omitted")
}

fwrite(flags, file.path(emp_dir, "takeup.csv"))
cat("\n================ TAKE-UP DATASET ================\n")
cat("eligible parcels: ", nrow(flags),
    " | applies: ", sum(flags$applies), sprintf(" (%.1f%%)", 100 * mean(flags$applies)),
    " | receives: ", sum(flags$receives), sprintf(" (%.1f%%)", 100 * mean(flags$receives)),
    "\n", sep = "")
cat("paper anchors: application rate lower bound ~10%; titled-by-2016 ~42% of audited\n")
cat("Wrote: ", file.path(emp_dir, "takeup.csv"), "\n", sep = "")
