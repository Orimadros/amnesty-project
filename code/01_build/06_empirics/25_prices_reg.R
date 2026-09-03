# Stage 25 (recovered code + data): the land-price regression dataset -- the
# data half of the market-expectations exhibit (tab:6, "Moral Hazard Estimates
# Using Land Prices").
#
# Port of 2_empirics.R:2306-2580 ("PRICE REGS"), runnable after the 2026-08-10
# fetch recovered its inputs: the parcel_nb_lavoura_wide.rds files for the
# legal / eligible / ineligible groups (July-2025 vintage, the only surviving
# one), now at data/legacy_dropbox/pptw_data_output/parcels_NB_Lavoura/.
#
# What the legacy block does:
#   :2329-2344  region-level area sums + parcel counts by group -> shares
#   :2346-2350  attach the legal group's price_YYYY_lavoura columns (legacy uses
#               POSITIONAL indexing l[, c(2, 1686:1701)]; here by name, verified)
#   :2352-2547  13 hand-patched regions whose lavoura price is taken from a
#               specific FNP series (stem table below, verbatim from the code)
#   :2557-2570  total area/count, pivot to long -> prices_reg
#   :2572-2574  joins of yearly_average_price_region (price_north) and `output`
#               (turnover shares) -- PRODUCERS MISSING from every recovered
#               script; see the walls note below
#   :2576-2579  write prices_reg.dta + shares5.csv
#   :2317-2324  the less_1500 region table (needs less_1500_merge.dta, missing;
#               the code's own commented alternative :2318 -- NUM_ARE < 1500 --
#               is used instead, behind EMP_LESS1500_RULE)
#
# WALLS (documented, not invented):
#   1. yearly_average_price_region: no recovered script builds it. The :2573
#      fallback (`price_north <- price_lavoura` when NA) is exactly what happens
#      if the join contributes nothing, so prices_reg here carries price_north =
#      price_lavoura for every region-year. Flagged in the output.
#   2. share_{eligible,ineligible}_turnover: producer missing (land-transactions
#      work). Columns written as NA.
#   3. The tab:6 regressions ran in Stata; no do-file recovered. This stage
#      stops at the dataset.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(here)
})

dd <- here("data", "legacy_dropbox", "pptw_data_output", "parcels_NB_Lavoura")
emp_dir <- here("data", "intermediate", "empirics")

message("reading the parcel_nb_lavoura wides (large)...")
legal <- readRDS(file.path(dd, "legal_parcels_all", "legal_parcel_nb_lavoura_wide.rds"))
eligible <- readRDS(file.path(dd, "eligible_parcels_all", "eligible_parcel_nb_lavoura_wide.rds"))
ineligible <- readRDS(file.path(dd, "ineligible_parcels_all", "ineligible_parcel_nb_lavoura_wide.rds"))
message("legal ", nrow(legal), " x ", ncol(legal),
        " | eligible ", nrow(eligible), " x ", ncol(eligible),
        " | ineligible ", nrow(ineligible), " x ", ncol(ineligible))

psum <- function(..., na.rm = TRUE) rowSums(cbind(...), na.rm = na.rm)

# ---- region-level areas and counts (:2329-2344) ------------------------------
p1 <- legal %>% group_by(region_id) %>%
  mutate(legal_area = sum(area, na.rm = TRUE)) %>% add_count() %>%
  filter(row_number(legal_area) == 1) %>% dplyr::select(region_id, legal_area, n)
p2 <- eligible %>% group_by(region_id) %>%
  mutate(eligible_area = sum(area, na.rm = TRUE)) %>% add_count() %>%
  filter(row_number(eligible_area) == 1) %>% dplyr::select(region_id, eligible_area, n)
p3 <- ineligible %>% group_by(region_id) %>%
  mutate(ineligible_area = sum(NUM_ARE, na.rm = TRUE)) %>% add_count() %>%
  filter(row_number(ineligible_area) == 1) %>% dplyr::select(region_id, ineligible_area, n)
colnames(p1)[3] <- "legal_count"
colnames(p2)[3] <- "eligible_count"
colnames(p3)[3] <- "ineligible_count"

shares <- left_join(left_join(p1, p2, "region_id"), p3, "region_id")
shares[is.na(shares)] <- 0

shares$eligible_share <- shares$eligible_area /
  psum(shares$legal_area, shares$eligible_area, shares$ineligible_area)
shares$ineligible_share <- shares$ineligible_area /
  psum(shares$legal_area, shares$eligible_area, shares$ineligible_area)
shares$eligible_share2 <- shares$eligible_count /
  (shares$eligible_count + shares$ineligible_count + shares$legal_count)
shares$ineligible_share2 <- shares$ineligible_count /
  (shares$eligible_count + shares$ineligible_count + shares$legal_count)

# ---- attach the legal group's lavoura price columns (:2346-2350) -------------
l <- legal %>% group_by(region_id) %>% filter(row_number(region_id) == 1) %>% ungroup()
price_cols <- grep("^price_[0-9]{4}_lavoura$", names(l), value = TRUE)
message(length(price_cols), " price_YYYY_lavoura columns found (legacy indexed 16 by position)")
shares <- left_join(shares, l[, c("region_id", price_cols)], "region_id")

# ---- hand-patched regions (:2352-2547, stems verbatim) -----------------------
patch <- c(
  `77`  = "preco_cerrado_proximo_a_belem_brasilia_a_leste",
  `119` = "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina",
  `120` = "preco_pastagem_formada_de_alto_suporte_redencao",
  `121` = "preco_mata_de_facil_acesso",
  `122` = "preco_pastagem_nativa_com_acesso_marajo",
  `124` = "preco_pastagem_formada_vilhena_pimenta_bueno",
  `125` = "preco_pastagem_formada_porto_velho",
  `126` = "preco_pastagem_formada_no_asfalto_rio_branco",
  `127` = "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca",
  `128` = "preco_mata",
  `129` = "preco_terra_agricola_de_baixa_produtividade_em_varzea",
  `130` = "preco_pastagem_nativa_em_varzea",
  `131` = "preco_pastagem_formada_de_alto_suporte"
)
for (rid in names(patch)) {
  r <- as.numeric(rid)
  if (!r %in% shares$region_id) { message("patch region ", r, " absent from shares -- skipped"); next }
  for (y in 2002:2015) {
    src <- paste0(patch[[rid]], "_", y)
    dst <- paste0("price_", y, "_lavoura")
    if (!src %in% names(legal)) { message("MISSING source column ", src, " -- patch skipped"); next }
    if (!dst %in% names(shares)) next
    val <- legal[legal$region_id == r, src][1]
    shares[shares$region_id == r, dst] <- as.numeric(unlist(val))[1]
  }
}

# ---- totals + pivot (:2557-2570) ---------------------------------------------
shares$total_area <- shares$legal_area + shares$eligible_area + shares$ineligible_area
shares$total_count <- shares$eligible_count + shares$ineligible_count + shares$legal_count

prices_reg <- shares %>%
  dplyr::select(-legal_area, -eligible_area, -ineligible_area) %>%
  pivot_longer(cols = starts_with("price_"), names_to = "year",
               values_to = "price_lavoura") %>%
  mutate(year = as.integer(gsub("price_(\\d{4})_lavoura", "\\1", year)))

# ---- the :2572-2574 joins ----------------------------------------------------
# yearly_average_price_region RECOVERED 2026-08-10 as an xlsx in
# data/input/landvalues/vnp/ (built by the REGION_ID_CORRECTION/MAPLAVOURA
# chain in the same folder): (region_id, year, price_north). :2573's fallback
# fills price_north with price_lavoura where the join is NA -- as in legacy.
yap_f <- here("data", "legacy_dropbox", "input_landvalues", "vnp",
              "yearly_average_price_region.xlsx")
if (file.exists(yap_f)) {
  yap <- readxl::read_excel(yap_f)
  prices_reg <- left_join(prices_reg, yap, c("region_id", "year"))
  n_north <- sum(!is.na(prices_reg$price_north))
  prices_reg$price_north <- ifelse(is.na(prices_reg$price_north),
                                   prices_reg$price_lavoura, prices_reg$price_north)
  message("price_north joined from yearly_average_price_region.xlsx: ",
          n_north, " region-years matched")
} else {
  prices_reg$price_north <- prices_reg$price_lavoura
  message("WALL: yearly_average_price_region.xlsx not found -- price_north = price_lavoura")
}
# turnover shares: producer still missing from every recovered script.
prices_reg$share_eligible_turnover <- NA_real_
prices_reg$share_ineligible_turnover <- NA_real_
message("WALL: turnover shares NA (producer missing)")

# ---- less_1500 (:2317-2324) --------------------------------------------------
# less_1500_merge.dta missing; the code's commented rule :2318 (NUM_ARE < 1500)
# is the only recoverable definition. eligible_second_policy needs the did.dta
# `subs` covariates -- computed from stage 19's rebuilt panel + temas areas.
if (Sys.getenv("EMP_LESS1500_RULE", unset = "1") != "0") {
  inel <- ineligible
  inel$less_1500 <- ifelse(inel$NUM_ARE < 1500, 1, 0)

  esp <- tryCatch({
    rp <- as.data.table(readRDS(here("data", "legacy_dropbox", "output_full",
                                     "rebuilt_did_panel.rds")))
    fy <- rp[group == "inelegible" & rate > 10, .(first_year = min(year)), by = COD_IMO]
    tem_f <- here("data", "legacy_dropbox", "miseEnPlace_full", "temas_ambientais_update.csv")
    if (!file.exists(tem_f)) tem_f <- here("data", "input", "sicar", "microdata",
                                           "temas_ambientais.csv")
    tem <- fread(tem_f, select = c("registro_car", "area_do_imovel"))
    tem <- unique(tem, by = "registro_car")
    fy <- merge(fy, tem, by.x = "COD_IMO", by.y = "registro_car", all.x = TRUE)
    fy[, eligible_second_policy := as.integer(first_year > 2008 & first_year < 2012 &
                                              area_do_imovel < 2500)]
    fy[, .(COD_IMO, eligible_second_policy)]
  }, error = function(e) { message("eligible_second_policy unavailable: ", e$message); NULL })

  if (!is.null(esp) && "COD_IMO" %in% names(inel)) {
    inel <- left_join(inel, esp, "COD_IMO")
  } else {
    inel$eligible_second_policy <- NA_integer_
    message("NOTE: eligible_second_policy not joined (no COD_IMO key or panel missing)")
  }
  less_1500 <- inel %>% group_by(region_id) %>%
    mutate(less_1500 = mean(less_1500, na.rm = TRUE),
           eligible_second_policy = mean(eligible_second_policy, na.rm = TRUE)) %>%
    filter(row_number(less_1500) == 1) %>%
    dplyr::select(region_id, less_1500, eligible_second_policy)
  less_1500$eligible_second_policy <- ifelse(is.nan(less_1500$eligible_second_policy),
                                             NA, less_1500$eligible_second_policy)
  fwrite(as.data.table(less_1500), file.path(emp_dir, "less_1500.csv"))
  message("less_1500 regions: ", nrow(less_1500))
}

fwrite(as.data.table(prices_reg), file.path(emp_dir, "prices_reg.csv"))
fwrite(as.data.table(shares), file.path(emp_dir, "shares5.csv"))

cat("\n================ PRICES_REG (tab:6 data half) ================\n")
cat("regions: ", length(unique(prices_reg$region_id)),
    " | region-years: ", nrow(prices_reg),
    " | years: ", paste(range(prices_reg$year), collapse = "-"), "\n", sep = "")
s <- as.data.table(prices_reg)[!is.na(price_lavoura)]
cat("region-years with a lavoura price: ", nrow(s), "\n", sep = "")
cat("mean eligible_share ", round(mean(shares$eligible_share, na.rm = TRUE), 3),
    " | mean ineligible_share ", round(mean(shares$ineligible_share, na.rm = TRUE), 3),
    "\n", sep = "")
cat("Wrote: prices_reg.csv, shares5.csv",
    if (Sys.getenv("EMP_LESS1500_RULE", unset = "1") != "0") ", less_1500.csv", "\n", sep = "")
