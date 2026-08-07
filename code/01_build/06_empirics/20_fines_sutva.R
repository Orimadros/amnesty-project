# Stage 20 (recovered code): the SUTVA / land-grabber-response exhibit.
#
# Port of two recovered legacy scripts, run end to end:
#   1. data/legacy_dropbox/miseEnPlace/multas_updated.R (build section) -- builds
#      `reg1_n.dta`: every IBAMA fine inside the undesignated federal glebas
#      (CNFP 2013, protecao "SEM DESTINACAO", control areas erased), flagged
#      `prior_fine` = 1 on the row of the entity's FIRST gleba-year fine when the
#      entity was fined inside the control areas (federal UC/TI) in an earlier
#      year. The exported reg1_n.dta itself is NOT in the Dropbox share, but all
#      of its inputs are on disk, so it is rebuilt here.
#   2. legacy_repo/fines_robustness/fines_robustness_sutva.do -- the regressions:
#      areg prior_fine after [controls] if gleba_first_year > 2005,
#        a(MUNICIPIO|UF) cluster(MUNICIPIO)
#      plus the enforcement_clouds series: reg enf_{target,control}_adj after
#      if year > 2005, robust; and the pre-2009 means.
#
# Inputs (all in hand):
#   data/legacy_dropbox/miseEnPlace/autos_infracao_df.rds     IBAMA fines microdata
#   data/input/cnfp/SHP_2013/<UF>/<UF>.shp                    CNFP 2013
#   data/input/aux/amazon_biome_border/amazon_biome_border.shp
#   data/legacy_dropbox/fines_robustness/enforcement_clouds.dta
#
# Faithfulness notes:
#   - sf_use_s2(FALSE): the legacy scripts run GEOS planar ops on geographic
#     coordinates; replicated (the project's s2 default is overridden here only).
#   - Legacy's cloud/cloud_adj rowwise loop indexes reg1$CPF_CNPJ_INFRATOR, which
#     survives the dplyr::select only because reg1 is still grouped by it (grouped
#     select retains grouping variables), so the loop DOES run full length. Here
#     the same quantity is computed vectorised: mean enforcement over all years
#     <= the entity's gleba_first_year.
#   - multas_updated.R does NOT restrict to year >= 2002 (that filter is
#     commented out there, unlike multas_RegsFE.R).

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
  library(sf)
  library(haven)
  library(here)
})
source(here("code", "01_build", "06_empirics", "_helpers_feols.R"))
if (!feols_selftest()) stop("feols self-test failed -- do not trust these estimates")

sf_use_s2(FALSE)

emp_dir <- here("data", "intermediate", "empirics")

# ---- fines microdata ---------------------------------------------------------
message("reading fines microdata...")
autos <- readRDS(here("data", "legacy_dropbox", "miseEnPlace", "autos_infracao_df.rds"))
autos <- autos %>% filter(CPF_CNPJ_INFRATOR != "")
autos$year <- as.numeric(str_extract(autos$DAT_LANCAMENTO, "([0-9])([0-9])([0-9])([0-9])"))
autos <- autos %>% filter(!is.na(year))

# ---- CNFP 2013: control areas (federal UC/TI) and target glebas --------------
message("reading CNFP 2013 + biome border...")
amazon_bioma <- read_sf(here("data", "input", "aux", "amazon_biome_border",
                             "amazon_biome_border.shp"))
shp <- list.files(here("data", "input", "cnfp", "SHP_2013"),
                  pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
fpnd <- do.call(rbind, lapply(shp, read_sf))
fpnd <- st_intersection(fpnd, amazon_bioma)

control_areas <- fpnd %>% filter(governo == "FEDERAL") %>%
  filter(str_detect(classe, "UC") | str_detect(classe, "TI"))
control_areas <- st_intersection(control_areas, amazon_bioma)

glebas_alt <- fpnd %>% filter(governo == "FEDERAL") %>%
  filter(str_detect(protecao, "SEM DESTINACAO"))
glebas_alt <- st_intersection(glebas_alt, amazon_bioma)
glebas_alt <- st_difference(glebas_alt, st_union(control_areas))
glebas_alt_valid <- st_make_valid(glebas_alt)
message("control polygons: ", nrow(control_areas),
        " | gleba polygons: ", nrow(glebas_alt_valid))

# ---- fines -> points -> inside control / inside glebas -----------------------
autos$NUM_LATITUDE_AUTO <- as.numeric(str_replace(autos$NUM_LATITUDE_AUTO, ",", "."))
autos$NUM_LONGITUDE_AUTO <- as.numeric(str_replace(autos$NUM_LONGITUDE_AUTO, ",", "."))
multas_final <- autos %>% filter(!is.na(NUM_LATITUDE_AUTO)) %>%
  filter(NUM_LATITUDE_AUTO != 0)

to_sf <- function(df, target) {
  df %>%
    mutate(lon = as.numeric(NUM_LONGITUDE_AUTO), lat = as.numeric(NUM_LATITUDE_AUTO)) %>%
    filter(!is.na(lon), !is.na(lat)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = st_crs(target))
}

message("locating fines inside control areas / glebas...")
fines_in_control_areas <- st_join(to_sf(multas_final, control_areas), control_areas,
                                  join = st_within, left = FALSE)
fines_in_glebas_alt <- st_join(to_sf(multas_final, glebas_alt_valid), glebas_alt_valid,
                               join = st_within, left = FALSE)
message("fines in control: ", nrow(fines_in_control_areas),
        " | fines in glebas: ", nrow(fines_in_glebas_alt))

# ---- prior_fine construction (multas_updated.R:276-347) ----------------------
fines_in_control_areas <- fines_in_control_areas %>%
  group_by(CPF_CNPJ_INFRATOR) %>%
  mutate(n_municipios_control = log(n_distinct(MUNICIPIO))) %>%
  ungroup()
fines_in_glebas_alt <- fines_in_glebas_alt %>%
  group_by(CPF_CNPJ_INFRATOR) %>%
  mutate(n_municipios_target = log(n_distinct(MUNICIPIO))) %>%
  ungroup()

control_lookup <- fines_in_control_areas %>% st_drop_geometry() %>%
  group_by(CPF_CNPJ_INFRATOR) %>%
  summarize(min_year_control = min(year, na.rm = TRUE), .groups = "drop")
gleba_first <- fines_in_glebas_alt %>% st_drop_geometry() %>%
  group_by(CPF_CNPJ_INFRATOR) %>%
  summarize(first_year_gleba = min(year, na.rm = TRUE), .groups = "drop")

fines_df <- fines_in_glebas_alt %>% st_drop_geometry() %>%
  left_join(control_lookup, by = "CPF_CNPJ_INFRATOR") %>%
  left_join(gleba_first, by = "CPF_CNPJ_INFRATOR") %>%
  mutate(prior_fine = ifelse(!is.na(min_year_control) &
                               year == first_year_gleba &
                               min_year_control < year, 1, 0),
         gleba_first_year = first_year_gleba)

n_ctl <- fines_in_control_areas %>% st_drop_geometry() %>%
  group_by(CPF_CNPJ_INFRATOR) %>%
  arrange(UF, .by_group = TRUE) %>% slice(1) %>% ungroup() %>%
  select(CPF_CNPJ_INFRATOR, n_municipios_control)
fines_df <- fines_df %>% select(-any_of("n_municipios_control")) %>%
  left_join(n_ctl, by = "CPF_CNPJ_INFRATOR")

reg1 <- fines_df %>%
  filter(gleba_first_year > 2004 & gleba_first_year < 2015) %>%
  select(CPF_CNPJ_INFRATOR, gleba_first_year, MUNICIPIO, UF, prior_fine,
         n_municipios_target, n_municipios_control)
reg1$propensity_move <- pmax(reg1$n_municipios_target, reg1$n_municipios_control,
                             na.rm = TRUE)

# cloud controls: mean enforcement over all series-years <= gleba_first_year
ec <- as.data.table(read_dta(here("data", "legacy_dropbox", "fines_robustness",
                                  "enforcement_clouds.dta")))
ec[, enf_control_adj := fraction_control * enforcement_control]
cloud_by_year <- data.table(gleba_first_year = sort(unique(reg1$gleba_first_year)))
cloud_by_year[, cloud := sapply(gleba_first_year, function(g)
  mean(ec[year <= g, enforcement_control]))]
cloud_by_year[, cloud_adj := sapply(gleba_first_year, function(g)
  mean(ec[year <= g, enf_control_adj]))]
reg1 <- reg1 %>% left_join(cloud_by_year, by = "gleba_first_year") %>%
  mutate(cloud = ifelse(is.nan(cloud), NA, cloud),
         cloud_adj = ifelse(is.nan(cloud_adj), NA, cloud_adj))

fwrite(reg1, file.path(emp_dir, "fines_sutva_reg1.csv"))
message("reg1 rebuilt: ", nrow(reg1), " gleba fine rows, ",
        uniqueN(reg1$CPF_CNPJ_INFRATOR), " entities, prior_fine=1 on ",
        sum(reg1$prior_fine), " rows")

# ---- fines_robustness_sutva.do, part 1 ---------------------------------------
r <- as.data.table(reg1)[gleba_first_year > 2005]
r[, after := as.integer(gleba_first_year >= 2009)]

specs <- list()
f1 <- fe_ols(r$prior_fine, cbind(after = r$after), r$MUNICIPIO, r$MUNICIPIO)
specs[["after | a(MUNICIPIO)"]] <- f1
f2 <- fe_ols(r$prior_fine, cbind(after = r$after), r$UF, r$MUNICIPIO)
specs[["after | a(UF)"]] <- f2
f3 <- fe_ols(r$prior_fine, cbind(after = r$after, propensity_move = r$propensity_move),
             r$MUNICIPIO, r$MUNICIPIO)
specs[["after + propensity_move | a(MUNICIPIO)"]] <- f3
f4 <- fe_ols(r$prior_fine, cbind(after = r$after, cloud = r$cloud),
             r$MUNICIPIO, r$MUNICIPIO)
specs[["after + cloud | a(MUNICIPIO)"]] <- f4
f5 <- fe_ols(r$prior_fine, cbind(after = r$after, cloud_adj = r$cloud_adj),
             r$MUNICIPIO, r$MUNICIPIO)
specs[["after + cloud_adj | a(MUNICIPIO)"]] <- f5

res1 <- rbindlist(lapply(names(specs), function(nm) {
  f <- specs[[nm]]
  cbind(data.table(spec = nm), f$coefs,
        data.table(n_obs = f$n_obs, n_clusters = f$n_clusters))
}))

pre_mean <- r[gleba_first_year < 2009, mean(prior_fine)]

cat("\n=========== SUTVA: prior_fine regressions (cluster MUNICIPIO) ===========\n")
print(as.data.frame(res1), digits = 4)
cat(sprintf("\npre-2009 mean prior_fine (2005 < gleba_first_year < 2009): %.5f\n",
            pre_mean))

# event-study variant (:21): year dummies (2008 omitted) + propensity_move
yrs <- setdiff(sort(unique(r$gleba_first_year)), 2008)
Xev <- cbind(sapply(yrs, function(y) as.integer(r$gleba_first_year == y)),
             propensity_move = r$propensity_move)
colnames(Xev) <- c(paste0("y", yrs), "propensity_move")
fev <- fe_ols(r$prior_fine, Xev, r$MUNICIPIO, r$MUNICIPIO)
cat("\n---------- event-study variant (ref 2008, + propensity_move) ----------\n")
print(as.data.frame(fev$coefs), digits = 4)

# ---- fines_robustness_sutva.do, part 2: enforcement intensity ----------------
ec[, `:=`(after = as.integer(year >= 2009),
          enf_target_adj = enforcement_target * fraction_target)]
e <- ec[year > 2005]
res2 <- rbind(
  cbind(data.table(outcome = "enf_target_adj"),
        ols_hc1(e$enf_target_adj, cbind(after = e$after))),
  cbind(data.table(outcome = "enf_control_adj"),
        ols_hc1(e$enf_control_adj, cbind(after = e$after)))
)

cat("\n=========== enforcement_clouds: adj series on after (robust) ===========\n")
print(as.data.frame(res2), digits = 4)
cat(sprintf("\npre-2009 means (2005 < year < 2009): target_adj %.5f | control_adj %.5f\n",
            ec[year > 2005 & year < 2009, mean(enf_target_adj)],
            ec[year > 2005 & year < 2009, mean(enf_control_adj)]))

fwrite(rbind(res1, cbind(data.table(spec = "eventdd"), fev$coefs,
                         data.table(n_obs = fev$n_obs, n_clusters = fev$n_clusters))),
       file.path(emp_dir, "fines_sutva_results.csv"))
fwrite(res2, file.path(emp_dir, "fines_enforcement_results.csv"))
cat("\nWrote: fines_sutva_reg1.csv, fines_sutva_results.csv, fines_enforcement_results.csv\n")
