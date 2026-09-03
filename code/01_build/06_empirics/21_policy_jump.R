# Stage 21 (recovered code): the Policy-Jump table.
#
# Port of data/legacy_dropbox/miseEnPlace/multas_RegsFE.R: hazard-style linear
# probability models on first-time-fined entities around the 2009 policy break,
# municipality FE, errors clustered by municipality. Four outcomes:
#   1. enters                    -- entity established in the control areas
#                                   (federal UC/TI municipalities) is fined in a
#                                   target (gleba) municipality for the 1st time
#   2. enters_target_no_control  -- first target fine with NO prior control fine
#   3. enters_control_after_target -- first control fine AFTER a target fine
#   4. defor_arson               -- fine is deforestation/arson-related
# each regressed on policy = 1{first-fine year >= 2009} (models 1-3) or
# 1{year >= 2009} (model 4). The year-FE event versions (ref 2008) are also
# estimated and their coefficient paths written to CSV.
#
# Inputs (all in hand):
#   data/legacy_dropbox/miseEnPlace/autos_infracao_df.rds   IBAMA fines microdata
#   data/legacy_dropbox/miseEnPlace/muni_control_areas.gpkg municipalities holding
#   data/legacy_dropbox/miseEnPlace/muni_target_areas.gpkg  control/target areas
#
# Faithfulness notes:
#   - Areas here are MUNICIPALITY-level (fine assigned by COD_MUNICIPIO), unlike
#     stage 20's point-in-polygon; that is the legacy design, not a shortcut.
#   - Legacy's `group_by(CPF, year) %>% mutate(min_year = min(year))` makes
#     min_year == year row by row; the loop over 2005-2014 then defines entry as
#     "target fine in year y & any control fine <= y-1". Replicated as written.
#   - multas_RegsFE.R DOES filter year >= 2002 (multas_updated.R does not).
#   - COD_MUNICIPIO and CD_MUN are compared as characters.

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
  library(sf)
  library(here)
})
source(here("code", "01_build", "06_empirics", "_helpers_feols.R"))
if (!feols_selftest()) stop("feols self-test failed -- do not trust these estimates")

emp_dir <- here("data", "intermediate", "empirics")
dd <- here("data", "legacy_dropbox", "miseEnPlace")

# ---- fines microdata ---------------------------------------------------------
autos <- readRDS(file.path(dd, "autos_infracao_df.rds"))
autos <- autos %>% filter(CPF_CNPJ_INFRATOR != "")
autos$year <- as.numeric(str_extract(autos$DAT_LANCAMENTO, "([0-9])([0-9])([0-9])([0-9])"))
autos <- autos %>% filter(!is.na(year))
multas <- autos %>% filter(year >= 2002)
multas$COD_MUNICIPIO <- as.character(multas$COD_MUNICIPIO)

muni_control <- st_read(file.path(dd, "muni_control_areas.gpkg"), quiet = TRUE)
muni_target <- st_read(file.path(dd, "muni_target_areas.gpkg"), quiet = TRUE)
ctl_mun <- as.character(unique(muni_control$CD_MUN))
tgt_mun <- as.character(unique(muni_target$CD_MUN))
message("control munis: ", length(ctl_mun), " | target munis: ", length(tgt_mun))

# ---- outcome 1: enters (multas_RegsFE.R:121-164) -----------------------------
multas_control_areas <- multas %>% filter(COD_MUNICIPIO %in% ctl_mun) %>%
  group_by(CPF_CNPJ_INFRATOR, year) %>% mutate(min_year = min(year, na.rm = TRUE)) %>%
  ungroup()
multas_target_areas <- multas %>% filter(COD_MUNICIPIO %in% tgt_mun)
multas_target_areas <- multas_target_areas[
  multas_target_areas$CPF_CNPJ_INFRATOR %in% unique(multas_control_areas$CPF_CNPJ_INFRATOR), ]
multas_target_areas <- multas_target_areas %>%
  group_by(CPF_CNPJ_INFRATOR, year) %>% mutate(min_year = min(year, na.rm = TRUE)) %>%
  ungroup()

f <- data.frame()
for (y in seq(2005, 2014)) {
  est <- unique(multas_control_areas %>% filter(min_year <= y - 1) %>%
                  pull(CPF_CNPJ_INFRATOR))
  found <- multas_target_areas[multas_target_areas$min_year == y &
                                 multas_target_areas$CPF_CNPJ_INFRATOR %in% est, ]
  found <- found %>% group_by(CPF_CNPJ_INFRATOR) %>% slice(1) %>% ungroup() %>%
    mutate(enters = 1) %>%
    select(CPF_CNPJ_INFRATOR, COD_MUNICIPIO, min_year, enters)
  f <- rbind(f, found)
}

f <- f %>% group_by(CPF_CNPJ_INFRATOR) %>%
  mutate(min_year = min(min_year, na.rm = TRUE)) %>% slice(1) %>% ungroup()
f$entry_year <- f$min_year

f_not <- multas_control_areas[
  !multas_control_areas$CPF_CNPJ_INFRATOR %in% unique(f$CPF_CNPJ_INFRATOR), ] %>%
  mutate(enters = 0) %>%
  select(CPF_CNPJ_INFRATOR, COD_MUNICIPIO, min_year, enters)

f_yes <- multas_control_areas[
  multas_control_areas$CPF_CNPJ_INFRATOR %in% unique(f$CPF_CNPJ_INFRATOR), ] %>%
  select(CPF_CNPJ_INFRATOR, COD_MUNICIPIO, min_year, year) %>%
  left_join(f %>% select(CPF_CNPJ_INFRATOR, entry_year), by = "CPF_CNPJ_INFRATOR") %>%
  filter(year != entry_year) %>%
  select(-entry_year, -year) %>%
  mutate(enters = 0)

multas_final <- rbind(f_yes, f %>% select(-entry_year), f_not) %>%
  filter(min_year >= 2005, min_year <= 2025)
message("multas_final: ", nrow(multas_final), " rows, ",
        sum(multas_final$enters), " entries")

# ---- outcome 2: first target fine, no prior control fine (:166-187) ----------
multas_target_all <- multas %>% filter(COD_MUNICIPIO %in% tgt_mun) %>%
  group_by(CPF_CNPJ_INFRATOR) %>% mutate(target_min_year = min(year, na.rm = TRUE)) %>%
  ungroup()
control_min_year <- multas %>% filter(COD_MUNICIPIO %in% ctl_mun) %>%
  group_by(CPF_CNPJ_INFRATOR) %>%
  summarise(control_min_year = min(year, na.rm = TRUE), .groups = "drop")
multas_target_all <- multas_target_all %>%
  left_join(control_min_year, by = "CPF_CNPJ_INFRATOR") %>%
  mutate(enters_target_no_control = ifelse(
    year == target_min_year &
      (is.na(control_min_year) | target_min_year < control_min_year), 1, 0)) %>%
  filter(target_min_year >= 2005, target_min_year <= 2025)

# ---- outcome 3: first control fine after a target fine (:189-210) ------------
multas_control_all <- multas %>% filter(COD_MUNICIPIO %in% ctl_mun) %>%
  group_by(CPF_CNPJ_INFRATOR) %>% mutate(control_min_year = min(year, na.rm = TRUE)) %>%
  ungroup()
target_min_year <- multas %>% filter(COD_MUNICIPIO %in% tgt_mun) %>%
  group_by(CPF_CNPJ_INFRATOR) %>%
  summarise(target_min_year = min(year, na.rm = TRUE), .groups = "drop")
multas_control_all <- multas_control_all %>%
  left_join(target_min_year, by = "CPF_CNPJ_INFRATOR") %>%
  filter(!is.na(target_min_year)) %>%
  mutate(enters_control_after_target = ifelse(
    year == control_min_year & control_min_year > target_min_year, 1, 0))

# ---- outcome 4: deforestation/arson fines (:211, :339-349) -------------------
multas <- multas %>% filter(year >= 2005, year <= 2025)
defor_keywords <- c("desmatar", "desmatamento", "destruir.*florest", "cortar árvore",
                    "queimar", "queimada", "fogo", "incêndio")
multas$defor_arson <- ifelse(
  str_detect(tolower(multas$DES_INFRACAO), paste(defor_keywords, collapse = "|")),
  1, 0)

# ---- policy-jump regressions (:395-441) --------------------------------------
multas_final <- multas_final %>% mutate(policy = ifelse(min_year >= 2009, 1, 0))
multas_target_all <- multas_target_all %>%
  mutate(policy = ifelse(target_min_year >= 2009, 1, 0))
multas_control_all <- multas_control_all %>%
  mutate(policy = ifelse(target_min_year >= 2009, 1, 0))
multas <- multas %>% mutate(policy = ifelse(year >= 2009, 1, 0))

run_policy <- function(label, df, yvar) {
  fit <- fe_ols(df[[yvar]], cbind(policy = df$policy),
                df$COD_MUNICIPIO, df$COD_MUNICIPIO)
  data.table(Model = label, Coef = fit$coefs$beta, SE = fit$coefs$se,
             p = fit$coefs$p, NObs = fit$n_obs, NClusters = fit$n_clusters,
             pre_mean = mean(df[[yvar]][df$policy == 0], na.rm = TRUE))
}

results_table <- rbind(
  run_policy("Target first time", multas_final, "enters"),
  run_policy("Target first time | no control", multas_target_all, "enters_target_no_control"),
  run_policy("Control after target", multas_control_all, "enters_control_after_target"),
  run_policy("Deforestation/Arson", multas, "defor_arson")
)

cat("\n================= POLICY-JUMP TABLE (muni FE, cluster muni) =================\n")
print(as.data.frame(results_table), digits = 4)

# ---- year-FE event versions (ref 2008), coefficient paths to CSV -------------
run_yearfe <- function(label, df, yvar, tvar) {
  yrs <- setdiff(sort(unique(df[[tvar]])), 2008)
  X <- sapply(yrs, function(y) as.integer(df[[tvar]] == y))
  colnames(X) <- paste0("y", yrs)
  fit <- fe_ols(df[[yvar]], X, df$COD_MUNICIPIO, df$COD_MUNICIPIO)
  cbind(data.table(model = label), fit$coefs,
        data.table(n_obs = fit$n_obs, n_clusters = fit$n_clusters))
}

yearfe <- rbind(
  run_yearfe("Target first time", multas_final, "enters", "min_year"),
  run_yearfe("Target first time | no control", multas_target_all,
             "enters_target_no_control", "target_min_year"),
  run_yearfe("Control after target", multas_control_all,
             "enters_control_after_target", "target_min_year"),
  run_yearfe("Deforestation/Arson", multas, "defor_arson", "year")
)

cat("\n---- year-FE coefficient paths written (ref 2008); 2005-2014 excerpt ----\n")
print(as.data.frame(yearfe[term %in% paste0("y", 2005:2014)]), digits = 3)

fwrite(results_table, file.path(emp_dir, "policy_jump_table.csv"))
fwrite(yearfe, file.path(emp_dir, "policy_jump_yearfe_coefs.csv"))
cat("\nWrote: policy_jump_table.csv, policy_jump_yearfe_coefs.csv\n")
