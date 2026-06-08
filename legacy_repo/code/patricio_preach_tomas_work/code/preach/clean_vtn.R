library(readxl)
library(dplyr)
library(janitor)
library(tidyr)
library(tidyverse)

# Pick only data from source 1 or Municipios

# Load the data for all years
df_vtn_2015 <- readRDS("../../data/input/landvalues/vtn/vtn_2015.rds")
df_vtn_2016 <- readRDS("../../data/input/landvalues/vtn/vtn_2016.rds")
df_vtn_2017 <- readRDS("../../data/input/landvalues/vtn/vtn_2017.rds")
df_vtn_2018 <- readRDS("../../data/input/landvalues/vtn/vtn_2018.rds")
df_vtn_2019 <- readRDS("../../data/input/landvalues/vtn/vtn_2019.rds")
df_vtn_2021 <- readRDS("../../data/input/landvalues/vtn/vtn_2021.rds")
df_vtn_2022 <- readRDS("../../data/input/landvalues/vtn/vtn_2022.rds")

# Combine all years into one data frame
vtn_list <- list(
  df_vtn_2015 = 2015,
  df_vtn_2016 = 2016,
  df_vtn_2017 = 2017,
  df_vtn_2018 = 2018,
  df_vtn_2019 = 2019,
  df_vtn_2021 = 2021,
  df_vtn_2022 = 2022
)

df_vtn_all <- bind_rows(
  lapply(names(vtn_list), function(name) {
    df <- get(name)
    return(df)
  })
)

# Rename 'year' column to 'Period'
df_vtn_all <- df_vtn_all %>%
  rename(Period = year)

# Filter data for 2015-2018 and keep only receita columns
df_vtn_2015_2018 <- df_vtn_all %>%
  filter(Period >= 2015 & Period <= 2018) %>%
  select(state, mun_code, mun_name, Period, starts_with("receita")) %>%
  rename_with(~ gsub("^receita_", "", .x), starts_with("receita"))

# Filter data for 2021-2022 with source == 1
df_vtn_2021_2022 <- df_vtn_all %>%
  filter(Period >= 2021 & Period <= 2022 & source == 1)

# Combine both datasets
df_vtn_combined <- bind_rows(df_vtn_2015_2018, df_vtn_2021_2022)

# Remove all columns starting with "receita_"
df_vtn_combined <- df_vtn_combined %>%
  select(-starts_with("receita_"))

# remove unwanted columns like "source" and "unico" and columns that start with mapa
df_vtn_combined <- df_vtn_combined %>%
  select(-starts_with("receita_"), -starts_with("mapa_"), -any_of(c("source", "unico", "mun_code"))) %>%
  mutate(across(where(is.character), tolower))

df_vtn_combined <- df_vtn_combined %>%
  arrange(state, mun_name, Period)

# Export cleaned dataset
write_csv(df_vtn_combined, "./cleaned_data/cleaned_vtn.csv")
