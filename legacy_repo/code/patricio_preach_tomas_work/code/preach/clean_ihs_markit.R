library(readxl)
library(dplyr)
library(janitor)
library(tidyr)
library(stringi)
library(tidyverse)

# Define the file path for the data
file_path <- "../../data/input/landvalues/ihs_markit/IHS Markit S&P Jun23.xlsx"

# Read IBGE & Municipality data from sheet "T14" and skip 5 rows
table_14 <- read_excel(file_path, sheet = "T14", skip = 5)

# Read from sheet "T13", starting at row 10 (i.e., skip 9 rows)
table_13 <- read_excel(file_path, sheet = "T13", skip = 9, n_max = 1050)

# Remove the topmost row (first row of the dataframe) in table_13
table_13 <- table_13 %>% slice(-1)

# Convert all values to lowercase and remove accents in both tables
table_13_normalized <- table_13 %>%
  mutate(across(everything(), ~ stri_trans_general(tolower(as.character(.)), "Latin-ASCII")))

table_14_normalized <- table_14 %>%
  mutate(across(everything(), ~ stri_trans_general(tolower(as.character(.)), "Latin-ASCII")))

# Clean and rename columns in table_13
colnames(table_13_normalized) <- c(
  "Id", 
  "Id Região", 
  "Região", 
  "UF", 
  "Município", 
  "Grupo", 
  "Subgrupo", 
  "Uso", 
  "Cap. Prod.", 
  "Detalhamento", 
  "Unidade", 
  "Bioma", 
  "Média 2020 (BRL)", 
  "Média 2022 (BRL)", 
  "Média Abr/Jun 23 (BRL)", 
  "CAGR (BRL)", 
  "Média 2020 (USD)", 
  "Média 2022 (USD)", 
  "Média Abr/Jun 23 (USD)", 
  "CAGR (USD)"
)

# Ensure the relevant columns are numeric in table_13
table_13_normalized <- table_13_normalized %>%
  mutate(across(
    c(
      "Média 2020 (BRL)", "Média 2022 (BRL)", "Média Abr/Jun 23 (BRL)",
      "Média 2020 (USD)", "Média 2022 (USD)", "Média Abr/Jun 23 (USD)"
    ),
    ~ as.numeric(.)
  ))

# Pivot the monetary columns to long format
table_13_long <- table_13_normalized %>%
  pivot_longer(
    cols = c(
      "Média 2020 (BRL)", "Média 2022 (BRL)", "Média Abr/Jun 23 (BRL)",
      "Média 2020 (USD)", "Média 2022 (USD)", "Média Abr/Jun 23 (USD)"
    ),
    names_to = c("Period", "Currency"),
    names_pattern = "Média (.*) \\((.*)\\)",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = Currency,
    values_from = Value,
    names_prefix = "Value_"
  )

# Clean and normalize table_14
table_14_clean <- table_14_normalized %>%
  rename(
    Cod_Mun_IBGE = `Cód. Mun. IBGE`,
    Municipio = `Município`,
    Regiao = `Região`
  )

# Clean table_13_long column names
table_13_long_clean <- table_13_long %>%
  rename(
    Municipio = `Município`,
    Regiao = `Região`
  )

# Merge table_13_long_clean and table_14_clean by matching Municipio, UF, and Regiao
table_13_long_merged <- table_13_long_clean %>%
  left_join(
    table_14_clean %>% select(Cod_Mun_IBGE, Municipio, UF, Regiao),
    by = c("Municipio", "UF", "Regiao")
  ) %>%
  relocate(Cod_Mun_IBGE, .before = everything())

# Save the merged table to an Excel file
write_csv(table_13_long_merged, "cleaned_data/cleaned_ihs_markit.csv")
