library(readxl)
library(dplyr)
library(tidyr)
library(stringi)
library(readr)

# Load data
df_vnp_2002_2017 <- read_excel("../../data/input/landvalues/vnp/vnp_2002_2017.xlsx")
df_lavoura_fnp <- read_excel("../../data/input/landvalues/vnp/Lavoura_FNP.xlsx", skip = 3, n_max = 134)

# Cleaning helpers
clean_names <- function(x) {
  x <- tolower(x)
  x <- stri_trans_general(x, "latin-ascii")
  return(x)
}

clean_data <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) return(clean_names(x))
    return(x)
  })
  return(df)
}

# Clean column names and values
colnames(df_vnp_2002_2017) <- clean_names(colnames(df_vnp_2002_2017))
colnames(df_lavoura_fnp) <- clean_names(colnames(df_lavoura_fnp))

df_vnp_2002_2017 <- clean_data(df_vnp_2002_2017)
df_lavoura_fnp <- clean_data(df_lavoura_fnp)

# Rename for consistency
df_lavoura_fnp <- df_lavoura_fnp %>%
  rename(nome = 'regiao', land_type = 'tipo de terra')

df_vnp_2002_2017 <- df_vnp_2002_2017 %>%
  rename_with(~ gsub("^price_", "", .x), starts_with("price_"))

# Year columns
year_columns <- as.character(2002:2017)

# Remove rows with "-"
df_vnp_2002_2017 <- df_vnp_2002_2017 %>%
  filter(!apply(select(., all_of(year_columns)), 1, function(x) any(x == "-")))

df_lavoura_fnp <- df_lavoura_fnp %>%
  filter(!apply(select(., all_of(year_columns)), 1, function(x) any(x == "-")))

# Ensure numeric prices
df_vnp_2002_2017[year_columns] <- lapply(df_vnp_2002_2017[year_columns], as.numeric)
df_lavoura_fnp[year_columns] <- lapply(df_lavoura_fnp[year_columns], as.numeric)

# Merge on nome and land_type
merged_df <- df_lavoura_fnp %>%
  select(nome, land_type, all_of(year_columns)) %>%
  inner_join(df_vnp_2002_2017 %>%
               select(nome, land_type, all_of(year_columns)),
             by = c("nome", "land_type"),
             suffix = c(".fnp", ".vnp"))

# Compare year-by-year prices and flag differences, then move flag column to front
merged_df <- merged_df %>%
  rowwise() %>%
  mutate(price_match = ifelse(all(c_across(all_of(paste0(year_columns, ".fnp"))) ==
                                    c_across(all_of(paste0(year_columns, ".vnp")))),
                              "same", "different")) %>%
  ungroup() %>%
  select(price_match, everything())

# Save to CSV
write_csv(merged_df, "./cleaned_data/clean_vnp_price_comparison.csv")
