library(readxl)
library(dplyr)
library(tidyr)
library(stringi)

df_vnp_2002_2017 <- read_excel("../../data/input/landvalues/vnp/vnp_2002_2017.xlsx")
df_lavoura_fnp <- read_excel("../../data/input/landvalues/vnp/Lavoura_FNP.xlsx", skip = 3, n_max = 134)

# Function to remove accents and convert to lowercase
clean_names <- function(x) {
  x <- tolower(x)  # Convert to lowercase
  x <- stri_trans_general(x, "latin-ascii")  # Remove accents
  return(x)
}

# Function to clean all character columns in a dataframe
clean_data <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      return(clean_names(x))
    }
    return(x)
  })
  return(df)
}

# Clean column names by applying the clean_names function
colnames(df_vnp_2002_2017) <- clean_names(colnames(df_vnp_2002_2017))
colnames(df_lavoura_fnp) <- clean_names(colnames(df_lavoura_fnp))

# Clean the actual data values in both datasets (apply clean_names to all character columns)
df_vnp_2002_2017 <- clean_data(df_vnp_2002_2017)
df_lavoura_fnp <- clean_data(df_lavoura_fnp)

# Rename columns in df_lavoura_fnp to match df_vnp_2002_2017
df_lavoura_fnp <- df_lavoura_fnp %>%
  rename(nome = 'regiao', land_type = 'tipo de terra')

# Clean column names in df_vnp (only contain year and no price in front)
df_vnp_2002_2017 <- df_vnp_2002_2017 %>%
  rename_with(~ gsub("^price_", "", .x), starts_with("price_"))

# Create a vector of year columns
year_columns <- as.character(2002:2017)

# Filter out rows with dashes 
df_vnp_2002_2017 <- df_vnp_2002_2017 %>%
  filter(!apply(select(., all_of(year_columns)), 1, function(x) any(x == "-")))

df_lavoura_fnp <- df_lavoura_fnp %>%
  filter(!apply(select(., all_of(year_columns)), 1, function(x) any(x == "-")))

# Merge by year columns (2002, 2003, ..., 2017)
merged_df <- df_lavoura_fnp %>%
  select(nome, land_type, all_of(year_columns)) %>%
  full_join(df_vnp_2002_2017 %>%
               select(nome, land_type, all_of(year_columns)), 
             by = year_columns, suffix = c(".lavoura_fnp", ".vnp_2002_2017"))

# Move all 'nome' and 'land_type' to the front
merged_df <- merged_df %>%
  select(nome.vnp_2002_2017, land_type.vnp_2002_2017, everything())

print(merged_df)

# Save the cleaned data to a CSV file
write_csv(merged_df, "./cleaned_data/vnp_match_by_price.csv")
