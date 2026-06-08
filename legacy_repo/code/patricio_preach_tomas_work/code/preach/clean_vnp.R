library(readxl)
library(tidyr)
library(dplyr)
library(readr)
library(tidyverse)

#1 vnp/Land_Price_North_Brazil_FNP.xlsx
df_1 <- read_excel("../../data/input/landvalues/vnp/Land_Price_North_Brazil_FNP.xlsx", sheet = 1)
df_2 <- read_excel("../../data/input/landvalues/vnp/Land_Price_North_Brazil_FNP.xlsx", sheet = 2)

df_1$Yield <- NA
df_1$Detail <- NA

id_cols <- c(
  "Region FNP", "City - Region", "State",
  "Land Type - English", "Land Type - Portuguese", 
  "Reference", "Yield", "Detail"
)

df_1[] <- lapply(df_1, function(x) {
  if (is.character(x)) tolower(x) else x
})

df_2[] <- lapply(df_2, function(x) {
  if (is.character(x)) tolower(x) else x
})

df_1 <- df_1 %>%
  pivot_longer(
    cols = -all_of(id_cols),     
    names_to = "Period",         
    values_to = "Value"    
  )

df_2 <- df_2 %>%
  pivot_longer(
    cols = -all_of(id_cols),     
    names_to = "Period",         
    values_to = "Value"    
  )


df_long <- rbind(df_1, df_2)
df_long <- df_long[order(df_long$`Region FNP`), ]
df_long[] <- lapply(df_long, function(x) {
  if (is.character(x)) tolower(x) else x
})

write.csv(df_long, "./cleaned_data/vnp/cleaned_land_price_north_brazil_fnp.csv", row.names = FALSE)


#2 vnp/Lavoura_FNP.xlsx
df_raw <- read_excel("../../data/input/landvalues/vnp/Lavoura_FNP.xlsx")

df_clean <- df_raw[-1, ]

colnames(df_clean) <- as.character(df_clean[1, ])

df_clean <- df_clean[-1, ]

colnames(df_clean) <- make.names(colnames(df_clean), unique = TRUE)

head(df_clean)

df_long <- df_clean %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Period",
    values_to = "Value"
  ) %>%
  mutate(
    Period = gsub("X", "", Period),
    Period = as.integer(Period),
    Value = gsub("[^0-9,.]", "", Value),
    Value = gsub("\\.", "", Value),
    Value = gsub(",", ".", Value),
    Value = as.numeric(Value)
  )

head(df_long)
df_long[] <- lapply(df_long, function(x) {
  if (is.character(x)) tolower(x) else x
})
write.csv(df_long, "./cleaned_data/vnp/cleaned_lavoura_fnp.csv", row.names = FALSE)

#3 vnp_2002_2017.xlsx
df_raw <- read_excel("../../data/input/landvalues/vnp/vnp_2002_2017.xlsx")
df_long <- df_raw %>%
  pivot_longer(
    cols = starts_with("price_"),
    names_to = "Period",
    values_to = "Value"
  ) %>%
  mutate(
    Period = gsub("price_", "", Period),       # remove "price_" prefix
    Period = as.integer(Period),
    Value = gsub("[^0-9,.]", "", Value),       # clean up currency-style text
    Value = gsub("\\.", "", Value),
    Value = gsub(",", ".", Value),
    Value = as.numeric(Value)
  )

df_long[] <- lapply(df_long, function(x) {
  if (is.character(x)) tolower(x) else x
})

colnames(df_long) <- c("Where", "State", "Number", "Nome", "Land_Type", "Period", "Value")

write.csv(df_long, "./cleaned_data/vnp/cleaned_vnp_2002_2017.csv", row.names = FALSE)
