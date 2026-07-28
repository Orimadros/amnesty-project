##########################################
# filename: create_SicarMuniOverlap_variables.R
# author: Thiago Alckmin Oct/2023
# description: This script identifies overlapping properties in the SICAR database 
##########################################

# Objective 1: create a variable at the municipal level of the area of 'Imoveis Rurais' that overlaps with 'Unidades de Conservacao' 'Terras Indigenas' and federal land. Technical term is "Sobre-posições de terras"

# Objective 2: create a variable at the municipal level of whether (or number of times) that there is an environmental issue "irregularidade ambiental"

# Objective 3: create a variable at the municipal level using Cartório data of the number or area of illegal

# dados GTA: https://dados.gov.br/dataset/gta-guia-de-transito-animal

# section 0: set-up ------

# clear genv 
rm(list=ls()); gc()

# section 0.1 load libraries ------
library(data.table)
library(magrittr)
library(dplyr)
library(sf)
library(st)
library(stringi)
library(logr)
library(geobr)
library(classInt)
library(ggplot2)
library(xtable)

# dir_wd <- getwd() # make sure to load the project amazonLandPrices_project.Rproj
dir_wd <- "/data/research/Thiago/Amazon/"
setwd(dir_wd)
dir_data <- paste0(dir_wd, "/data/")
dir_main <- paste0(dir_wd, "code/prep/create_muni_year_intersections/")


#section 0.2 custom pre-loaded functions -----
setwd(dir_main)
source("helper_functions.R")

# set up file parameters -----

fp <- list()
fp$CHECKS <- FALSE
fp$SUMSTATS <- FALSE
fp$EXAMPLE <- FALSE
fp$RUNNING_TESTS <- FALSE

# SECTION A
fp$SECTION_A <- TRUE
fp$QUICK_FIX <- FALSE
fp$LOAD_DATA <- TRUE

# SECTION B
fp$SECTION_B <- FALSE
fp$CARDIDS_AND_YEARS <- FALSE ## NO NEED TO RUN AGAIN
fp$INTERSECT_SFS <- FALSE
fp$INTERSECT_SFS_fixing_issues <- FALSE
fp$CONSOLIDATE_VARIABLES <- FALSE
fp$INTERSECT_SFS_S2_OR_VALIDATED <- FALSE
fp$CONSOLIDATE_VARIABLES_S2_OR_VALIDATED <- TRUE


# SECTION C
fp$SECTION_C <- FALSE
fp$CLEAN_CARS <- FALSE
fp$CAR_OVERLAP <- FALSE
fp$INTERSECT_CARS_ROBUST <- FALSE
fp$INTERSECT_CARS_S2 <- FALSE
fp$CAR_OVERLAP_FINAL <- FALSE

# SECTION D
fp$SECTION_D <- TRUE
fp$OUTPUT_DATA <- FALSE
fp$GENERATE_DOCUMENTATION <- TRUE

# Section A: prepare intial data -------------
setwd(dir_main)
if(fp$SECTION_A){
  message_with_lines("RUNNING SECTION A")
  source("A_prepare_initial_data.R")
}

# Section B: create municipal melted (union) CAR intersections with sensitive land -----
setwd(dir_main)
if(fp$SECTION_B){
  message_with_lines("RUNNING SECTION B")
  source("B_intersect_car_union_sensitive_land.R")
}

# Section C: Intersect Individual CARs and compute areas -----------------
setwd(dir_main)
if(fp$SECTION_C){
  message_with_lines("RUNNING SECTION C")
  source("C_intersect_individual_cars.R")
}

# Section D: Consolidate ---------------------
setwd(dir_main)
if(fp$SECTION_D){
  message_with_lines("RUNNING SECTION D")
  source("D_consolidate.R")
}








