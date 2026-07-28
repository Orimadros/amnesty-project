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

#section 0.2 custom pre-loaded functions -----
source("https://raw.githubusercontent.com/Thiago-Alckmin/helper_functions_scripts/main/helper_functions_simple.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/Thiago-Alckmin/helper_functions_scripts/main/helper_functions_brazil.R", encoding = "UTF-8")

# section 0.3: code-specfic functions -----

# section 0.3.1: create workhorse function to clean sf objects -----
# clean shape by making it valid, removing any empty or invalid geometries  (INTENTIONALLY DOES NOT DROP DUPLICATES)

clean_shape_basic <- function(sf_obj){
  
  sf_obj %<>% 
    # make the invalid polygons valid
    st_make_valid() %>%
    .[!duplicated(.$geometry),] %>% 
    mutate( empty = st_is_empty(geometry)) %>% 
    mutate( valid = st_is_valid(geometry)) 
  
  # drop empty geometreis
  sf_obj %<>% 
    .[which(sf_obj$empty==FALSE), ]

  return(sf_obj)
  
}


clean_shape <- function(sf_obj){
  
  sf_obj %<>% 
    # make the invalid polygons valid
    st_make_valid() %>%
    .[!duplicated(.$geometry),] %>% 
    mutate( empty = st_is_empty(geometry)) %>% 
    mutate( valid = st_is_valid(geometry)) 
  
  # drop empty geometreis
  sf_obj %<>% 
    .[which(sf_obj$empty==FALSE), ]
  
  # drop invalid geometries 
  sf_obj %<>% 
    .[which(sf_obj$valid==TRUE), ]
  
  return(sf_obj)
  
}

# same as above but replace geometry for x
clean_shape_reenforced <- function(sf_obj){
  
  sf_obj %<>% 
    # make the invalid polygons valid
    st_make_valid(., geos_method = "valid_structure",
                  geos_keep_collapsed = FALSE) %>% 
    .[!duplicated(.$geometry),] %>% 
    mutate( empty = st_is_empty(geometry)) %>% 
    mutate( valid = st_is_valid(geometry)) 
  
  # drop empty geometreis
  sf_obj %<>% 
    .[which(sf_obj$empty==FALSE), ]
  
  # drop invalid geometries 
  sf_obj %<>% 
    .[which(sf_obj$valid==TRUE), ]
  
  return(sf_obj)
  
}

clean_shape_s2 <- function(car){
  
  info <- car %>%  st_drop_geometry()
  clean <- car %>%  st_as_s2() %>% st_as_sf() 
  
  out <- cbind(clean, info) %>% clean_shape_reenforced()
  
  
  
  out %>% return()
  
}

clean_shape_s2_NOT <- function(car){
  
  info <- car %>%  st_drop_geometry()
  clean <- car %>%  st_as_s2(FALSE) %>% st_as_sf() 
  
  out <- cbind(clean, info) %>% clean_shape_reenforced()
  
  
  
  out %>% return()
  
}



# same as above but replace geometry for x
clean_shape_lands_reenforced <- function(sf_obj){
  
  sf_obj %<>% 
    # make the invalid polygons valid
    st_make_valid(., geos_method = "valid_structure",
                  geos_keep_collapsed = FALSE) %>% 
    .[!duplicated(.$geometry),] %>% 
    mutate( empty = st_is_empty(geometry)) %>% 
    mutate( valid = st_is_valid(geometry)) 
  
  # drop empty geometreis
  sf_obj %<>% 
    .[which(sf_obj$empty==FALSE), ]
  
  # drop invalid geometries 
  # sf_obj %<>% 
  #   .[which(sf_obj$valid==TRUE), ]
  
  return(sf_obj)
  
}
clean_shape_lands_reenforced2 <- function(sf_obj){
  
  sf_obj %<>% 
    # make the invalid polygons valid
    st_make_valid(., geos_method = "valid_structure",
                  geos_keep_collapsed = FALSE) %>% 
    .[!duplicated(.$geometry),] %>% 
    mutate( empty = st_is_empty(geometry)) %>% 
    mutate( valid = st_is_valid(geometry)) 
  
  # drop empty geometreis
  sf_obj %<>% 
    .[which(sf_obj$empty==FALSE), ]
  
  # drop invalid geometries
  sf_obj %<>%
    .[which(sf_obj$valid==TRUE), ]
  
  return(sf_obj)
  
}

# same as above but replace geometry for x
clean_shape_reenforced_CARs <- function(sf_obj){
  
  sf_obj %<>% 
    st_as_sf() %>% 
    # make the invalid polygons valid
    st_make_valid(., geos_method = "valid_structure",
                  geos_keep_collapsed = FALSE) %>% 
    .[!duplicated(.$geometry),] %>% 
    mutate( empty = st_is_empty(geometry)) %>% 
    mutate( valid = st_is_valid(geometry)) 
  
  # drop empty geometreis
  sf_obj %<>% 
    .[which(sf_obj$empty==FALSE), ]
  
  # drop invalid geometries 
  sf_obj %<>% 
    .[which(sf_obj$valid==TRUE), ]
  
  return(sf_obj)
  
}

# clean transformed shape by making sf & dropping linestrings
clean_union <-function(sf_obj){
  
  sf_obj %>% 
    st_as_sf() %>% 
    st_make_valid(., geos_method = "valid_structure",
                  geos_keep_collapsed = FALSE) %>% 
    # # drop linestring
    # .[st_geometry_type(.) != "LINESTRING", ] %>% 
    return()
  
}

clean_union_reenforced <-function(sf_obj){
  
  sf_obj %>% 
    st_as_sf() %>% 
    st_make_valid(., 
                  geos_method = "valid_structure",
                  geos_keep_collapsed = FALSE) %>% 
    mutate(empty= st_is_empty(x)) %>% 
    .[.$empty!=TRUE, ] %>% 
    select(-c("empty")) %>%
    rename(geometry=x) %>% 
    # # drop linestring
    # .[st_geometry_type(.) != "LINESTRING", ] %>%
    return()
  
  
  # st_buffer(., dist = 0) %>% 
  # st_snap(.) %>% 
  # st_simplify(., dTolerance = 1, preserveTopology = TRUE) %>%
  # return()
  
}

drop_shape_duplicates <- function(sf_obj){
  
  sf_obj[!duplicated(sf_obj$geometry),] %>% return()
  
}

# define cleaning functions for municipal CAR varaibles -----

clean_cars_normal <- function(munis_and_years){
  i <- 1
  for(i in 1:nrow(munis_and_years)){
    
    # select municipality  
    THIS_muni <- munis_and_years[i, municipio]
    THIS_year <-munis_and_years[i, year] 
    
    property_directories_RUNBATCH <- property_directories %>% 
      .[stri_detect_fixed(., THIS_muni)] %>% 
      .[order(.)] %>% 
      .[!duplicated(.)] %>%
      c()
    
    
    if(length(property_directories_RUNBATCH)>1){
      message_with_lines("ADJUST CODE IF THIS HAPPENS")
      break
    }
    
    # get the file path to the shape file & municipality name 
    PROPERTY_SHP <- property_directories_RUNBATCH[1]
    PROPERTY_MUNI <- stri_extract_all_regex(PROPERTY_SHP, "\\d{7}") %>% unlist()
    PROPERTY_UF <- PROPERTY_MUNI %>% substr(., 1,2)
    
    # load forest data 
    forests <- load_forrest_data(PROPERTY_UF) %>% 
      mutate(typeA = startsWith(codigo, "FPA")*1) %>% 
      mutate(typeB = startsWith(codigo, "FPB")*1) %>% 
      mutate(typeC = startsWith(codigo, "FPC")*1)  %>% 
      clean_shape()
    
    # status update
    paste0("Fixing: Municipio ", PROPERTY_SHP, " in year ", THIS_year ) %>%  message_with_lines()
    paste0("Fixing ", i, " of ", nrow(munis_and_years), " municipalities left.") %>% message_with_lines()
    
    # 3.2: subset municipality of interest in the micro-dataset -----
    microdata_muni <- microdata %>%
      # relevant variables
      .[, .(
        registro_car,
        codigo_ibge,
        data_inscricao,
        situacao_cadastro
      )] %>% copy() %>% 
      # subset data-set
      .[codigo_ibge == PROPERTY_MUNI] 
    
    # 3.3: get an index of the cars that are present per year 
    car_years_muni <- car_and_reg_year_wide %>% copy() %>% 
      .[municipio == PROPERTY_MUNI] %>% 
      select(-c("municipio")) %>% 
      melt.data.table(id.vars = c("registro_car")) %>% 
      rename_columns(c("variable"),c("year")) %>% 
      .[, year := stri_replace_all_fixed(year, "y", "")]
    
    # 3.4: for each year -----------
    YEAR <- THIS_year
    
    paste0( PROPERTY_SHP, " -- ",  YEAR) %>% message_with_lines()
    
    # subset the cars for that year 
    CARS_THIS_YEAR <- car_years_muni %>% 
      .[year==YEAR & value==1] %>% 
      .[, registro_car]
    
    # if there are cars this year, then proceeds; otherwise skip year 
    if(length(CARS_THIS_YEAR)>0){
      
      # 1: load property data by municipio;  Change 4674 crs -----
      car <- PROPERTY_SHP  %>% 
        read_sf() %>% 
        st_transform(4674)  %>% 
        # subset to relevant properties 
        .[which(.$COD_IMOVEL %in% CARS_THIS_YEAR),]
      
      # 2: flag invalid polygons then convert them to valid ones 
      car$flag_valid <- st_is_valid(car) 
      
      # 3: make invalid polygons valid as well
      car %<>% clean_shape(sf_obj = .)
      
      # 4: create the union of the polygons 
      car_union <- car %>% 
        st_union() %>% 
        clean_union_reenforced()
      
      st_is_valid(car_union)
      
      # 5: compute the union area
      car_area <- car_union %>% 
        st_area()
      
      # 6: compute the intersection with indigenous land area
      car_area_intersect_indi <- car_union %>%
        # calculate intersection area
        st_intersection(indigenous) %>% 
        st_area() %>%
        sum()
      
      # 7: compute the intersection with conservation land area
      car_area_intersect_conserve <- car_union %>%
        # calculate intersection area
        st_intersection(conservation) %>% 
        st_area() %>%
        sum()
      
      # 8: compute the intersection with forested land area total
      # car_area_intersect_forest_total <-
      #   forests %>%
      #   # calculate intersection area
      #   st_intersection( car_union) %>% #plot(., col="red")
      #   st_area() %>%
      #   sum()
      
      # 8A: compute the intersection with forested land area
      car_area_intersect_forestA <- 
        forests[forests$typeA==1, ] %>% 
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestA %<>% 
        st_make_valid() %>% #plot(., col="red")
        st_area() %>%
        sum()
      
      # 8b: compute the intersection with forested land area
      car_area_intersect_forestB <- 
        forests[forests$typeB==1, ] %>% 
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestB %<>% 
        st_make_valid() %>% #plot(., col="red")
        st_area() %>%
        sum()
      
      # 8c: compute the intersection with forested land area
      car_area_intersect_forestC <- 
        forests[forests$typeC==1, ] %>% 
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestC %<>% 
        st_make_valid() %>% #plot(., col="red")
        st_area() %>%
        sum()
      
      
      # 9.1: identify Cancelled CARs -----
      car_cancelled <- car[which(car$SITUACAO == "CA"), ] %>% 
        mutate( empty = st_is_empty(geometry)) %>% 
        mutate( valid = st_is_valid(geometry)) 
      
      # drop empty geometreis
      car_cancelled %<>% 
        .[which(car_cancelled$empty==FALSE), ]
      
      # drop invalid geometries 
      car_cancelled %<>% 
        .[which(car_cancelled$valid==TRUE), ]
      
      car_cancelled  %<>%
        st_union() %>% 
        st_make_valid()   
      
      car_area_cancelled <- car_cancelled %>% 
        st_area()
      
      # 9.2: identify not Cancelled CARs -----
      car_notcancelled <- car[which(car$SITUACAO != "CA"), ] %>% 
        mutate( empty = st_is_empty(geometry)) %>% 
        mutate( valid = st_is_valid(geometry)) 
      
      # drop empty geometreis
      car_notcancelled %<>% 
        .[which(car_notcancelled$empty==FALSE), ]
      
      # drop invalid geometries 
      car_notcancelled %<>% 
        .[which(car_notcancelled$valid==TRUE), ]
      
      car_notcancelled  %<>%
        st_union() %>% 
        st_make_valid()   
      
      car_area_notcancelled <- car_notcancelled %>% 
        st_area()
      
      car_area_intersections <- st_intersection(car_cancelled, car_notcancelled) %>% 
        st_make_valid() %>% 
        st_area() %>% 
        sum()
      
      # for each cancelled CAR, what share of its land was cancelled ------
      
      # cancelled_cars <- car[which(car$SITUACAO == "CA"),  ] %>% 
      #   st_make_valid() %>% 
      #   mutate( car_area = st_area(geometry))
      
      data.table(
        car_union_area = car_area,
        car_area_intersect_indi = car_area_intersect_indi,
        car_area_intersect_conserve = car_area_intersect_conserve,
        # car_area_intersect_forest_total = car_area_intersect_forest_total,
        car_area_intersect_forestA = car_area_intersect_forestA,
        car_area_intersect_forestB = car_area_intersect_forestB,
        car_area_intersect_forestC = car_area_intersect_forestC,
        car_area_cancelled = car_area_cancelled, 
        car_area_notcancelled = car_area_notcancelled,
        year = YEAR,
        municipio = PROPERTY_MUNI
      ) %>% 
        fwrite(paste0("data/processing/SicarMuniOverlap/muni", PROPERTY_MUNI, "_", YEAR, ".csv"))
      
      
      if(YEAR == 2023){
        sf::write_sf(car, paste0(dir_wd, "data/processing/CleanShps/muni", THIS_muni,  ".shp"))
      }
      
      
    }
    
  }
  
  gc()
  
  
}

clean_cars_robust <- function(munis_and_years){
  
  i <- 1
  for(i in 1:nrow(munis_and_years)){
    
    # select municipality  
    THIS_muni <- munis_and_years[i, municipio]
    THIS_year <-munis_and_years[i, year] 
    
    property_directories_RUNBATCH <- property_directories %>% 
      .[stri_detect_fixed(., THIS_muni)] %>% 
      .[order(.)] %>% 
      .[!duplicated(.)] %>%
      .[!stri_detect_fixed(., "AREA_IMOVEL_")] %>%
      c()
    
    if(length(property_directories_RUNBATCH)>1){
      message_with_lines("ADJUST CODE IF THIS HAPPENS")
      break
    }
    
    # get the file path to the shape file & municipality name 
    PROPERTY_SHP <- property_directories_RUNBATCH[1]
    PROPERTY_MUNI <- stri_extract_all_regex(PROPERTY_SHP, "\\d{7}") %>% unlist()
    PROPERTY_UF <- PROPERTY_MUNI %>% substr(., 1,2)
    
    # load forest data 
    forests <- load_forrest_data(PROPERTY_UF) %>% 
      mutate(typeA = startsWith(codigo, "FPA")*1) %>% 
      mutate(typeB = startsWith(codigo, "FPB")*1) %>% 
      mutate(typeC = startsWith(codigo, "FPC")*1)  %>% 
      clean_shape()
    
    # status update
    paste0("Fixing: Municipio ", PROPERTY_SHP, " in year ", THIS_year ) %>%  message_with_lines()
    paste0("Fixing ", i, " of ", nrow(munis_and_years), " municipalities left.") %>% message_with_lines()
    
    # 3.2: subset municipality of interest in the micro-dataset -----
    microdata_muni <- microdata %>%
      # relevant variables
      .[, .(
        registro_car,
        codigo_ibge,
        data_inscricao,
        situacao_cadastro
      )] %>% copy() %>% 
      # subset data-set
      .[codigo_ibge == PROPERTY_MUNI] 
    
    # 3.3: get an index of the cars that are present per year 
    car_years_muni <- car_and_reg_year_wide %>% copy() %>% 
      .[municipio == PROPERTY_MUNI] %>% 
      select(-c("municipio")) %>% 
      melt.data.table(id.vars = c("registro_car")) %>% 
      rename_columns(c("variable"),c("year")) %>% 
      .[, year := stri_replace_all_fixed(year, "y", "")]
    
    # 3.4: for each year -----------
    YEAR <- THIS_year
    
    paste0( PROPERTY_SHP, " -- ",  YEAR) %>% message_with_lines()
    
    # subset the cars for that year 
    CARS_THIS_YEAR <- car_years_muni %>% 
      .[year==YEAR & value==1] %>% 
      .[, registro_car]
    
    # if there are cars this year, then proceeds; otherwise skip year 
    if(length(CARS_THIS_YEAR)>0){
      
      # 1: load property data by municipio;  Change 4674 crs -----
      car <- PROPERTY_SHP  %>% 
        read_sf() %>% 
        st_transform(4674)  %>% 
        # subset to relevant properties 
        .[which(.$COD_IMOVEL %in% CARS_THIS_YEAR),]
      
      # 2: flag invalid polygons then convert them to valid ones 
      car$flag_valid <- st_is_valid(car) 
      
      # 3: make invalid polygons valid as well
      car %<>% clean_shape(sf_obj = .)
      
      # 4: create the union of the polygons 
      car_union <- car %>% 
        st_union() %>% 
        clean_union_reenforced()
      
      st_is_valid(car_union)
      
      # 5: compute the union area
      car_area <- car_union %>% 
        st_area()
      
      # 6: compute the intersection with indigenous land area
      car_area_intersect_indi <- car_union %>%
        # calculate intersection area
        st_intersection(indigenous_clean) %>% 
        clean_shape_lands_reenforced()    %>% 
        st_area() %>%
        sum()
      
      # 7: compute the intersection with conservation land area
      car_area_intersect_conserve <- car_union %>%
        # calculate intersection area
        st_intersection(conservation_clean) %>% 
        clean_shape_lands_reenforced()    %>% 
        st_area() %>%
        sum()
      
      # 8A: compute the intersection with forested land area
      car_area_intersect_forestA <- 
        forests[forests$typeA==1, ] %>% 
        clean_shape_lands_reenforced()    %>% 
        # calculate intersection area
        st_intersection( car_union)  
      
      car_area_intersect_forestA %<>% 
        clean_shape_lands_reenforced() %>% 
        st_area() %>%
        sum()
      
      # 8b: compute the intersection with forested land area
      car_area_intersect_forestB <- 
        forests[forests$typeB==1, ] %>% 
        clean_shape_lands_reenforced() %>% 
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestB %<>% 
        clean_shape_lands_reenforced() %>% 
        st_area() %>%
        sum()
      
      # 8c: compute the intersection with forested land area
      car_area_intersect_forestC <- 
        forests[forests$typeC==1, ] %>% 
        clean_shape_lands_reenforced() %>%
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestC %<>% 
        clean_shape_lands_reenforced() %>%
        st_area() %>%
        sum()
      
      
      # 9.1: identify Cancelled CARs -----
      car_cancelled <- car[which(car$SITUACAO == "CA"), ] %>% clean_shape_reenforced()
      
      car_cancelled  %<>%
        st_union() %>% 
        st_make_valid()   
      
      car_area_cancelled <- car_cancelled %>% 
        st_area()
      
      # 9.2: identify not Cancelled CARs -----
      car_notcancelled <- car[which(car$SITUACAO != "CA"), ] %>% clean_shape_reenforced()
      
      car_notcancelled  %<>%
        st_union() %>% 
        st_make_valid()   
      
      car_area_notcancelled <- car_notcancelled %>% 
        st_area()
      
      # car_area_intersections <- st_intersection(car_cancelled, car_notcancelled) %>% 
      #   st_make_valid() %>% 
      #   st_area() %>% 
      #   sum()
      
      # for each cancelled CAR, what share of its land was cancelled ------
      
      data.table(
        car_union_area = car_area,
        car_area_intersect_indi = car_area_intersect_indi,
        car_area_intersect_conserve = car_area_intersect_conserve,
        car_area_intersect_forestA = car_area_intersect_forestA,
        car_area_intersect_forestB = car_area_intersect_forestB,
        car_area_intersect_forestC = car_area_intersect_forestC,
        car_area_cancelled = car_area_cancelled, 
        car_area_notcancelled = car_area_notcancelled,
        year = YEAR,
        municipio = PROPERTY_MUNI
      ) %>% 
        fwrite(paste0("data/processing/SicarMuniOverlap/muni", PROPERTY_MUNI, "_", YEAR, ".csv"))
      
      rm(car_area)
      rm(car_area_intersect_indi)
      rm(car_area_intersect_conserve)
      rm(car_area_intersect_forestA)
      rm(car_area_intersect_forestB)
      rm(car_area_intersect_forestC)
      
      
      if(YEAR == 2022){
        sf::write_sf(car, paste0(dir_wd, "data/processing/CleanShps/muni", THIS_muni,  ".shp"))
      }
      
      
    }
    
  }
  
  gc()
  
}

clean_cars_robust_alternate <- function(munis_and_years){
  i <- 1
  for(i in 1:nrow(munis_and_years)){
    
    # select municipality  
    THIS_muni <- munis_and_years[i, municipio]
    THIS_year <-munis_and_years[i, year] 
    
    property_directories_RUNBATCH <- property_directories %>% 
      .[stri_detect_fixed(., THIS_muni)] %>% 
      .[order(.)] %>% 
      .[!duplicated(.)] %>%
      .[!stri_detect_fixed(., "AREA_IMOVEL_")] %>%
      c()
    
    if(length(property_directories_RUNBATCH)>1){
      message_with_lines("ADJUST CODE IF THIS HAPPENS")
      break
    }
    
    # get the file path to the shape file & municipality name 
    PROPERTY_SHP <- property_directories_RUNBATCH[1]
    PROPERTY_MUNI <- stri_extract_all_regex(PROPERTY_SHP, "\\d{7}") %>% unlist()
    PROPERTY_UF <- PROPERTY_MUNI %>% substr(., 1,2)
    
    # load forest data 
    forests <- load_forrest_data(PROPERTY_UF) %>% 
      mutate(typeA = startsWith(codigo, "FPA")*1) %>% 
      mutate(typeB = startsWith(codigo, "FPB")*1) %>% 
      mutate(typeC = startsWith(codigo, "FPC")*1)  %>% 
      clean_shape()
    
    # status update
    paste0("Fixing: Municipio ", PROPERTY_SHP, " in year ", THIS_year ) %>%  message_with_lines()
    paste0("Fixing ", i, " of ", nrow(munis_and_years), " municipalities left.") %>% message_with_lines()
    
    # 3.2: subset municipality of interest in the micro-dataset -----
    microdata_muni <- microdata %>%
      # relevant variables
      .[, .(
        registro_car,
        codigo_ibge,
        data_inscricao,
        situacao_cadastro
      )] %>% copy() %>% 
      # subset data-set
      .[codigo_ibge == PROPERTY_MUNI] 
    
    # 3.3: get an index of the cars that are present per year 
    car_years_muni <- car_and_reg_year_wide %>% copy() %>% 
      .[municipio == PROPERTY_MUNI] %>% 
      select(-c("municipio")) %>% 
      melt.data.table(id.vars = c("registro_car")) %>% 
      rename_columns(c("variable"),c("year")) %>% 
      .[, year := stri_replace_all_fixed(year, "y", "")]
    
    # 3.4: for each year -----------
    YEAR <- THIS_year
    
    paste0( PROPERTY_SHP, " -- ",  YEAR) %>% message_with_lines()
    
    # subset the cars for that year 
    CARS_THIS_YEAR <- car_years_muni %>% 
      .[year==YEAR & value==1] %>% 
      .[, registro_car]
    
    # if there are cars this year, then proceeds; otherwise skip year 
    if(length(CARS_THIS_YEAR)>0){
      
      # 1: load property data by municipio;  Change 4674 crs -----
      car <- PROPERTY_SHP  %>% 
        read_sf() %>% 
        st_transform(4674)  %>% 
        # subset to relevant properties 
        .[which(.$COD_IMOVEL %in% CARS_THIS_YEAR),]
      
      # 2: flag invalid polygons then convert them to valid ones 
      car$flag_valid <- st_is_valid(car) 
      
      # 3: make invalid polygons valid as well
      car %<>% clean_shape(sf_obj = .)
      
      # 4: create the union of the polygons 
      car_union <- car %>% 
        st_union() %>% 
        clean_union_reenforced()
      
      st_is_valid(car_union)
      
      # 5: compute the union area
      car_area <- car_union %>% 
        st_area()
      
      # 6: compute the intersection with indigenous land area
      car_area_intersect_indi <- car_union %>%
        # calculate intersection area
        st_intersection(indigenous_clean) %>% 
        clean_shape_lands_reenforced()    %>% 
        st_area() %>%
        sum()
      
      # 7: compute the intersection with conservation land area
      car_area_intersect_conserve <- car_union %>%
        # calculate intersection area
        st_intersection(conservation_clean) %>% 
        clean_shape_lands_reenforced()    %>% 
        st_area() %>%
        sum()
      
      # 8A: compute the intersection with forested land area
      car_area_intersect_forestA <- 
        forests[forests$typeA==1, ] %>% 
        clean_shape_lands_reenforced()    %>% 
        # calculate intersection area
        st_intersection( car_union)  
      
      car_area_intersect_forestA %<>% 
        st_area() %>%
        sum()
      
      # 8b: compute the intersection with forested land area
      car_area_intersect_forestB <- 
        forests[forests$typeB==1, ] %>% 
        clean_shape_lands_reenforced() %>% 
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestB %<>% 
        st_area() %>%
        sum()
      
      # 8c: compute the intersection with forested land area
      car_area_intersect_forestC <- 
        forests[forests$typeC==1, ] %>% 
        clean_shape_lands_reenforced() %>%
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestC %<>% 
        clean_shape_lands_reenforced() %>%
        st_area() %>%
        sum()
      
      
      # 9.1: identify Cancelled CARs -----
      car_cancelled <- car[which(car$SITUACAO == "CA"), ] %>% clean_shape_reenforced()
      
      car_cancelled  %<>%
        st_union() %>% 
        st_make_valid()   
      
      car_area_cancelled <- car_cancelled %>% 
        st_area()
      
      # 9.2: identify not Cancelled CARs -----
      car_notcancelled <- car[which(car$SITUACAO != "CA"), ] %>% clean_shape_reenforced()
      
      car_notcancelled  %<>%
        st_union() %>% 
        st_make_valid()   
      
      car_area_notcancelled <- car_notcancelled %>% 
        st_area()
      
      # car_area_intersections <- st_intersection(car_cancelled, car_notcancelled) %>% 
      #   st_make_valid() %>% 
      #   st_area() %>% 
      #   sum()
      
      # for each cancelled CAR, what share of its land was cancelled ------
      
      data.table(
        car_union_area = car_area,
        car_area_intersect_indi = car_area_intersect_indi,
        car_area_intersect_conserve = car_area_intersect_conserve,
        car_area_intersect_forestA = car_area_intersect_forestA,
        car_area_intersect_forestB = car_area_intersect_forestB,
        car_area_intersect_forestC = car_area_intersect_forestC,
        car_area_cancelled = car_area_cancelled, 
        car_area_notcancelled = car_area_notcancelled,
        year = YEAR,
        municipio = PROPERTY_MUNI
      ) %>% 
        fwrite(paste0("data/processing/SicarMuniOverlap/muni", PROPERTY_MUNI, "_", YEAR, ".csv"))
      
      rm(car_area)
      rm(car_area_intersect_indi)
      rm(car_area_intersect_conserve)
      rm(car_area_intersect_forestA)
      rm(car_area_intersect_forestB)
      rm(car_area_intersect_forestC)
      
      
      if(YEAR == 2022){
        sf::write_sf(car, paste0(dir_wd, "data/processing/CleanShps/muni", THIS_muni,  ".shp"))
      }
      
      
    }
    
  }
  
  gc()
  
  
}

clean_cars_robust_alternate2 <-  function(munis_and_years){
  i <- 1
  for(i in 1:nrow(munis_and_years)){
    
    # select municipality  
    THIS_muni <- munis_and_years[i, municipio]
    THIS_year <-munis_and_years[i, year] 
    
    property_directories_RUNBATCH <- property_directories %>% 
      .[stri_detect_fixed(., THIS_muni)] %>% 
      .[order(.)] %>% 
      .[!duplicated(.)] %>%
      .[!stri_detect_fixed(., "AREA_IMOVEL_")] %>%
      c()
    
    if(length(property_directories_RUNBATCH)>1){
      message_with_lines("ADJUST CODE IF THIS HAPPENS")
      break
    }
    
    # get the file path to the shape file & municipality name 
    PROPERTY_SHP <- property_directories_RUNBATCH[1]
    PROPERTY_MUNI <- stri_extract_all_regex(PROPERTY_SHP, "\\d{7}") %>% unlist()
    PROPERTY_UF <- PROPERTY_MUNI %>% substr(., 1,2)
    
    # load forest data 
    forests <- load_forrest_data(PROPERTY_UF) %>% 
      mutate(typeA = startsWith(codigo, "FPA")*1) %>% 
      mutate(typeB = startsWith(codigo, "FPB")*1) %>% 
      mutate(typeC = startsWith(codigo, "FPC")*1)  %>% 
      clean_shape()
    
    # status update
    paste0("Fixing: Municipio ", PROPERTY_SHP, " in year ", THIS_year ) %>%  message_with_lines()
    paste0("Fixing ", i, " of ", nrow(munis_and_years), " municipalities left.") %>% message_with_lines()
    
    # 3.2: subset municipality of interest in the micro-dataset -----
    microdata_muni <- microdata %>%
      # relevant variables
      .[, .(
        registro_car,
        codigo_ibge,
        data_inscricao,
        situacao_cadastro
      )] %>% copy() %>% 
      # subset data-set
      .[codigo_ibge == PROPERTY_MUNI] 
    
    # 3.3: get an index of the cars that are present per year 
    car_years_muni <- car_and_reg_year_wide %>% copy() %>% 
      .[municipio == PROPERTY_MUNI] %>% 
      select(-c("municipio")) %>% 
      melt.data.table(id.vars = c("registro_car")) %>% 
      rename_columns(c("variable"),c("year")) %>% 
      .[, year := stri_replace_all_fixed(year, "y", "")]
    
    # 3.4: for each year -----------
    YEAR <- THIS_year
    
    paste0( PROPERTY_SHP, " -- ",  YEAR) %>% message_with_lines()
    
    # subset the cars for that year 
    CARS_THIS_YEAR <- car_years_muni %>% 
      .[year==YEAR & value==1] %>% 
      .[, registro_car]
    
    # if there are cars this year, then proceeds; otherwise skip year 
    if(length(CARS_THIS_YEAR)>0){
      
      # 1: load property data by municipio;  Change 4674 crs -----
      car <- PROPERTY_SHP  %>% 
        read_sf() %>% 
        st_transform(4674)  %>% 
        # subset to relevant properties 
        .[which(.$COD_IMOVEL %in% CARS_THIS_YEAR),]
      
      # 2: flag invalid polygons then convert them to valid ones 
      car$flag_valid <- st_is_valid(car) 
      
      # 3: make invalid polygons valid as well
      car %<>% clean_shape(sf_obj = .)
      
      # 4: create the union of the polygons 
      car_union <- car %>% 
        st_union() %>% 
        clean_union_reenforced()
      
      st_is_valid(car_union)
      
      # 5: compute the union area
      car_area <- car_union %>% 
        st_area()
      
      # 6: compute the intersection with indigenous land area
      car_area_intersect_indi <- car_union %>%
        # calculate intersection area
        st_intersection(indigenous_clean) %>% 
        clean_shape_lands_reenforced()    %>% 
        st_area() %>%
        sum()
      
      # 7: compute the intersection with conservation land area
      car_area_intersect_conserve <- car_union %>%
        # calculate intersection area
        st_intersection(conservation_clean) %>% 
        clean_shape_lands_reenforced()    %>% 
        st_area() %>%
        sum()
      
      # 8A: compute the intersection with forested land area
      car_area_intersect_forestA <- 
        forests[forests$typeA==1, ] %>% 
        clean_shape_lands_reenforced()    %>% 
        # calculate intersection area
        st_intersection( car_union)  
      
      car_area_intersect_forestA %<>% 
        clean_shape_lands_reenforced()    %>% 
        st_area() %>%
        sum()
      
      # 8b: compute the intersection with forested land area
      car_area_intersect_forestB <- 
        forests[forests$typeB==1, ] %>% 
        clean_shape_lands_reenforced() %>% 
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestB %<>% 
        clean_shape_lands_reenforced()    %>%
        st_area() %>%
        sum()
      
      # 8c: compute the intersection with forested land area
      car_area_intersect_forestC <- 
        forests[forests$typeC==1, ] %>% 
        clean_shape_lands_reenforced() %>%
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestC %<>% 
        clean_shape_lands_reenforced() %>%
        st_area() %>%
        sum()
      
      
      # 9.1: identify Cancelled CARs -----
      car_cancelled <- car[which(car$SITUACAO == "CA"), ] %>% clean_shape_reenforced()
      
      car_cancelled  %<>%
        st_union() %>% 
        st_make_valid()   
      
      car_area_cancelled <- car_cancelled %>% 
        st_area()
      
      # 9.2: identify not Cancelled CARs -----
      car_notcancelled <- car[which(car$SITUACAO != "CA"), ] %>% clean_shape_reenforced()
      
      car_notcancelled  %<>%
        st_union() %>% 
        st_make_valid()   
      
      car_area_notcancelled <- car_notcancelled %>% 
        st_area()
      
      # car_area_intersections <- st_intersection(car_cancelled, car_notcancelled) %>% 
      #   st_make_valid() %>% 
      #   st_area() %>% 
      #   sum()
      
      # for each cancelled CAR, what share of its land was cancelled ------
      
      data.table(
        car_union_area = car_area,
        car_area_intersect_indi = car_area_intersect_indi,
        car_area_intersect_conserve = car_area_intersect_conserve,
        car_area_intersect_forestA = car_area_intersect_forestA,
        car_area_intersect_forestB = car_area_intersect_forestB,
        car_area_intersect_forestC = car_area_intersect_forestC,
        car_area_cancelled = car_area_cancelled, 
        car_area_notcancelled = car_area_notcancelled,
        year = YEAR,
        municipio = PROPERTY_MUNI
      ) %>% 
        fwrite(paste0("data/processing/SicarMuniOverlap/muni", PROPERTY_MUNI, "_", YEAR, ".csv"))
      
      rm(car_area)
      rm(car_area_intersect_indi)
      rm(car_area_intersect_conserve)
      rm(car_area_intersect_forestA)
      rm(car_area_intersect_forestB)
      rm(car_area_intersect_forestC)
      
      
      if(YEAR == 2022){
        sf::write_sf(car, paste0(dir_wd, "data/processing/CleanShps/muni", THIS_muni,  ".shp"))
      }
      
      
    }
    
  }
  
  gc()
  
  
}

clean_cars_robust_alternate3 <-  function(munis_and_years){
  i <- 1
  for(i in 1:nrow(munis_and_years)){
    
    # select municipality  
    THIS_muni <- munis_and_years[i, municipio]
    THIS_year <-munis_and_years[i, year] 
    
    property_directories_RUNBATCH <- property_directories %>% 
      .[stri_detect_fixed(., THIS_muni)] %>% 
      .[order(.)] %>% 
      .[!duplicated(.)] %>%
      .[!stri_detect_fixed(., "AREA_IMOVEL_")] %>%
      c()
    
    if(length(property_directories_RUNBATCH)>1){
      message_with_lines("ADJUST CODE IF THIS HAPPENS")
      break
    }
    
    # get the file path to the shape file & municipality name 
    PROPERTY_SHP <- property_directories_RUNBATCH[1]
    PROPERTY_MUNI <- stri_extract_all_regex(PROPERTY_SHP, "\\d{7}") %>% unlist()
    PROPERTY_UF <- PROPERTY_MUNI %>% substr(., 1,2)
    
    # load forest data 
    forests <- load_forrest_data(PROPERTY_UF) %>% 
      mutate(typeA = startsWith(codigo, "FPA")*1) %>% 
      mutate(typeB = startsWith(codigo, "FPB")*1) %>% 
      mutate(typeC = startsWith(codigo, "FPC")*1)  %>% 
      clean_shape()
    
    # status update
    paste0("Fixing: Municipio ", PROPERTY_SHP, " in year ", THIS_year ) %>%  message_with_lines()
    paste0("Fixing ", i, " of ", nrow(munis_and_years), " municipalities left.") %>% message_with_lines()
    
    # 3.2: subset municipality of interest in the micro-dataset -----
    microdata_muni <- microdata %>%
      # relevant variables
      .[, .(
        registro_car,
        codigo_ibge,
        data_inscricao,
        situacao_cadastro
      )] %>% copy() %>% 
      # subset data-set
      .[codigo_ibge == PROPERTY_MUNI] 
    
    # 3.3: get an index of the cars that are present per year 
    car_years_muni <- car_and_reg_year_wide %>% copy() %>% 
      .[municipio == PROPERTY_MUNI] %>% 
      select(-c("municipio")) %>% 
      melt.data.table(id.vars = c("registro_car")) %>% 
      rename_columns(c("variable"),c("year")) %>% 
      .[, year := stri_replace_all_fixed(year, "y", "")]
    
    # 3.4: for each year -----------
    YEAR <- THIS_year
    
    paste0( PROPERTY_SHP, " -- ",  YEAR) %>% message_with_lines()
    
    # subset the cars for that year 
    CARS_THIS_YEAR <- car_years_muni %>% 
      .[year==YEAR & value==1] %>% 
      .[, registro_car]
    
    # if there are cars this year, then proceeds; otherwise skip year 
    if(length(CARS_THIS_YEAR)>0){
      
      # 1: load property data by municipio;  Change 4674 crs -----
      car <- PROPERTY_SHP  %>% 
        read_sf() %>% 
        st_transform(4674)  %>% 
        # subset to relevant properties 
        .[which(.$COD_IMOVEL %in% CARS_THIS_YEAR),]
      
      # 2: flag invalid polygons then convert them to valid ones 
      car$flag_valid <- st_is_valid(car) 
      
      # 3: make invalid polygons valid as well
      car %<>% clean_shape(sf_obj = .)
      
      # 4: create the union of the polygons 
      car_union <- car %>% 
        st_union() %>% 
        clean_union_reenforced()
      
      st_is_valid(car_union)
      
      # 5: compute the union area
      car_area <- car_union %>% 
        st_area()
      
      # 6: compute the intersection with indigenous land area
      car_area_intersect_indi <- car_union %>%
        # calculate intersection area
        st_intersection(indigenous_clean) %>% 
        clean_shape_lands_reenforced()    %>% 
        st_area() %>%
        sum()
      
      # 7: compute the intersection with conservation land area
      car_area_intersect_conserve <- car_union %>%
        # calculate intersection area
        st_intersection(conservation_clean) %>% 
        clean_shape_lands_reenforced()    %>% 
        st_area() %>%
        sum()
      
      # 8A: compute the intersection with forested land area
      car_area_intersect_forestA <- 
        forests[forests$typeA==1, ] %>% 
        clean_shape_lands_reenforced()    %>% 
        # calculate intersection area
        st_intersection( car_union)  
      
      car_area_intersect_forestA %<>% 
        clean_shape_lands_reenforced2()    %>% 
        st_area() %>%
        sum()
      
      # 8b: compute the intersection with forested land area
      car_area_intersect_forestB <- 
        forests[forests$typeB==1, ] %>% 
        clean_shape_lands_reenforced() %>% 
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestB %<>% 
        clean_shape_lands_reenforced2()    %>%
        st_area() %>%
        sum()
      
      # 8c: compute the intersection with forested land area
      car_area_intersect_forestC <- 
        forests[forests$typeC==1, ] %>% 
        clean_shape_lands_reenforced() %>%
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestC %<>% 
        clean_shape_lands_reenforced2() %>%
        st_area() %>%
        sum()
      
      
      # 9.1: identify Cancelled CARs -----
      car_cancelled <- car[which(car$SITUACAO == "CA"), ] %>% clean_shape_reenforced()
      
      car_cancelled  %<>%
        st_union() %>% 
        st_make_valid()   
      
      car_area_cancelled <- car_cancelled %>% 
        st_area()
      
      # 9.2: identify not Cancelled CARs -----
      car_notcancelled <- car[which(car$SITUACAO != "CA"), ] %>% clean_shape_reenforced()
      
      car_notcancelled  %<>%
        st_union() %>% 
        st_make_valid()   
      
      car_area_notcancelled <- car_notcancelled %>% 
        st_area()
      
      # car_area_intersections <- st_intersection(car_cancelled, car_notcancelled) %>% 
      #   st_make_valid() %>% 
      #   st_area() %>% 
      #   sum()
      
      # for each cancelled CAR, what share of its land was cancelled ------
      
      data.table(
        car_union_area = car_area,
        car_area_intersect_indi = car_area_intersect_indi,
        car_area_intersect_conserve = car_area_intersect_conserve,
        car_area_intersect_forestA = car_area_intersect_forestA,
        car_area_intersect_forestB = car_area_intersect_forestB,
        car_area_intersect_forestC = car_area_intersect_forestC,
        car_area_cancelled = car_area_cancelled, 
        car_area_notcancelled = car_area_notcancelled,
        year = YEAR,
        municipio = PROPERTY_MUNI
      ) %>% 
        fwrite(paste0("data/processing/SicarMuniOverlap/muni", PROPERTY_MUNI, "_", YEAR, ".csv"))
      
      rm(car_area)
      rm(car_area_intersect_indi)
      rm(car_area_intersect_conserve)
      rm(car_area_intersect_forestA)
      rm(car_area_intersect_forestB)
      rm(car_area_intersect_forestC)
      
      
      if(YEAR == 2022){
        sf::write_sf(car, paste0(dir_wd, "data/processing/CleanShps/muni", THIS_muni,  ".shp"))
      }
      
      
    }
    
  }
  
  gc()
  
  
}

clean_cars_robust_alternate4 <- function(munis_and_years){
  
  
  # function(munis_and_years){
  i <- 1
  for(i in 1:nrow(munis_and_years)){
    
    # select municipality  
    THIS_muni <- munis_and_years[i, municipio]
    THIS_year <-munis_and_years[i, year] 
    
    property_directories_RUNBATCH <- property_directories %>% 
      .[stri_detect_fixed(., THIS_muni)] %>% 
      .[order(.)] %>% 
      .[!duplicated(.)] %>%
      .[!stri_detect_fixed(., "AREA_IMOVEL_")] %>%
      c()
    
    if(length(property_directories_RUNBATCH)>1){
      message_with_lines("ADJUST CODE IF THIS HAPPENS")
      break
    }
    
    # get the file path to the shape file & municipality name 
    PROPERTY_SHP <- property_directories_RUNBATCH[1]
    PROPERTY_MUNI <- stri_extract_all_regex(PROPERTY_SHP, "\\d{7}") %>% unlist()
    PROPERTY_UF <- PROPERTY_MUNI %>% substr(., 1,2)
    
    # load forest data 
    forests <- load_forrest_data(PROPERTY_UF) %>% 
      mutate(typeA = startsWith(codigo, "FPA")*1) %>% 
      mutate(typeB = startsWith(codigo, "FPB")*1) %>% 
      mutate(typeC = startsWith(codigo, "FPC")*1)  %>% 
      clean_shape()
    
    # status update
    paste0("Fixing: Municipio ", PROPERTY_SHP, " in year ", THIS_year ) %>%  message_with_lines()
    paste0("Fixing ", i, " of ", nrow(munis_and_years), " municipalities left.") %>% message_with_lines()
    
    # 3.2: subset municipality of interest in the micro-dataset -----
    microdata_muni <- microdata %>%
      # relevant variables
      .[, .(
        registro_car,
        codigo_ibge,
        data_inscricao,
        situacao_cadastro
      )] %>% copy() %>% 
      # subset data-set
      .[codigo_ibge == PROPERTY_MUNI] 
    
    # 3.3: get an index of the cars that are present per year 
    car_years_muni <- car_and_reg_year_wide %>% copy() %>% 
      .[municipio == PROPERTY_MUNI] %>% 
      select(-c("municipio")) %>% 
      melt.data.table(id.vars = c("registro_car")) %>% 
      rename_columns(c("variable"),c("year")) %>% 
      .[, year := stri_replace_all_fixed(year, "y", "")]
    
    # 3.4: for each year -----------
    YEAR <- THIS_year
    
    paste0( PROPERTY_SHP, " -- ",  YEAR) %>% message_with_lines()
    
    # subset the cars for that year 
    CARS_THIS_YEAR <- car_years_muni %>% 
      .[year==YEAR & value==1] %>% 
      .[, registro_car]
    
    # if there are cars this year, then proceeds; otherwise skip year 
    if(length(CARS_THIS_YEAR)>0){
      
      # 1: load property data by municipio;  Change 4674 crs -----
      car <- PROPERTY_SHP  %>% 
        read_sf() %>% 
        st_transform(4674)  %>% 
        # subset to relevant properties 
        .[which(.$COD_IMOVEL %in% CARS_THIS_YEAR),]
      
      # 2: flag invalid polygons then convert them to valid ones 
      car$flag_valid <- st_is_valid(car) 
      
      # 3: make invalid polygons valid as well
      car %<>% clean_shape(sf_obj = .)
      
      # 4: create the union of the polygons 
      car_union <- car %>% 
        st_union() %>% 
        clean_union_reenforced()
      
      st_is_valid(car_union)
      
      # 5: compute the union area
      car_area <- car_union %>% 
        st_area()
      
      # 6: compute the intersection with indigenous land area
      car_area_intersect_indi <- car_union %>%
        # calculate intersection area
        st_intersection(indigenous_clean) %>% 
        clean_shape_lands_reenforced()    %>% 
        st_area() %>%
        sum()
      
      # 7: compute the intersection with conservation land area
      car_area_intersect_conserve <- car_union %>%
        # calculate intersection area
        st_intersection(conservation_clean) %>% 
        clean_shape_lands_reenforced()    %>% 
        st_area() %>%
        sum()
      
      # 8A: compute the intersection with forested land area
      car_area_intersect_forestA <- 
        forests[forests$typeA==1, ] %>% 
        clean_shape_lands_reenforced()    %>% 
        # calculate intersection area
        st_intersection( car_union)  
      
      car_area_intersect_forestA %<>% 
        clean_shape_lands_reenforced2()    %>% 
        st_area() %>%
        sum()
      
      # 8b: compute the intersection with forested land area
      car_area_intersect_forestB <- 
        forests[forests$typeB==1, ] %>% 
        clean_shape_lands_reenforced() %>% 
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestB %<>% 
        clean_shape_lands_reenforced2()    %>%
        st_area() %>%
        sum()
      
      # 8c: compute the intersection with forested land area
      car_area_intersect_forestC <- 
        forests[forests$typeC==1, ] %>% 
        clean_shape_lands_reenforced() %>%
        # calculate intersection area
        st_intersection( car_union)
      
      car_area_intersect_forestC %<>% 
        clean_shape_lands_reenforced2() %>%
        st_area() %>%
        sum()
      
      
      # 9.1: identify Cancelled CARs -----
      car_cancelled <- car[which(car$SITUACAO == "CA"), ] %>% clean_shape_reenforced()
      
      car_cancelled  %<>%
        st_union() %>% 
        st_make_valid()   
      
      car_area_cancelled <- car_cancelled %>% 
        st_area()
      
      # 9.2: identify not Cancelled CARs -----
      car_notcancelled <- car[which(car$SITUACAO != "CA"), ] %>% clean_shape_reenforced()
      
      car_notcancelled  %<>%
        st_union() %>% 
        st_make_valid()   
      
      car_area_notcancelled <- car_notcancelled %>% 
        st_area()
      
      # car_area_intersections <- st_intersection(car_cancelled, car_notcancelled) %>% 
      #   st_make_valid() %>% 
      #   st_area() %>% 
      #   sum()
      
      # for each cancelled CAR, what share of its land was cancelled ------
      
      data.table(
        car_union_area = car_area,
        car_area_intersect_indi = car_area_intersect_indi,
        car_area_intersect_conserve = car_area_intersect_conserve,
        car_area_intersect_forestA = car_area_intersect_forestA,
        car_area_intersect_forestB = car_area_intersect_forestB,
        car_area_intersect_forestC = car_area_intersect_forestC,
        car_area_cancelled = car_area_cancelled, 
        car_area_notcancelled = car_area_notcancelled,
        year = YEAR,
        municipio = PROPERTY_MUNI
      ) %>% 
        fwrite(paste0("data/processing/SicarMuniOverlap/muni", PROPERTY_MUNI, "_", YEAR, ".csv"))
      
      rm(car_area)
      rm(car_area_intersect_indi)
      rm(car_area_intersect_conserve)
      rm(car_area_intersect_forestA)
      rm(car_area_intersect_forestB)
      rm(car_area_intersect_forestC)
      
      
      if(YEAR == 2022){
        sf::write_sf(car, paste0(dir_wd, "data/processing/CleanShps/muni", THIS_muni,  ".shp"))
      }
      
      
    }
    
  }
  
  gc()
  
  
}




# sectoin 5.1: define function which consolidates municipality year SICAR variables ----  
consolidate_muni_year_variables <-   function(){
  
  setwd("data/processing/SicarMuniOverlap/")
  
  csv_files <- list.files() 
  
  # Read all files and combine
  sicar_vars <- csv_files %>% .[1] %>% fread()
  for(FILE in csv_files[-1]){
    
    message_with_lines(FILE)
    
    sicar_vars <- fread(FILE) %>% 
      rbind(sicar_vars, ., fill=T)
    
  }
  
  fwrite(sicar_vars,paste0(dir_wd, "data/cleaned/sicar_overlap_variables.csv"))
  
  # section 5.1: update consolidated data with actual number of CARs per municipality -----
  
  setwd(dir_wd)
  
  # 5.1.1 which municipalities had data but then had no more? according to the overlap data ----
  microdata <- fread("data/raw/sicar/microdata/temas_ambientais_update.csv")
  sicar_vars <- fread("data/cleaned/sicar_overlap_variables.csv") %>%     .[year<2023]  

  
  sicar_vars <-
    # expand 
    CJ(unique(sicar_vars$municipio), unique(sicar_vars$year)) %>% 
    rename_columns(c("V1", "V2"), c("municipio", "year")) %>% 
    merge(sicar_vars, by=c("municipio", "year"), all=T) %>% 
    .[, year := as.integer(year)]
  
  
  # 5.1.2: identify all municipalities that actually have 0 CARs in a given year according to microdata ------
  
  # get expanded version of muni-years to identify munis with missing data from the get-go
  munis_years_exp <- CJ(unique(microdata$codigo_ibge), c(2014:2022)) %>% 
    rename_columns(c("V1", "V2"), c("codigo_ibge", "year"))
  
  muni_years_with_zero_cars <- microdata %>% copy() %>% 
    .[!duplicated(registro_car)] %>% 
    .[, .N, .(year(data_inscricao), codigo_ibge)] %>%
    merge(munis_years_exp, c("codigo_ibge", "year"), all=T) %>% 
    setnafill(cols = c("N"), fill=0) %>%
    .[order(codigo_ibge, year)] %>% 
    .[order(year), cumN := cumsum(N), codigo_ibge]  %>% 
    rename_columns(c("N", "cumN", "codigo_ibge"),c("n_new_CARs_microdata","n_CARs_microdata", "municipio")) %>% 
    .[, year := as.integer(year)]
  
  merge(muni_years_with_zero_cars, sicar_vars, c("municipio", "year"), all=T)  %>% 
    fwrite(paste0(dir_wd, "data/cleaned/sicar_overlap_variables_exapanded.csv"))

  sicar_vars_exp <- fread(paste0(dir_wd, "data/cleaned/sicar_overlap_variables_exapanded.csv"))
  
  # only consider missings which we know have CARs    
  sicar_vars_probing_missings <- sicar_vars_exp %>% copy() %>% 
    .[n_CARs_microdata >0 & is.na(car_union_area)] 
  
  sicar_vars_probing_missings %>% 
    fwrite("data/processing/identifying_errors/car_union_area_missing_v2.csv")
  
}

# sectoin 5.1: define function which consolidates municipality year SICAR variables ----  
consolidate_muni_year_variables_s2 <-   function(){
  
  setwd(dir_wd)
  
  setwd("data/processing/SicarMuniOverlap_s2/")
  
  csv_files <- list.files() 
  
  # Read all files and combine
  sicar_vars <- csv_files %>% .[1] %>% fread()
  for(FILE in csv_files[-1]){
    
    message_with_lines(FILE)
    
    sicar_vars <- fread(FILE) %>% 
      rbind(sicar_vars, ., fill=T)
    
  }
  
  fwrite(sicar_vars,paste0(dir_wd, "data/cleaned/sicar_overlap_variables_s2.csv"))
  
  # section 5.1: update consolidated data with actual number of CARs per municipality -----
  
  setwd(dir_wd)
  
  # 5.1.1 which municipalities had data but then had no more? according to the overlap data ----
  microdata <- fread("data/raw/sicar/microdata/temas_ambientais_update.csv")
  sicar_vars <- fread("data/cleaned/sicar_overlap_variables_s2.csv") %>%     .[year<2023]  

  
  sicar_vars <-
    # expand 
    CJ(unique(sicar_vars$municipio), unique(sicar_vars$year)) %>% 
    rename_columns(c("V1", "V2"), c("municipio", "year")) %>% 
    merge(sicar_vars, by=c("municipio", "year"), all=T) %>% 
    .[, year := as.integer(year)]
  
  
  # 5.1.2: identify all municipalities that actually have 0 CARs in a given year according to microdata ------
  
  # get expanded version of muni-years to identify munis with missing data from the get-go
  munis_years_exp <- CJ(unique(microdata$codigo_ibge), c(2014:2022)) %>% 
    rename_columns(c("V1", "V2"), c("codigo_ibge", "year"))
  
  muni_years_with_zero_cars <- microdata %>% copy() %>% 
    .[!duplicated(registro_car)] %>% 
    .[, .N, .(year(data_inscricao), codigo_ibge)] %>%
    merge(munis_years_exp, c("codigo_ibge", "year"), all=T) %>% 
    setnafill(cols = c("N"), fill=0) %>%
    .[order(codigo_ibge, year)] %>% 
    .[order(year), cumN := cumsum(N), codigo_ibge]  %>% 
    rename_columns(c("N", "cumN", "codigo_ibge"),c("n_new_CARs_microdata","n_CARs_microdata", "municipio")) %>% 
    .[, year := as.integer(year)]
  
  merge(muni_years_with_zero_cars, sicar_vars, c("municipio", "year"), all=T)  %>% 
    fwrite(paste0(dir_wd, "data/cleaned/sicar_overlap_variables_exapanded_s2.csv"))

  sicar_vars_exp <- fread(paste0(dir_wd, "data/cleaned/sicar_overlap_variables_exapanded_s2.csv"))
  
  # only consider missings which we know have CARs    
  sicar_vars_probing_missings <- sicar_vars_exp %>% copy() %>% 
    .[n_CARs_microdata >0 & is.na(car_union_area)] 
  
  sicar_vars_probing_missings %>% 
    fwrite("data/processing/identifying_errors/car_union_area_missing_v2_s2.csv")
  
}





