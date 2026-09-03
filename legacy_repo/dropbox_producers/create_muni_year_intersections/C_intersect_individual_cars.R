
# section 6: construct CAR overlap variables --------------

if (fp$CAR_OVERLAP) {
  
  # section 6.0: set-up -------
  
  # section 6.0.1: load data and set up key variables -------
  
  # get the updated micro-data
  microdata <-
    fread("data/raw/sicar/microdata/temas_ambientais_update.csv") %>% 
    .[!duplicated(registro_car)]
  
  
  setwd(dir_wd)
  
  # load years
  car_and_reg_year_wide <-
    fread("data/processing/car_and_reg_year_wide.csv") %>%
    .[, municipio := as.numeric(substr(registro_car, 4, 10))]
  
  # get all file names
  property_directories <-
    list.dirs("data/raw/sicar/shapefiles/", recursive = T) %>%
    .[!stri_detect_fixed(., "AREA_IMOVEL_")] %>% 
    .[stri_detect_fixed(., "AREA_IMOVEL")] %>%
    .[order(.)] %>%
    data.table(path = .) %>%
    .[, municipio := stri_extract_all_regex(path, "\\d{7}")] %>%
    # subset to municipalities in the amazon
    .[municipio %in% municipalities_amazon, path] %>%
    .[order(.)]
  
  # section 6.0.2: do an update-as-you-go framework to exclude municipalities which have been identified -----------------
  # define function to identify munis that need to be cleaned
  get_munis_that_need_to_be_cleaned2 <- function(dir = dir_wd) {
    setwd(dir)
    
    # identify all municipalities that will be cleaned
    all_munis <- property_directories %>%
      stri_extract_all_regex(`.`, "\\d{7}") %>%
      unlist
    
    # BEFORE identifying the files for this batch, identify which files have already been run and remove them from the list
    munis_already_completed <-
      paste0("data/processing/CAROverlap/") %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    # identify municipalities that are still available
    munis_that_still_need_to_be_cleaned <-
      all_munis %>% .[!(. %in% munis_already_completed)]
    
    munis_that_still_need_to_be_cleaned %>%
      return()
    
  }
  
  # define function that selects and claims municipalities
  select_muni_to_clean2 <- function(dir = dir_wd,
                                    available_munis = available_munis) {
    setwd(dir = dir)
    
    
    FILE <- "data/processing/munis_already_claimed_CAR.csv"
    
    if (!file.exists(FILE)) {
      data.table(muni = c("")) %>%
        .[0] %>%
        fwrite(., FILE)
      
    }
    
    
    munis_already_completed <-
      paste0("data/processing/CAROverlap/") %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    already_claimed <-
      fread("data/processing/munis_already_claimed_CAR.csv")
    
    this_muni <- available_munis %>%
      # make sure they are still available
      .[!. %in% munis_already_completed] %>%
      .[!. %in% already_claimed] %>%
      .[sample(length(.))] %>%
      .[1]
    
    data.table(muni = this_muni) %>%
      fwrite("data/processing/munis_already_claimed_CAR.csv",
             append = T)
    
    return(this_muni)
    
  }
  
  # identify claimed municipalities (may or may not appear in the `get_munis_that_need...` output)
  get_munis_that_were_claimed_or_cleaned2 <- function(dir = dir_wd) {
    setwd(dir)
    
    FILE <- "data/processing/munis_already_claimed_CAR.csv"
    
    if (!file.exists(FILE)) {
      data.table(muni = c("")) %>%
        .[0] %>%
        fwrite(., FILE)
      
    }
    
    munis1 <- fread(FILE) %>%
      .[, muni]
    
    munis2 <- get_munis_that_need_to_be_cleaned2(dir = dir)
    
    munis2 %>% .[!. %in% munis1] %>%
      .[!duplicated(.)] %>%
      return()
    
  }
  
  # identify available municipalities
  available_munis <- get_munis_that_need_to_be_cleaned2()
  
  # section 6.1: while there is still an available municipality------
  while (length(available_munis) > 0) {
    
    # section 6.1.1: identify and select the muni we will work with in this iteration ------
    available_munis <- get_munis_that_need_to_be_cleaned2() #%>% .[.%in%c(1200401)] # 1100205
    
    if(length(available_munis)<1){break}
    
    # section 6.1.2: check which municipalities are available (useful for manual attempts) ------
    order_by_size <- microdata %>%
      .[codigo_ibge %in% available_munis] %>%
      .[, .N, codigo_ibge] %>%
      .[order(-N)]  
    # .[codigo_ibge!=1702208] # this one leads to an issue.. looks like there are duplicated CARs
    
    # available_munis <- order_by_size[, codigo_ibge][1]
    # DELETE LATER!!!!
    # available_munis <- c(1702208)
    
    THIS_muni <-
      select_muni_to_clean2(dir = dir_wd, available_munis = available_munis)
    
    # THIS_muni <- 1702208   
    
    if(fp$CHECKS){
      
      n_unique_cars_zero <- "data/processing/identifying_errors/n_unique_cars_zero.csv" %>% fread() %>% .[, .(municipio, year)]
      n_unique_conflicting_cars_zero <- "data/processing/identifying_errors/n_unique_conflicting_cars_zero.csv" %>% fread() %>% .[, .(municipio, year)]
      
      
      n_unique_cars_zero %>% 
        .[, .N, year]
      
      n_unique_conflicting_cars_zero %>% 
        .[, .N, year]
    }
    
    # THIS_muni <- 2111300  
    # THIS_muni <- 1702208 1502509   #1508308 # 1505064
    
    # select property directoy
    property_directories_RUNBATCH <- property_directories %>%
      .[!stri_detect_fixed(., "AREA_IMOVEL_")] %>%
      .[stri_detect_fixed(., THIS_muni)] %>%
      .[order(.)] %>%
      .[!duplicated(.)] %>%
      c()
    
    # get the file path to the shape file & municipality name
    PROPERTY_SHP <- property_directories_RUNBATCH # only consider first if there are multiple
    PROPERTY_MUNI <-
      stri_extract_all_regex(PROPERTY_SHP, "\\d{7}") %>% unlist()
    PROPERTY_UF <- PROPERTY_MUNI %>% substr(., 1, 2)
    
    # status update
    message_with_lines(PROPERTY_SHP)
    paste0(length(available_munis), " municipalities left.") %>% message_with_lines()
    
    # 6.2: subset municipality of interest in the micro-data set -----
    microdata_muni <- microdata %>%
      # relevant variables
      .[, .(registro_car,
            codigo_ibge,
            data_inscricao,
            situacao_cadastro)] %>% copy() %>%
      # subset data-set
      .[codigo_ibge == PROPERTY_MUNI]  %>% 
      .[!duplicated(registro_car)]
    
    
    microdata_muni %>% .[, .N, year(data_inscricao)] %>% .[order(year)]
    
    # section 6.4: for 2023 (another deprecated for-loop) -----------
    paste0(PROPERTY_SHP) %>% message_with_lines()
    
    # identify the years that they are present 
    car_years_muni <- car_and_reg_year_wide %>% copy() %>% 
      .[, FULL := 1] %>% 
      .[municipio == PROPERTY_MUNI] %>% 
      select(-c("municipio")) %>% 
      melt.data.table(id.vars = c("registro_car")) %>% 
      rename_columns(c("variable"),c("year")) %>% 
      .[, year := stri_replace_all_fixed(year, "y", "")]
    
    CARS_THIS_YEAR <- car_years_muni %>% 
      .[year=="FULL" & value==1] %>% 
      .[, registro_car]
    
    #  section 6.4.1: if there are CARs this year, then proceeds; otherwise skip year  ------
    if (length(CARS_THIS_YEAR) > 0) {
      
      setwd(dir_wd)
      
      # 1: load property data by municipio;  Change 4674 crs -----
      car <- PROPERTY_SHP  %>%
        read_sf() %>%
        st_transform(4674)  %>%
        # subset to relevant properties
        .[which(.$COD_IMOVEL %in% CARS_THIS_YEAR), ]
      
      # 2: flag invalid polygons then convert them to valid ones ----
      car$flag_valid <- st_is_valid(car)
      
      # if there are invalid CARs, save them to check later
      if(sum(car$flag_valid==FALSE)>0){
        
        dir_save_errors1 <- paste0("data/processing/CAROverlap_invalid_preclean/", PROPERTY_MUNI, "/")
        
        if(!dir.exists(dir_save_errors1)){dir.create(dir_save_errors1)}
        
        # for manual checking
        if(fp$CHECKS){
          car[which(car$flag_valid==FALSE),] %>% View()
          car[which(car$flag_valid==FALSE),] %>% plot(., axes=T)
        }
        
        car[which(car$flag_valid==FALSE),] %>% 
          st_write(paste0("data/processing/CAROverlap_invalid_preclean/",  PROPERTY_MUNI, "/", PROPERTY_MUNI, ".shp"), append = TRUE)
        
        message_with_lines(paste0("CHECK LATER: Saved: data/processing/CAROverlap_invalid_preclean/",  PROPERTY_MUNI, "/",  PROPERTY_MUNI, ".shp"))
      }
      
      # 3: make invalid polygons valid as well ----
      car %<>%
        clean_shape()
      
      # 3.2: flag invalid polygons that have already been cleaned, drop them and save again ----
      car$flag_valid <- st_is_valid(car)
      
      # if there are invalid CARs, save them to check later
      if(sum(car$flag_valid==FALSE)>0){
        
        # STILL PROTOTYPING   
        
        # for manual checking
        if(fp$CHECKS){
          car[which(car$flag_valid==FALSE),] %>% View()
          car[which(car$flag_valid==FALSE),] %>% plot(., axes=T)
        }
        
        dir_save_errors2 <- paste0("data/processing/CAROverlap_invalid_preclean/", PROPERTY_MUNI, "_second/")
        
        if(!dir.exists(dir_save_errors2)){dir.create(dir_save_errors2)}
        
        # first, save invalids
        
        car[which(car$flag_valid==FALSE),] %>% 
          st_write(paste0("data/processing/CAROverlap_invalid_preclean/",  PROPERTY_MUNI, "_second/", PROPERTY_MUNI, ".shp"))
        
        message_with_lines(paste0("CHECK LATER: Saved: data/processing/CAROverlap_invalid_preclean/",  PROPERTY_MUNI, "_second/", PROPERTY_MUNI, ".shp"))
        
        
        # second, try to fix CARs 
        
        car %<>% copy() %>%  clean_shape_reenforced_CARs()
        
      }
      
      # second_trial <- "data/processing/CAROverlap_invalid_preclean/1100700_second/1100700.shp" %>% st_read()

      # 5: intersect all CARs ------
      car_intersection <- st_intersection(car, car)
      
      # 6: identify any issues and correct them ------
      car_intersection %<>% clean_shape()
      # 8: compute the area -------
      car_intersection %<>%
        mutate(int_area = st_area(geometry))
      
      # 9: drop the geometries -------
      out <- car_intersection %>%
        as.data.table() %>% copy() %>%
        .[, geometry := NULL]
      
      # 10: add CAR date information -----
      car_date <-
        microdata_muni[, .(registro_car, data_inscricao)]
      
      # 11: save ------
      out %>%
        .[, .(
          COD_IMOVEL,
          COD_IMOVEL.1,
          int_area,
          NUM_AREA,
          COD_ESTADO,
          NUM_AREA.1,
          COD_ESTADO.1,
          SITUACAO,
          CONDICAO_I,
          SITUACAO.1,
          CONDICAO_I.1
        )] %>%
        merge(
          x = .,
          y = car_date,
          by.x = "COD_IMOVEL.1",
          by.y = "registro_car",
          all.x = T,
          all.y = F
        ) %>% rename_columns(c("data_inscricao"), c("data_inscricao.1")) %>%
        merge(
          x = .,
          y = car_date,
          by.x = "COD_IMOVEL",
          by.y = "registro_car",
          all.x = T,
          all.y = F
        ) %>%
        fwrite(
          x = ,
          file =
            paste0(
              "data/processing/CAROverlap/muni",
              PROPERTY_MUNI,
              ".csv"
            )
        )
      
      message_with_lines(paste0(
        "data/processing/CAROverlap/muni",
        PROPERTY_MUNI,
        ".csv"
      ))
      
      
      gc()
      
      
    }
    
    
    gc()
    
    
    # 12: update the available municipalities ------
    not_available_munis <- get_munis_that_were_claimed_or_cleaned2()
    available_munis <- get_munis_that_need_to_be_cleaned2()
    available_munis %>% .[!. %in% not_available_munis]
    
  }
  

}


  
if(fp$INTERSECT_CARS_ROBUST){

  setwd(dir_wd)
  property_directory <- "/data/research/Thiago/Amazon/data/processing/CleanCARShapes_robust/"
  car_intersection_directory <- "/data/research/Thiago/Amazon/data/processing/CAROverlap_robust/"
  claimed_file <- "/data/research/Thiago/Amazon/data/processing/munis_already_claimed_CAROverlap_robust.csv"
  if(!dir.exists(car_intersection_directory)){dir.create(car_intersection_directory)}
  
  # load micro data 
  microdata <- fread("data/raw/sicar/microdata/temas_ambientais_update.csv") %>% 
    .[!duplicated(registro_car)]

  # function to intersect municipal cars -------
  compute_intersections <- function(car, microdata){
    
    # 5: intersect all CARs ------
    car_intersection <- st_intersection(car, car)
    
    # 6: identify any issues and correct them ------
    car_intersection %<>% clean_shape()
    # 8: compute the area -------
    car_intersection %<>%
      mutate(int_area = st_area(geometry))
    
    # 9: drop the geometries -------
    out <- car_intersection %>%
      as.data.table() %>% copy() %>%
      .[, geometry := NULL]
    
    # 10: add CAR date information -----
    car_date <-
      microdata_muni[, .(registro_car, data_inscricao)]
    
    # 11: save ------
    out %>%
      .[, .(
        COD_IMOVEL,
        COD_IMOVEL.1,
        int_area,
        NUM_AREA,
        COD_ESTADO,
        NUM_AREA.1,
        COD_ESTADO.1,
        SITUACAO,
        CONDICAO_I,
        SITUACAO.1,
        CONDICAO_I.1
      )] %>%
      merge(
        x = .,
        y = car_date,
        by.x = "COD_IMOVEL.1",
        by.y = "registro_car",
        all.x = T,
        all.y = F
      ) %>% rename_columns(c("data_inscricao"), c("data_inscricao.1")) %>%
      merge(
        x = .,
        y = car_date,
        by.x = "COD_IMOVEL",
        by.y = "registro_car",
        all.x = T,
        all.y = F
      ) %>% return()
  }

  get_munis_already_intersected <- function(){
    
    munis_already_completed <-
      car_intersection_directory %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()

    return(munis_already_completed)
    
  }
  
  get_munis_whose_cars_need_to_be_intersected <- function() {
    
    setwd(dir_wd)
    
    # identify all municipalities that will be cleaned
    all_munis <-     
      property_directory %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    munis_already_completed <- get_munis_already_intersected()
    
    # identify municipalities that are still available
    munis_that_still_need_to_be_cleaned <-
      all_munis %>% .[!(. %in% munis_already_completed)]
    
    munis_that_still_need_to_be_cleaned %>%
      return()
    
  }
  
  update_claimed_muni_list_for_intersection <- function(){
    
    setwd(dir = dir_wd)
    
    FILE <- claimed_file    
    if (!file.exists(FILE)) {
      data.table(muni = c("")) %>%
        .[0] %>%
        fwrite(., FILE)
    }
    
    # only update with what was completed
    munis_already_completed <- get_munis_already_intersected()
    
    munis_claimed_or_intersected <- fread(FILE) %>% rbind(
      data.table(muni = munis_already_completed) ) %>% 
      .[!duplicated(muni)] 

    munis_claimed_or_intersected %>% 
      fwrite(FILE)
    
    return(munis_claimed_or_intersected)
    
    
  }
  
  # define function that selects and claims municipalities
  select_muni_to_intersect_CAR <- function() {
    
    already_claimed <- claimed_file %>% 
      fread()
    

    available_munis <- get_munis_whose_cars_need_to_be_intersected()
    
    # only update with what was completed
    munis_already_completed <- get_munis_already_intersected()
    
    
    this_muni <- available_munis %>%
      # make sure they are still available
      .[!. %in% munis_already_completed] %>%
      .[!. %in% already_claimed] %>%
      .[sample(length(.))] %>%
      .[1]
    
    if(!is.na(this_muni)){
      data.table(muni = this_muni) %>%
        fwrite(claimed_file,append = T)
    }
    
    return(this_muni)
    
  }
  
  available_munis <- get_munis_whose_cars_need_to_be_intersected()
  
  while(length(available_munis)>0){
    
    THIS_muni <- select_muni_to_intersect_CAR()
    
    # select property directoy
    property_directories_RUNBATCH <- property_directory %>%
      list.dirs() %>% 
      .[stri_detect_fixed(., THIS_muni)] %>%
      .[order(.)] %>%
      .[!duplicated(.)] %>%
      c()
    
    # get the file path to the shape file & municipality name
    PROPERTY_SHP <- property_directories_RUNBATCH # only consider first if there are multiple
    PROPERTY_MUNI <-
      stri_extract_all_regex(PROPERTY_SHP, "\\d{7}") %>% unlist()
    PROPERTY_UF <- PROPERTY_MUNI %>% substr(., 1, 2)
    
    message_with_lines(PROPERTY_SHP)
    paste0(length(available_munis), " municipalities left.") %>% message_with_lines()
    
    
    # 6.2: subset municipality of interest in the micro-data set -----
    microdata_muni <- microdata %>%
      # relevant variables
      .[, .(registro_car,
            codigo_ibge,
            data_inscricao,
            situacao_cadastro)] %>% copy() %>%
      # subset data-set
      .[codigo_ibge == PROPERTY_MUNI]  %>% 
      .[!duplicated(registro_car)] %>% 
      .[year(data_inscricao )<2023]
    
    microdata_muni %>% .[, .N, year(data_inscricao)] %>% .[order(year)]
    
    # section 6.4: for 2023 (another deprecated for-loop) -----------
    paste0(PROPERTY_SHP) %>% message_with_lines()
    
    CARS_IN_SAMPLE <- microdata_muni %>% 
      .[, unique(registro_car)]
    
    #  section 6.4.1: if there are CARs this year, then proceeds; otherwise skip year  ------
    if (length(CARS_IN_SAMPLE) > 0) {
      
      setwd(dir_wd)
      
      # 1: load property data by municipio;  Change 4674 crs -----
      CARs <- PROPERTY_SHP  %>%
        read_sf() %>% 
        .[which(.$COD_IMOVEL%in%CARS_IN_SAMPLE), ]
    
      out <- tryCatch(expr = {compute_intersections(car=CARs, microdata = microdata_muni)})
      
      out %>%
          fwrite(
            x = ,
            file =
              paste0(
                car_intersection_directory, 
                "muni",
                PROPERTY_MUNI,
                ".csv"
              )
          )
      
    }

    # check which municipalities are available 
    available_munis <- get_munis_whose_cars_need_to_be_intersected()
    
  }
  
  
  
  # %>%
  #   fwrite(
  #     x = ,
  #     file =
  #       paste0(
  #         "data/processing/CAROverlap/muni",
  #         PROPERTY_MUNI,
  #         ".csv"
  #       )
  #   )
  # 
  # message_with_lines(paste0(
  #   "data/processing/CAROverlap/muni",
  #   PROPERTY_MUNI,
  #   ".csv"
  # ))
  # 
  # 
  # gc()
  # 
  
}
  
if(fp$INTERSECT_CARS_S2){
  
  # this code takes the municipalities which did not run in the 'robust' cleaning method and runs them using the 's2' cleaning method. 
  
  setwd(dir_wd)
  property_directory <- "/data/research/Thiago/Amazon/data/processing/CleanCARShapes_s2/"
  car_intersection_directory <- "/data/research/Thiago/Amazon/data/processing/CAROverlap_s2/"
  claimed_file <- "/data/research/Thiago/Amazon/data/processing/munis_already_claimed_CAROverlap_s2.csv"
  if(!dir.exists(car_intersection_directory)){dir.create(car_intersection_directory)}
  
  # load micro data 
  microdata <- fread("data/raw/sicar/microdata/temas_ambientais_update.csv") %>% 
    .[!duplicated(registro_car)]
  
  # function to intersect municipal cars -------
  compute_intersections <- function(car, microdata){
    
    # 5: intersect all CARs ------
    car_intersection <- st_intersection(car, car)
    
    # 6: identify any issues and correct them ------
    car_intersection %<>% clean_shape()
    # 8: compute the area -------
    car_intersection %<>%
      mutate(int_area = st_area(geometry))
    
    # 9: drop the geometries -------
    out <- car_intersection %>%
      as.data.table() %>% copy() %>%
      .[, geometry := NULL]
    
    # 10: add CAR date information -----
    car_date <-
      microdata_muni[, .(registro_car, data_inscricao)]
    
    # 11: save ------
    out %>%
      .[, .(
        COD_IMOVEL,
        COD_IMOVEL.1,
        int_area,
        NUM_AREA,
        COD_ESTADO,
        NUM_AREA.1,
        COD_ESTADO.1,
        SITUACAO,
        CONDICAO_I,
        SITUACAO.1,
        CONDICAO_I.1
      )] %>%
      merge(
        x = .,
        y = car_date,
        by.x = "COD_IMOVEL.1",
        by.y = "registro_car",
        all.x = T,
        all.y = F
      ) %>% rename_columns(c("data_inscricao"), c("data_inscricao.1")) %>%
      merge(
        x = .,
        y = car_date,
        by.x = "COD_IMOVEL",
        by.y = "registro_car",
        all.x = T,
        all.y = F
      ) %>% return()
  }
  
  get_munis_already_intersected_robust <- function(){
    
    munis_already_completed <-
      car_intersection_directory %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    return(munis_already_completed)
    
  }
  
  get_munis_already_intersected <- function(){
    
    munis_already_completed <-
      car_intersection_directory %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    return(munis_already_completed)
    
  }
  
  get_munis_whose_cars_need_to_be_intersected <- function() {
    
    setwd(dir_wd)
    
    # identify all municipalities that will be cleaned
    all_munis <-     
      property_directory %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    munis_already_completed <- get_munis_already_intersected()
    
    # identify municipalities that are still available
    munis_that_still_need_to_be_cleaned <-
      all_munis %>% .[!(. %in% munis_already_completed)]
    
    munis_that_still_need_to_be_cleaned %>%
      return()
    
  }
  
  update_claimed_muni_list_for_intersection <- function(){
    
    setwd(dir = dir_wd)
    
    FILE <- claimed_file    
    if (!file.exists(FILE)) {
      data.table(muni = c("")) %>%
        .[0] %>%
        fwrite(., FILE)
    }
    
    # only update with what was completed
    munis_already_completed <- get_munis_already_intersected()
  
    
    munis_claimed_or_intersected <- fread(FILE) %>% rbind(
      data.table(muni = munis_already_completed) ) %>% 
      .[!duplicated(muni)] 
    
    munis_claimed_or_intersected %>% 
      fwrite(FILE)
    
    return(munis_claimed_or_intersected)
    
    
  }
  
  # define function that selects and claims municipalities
  select_muni_to_intersect_CAR <- function() {
    
    already_claimed <- claimed_file %>% 
      fread()
    
    
    available_munis <- get_munis_whose_cars_need_to_be_intersected()
    
    # only update with what was completed
    munis_already_completed <- get_munis_already_intersected()
    
    
    this_muni <- available_munis %>%
      # make sure they are still available
      .[!. %in% munis_already_completed] %>%
      .[!. %in% already_claimed] %>%
      .[sample(length(.))] %>%
      .[1]
    
    if(!is.na(this_muni)){
      data.table(muni = this_muni) %>%
        fwrite(claimed_file,append = T)
    }
    
    return(this_muni)
    
  }
  
  available_munis <- get_munis_whose_cars_need_to_be_intersected() 
  
  while(length(available_munis)>0){
    
    THIS_muni <- select_muni_to_intersect_CAR()
    # THIS_muni <- 1505064
    
    # select property directoy
    property_directories_RUNBATCH <- property_directory %>%
      list.dirs() %>% 
      .[stri_detect_fixed(., THIS_muni)] %>%
      .[order(.)] %>%
      .[!duplicated(.)] %>%
      c()
    
    # get the file path to the shape file & municipality name
    PROPERTY_SHP <- property_directories_RUNBATCH # only consider first if there are multiple
    PROPERTY_MUNI <-
      stri_extract_all_regex(PROPERTY_SHP, "\\d{7}") %>% unlist()
    PROPERTY_UF <- PROPERTY_MUNI %>% substr(., 1, 2)
    
    message_with_lines(PROPERTY_SHP)
    paste0(length(available_munis), " municipalities left.") %>% message_with_lines()
    
    
    # 6.2: subset municipality of interest in the micro-data set -----
    microdata_muni <- microdata %>%
      # relevant variables
      .[, .(registro_car,
            codigo_ibge,
            data_inscricao,
            situacao_cadastro)] %>% copy() %>%
      # subset data-set
      .[codigo_ibge == PROPERTY_MUNI]  %>% 
      .[!duplicated(registro_car)] %>% 
      .[year(data_inscricao )<2023]
    
    microdata_muni %>% .[, .N, year(data_inscricao)] %>% .[order(year)]
    
    # section 6.4: for 2023 (another deprecated for-loop) -----------
    paste0(PROPERTY_SHP) %>% message_with_lines()
    
    CARS_IN_SAMPLE <- microdata_muni %>% 
      .[, unique(registro_car)]
    
    #  section 6.4.1: if there are CARs this year, then proceeds; otherwise skip year  ------
    if (length(CARS_IN_SAMPLE) > 0) {
      
      setwd(dir_wd)
      
      # 1: load property data by municipio;  Change 4674 crs -----
      CARs <- PROPERTY_SHP  %>%
        read_sf() %>% 
        .[which(.$COD_IMOVEL%in%CARS_IN_SAMPLE), ]
      
      out <- tryCatch(expr = {compute_intersections(car=CARs, microdata = microdata_muni)})
      
      out %>%
        fwrite(
          x = ,
          file =
            paste0(
              car_intersection_directory, 
              "muni",
              PROPERTY_MUNI,
              ".csv"
            )
        )
      
    }
    
    # check which municipalities are available 
    available_munis <- get_munis_whose_cars_need_to_be_intersected()
    
  }
  
}

# section 7: consolidate CAR overlap variables --------------

if (fp$CONSOLIDATE_VARIABLES_ROBUST) {
  
  # section 7.0: consolidate CAR information from Shapefiles -------------
  CONDOLIDATE_SHAPEFILE_DATA <- TRUE
  if(CONDOLIDATE_SHAPEFILE_DATA){
    
    setwd(dir_wd)
    
    # section 7.0.1: get directories where shape files are located ------
    directories <- list.dirs("data/raw/sicar/shapefiles/", recursive = T) %>% .[stri_detect_fixed(., "AREA_IMOVEL")] %>% .[endsWith(., "AREA_IMOVEL")] %>% stri_replace_all_fixed(., "//", "/")
    
    # section 7.0.2: read em in ------  
    full <-  paste0(directories[1],  "/AREA_IMOVEL.dbf") %>% foreign::read.dbf() %>% as.data.table() 
    
    for(DIR in directories[-1]){
      
      full <-  paste0(DIR,  "/AREA_IMOVEL.dbf") %>% foreign::read.dbf() %>% as.data.table() %>% rbind(full, ., fill=T)
      
    }
    
    fwrite(full, "data/cleaned/sicar_area_imovel_combined.csv")
    
  }
  
  # section 7.1: load in all municipal CAR intersection data-sets ---------
  CONSOLIDATE_CAR_INTERSECTIONS <- TRUE
  if(CONSOLIDATE_CAR_INTERSECTIONS){
    
    
    # section 7.1: consolidate all CAR intersections -----
    paste0(dir_wd, "data/processing/CAROverlap_s2/") %>% setwd()
    
    csv_files <- list.files()
    
    car_vars_s2 <- csv_files %>% .[1] %>% fread()
    for (FILE in csv_files[-1]) {
      message_with_lines(FILE)
      
      car_vars_s2 <- fread(FILE) %>%
        .[, `data_inscricao.1` := as.IDate(`data_inscricao.1`)] %>%
        .[, data_inscricao := as.IDate(data_inscricao)] %>%
        rbind(car_vars, ., fill = T)
      
    }
    
    fwrite(car_vars_s2,
           paste0(dir_wd, "data/cleaned/CAR_overlap_variables_s2.csv"))
    
    # section 7.2: consolidate all CAR intersections -----
    paste0(dir_wd, "data/processing/CAROverlap_robust/") %>% setwd()
    
    csv_files <- list.files()
    
    car_vars_robust <- csv_files %>% .[1] %>% fread()
    for (FILE in csv_files[-1]) {
      message_with_lines(FILE)
      
      car_vars <- fread(FILE) %>%
        .[, `data_inscricao.1` := as.IDate(`data_inscricao.1`)] %>%
        .[, data_inscricao := as.IDate(data_inscricao)] %>%
        rbind(car_vars, ., fill = T)
      
    }
    
    fwrite(car_vars_robust,
           paste0(dir_wd, "data/cleaned/CAR_overlap_variables_robust.csv"))
    
    # section 7.3: consolidate the S2 and Robust intersections, prioritizing S2 intersections ----------------
    car_vars_s2 <-   fread(
      paste0(dir_wd, "data/cleaned/CAR_overlap_variables_s2.csv")) %>% 
      .[year(data_inscricao.1)<2023& year(data_inscricao)<2023] %>% 
      .[, cleaning_method := "1_s2_true"]
    
    car_vars_robust <-   fread(
      paste0(dir_wd, "data/cleaned/CAR_overlap_variables_robust.csv")) %>% 
      .[year(data_inscricao.1)<2023& year(data_inscricao)<2023] %>% 
      .[, cleaning_method := "2_robust"]
    
    car_vars <- rbind(car_vars_s2, car_vars_robust) 
    
    car_vars %>% fwrite(.,  paste0(dir_wd, "data/cleaned/CAR_overlap_variables_FINAL.csv"))
    
    # section 7.4: force CAR intersections into reference and target nomenclature, where reference is registered either after of on same day as target, allow for self intersections, prioritize s2 cleaning -----

    # get all intersections where COD_IMOVEL comes AFTER 
    car_vars_cod_after_cod1 <- car_vars %>% copy() %>% 
      .[data_inscricao > data_inscricao.1] %>% 
      .[,  GRP:= .GRP, .(COD_IMOVEL, COD_IMOVEL.1) ] %>% 
      .[order(cleaning_method)] %>% 
      .[!duplicated(GRP)] %>% .[, GRP:=NULL]

    car_vars_cod_after_cod1[, .N, cleaning_method] # check cleaning methods
    
    # equals
    car_vars_cod_equal_cod1 <- car_vars %>% copy() %>% 
      .[data_inscricao == data_inscricao.1] %>% 
      .[,  GRP:= .GRP, .(COD_IMOVEL, COD_IMOVEL.1) ] %>%
      .[order(cleaning_method)] %>% 
      .[!duplicated(GRP)] %>% .[, GRP:=NULL]
    
    car_vars_cod_equal_cod1[, .N, cleaning_method] # check cleaning methods
    
    # reference comes before
    car_vars_cod_before_cod1 <- car_vars %>% copy() %>% 
      .[data_inscricao < data_inscricao.1] %>% 
      .[,  GRP:= .GRP, .(COD_IMOVEL, COD_IMOVEL.1) ] %>%
      .[order(cleaning_method)] %>% 
      .[!duplicated(GRP)]  %>% .[, GRP:=NULL] %>% 
      rename_columns(c("COD_IMOVEL", "COD_IMOVEL.1","data_inscricao", "SITUACAO", "SITUACAO.1", "data_inscricao.1"), 
                     c("COD_IMOVEL.1", "COD_IMOVEL","data_inscricao.1", "SITUACAO.1", "SITUACAO", "data_inscricao"))
    
    # conflicts where the reference CAR is ALWYS the COD_IMOVEL
    car_vars_ordered <- rbind(car_vars_cod_after_cod1, car_vars_cod_equal_cod1) %>% 
      rbind(car_vars_cod_before_cod1) %>% 
      .[order(cleaning_method)] %>%
      .[,  GRP:= .GRP, .(COD_IMOVEL, COD_IMOVEL.1) ] %>%
      .[!duplicated(GRP)]  %>% .[, GRP:=NULL]
    
    car_vars_ordered %<>% 
      rename_columns(c("COD_IMOVEL", "COD_IMOVEL.1","data_inscricao", "SITUACAO", "SITUACAO.1", "data_inscricao.1"), 
                     c("carid_reference", "carid_target","data_inscricao_reference", "SITUACAO_reference", "SITUACAO_target", "data_inscricao_target"))
    
    fwrite(car_vars_ordered,
           paste0(dir_wd, "data/cleaned/CAR_overlap_variables_reftarget.csv"))
    
    
    car_vars_ordered %>% .[carid_reference!=carid_target] %>% 
      fwrite(., paste0(dir_wd, "data/cleaned/CAR_overlap_variables_conflicts.csv"))
    
    # section 7.5: get the intersection area for cars that did not self intersect (Rough method using avaialble data) -----
    
    # data on all car intersections
    car_vars_ordered <- fread(
      paste0(dir_wd, "data/cleaned/CAR_overlap_variables_reftarget.csv"))
    
    # section 7.3.1: detect any missing CARs for further probing ------
    
    # two ways of checking 
    # rough 
    car_vars %>% 
      .[ , sum(COD_IMOVEL==COD_IMOVEL.1), COD_IMOVEL ] %>%
      .[, .N, V1] %>%
      rename_columns(c("V1", "N"), c("Self-Intersections per CAR", "Count")) %>%
      xtable() %>%
      print(., include.rownames = FALSE)
    
    # more precise
    car_vars_ordered %>% copy() %>%
      .[, sum(carid_reference == carid_target), carid_reference] %>%
      .[, .N, V1] %>%
      rename_columns(c("V1", "N"), c("Self-Intersections per CAR", "Count")) %>%
      xtable() %>%
      print(., include.rownames = FALSE)
    
    # section 7.3.2: retrieve own CAR area computation using sf method ---------------
    own_area <- car_vars_ordered %>%
      copy() %>%
      .[carid_reference == carid_target] %>%
      .[, .(NUM_AREA, carid_reference, int_area, data_inscricao_reference, SITUACAO_reference)] %>%
      .[!duplicated(carid_reference)] %>%
      rename_columns(
        c("int_area"),
        c("int_area_ref")
      )
    
    # section 7.4: correct missing self intersection data using official area data -----
    
    # section 7.4.0: load shapefile data
    sicar_area_imovel_combined <- fread( paste0(dir_wd,"data/cleaned/sicar_area_imovel_combined.csv"))
    
    # section 7.4.1: identify the municipalities that need updating
    codes_with_area <- own_area %>% .[, unique(carid_reference)]
    codes_all <- car_vars_ordered %>% .[, unique(carid_reference)]
    find_info <- codes_all %>% .[!(.%in% codes_with_area)]
    
    
    # create supplementaty data-set for own data
    own_area_supplement1 <- sicar_area_imovel_combined %>%
      copy() %>%
      .[COD_IMOVEL%in%find_info] %>%
      .[, .(NUM_AREA, COD_IMOVEL, SITUACAO)]
    
    own_area_supplement2 <- car_vars_ordered %>%
      copy() %>%
      .[carid_reference%in%find_info] %>%
      .[!duplicated(carid_reference)] %>%
      .[, .(carid_reference, data_inscricao_reference)]
    
    own_area <- merge(y=own_area_supplement1, x=own_area_supplement2, by.y = "COD_IMOVEL", by.x="carid_reference", all=T) %>%
      .[, supplemented := TRUE] %>%
      rbind(own_area, ., fill=T) %>%
      .[is.na(int_area_ref), int_area_ref := NUM_AREA*10000] %>%
      .[, .(carid_reference , int_area_ref)]
    
    car_vars_ordered_out <- 
      merge(car_vars_ordered, own_area, by.x = "carid_reference", by.y = "carid_reference", allx=T, all.y=F) %>% 
      rename_columns(
        c("int_area_ref"),
        c("int_area_reference")
      ) %>% 
      merge(., own_area, by.x = "carid_target", by.y = "carid_reference", allx=T, all.y=F) %>% 
      rename_columns(
        c("int_area_ref"),
        c("int_area_target")
      ) 
    
    fwrite(car_vars_ordered_out,
           paste0(dir_wd, "data/cleaned/CAR_overlap_variables_reftarget_areas.csv"))
    
    
    # section 7.4.2:  check to see if there are any patterns for the CARs and Munis with no self intersection -----
    if(fp$CHECKS){
      
      setwd(dir_wd)
      
      # section 7.4.2.1: identify the munis with missing data ------
      munis_with_missing_self_intersections<- find_info %>% 
        substr(4,10) %>% 
        unique()
      
      # section 7.4.2.2: check what share of the muni self intersects
      own_area2 <- own_area %>% copy() %>% .[, muni:= substr(carid_reference, 4,10)]
      car_vars_ordered2 <- car_vars_ordered %>% copy() %>% .[, muni:= substr(carid_reference, 4,10)]
      for(MUNICIPIO in munis_with_missing_self_intersections){
        
        message_with_lines(MUNICIPIO)
        
        reference_cars_with_self_intersection <- own_area2 %>% copy() %>% 
          .[muni==MUNICIPIO] %>% 
          .[, unique(carid_reference)]
        
        all_cars_in_muni <- car_vars_ordered2 %>% copy() %>% 
          .[muni==MUNICIPIO] %>% 
          .[, .(carid_target, carid_reference)] %>% 
          .[, index := 1:.N] %>%
          melt.data.table(id.vars = "index") %>% 
          .[, unique(value)]
        
        # 1) find the share of CARs with intersection  ----
        share_of_munis <- round(100*uniqueN(reference_cars_with_self_intersection)/uniqueN(all_cars_in_muni) )
        
        paste0("Share of CARs with self intersection ", share_of_munis, "% (", uniqueN(reference_cars_with_self_intersection), ").
           Municipio: ", MUNICIPIO) %>% message_with_lines()
        
        # 2) find the average, min, max, median area of the CARs with missing data 
        cars_with_missing_self_intersection <- 
          all_cars_in_muni %>% .[!.%in%reference_cars_with_self_intersection]
        
        # info on CARs with missing data 
        cars_with_missing_data <- own_area_supplement1 %>% copy() %>% 
          .[COD_IMOVEL%in%cars_with_missing_self_intersection]  
        
        paste0("CARs with missing data for municipio: ", MUNICIPIO) %>% message_with_lines()
        cars_with_missing_data %>% 
          summary() %>% 
          print()    
        
        # info on CARs without missing data 
        cars_with_data <- own_area_supplement1 %>% copy() %>% 
          .[!COD_IMOVEL%in%cars_with_missing_self_intersection]  
        
        paste0("CARs with data for municipio: ", MUNICIPIO) %>% message_with_lines()
        cars_with_data %>% 
          summary() %>% 
          print()
        
        # 3) check to see if these CARs have connections at least
        car_vars_ordered2 %>% 
          copy() %>% 
          .[muni==MUNICIPIO & carid_reference%in%cars_with_missing_self_intersection] %>% 
          nrow() %>% 
          paste0("Number of rows with reference CARs w/o self-intersection: ", .) %>% 
          print()
        
        car_vars_ordered2 %>% 
          copy() %>% 
          .[muni==MUNICIPIO & carid_target%in%cars_with_missing_self_intersection] %>% 
          nrow() %>% 
          paste0("Number of rows with target CARs w/o self-intersection: ", .) %>% 
          print()
        
        # 4) Check the distribution of status -------
        paste0("Distribution of Status for reference CARs with missing self-int:") %>% message_with_lines()
        car_vars_ordered2 %>% 
          copy() %>% 
          .[carid_reference %in%cars_with_missing_self_intersection] %>% 
          .[, .N, SITUACAO_reference ] %>% print()
        
        paste0("Distribution of Status for target CARs with missing self-int:") %>% message_with_lines()
        car_vars_ordered2 %>% 
          copy() %>% 
          .[carid_target %in%cars_with_missing_self_intersection] %>% 
          .[, .N, SITUACAO_target ] %>% print()
        
        # 5) mean year of the CARs
        
        car_vars_ordered2 %>% 
          copy() %>% 
          .[carid_target %in%cars_with_missing_self_intersection] %>% 
          .[, median(year(data_inscricao_target)) ] %>% paste0("Median year for target CARs w/o self-int: ", .)
        
      }
      

      # section 7.4.2.3: quickly check to see 
      
      # municipio = 5107305  
      # municipio = 5107305  
      # municipio = 5107305  
      
    }

  }
  
  # section 7.2: load in all municipal CAR intersection data-sets ---------
  CONSOLIDATE_CAR_INTERSECTIONS_all_versions <- TRUE
  if(CONSOLIDATE_CAR_INTERSECTIONS_all_versions){
    
  setwd(dir_wd)
    
    # section 7.1: consolidate all CAR intersections -----
    # v4 ------
    paste0("data/processing/archive/CAROverlap_v4/") %>% setwd()
    
    csv_files <- list.files()
    
    v4 <- csv_files %>% .[1] %>% fread()
    for (FILE in csv_files[-1]) {
      message_with_lines(FILE)
      
      v4 <- fread(FILE) %>%
        .[, `data_inscricao.1` := as.IDate(`data_inscricao.1`)] %>%
        .[, data_inscricao := as.IDate(data_inscricao)] %>%
        rbind(car_vars, ., fill = T)
      
    }
    
    fwrite(v4,
           paste0(dir_wd, "data/cleaned/CAR_overlap_variables_v4.csv"))
    
    # v3 ------
    paste0(dir_wd, "data/processing/archive/CAROverlap_v3/") %>% setwd()
    
    csv_files <- list.files()
    
    car_vars <- csv_files %>% .[1] %>% fread()
    for (FILE in csv_files[-1]) {
      message_with_lines(FILE)
      
      car_vars <- fread(FILE) %>%
        .[, `data_inscricao.1` := as.IDate(`data_inscricao.1`)] %>%
        .[, data_inscricao := as.IDate(data_inscricao)] %>%
        rbind(car_vars, ., fill = T)
      
    }
    
    fwrite(car_vars,
           paste0(dir_wd, "data/cleaned/CAR_overlap_variables_v3.csv"))
    
    # v2 ------
    paste0(dir_wd, "data/processing/archive/CAROverlap_v2/") %>% setwd()
    
    csv_files <- list.files()
    
    car_vars <- csv_files %>% .[1] %>% fread()
    for (FILE in csv_files[-1]) {
      message_with_lines(FILE)
      
      car_vars <- fread(FILE) %>%
        .[, `data_inscricao.1` := as.IDate(`data_inscricao.1`)] %>%
        .[, data_inscricao := as.IDate(data_inscricao)] %>%
        rbind(car_vars, ., fill = T)
      
    }
    
    fwrite(car_vars,
           paste0(dir_wd, "data/cleaned/CAR_overlap_variables_v2.csv"))
    
    
    # v1 ------
    paste0(dir_wd, "data/processing/archive/CAROverlap_v1/") %>% setwd()
    
    csv_files <- list.files()
    
    car_vars <- csv_files %>% .[1] %>% fread()
    for (FILE in csv_files[-1]) {
      message_with_lines(FILE)
      
      car_vars <- fread(FILE) %>%
        .[, `data_inscricao.1` := as.IDate(`data_inscricao.1`)] %>%
        .[, data_inscricao := as.IDate(data_inscricao)] %>%
        rbind(car_vars, ., fill = T)
      
    }
    
    fwrite(car_vars,
           paste0(dir_wd, "data/cleaned/CAR_overlap_variables_v1.csv"))
    
    # load the data ------------
    v1 <- fread(paste0(dir_wd, "data/cleaned/CAR_overlap_variables_v1.csv")) %>% .[, cleaning_method := 'v1']
    v2 <- fread(paste0(dir_wd, "data/cleaned/CAR_overlap_variables_v2.csv")) %>% .[, cleaning_method := 'v2']
    v3 <- fread(paste0(dir_wd, "data/cleaned/CAR_overlap_variables_v3.csv")) %>% .[, cleaning_method := 'v3']
    v4 <- fread(paste0(dir_wd, "data/cleaned/CAR_overlap_variables_v4.csv")) %>% .[, cleaning_method := 'v4']
    final <- fread(paste0(dir_wd, "data/cleaned/CAR_overlap_variables_FINAL.csv")) 
    
    # join the final version with the previous versions to get complete map ----
    car_vars <- rbind(final, v4, fill=T) %>% 
      rbind(., v3, fill=T) %>% 
      rbind(., v2, fill=T) %>% 
      rbind(., v1, fill=T) %>%
      # only keep relevant years 
      .[year(data_inscricao.1)<2023& year(data_inscricao)<2023]
    
    car_vars %>% copy() %>% fwrite(paste0(dir_wd, "data/cleaned/CAR_overlap_variables_final.csv"))
    
    # get all intersections where COD_IMOVEL comes AFTER 
    car_vars_cod_after_cod1 <- car_vars %>% copy() %>% 
      .[data_inscricao > data_inscricao.1] %>% 
      .[,  GRP:= .GRP, .(COD_IMOVEL, COD_IMOVEL.1) ] %>% 
      .[order(cleaning_method)] %>% 
      .[!duplicated(GRP)] %>% .[, GRP:=NULL]
    
    car_vars_cod_after_cod1[, .N, cleaning_method] # check cleaning methods
    
    # equals
    car_vars_cod_equal_cod1 <- car_vars %>% copy() %>% 
      .[data_inscricao == data_inscricao.1] %>% 
      .[,  GRP:= .GRP, .(COD_IMOVEL, COD_IMOVEL.1) ] %>%
      .[order(cleaning_method)] %>% 
      .[!duplicated(GRP)] %>% .[, GRP:=NULL]
    
    car_vars_cod_equal_cod1[, .N, cleaning_method] # check cleaning methods
    
    # reference comes before
    car_vars_cod_before_cod1 <- car_vars %>% copy() %>% 
      .[data_inscricao < data_inscricao.1] %>% 
      .[,  GRP:= .GRP, .(COD_IMOVEL, COD_IMOVEL.1) ] %>%
      .[order(cleaning_method)] %>% 
      .[!duplicated(GRP)]  %>% .[, GRP:=NULL] %>% 
      rename_columns(c("COD_IMOVEL", "COD_IMOVEL.1","data_inscricao", "SITUACAO", "SITUACAO.1", "data_inscricao.1"), 
                     c("COD_IMOVEL.1", "COD_IMOVEL","data_inscricao.1", "SITUACAO.1", "SITUACAO", "data_inscricao"))
    
    # conflicts where the reference CAR is ALWYS the COD_IMOVEL
    car_vars_ordered <- rbind(car_vars_cod_after_cod1, car_vars_cod_equal_cod1) %>% 
      rbind(car_vars_cod_before_cod1) %>% 
      .[order(cleaning_method)] %>%
      .[,  GRP:= .GRP, .(COD_IMOVEL, COD_IMOVEL.1) ] %>%
      .[!duplicated(GRP)]  %>% .[, GRP:=NULL]
    
    car_vars_ordered %<>% 
      rename_columns(c("COD_IMOVEL", "COD_IMOVEL.1","data_inscricao", "SITUACAO", "SITUACAO.1", "data_inscricao.1"), 
                     c("carid_reference", "carid_target","data_inscricao_reference", "SITUACAO_reference", "SITUACAO_target", "data_inscricao_target"))
    
    fwrite(car_vars_ordered,
           paste0(dir_wd, "data/cleaned/CAR_overlap_variables_reftarget.csv"))
    
    
    car_vars_ordered %>% .[carid_reference!=carid_target] %>% 
      fwrite(., paste0(dir_wd, "data/cleaned/CAR_overlap_variables_conflicts.csv"))
    
    
    
    
    
    
    
    
    
    # section 7.3.: get the intersection area for cars that did not self intersect (Rough method using avaialble data) -----
    
    # data on all car intersections
    car_vars_ordered <- fread(
      paste0(dir_wd, "data/cleaned/CAR_overlap_variables_reftarget.csv"))
    
    # section 7.3.1: detect any missing CARs for further probing ------
    
    # two ways of checking 
    # rough 
    car_vars %>% 
      .[ , sum(COD_IMOVEL==COD_IMOVEL.1), COD_IMOVEL ] %>%
      .[, .N, V1] %>%
      rename_columns(c("V1", "N"), c("Self-Intersections per CAR", "Count")) %>%
      xtable() %>%
      print(., include.rownames = FALSE)
    
    # more precise
    car_vars_ordered %>% copy() %>%
      .[, sum(carid_reference == carid_target), carid_reference] %>%
      .[, .N, V1] %>%
      rename_columns(c("V1", "N"), c("Self-Intersections per CAR", "Count")) %>%
      xtable() %>%
      print(., include.rownames = FALSE)
    
    # section 7.3.2: retrieve own CAR area computation using sf method ---------------
    own_area <- car_vars_ordered %>%
      copy() %>%
      .[carid_reference == carid_target] %>%
      .[, .(NUM_AREA, carid_reference, int_area, data_inscricao_reference, SITUACAO_reference)] %>%
      .[!duplicated(carid_reference)] %>%
      rename_columns(
        c("int_area"),
        c("int_area_ref")
      )
    
    
    
    
    # section 7.4: correct missing self intersection data using official area data -----
    
    setwd(dir_wd)
    
    # section 7.4.0.1: load shapefile data
    sicar_area_imovel_combined <- fread( paste0(dir_wd,"data/cleaned/sicar_area_imovel_combined.csv"))
    
    # section 7.4.0.2: load microdata data
    microdata_cars <-
      fread("data/raw/sicar/microdata/temas_ambientais_update.csv") %>% 
      .[!duplicated(registro_car)] %>% .[year(data_inscricao )<2023] %>% 
      .[, registro_car]
    
    # section 7.4.0.3: get all of the CARs in the Amazon biome 
    sicar_area_imovel_combined %<>%
      copy() %>%
      .[, municipio := substr(COD_IMOVEL, 4,10)] %>%
      .[municipio%in%municipalities_amazon] %>%
      .[COD_IMOVEL%in%microdata_cars] 
      
    # section 7.4.1: identify the CARS that need updating
    codes_with_area <- own_area %>% .[, unique(carid_reference)]
    codes_all <- car_vars_ordered %>% .[, unique(carid_reference)] %>% append(microdata_cars) %>% unique()
    find_info <- codes_all %>% .[!(.%in% codes_with_area)]
    
    setwd(dir_wd)
    data.table(carid=find_info) %>% fwrite("data/processing/missing_car_self_int.csv")

    # create supplementaty data-set for own data
    own_area_supplement1 <- sicar_area_imovel_combined %>%
      copy() %>%
      .[COD_IMOVEL%in%find_info] %>%
      .[, .(NUM_AREA, COD_IMOVEL, SITUACAO)] 
    
    own_area_supplement2 <- car_vars_ordered %>%
      copy() %>%
      .[carid_reference%in%find_info] %>%
      .[!duplicated(carid_reference)] %>%
      .[, .(carid_reference, data_inscricao_reference)] 
    
    own_area <- merge(y=own_area_supplement1, x=own_area_supplement2, by.y = "COD_IMOVEL", by.x="carid_reference", all=T) %>%
      .[, supplemented := TRUE] %>%
      rbind(own_area, ., fill=T) %>%
      .[is.na(int_area_ref), int_area_ref := NUM_AREA*10000] %>%
      .[, .(carid_reference , int_area_ref)] %>% 
      .[, car_area_given_not_computed := TRUE]
    
    car_vars_ordered_out <- 
      merge(car_vars_ordered, own_area, by.x = "carid_reference", by.y = "carid_reference", allx=T, all.y=F) %>% 
      rename_columns(
        c("int_area_ref", "car_area_given_not_computed"),
        c("int_area_reference", "car_area_given_not_computed_ref")
      ) %>% 
      merge(., own_area, by.x = "carid_target", by.y = "carid_reference", allx=T, all.y=F) %>% 
      rename_columns(
        c("int_area_ref", "car_area_given_not_computed"),
        c("int_area_target", "car_area_given_not_computed_target")
      ) 

    fwrite(car_vars_ordered_out,
           paste0(dir_wd, "data/cleaned/CAR_overlap_variables_reftarget_areas.csv"))
    
    
    # section 7.4.2:  check to see if there are any patterns for the CARs and Munis with no self intersection -----
    if(fp$CHECKS){
      
      setwd(dir_wd)
      
      # section 7.4.2.1: identify the munis with missing data ------
      munis_with_missing_self_intersections<- find_info %>% 
        substr(4,10) %>% 
        unique()
      
      # section 7.4.2.2: check what share of the muni self intersects
      own_area2 <- own_area %>% copy() %>% .[, muni:= substr(carid_reference, 4,10)]
      car_vars_ordered2 <- car_vars_ordered %>% copy() %>% .[, muni:= substr(carid_reference, 4,10)]
      for(MUNICIPIO in munis_with_missing_self_intersections){
        
        message_with_lines(MUNICIPIO)
        
        reference_cars_with_self_intersection <- own_area2 %>% copy() %>% 
          .[muni==MUNICIPIO] %>% 
          .[, unique(carid_reference)]
        
        all_cars_in_muni <- car_vars_ordered2 %>% copy() %>% 
          .[muni==MUNICIPIO] %>% 
          .[, .(carid_target, carid_reference)] %>% 
          .[, index := 1:.N] %>%
          melt.data.table(id.vars = "index") %>% 
          .[, unique(value)]
        
        # 1) find the share of CARs with intersection  ----
        share_of_munis <- round(100*uniqueN(reference_cars_with_self_intersection)/uniqueN(all_cars_in_muni) )
        
        paste0("Share of CARs with self intersection ", share_of_munis, "% (", uniqueN(reference_cars_with_self_intersection), ").
           Municipio: ", MUNICIPIO) %>% message_with_lines()
        
        # 2) find the average, min, max, median area of the CARs with missing data 
        cars_with_missing_self_intersection <- 
          all_cars_in_muni %>% .[!.%in%reference_cars_with_self_intersection]
        
        # info on CARs with missing data 
        cars_with_missing_data <- own_area_supplement1 %>% copy() %>% 
          .[COD_IMOVEL%in%cars_with_missing_self_intersection]  
        
        paste0("CARs with missing data for municipio: ", MUNICIPIO) %>% message_with_lines()
        cars_with_missing_data %>% 
          summary() %>% 
          print()    
        
        # info on CARs without missing data 
        cars_with_data <- own_area_supplement1 %>% copy() %>% 
          .[!COD_IMOVEL%in%cars_with_missing_self_intersection]  
        
        paste0("CARs with data for municipio: ", MUNICIPIO) %>% message_with_lines()
        cars_with_data %>% 
          summary() %>% 
          print()
        
        # 3) check to see if these CARs have connections at least
        car_vars_ordered2 %>% 
          copy() %>% 
          .[muni==MUNICIPIO & carid_reference%in%cars_with_missing_self_intersection] %>% 
          nrow() %>% 
          paste0("Number of rows with reference CARs w/o self-intersection: ", .) %>% 
          print()
        
        car_vars_ordered2 %>% 
          copy() %>% 
          .[muni==MUNICIPIO & carid_target%in%cars_with_missing_self_intersection] %>% 
          nrow() %>% 
          paste0("Number of rows with target CARs w/o self-intersection: ", .) %>% 
          print()
        
        # 4) Check the distribution of status -------
        paste0("Distribution of Status for reference CARs with missing self-int:") %>% message_with_lines()
        car_vars_ordered2 %>% 
          copy() %>% 
          .[carid_reference %in%cars_with_missing_self_intersection] %>% 
          .[, .N, SITUACAO_reference ] %>% print()
        
        paste0("Distribution of Status for target CARs with missing self-int:") %>% message_with_lines()
        car_vars_ordered2 %>% 
          copy() %>% 
          .[carid_target %in%cars_with_missing_self_intersection] %>% 
          .[, .N, SITUACAO_target ] %>% print()
        
        # 5) mean year of the CARs
        
        car_vars_ordered2 %>% 
          copy() %>% 
          .[carid_target %in%cars_with_missing_self_intersection] %>% 
          .[, median(year(data_inscricao_target)) ] %>% paste0("Median year for target CARs w/o self-int: ", .)
        
      }
      
      
      
      # section 7.4.2.3: quickly check to see 
      
      # municipio = 5107305  
      # municipio = 5107305  
      # municipio = 5107305  
      
    }
    
    
    
    
  }
  
  # section 7.3 load the CARS 
  if(FALSE){
    
    
    cars_missing_self_intersection_or_any_intersection <- fread("data/processing/missing_car_self_int.csv") %>% 
      .[, municipio := substr(carid, 4,10)]
    
    # check to see which municipalities if any 
    cars_missing_self_intersection_or_any_intersection %>% 
      .[, .N, municipio] %>% 
      rename(missing_cars=N) %>% 
      .[order(missing_cars)] %>% 
      .[, .N, missing_cars] %>% 
      ggplot(., aes(x=missing_cars, y=N)) + 
      geom_point()
    
    
    cars_missing_self_intersection_or_any_intersection %>% 
      .[, .N, municipio] %>% 
      rename(missing_cars=N) %>% 
      .[order(missing_cars)] %>% 
      .[, .N, missing_cars]
    
    
    municipalities <- cars_missing_self_intersection_or_any_intersection %>% 
      .[, .N, municipio] %>% 
      .[order(-N)] %>% 
      .[N>10]
    
  
    
    
  }
  
}

# section 7.2: create examples for the documentation ----------------------------
if (fp$EXAMPLE) {
  
  # section 7.2.1: Create top line summary text ----------------
  SUMMARY_TEXT <- TRUE
  if (SUMMARY_TEXT) {
    # section 7.2.1.0: load in the data & separate into intersection and self intersection data-sets ------------
    car_vars <-
      fread(paste0(dir_wd, "data/cleaned/CAR_overlap_variables_robust.csv"))
    
    # section 7.2.1.1: retrieve own CAR area computation using sf method ---------------
    own_area <- car_vars %>%
      copy() %>%
      .[`COD_IMOVEL` == `COD_IMOVEL.1`] %>%
      .[, .(NUM_AREA, COD_IMOVEL, int_area, data_inscricao, SITUACAO)] %>%
      .[!duplicated(COD_IMOVEL)] %>%
      rename_columns(
        c("int_area", "data_inscricao", "SITUACAO"),
        c("int_area_ref", "data_inscricao_ref", "SITUACAO_ref")
      )
    
    # section 7.2.1.2: retrieve CAR intersections ------------
    intersection_area <- car_vars %>%
      copy() %>%
      .[`COD_IMOVEL` != `COD_IMOVEL.1`] %>%
      .[, GRP := .GRP, .(COD_IMOVEL, `COD_IMOVEL.1`)] %>%
      .[!duplicated(GRP)] %>%
      .[, .(
        COD_IMOVEL,
        `COD_IMOVEL.1`,
        int_area,
        data_inscricao,
        SITUACAO,
        SITUACAO.1,
        data_inscricao.1
      )] 
    
    # section 7.2.2: Construct statistics for the text -------------------
    
    # section 7.2.2.1: with both Canceled and non-Canceled CARs -------------------
    
    avg_intersections <- intersection_area %>%
      .[, .N, .(COD_IMOVEL)] %>%
      .[, mean(N)]
    
    avg_intersection_area <- intersection_area_perc %>% copy() %>%
      .[!is.na(int_area)] %>%
      .[!is.na(int_area_ref)] %>%
      .[, round(mean(int_area / int_area_ref) * 100, 2)]
    
    intersection_area_perc <- intersection_area %>%
      merge(
        x = .,
        y = own_area,
        by = "COD_IMOVEL",
        all.x = T,
        all.y = F
      )
    
    avg_intersection_union_area <-
      intersection_area_perc %>% copy() %>%
      .[, int_area_union := sum(int_area), COD_IMOVEL] %>%
      .[!duplicated(COD_IMOVEL)] %>%
      .[!is.na(int_area_union)] %>%
      .[!is.na(int_area_ref)] %>%
      .[, round(mean(int_area_union / int_area_ref) * 100, 2)]
    
    # section 7.2.2.2: with only non-Canceled CARs -------------------
    
    cancelled_cars <- car_vars %>%
      .[SITUACAO == "CA"] %>%
      .[!duplicated(COD_IMOVEL)] %>%
      nrow()
    
    avg_intersections2 <- intersection_area %>%
      .[SITUACAO != "CA"] %>%
      .[, .N, .(COD_IMOVEL)] %>%
      .[, mean(N)]
    
    intersection_area_perc2 <- intersection_area %>%
      .[SITUACAO != "CA"] %>%
      merge(
        x = .,
        y = own_area[SITUACAO_ref != "CA"] ,
        by = "COD_IMOVEL",
        all.x = T,
        all.y = F
      )
    
    avg_intersection_area2 <-
      intersection_area_perc2 %>% copy() %>%
      .[!is.na(int_area)] %>%
      .[!is.na(int_area_ref)] %>%
      
      .[, round(mean(int_area / int_area_ref) * 100, 2)]
    
    avg_intersection_union_area2 <-
      intersection_area_perc2 %>% copy() %>%
      .[, int_area_union := sum(int_area), COD_IMOVEL] %>%
      .[!duplicated(COD_IMOVEL)] %>%
      .[!is.na(int_area_union)] %>%
      .[!is.na(int_area_ref)] %>%
      .[, round(mean(int_area_union / int_area_ref) * 100, 2)]
    
    
    # section 7.2.2.3: with only Canceled CARs -------------------
    
    avg_intersections3 <- intersection_area %>%
      .[SITUACAO == "CA"] %>%
      .[, .N, .(COD_IMOVEL)] %>%
      .[, mean(N)]
    
    intersection_area_perc3 <- intersection_area %>%
      .[SITUACAO == "CA"] %>%
      merge(
        x = .,
        y = own_area[SITUACAO_ref == "CA"] ,
        by = "COD_IMOVEL",
        all.x = T,
        all.y = F
      )
    
    avg_intersection_area3 <-
      intersection_area_perc3 %>% copy() %>%
      .[!is.na(int_area)] %>%
      .[!is.na(int_area_ref)] %>%
      .[, round(mean(int_area / int_area_ref) * 100, 2)]
    
    avg_intersection_union_area3 <-
      intersection_area_perc3 %>% copy() %>%
      .[, int_area_union := sum(int_area), COD_IMOVEL] %>%
      .[!duplicated(COD_IMOVEL)] %>%
      .[!is.na(int_area_union)] %>%
      .[!is.na(int_area_ref)] %>%
      .[, round(mean(int_area_union / int_area_ref) * 100, 2)]
    
    
    cancelled_props2 <-
      intersection_area[SITUACAO == "CA"][!duplicated(COD_IMOVEL)][, COD_IMOVEL]
    
    # section 7.2.2.4: all CARs in amazon biome -------------------
    
    # get municipios in Amazon biome 
    # municipalities_amazon
    
    # get municipalities we have processed
    munis <- car_vars %>% copy() %>% 
      .[, municipio := substr(COD_IMOVEL, 4, 10)] %>% 
      .[, unique(municipio)]
    
    # get the raw data and only look at municipalities we processed
    sicar_area_imovel_combined <- fread(paste0(dir_wd, "data/cleaned/sicar_area_imovel_combined.csv")) %>% 
      .[, municipio := substr(COD_IMOVEL, 4, 10)] %>% 
      .[municipio%in%munis]
    
    # get the unique CARs which 
    unique_CARs_total <- sicar_area_imovel_combined %>% 
      .[, uniqueN(COD_IMOVEL)]
    
    
    # talk about pending and suspended CARs ----------------
    
    share <- function(x,y){return(round(100*x/y,0))}
    
    
    conflicts <- fread(paste0(dir_wd, "data/cleaned/CAR_overlap_variables_conflicts.csv"))
    
    
    # check that reference is ALWAYS after targe
    conflicts %>% nrow()
    conflicts %>% .[data_inscricao.1 <= data_inscricao] %>% nrow()
    
    # A = reference, b = TARGET
    # total conflictst (AB & BA)      
    total_conflicts <- conflicts_onordered %>% 
      .[, .GRP, .( COD_IMOVEL, COD_IMOVEL.1)] %>% 
      nrow()
    
    # compute conditional share of conflicts -----
    conditional_share <- function(situation, ref_procedes_target=conflicts){
      
      
      conflict_after_target <- ref_procedes_target %>% 
        .[SITUACAO==situation] %>% 
        .[, .GRP, .( COD_IMOVEL, COD_IMOVEL.1)] %>% 
        nrow()
      
      conflict_before_target <- ref_procedes_target %>% 
        .[SITUACAO.1==situation] %>% 
        .[, .GRP, .( COD_IMOVEL, COD_IMOVEL.1)] %>% 
        nrow()
      
      denom <- (conflict_after_target+conflict_before_target)
      share(x=conflict_after_target, y=denom) %>% 
        return()
      
      
    }
    
    # reference comes AFTER (BA)
    # total conflicts where reference comes AFTER (BA)
    total_conflicts_ref <- conflicts %>% 
      .[, .GRP, .( COD_IMOVEL, COD_IMOVEL.1)] %>% 
      nrow()
    
    cancelled_conflicts_ref <- conflicts[SITUACAO=="CA"] %>% nrow()
    active_conflicts_ref <- conflicts[SITUACAO=="AT"] %>% nrow()
    suspended_conflicts_ref <- conflicts[SITUACAO=="SU"] %>% nrow()
    pending_conflicts_ref <- conflicts[SITUACAO=="PE"] %>% nrow()
    
    
    # X.5: For each CAR type, identify the number of CARs that are born into conflict ---------
    
    car_vars_ordered <- fread(
      paste0(dir_wd, "data/cleaned/CAR_overlap_variables_reftarget.csv"))
    
    # X.5.1: 
    
    born_into_conflict <- function(sit, data=car_vars_ordered){
      
      # get the REFERENCE CARIDs (always comes after or equal to date)
      earliest_intersection <- data %>% copy() %>% 
        .[SITUACAO_reference==sit] %>% 
        # earliest target date
        .[, earliest_conflict_date := min(data_inscricao_target), carid_reference] %>% 
        .[earliest_conflict_date==data_inscricao_target]
      
      # check if there are double entries; which there should be by construction 
      earliest_intersection[, .N, carid_reference]
      
      # for those which intersect, only consider the conflicts
      cars_which_were_not_alone <- earliest_intersection[carid_target!=carid_reference][, unique(carid_reference)]
      
      # get all of the cars which equal themselves
      all_cancelled_cars <- earliest_intersection[carid_target==carid_reference][, unique(carid_reference)]
      
      # get the raio of the cars that intersected when they were born over all cars FOR THAT SITUACAO 
      length(cars_which_were_not_alone)/length(all_cancelled_cars) %>% 
        return()
      
      
    }
    
    paste0(
      "It is not uncommon for land claims to overlap in the Amazon\footnote{Of the ",
      uniqueN(municipalities_amazon), " municipalities in the Amazon biome, ",
      uniqueN(munis), " municipalities have been fully processed. ", 
      "Within these processed municipalities, we observe ",
      format_with_commas(unique_CARs_total), 
      " unique CARs in the raw data-files and successfully process ",
      format_with_commas(own_area[, uniqueN(COD_IMOVEL)]),
      " CARs. Additional work needs to be done to understand this slippage.}. Of the ",
      format_with_commas(own_area[, uniqueN(COD_IMOVEL)]),
      " CARs we have processed, (",
      round(100*(intersection_area[, uniqueN(COD_IMOVEL)]/own_area[, uniqueN(COD_IMOVEL)])), "%) ",
      format_with_commas(intersection_area[, uniqueN(COD_IMOVEL)]),
      " intersect with at least one other CAR. ",
      "On average, a CAR which interects with at least one other CAR, has ",
      round(avg_intersections, 2),
      " intersections, with each intersection composing roughly ",
      avg_intersection_area,
      "% of the total area of a CAR and, with the total intersection (including potential intersection overlaps) being ",
      avg_intersection_union_area,
      "%",
      
      "\footnote{These figures do not change much when we exclude cancelled CARs (since they are relatively few at ",
      format_with_commas(cancelled_cars),
      "), and are: ",
      round(avg_intersections2, 2),
      ", ",
      avg_intersection_area2,
      "%, ",
      avg_intersection_union_area2,
      "% respectively.}. ",
      
      "
        
        Cancelled CARs, on the other hand, have higher levels of intersections. ",
      "In our sample, we observe ", 
      format_with_commas(cancelled_cars), " cancelled CARs. Of these, ",
      round((
        length(cancelled_props2) / cancelled_cars
      ) * 100, 2), "% (",
      format_with_commas(length(cancelled_props2)), " unique CARs) have at least one intersection and " ,
      " intersect with other CARs ",
      round(avg_intersections3, 2),
      " times. ",
      
      
      "Each intersection is on average ",
      round(avg_intersection_area3, 2),
      "% of the cancelled CAR area and in total ",
      avg_intersection_union_area3,
      "% of the area, including intersection overlaps.
        
        ",
      "There does not appear to be any significant relationship which can be used to clearly explain the reason for a CAR cancellation. ", 
      "This holds both over time and relative to conflicting CARs. ",
      
      "Overall, there are: ", format_with_commas(total_conflicts_ref), " unique CAR conflicts in our data.",
      "\footnote{When computing conflicts for CARs A and B, we compute both the intersection of A on B and the intersection of B on A. ",
      "This is not the unique number of CAR conflicts, however, because it naturally double counts most conflicts. ",
      "In total, the raw number of CAR conflicts when we double count is: ",
      format_with_commas(total_conflicts), ". ",
      "To ensure we correctly identify all conflicts, we define the reference CAR as the CAR that proceded an original target CAR. ",
      "Once we restrict observations only to reference CARs, e.g. CARs that proceeded the target CARs, we are left with ", format_with_commas(total_conflicts_ref), 
      " unique conflicts.}. ", 
      
      "Of these, the share of CAR conflicts where a reference CAR is cancelled/pending/suspended/active is: ",
      share(x = cancelled_conflicts_ref, y = total_conflicts_ref) ,"%, ", share(x = pending_conflicts_ref, y = total_conflicts_ref),"%, ", share(x = suspended_conflicts_ref, y = total_conflicts_ref),"%, and ", share(x = active_conflicts_ref, y = total_conflicts_ref), 
      
      "%. For CARs which are eventually cancelled, ",  round(100*born_into_conflict(sit = "CA")), "% of the created CARs have at least one conflict. ",
      "This figure is similar for pending, suspended and active CARs: ", round(born_into_conflict(sit = "PE")*100), "%, ", 
      round(born_into_conflict(sit = "SU")*100), "%, ", round(born_into_conflict(sit = "AT")*100), "%."
      
    )
    
  }
  
  BACK_OF_THE_ENVELOPE <- TRUE
  if(BACK_OF_THE_ENVELOPE){
    
    # 1) compute the number of computations required ------
    car_vars_ordered <- fread(
      paste0(dir_wd, "data/cleaned/CAR_overlap_variables_reftarget.csv"))
    
    # unique cars per municipality   
    bote <- rbind(car_vars_ordered[, .(carid_reference)],
                  car_vars_ordered[, .(carid_reference)]) %>% 
      .[!duplicated(carid_reference)] %>% 
      .[, muni := substr(carid_reference, 4, 10)] 
    
    bote %>% 
      .[, uniqueN(carid_reference), muni] %>% 
      .[, sum(V1**2)] 
    
    
    
  }
  
  
  # section 7.4: compute the baseline probability that a newborn CAR is born into conflict by bucket 
  OVERLAP_BUCKET_FIGURE <- TRUE
  if(OVERLAP_BUCKET_FIGURE){
    
    
    car_vars_ordered <- fread(paste0(dir_wd, "data/cleaned/CAR_overlap_variables_reftarget_areas.csv"))
    
    
    sit="CA"
    born_into_conflict_by_largest_bucket <- function(sit, data=car_vars_ordered){
      
      
      # e.g. what is the probability a cancelled CAR is born into 100% overlap?
      
      # get the REFERENCE CARIDs (always comes after or equal to date)
      earliest_intersection <- data %>% copy() %>% 
        .[SITUACAO_reference==sit] %>% 
        # earliest target date
        .[, earliest_conflict_date := min(data_inscricao_target), carid_reference] %>% 
        .[earliest_conflict_date==data_inscricao_target]  
      
      
      # check if there are double entries; which there should be by construction 
      earliest_intersection[, .N, carid_reference]
      
      # for those which intersect, only consider the conflicts
      cars_which_were_not_alone <- earliest_intersection[carid_target!=carid_reference][, unique(carid_reference)]
      
      # GET the largest intersection for that CAR
      cars_which_were_not_alone_top <- earliest_intersection %>% copy() %>%  
        .[carid_reference%in%cars_which_were_not_alone] %>% 
        .[carid_target!=carid_reference] %>% 
        .[, largest_area := max(int_area), carid_reference] %>% 
        .[largest_area==int_area] %>% 
        .[, int_area_perc:= int_area/int_area_reference] %>% 
        .[, buckets := 33] %>% 
        .[, buckets := 33] %>% 
        .[int_area_perc>=.33, buckets := 66] %>% 
        .[int_area_perc>=.66, buckets := 99] %>% 
        .[int_area_perc>=.99, buckets := 100]  
      
      
      # get all of the cars which equal themselves
      all_cancelled_cars <- earliest_intersection[carid_target==carid_reference][, uniqueN(carid_reference)]
      
      # get the number of CARS per percentile bucket 
      cars_which_were_not_alone_top %>% 
        .[, uniqueN(carid_reference)/all_cancelled_cars, buckets] %>% 
        rename_columns(c("V1"), sit) %>% 
        return()
      
      
    }
    
    
    cancelleds <- born_into_conflict_by_largest_bucket(sit = "CA")
    actives <- born_into_conflict_by_largest_bucket(sit = "AT")
    pendings <- born_into_conflict_by_largest_bucket(sit = "PE")
    suspendeds <- born_into_conflict_by_largest_bucket(sit = "SU")
    
    merge(cancelleds, actives, "buckets") %>% 
      merge(., pendings, "buckets") %>% 
      merge(., suspendeds, "buckets") %>%
      xtable() %>% 
      print(. , include.rownames = FALSE)
    
    
  }
  
  
  # X.5: For each CAR type, identify the number of CARs that are born into conflict ---------
  
  
  # section 7.2.2: construct CAR overlap figures -----------
  OVERLAP_FIGURES <- TRUE
  if(OVERLAP_FIGURES){
    
    # section 7.2.2.1: load & polish data -------------
    
    message_with_lines("CHECK TO SEE WHERE THESE OBSERVATIONS ARE BEING DROPPED AND WHY")
    
    # section 7.2.2.2: retrieve CAR intersections ------------
    # data only for  conflicts, wwhere reference  (COD_IMOVEL) is always after target
    conflicts <- fread(paste0(dir_wd, "data/cleaned/CAR_overlap_variables_conflicts.csv"))
    
    
    # 
    # intersection_area <- car_vars %>%
    #   copy() %>%
    #   .[`COD_IMOVEL` != `COD_IMOVEL.1`] %>%
    #   .[, GRP := .GRP, .(COD_IMOVEL, `COD_IMOVEL.1`)] %>%
    #   .[!duplicated(GRP)] %>%
    #   .[, .(
    #     COD_IMOVEL,
    #     `COD_IMOVEL.1`,
    #     int_area,
    #     data_inscricao,
    #     SITUACAO,
    #     SITUACAO.1,
    #     data_inscricao.1
    #   )]
    
    # section 7.2.2.3: merge Intersection area with computed CAR area; only useful for intersection information ------------
    intersection_area_perc <-
      merge(
        x = intersection_area,
        y = own_area,
        by = "COD_IMOVEL",
        all.x = T,
        all.y = F
      )  %>%
      .[ COD_IMOVEL!=COD_IMOVEL.1 ] %>% 
      .[, year_ref := year(data_inscricao_ref)] %>%
      .[, year_target := year(data_inscricao.1)] %>%
      .[, overlap_perc := round(100 * int_area / int_area_ref, digits =2)] %>%
      .[, overlap_perc_bin :=  (overlap_perc<=33)*33 + (overlap_perc>33&overlap_perc<=66)*66 + (overlap_perc>66&overlap_perc<=99.99)*99 + (overlap_perc>66&overlap_perc==100)*100 ] %>%
      .[, cod_muni := as.numeric(substr(COD_IMOVEL, 4, 10))] 
    
    # IF the intersection area is bigger than the actual area of the CAR, something is wrong 
    if(fp$CHECKS){
      # check median difference
      intersection_area_perc %>% 
        copy() %>% 
        .[int_area>int_area_ref]  %>% 
        .[, median(int_area-int_area_ref, na.rm=T)]
      
      # check median difference
      intersection_area_perc %>% 
        copy() %>% 
        .[int_area>int_area_ref]  %>% 
        .[, max(int_area-int_area_ref)]
      
    }
    
    # if the difference is +-3% of the CARs area, then that's fine and adopt the larger area
    intersection_area_perc %<>% 
      # here, we assume that the overlap is the actual size of the CAR, in order to maintain consistency.
      .[, dif_area := (int_area -int_area_ref)/int_area] %>% 
      .[, dif_area := (int_area -int_area_ref)/int_area] %>% 
      .[dif_area<=0.03&dif_area>0, int_area_ref := int_area]
    
    # all clear, looks like it was just a very small issue 
    if(fp$CHECKS){
      # check median difference
      intersection_area_perc %>% 
        copy() %>% 
        .[int_area>int_area_ref]  %>% 
        .[, median(int_area-int_area_ref, na.rm=T)]
      
      # check median difference
      intersection_area_perc %>% 
        copy() %>% 
        .[int_area>int_area_ref]  %>% 
        .[, max(int_area-int_area_ref)]
      
    }
    
    # section 7.2.3: compute the number of CARs in each overlap bin per year, such that the CAR is newer than the cars it is intersecting ------
    
    # intersection area percentage for reference CARs with a start data AFTER the target CARs
    intp <-     intersection_area_perc   %>% copy() %>% 
      .[data_inscricao.1 <= data_inscricao_ref]
    
    
    # 7.2.3.1: For each municipality, what is the % of CARs that overlaps between (0-33], (33, 66], (66, 99], 100%  ? --------
    muni <- read_municipality() %>%
      st_transform(4674)
    
    # construct the data-set for plotting 
    for (YEAR in c(2015, 2017, 2018, 2020)) {
      
      message_with_lines(YEAR)
      
      # .1 restrict to all CARs this year or prior ----
      data <- intp %>% copy() %>%
        .[(year_ref <= YEAR)]
      
      # .2: create a figure illustrating the breaks -----
      data %>% copy() %>%
        .[, .N, .(overlap_perc, overlap_perc_bin)] %>% 
        ggplot(aes(x = overlap_perc, y = overlap_perc_bin, size = N)) + 
        geom_point() + 
        theme_bw() + 
        xlab("Overlap %") + 
        ylab("Overlap Bin") + 
        theme(
          axis.text = element_text(size=24),
          axis.title = element_text(size=24)
        )
      
      ggsave(
        plot = last_plot(),
        filename = paste0(
          "output/generating_documentation/share_of_intersected_cars_",
          YEAR,
          "_supp.pdf"
        ),
        device = cairo_pdf,
        width = 18,
        height = 9,
        scale = 1,
        dpi = 300
      )
      
      # .3 compute number of CARs per municipality within each Bucket -----
      
      plot_data <- data  %>% copy() %>%
        # some CARs that just touch were included and should be removed 
        .[overlap_perc_bin>0] %>% 
        .[, .N, .(overlap_perc_bin, cod_muni)]
      
      # expand to include zeros
      plot_data <- CJ(plot_data[, unique(overlap_perc_bin)],
                      plot_data[, unique(cod_muni)]) %>% 
        rename_columns(c("V1", "V2"), c("overlap_perc_bin", "cod_muni")) %>% 
        merge(plot_data, c("overlap_perc_bin", "cod_muni"), all=T) %>% 
        setnafill(cols=c("N"), fill=0) %>% 
        .[, totalN := sum(N), cod_muni] %>%
        # of the cars that have overlaps, what % are within each
        .[, percN := round(100 * N / totalN, 2)] %>% 
        .[, year := YEAR]
      
      if(YEAR == 2015){
        
        plot_data_out <- plot_data %>% copy()
        
      }else{
        
        plot_data_out <- plot_data %>% copy() %>% rbind(plot_data_out, .)
        
      }
      
      
    }
    
    
    # .4: amount of CAR claimed land --------
    
    plotting_microdata_output <- plot_data_out %>%
      dplyr::inner_join(muni, ., by = c("code_muni" = "cod_muni"))
    
    breaks <- c(0, 0.1, 0.5, 1, 2, 5, 10, 25, 50, 75, 90, 101)
    labels <- c(0, 0.1, 0.5, 1, 2, 5, 10, 25, 50, 75, 90)
    plotting_microdata_output$V1_cat <-
      cut(
        plotting_microdata_output$percN,
        breaks = breaks,
        labels = labels,
        include.lowest = TRUE
      )
    
    # create funciton to help with labelling the overlap bin
    label_overlap_bin <- function(x){
      
      # ifelse(x==33, "CAR intersection of (0,33]%", ifelse(x==66, "CAR intersection of (33,66]%",  ifelse(x==99, "CAR intersection of (66,99.9]%",  ifelse(x==100, "CAR intersection of 100%", "")))) %>% 
      #   return()
      
      ifelse(x==33, "(0,33]%", ifelse(x==66, "(33,66]%",  ifelse(x==99, "(66,99.9]%",  ifelse(x==100, "100%", "")))) %>% 
        return()
      
      
    }
    
    plotting_microdata_output %>% copy() %>%
      ggplot(data = .) +
      geom_sf(aes(fill = V1_cat), color = NA, size = .15) +
      facet_wrap(year ~ label_overlap_bin(overlap_perc_bin), ) +
      scale_fill_brewer(palette = "RdYlBu", direction = -1, "%") +
      theme_minimal() +
      theme(text = element_text(size = 20),
            # axis.text = element_text(size = 12),
            axis.text = element_blank())
    
    ggsave(
      plot = last_plot(),
      filename = paste0(
        "output/generating_documentation/share_of_intersected_cars",
        ".pdf"
      ),
      device = cairo_pdf,
      width = 8.8,
      height = 8.2,
      scale = 1,
      dpi = 300
    )
    
    
    
    # .5 construct the data-set for plotting ------
    for (YEAR in c(2014:2023)) {
      
      message_with_lines(YEAR)
      
      # .1 restrict to all CARs this year or prior ----
      data <- intp %>% copy() %>% .[overlap_perc_bin>0] %>% 
        .[(year_ref <= YEAR)]
      
      
      # Conditional on CAR being X,YorZ, did it have a conflict that was 20%/50%/90% of its area -----
      tmp <- data %>% copy() %>% 
        .[, .N, .(overlap_perc_bin, SITUACAO.1, SITUACAO)] %>% 
        .[, year := YEAR]
      
      
      
      
      if (YEAR == 2014) {
        data_out <- tmp %>% copy()
        
      } else{
        data_out <-
          tmp %>% copy() %>% rbind(., data_out)
        
        
      }
      
      
    }
    
    data_out %<>% .[overlap_perc_bin>0]
    
    breaks <- c(0,33, 66, 99, 100)
    labels <- c(0,33, 66, 99)
    data_out$V1_cat <-
      cut(
        data_out$overlap_perc_bin,
        breaks = breaks,
        labels = labels
      )
    
    
    # .6 create heat map which shows: CONDITIONAL on CAR being cancelled, Number of Cars in each bucket
    data_out %>% 
      .[SITUACAO=="CA"] %>%
      .[, sum(N), .(V1_cat, year) ] %>% 
      ggplot(aes(
        x = year,
        y = V1_cat,
        fill = V1,
        label = format_with_commas(V1)
      )) +
      geom_tile() +
      geom_text(col = "grey60", size=8) +
      ylab("N. of Overlaps | Reference CAR being Cancelled") +
      xlab("Year") +
      labs(fill="Unique Intersections") +
      scale_x_continuous(breaks = c(2014:2023)) +
      scale_y_discrete(breaks = c(33, 66, 99, 100)) +
      scale_fill_distiller() +
      theme_bw() +
      theme(text = element_text(size = 20),
            axis.text = element_text(size = 16), legend.position = "none")
    
    ggsave(
      plot = last_plot(),
      filename = paste0(
        "output/generating_documentation/n_overlaps_conditional_on_cancelled.pdf"
      ),
      device = cairo_pdf,
      width = 18,
      height = 9,
      scale = 1,
      dpi = 150
    )
    
    
    data_out %>% 
      .[SITUACAO=="PE"] %>%
      .[, sum(N), .(V1_cat, year) ] %>% 
      ggplot(aes(
        x = year,
        y = V1_cat,
        fill = V1,
        label = format_with_commas(V1)
      )) +
      geom_tile() +
      geom_text(col = "grey60", size=8) +
      ylab("N. of Overlaps | Reference CAR being Pending") +
      xlab("Year") +
      labs(fill="Unique Intersections") +
      scale_x_continuous(breaks = c(2014:2023)) +
      scale_y_discrete(breaks = c(33, 66, 99, 100)) +
      scale_fill_distiller() +
      theme_bw() +
      theme(text = element_text(size = 20),
            axis.text = element_text(size = 16), legend.position = "none")
    
    ggsave(
      plot = last_plot(),
      filename = paste0(
        "output/generating_documentation/n_overlaps_conditional_on_pending.pdf"
      ),
      device = cairo_pdf,
      width = 18,
      height = 9,
      scale = 1,
      dpi = 150
    )
    
    data_out %>% 
      .[SITUACAO=="AT"] %>%
      .[, sum(N), .(V1_cat, year) ] %>% 
      ggplot(aes(
        x = year,
        y = V1_cat,
        fill = V1,
        label = format_with_commas(V1)
      )) +
      geom_tile() +
      geom_text(col = "grey60", size=8) +
      ylab("N. of Overlaps | Reference CAR being Active") +
      xlab("Year") +
      labs(fill="Unique Intersections") +
      scale_x_continuous(breaks = c(2014:2023)) +
      scale_y_discrete(breaks = c(33, 66, 99, 100)) +
      scale_fill_distiller() +
      theme_bw() +
      theme(text = element_text(size = 20),
            axis.text = element_text(size = 16), legend.position = "none")
    
    ggsave(
      plot = last_plot(),
      filename = paste0(
        "output/generating_documentation/n_overlaps_conditional_on_active.pdf"
      ),
      device = cairo_pdf,
      width = 18,
      height = 9,
      scale = 1,
      dpi = 150
    )
    
    
    #   
    #   
    # binned_intersections %>%
    #   .[, .(year, overlap_perc_bin, `Intersection Ratio`)] %>%
    #   .[, `Intersection %` := round(`Intersection Ratio` * 100)] %>%
    #   # dcast.data.table(formula = year ~ overlap_perc_bin, value.var = "Intersection Ratio") %>%
    #   ggplot(aes(
    #     x = year,
    #     y = overlap_perc_bin,
    #     fill = `Intersection %`,
    #     label = `Intersection %`
    #   )) +
    #   geom_tile() +
    #   geom_text(col = "white") +
    #   ylab("% Intersection of Reference CAR") +
    #   xlab("Year") +
    #   labs("Intersections of Cancelled CARs/ Total Intersections") +
    #   scale_x_continuous(breaks = c(2014:2023)) +
    #   theme_bw() +
    #   theme(text = element_text(size = 20),
    #         axis.text = element_text(size = 16))
    # 
    # ggsave(
    #   plot = last_plot(),
    #   filename = paste0(
    #     "output/generating_documentation/intersection_buckets_cancelled_over_total.pdf"
    #   ),
    #   device = cairo_pdf,
    #   width = 18,
    #   height = 9,
    #   scale = 1,
    #   dpi = 150
    # )
    # 
    # 
    # binned_intersections %>%
    #   .[, .(year, overlap_perc_bin, `Area Ratio`)] %>%
    #   .[, `Area %` := round(`Area Ratio` * 100)] %>%
    #   ggplot(aes(
    #     x = year,
    #     y = overlap_perc_bin,
    #     fill = `Area %`,
    #     label = `Area %`
    #   )) +
    #   geom_tile() +
    #   geom_text(col = "white") +
    #   ylab("% Intersection of Reference CAR") +
    #   xlab("Year") +
    #   labs("Intersections of Cancelled CARs/ Total Intersections") +
    #   scale_x_continuous(breaks = c(2014:2023)) +
    #   theme_bw() +
    #   theme(text = element_text(size = 20),
    #         axis.text = element_text(size = 16))
    # 
    # ggsave(
    #   plot = last_plot(),
    #   filename = paste0(
    #     "output/generating_documentation/area_buckets_cancelled_over_total.pdf"
    #   ),
    #   device = cairo_pdf,
    #   width = 18,
    #   height = 9,
    #   scale = 1,
    #   dpi = 150
    # )
    # 
    # 
    
    # reference is cancelled
    for (YEAR in c(2014:2023)) {
      message_with_lines(YEAR)
      
      data <- intp %>% copy() %>%
        .[SITUACAO_ref == "CA"] %>%
        .[(year_og <= YEAR) & (yearB <= YEAR)]
      
      data[, .N, .(overlap_perc, overlap_perc_bin)] %>% ggplot(aes(x = overlap_perc, y =
                                                                     overlap_perc_bin, size = N)) + geom_point()
      
      ggsave(
        plot = last_plot(),
        filename = paste0(
          "output/generating_documentation/share_of_intersected_cars_",
          YEAR,
          "_cancelled_supp.pdf"
        ),
        device = cairo_pdf,
        width = 18,
        height = 9,
        scale = 1,
        dpi = 300
      )
      
      # compute number of CARs per municipality with 0-20% intersection -----
      
      plot_data <- data  %>% copy() %>%
        .[, .N, .(overlap_perc_bin, cod_muni)] %>%
        .[, totalN := sum(N), cod_muni] %>%
        # of the cars that have overlaps, what % are within each
        .[, percN := round(100 * N / totalN, 2)]
      
      
      # 5.2: amount of CAR claimed land --------
      
      plotting_microdata_output <- plot_data %>%
        dplyr::inner_join(muni, ., by = c("code_muni" = "cod_muni"))
      
      breaks <- c(0, 1, 2, 5, 10, 25, 50, 75, 90, 100, 101)
      labels <- c(0, 1, 2, 5, 10, 25, 50, 75, 90, 100)
      plotting_microdata_output$V1_cat <-
        cut(
          plotting_microdata_output$percN,
          breaks = breaks,
          labels = labels,
          include.lowest = TRUE
        )
      
      
      plotting_microdata_output %>% copy() %>%
        ggplot(data = .) +
        geom_sf(aes(fill = V1_cat), color = NA, size = .15) +
        facet_wrap( ~ paste(
          "CAR overlaps (",
          ifelse(overlap_perc_bin == 100, 99.99, overlap_perc_bin),
          ", ",
          ifelse(overlap_perc_bin + 10 > 100, 100, overlap_perc_bin + 10),
          "]%"
        ),) +
        scale_fill_brewer(palette = "RdYlBu", direction = -1, "%") +
        theme_minimal() +
        theme(text = element_text(size = 20),
              # axis.text = element_text(size = 12),
              axis.text = element_blank())
      
      ggsave(
        plot = last_plot(),
        filename = paste0(
          "output/generating_documentation/share_of_intersected_cars_",
          YEAR,
          "_cancelled.pdf"
        ),
        device = cairo_pdf,
        width = 18,
        height = 9,
        scale = 1,
        dpi = 300
      )
      
    }
    
    
    joint <- rbind(own_area, intersection_area_perc, fill = T) %>%
      .[, year_og := year(data_inscricao_ref)] %>%
      .[, yearB := year(data_inscricao.1)] %>%
      # .[(yearB<=year_og)|is.na(yearB)] %>%
      .[data_inscricao.1 <= data_inscricao_ref |
          is.na(data_inscricao.1)] %>%
      .[, GRP := .GRP, .(COD_IMOVEL, COD_IMOVEL.1, data_inscricao_ref)]
    
    # Table 1: Number of observations per bucket -------------
    
    totals <- joint %>%
      copy() %>%
      .[, .N, .(SITUACAO_ref, SITUACAO.1)] %>%
      .[SITUACAO_ref!="RE"] %>% 
      .[is.na(SITUACAO.1), SITUACAO.1 := "No Intersection"] %>% 
      rename_columns(c("SITUACAO_ref", "SITUACAO.1"),
                     c("Reference Status", "Target Status")) %>%
      dcast(formula = `Reference Status` ~ `Target Status`, value.var = "N") %>%
      setnafill(
        x = .,
        cols = c("No Intersection", "AT", "CA", "PE", "RE", "SU"),
        fill = 0
      )
    
    xtable(totals)
    
    # Table 2: Proportion of observations per bucket -------------
    
    proportions <- joint %>%
      copy() %>%
      .[, .N, .(SITUACAO_ref, SITUACAO.1)] %>%
      .[SITUACAO_ref!="RE"] %>% 
      .[is.na(SITUACAO.1), SITUACAO.1 := "No Intersection"] %>% 
      .[, sumN := sum(N)] %>%
      .[, prop := round(100 * N / sumN, 1)] %>%
      rename_columns(c("SITUACAO_ref", "SITUACAO.1"),
                     c("Reference Status", "Target Status")) %>%
      dcast(formula = `Reference Status` ~ `Target Status`, value.var = "prop") %>%
      setnafill(
        x = .,
        cols = c("No Intersection", "AT", "CA", "PE", "RE", "SU"),
        fill = 0
      )
    
    xtable(proportions)
    
    
    # Table 3: Proportion of CARs that intersect, with multiple conditions ------
    
    # A) Per year: total number of CARs
    total_n_cars_yearly <- joint %>%
      .[, uniqueN(COD_IMOVEL), year_og] %>%
      .[order(year_og)] %>%
      .[, V1 := format_with_commas(V1)] %>%
      rename_columns(c("V1"), c("N. CARs"))
    
    # B) Per year: total number of CARs with intersections
    total_n_cars_intersection_yearly <- joint %>%
      .[!is.na(COD_IMOVEL.1)] %>%
      .[, uniqueN(COD_IMOVEL), year_og] %>%
      .[order(year_og)] %>%
      .[, V1 := format_with_commas(V1)] %>%
      rename_columns(c("V1"), c("N. CARs: with intersection"))
    
    
    # D) Per year: total number of CARs with intersections from cancelled CARs
    total_n_cars_intersection_both_active_yearly <- joint %>%
      .[!is.na(COD_IMOVEL.1)] %>%
      .[SITUACAO.1 == "AT" & SITUACAO_ref == "AT"] %>%
      .[, uniqueN(COD_IMOVEL), year_og] %>%
      .[order(year_og)] %>%
      .[, V1 := format_with_commas(V1)] %>%
      rename_columns(c("V1"), c("N. CARs: both AT"))
    
    # D) Per year: total number of CARs with intersections from cancelled CARs
    total_n_cars_intersection_neither_cancelled_yearly <- joint %>%
      .[!is.na(COD_IMOVEL.1)] %>%
      .[SITUACAO.1 != "CA" & SITUACAO_ref != "CA"] %>%
      .[, uniqueN(COD_IMOVEL), year_og] %>%
      .[order(year_og)] %>%
      .[, V1 := format_with_commas(V1)] %>%
      rename_columns(c("V1"), c("N. CARs: Neither CA"))
    
    # D) Per year: total number of CARs with intersections from cancelled CARs
    total_n_cars_intersection_with_cancelled_yearly <- joint %>%
      .[!is.na(COD_IMOVEL.1)] %>%
      .[SITUACAO.1 == "CA"] %>%
      .[, uniqueN(COD_IMOVEL), year_og] %>%
      .[order(year_og)] %>%
      .[, V1 := format_with_commas(V1)] %>%
      rename_columns(c("V1"), c("N. CARs: Target is CA"))
    
    # E) Per year: total number of CARs with intersections from cancelled CARs
    total_n_cars_intersection_is_cancelled_yearly <- joint %>%
      # .[!is.na(COD_IMOVEL.1)] %>%
      .[SITUACAO_ref == "CA"] %>%
      .[, uniqueN(COD_IMOVEL), year_og] %>%
      .[order(year_og)] %>%
      .[, V1 := format_with_commas(V1)] %>%
      rename_columns(c("V1"), c("N. CARs: Reference is CA"))
    
    cars_and_intersections <-
      merge(
        total_n_cars_yearly,
        total_n_cars_intersection_yearly,
        by = "year_og",
        all = T
      ) %>%
      merge(
        .,
        total_n_cars_intersection_both_active_yearly,
        by = "year_og",
        all = T
      ) %>%
      merge(
        .,
        total_n_cars_intersection_neither_cancelled_yearly,
        by = "year_og",
        all = T
      ) %>%
      merge(
        .,
        total_n_cars_intersection_with_cancelled_yearly,
        by = "year_og",
        all = T
      ) %>%
      merge(
        .,
        total_n_cars_intersection_is_cancelled_yearly,
        by = "year_og",
        all = T
      ) %>%
      .[!is.na(year_og)]
    
    print(xtable(cars_and_intersections), include.rownames = FALSE)
    
  }
  
  
  # Table 4: look at yearly intersection %s --------------
  
  # number of CARs with intersection
  # number of CARs with 10% total intersection
  # number of CARs with 20% intersection
  
  # total area intersected by CARs: Both AT
  # total area intersected by CARs: Both CA
  
  
  
  
  # Table 2 -------------
  
  
  
  
  
  
}


# 1505486
# 1504703

# 1505064


# 1501907
