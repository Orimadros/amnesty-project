

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
  .[N>5]

# Address these points specifically -------------------------------

clean_conflicting_cars_version5 <-
  function(MUNICIPALITY){
  
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
    
    # section 6.1: while there is still an available municipality------
    while (length(available_munis) > 0) {
      
      # section 6.1.1: identify and select the muni we will work with in this iteration ------
      available_munis <- MUNICIPALITY
      
      if(length(available_munis)<1){break}
      
      # section 6.1.2: check which municipalities are available (useful for manual attempts) ------
      order_by_size <- microdata %>%
        .[codigo_ibge %in% available_munis] %>%
        .[, .N, codigo_ibge] %>%
        .[order(-N)]  
      
      THIS_muni <- order_by_size[1]   
  
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