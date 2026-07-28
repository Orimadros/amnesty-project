# 
# 
# munis_chech <- fread("data/processing/identifying_errors/car_union_area_missing.csv") %>% 
#   .[, .N, .(municipio)] %>%  
#   .[, municipio]
# 
#  munis_drop <-  munis_chech %>% 
#    paste0("muni", .) 
#  
# 
# drop_these <- c()
# for(YEAR in c(2014:2023)){
#   
#   drop_these <- munis_drop %>% 
#     paste0(., "_", YEAR, ".csv") %>% 
#     append(drop_these, .)
#   
# }
# 
# drop_these <- munis_drop %>% 
#   paste0(., "_", "FULL", ".csv") %>% 
#   append(drop_these, .)
# 
# 
# drop_these_found <- "data/processing/SicarMuniOverlap/" %>% list.files() %>% 
#   .[.%in%drop_these] 
#   
# for(FILE in drop_these_found){
#   
#   message_with_lines(FILE)
#   
#   file.remove(FILE)
#   
#   
# }
# 
# 
# "data/processing/munis_already_claimed.csv" %>% 
#   fread() %>% 
#   .[!muni%in%munis_chech] %>% 
#   fwrite(., "data/processing/munis_already_claimed.csv")
# 
# 
# 
# 











# section 5B: correcting issues in the data ------



if(fp$INTERSECT_SFS_fixing_issues){
  
 
  # section 3B.1: correct CARs where the union is equal to zero ------- 
  FIX_CAR_UNION_AREAS_MISSING_MANUAL <- TRUE
  if(FIX_CAR_UNION_AREAS_MISSING_MANUAL){
 
    
    # 1200336     -----
    
    MUNICIPIO <- 1200336
    
    STATE <- states %>% .[which(.==substr(MUNICIPIO, 1,2))] %>% names()
    
    munis_and_years <- fread("data/processing/identifying_errors/car_union_area_missing.csv") %>% 
      .[, .(year, municipio)] %>% 
      .[municipio==MUNICIPIO]
    
    
    munis_and_years %>% copy() %>% 
      .[, i := 1] %>% 
      .[year<2023] %>% 
      dcast.data.table(formula = municipio ~year, value.var = "i") %>% 
      setnafill(x=., cols = paste0(c(2015:2022)), fill=0) %>% 
      .[order(`2022`, `2021`, `2020`, `2019`, `2018`, `2017`, `2016`, `2015`)]
    
    
    # first, start by plotting the municipality 
    
    plot_muni_with_missings(STATE_CODE = STATE, MUNICIPIO = MUNICIPIO, sicar_vars = sicar_vars )
    
    
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
    
    
    
    
    # 1300904      -----
    
    MUNICIPIO <- 1300904
    
    STATE <- states %>% .[which(.==substr(MUNICIPIO, 1,2))] %>% names()
    
    munis_and_years <- fread("data/processing/identifying_errors/car_union_area_missing.csv") %>% 
      .[, .(year, municipio)] %>% 
      .[municipio==MUNICIPIO]
    
    
    munis_and_years %>% copy() %>% 
      .[, i := 1] %>% 
      .[year<2023] %>% 
      dcast.data.table(formula = municipio ~year, value.var = "i") %>% 
      setnafill(x=., cols = paste0(c(2015:2022)), fill=0) %>% 
      .[order(`2022`, `2021`, `2020`, `2019`, `2018`, `2017`, `2016`, `2015`)]
    
    
    # first, start by plotting the municipality 
    
    plot_muni_with_missings(STATE_CODE = STATE, MUNICIPIO = MUNICIPIO, sicar_vars = sicar_vars )
    
    
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
  
    FIX_CAR_UNION_AREAS_MISSING_AUTO <- FALSE
  if(FIX_CAR_UNION_AREAS_MISSING_AUTO){
    
    # preliminaries ------------
    
    # get the updated micro-data  
    microdata <- fread("data/raw/sicar/microdata/temas_ambientais_update.csv")
    
    # load years  
    car_and_reg_year_wide <- fread("data/processing/car_and_reg_year_wide.csv") %>% 
      .[, municipio := as.numeric(substr(registro_car, 4, 10))] 
    
    # get all file names 
    property_directories <- list.dirs("data/raw/sicar/shapefiles/", recursive = T) %>% 
      .[stri_detect_fixed(., "AREA_IMOVEL")] %>% 
      .[order(.)] %>%
      data.table(path=.) %>% 
      .[, municipio := stri_extract_all_regex(path, "\\d{7}")] %>% 
      # subset to municipalities in the amazon
      .[municipio %in% municipalities_amazon, path] %>% 
      .[order(.)] 
    
    munis_and_years <- fread("data/processing/identifying_errors/car_union_area_missing.csv") %>% 
      .[, .(year, municipio)]
    
    munis_and_years %>% copy() %>% 
      .[, i := 1] %>% 
      .[year<2023] %>% 
      dcast.data.table(formula = municipio ~year, value.var = "i") %>% 
      setnafill(x=., cols = paste0(c(2015:2022)), fill=0) %>% 
      .[order(`2022`, `2021`, `2020`, `2019`, `2018`, `2017`, `2016`, `2015`)]
    
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
      paste0("Fixing: Municipio ", PROPERTY_SHP, " in year ", YEAR ) %>%  message_with_lines()
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
  
  
}



if(fp$EXAMPLE){
  
  setwd(dir_wd)
  
  sicar_vars <- fread("data/cleaned/sicar_overlap_variables.csv")
  
  # 5.1: prep for mapping --------
  library(geobr)
  library(classInt)
  library(ggplot2)
  
  muni <- read_municipality() %>% 
    st_transform(4674) 
  
  states_sf <- geobr::read_state() %>%
    .[which(.$abbrev_state%in%names(states)), ] %>% 
    st_transform(4674)  
  
  
  # 5.2: amount of CAR claimed land --------
  
  plotting_microdata_output <- sicar_vars %>% copy() %>% 
    # .[year==2014] %>% 
    .[, area_km2 := car_union_area/(1000**2) ] %>% 
    .[, .(municipio,  area_km2, year)] %>% 
    dplyr::inner_join(muni, ., by = c("code_muni" = "municipio") ) 
  
  # breaks_qt <- classIntervals(
  #   c(0, plotting_microdata_output$area_km2 ),
  #   style = "fixed",
  #   fixedBreaks=c(0, 10, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000))
  # plotting_microdata_output <- mutate(plotting_microdata_output, V1_cat = cut(area_km2, breaks_qt$brks)) 
  
  breaks <- c(0, 10, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000)
  labels <- format_with_commas(c(10, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000))
  plotting_microdata_output$V1_cat <- cut(plotting_microdata_output$area_km2, breaks=breaks, labels=labels, include.lowest=TRUE)
  
  
  plotting_microdata_output %>% 
    ggplot(data=.) +
    geom_sf(aes(fill=V1_cat), color= "grey50", size=.15) +
    facet_wrap(~year) + 
    #    labs(subtitle=bquote("Municipal CAR Area in " km^2)), size=8) +
    scale_fill_brewer(palette = "RdYlBu", direction=-1, bquote(km^2)) + 
    # scale_fill_distiller(palette = "Blues", name="Ratio") +
    theme_minimal() + 
    theme( 
      text = element_text(size = 20), 
      # axis.text = element_text(size = 12), 
      axis.text = element_blank()) + 
    geom_sf(data = states_sf, color = "black", fill = NA)
  
  
  ggsave(
    plot = last_plot(),
    filename = "output/generating_documentation/car_union_area_all_map.pdf",
    device = cairo_pdf,
    width = 18,
    height = 9,
    scale = 1, 
    dpi = 300
  )
  
  
  # 5.2.1: amount of CAR claimed land which intersects with indians --------
  
  plotting_microdata_output <- sicar_vars %>% copy() %>% 
    # .[year==2014] %>% 
    .[, area_km2 := car_area_intersect_indi/(1000**2) ] %>% 
    .[, .(municipio,  area_km2, year)] %>% 
    dplyr::inner_join(muni, ., by = c("code_muni" = "municipio") ) 
  
  breaks <- c(0, 10, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000)
  labels <- format_with_commas(c(10, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000))
  plotting_microdata_output$V1_cat <- cut(plotting_microdata_output$area_km2, breaks=breaks, labels=labels, include.lowest=TRUE)
  
  
  plotting_microdata_output %>% 
    ggplot(data=.) +
    geom_sf(aes(fill=V1_cat), color= NA, size=.15) +
    facet_wrap(~year) + 
    labs(subtitle="Intersection of Indigenous and Claimed CAR Area by municipality in km^2", size=8) +
    scale_fill_brewer(palette = "RdYlBu", direction-1, bquote(km^2)) + 
    # scale_fill_distiller(palette = "Blues", name="Ratio") +
    theme_minimal() + 
    theme( 
      text = element_text(size = 20), 
      # axis.text = element_text(size = 12),
      axis.text = element_blank()
    )
  
  ggsave(
    plot = last_plot(),
    filename = "output/generating_documentation/car_area_intersect_indi_all_map.pdf",
    device = cairo_pdf,
    width = 18,
    height = 9,
    scale = 1, 
    dpi = 300
  )
  
  
  # 5.2.2: percentage  of CAR claimed land which intersects with indians --------
  
  plotting_microdata_output <- sicar_vars %>% copy() %>% 
    .[car_area_intersect_indi<1, car_area_intersect_indi := 0 ] %>% 
    .[car_union_area<1, car_union_area :=1 ] %>% 
    .[, area_perc := round((100*car_area_intersect_indi)/car_union_area, digits=2) ] %>% 
    .[, .(municipio,  area_perc, year)] %>%
    dplyr::inner_join(muni, ., by = c("code_muni" = "municipio") ) 
  
  breaks <- c(-1,0,0.5, 1,2,5, 10, 25, 50, 75, 90, 100)
  labels <- c(0,0.5, 1,2,5, 10, 25, 50, 75, 90, 100)
  plotting_microdata_output$V1_cat <- cut(plotting_microdata_output$area_perc, breaks=breaks, labels=labels, include.lowest=TRUE)
  
  plotting_microdata_output %>% 
    ggplot(data=.) +
    geom_sf(aes(fill=V1_cat), color= NA, size=.15) +
    facet_wrap(~year) + 
    scale_fill_brewer(palette = "RdYlBu", direction = -1, "%") + 
    theme_minimal() + 
    theme( 
      text = element_text(size = 20), 
      axis.text = element_blank()
    )
  
  ggsave(
    plot = last_plot(),
    filename = "output/generating_documentation/car_area_intersect_indi_over_car_map.pdf",
    device = cairo_pdf,
    width = 18,
    height = 9,
    scale = 1, 
    dpi = 300
  )
  
  
  testing <-   plotting_microdata_output %>% copy() %>% 
    st_drop_geometry() %>% 
    .[, c( "area_perc", "V1_cat", "code_muni")]
  
  
  testing %>% 
    .[, .N,.( area_perc, V1_cat)] %>% 
    .[!is.na(area_perc)] %>% 
    .[area_perc<100] %>% 
    ggplot(aes(x=area_perc, y=V1_cat, size=N)) + 
    geom_point()
  
  
  
  testing %>% 
    .[area_perc>100] 
  
  plotting_microdata_output[which(plotting_microdata_output$area_perc>100),]
  
  1200203
  
  
  # 5.3.1: amount of CAR claimed land which intersects with conservation lands --------
  
  plotting_microdata_output <- sicar_vars %>% copy() %>% 
    # .[year==2014] %>% 
    .[, area_km2 := car_area_intersect_conserve/(1000**2) ] %>% 
    .[, .(municipio,  area_km2, year)] %>% 
    dplyr::inner_join(muni, ., by = c("code_muni" = "municipio") ) 
  
  
  breaks <- c(0, 10, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000)
  labels <- format_with_commas(c(10, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000))
  plotting_microdata_output$V1_cat <- cut(plotting_microdata_output$area_km2, breaks=breaks, labels=labels, include.lowest=TRUE)
  
  plotting_microdata_output %>% 
    ggplot(data=.) +
    geom_sf(aes(fill=V1_cat), color= NA, size=.15) +
    facet_wrap(~year) + 
    scale_fill_brewer(palette = "RdYlBu", direction=-1, bquote(km^2)) + 
    theme_minimal() + 
    theme( 
      text = element_text(size = 20), 
      axis.text = element_blank())
  
  ggsave(
    plot = last_plot(),
    filename = "output/generating_documentation/car_area_intersect_conserve_all_map.pdf",
    device = cairo_pdf,
    width = 18,
    height = 9,
    scale = 1, 
    dpi = 300
  )
  
  
  
  # 5.3.2: amount of CAR claimed land which intersects with conservation lands --------
  
  plotting_microdata_output <- sicar_vars %>% copy() %>% 
    .[car_union_area<1, car_union_area :=1 ] %>% 
    .[car_area_intersect_conserve<1, car_area_intersect_conserve := 0 ] %>% 
    .[, area_perc := round((100*car_area_intersect_conserve)/car_union_area, digits=2) ] %>% 
    .[, .(municipio,  area_perc, year)] %>% 
    dplyr::inner_join(muni, ., by = c("code_muni" = "municipio") ) 
  
  breaks <- c(0,1,2,5, 10, 25, 50, 75, 90, 100, 101)
  labels <- c(0,1,2,5, 10, 25, 50, 75, 90, 100)
  plotting_microdata_output$V1_cat <- cut(plotting_microdata_output$area_perc, breaks=breaks, labels=labels, include.lowest=TRUE)
  
  plotting_microdata_output %>% 
    ggplot(data=.) +
    geom_sf(aes(fill=V1_cat), color= NA, size=.15) +
    facet_wrap(~year) + 
    scale_fill_brewer(palette = "RdYlBu", direction=-1, bquote(km^2)) + 
    theme_minimal() + 
    theme( 
      text = element_text(size = 20), 
      axis.text = element_blank())
  
  ggsave(
    plot = last_plot(),
    filename = "output/generating_documentation/car_area_intersect_conserve_over_car_map.pdf",
    device = cairo_pdf,
    width = 18,
    height = 9,
    scale = 1, 
    dpi = 300
  )
  
  
  # 5.4.1: amount of CAR claimed land which intersects with conservation lands --------
  
  plotting_microdata_output <- sicar_vars %>% copy() %>% 
    # .[year==2014] %>% 
    .[, area_km2 := car_area_intersect_forest_total /(1000**2) ] %>% 
    .[, .(municipio,  area_km2, year)] %>% 
    dplyr::inner_join(muni, ., by = c("code_muni" = "municipio") ) 
  
  breaks <- c(0, 10, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000)
  labels <- format_with_commas(c(10, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000))
  plotting_microdata_output$V1_cat <- cut(plotting_microdata_output$area_km2, breaks=breaks, labels=labels, include.lowest=TRUE)
  
  plotting_microdata_output %>% 
    ggplot(data=.) +
    geom_sf(aes(fill=V1_cat), color= NA, size=.15) +
    facet_wrap(~year) + 
    scale_fill_brewer(palette = "RdYlBu", direction=-1, bquote(km^2)) + 
    theme_minimal() + 
    theme( 
      text = element_text(size = 20), 
      axis.text = element_blank())
  
  ggsave(
    plot = last_plot(),
    filename = "output/generating_documentation/car_area_intersect_forest_total_all_map.pdf",
    device = cairo_pdf,
    width = 18,
    height = 9,
    scale = 1, 
    dpi = 300
  )
  
  
  
  # 5.4.2: amount of CAR claimed land which intersects with conservation lands --------
  
  # quick correction 
  sicar_vars %<>% copy() %>% 
    .[, max_car := max(car_area_intersect_indi, car_area_intersect_conserve, car_area_intersect_forest_total, car_area_intersect_forestA, car_area_intersect_forestB, car_area_intersect_forestC)] %>% 
    .[car_union_area<max_car,flag:=1] %>% 
    .[car_union_area<max_car,car_union_area:=max_car]
  
  plotting_microdata_output <- sicar_vars %>% copy() %>% 
    .[car_union_area<1, car_union_area :=1 ] %>% 
    .[car_area_intersect_forest_total<1, car_area_intersect_forest_total := 0 ] %>% 
    .[, area_perc := round((100*car_area_intersect_forest_total)/car_union_area, digits=2) ] %>% 
    .[, .(municipio,  area_perc, year)] %>% 
    dplyr::inner_join(muni, ., by = c("code_muni" = "municipio") ) 
  
  breaks <- c(0,1,2,5, 10, 25, 50, 75, 90, 100, 101)
  labels <- c(0,1,2,5, 10, 25, 50, 75, 90, 100)
  plotting_microdata_output$V1_cat <- cut(plotting_microdata_output$area_perc, breaks=breaks, labels=labels, include.lowest=TRUE)
  
  plotting_microdata_output %>% 
    ggplot(data=.) +
    geom_sf(aes(fill=V1_cat), color= NA, size=.15) +
    facet_wrap(~year) + 
    scale_fill_brewer(palette = "RdYlBu", direction=-1, bquote(km^2)) + 
    theme_minimal() + 
    theme( 
      text = element_text(size = 20), 
      axis.text = element_blank())
  
  ggsave(
    plot = last_plot(),
    filename = "output/generating_documentation/car_area_intersect_forest_over_car_map.pdf",
    device = cairo_pdf,
    width = 18,
    height = 9,
    scale = 1, 
    dpi = 300
  )
  
  
}
