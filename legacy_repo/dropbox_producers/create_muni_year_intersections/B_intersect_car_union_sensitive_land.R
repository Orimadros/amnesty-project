# section 3: load CAR, clean CAR data, intersect with the sf's above & compute areas for each MUNI & YEAR ------ 

if(fp$INTERSECT_SFS){
  
  # year municipio
  # 6: 2021   2112852
  # 
  #  1500404 2112852
  
  # section 3.0: set-up -------
  
  BATCH <- paste0("CODE", round(runif(n = 1)*10000))
  # BATCH <- "AMA5"
  log_open(paste0("data/logs/log_create_SicarMuniOverlap_variables_BATCH",BATCH,".txt"))
  
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
  
  # do an update-as-you-go framework -----------------
  
  # define funciton to identify munis that need to be cleaned
  get_munis_that_need_to_be_cleaned <- function(dir=dir_wd){
    
    setwd(dir)
    
    # identify all municipalities that will be cleaned 
    all_munis <- property_directories %>% 
      stri_extract_all_regex(`.`, "\\d{7}") %>% 
      unlist
    
    # BEFORE identifying the files for this batch, identify which files have already been run and remove them from the list
    munis_already_completed <-
      paste0("data/processing/SicarMuniOverlap/") %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    # identify municipalities that are still available 
    munis_that_still_need_to_be_cleaned <- all_munis %>% .[!(.%in%munis_already_completed)]
    
    munis_that_still_need_to_be_cleaned %>% 
      return()
    
  }
  
  # define function that selects and claims municipalities
  select_muni_to_clean <- function(
    dir=dir_wd,
    available_munis=available_munis){
    
    setwd(dir = dir)
    
    FILE <- "data/processing/munis_already_claimed.csv"
    
    if (!file.exists(FILE)) {
      data.table(muni = c("")) %>%
        .[0] %>%
        fwrite(., FILE)
      
    }
    
    munis_already_completed <-
      paste0("data/processing/SicarMuniOverlap/") %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    
    FILE <- "data/processing/munis_already_claimed.csv"
    
    if(!file.exists(FILE)){
      
      data.table(muni=c("")) %>% 
        fwrite(., FILE)
      
    }
    
    already_claimed <- fread(FILE)
    
    fread(FILE)
    
    this_muni <- available_munis %>% 
      # make sure they are still available
      .[!.%in%munis_already_completed] %>% 
      .[!.%in%already_claimed] %>% 
      .[sample(length(.))] %>% 
      .[1]
    
    data.table(muni=this_muni) %>% 
      fwrite(FILE, append = T)
    
    return(this_muni)
    
  }
  
  # identify claimed municipalities (may or may not appear in the `get_munis_that_need...` output)
  get_munis_that_were_claimed_or_cleaned <- function(dir=dir_wd){
    
    setwd(dir)
    
    FILE <- "data/processing/munis_already_claimed.csv"
    
    if(!file.exists(FILE)){
      
      data.table(muni=c("")) %>% 
        fwrite(., FILE)
      
    }
    
    munis1 <- fread(FILE) %>% 
      .[, muni] 
    
    munis2 <- get_munis_that_need_to_be_cleaned(dir = dir)
    
    munis2 %>% .[!.%in%munis1] %>% 
      .[!duplicated(.)] %>% 
      return()
    
  }
  
  # identify already cleaned munis
  
  available_munis <- get_munis_that_need_to_be_cleaned()
  
  # 3.1: for each municipality ------
  
  while(length(available_munis)>0){
    
    # select municipality  
    available_munis <- get_munis_that_need_to_be_cleaned()
    THIS_muni <- select_muni_to_clean(dir = dir_wd, available_munis=available_munis)
    
    
    # data/raw/sicar/shapefiles//RO/SHAPE_1101203/AREA_IMOVEL -- 2015
    # THIS_muni <-   1506005 #1506005 #, 1500404
    # THIS_muni <-  2112852
    
    # select property directoy 
    
    property_directories_RUNBATCH <- property_directories %>% 
      .[!stri_detect_fixed(., "AREA_IMOVEL_")] %>% 
      .[stri_detect_fixed(., THIS_muni)] %>% 
      .[order(.)] %>% 
      .[!duplicated(.)] %>% 
      c()
    i<- 1
    for(i in 1:length(property_directories_RUNBATCH)){
      
      gc()
      
      # get the file path to the shape file & municipality name 
      PROPERTY_SHP <- property_directories_RUNBATCH[i]
      PROPERTY_MUNI <- stri_extract_all_regex(PROPERTY_SHP, "\\d{7}") %>% unlist()
      PROPERTY_UF <- PROPERTY_MUNI %>% substr(., 1,2)
      
      # load forest data 
      forests <- load_forrest_data(PROPERTY_UF) %>% 
        mutate(typeA = startsWith(codigo, "FPA")*1) %>% 
        mutate(typeB = startsWith(codigo, "FPB")*1) %>% 
        mutate(typeC = startsWith(codigo, "FPC")*1)  %>% 
        st_make_valid() %>% 
        mutate(is_valid = st_is_valid(.))
      
      
      forests <- forests[which(forests$is_valid==TRUE),]
      
      
      # status update
      message_with_lines(PROPERTY_SHP)
      paste0(length(available_munis), " municipalities left.") %>% message_with_lines()
      
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
        .[, FULL := 1] %>% 
        .[municipio == PROPERTY_MUNI] %>% 
        select(-c("municipio")) %>% 
        melt.data.table(id.vars = c("registro_car")) %>% 
        rename_columns(c("variable"),c("year")) %>% 
        .[, year := stri_replace_all_fixed(year, "y", "")]
      
      # 3.4: for each year -----------
      years <- c(2014:2022) %>% as.character() %>% append(c("FULL")) 
      for(YEAR in years){
        
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
          
          if(length(car_area_cancelled)>1){
            car_area_cancelled <- max(car_area_cancelled)
          }
          
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
          
          if(length(car_area_notcancelled)>1){
            car_area_notcancelled <- max(car_area_notcancelled)
          }
          
          # car_area_intersections <- st_intersection(car_cancelled, car_notcancelled) %>% 
          #   st_make_valid() %>% 
          #   st_area() %>% 
          #   sum()
          
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
          
          
          if(YEAR == 2022){
            sf::write_sf(car, paste0(dir_wd, "data/processing/CleanShps/muni", THIS_muni,  ".shp"))
          }
          
          
        }
        
      }
      
      
      gc()
      
    }
    
    
    not_available_munis <- get_munis_that_were_claimed_or_cleaned()
    # update the available municipalities 
    available_munis <- get_munis_that_need_to_be_cleaned()
    
    available_munis %>% .[!.%in%not_available_munis] 
    
  }
  
  
  log_close()
  
}

# section 4: create examples for documentation -------------

if(fp$EXAMPLE){
  
  # use this example municipality that has a clear example of irregular properties 
  PROPERTY_SHP <- property_directories %>%  .[stri_detect_fixed(., "1303536")]
  
  message_with_lines(PROPERTY_SHP)
  
  PROPERTY_MUNI <- stri_extract_all_regex(PROPERTY_SHP, "\\d{7}") %>% unlist()
  
  # 2.0: subset municipality of interest in the micro-dataset -----
  microdata_muni <- microdata %>%
    # relevant variables
    .[, .(
      registro_car,
      codigo_ibge,
      data_inscricao,
      situacao_cadastro,
      cancelled
    )] %>%
    # subset data-set
    .[codigo_ibge == PROPERTY_MUNI] 
  
  # 2.1: load property data by municipio -----
  car <- PROPERTY_SHP  %>% 
    read_sf() %>% 
    st_transform(4674)  # Change 4674 crs
  
  # 3: apply adapted CAR cleaning code, following this procedure ------
  
  # 3.1: validate each polygon, drop the ones which aren't valid or have been cancelled
  car$valid <- st_is_valid(car) %>% as.numeric() ## got this error message, despite the function working: 1: In st_is_longlat(x) :bounding box has potentially an invalid value range for longlat data
  
  # 1) PLOTTING INVALID CAR EXAMPLE -------------------------
  
  # first, show valid & invalid
  car %>% copy() %>% 
    .[, "valid"] %>% 
    # .[which(.$valid == 0), "valid"] %>% 
    plot(.,  pal = terrain.colors(2)  )   #rainbow(2))   heat.colors(2) terrain.colors(2) topo.colors(2)  cm.colors(2) 
  
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1303536_invalid_example1.pdf'), width=20, height=8)
  
  # now, show only invalid
  car %>% copy() %>% 
    .[which(.$valid == 0), "valid"] %>% 
    plot(., pal=  terrain.colors(2) ) 
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1303536_invalid_example2.pdf'), width=20, height=8)
  
  # now, show only invalid made corrected  
  car2 <- car %>% copy() %>% 
    .[which(.$valid == 0), ]  
  
  car2$valid2 <- car2 %>% copy() %>% 
    st_make_valid() %>% 
    st_is_valid() %>% 
    as.numeric()
  
  # car %>% copy() %>% 
  #   .[which(.$valid == 0), "valid"] %>% 
  #   plot(.,  pal=  terrain.colors(2) ) 
  # 
  
  car2 %>% 
    .[which(.$valid2 == 1), "valid"]  %>% 
    plot(.,  pal=  terrain.colors(2) )
  
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1303536_invalid_example3.pdf'), width=20, height=8)
  
  # 2) PLOTTING INDIGENOUS OVERLAP EXAMPLE -------------------------
  # a property that has been cancelled, a property that intersects native lands, a property tha
  
  car %<>% copy() %>% 
    st_make_valid()
  
  car_union <- car %>% 
    st_union()  
  
  intersect_pct <- st_intersection(car_union, indigenous)
  
  plot(car$geometry)
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1303536_indigenous_example1.pdf'), width=6, height=6)
  plot(car_union)
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1303536_indigenous_example2.pdf'), width=6, height=6)
  plot(indigenous$geometry, add=T )
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1303536_indigenous_example3.pdf'), width=6, height=6)
  plot(intersect_pct, add=T, col = "red")
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1303536_indigenous_example4.pdf'), width=6, height=6)
  
  
  # 2) PLOTTING CANCELLED CAR OVERLAP EXAMPLE --- perfect overlap -------------------------
  
  # 2.1) illustrate intersection examples -------
  microdata_muni <- microdata %>%
    # relevant variables
    .[, .(
      registro_car,
      codigo_ibge,
      data_inscricao,
      situacao_cadastro,
      cancelled
    )] %>% 
    # subset data-set
    .[codigo_ibge == 1200013]  #### NOTICE, cancelled cars don't have muni codes, yet
  
  PROPERTY_SHP <- property_directories %>%  .[stri_detect_fixed(., "1200013")]
  
  car <- PROPERTY_SHP  %>% 
    read_sf() %>% 
    st_transform(4674) %>% 
    st_make_valid() 
  
  # identify a cancelled CAR
  microdata_muni[!is.na(cancelled)]
  
  
  check_intersects <- st_intersects(car[car$COD_IMOVEL=="AC-1200013-7F9077EED34645E382BEB48DACF66206", ], car)
  #   Sparse geometry binary predicate list of length 3370, where the predicate was `intersects'
  # first 10 elements:
  # 6, 598, 1162, 1963, 2238
  
  these <- car[c(6, 598, 1162, 1963, 2238), ] 
  
  microdata[registro_car%in%these$COD_IMOVEL][, .(registro_car, data_inscricao, cancelled)] %>% .[order(data_inscricao)]
  
  
  
  
  
  # "AC-1200013-7F9077EED34645E382BEB48DACF66206"   2014-05-30  <- "fist" & Cancelled
  # "AC-1200013-47340A0E7BFD43D999869111D3C47A6B"   2014-08-27  <- "ACTIVE"
  
  # "AC-1200013-C9AA224F678844A389563238E0DAB987"     2015-02-04 <- "PE"
  # "AC-1200013-3FA22235433E4725969D1C3AEF755777"     2016-04-28 <- "Ativo"
  
  # "AC-1200013-78B561B83CBA4F0AB0A2AF4563D617F5"      2018-07-13 <- "Cancelled"
  
  
  
  
  ### 2014 
  first_and_cancelled <- these[which(these$COD_IMOVEL%in%c("AC-1200013-7F9077EED34645E382BEB48DACF66206")) ,  "geometry"]
  
  first_and_cancelled %>% 
    plot(axes = TRUE)
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1200013_cancelled_overlap_example1.pdf'))
  
  
  first_two <- these[which(these$COD_IMOVEL%in%c("AC-1200013-7F9077EED34645E382BEB48DACF66206", "AC-1200013-47340A0E7BFD43D999869111D3C47A6B")) ,  "geometry"]
  first_two_int <-  st_intersection(first_two[1,], first_two[2,])
  
  #### 2014 - combine both
  first_two %>% plot(axes = TRUE)
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1200013_cancelled_overlap_example2.pdf'))
  
  #### 2014 - combine both - show intersection   
  first_two_int[, 'geometry'] %>% plot(axes = TRUE, add=T, col="red")
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1200013_cancelled_overlap_example3.pdf'))
  
  #### 2014 - combine both - show intersection   
  first_two_situ <- these[which(these$COD_IMOVEL%in%c("AC-1200013-7F9077EED34645E382BEB48DACF66206", "AC-1200013-47340A0E7BFD43D999869111D3C47A6B")) ,  "SITUACAO"]
  first_two_situ %>% plot(axes = TRUE)
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1200013_cancelled_overlap_example4.pdf'))
  
  
  #### 2015
  first_three <- these[which(these$COD_IMOVEL%in%c("AC-1200013-7F9077EED34645E382BEB48DACF66206", "AC-1200013-47340A0E7BFD43D999869111D3C47A6B", "AC-1200013-C9AA224F678844A389563238E0DAB987")) ,  "geometry"]
  first_three %>% plot(axes = TRUE)
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1200013_cancelled_overlap_example5.pdf'))
  
  
  #### 2016
  first_four <- these[which(these$COD_IMOVEL%in%c("AC-1200013-7F9077EED34645E382BEB48DACF66206", "AC-1200013-47340A0E7BFD43D999869111D3C47A6B", "AC-1200013-C9AA224F678844A389563238E0DAB987", "AC-1200013-3FA22235433E4725969D1C3AEF755777")) ,  "geometry"]
  first_four %>% plot(axes = TRUE)
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1200013_cancelled_overlap_example6.pdf'))
  
  
  #### 2018
  # these[ ,  "geometry"] %>% plot(axes = TRUE)
  # dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1200013_cancelled_overlap_example7.pdf'))
  # 
  #### 2018
  last_two <-these[which(these$COD_IMOVEL%in%c("AC-1200013-3FA22235433E4725969D1C3AEF755777", "AC-1200013-78B561B83CBA4F0AB0A2AF4563D617F5")) ,  "geometry"] 
  last_two_int <-  st_intersection(last_two[1,], last_two[2,])
  last_two_int %>% plot(axes = TRUE, col="red", add=T)
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1200013_cancelled_overlap_example7.pdf'))
  
  
  # 3) PLOTTING summary statistics about the cancelled CARs -------------------------
  
  # 3.1) distribution of cancelled CARs  --------
  muni_cancelled_cars <- microdata %>%
    # relevant variables
    .[, .(
      registro_car,
      codigo_ibge,
      data_inscricao,
      situacao_cadastro,
      cancelled
    )] %>%
    # number of total unique CARs by municipality
    .[, unique_CARs := uniqueN(registro_car), .(codigo_ibge, cancelled)] %>% 
    .[, .GRP, .(codigo_ibge, cancelled, unique_CARs)] %>% 
    .[, .(codigo_ibge, cancelled, unique_CARs)] %>% 
    .[, total_cars := sum(unique_CARs), codigo_ibge] %>% 
    .[, perc := round(100*unique_CARs/total_cars)]
  
  
  # For text: 
  
  muni_cancelled_cars %>% 
    .[cancelled==TRUE] %>% 
    .[order(-unique_CARs )] %>% 
    ggplot(data = ., aes(x=total_cars, y = unique_CARs, label = codigo_ibge, size = perc )) + 
    geom_text() + 
    geom_abline(slope = .1, linetype="dashed") + 
    theme_bw() + 
    xlab("Total CARs") + 
    ylab("Cancelled CARs") + 
    labs(size="%") + 
    theme(text=element_text(20))
  
  ggsave(
    plot = last_plot(),
    filename = "output/generating_documentation/cancelled_cars_rel_by_muni.pdf",
    device = cairo_pdf,
    width = 12,
    height = 8,
    scale = 1
  )
  
  
  
  # 3.1.0: prep for mapping --------
  library(geobr)
  library(classInt)
  
  muni <- read_municipality() %>% 
    st_transform(4674) 
  
  # 3.1.1: NUMBER of cancelled CARs --------
  plotting_microdata_output <- muni_cancelled_cars %>% copy() %>% 
    dcast.data.table(formula = codigo_ibge +  total_cars ~ cancelled, value.var = "unique_CARs") %>% 
    melt.data.table(id.vars = c("codigo_ibge","total_cars")) %>% 
    rename_columns(c("variable", "value"), c("cancelled", "unique_CARs")) %>% 
    .[cancelled==TRUE] %>% 
    setnafill(x=., cols = c("unique_CARs"), fill=0) %>% 
    dplyr::inner_join(muni, ., by = c("code_muni" = "codigo_ibge") ) 
  
  breaks_qt <- classIntervals(c(min(plotting_microdata_output$unique_CARs ) - .0000001, plotting_microdata_output$unique_CARs ), n = 10, style = "quantile")
  
  plotting_microdata_output <- mutate(plotting_microdata_output, V1_cat = cut(unique_CARs, breaks_qt$brks)) 
  
  plotting_microdata_output %>% 
    ggplot(data=.) +
    geom_sf(aes(fill=V1_cat), color= NA, size=.15) +
    labs(subtitle="Number of Cancelled CARs", size=8) +
    scale_fill_brewer(palette = "RdYlBu") + 
    # scale_fill_distiller(palette = "Blues", name="Ratio") +
    theme_minimal() + 
    theme(legend.title = element_blank(), 
          text = element_text(size = 20))
  
  ggsave(
    plot = last_plot(),
    filename = "output/generating_documentation/cancelled_cars_number_by_muni_map.pdf",
    device = cairo_pdf,
    width = 18,
    height = 9,
    scale = 1
  )
  
  
  # 3.1.2: NUMBER of cancelled CARs --------
  plotting_microdata_output <- muni_cancelled_cars %>% copy() %>% 
    .[is.na(cancelled)] %>% 
    .[, perc := 100-perc] %>% 
    setnafill(x=., cols = c("perc"), fill=0) %>% 
    .[perc<0, perc:=round(0, 0)] %>% 
    dplyr::inner_join(muni, ., by = c("code_muni" = "codigo_ibge") ) 
  
  breaks_qt <- classIntervals(c(min(plotting_microdata_output$perc ) - .0000001, plotting_microdata_output$perc ), n = 6, style = "quantile")
  
  plotting_microdata_output <- mutate(plotting_microdata_output, V1_cat = cut(perc, breaks_qt$brks)) 
  
  plotting_microdata_output %>% 
    ggplot(data=.) +
    geom_sf(aes(fill=V1_cat), color= NA, size=.15) +
    labs(subtitle="Cancelled CARs over Total CARs", size=8) +
    scale_fill_brewer(palette = "RdYlBu", name="Share of CARs") + 
    scale_color_continuous() + 
    theme_minimal() + 
    theme(legend.title = element_text(), 
          text = element_text(size = 20))
  
  ggsave(
    plot = last_plot(),
    filename = "output/generating_documentation/cancelled_cars_perc_by_muni_map.pdf",
    device = cairo_pdf,
    width = 18,
    height = 9,
    scale = 1
  )
  
  
  # 3.1.3: MEDIAN year of cancelled CARs --------
  
  med_year_cancelled <- microdata %>%
    # relevant variables
    .[, .(
      registro_car,
      codigo_ibge,
      data_inscricao,
      situacao_cadastro,
      cancelled
    )]  %>% 
    .[,year :=  year(data_inscricao)] %>% 
    .[, median(year), .(cancelled, codigo_ibge )] %>% 
    dcast.data.table(formula = codigo_ibge   ~cancelled, value.var = "V1") %>% 
    .[, diff := `NA` - `TRUE` ]
  
  plotting_microdata_output <- med_year_cancelled %>% copy() %>% 
    setnafill(x=., cols = c("diff"), fill=0) %>% 
    dplyr::inner_join(muni, ., by = c("code_muni" = "codigo_ibge") ) 
  
  breaks_qt <- classIntervals(c(min(plotting_microdata_output$diff ) - .0000001, plotting_microdata_output$diff ), n = 6, style = "quantile")
  
  plotting_microdata_output <- mutate(plotting_microdata_output, V1_cat = cut(diff, breaks_qt$brks)) 
  
  plotting_microdata_output %>% 
    ggplot(data=.) +
    geom_sf(aes(fill=V1_cat), color= NA, size=.15) +
    labs(subtitle="Median Registration Year of Active CARs MINUS Median Registration Year of Cancelled CARs", size=8) +
    scale_fill_brewer(palette = "RdYlBu", name="Difference") + 
    scale_color_continuous() + 
    theme_minimal() + 
    theme(legend.title = element_text(), 
          text = element_text(size = 20))
  
  ggsave(
    plot = last_plot(),
    filename = "output/generating_documentation/cancelled_cars_year_diff_by_muni_map.pdf",
    device = cairo_pdf,
    width = 18,
    height = 9,
    scale = 1
  )
  
  # 4) PLOTTING CANCELLED CAR OVERLAP EXAMPLE --- biggest cancelled ones overlap -------------------------
  
  # 4.1) find a municipality and only look at the cancelled CARs ----
  
  
  # 3.2.X: illustrate intersection examples -------
  microdata_muni <- microdata %>%
    # relevant variables
    .[, .(
      registro_car,
      codigo_ibge,
      data_inscricao,
      situacao_cadastro,
      cancelled
    )] %>% 
    # subset data-set
    .[codigo_ibge == 1200013]  #### NOTICE, cancelled cars don't have muni codes, yet
  
  PROPERTY_SHP <- property_directories %>%  .[stri_detect_fixed(., "1200013")]
  
  car <- PROPERTY_SHP  %>% 
    read_sf() %>% 
    st_transform(4674) %>% 
    st_make_valid() 
  
  # identify a cancelled CAR
  microdata_muni[!is.na(cancelled)]
  
  
  check_intersects <- st_intersects(car[car$COD_IMOVEL=="AC-1200013-7F9077EED34645E382BEB48DACF66206", ], car)
  #   Sparse geometry binary predicate list of length 3370, where the predicate was `intersects'
  # first 10 elements:
  # 6, 598, 1162, 1963, 2238
  
  these <- car[c(6, 598, 1162, 1963, 2238), ] 
  
  microdata[registro_car%in%these$COD_IMOVEL][, .(registro_car, data_inscricao, cancelled)] %>% .[order(data_inscricao)]
  
  ### 2014 
  
  these[which(these$COD_IMOVEL%in%c("AC-1200013-3FA22235433E4725969D1C3AEF755777")) ,  "SITUACAO"] %>% 
    plot(axes = TRUE)
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1200013_cancelled_overlap_example1.pdf'))
  
  #### 2016 
  these[which(these$COD_IMOVEL%in%c("AC-1200013-3FA22235433E4725969D1C3AEF755777", "AC-1200013-C9AA224F678844A389563238E0DAB987")) ,  "SITUACAO"] %>% 
    plot(axes = TRUE)
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1200013_cancelled_overlap_example2.pdf'))
  
  these[,  "SITUACAO"] %>% 
    plot(axes = TRUE)
  
  dev.print(pdf, paste0(dir_wd, "/output/generating_documentation/", 'muni_1200013_cancelled_overlap_example3.pdf'))
  
  
  
  
}

# section 5: consolidate SICAR variables ------------

if(fp$CONSOLIDATE_VARIABLES){
  
  # sectoin 5.1: define function which consolidates municipality year SICAR variables ----  
  consolidate_muni_year_variables()
  
  # section 5.2: visualize misisng data -----
  
  # define funciton to identify munis that need to be cleaned
  get_munis_that_need_to_be_cleaned <- function(dir=dir_wd){
    
    setwd(dir)
    
    # identify all municipalities that will be cleaned 
    all_munis <- property_directories %>% 
      stri_extract_all_regex(`.`, "\\d{7}") %>% 
      unlist
    
    # BEFORE identifying the files for this batch, identify which files have already been run and remove them from the list
    munis_already_completed <-
      paste0("data/processing/SicarMuniOverlap/") %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    # identify municipalities that are still available 
    munis_that_still_need_to_be_cleaned <- all_munis %>% .[!(.%in%munis_already_completed)]
    
    munis_that_still_need_to_be_cleaned %>% 
      return()
    
  }
  
  available_munis <- get_munis_that_need_to_be_cleaned()
  
  # Section 3B.2: identify & map municipalities that have zero CARs that year, save data-set ------------
  
  sicar_vars_probing_missings <-
    fread("data/processing/identifying_errors/car_union_area_missing_v2.csv")
  
  missing_munis <- sicar_vars_probing_missings %>% .[, unique(municipio)]
  
  plot_munis_years(state_codes = states, MUNICIPIOS = missing_munis, sicar_vars_exp = sicar_vars_exp)
  
  ggsave(
    plot = last_plot(),
    filename = "output/generating_documentation/missing_car_union_with_cars_that_year.pft",
    device = cairo_pdf,
    width = 12,
    height = 8,
    scale = 1
  )
  
  # CORRECT the municipalities with errors --------
  
  ROUND_ONE <- FALSE
  if(ROUND_ONE){
    
    consolidate_muni_year_variables()
    
    municipalities_didnt_run <- c()
    #municipalities <- c(1503903, 1301704, 1302207, 1301001, 1300631, 1300409, 1300102, 1100205, 1100031, , 1505205, 1507300, 5107305, 1100031, 1302207, 1506005, 1100205, 1300409, 1300631, 1301001, 1301704) # ALREADY RUN 
    
    municipalities <- c(2111300)
    
    for(MUNICIPIO in municipalities){
      
      message_with_lines(MUNICIPIO)
      
      STATE <- states %>% .[which(.==substr(MUNICIPIO, 1,2))] %>% names()
      
      munis_and_years_muni <- fread("data/cleaned/sicar_overlap_variables.csv") %>% 
        .[is.na(car_union_area )] %>% 
        .[, .(year, municipio)] %>% 
        .[municipio==MUNICIPIO]
      
      munis_and_years_muni %>% copy() %>% 
        .[, i := 1] %>% 
        .[year<2023] %>% 
        dcast.data.table(formula = municipio ~year, value.var = "i")
      
      # first, start by plotting the municipality 
      
      plot_muni_with_missings(STATE_CODE = STATE, MUNICIPIO = MUNICIPIO, sicar_vars = sicar_vars )
      
      ggsave(
        plot = last_plot(),
        filename = paste0(
          "output/generating_documentation/municipal_car_missing_mun",MUNICIPIO,".csv.pdf"
        ),
        device = cairo_pdf,
        width = 18,
        height = 9,
        scale = 1,
        dpi = 300
      )
      
      tryCatch(expr = {
        clean_cars_normal(munis_and_years_muni);
        print(MUNICIPIO)},
        finally = {
          municipalities_didnt_run <- append(c(MUNICIPIO), municipalities_didnt_run)
          print(paste("Municipio: ", MUNICIPIO, " DID NOT RUN"))
        })
      
      
      
    }
    
  }
  
  ROUND_TWO <- FALSE
  if(ROUND_TWO){
    
    consolidate_muni_year_variables()
    
    # clean conservation/indi lands  
    conservation_clean <- conservation %>% 
      clean_shape_lands_reenforced()
    
    indigenous_clean <- indigenous %>% 
      clean_shape_lands_reenforced()
    
    # identify the problematic municipalities 
    sicar_vars_probing_missings <-
      fread("data/processing/identifying_errors/car_union_area_missing_v2.csv")
    
    missing_munis <- sicar_vars_probing_missings %>% .[, unique(municipio)]
    
    municipalities_didnt_run <- c()
    municipalities <- missing_munis
    
    for(MUNICIPIO in municipalities){
      
      message_with_lines(MUNICIPIO)
      
      STATE <- states %>% .[which(.==substr(MUNICIPIO, 1,2))] %>% names()
      
      setwd(dir_wd)
      
      munis_and_years_muni <- fread("data/cleaned/sicar_overlap_variables_exapanded.csv") %>% 
        .[is.na(car_union_area )] %>% 
        .[, .(year, municipio)] %>% 
        .[municipio==MUNICIPIO]
      
      # first, start by plotting the municipality 
      
      plot_muni_with_missings(STATE_CODE = STATE, MUNICIPIO = MUNICIPIO, sicar_vars = sicar_vars )
      
      ggsave(
        plot = last_plot(),
        filename = paste0(
          "output/generating_documentation/municipal_car_missing_mun",MUNICIPIO,".csv.pdf"
        ),
        device = cairo_pdf,
        width = 18,
        height = 9,
        scale = 1,
        dpi = 300
      )
      
      tryCatch(expr = {
        clean_cars_robust(munis_and_years_muni);
        print(MUNICIPIO)},
        finally = {
          municipalities_didnt_run <- append(c(MUNICIPIO), municipalities_didnt_run)
          print(paste("Municipio: ", MUNICIPIO, " DID NOT RUN"))
        })
      
    }
    
  }
  
  ROUND_THREE <- FALSE
  if(ROUND_THREE){
    
    consolidate_muni_year_variables()
    
    # clean conservation/indi lands  
    conservation_clean <- conservation %>% 
      clean_shape_lands_reenforced()
    
    indigenous_clean <- indigenous %>% 
      clean_shape_lands_reenforced()
    
    # identify the problematic municipalities 
    sicar_vars_probing_missings <-
      fread("data/processing/identifying_errors/car_union_area_missing_v2.csv")
    
    missing_munis <- sicar_vars_probing_missings %>% .[, unique(municipio)]
    
    municipalities_didnt_run <- c()
    municipalities <- c(2111300, 1504109, 1504752, 1505205, 1507300, 1600550, 5106752) # 
    
    for(MUNICIPIO in municipalities){
      
      message_with_lines(MUNICIPIO)
      
      STATE <- states %>% .[which(.==substr(MUNICIPIO, 1,2))] %>% names()
      
      setwd(dir_wd)
      
      munis_and_years_muni <- fread("data/cleaned/sicar_overlap_variables_exapanded.csv") %>% 
        .[is.na(car_union_area )] %>% 
        .[, .(year, municipio)] %>% 
        .[municipio==MUNICIPIO]
      
      # first, start by plotting the municipality 
      
      plot_muni_with_missings(STATE_CODE = STATE, MUNICIPIO = MUNICIPIO, sicar_vars = sicar_vars )
      
      ggsave(
        plot = last_plot(),
        filename = paste0(
          "output/generating_documentation/municipal_car_missing_mun",MUNICIPIO,".csv.pdf"
        ),
        device = cairo_pdf,
        width = 18,
        height = 9,
        scale = 1,
        dpi = 300
      )
      
      tryCatch(expr = {
        clean_cars_robust_alternate(munis_and_years_muni);
        print(MUNICIPIO)},
        finally = {
          municipalities_didnt_run <- append(c(MUNICIPIO), municipalities_didnt_run)
          print(paste("Municipio: ", MUNICIPIO, " DID NOT RUN"))
        })
      
    }
    
  }
  
  ROUND_FOUR <- FALSE
  if(ROUND_FOUR){
    
    consolidate_muni_year_variables()
    
    # clean conservation/indi lands  
    conservation_clean <- conservation %>% 
      clean_shape_lands_reenforced()
    
    indigenous_clean <- indigenous %>% 
      clean_shape_lands_reenforced()
    
    # identify the problematic municipalities 
    sicar_vars_probing_missings <-
      fread("data/processing/identifying_errors/car_union_area_missing_v2.csv")
    
    missing_munis <- sicar_vars_probing_missings %>% .[, unique(municipio)]
    
    municipalities_didnt_run <- c()
    municipalities <- c(2111300, 5106752) # 1505205, 1600550, 1507300
    #1504752, 1507300
    
    for(MUNICIPIO in municipalities){
      
      message_with_lines(MUNICIPIO)
      
      STATE <- states %>% .[which(.==substr(MUNICIPIO, 1,2))] %>% names()
      
      setwd(dir_wd)
      
      munis_and_years_muni <- fread("data/cleaned/sicar_overlap_variables_exapanded.csv") %>% 
        .[is.na(car_union_area )] %>% 
        .[, .(year, municipio)] %>% 
        .[municipio==MUNICIPIO]
      
      # first, start by plotting the municipality 
      
      plot_muni_with_missings(STATE_CODE = STATE, MUNICIPIO = MUNICIPIO, sicar_vars = sicar_vars )
      
      ggsave(
        plot = last_plot(),
        filename = paste0(
          "output/generating_documentation/municipal_car_missing_mun",MUNICIPIO,".csv.pdf"
        ),
        device = cairo_pdf,
        width = 18,
        height = 9,
        scale = 1,
        dpi = 300
      )
      
      tryCatch(expr = {
        clean_cars_robust_alternate2(munis_and_years_muni);
        print(MUNICIPIO)},
        finally = {
          municipalities_didnt_run <- append(c(MUNICIPIO), municipalities_didnt_run)
          print(paste("Municipio: ", MUNICIPIO, " DID NOT RUN"))
        })
      
    }
    
  }
  
  ROUND_FIVE <- FALSE
  if(ROUND_FIVE){
    
    consolidate_muni_year_variables()
    
    fread("data/processing/identifying_errors/car_union_area_missing_v2.csv") %>% 
      .[!(year==2015&municipio==1504752)] %>% #1504752, 2015, "NO shapes, but there are registered CARs"
      .[!(year==2016&municipio==1504752)] %>% #1504752, 2015, "NO shapes, but there are registered CARs"
      fwrite("data/processing/identifying_errors/car_union_area_missing_v2.csv") 
    
    # clean conservation/indi lands  
    conservation_clean <- conservation %>% 
      clean_shape_lands_reenforced()
    
    indigenous_clean <- indigenous %>% 
      clean_shape_lands_reenforced()
    
    # identify the problematic municipalities 
    sicar_vars_probing_missings <-
      fread("data/processing/identifying_errors/car_union_area_missing_v2.csv")
    
    missing_munis <- sicar_vars_probing_missings %>% .[, unique(municipio)]
    
    municipalities_didnt_run <- c()
    municipalities <- c(2111300, 1507300) 
    
    for(MUNICIPIO in municipalities){
      
      message_with_lines(MUNICIPIO)
      
      STATE <- states %>% .[which(.==substr(MUNICIPIO, 1,2))] %>% names()
      
      setwd(dir_wd)
      
      munis_and_years_muni <- fread("data/cleaned/sicar_overlap_variables_exapanded.csv") %>% 
        .[is.na(car_union_area )] %>% 
        .[, .(year, municipio)] %>% 
        .[municipio==MUNICIPIO]
      
      # first, start by plotting the municipality 
      
      plot_muni_with_missings(STATE_CODE = STATE, MUNICIPIO = MUNICIPIO, sicar_vars = sicar_vars )
      
      ggsave(
        plot = last_plot(),
        filename = paste0(
          "output/generating_documentation/municipal_car_missing_mun",MUNICIPIO,".csv.pdf"
        ),
        device = cairo_pdf,
        width = 18,
        height = 9,
        scale = 1,
        dpi = 300
      )
      
      tryCatch(expr = {
        clean_cars_robust_alternate4(munis_and_years_muni);
        print(MUNICIPIO)},
        finally = {
          municipalities_didnt_run <- append(c(MUNICIPIO), municipalities_didnt_run)
          print(paste("Municipio: ", MUNICIPIO, " DID NOT RUN"))
        })
      
    }
    
  }
  
  # final consolidation -----
  
  consolidate_muni_year_variables()
  
}

# section 6: intersect with the sf's above & compute areas for each MUNI & YEAR using S2 data ------ 

if(fp$INTERSECT_SFS_S2_OR_VALIDATED){
  
  setwd(dir_wd)
  
  # section 3.0: set-up -------
  
  # municipal borders 
  municipal_borders <- geobr::read_municipality() 
  
  # get the updated micro-data  
  microdata <- fread("data/raw/sicar/microdata/temas_ambientais_update.csv")
  
  # load years  
  car_and_reg_year_wide <- fread("data/processing/car_and_reg_year_wide.csv") %>% 
    .[, municipio := as.numeric(substr(registro_car, 4, 10))] 
  
  # get all file names 
  property_directories_s2 <- list.dirs("data/processing/CleanCARShapes_s2/", recursive = T) %>% 
    .[order(.)] %>%
    data.table(path=.) %>% 
    .[, municipio := stri_extract_all_regex(path, "\\d{7}")] %>% 
    # subset to municipalities in the amazon
    .[municipio %in% municipalities_amazon, path] %>% 
    .[order(.)] 
  
  property_directories_light <- list.dirs("data/processing/CleanCARShapes_light/", recursive = T) %>% 
    .[order(.)] %>%
    data.table(path=.) %>% 
    .[, municipio := stri_extract_all_regex(path, "\\d{7}")] %>% 
    # subset to municipalities in the amazon
    .[municipio %in% municipalities_amazon, path] %>% 
    .[order(.)] 
  
  munis_s2 <- property_directories_s2 %>% stri_replace_all_fixed(., "data/processing/CleanCARShapes_s2//muni", "")
  munis_light <- property_directories_light %>% stri_replace_all_fixed(., "data/processing/CleanCARShapes_light//muni", "")
  
  # find munis not in s2 but in light cleaning 
  these <- munis_light %>% .[!(.%in%munis_s2)] 
  
  property_directories <- property_directories_s2 %>% append(paste0("data/processing/CleanCARShapes_light//muni", these))
  
  # do an update-as-you-go framework -----------------
  
  # define funciton to identify munis that need to be cleaned
  get_munis_that_need_to_be_cleaned <- function(dir=dir_wd){
    
    setwd(dir)
    
    # identify all municipalities that will be cleaned 
    all_munis <- property_directories %>% 
      stri_extract_all_regex(`.`, "\\d{7}") %>% 
      unlist
    
    # BEFORE identifying the files for this batch, identify which files have already been run and remove them from the list
    munis_already_completed <-
      paste0("data/processing/SicarMuniOverlap_s2/") %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    # identify municipalities that are still available 
    munis_that_still_need_to_be_cleaned <- all_munis %>% .[!(.%in%munis_already_completed)]
    
    munis_that_still_need_to_be_cleaned %>% 
      return()
    
  }
  
  # define function that selects and claims municipalities
  select_muni_to_clean <- function(
    dir=dir_wd,
    available_munis=available_munis){
    
    setwd(dir = dir)
    
    FILE <- "data/processing/munis_already_claimed_s2.csv"
    
    if (!file.exists(FILE)) {
      data.table(muni = c("")) %>%
        .[0] %>%
        fwrite(., FILE)
      
    }
    
    munis_already_completed <-
      paste0("data/processing/SicarMuniOverlap_s2/") %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    
    FILE <- "data/processing/munis_already_claimed_s2.csv"
    
    if(!file.exists(FILE)){
      
      data.table(muni=c("")) %>% 
        fwrite(., FILE)
      
    }
    
    already_claimed <- fread(FILE)
    
    fread(FILE)
    
    this_muni <- available_munis %>% 
      # make sure they are still available
      .[!.%in%munis_already_completed] %>% 
      .[!.%in%already_claimed] %>% 
      .[sample(length(.))] %>% 
      .[1]
    
    data.table(muni=this_muni) %>% 
      fwrite(FILE, append = T)
    
    return(this_muni)
    
  }
  
  # identify claimed municipalities (may or may not appear in the `get_munis_that_need...` output)
  get_munis_that_were_claimed_or_cleaned <- function(dir=dir_wd){
    
    setwd(dir)
    
    FILE <- "data/processing/munis_already_claimed_s2.csv"
    
    if(!file.exists(FILE)){
      
      data.table(muni=c("")) %>% 
        fwrite(., FILE)
      
    }
    
    munis1 <- fread(FILE) %>% 
      .[, muni] 
    
    munis2 <- get_munis_that_need_to_be_cleaned(dir = dir)
    
    munis2 %>% .[!.%in%munis1] %>% 
      .[!duplicated(.)] %>% 
      return()
    
  }
  
  # identify already cleaned munis
  
  available_munis <- get_munis_that_need_to_be_cleaned()
  available_munis <- 1
  print(length(available_munis))

  # 3.1: for each municipality ------
  
  while(length(available_munis)>0){
    
    # select municipality  
    available_munis <- get_munis_that_need_to_be_cleaned()
    THIS_muni <- select_muni_to_clean(dir = dir_wd, available_munis=available_munis)
    
    
    # data/raw/sicar/shapefiles//RO/SHAPE_1101203/AREA_IMOVEL -- 2015
    # THIS_muni <-   1506005 #1506005 #, 1500404
    # THIS_muni <-  2112852
    # THIS_muni <-  1300631
    THIS_muni <-  1507300
    
    # select property directoy 
    
    property_directories_RUNBATCH <- property_directories %>% 
      .[!stri_detect_fixed(., "AREA_IMOVEL_")] %>% 
      .[stri_detect_fixed(., THIS_muni)] %>% 
      .[order(.)] %>% 
      .[!duplicated(.)] %>% 
      c()
    
      # get the file path to the shape file & municipality name 
      PROPERTY_SHP <- property_directories_RUNBATCH
      PROPERTY_MUNI <- stri_extract_all_regex(PROPERTY_SHP, "\\d{7}") %>% unlist()
      PROPERTY_UF <- PROPERTY_MUNI %>% substr(., 1,2)
      
      message_with_lines(paste0("Running this municipality: ", PROPERTY_MUNI))
      
      # load forest data 
      message_with_lines("Cleaning forests")
      forests <- load_forrest_data(PROPERTY_UF) %>% 
        mutate(typeA = startsWith(codigo, "FPA")*1) %>% 
        mutate(typeB = startsWith(codigo, "FPB")*1) %>% 
        mutate(typeC = startsWith(codigo, "FPC")*1)  %>% 
        st_make_valid() %>% 
        mutate(is_valid = st_is_valid(.))
      
      forests <- forests[which(forests$is_valid==TRUE),]
    
      # clean 
      forestA <- forests[forests$typeA==1, ] %>% 
        st_union() %>% 
        st_make_valid()
      
      forestB <- forests[forests$typeB==1, ] %>% 
        st_union() %>% 
        st_make_valid()
      
      forestC <- forests[forests$typeC==1, ] %>% 
        st_union() %>% 
        st_make_valid()
      
      forestsALL <- forests %>% 
        st_union() %>% 
        st_make_valid()
      
      message_with_lines("Forest Intersections")
      
      # compute intersections of forested and conservation lands 
      intersect_forestAB <-  st_intersection(forestA, forestB ) %>% 
        st_make_valid() %>% #plot(., col="red")
        st_area() %>%
        sum() %>% 
        .[1]
      
      intersect_forestAC <-  st_intersection(forestA, forestC ) %>% 
        st_make_valid() %>% #plot(., col="red")
        st_area() %>%
        sum() %>% 
        .[1]
      
      intersect_forestBC <-  st_intersection(forestB, forestC ) %>% 
        st_make_valid() %>% #plot(., col="red")
        st_area() %>%
        sum() %>% 
        .[1]
      
      intersect_forestALL <-  forestsALL %>% 
        st_area() %>%
        sum() %>% 
        .[1]
      
      # intersect_forests_indigenous <-  st_intersection(forestsALL, indigenous ) %>% 
      #   st_make_valid() %>% #plot(., col="red")
      #   st_area() %>%
      #   sum() %>% 
      #   .[1]
      # 
      # intersect_forests_conservation <-  st_intersection(forestsALL, conservation ) %>% 
      #   st_make_valid() %>% #plot(., col="red")
      #   st_area() %>%
      #   sum() %>% 
      #   .[1]
      
      message_with_lines("Municipal Intersections")
      
      # get municipality border 
      muni_border <- municipal_borders[municipal_borders$code_muni==as.numeric(PROPERTY_MUNI),]
      
      muni_border %<>%     
        st_make_valid() %>% 
        st_union() %>% 
        st_make_valid()
      
      muni_area <- muni_border %>% 
        st_area() %>%
        sum() %>% 
        .[1]
      
      # status update
      message_with_lines(PROPERTY_SHP)
      paste0(length(available_munis), " municipalities left.") %>% message_with_lines()
      
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
        .[, FULL := 1] %>% 
        .[municipio == PROPERTY_MUNI] %>% 
        select(-c("municipio")) %>% 
        melt.data.table(id.vars = c("registro_car")) %>% 
        rename_columns(c("variable"),c("year")) %>% 
        .[, year := stri_replace_all_fixed(year, "y", "")]
      
      
      # 3.4: for each year -----------
      years <- c(2014:2022) %>% as.character() 
      for(YEAR in years){
        
        paste0( PROPERTY_SHP, " -- ",  YEAR) %>% message_with_lines()
        
        # subset the cars for that year 
        CARS_THIS_YEAR <- car_years_muni %>% 
          .[year==YEAR & value==1] %>% 
          .[, registro_car]
        
        # if there are cars this year, then proceeds; otherwise skip year 
        if(length(CARS_THIS_YEAR)>0){
          
          l <- 1
          message_with_lines(l)
          
          # 1: load property data by municipio;  Change 4674 crs -----
          car <- PROPERTY_SHP  %>% 
            read_sf() %>% 
            st_transform(4674)  %>% 
            # subset to relevant properties 
            .[which(.$COD_IMOVEL %in% CARS_THIS_YEAR),]
          
          # # 2: flag invalid polygons then convert them to valid ones 
          # car$flag_valid <- st_is_valid(car) 
          # 
          # # 3: make invalid polygons valid as well
          # car %<>% clean_shape(sf_obj = .)
          
          l <- l+3
          message_with_lines(l)
          
          # 4: create the union of the polygons ------
          if(THIS_muni!="2111300"){
          car_union <- car %>% 
            st_union() %>% 
            clean_union_reenforced()
          }else{
            
            car_union <- car %>% 
              clean_shape() %>% 
              st_union() 
            
          }
          
          st_is_valid(car_union)
          
          l <- l+1
          message_with_lines(l)
          # 5: compute the union area ------
          car_area <- car_union %>% 
            st_area() %>% 
            .[1]
          
          l <- l+1
          message_with_lines(l)
          # 6: compute the intersection with indigenous land area
          car_area_intersect_indi <- car_union %>%
            # calculate intersection area
            st_intersection(indigenous) %>% 
            st_area() %>%
            sum() %>% 
            .[1]
          
          l <- l+1
          message_with_lines(l)
          # 7: compute the intersection with conservation land area
          if(THIS_muni%in%c("1300631")){
            
            car_area_intersect_conserve <- NA
            
          }else{
            
            car_area_intersect_conserve <- car_union %>%
              # calculate intersection area
              st_intersection(conservation) %>% 
              st_area() %>%
              sum()  %>% 
              .[1]            
            
          }
          
          l <- l+1
          message_with_lines(l)
        
          # 8A: compute the intersection with forested land area

          car_intersect_forestA <- st_intersection(forestA, car_union)
          
          if(THIS_muni%in%c("1507300")){
            
            car_area_intersect_forestA <- car_intersect_forestA %>% 
              .[2] %>% 
              clean_union_reenforced() %>%
              st_make_valid() %>% 
              st_area() %>%
              sum()  %>% 
              .[1]
            
          }else{
            
            car_area_intersect_forestA <- car_intersect_forestA %>% 
              st_make_valid() %>% #plot(., col="red")
              st_area() %>%
              sum()  %>% 
              .[1]
            
          }

          message_with_lines(l)
          # 8b: compute the intersection with forested land area
          car_intersect_forestB <- 
            st_intersection(forestB, car_union)
          
          car_area_intersect_forestB <- car_intersect_forestB %>% 
            st_make_valid() %>% #plot(., col="red")
            st_area() %>%
            sum() %>% 
            .[1]


          message_with_lines(l)
          # 8c: compute the intersection with forested land area
          car_intersect_forestC <- 
            st_intersection(forestC, car_union)
          
          car_area_intersect_forestC <- car_intersect_forestC %>% 
            st_make_valid() %>% #plot(., col="red")
            st_area() %>%
            sum() %>% 
            .[1]
          
          # 8d: compute the intersection with forested land area
          car_intersect_forestALL <- 
            st_intersection(forestsALL, car_union)
          
          if(THIS_muni%in%c("1507300")){
            
            car_area_intersect_forestALL <- car_intersect_forestALL %>% 
              .[2] %>% 
              clean_union_reenforced() %>%
              st_make_valid() %>% 
              st_area() %>%
              sum() %>% 
              .[1]
            
          }else{
            
            car_area_intersect_forestALL <- car_intersect_forestALL %>% 
              st_make_valid() %>% #plot(., col="red")
              st_area() %>%
              sum() %>% 
              .[1]
          }
          
          # 8e: compute intersections of forested lands  
          
          l <- l+1
          message_with_lines(l)
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
            st_area() %>% 
            sum() %>% 
            .[1]
          
          if(length(car_area_cancelled)>1){
            car_area_cancelled <- max(car_area_cancelled)
          }
          
          l <- l+0.2
          message_with_lines(l)
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
            st_area() %>% 
            .[1]
          
          if(length(car_area_notcancelled)>1){
            car_area_notcancelled <- max(car_area_notcancelled)
          }
          
          car_area_intersections <- st_intersection(car_cancelled, car_notcancelled) %>%
            st_make_valid() %>%
            st_area() %>%
            sum() %>% 
            .[1]
          
          l <- l+0.8
          message_with_lines(l)
          # 10 compute union of municipality and all CARs to ensure everything is correct -----
          
          car_area_in_muni <- st_intersection(muni_border, car_union) %>%
            st_make_valid() %>%
            st_area() %>%
            sum() %>% 
            .[1]
          
          data.table(
            # area of Union of CARs
            car_union_area = car_area,
            car_union_area_in_muni = car_area_in_muni,
            # area of Union of CARS with sensitive lands 
            car_area_intersect_indi = car_area_intersect_indi,
            car_area_intersect_conserve = car_area_intersect_conserve,
            car_area_intersect_forestA = car_area_intersect_forestA,
            car_area_intersect_forestB = car_area_intersect_forestB,
            car_area_intersect_forestC = car_area_intersect_forestC,
            # intersection of senstive lands with them-selves 
            intersect_forestAB=intersect_forestAB,
            intersect_forestAC=intersect_forestAC,
            intersect_forestBC=intersect_forestBC,
            intersect_forestALL=intersect_forestALL,
            # intersect_forests_indigenous=intersect_forests_indigenous,
            # intersect_forests_conservation=intersect_forests_conservation,
            # relationship of cancelled and non-cancelled CAR areas
            car_area_cancelled = car_area_cancelled, 
            car_area_notcancelled = car_area_notcancelled,
            car_area_ca_notca_intersections=car_area_intersections,
            # computed municipal area
            muni_area = muni_area,
            # information 
            year = YEAR,
            municipio = PROPERTY_MUNI
          ) %>% 
            fwrite(paste0("data/processing/SicarMuniOverlap_s2/muni", PROPERTY_MUNI, "_", YEAR, ".csv"))
          
      }
      
      
      gc()
      
    }
    
    not_available_munis <- get_munis_that_were_claimed_or_cleaned()
    # update the available municipalities 
    available_munis <- get_munis_that_need_to_be_cleaned()
    
    available_munis %>% .[!.%in%not_available_munis] 
    
  }
  
  
}

# section 5: consolidate SICAR variables ------------

if(fp$CONSOLIDATE_VARIABLES_S2_OR_VALIDATED){
  
  # sectoin 5.1: define function which consolidates municipality year SICAR variables ----  
  consolidate_muni_year_variables_s2()
  
  # section 5.2: visualize misisng data -----
  
  # define funciton to identify munis that need to be cleaned
  get_munis_that_need_to_be_cleaned <- function(dir=dir_wd){
    
    setwd(dir)
    
    # identify all municipalities that will be cleaned 
    all_munis <- property_directories %>% 
      stri_extract_all_regex(`.`, "\\d{7}") %>% 
      unlist
    
    # BEFORE identifying the files for this batch, identify which files have already been run and remove them from the list
    munis_already_completed <-
      paste0("data/processing/SicarMuniOverlap_s2/") %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    # identify municipalities that are still available 
    munis_that_still_need_to_be_cleaned <- all_munis %>% .[!(.%in%munis_already_completed)]
    
    munis_that_still_need_to_be_cleaned %>% 
      return()
    
  }
  
  available_munis <- get_munis_that_need_to_be_cleaned()
  
  # everything was cleaned! 
}
