
# section 3A: correct CARs where the union is equal to zero ------- 
FIX_CAR_UNION_AREAS_ZERO <- TRUE
if(FIX_CAR_UNION_AREAS_ZERO){
  
  munis_and_years <- fread("data/processing/identifying_errors/car_union_area_zero.csv") %>% 
    .[, .(year, municipio)]
  
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
      
      # 8: compute the intersection with forested land area total
      car_area_intersect_forest_total <-
        forests %>%
        # calculate intersection area
        st_intersection( car_union) %>% #plot(., col="red")
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


# section 3B: correct CARs where the union is equal to zero ------- 

# Section 3B.1: preliminaries ------------

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

sicar_vars_exp <- fread("data/cleaned/sicar_overlap_variables_exapanded.csv")


# define helper functions to visualize munis -----

muni_shapes <- read_municipality() %>% 
  st_transform(4674) 

states_shapes <- geobr::read_state() %>%
  st_transform(4674)  


plot_muni_with_missings <- function(STATE_CODE="RO", MUNICIPIO=1100031, sicar_vars, muni=muni_shapes, states_sf=states_shapes){
  
  # 5.1: prep for mapping --------
  library(geobr)
  library(classInt)
  library(ggplot2)
  
  # 5.2: amount of CAR claimed land --------
  
  states_sf  %<>%        .[which(.$abbrev_state%in%c(STATE_CODE)), ] 
  
  plotting_microdata_output <- sicar_vars %>% copy() %>% 
    .[municipio==MUNICIPIO] %>% 
    .[, is_missing := is.na(car_union_area) ] %>% 
    .[, .(municipio,  is_missing, year)] %>% 
    dplyr::inner_join(muni, ., by = c("code_muni" = "municipio") ) 
  
  plotting_microdata_output %>% 
    ggplot(data=.) +
    geom_sf(aes(fill=is_missing), color= "grey50", size=.15) +
    facet_wrap(~year) + 
    scale_fill_brewer(palette = "RdYlBu", direction=-1, bquote(km^2)) + 
    theme_minimal() + 
    theme( 
      text = element_text(size = 20), 
      # axis.text = element_text(size = 12), 
      axis.text = element_blank()) + 
    geom_sf(data = states_sf, color = "black", fill = NA) + 
    geom_sf_text(data = states_sf, aes(label = abbrev_state)) + 
    ylab("") + xlab("") %>% return()
  
  
}

plot_munis_years <- function(state_codes=states, MUNICIPIOS=missing_munis, sicar_vars_exp, muni=muni_shapes, states_sf=states_shapes){
  
  # 5.1: prep for mapping --------
  library(geobr)
  library(classInt)
  library(ggplot2)
  
  # 5.2: amount of CAR claimed land --------
  
  states_sf  %<>% .[which(.$abbrev_state%in%names(state_codes)), ] 
  
  plotting_microdata_output <- sicar_vars_exp %>% copy() %>% 
    .[municipio%in%MUNICIPIOS] %>% 
    .[, is_missing := is.na(car_union_area) ] %>% 
    .[, .(municipio,  is_missing, year)] %>% 
    dplyr::inner_join(muni, ., by = c("code_muni" = "municipio") ) 
  
  plotting_microdata_output %>% 
    ggplot(data=.) +
    geom_sf(aes(fill=is_missing), color= "grey50", size=.15) +
    facet_wrap(~year) + 
    scale_fill_brewer(palette = "RdYlBu", direction=-1, bquote(km^2)) + 
    theme_minimal() + 
    theme( 
      text = element_text(size = 20), 
      # axis.text = element_text(size = 12), 
      axis.text = element_blank()) + 
    geom_sf(data = states_sf, color = "black", fill = NA) + 
    geom_sf_text(data = states_sf, aes(label = abbrev_state)) + 
    ylab("") + xlab("") %>% return()
  
  
}







# RUN this section 1100205 : confirm YES when run: Confrimation:PENDING    -----



# 1504752 1507300 2111300
# 
# MUNICIPIO <- 1505205
# 
# STATE <- states %>% .[which(.==substr(MUNICIPIO, 1,2))] %>% names()
# 
# munis_and_years <- fread("data/processing/identifying_errors/car_union_area_missing_v2.csv") %>%
#   .[, .(year, municipio)] #%>% 
#   # .[municipio==MUNICIPIO]
# 
# # first, start by plotting the municipality
# 
# plot_muni_with_missings(STATE_CODE = STATE, MUNICIPIO = MUNICIPIO, sicar_vars = sicar_vars )






















# RUN this section 1300631 : confirm YES when run: Confrimation:PENDING    -----


# munis_and_years_full <- fread("data/cleaned/sicar_overlap_variables_exapanded.csv")
# 
# munis_and_years_full_missings <- munis_and_years_full %>% copy() %>% 
#   .[is.na(car_union_area )] %>% 
#   .[, .(year, municipio)] 
# 
# all_that_have_alo_missing <- munis_and_years_full_missings %>% copy() %>% 
#   .[, i := 1] %>% 
#   .[year<2023] %>% 
#   dcast.data.table(formula = municipio ~year, value.var = "i") %>% 
#   setnafill(x=., cols = paste0(c(2014:2022)), fill=0) %>% 
#   .[order(`2022`, `2021`, `2020`, `2019`, `2018`, `2017`, `2016`, `2015`, `2014`)] %>% 
#   .[, total_years_missing := `2022`+ `2021`+ `2020`+ `2019`+ `2018`+ `2017`+ `2016`+ `2015`+ `2014`]



