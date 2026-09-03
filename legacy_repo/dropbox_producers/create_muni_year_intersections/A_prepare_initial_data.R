
setwd(dir_wd)


# quickly fix a few odd data-sets ----
if(fp$QUICK_FIX){
  
  a1 <- st_read("data/raw/sicar/shapefiles/RO/SHAPE_1100205/AREA_IMOVEL_1")
  a2 <- st_read("data/raw/sicar/shapefiles/RO/SHAPE_1100205/AREA_IMOVEL_2")
  a3 <- st_read("data/raw/sicar/shapefiles/RO/SHAPE_1100205/AREA_IMOVEL_3")
  a4 <- st_read("data/raw/sicar/shapefiles/RO/SHAPE_1100205/AREA_IMOVEL_4")
  
  "data/raw/sicar/shapefiles//RO/SHAPE_1100205/AREA_IMOVEL/" %>% dir.create()
  
  rbind(a1, a2) %>% 
    rbind(a3) %>% 
    rbind(a4) %>% st_write("data/raw/sicar/shapefiles//RO/SHAPE_1100205/AREA_IMOVEL/AREA_IMOVEL.shp")

}

# section 1: load main data-sets ------ 

if(fp$LOAD_DATA){

  # 1.0: relevant states
  brazil_states <- data.table::fread(
    "uf_code	uf_name	uf_sigla
  11	Rondônia	RO
  12	Acre	AC
  13	Amazonas	AM
  14	Roraima	RR
  15	Pará	PA
  16	Amapá	AP
  17	Tocantins	TO
  21	Maranhão	MA
  22	Piauí	PI
  23	Ceará	CE
  24	Rio Grande do Norte	RN
  25	Paraíba	PB
  26	Pernambuco	PE
  27	Alagoas	AL
  28	Sergipe	SE
  29	Bahia	BA
  31	Minas Gerais	MG
  32	Espírito Santo	ES
  33	Rio de Janeiro	RJ
  35	São Paulo	SP
  41	Paraná	PR
  42	Santa Catarina	SC
  43	Rio Grande do Sul (*)	RS
  50	Mato Grosso do Sul	MS
  51	Mato Grosso	MT
  52	Goiás	GO
  53	Distrito Federal	DF") 
  
  # 22, 50, 52, 53
  
  states <- brazil_states %>% .[uf_code%in%c(11:17, 21, 51)] %>%  .[, as.character(uf_code)]
  names(states) <- brazil_states %>% .[uf_code%in%c(11:17, 21, 51)] %>% .[, uf_sigla]
  
  # 1.1: municipal boundaries in amazon 
  municipalities <- read_sf("data/raw/terrabrasil/municipalities_amazon_biome/municipalities_amazon_biome.shp") %>% 
    st_transform(., crs = 4674) %>% # Change 4674 crs
    st_make_valid()
  
  municipalities_amazon <- municipalities %>% 
    st_drop_geometry() %>% as.data.table() %>%
    .[,unique(geocodigo)]
  
  # 1.2 amazon biome 
  amazon_bioma <- read_sf("data/raw/terrabrasil/amazon_biome_border/amazon_biome_border.shp") %>% 
    st_transform(., crs = 4674) %>% # Change 4674 crs
    st_make_valid()
  
  # 1.3 terras indigenas
  indigenous <- read_sf("data/raw/terrabrasil/indigenous_area_amazon_biome/indigenous_area_amazon_biome.shp") %>% 
    st_transform(., crs = 4674) %>% # Change 4674 crs
    st_make_valid()
  
  # 1.4 unidades de conservacao 
  conservation <- read_sf("data/raw/terrabrasil/conservation_units_amazon_biome") %>% 
    st_transform(., crs = 4674) %>% # Change 4674 crs
    st_make_valid()
  
  # 1.5 florestas publicas nao designadas - create funciton to load the forest shapefiles 
  
  # identify states of interest 
  # states <- "data/raw/sicar/shapefiles/" %>% list.dirs(recursive = F, full.names = F)
  load_forrest_data <- function(state_code, states_vector=states){
    
    STATE <- which(states_vector == state_code) %>% names()
    
    # load shapefiles
    file_name <- list.files(path="data/raw/cnfp/", pattern="*.shp", recursive = TRUE) %>%
      data.table(file=.) %>% 
      .[, state := stri_replace_all_fixed(file, "CNFP 2020 Shapefiles/CNFP_2020_", "")] %>%
      .[, state := stri_replace_all_fixed(state, ".shp", "")] %>%
      .[state == STATE] %>% 
      .[, file] %>% 
      .[order(.)]
    
    forest <- 
      read_sf(paste0("data/raw/cnfp/", file_name)) %>% 
      st_transform(., crs = 4674) %>% # Change 4674 crs
      st_make_valid() %>% 
      return()
    
  }
  
  
}

# section 2: Create wide data-set with id_car - registration year indicators indicating the land was registered on or before that year ------

if(fp$CARDIDS_AND_YEARS){
  
  # 2.0 imovel information ------------
  microdata_inc <-
    fread(
      "data/raw/sicar/microdata/temas_ambientais.csv"#,
      # select = c(
      #   "data_ultima_retificacao",
      #   "data_inscricao",
      #   "data_alteracao_condicao_cadastro",
      #   "registro_car",
      #   "codigo_ibge",
      #   "situacao_cadastro"
      # )
    ) %>% 
    # only keep relevant states
    .[, uf := as.numeric(substr(codigo_ibge, 1, 2))] %>%
    .[codigo_ibge %in% municipalities_amazon ] %>% 
    # convert data classes
    .[, `:=`(
      data_inscricao = as.IDate(data_inscricao),
      data_alteracao_condicao_cadastro = as.IDate(data_alteracao_condicao_cadastro),
      data_ultima_retificacao = as.IDate(data_ultima_retificacao)
    )]
  
  gc()
  
  
  # 2.0.1 quickly understand the dynamics of these date variables :: data_inscricao < data_ultima_retificacao < data_alteracao_condicao_cadastros
  if(fp$CHECKS){
    
    #  check date properties -----
    
    ## CONCLUSION: data_inscricao < data_ultima_retificacao < data_alteracao_condicao_cadastro
    
    # data_inscricao ALWAYS EARILIER THAN data_alteracao_condicao_cadastro
    microdata_inc %>% copy() %>% 
      .[data_inscricao>data_alteracao_condicao_cadastro]
    # data_inscricao ALWAYS EARILIER THAN data_ultima_retificacao
    microdata_inc %>% copy() %>% 
      .[data_inscricao>data_ultima_retificacao]
    # data_ultima_retificacao ALWAYS EARILIER THAN  data_alteracao_condicao_cadastro
    microdata_inc %>% copy() %>% 
      .[data_ultima_retificacao>data_alteracao_condicao_cadastro]
    
  }
  
  # 2.1: create wide data-set with id_car - registration year indicators indicating the land was registered on or before that year ------
  
  # section 2.1.1.1: identify CARs that were canceled, use the API to identify and fill in this information ------ 
  all_cars <- foreign::read.dbf( "data/cleaned/sicar/car_combined.dbf") %>% 
    data.table() %>% 
    .[,municipio := as.numeric(substr(COD_IMOVEL, 4, 10))] %>% 
    # only keep municipalities in the amazom
    .[municipio %in% municipalities_amazon] %>% 
    .[!duplicated(COD_IMOVEL)]
  
  if(fp$RUNNING_TESTS){
    
    
    all_carids_in_amazon_biome <- all_cars %>% 
      .[, COD_IMOVEL]
    
    
    # section 2.1.1.2: load FULL microdata_inc ------
    microdata_inc <-
      fread(
        "data/raw/sicar/microdata/temas_ambientais.csv"#,
        # select = c(
        #   "data_ultima_retificacao",
        #   "data_inscricao",
        #   "data_alteracao_condicao_cadastro",
        #   "registro_car",
        #   "codigo_ibge",
        #   "situacao_cadastro"
        # )
      ) %>% 
      # only keep relevant states
      .[, uf := as.numeric(substr(codigo_ibge, 1, 2))] 
    
    microdata_inc %>% 
      .[registro_car %in% all_carids_in_amazon_biome]
    
    microdata_inc %>% 
      .[registro_car %in% all_carids_in_amazon_biome]
    
    
    paste0("Within the municipalities in Amazon Biome, therea are ", format_with_commas(all_cars[, uniqueN(COD_IMOVEL)]), " unique CARs.")
    
  }
  
  # 2.1.1: identify % of CARs that were cancelled
  if(fp$SUMSTATS){
    
    all_cars[, uniqueN(COD_IMOVEL)]
    all_cars[SITUACAO=="CA", uniqueN(COD_IMOVEL)]
    
    # percentage of CARs that were cancelled
    all_cars %>% copy() %>%
      .[, CAR_CANCELLED :=  SITUACAO=="CA"]  %>%
      .[, .N ,CAR_CANCELLED]  %>%
      .[, .(paste0(round(100*(N/sum(N)),0), "%"), CAR_CANCELLED)] %>% 
      fwrite("data/processing/summary_statistics/cars_cancelled_N_perc.csv")
    
    # percentage of CAR area that was cancelled
    all_cars %>% 
      .[, sum(NUM_AREA), SITUACAO]  %>%
      .[, .(paste0(round(100*(V1/sum(V1)),0), "%"), SITUACAO)]  %>% 
      fwrite("data/processing/summary_statistics/cars_cancelled_area_perc.csv")
    
    
    # compute area or CAR overlap 
    sicar <- st_read("data/cleaned/sicar/") %>% 
      st_transform(., crs = 4674) %>% # Change 4674 crs
      st_make_valid() %>% 
      mutate( empty = st_is_empty(geometry)) %>% 
      mutate( valid = st_is_valid(geometry))  
    
    sicar %<>% 
      .[which(sicar$empty==FALSE), ]
    
    sicar %<>% 
      .[which(sicar$valid==TRUE), ]
    
    # reduce to work with data 
    # sicar_small <- sicar %>% copy() %>% 
    #   .[1:100, ]
    
    # compute the area of intersection between all CARs 
    sicar_intersection <- st_intersection(sicar, sicar) %>% 
      mutate(int_area= st_area(geometry))
    
    out <- sicar_intersection %>% 
      as.data.table() %>% copy() %>% 
      .[, geometry := NULL]
    
    fwrite(x = out, file = "data/cleaned/car_intersection_areas.csv")
    
  }
  
  # 2.1.2: identify the CARs that DO NOT have information  ---------
  
  # 2.1.2.1: first, separate out the CAncelled from NOT-CAncelled CARs
  cancelled_cars <- all_cars %>% copy() %>% 
    .[SITUACAO=="CA"] %>%
    .[!duplicated(COD_IMOVEL)]
  
  
  not_cancelled_cars <- all_cars %>% copy() %>% 
    .[SITUACAO!="CA"]   %>%
    .[!duplicated(COD_IMOVEL)]
  
  # 2.1.2.2: second, bind the IDs together  
  CAR_IDS <- rbind(cancelled_cars, not_cancelled_cars) %>%  .[, COD_IMOVEL] # ok, 
  
  if(fp$CHECKS){
    
    # are teh CAR_IDS unique? 
    CAR_IDS %>% length() ==  CAR_IDS %>% uniqueN()
    
  }
  
  #  get the CARs that were cancelled & have date information 
  CAR_IDS_WITH_DATES <- microdata_inc %>% 
    .[registro_car%in%CAR_IDS] %>% 
    .[!is.na(data_inscricao)] %>% 
    .[, registro_car]
  
  # search for the CARs that were cancelled & don't have date information
  CAR_IDS_WITHOUT_DATES <- CAR_IDS %>%  .[!.%in%CAR_IDS_WITH_DATES]
  
  CAR_IDS_WITHOUT_DATES_not_cancelled <- CAR_IDS_WITHOUT_DATES %>% .[(.%in%not_cancelled_cars[, COD_IMOVEL])]
  CAR_IDS_WITHOUT_DATES_cancelled <- CAR_IDS_WITHOUT_DATES %>% .[(.%in%cancelled_cars[, COD_IMOVEL])]
  
  if(fp$RUNNING_TESTS){
    
    # CHECK if unique number of not-cancelled & cancelled CARs WITHOUT DATES  + CARs in micro data == all of the cars
    total_missing_dates <- uniqueN(CAR_IDS_WITHOUT_DATES_not_cancelled) +   uniqueN(CAR_IDS_WITHOUT_DATES_cancelled)
    
    (microdata_inc[, uniqueN(registro_car)] + total_missing_dates ) == all_cars[, uniqueN(COD_IMOVEL)]
    
    combo <- microdata_inc[, unique(registro_car)] %>% 
      append(CAR_IDS_WITHOUT_DATES_not_cancelled) %>% 
      append(CAR_IDS_WITHOUT_DATES_cancelled) %>% 
      .[!duplicated(.)]
    
    uniqueN(combo) ==  all_cars[, uniqueN(COD_IMOVEL)]
    
    all_cars[COD_IMOVEL%in%CAR_IDS_WITHOUT_DATES_not_cancelled][, .N, SITUACAO]
    
    # which municipalities are the not cancelled ones from? 
    CAR_IDS_WITHOUT_DATES_not_cancelled %>% 
      substr(., 4, 10) %>% 
      data.table() %>% 
      .[, .N, `.`] # all over
    
    CAR_IDS_WITHOUT_DATES_not_cancelled[2000]
    
    
  }
  
  # 2.2: save these IDs to scrape them with Python 
  read_these <- CAR_IDS_WITHOUT_DATES %>%
    data.table() %>% 
    .[,     municipio := as.numeric(substr(`.`, 4, 10))] %>% 
    .[municipio %in% municipalities_amazon]
  
  fwrite(read_these, "data/processing/missing_car_dates/car_ids_muni.csv")
  
  rm(all_cars)
  gc()
  
  # section 2.2: merge in the cancelled CARs ------
  
  trial_one <- fread("data/processing/missing_car_dates/cancelled_car_dates.csv") %>% 
    .[, .(date, registro_car)] %>% 
    rename_columns(c("date"), c("data_inscricao_can"))
  
  trial_two <-fread("data/processing/missing_car_dates/202310_report_data_registro_car.csv") %>% 
    .[, .(data_registro, car)] %>% 
    rename_columns(c("data_registro", "car"), c("data_inscricao_can", "registro_car"))
  
  cancelled_cars_with_dates <- rbind(trial_one, trial_two) %>%
    .[, dd := substr(data_inscricao_can, 1,2)] %>% 
    .[, mm := substr(data_inscricao_can, 4,5)] %>% 
    .[, yyyy := substr(data_inscricao_can, 7,10)] %>% 
    .[, data_inscricao_can := paste0(yyyy, "-", mm, "-", dd)] %>% 
    .[, data_inscricao_can := as.IDate(data_inscricao_can)] %>% 
    .[, .(registro_car, data_inscricao_can)] 
  
  # section 2.3: update micro-data with information 
  
  microdata_inc %<>% copy() %>% 
    merge(x=., y=cancelled_cars_with_dates, "registro_car", all=T)  %>% 
    .[is.na(data_inscricao), cancelled := TRUE] %>% 
    .[, data_inscricao := as.IDate(data_inscricao)] %>% 
    .[is.na(data_inscricao), data_inscricao := data_inscricao_can] %>% 
    .[,  data_inscricao_can := NULL ]  %>% 
    .[is.na(codigo_ibge ), codigo_ibge :=substr(registro_car, 4, 10)] %>% 
    .[, uf:=substr(codigo_ibge, 1, 2)] %>% 
    .[!is.na(cancelled), situacao_cadastro := "CA"]  %>% 
    rename_columns(c("cancelled"), c("cancelled_indic")) %>% 
    .[, cancelled_indic := !is.na(cancelled_indic) ]
  
  
  microdata_inc %>% 
    # PURPOSEFULLY EXCLUDE CARs registered in 2023 ------
  .[year(data_inscricao) < 2023 ] %>% 
    fwrite(.,
           "data/raw/sicar/microdata/temas_ambientais_update.csv")
  
  # section 2.3: consolidate data infoRmation for CAR ids; save in simple CSV------
  
  years_dt <- data.table(year=c(2013:2022), merge_index=rep(1, 10)) 
  
  # create a balanced panel at the registro_car - year level
  car_and_reg_year_tmp <- microdata %>% copy() %>% 
    # get the registration year 
    .[, year_car_registration := year(data_inscricao)] %>% 
    # index at year - id_car level
    .[, .N, .(year_car_registration, registro_car)] %>%
    .[, .(year_car_registration, registro_car)] %>%
    # create a year identifier 
    .[, merge_index := 1] %>% 
    merge(years_dt, "merge_index", all=T, allow.cartesian=T)   %>% 
    # create a year identifier
    .[, year_car_reg_indic := (year_car_registration <= year)*1 ] %>% 
    .[, .(registro_car, year_car_reg_indic,year)] %>% 
    .[, year := paste0("y", year)]
  
  # take the CARs that were cancelled & still have missing data info and create a year variable that indicates the missing date
  car_and_reg_year_tmp_nas <- car_and_reg_year_tmp %>% copy() %>% 
    .[is.na(year_car_reg_indic)] %>% 
    .[!duplicated(registro_car)] %>% 
    .[, year := "yNA" ] %>% 
    .[, year_car_reg_indic := 1 ]
  
  # create WIDE data-set; bingin both back in and creating the final dataset indexed at the CARID level 
  car_and_reg_year_wide <-  car_and_reg_year_tmp %>% copy() %>% 
    .[, GRP := .GRP, .(registro_car, year)] %>% .[!duplicated(GRP)] %>% .[, GRP := NULL] %>% 
    setnafill(fill = 0, cols = c("year_car_reg_indic")) %>%
    dcast(registro_car~year, value.var = "year_car_reg_indic") %>% 
    # order 
    .[order(y2013,y2014,y2015,y2016,y2017,y2018,y2019, y2020,y2021, y2022)]
  
  fwrite(car_and_reg_year_wide, "data/processing/car_and_reg_year_wide.csv")
  
  rm(car_and_reg_year_tmp_nas, car_and_reg_year_tmp, years_dt, car_and_reg_year_wide)
  gc()
  
}

# section 3: Clean CARs -----------------
if(fp$CLEAN_CARS) {
  
  # section 6.0: set-up -------
  
  setwd(dir_wd)
  
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
  
  get_munis_already_completed <- function(INCLUDE_ANY=FALSE){
    
    # BEFORE identifying the files for this batch, identify which files have already been run and remove them from the list
    munis_already_completed_light <-
      paste0(dir_wd, "data/processing/CleanCARShapes_light/") %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    munis_already_completed_robust <-
      paste0(dir_wd, "data/processing/CleanCARShapes_robust/") %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    munis_already_completed_s2 <-
      paste0(dir_wd, "data/processing/CleanCARShapes_s2/") %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    munis_already_completed_s2_not <-
      paste0(dir_wd, "data/processing/CleanCARShapes_s2_false/") %>%
      list.files() %>%
      stri_extract_all_regex(., "\\d{7}") %>% unlist() %>%
      unique()
    
    munis1a <- munis_already_completed_light %>% .[.%in%munis_already_completed_robust]
    munis1b <-munis_already_completed_robust %>% .[.%in% munis_already_completed_light]
    munis1 <- munis1a %>% .[.%in% munis1b]
    
    munis2a <- munis_already_completed_robust %>% .[.%in%munis_already_completed_s2]
    munis2b <- munis_already_completed_s2 %>% .[.%in%munis_already_completed_robust]
    munis2 <- munis2a %>% .[.%in%munis2b]
    
    munis3a <- munis_already_completed_s2 %>% .[.%in%munis_already_completed_light]
    munis3b <-  munis_already_completed_light %>% .[.%in%munis_already_completed_s2]
    munis3 <- munis3a %>% .[.%in%munis3b]
    
    # munis4a <- munis_already_completed_s2_not %>% .[.%in%munis_already_completed_light]
    # munis4b <-  munis_already_completed_light %>% .[.%in%munis_already_completed_s2_not]
    # munis4c <-  munis_already_completed_robust %>% .[.%in%munis_already_completed_s2_not]
    # munis4d <-  munis_already_completed_s2_not %>% .[.%in%munis_already_completed_robust]
    # munis4 <- munis4a %>% .[.%in%munis4b] %>% .[.%in%munis4c] %>% .[.%in%munis4d]
    
    if(INCLUDE_ANY){
      munis_already_completed <- append(munis1, munis2) %>% append(munis3) %>% #append(munis4) %>%
        unique()
      
    }else{
      
      munis_already_completed  <- munis1 %>% .[.%in%munis2] %>% .[.%in%munis3] #%>% .[.%in%munis4]
      
    }
    
    
    return(munis_already_completed)
    
  }
  
  get_munis_whose_cars_need_to_be_cleaned <- function() {
    
    setwd(dir_wd)
    
    # identify all municipalities that will be cleaned
    all_munis <- property_directories %>%
      stri_extract_all_regex(`.`, "\\d{7}") %>%
      unlist
    
    munis_already_completed <- get_munis_already_completed()
    
    # identify municipalities that are still available
    munis_that_still_need_to_be_cleaned <-
      all_munis %>% .[!(. %in% munis_already_completed)]
    
    munis_that_still_need_to_be_cleaned %>%
      return()
    
  }
  
  update_claimed_muni_list <- function(){
    
    setwd(dir = dir_wd)
    
    FILE <- "data/processing/munis_already_claimed_CAR.csv"
    
    if (!file.exists(FILE)) {
      data.table(muni = c("")) %>%
        .[0] %>%
        fwrite(., FILE)
    }
    
    # only update with what was completed
    munis_already_completed <- get_munis_already_completed()
    
    munis_claimed_or_cleaned <- fread(FILE) %>% rbind(
      data.table(muni = munis_already_completed) ) %>% 
      .[!duplicated(muni)] 
    
    
    munis_claimed_or_cleaned %>% 
      fwrite(FILE)
    
    return(munis_claimed_or_cleaned)
    
    
  }
  
  # define function that selects and claims municipalities
  select_muni_to_clean_CAR <- function() {
    
    already_claimed <-
      fread("data/processing/munis_already_claimed_CAR.csv") 
    
    munis_already_completed <- get_munis_already_completed(INCLUDE_ANY=FALSE)
    
    this_muni <- available_munis %>%
      # make sure they are still available
      .[!. %in% munis_already_completed] %>%
      .[!. %in% already_claimed] %>%
      .[sample(length(.))] %>%
      .[1]
    
    if(!is.na(this_muni)){
      data.table(muni = this_muni) %>%
        fwrite("data/processing/munis_already_claimed_CAR.csv",
               append = T)
    }
    
    return(this_muni)
    
  }
  
  available_munis <- get_munis_whose_cars_need_to_be_cleaned()
  
  # section 6.1: while there is still an available municipality------
  while (length(available_munis) > 0) {
    
    # section 6.1.1: identify and select the muni we will work with in this iteration ------
    available_munis <- get_munis_whose_cars_need_to_be_cleaned() #%>% .[.%in%c(1200401)] # 1100205
    
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
      select_muni_to_clean_CAR()
    
    update_claimed_muni_list()
    
    # THIS_muni <- 1702208   
    # THIS_muni <- 1500347    
    # THIS_muni <- 5103957    
    # THIS_muni <- 2111300  
    # THIS_muni <- 1702208 1502509   #1508308 # 1505064
    
    if(fp$CHECKS){
      
      n_unique_cars_zero <- "data/processing/identifying_errors/n_unique_cars_zero.csv" %>% fread() %>% .[, .(municipio, year)]
      n_unique_conflicting_cars_zero <- "data/processing/identifying_errors/n_unique_conflicting_cars_zero.csv" %>% fread() %>% .[, .(municipio, year)]
      
      
      n_unique_cars_zero %>% 
        .[, .N, year]
      
      n_unique_conflicting_cars_zero %>% 
        .[, .N, year]
    }
    
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
      car <- PROPERTY_SHP  %>%
        read_sf() %>%
        st_transform(4674)  %>%
        # subset to relevant properties
        .[which(.$COD_IMOVEL %in% CARS_IN_SAMPLE), ]
      
      # 2: flag invalid polygons then convert them to valid ones ----
      car$flag_valid <- st_is_valid(car)
      
      # 2.1: if there are invalid CARs, save them to check later
      if(sum(car$flag_valid==FALSE)>0){
        
        # 2.1.1: assing the folder where we save the invalid CARs for municipality PROPERTY_MUNI
        dir_save_errors1 <- paste0(dir_wd, "data/processing/CAROverlap_invalid_preclean/muni", PROPERTY_MUNI, "/")
        
        # 2.1.2: save the originally invalid cars 
        # if the directory already exists, then skip this, as we have already done this step in another batch. 
        if(!dir.exists(dir_save_errors1)){
          
          # 2.1.2.1: create the directory
          dir.create(dir_save_errors1)
          
          # for manual checking
          if(fp$CHECKS){
            car[which(car$flag_valid==FALSE),] %>% View()
            car[which(car$flag_valid==FALSE),] %>% plot(., axes=T)
          }
          
          # 2.1.2.2: save ONLY the invalid CARs
          car[which(car$flag_valid==FALSE),] %>% 
            st_write(paste0(dir_save_errors1, PROPERTY_MUNI, ".shp"), append = TRUE)
          
          message_with_lines(paste0("CHECK LATER: Saved:", dir_save_errors1, "/",  PROPERTY_MUNI, ".shp"))
          
        }
        
      }
      
      # 3: lightly clean CARs & save -----
      # Regardless whether there are or aren't invalid CARS, lightly clean them, 
      # do not remove any cars in this step, and save this lightly cleaned version
      
      car %<>% clean_shape_basic()
      
      if(FALSE){
        
        dir_light_cleaned_cars <- paste0(dir_wd, "data/processing/CleanCARShapes_light/muni", PROPERTY_MUNI, "/")
        if(!dir.exists(dir_light_cleaned_cars)){dir.create(dir_light_cleaned_cars)}
        filename_light <- paste0(dir_light_cleaned_cars, "light",PROPERTY_MUNI, ".shp")
        if(!file.exists(filename_light)){
          car %>% st_write(., filename_light)
        }
        
        # 4.1: flag invalid polygons that have already been cleaned, drop them and save again ----
        car$flag_valid <- st_is_valid(car)
        
        # if there are invalid CARs, save them to check later. This time, attempt to clean in a more robust fashion.
        if(sum(car$flag_valid==FALSE)>0){
          
          # 4.1.1: assing the folder where we save the invalid CARs for municipality PROPERTY_MUNI
          dir_save_errors2 <- paste0("data/processing/CAROverlap_invalid_preclean/", PROPERTY_MUNI, "_second/")
          
          # 4.1.2: save the originally invalid cars 
          # if the directory already exists, then skip this, as we have already done this step in another batch. 
          if(!dir.exists(dir_save_errors2)){
            
            # 4.1.2.1: create the directory
            dir.create(dir_save_errors2)
            
            # for manual checking
            if(fp$CHECKS){
              car[which(car$flag_valid==FALSE),] %>% View()
              car[which(car$flag_valid==FALSE),] %>% plot(., axes=T)
            }
            
            # 4.1.2.2: save ONLY the invalid CARs
            car[which(car$flag_valid==FALSE),] %>% 
              st_write(paste0(dir_save_errors2, PROPERTY_MUNI, ".shp"), append = TRUE)
            
            message_with_lines(paste0("CHECK LATER: Saved:", dir_save_errors2, "/",  PROPERTY_MUNI, ".shp"))
            
          }
          
        }
        
        # 5: regardless of whether we have problematic CARs or not
        # now perform a more robust cleaning approach, which drops invalid CARs if they do not conform
        
        car_robust <- car %>% copy() %>% clean_shape_reenforced()
        
        dir_robust_cleaned_cars <- paste0(dir_wd, "data/processing/CleanCARShapes_robust/muni", PROPERTY_MUNI, "/")
        if(!dir.exists(dir_robust_cleaned_cars)){dir.create(dir_robust_cleaned_cars)}
        filename_robust <- paste0(dir_robust_cleaned_cars, "robust",PROPERTY_MUNI, ".shp")
        if(!file.exists(filename_robust)){
          car_robust %>% st_write(., filename_robust)
        }
        
        # 6: regardless of whether we have problematic CARs or not, convert to st_as_s2
        # now perform a more robust cleaning approach, which drops invalid CARs if they do not conform
        
        
        car_s2 <- car %>% copy() %>% clean_shape_s2() 
        
        dir_s2_cleaned_cars <- paste0(dir_wd, "data/processing/CleanCARShapes_s2/muni", PROPERTY_MUNI, "/")
        if(!dir.exists(dir_s2_cleaned_cars)){dir.create(dir_s2_cleaned_cars)}
        filename_s2 <- paste0(dir_s2_cleaned_cars, "s2_",PROPERTY_MUNI, ".shp")
        if(!file.exists(filename_s2)){
          car_s2 %>% st_write(., filename_s2)
        }
        
      }
      
      #  car_s2_robust_AREA <- car_robust %>% st_area()
      # #  
      #  car_s2_not_AREA <- car %>% copy() %>% st_as_s2(FALSE) %>% st_as_sf() %>%  st_area()
      # 
      #  car_s2_AREA <- car_s2 %>% st_area()
      #  
      #  car_AREA <- car %>% st_area()
      # 
      #  x <- data.table(s2_true=as.numeric(car_s2_AREA), s2_false=as.numeric(car_s2_not_AREA), never_used_s2 = as.numeric(car_AREA)) %>%
      #    .[, true_minus_false := s2_true - s2_false] %>% 
      #    .[, true_minus_never := s2_true - never_used_s2]
      # 
      # ggplot(x, aes(x=s2_true, y=s2_false)) +
      #    geom_point() +
      #    theme_bw() +
      #    theme(text = element_text(size=30))
      # 
      # ggplot(x, aes(x=s2_true, y=true_minus_never)) +
      #    geom_point() +
      #    theme_bw() +
      #    theme(text = element_text(size=30))
      # 
      #    ggplot(data = x, aes(x=s2_true, y=true_minus_false)) +
      #    geom_point() +
      #      geom_hline(yintercept = 0) +
      #    theme_bw() +
      #    theme(text = element_text(size=30))
      # 
      
      
      # 
      # # 7: regardless of whether we have problematic CARs or not, convert to st_as_s2(FALSE)
      # # now perform a more robust cleaning approach, which drops invalid CARs if they do not conform
      # 
      # car_s2 <- car %>% copy() %>% clean_shape_s2_NOT() 
      # 
      # dir_s2_cleaned_cars <- paste0(dir_wd, "data/processing/CleanCARShapes_s2/muni", PROPERTY_MUNI, "/")
      # if(!dir.exists(dir_s2_cleaned_cars)){dir.create(dir_s2_cleaned_cars)}
      # filename_s2 <- paste0(dir_s2_cleaned_cars, "s2_",PROPERTY_MUNI, ".shp")
      # if(!file.exists(filename_s2)){
      #   car_s2 %>% st_write(., filename_s2)
      # }
      # car_s2 <- car %>% copy() %>% clean_shape_s2_NOT() 
      
      
      
      
      
    }
    
    # update 
    available_munis <- get_munis_whose_cars_need_to_be_cleaned()    
    
  }
  
}

# get the updated micro-data  
microdata <- fread("data/raw/sicar/microdata/temas_ambientais_update.csv")
