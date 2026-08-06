library(stars)
library(data.table)
#library(emilfun)
library(grDevices)
library(strex)
library(plyr)
library(tidyr)
library(dplyr)
library(readr)
library(doParallel)
library(ggplot2)
library(gridExtra)
library(grid)
library(lubridate)
library(rasterVis)
library(data.table)
library(haven)
library(foreign)
library(stringr)
library(fixest)        # Para regressões eficientes com FEs
library(sandwich)      # Matriz robusta de variâncias (vcov)
library(lmtest)        # Testes estatísticos para regressão
library(clubSandwich)  # Erros robustos clusterizados

sf_use_s2(FALSE)

remove_overlaps_optimized <- function(polygons_sf) {
  # Create a copy of the original sf object to store the result
  polygons_no_overlap <- polygons_sf
  
  # Create a spatial index for faster searching of overlaps
  idx <- st_intersects(polygons_sf)
  
  # Iterate through each polygon
  for (i in 1:nrow(polygons_sf)) {
    # Get the indices of polygons that overlap with the i-th polygon
    overlapping_polygons <- idx[[i]]
    
    # Remove self-intersection (since a polygon always overlaps itself)
    overlapping_polygons <- overlapping_polygons[overlapping_polygons != i]
    
    # If there are any overlapping polygons
    if (length(overlapping_polygons) > 0) {
      # Union all overlapping polygons into one geometry
      overlap_union <- st_union(polygons_sf[overlapping_polygons, ])
      
      # Perform the difference operation
      result_geom <- st_difference(polygons_sf[i, ], overlap_union)
      
      # Check if the result geometry is valid and not empty
      if (!is.null(result_geom) && length(result_geom$geometry) > 0 && !st_is_empty(result_geom$geometry[[1]])) {
        # Replace the geometry for the i-th polygon
        polygons_no_overlap$geometry[i] <- result_geom$geometry[[1]]
      }
    }
  }
  
  return(polygons_no_overlap)
}

avisos_2005 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/warnings/deter_modis_2005.shp")
avisos_2006 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/warnings/deter_modis_2006.shp")
avisos_2007 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/warnings/deter_modis_2007.shp")
avisos_2008 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/warnings/deter_modis_2008.shp")
avisos_2009 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/warnings/deter_modis_2009.shp")
avisos_2010 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/warnings/deter_modis_2010.shp")
avisos_2011 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/warnings/deter_modis_2011.shp")
avisos_2012 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/warnings/deter_modis_2012.shp")
avisos_2013 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/warnings/deter_modis_2013.shp")
avisos_2014 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/warnings/deter_modis_2014.shp")

avisos_2005 <- avisos_2005 %>%
  mutate(
    # extrai a sequência de 8 dígitos entre underscores
    VIEW_DATE = as.Date(
      str_extract(as.character(layer), "(?<=_)(\\d{8})(?=_)"),
      format = "%Y%m%d"
    )
  )

avisos_2006 <- avisos_2006 %>%
  mutate(
    # extrai a sequência de 8 dígitos entre underscores
    VIEW_DATE = as.Date(
      str_extract(as.character(layer), "(?<=_)(\\d{8})(?=_)"),
      format = "%Y%m%d"
    )
  )

avisos_2007 <- avisos_2007 %>%
  mutate(
    # 1. transforma 'data' em character, caso seja factor
    data_chr = as.character(data),
    
    # 2. VIEW_DATE:
    VIEW_DATE = case_when(
      !is.na(data_chr) & data_chr != "" ~ dmy(data_chr),                          # usa DD-MM-YYYY
      TRUE                               ~ as.Date(                               # fallback: pega YYYYMMDD do 'layer'
        str_extract(as.character(layer), "(?<=_)(\\d{8})(?=_)"), "%Y%m%d")
    )
  ) %>%
  select(-data_chr)   # descarta coluna temporária

# dicionário PT-BR  → número do mês
mes_map <- c(
  "JANEIRO"   = 1,
  "FEVEREIRO" = 2,
  "MARÇO"     = 3, "MARCO" = 3,     # aceita com/sem acento cedilha
  "ABRIL"     = 4,
  "MAIO"      = 5,
  "JUNHO"     = 6,
  "JULHO"     = 7,
  "AGOSTO"    = 8,
  "SETEMBRO"  = 9,
  "OUTUBRO"   = 10,
  "NOVEMBRO"  = 11,
  "DEZEMBRO"  = 12
)

avisos_2008 <- avisos_2008 %>%
  mutate(
    DIA_num  = as.integer(DIA),
    ANO_num  = as.integer(ANO),
    MES_num  = mes_map[ toupper(str_trim(MES)) ],   # converte nome → número
    VIEW_DATE = make_date(year  = ANO_num,
                          month = MES_num,
                          day   = DIA_num)
  )

avisos_2009 <- avisos_2009 %>%
  mutate(
    DIA_num  = as.integer(dia),
    ANO_num  = as.integer(ano),
    MES_num  = mes_map[ toupper(str_trim(mes)) ],   # converte nome → número
    VIEW_DATE = make_date(year  = ANO_num,
                          month = MES_num,
                          day   = DIA_num)
  )

avisos_2010 <- avisos_2010 %>%
  mutate(
    DIA_num  = as.integer(DIA),
    ANO_num  = as.integer(ANO),
    MES_num  = mes_map[ toupper(str_trim(MES)) ],   # converte nome → número
    VIEW_DATE = make_date(year  = ANO_num,
                          month = MES_num,
                          day   = DIA_num)
  )

avisos_list <- mget(ls(pattern = "^avisos_\\d{4}$"))

avisos_list <- lapply(
  avisos_list,
  \(x) x %>% select(VIEW_DATE, geometry)   # geometry fica preservado
)

avisos <- do.call(rbind, avisos_list)



########################################################
autos_infracao_df <- readRDS("D:/Dropbox/amazon_project/miseEnPlace/autos_infracao_df.rds")

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Preparar o data.frame de infrações: filtrar lat/lon e converter para sf
# -------------------------------------------------------------------------
autos_infracao_clean <- autos_infracao_df %>%
  # Criar colunas temporárias que substituem vírgula por ponto
  mutate(
    LAT_CHAR = as.character(NUM_LATITUDE_AUTO),
    LON_CHAR = as.character(NUM_LONGITUDE_AUTO),
    # Trocar a vírgula decimal por ponto e converter para numérico
    LAT_NUM = as.numeric( gsub(",", ".", LAT_CHAR) ),
    LON_NUM = as.numeric( gsub(",", ".", LON_CHAR) )
  ) %>%
  # Filtrar para remover NAs, strings vazias e coordenadas "0,0"
  filter(
    !is.na(LAT_NUM)     & LAT_CHAR != "",
    !is.na(LON_NUM)     & LON_CHAR != "",
    !(LAT_CHAR == "0,0" & LON_CHAR == "0,0")
  )

# -------------------------------------------------------------------------
# Converter para objeto sf usando o mesmo CRS de 'avisos' (SIRGAS 2000)
# 
# -------------------------------------------------------------------------
infractions_sf <- st_as_sf(
  autos_infracao_clean,
  coords = c("LON_NUM", "LAT_NUM"),
  crs    = 4326,   
  remove = FALSE   # mantém as colunas NUM_LATITUDE_AUTO/LONGITUDE_AUTO
)

infractions_sf <- infractions_sf %>%
  mutate(
    DAT_HORA_CHAR = as.character(DAT_HORA_AUTO_INFRACAO),
    DATA_AUTO     = as.Date(
      DAT_HORA_CHAR,
      format = "%d/%m/%Y"   # note que aqui só lemos a parte "dia/mês/ano"
      # O R vai ignorar tudo que vier após o espaço (i.e., " HH:MM:SS")
    )
  ) %>%
  select(-DAT_HORA_CHAR)

# -------------------------------------------------------------------------
# Reprojetar 'avisos' e 'infractions_sf' para CRS métrico 
#    para que o buffer em metros faça sentido.
# -------------------------------------------------------------------------

avisos_m  <- st_transform(avisos,      crs = 3857)
infra_m   <- st_transform(infractions_sf, crs = 3857)

# Contagem de avisos em control e target

muni_control_areas <- read_sf("D:/AmazonBiomes/muni_control_area")
muni_target_areas <- read_sf("D:/AmazonBiomes/muni_target_area/muni_target_areas.gpkg")

muni_control_areas_m  <- st_transform(muni_control_areas,      crs = 3857)
muni_target_areas_m   <- st_transform(muni_target_areas, crs = 3857)

# -------------------------------------------------------------------------
# Garantir que avisos_m tem uma coluna "year" (ano do aviso)
# -------------------------------------------------------------------------
avisos_m <- avisos_m %>%
  mutate(year = year(VIEW_DATE))

# -------------------------------------------------------------------------
# Unir shapefiles de áreas numa única camada com rótulo de tipo
# -------------------------------------------------------------------------
muni_control_areas_m <- muni_control_areas_m %>%
  mutate(area_type = "control") %>%
  select(area_type, geometry)         # mantém só o necessário

muni_target_areas_m  <- muni_target_areas_m %>%
  mutate(area_type = "target") %>%
  select(area_type, geom)
muni_target_areas_m <- muni_target_areas_m %>%
  rename(geometry = geom) %>%   
  st_set_geometry("geometry")   

# Pilha as duas camadas numa só (mesmo CRS 3857)
areas_all <- rbind(muni_control_areas_m, muni_target_areas_m)



############################################################################
##BUFFER DE 0.5 ha  ───────────────────────────────────────────────────
############################################################################
raio_m_05 <- sqrt(5000 / pi)                         # ≈ 39,89 m
avisos_buf_05 <- st_buffer(avisos_m, dist = raio_m_05)

# multas dentro do buffer + janela de 6 meses
join_buf_inf_05 <- st_join(
  avisos_buf_05, infra_m, join = st_contains, left = FALSE
) %>% 
  filter(DATA_AUTO >= VIEW_DATE,
         DATA_AUTO <= VIEW_DATE %m+% months(6)) %>%
  mutate(year = year(VIEW_DATE))

# avisos → control/target
avisos_tagged_05 <- st_join(
  avisos_buf_05 %>% select(year), areas_all,
  join = st_intersects, left = FALSE
)

# multas → control/target
multas_tagged_05 <- st_join(
  join_buf_inf_05 %>% select(year), areas_all,
  join = st_intersects, left = FALSE
)

# contagens
avisos_count_05 <- avisos_tagged_05 %>% 
  st_drop_geometry() %>% 
  group_by(area_type, year) %>% 
  summarise(n_avisos = n(), .groups = "drop")

multas_count_05 <- multas_tagged_05 %>% 
  st_drop_geometry() %>% 
  group_by(area_type, year) %>% 
  summarise(n_multas = n(), .groups = "drop")

############################################################################
##BUFFER DE 1 ha  ─────────────────────────────────────────────────────
############################################################################
raio_m_1 <- sqrt(10000 / pi)                         # ≈ 56,42 m
avisos_buf_1 <- st_buffer(avisos_m, dist = raio_m_1)

join_buf_inf_1 <- st_join(
  avisos_buf_1, infra_m, join = st_contains, left = FALSE
) %>% 
  filter(DATA_AUTO >= VIEW_DATE,
         DATA_AUTO <= VIEW_DATE %m+% months(6)) %>%
  mutate(year = year(VIEW_DATE))

avisos_tagged_1 <- st_join(
  avisos_buf_1 %>% select(year), areas_all,
  join = st_intersects, left = FALSE
)

multas_tagged_1 <- st_join(
  join_buf_inf_1 %>% select(year), areas_all,
  join = st_intersects, left = FALSE
)

avisos_count_1 <- avisos_tagged_1 %>% 
  st_drop_geometry() %>% 
  group_by(area_type, year) %>% 
  summarise(n_avisos = n(), .groups = "drop")

multas_count_1 <- multas_tagged_1 %>% 
  st_drop_geometry() %>% 
  group_by(area_type, year) %>% 
  summarise(n_multas = n(), .groups = "drop")

############################################################################
## BUFFER DE 2 ha  ─────────────────────────────────────────────────────
############################################################################
raio_m_2 <- sqrt(20000 / pi)                         # ≈ 79,80 m
avisos_buf_2 <- st_buffer(avisos_m, dist = raio_m_2)

join_buf_inf_2 <- st_join(
  avisos_buf_2, infra_m, join = st_contains, left = FALSE
) %>% 
  filter(DATA_AUTO >= VIEW_DATE,
         DATA_AUTO <= VIEW_DATE %m+% months(6)) %>%
  mutate(year = year(VIEW_DATE))

avisos_tagged_2 <- st_join(
  avisos_buf_2 %>% select(year), areas_all,
  join = st_intersects, left = FALSE
)

multas_tagged_2 <- st_join(
  join_buf_inf_2 %>% select(year), areas_all,
  join = st_intersects, left = FALSE
)

avisos_count_2 <- avisos_tagged_2 %>% 
  st_drop_geometry() %>% 
  group_by(area_type, year) %>% 
  summarise(n_avisos = n(), .groups = "drop")

multas_count_2 <- multas_tagged_2 %>% 
  st_drop_geometry() %>% 
  group_by(area_type, year) %>% 
  summarise(n_multas = n(), .groups = "drop")

############################################################################
## RATIOS (NAIVE ESTIMATOR) (multas ÷ avisos)  por ano e tipo de área
############################################################################

## ----- 0.5 ha -------------------------------------------------------------
ratio_05 <- full_join(avisos_count_05, multas_count_05,
                      by = c("area_type", "year")) %>%
  mutate(
    fines_per_warning = ifelse(n_avisos > 0, n_multas / n_avisos, NA_real_),
    buffer_ha = 0.5
  )

## ----- 1 ha ---------------------------------------------------------------
ratio_1 <- full_join(avisos_count_1, multas_count_1,
                     by = c("area_type", "year")) %>%
  mutate(
    fines_per_warning = ifelse(n_avisos > 0, n_multas / n_avisos, NA_real_),
    buffer_ha = 1
  )

## ----- 2 ha ---------------------------------------------------------------
ratio_2 <- full_join(avisos_count_2, multas_count_2,
                     by = c("area_type", "year")) %>%
  mutate(
    fines_per_warning = ifelse(n_avisos > 0, n_multas / n_avisos, NA_real_),
    buffer_ha = 2
  )

## ----- Tabela final -------------------------------------------------------
ratio_all <- bind_rows(ratio_05, ratio_1, ratio_2) %>%
  arrange(buffer_ha, area_type, year)

print(ratio_all)


#############################################

output_folder <- "D:/Dropbox/amazon_project/fines_robustness"
output_filename <- "fines_per_warning_ratios.csv"
full_path <- file.path(output_folder, output_filename)
write.csv(ratio_all, file = full_path, row.names = FALSE)



muni_control_areas <- read_sf("D:/AmazonBiomes/muni_control_area")
muni_control_areasU <- st_union(muni_control_areas)
muni_target_areas <- read_sf("D:/AmazonBiomes/muni_target_area/muni_target_areas.gpkg")
muni_target_areasU <- st_union(muni_target_areas)
area_control_total <- st_area(muni_control_areasU)
area_target_total <- st_area(muni_target_areasU) 

##### CLEAR SPOTS #######

clear_spots200505 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200505/clear_spots200505.shp") 
clear_spots200506 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200506/clear_spots200506.shp") 
clear_spots200507 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200507/clear_spots200507.shp") 
clear_spots200508 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200508/clear_spots200508.shp") 
clear_spots200509 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200509/clear_spots200509.shp") 
clear_spots200510 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200510/clear_spots200510.shp") 
clear_spots200601 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200601/clear_spots200601.shp") 
clear_spots200603 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200603/clear_spots200603.shp") 
clear_spots200604 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200604/clear_spots200604.shp") 
clear_spots200605A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200605/clear_spots200605A.shp") 
clear_spots200605B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200605/clear_spots200605B.shp") 
clear_spots200606 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200606/clear_spots200606.shp") 
clear_spots200607A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200607/clear_spots200607A.shp") 
clear_spots200607B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200607/clear_spots200607B.shp") 
clear_spots200608A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200608/clear_spots200608A.shp") 
clear_spots200608B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200608/clear_spots200608B.shp") 
clear_spots200609A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200609/clear_spots200609A.shp") 
clear_spots200609B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200609/clear_spots200609B.shp") 
clear_spots200610 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200610/clear_spots200610.shp") 
clear_spots200704A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200704/clear_spots200704A.shp") 
clear_spots200704B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200704/clear_spots200704B.shp") 
clear_spots200705A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200705/clear_spots200705A.shp") 
clear_spots200705B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200705/clear_spots200705B.shp") 
clear_spots200706A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200706/clear_spots200706A.shp") 
clear_spots200706B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200706/clear_spots200706B.shp") 
clear_spots200707A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200707/clear_spots200707A.shp") 
clear_spots200707B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200707/clear_spots200707B.shp") 
clear_spots200708A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200708/clear_spots200708A.shp") 
clear_spots200708B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200708/clear_spots200708B.shp") 
clear_spots200709A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200709/clear_spots200709A.shp") 
clear_spots200709B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200709/clear_spots200709B.shp") 
clear_spots200710A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200710/clear_spots200710A.shp") 
clear_spots200710B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200710/clear_spots200710B.shp") 
clear_spots200711A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200711/clear_spots200711A.shp") 
clear_spots200711B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200711/clear_spots200711B.shp") 
clear_spots200712A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200712/clear_spots200712A.shp") 
clear_spots200712B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200712/clear_spots200712B.shp") 
clear_spots200801A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200801/clear_spots200801A.shp") 
clear_spots200801B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200801/clear_spots200801B.shp") 
clear_spots200802 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200802/clear_spots200802.shp") 
clear_spots200803A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200803/clear_spots200803A.shp") 
clear_spots200803B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200803/clear_spots200803B.shp") 
clear_spots200804 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200804/clear_spots200804.shp") 
clear_spots200805A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200805/clear_spots200805A.shp") 
clear_spots200805B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200805/clear_spots200805B.shp") 
clear_spots200806A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200806/clear_spots200806A.shp") 
clear_spots200806B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200806/clear_spots200806B.shp") 
clear_spots200807A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200807/clear_spots200807A.shp") 
clear_spots200807B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200807/clear_spots200807B.shp") 
clear_spots200808A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200808/clear_spots200808A.shp") 
clear_spots200808B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200808/clear_spots200808B.shp") 
clear_spots200809A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200809/clear_spots200809A.shp") 
clear_spots200809B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200809/clear_spots200809B.shp") 
clear_spots200810A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200810/clear_spots200810A.shp") 
clear_spots200810B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200810/clear_spots200810B.shp") 
clear_spots200811A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200811/clear_spots200811A.shp") 
clear_spots200811B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200811/clear_spots200811B.shp") 
clear_spots200812 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200812/clear_spots200812.shp") 
clear_spots200901 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200901/clear_spots200901.shp") 
clear_spots200902A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200902/clear_spots200902A.shp") 
clear_spots200902B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200902/clear_spots200902B.shp") 
clear_spots200903A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200903/clear_spots200903A.shp") 
clear_spots200903B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200903/clear_spots200903B.shp") 
clear_spots200904A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200904/clear_spots200904A.shp") 
clear_spots200904B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200904/clear_spots200904B.shp") 
clear_spots200905A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200905/clear_spots200905A.shp") 
clear_spots200905B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200905/clear_spots200905B.shp") 
clear_spots200906A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200906/clear_spots200906A.shp") 
clear_spots200906B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200906/clear_spots200906B.shp") 
clear_spots200907A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200907/clear_spots200907A.shp") 
clear_spots200907B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200907/clear_spots200907B.shp") 
clear_spots200908A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200908/clear_spots200908A.shp") 
clear_spots200908B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200908/clear_spots200908B.shp") 
clear_spots200909A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200909/clear_spots200909A.shp") 
clear_spots200909B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200909/clear_spots200909B.shp") 
clear_spots200910A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200910/clear_spots200910A.shp") 
clear_spots200910B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200910/clear_spots200910B.shp") 
clear_spots200911A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200911/clear_spots200911A.shp") 
clear_spots200911B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/200911/clear_spots200911B.shp") 
clear_spots201001A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201001/clear_spots201001A.shp") 
clear_spots201001B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201001/clear_spots201001B.shp") 
clear_spots201002A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201002/clear_spots201002A.shp") 
clear_spots201002B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201002/clear_spots201002B.shp") 
clear_spots201003A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201003/clear_spots201003A.shp") 
clear_spots201003B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201003/clear_spots201003B.shp") 
clear_spots201004A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201004/clear_spots201004A.shp") 
clear_spots201004B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201004/clear_spots201004B.shp") 
clear_spots201005A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201005/clear_spots201005A.shp") 
clear_spots201005B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201005/clear_spots201005B.shp") 
clear_spots201006A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201006/clear_spots201006A.shp") 
clear_spots201006B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201006/clear_spots201006B.shp") 
clear_spots201007A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201007/clear_spots201007A.shp") 
clear_spots201007B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201007/clear_spots201007B.shp") 
clear_spots201008A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201008/clear_spots201008A.shp") 
clear_spots201008B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201008/clear_spots201008B.shp") 
clear_spots201009A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201009/clear_spots201009A.shp") 
clear_spots201009B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201009/clear_spots201009B.shp") 
clear_spots201010A <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201010/clear_spots201010A.shp") 
clear_spots201010B <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201010/clear_spots201010B.shp") 
clear_spots201011 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201011/clear_spots201011.shp") 
clear_spots201012 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201012/clear_spots201012.shp") 
clear_spots201101 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201101/clear_spots201101.shp") 
clear_spots201102 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201102/clear_spots201102.shp") 
clear_spots201103 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201103/clear_spots201103.shp") 
clear_spots201104 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201104/clear_spots201104.shp") 
clear_spots201105 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201105/clear_spots201105.shp") 
clear_spots201106 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201106/clear_spots201106.shp") 
clear_spots201107 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201107/clear_spots201107.shp") 
clear_spots201108 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201108/clear_spots201108.shp") 
clear_spots201109 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201109/clear_spots201109.shp") 
clear_spots201110 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201110/clear_spots201110.shp") 
clear_spots201111 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201111/clear_spots201111.shp") 
clear_spots201112 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201112/clear_spots201112.shp") 
clear_spots201201 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201201/clear_spots201201.shp") 
clear_spots201202 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201202/clear_spots201202.shp") 
clear_spots201203 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201203/clear_spots201203.shp") 
clear_spots201204 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201204/clear_spots201204.shp") 
clear_spots201205 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201205/clear_spots201205.shp") 
clear_spots201206 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201206/clear_spots201206.shp") 
clear_spots201207 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201207/clear_spots201207.shp") 
clear_spots201208 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201208/clear_spots201208.shp") 
clear_spots201209 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201209/clear_spots201209.shp") 
clear_spots201210 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201210/clear_spots201210.shp") 
clear_spots201211 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201211/clear_spots201211.shp") 
clear_spots201212 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201212/clear_spots201212.shp") 
clear_spots201301 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201301/clear_spots201301.shp") 
clear_spots201302 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201302/clear_spots201302.shp") 
clear_spots201303 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201303/clear_spots201303.shp") 
clear_spots201304 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201304/clear_spots201304.shp") 
clear_spots201305 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201305/clear_spots201305.shp") 
clear_spots201306 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201306/clear_spots201306.shp") 
clear_spots201307 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201307/clear_spots201307.shp") 
clear_spots201308 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201308/clear_spots201308.shp") 
clear_spots201309 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201309/clear_spots201309.shp") 
clear_spots201310 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201310/clear_spots201310.shp") 
clear_spots201311 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201311/clear_spots201311.shp") 
clear_spots201312 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201312/clear_spots201312.shp") 
clear_spots201401 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201401/clear_spots201401.shp") 
clear_spots201402 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201402/clear_spots201402.shp") 
clear_spots201403 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201403/clear_spots201403.shp") 
clear_spots201404 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201404/clear_spots201404.shp") 
clear_spots201405 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201405/clear_spots201405.shp") 
clear_spots201406 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201406/clear_spots201406.shp") 
clear_spots201407 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201407/clear_spots201407.shp") 
clear_spots201408 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201408/clear_spots201408.shp") 
clear_spots201409 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201409/clear_spots201409.shp") 
clear_spots201410 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201410/clear_spots201410.shp") 
clear_spots201411 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201411/clear_spots201411.shp") 
clear_spots201412 <- sf::st_read("D:/Dropbox/amazon_project/fines_robustness/Clear Spots - Shapefile/201412/clear_spots201412.shp") 


############################

names <- ls(pattern = "^clear_spots")
shapefile_list <- mget(names)
new_names <- gsub("clear_spots", "", names(shapefile_list))
names(shapefile_list) <- new_names


visiblefraction_df <- data.frame(
  id = character(),
  fraction_control = numeric(),
  fraction_target = numeric(), 
  stringsAsFactors = FALSE
)

for (spot_id in names(shapefile_list)) {
  
  # Pega o shapefile atual da lista
  current_spot <- shapefile_list[[spot_id]]
  
  print(paste("Processando:", spot_id))
  # CONTROL AREAS
  intersection_control <- st_intersection(muni_control_areasU, current_spot)
  
  if (length(intersection_control) == 0) {
    area_intersection <- 0
  } else {
    area_intersection <- st_area(intersection_control)
  }
  
  # Calcular a 'visible_fraction'
  fraction_control <- area_intersection / area_control_total

  intersection_target <- st_intersection(muni_target_areasU, current_spot)
  
  if (length(intersection_target) == 0) {
    area_intersection_target <- 0
  } else {
    area_intersection_target <- st_area(intersection_target)
  }
  
  fraction_target <- area_intersection_target / area_target_total
  
  # --- Adicionar ambos os resultados ao data frame ---
  visiblefraction_df <- rbind(visiblefraction_df, data.frame(
    id = spot_id,
    fraction_control = as.numeric(fraction_control),
    fraction_target = as.numeric(fraction_target) 
  ))
}

print("Processamento em lote concluído!")

output_filename <- "visible_fraction.csv"
full_path <- file.path(output_folder, output_filename)
write.csv(visiblefraction_df, file = full_path, row.names = FALSE)
