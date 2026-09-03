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

## CLOUDS (OPEN AT YOUR OWN RISK) #################
cloud_200505 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2005/Nuvem_20050531_shp/nuvem_20050531_pol.shp")
cloud_200506 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2005/Nuvem_20050630_shp/Nuvem_20050630_pol.shp")
cloud_200507 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2005/Nuvem_20050730_shp/nuvem_20050730_pol.shp")
cloud_200508 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2005/Nuvem_20050830_shp/Nuvem_20050830_pol.shp")
cloud_200509 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2005/Nuvem_20050930_shp/nuvem_20050930_pol.shp")
cloud_200510 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2005/Nuvem_20051030_shp/nuvem_20051030_pol.shp")

cloud_200601 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20060131_shp/Nuvem_20060131_pol.shp")
cloud_200603 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20060330_shp/Nuvem_20060330_pol.shp")
cloud_200604 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20060430_shp/Nuvem_20060430_pol.shp")
cloud_200605_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20060531_shp/Nuvem_20060531_A_pol.shp")
cloud_200605_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20060531_shp/Nuvem_20060531_B_pol.shp")
cloud_200606 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20060630_shp/Nuvens_jun2006_pol.shp")
cloud_200607_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20060731_shp/Nuvem_jul2006_A-inpe_pol.shp")
cloud_200607_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20060731_shp/Nuvem_jul2006_B-inpe_pol.shp")
cloud_200608_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20060831_shp/Nuvem_200608_A_pol.shp")
cloud_200608_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20060831_shp/Nuvem_200608_B_pol.shp")
cloud_200609_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20060930_shp/Nuvem_20060930_A_pol.shp")
cloud_200609_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20060930_shp/Nuvem_20060930_B_pol.shp")
cloud_200610 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2006/Nuvem_20061031_shp/Nuvem_out2006_pol.shp")

cloud_200704_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20070430_shp/Nuven_2007_Abril_A_pol.shp")
cloud_200704_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20070430_shp/Nuvens_2007_Abril_B_pol.shp")
cloud_200705_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20070530_shp/Nuvens_2007_05_A_pol.shp")
cloud_200705_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20070530_shp/Nuvens_2007_05_B_pol.shp")
cloud_200706_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20070630_shp/Nuvens_2007_Jun_A_pol.shp")
cloud_200706_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20070630_shp/Nuvens_2007_Jun_B_pol.shp")
cloud_200707_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20070730_shp/Nuvens_2007_Jul_A_pol.shp")
cloud_200707_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20070730_shp/Nuvens_2007_Jul_B_pol.shp")
cloud_200708_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20070830_shp/Nuvens_2007_Ago_A_pol.shp")
cloud_200708_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20070830_shp/Nuvens_2007_Ago_B_pol.shp")
cloud_200709_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20070930_shp/Nuvens_Set_A_pol.shp")
cloud_200709_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20070930_shp/Nuvens_2007_Set_B_pol.shp")
cloud_200710_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20071030_shp/Nuvens_2007_Out_A_pol.shp")
cloud_200710_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20071030_shp/Nuvens_2007_Out_B_pol.shp")
cloud_200711_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20071130_shp/Nuvens_2007_11A_pol.shp")
cloud_200711_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20071130_shp/Nuvens_2007_11B_pol.shp")
cloud_200712_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20071230_shp/Nuvens_2007_12_A_pol.shp")
cloud_200712_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2007/Nuvem_20071230_shp/Nuvens_2007_12_B_pol.shp")

cloud_200801_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080130_shp/DETER2008_JAN_1_NUVEM_pol.shp")
cloud_200801_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080130_shp/DETER2008_JAN_2_NUVEM_pol.shp")
cloud_200802 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080228_shp/DETER2008_FEV_NUVEM_pol.shp")
cloud_200803_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080330_shp/DETER2008_MAR_1_NUVEM_pol.shp")
cloud_200803_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080330_shp/DETER2008_MAR_2_NUVEM_pol.shp")
cloud_200804 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080430_shp/nuvem_20080430_pol.shp")
cloud_200805_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080530_shp/DETER2008_MAI_1_NUVEM_pol.shp")
cloud_200805_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080530_shp/DETER2008_MAI_2_NUVEM_pol.shp")
cloud_200806_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080630_shp/DETER2008_JUNHO_01_NUVEM_pol.shp")
cloud_200806_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080630_shp/DETER2008_JUNHO_02_NUVEM_pol.shp")
cloud_200807_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080730_shp/DETER2008_JULHO_01_NUVEM_pol.shp")
cloud_200807_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080730_shp/DETER2008_JULHO_02_NUVEM_pol.shp")
cloud_200808_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080830_shp/DETER2008_AGOSTO_01_NUVEM_pol.shp")
cloud_200808_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvem_20080830_shp/DETER2008_AGOSTO_02_NUVEM_pol.shp")
cloud_200809_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvens_20080930_shp/DETER2008_SETEMBRO_01_NUVEM_pol.shp")
cloud_200809_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvens_20080930_shp/DETER2008_SETEMBRO_02_NUVEM_pol.shp")
cloud_200810_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvens_20081030_shp/DETER2008_OUTUBRO_01_NUVEM_pol.shp")
cloud_200810_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvens_20081030_shp/DETER2008_OUTUBRO_02_NUVEM_pol.shp")
cloud_200811_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvens_20081130_shp/DETER2008_NOVEMBRO_01_NUVEM_pol.shp")
cloud_200811_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvens_20081130_shp/DETER2008_NOVEMBRO_02_NUVEM_pol.shp")
cloud_200812 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2008/Nuvens_20081231_shp/DETER2008_DEZEMBRO_NUVEM_pol.shp")

cloud_200901 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090131_shp/DETER2009_JANEIRO_NUVEM_pol.shp")
cloud_200902_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090228_shp/DETER2009_FEVEREIRO_01_NUVEM_pol.shp")
cloud_200902_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090228_shp/DETER2009_FEVEREIRO_02_NUVEM_pol.shp")
cloud_200903_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090331_shp/DETER2009_MARCO_01_NUVEM_pol.shp")
cloud_200903_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090331_shp/DETER2009_MARCO_02_NUVEM_pol.shp")
cloud_200904_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090430_shp/DETER2009_ABRIL_01_NUVEM_pol.shp")
cloud_200904_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090430_shp/DETER2009_ABRIL_02_NUVEM_pol.shp")
cloud_200905_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090531_shp/DETER2009_MAIO_01_NUVEM_pol.shp")
cloud_200905_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090531_shp/DETER2009_MAIO_02_NUVEM_pol.shp")
cloud_200906_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090630_shp/DETER2009_JUNHO_01_NUVEM_pol.shp")
cloud_200906_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090630_shp/DETER2009_JUNHO_02_NUVEM_pol.shp")
cloud_200907_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090731_shp/DETER2009_JULHO_01_NUVEM_pol.shp")
cloud_200907_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090731_shp/DETER2009_JULHO_02_NUVEM_pol.shp")
cloud_200908_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090831_shp/DETER2009_AGOSTO_01_NUVEM_pol.shp")
cloud_200908_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090831_shp/DETER2009_AGOSTO_02_NUVEM_pol.shp")
cloud_200909_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090930_shp/DETER2009_SETEMBRO_01_NUVEM_pol.shp")
cloud_200909_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20090930_shp/DETER2009_SETEMBRO_02_NUVEM_pol.shp")
cloud_200910_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20091031_shp/DETER2009_OUTUBRO_01_NUVEM_pol.shp")
cloud_200910_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20091031_shp/DETER2009_OUTUBRO_02_NUVEM_pol.shp")
cloud_200911_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20091130_shp/DETER2009_NOVEMBRO_01_NUVEM_pol.shp")
cloud_200911_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2009/Nuvens_20091130_shp/DETER2009_NOVEMBRO_02_NUVEM_pol.shp")

cloud_201001_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100131_shp/DETER2010_JANEIRO_01_NUVEM_pol.shp")
cloud_201001_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100131_shp/DETER2010_JANEIRO_02_NUVEM_pol.shp")
cloud_201002_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100228_shp/DETER2010_FEVEREIRO_01_NUVEM_pol.shp")
cloud_201002_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100228_shp/DETER2010_FEVEREIRO_02_NUVEM_pol.shp")
cloud_201003_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100331_shp/DETER2010_MARCO_01_NUVEM_pol.shp")
cloud_201003_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100331_shp/DETER2010_MARCO_02_NUVEM_pol.shp")
cloud_201004_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100430_shp/DETER2010_ABRIL_01_NUVEM_pol.shp")
cloud_201004_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100430_shp/DETER2010_ABRIL_02_NUVEM_pol.shp")
cloud_201005_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100531_shp/DETER2010_MAIO_01_NUVEM_pol.shp")
cloud_201005_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100531_shp/DETER2010_MAIO_02_NUVEM_pol.shp")
cloud_201006_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100630_shp/DETER2010_JUNHO_01_NUVEM_pol.shp")
cloud_201006_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100630_shp/DETER2010_JUNHO02_NUVEM_pol.shp")
cloud_201007_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100731_shp/DETER2010_JULHO_01_NUVEM_pol.shp")
cloud_201007_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100731_shp/DETER2010_JULHO_02_NUVEM_pol.shp")
cloud_201008_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100831_shp/DETER2010_AGOSTO_01_NUVEM_pol.shp")
cloud_201008_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100831_shp/DETER2010_AGOSTO_02_NUVEM_pol.shp")
cloud_201009_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100930_shp/DETER2010_SETEMBRO_01_NUVEM_pol.shp")
cloud_201009_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20100930_shp/DETER2010_SETEMBRO_02_NUVEM_pol.shp")
cloud_201010_A <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20101031_shp/DETER2010_OUTUBRO_01_NUVEM_pol.shp")
cloud_201010_B <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20101031_shp/DETER2010_OUTUBRO_02_NUVEM_pol.shp")
cloud_201011 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20101130_shp/DETER2010_NOVEMBRO_01_NUVEM_pol.shp")
cloud_201012 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2010/Nuvens_20101231_shp/DETER2010_DEZEMBRO_NUVEM_pol.shp")

cloud_201101 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2011/Nuvens_201101_shp/DETER2011_JANEIR0_NUVEM_pol.shp")
cloud_201102 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2011/Nuvens_201102_shp/DETER2011_FEVEREIRO_NUVEM_pol.shp")
cloud_201103 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2011/Nuvens_201103_shp/DETER2011_MARCO_NUVEM_pol.shp")
cloud_201104 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2011/Nuvens_201104_shp/DETER2011_ABRIL_NUVEM_pol.shp")
cloud_201105 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2011/Nuvens_201105_shp/DETER2011_MAIO_NUVEM_pol.shp")
cloud_201106 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2011/Nuvens_201106_shp/DETER2011_JUNHO_NUVEM_pol.shp")
cloud_201107 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2011/Nuvens_201107_shp/DETER2011_JULHO_NUVEM_pol.shp")
cloud_201108 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2011/Nuvens_201108_shp/DETER2011_AGOSTO_NUVEM_pol.shp")
cloud_201109 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2011/Nuvens_201109_shp/DETER2011_SETEMBRO_NUVEM_pol.shp")
cloud_201110 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2011/Nuvens_201110_shp/DETER2011_OUTUBRO_NUVEM_pol.shp")
cloud_201111 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2011/Nuvens_201111_shp/DETER2011_NOVEMBRO_NUVEM_pol.shp")
cloud_201112 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2011/Nuvens_201112_shp/DETER2011_DEZEMBRO_NUVEM_pol.shp")

cloud_201201 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2012/Nuvens_201201_shp/DETER2012_JANEIRO_NUVEM_pol.shp")
cloud_201202 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2012/Nuvens_201202_shp/DETER2012_FEVEREIRO_NUVEM_pol.shp")
cloud_201203 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2012/Nuvens_201203_shp/DETER2012_MARCO_NUVEM_pol.shp")
cloud_201204 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2012/Nuvens_201204_shp/DETER2012_ABRIL_NUVEM_pol.shp")
cloud_201205 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2012/Nuvens_201205_shp/DETER2012_MAIO_NUVEM_pol.shp")
cloud_201206 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2012/Nuvens_201206_shp/DETER2012_JUNHO_NUVEM_pol.shp")
cloud_201207 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2012/Nuvens_201207_shp/DETER2012_JULHO_NUVEM_pol.shp")
cloud_201208 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2012/Nuvens_201208_shp/DETER2012_AGOSTO_NUVEM_pol.shp")
cloud_201209 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2012/Nuvens_201209_shp/DETER2012_SETEMBRO_NUVEM_pol.shp")
cloud_201210 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2012/Nuvens_201210_shp/DETER2012_OUTUBRO_NUVEM_NUVEM__pol.shp")
cloud_201211 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2012/Nuvens_201211_shp/DETER2012_NOVEMBRO_NUVEM__pol.shp")
cloud_201212 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2012/Nuvens_201212_shp/DETER2012_DEZEMBRO_NUVEM_pol.shp")

cloud_201301 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201301_shp/DETER2013_JANEIRO_NUVEM_pol.shp")
cloud_201302 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201302_shp/DETER2013_FEVEREIRO_NUVEM_pol.shp")
cloud_201303 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201303_shp/DETER2013_MARCO_NUVEM_pol.shp")
cloud_201304 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201304_shp/DETER2013_ABRIL_NUVEM_pol.shp")
cloud_201305 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201305_shp/DETER2013_MAIO_NUVEM_pol.shp")
cloud_201306 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201306_shp/DETER2013_JUNHO_NUVEM_pol.shp")
cloud_201307 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201307_shp/DETER2013_JULHO_NUVEM_pol.shp")
cloud_201308 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201308_shp/DETER2013_AGOSTO_NUVEM_pol.shp")
cloud_201309 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201309_shp/DETER2013_SETEMBRO_NUVEM_pol.shp")
cloud_201310 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201310_shp/DETER2013_OUTUBRO_NUVEM_pol.shp")
cloud_201311 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201311_shp/DETER2013_NOVEMBRO_NUVEM_pol.shp")
cloud_201312 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201312_shp/DETER2013_DEZEMBRO_NUVEM_pol.shp")

cloud_201301 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201301_shp/DETER2013_JANEIRO_NUVEM_pol.shp")
cloud_201302 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201302_shp/DETER2013_FEVEREIRO_NUVEM_pol.shp")
cloud_201303 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201303_shp/DETER2013_MARCO_NUVEM_pol.shp")
cloud_201304 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201304_shp/DETER2013_ABRIL_NUVEM_pol.shp")
cloud_201305 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201305_shp/DETER2013_MAIO_NUVEM_pol.shp")
cloud_201306 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201306_shp/DETER2013_JUNHO_NUVEM_pol.shp")
cloud_201307 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201307_shp/DETER2013_JULHO_NUVEM_pol.shp")
cloud_201308 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201308_shp/DETER2013_AGOSTO_NUVEM_pol.shp")
cloud_201309 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201309_shp/DETER2013_SETEMBRO_NUVEM_pol.shp")
cloud_201310 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201310_shp/DETER2013_OUTUBRO_NUVEM_pol.shp")
cloud_201311 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201311_shp/DETER2013_NOVEMBRO_NUVEM_pol.shp")
cloud_201312 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2013/Nuvens_201312_shp/DETER2013_DEZEMBRO_NUVEM_pol.shp")

cloud_201401 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2014/Nuvens_201401_shp/DETER2014_JANEIRO_NUVEM_pol.shp")
cloud_201402 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2014/Nuvens_201402_shp/DETER2014_FEVEREIRO_NUVEM_pol.shp")
cloud_201403 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2014/Nuvens_201403_shp/DETER2014_MARCO_NUVEM_pol.shp")
cloud_201404 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2014/Nuvens_201404_shp/DETER2014_ABRIL_NUVEM_pol.shp")
cloud_201405 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2014/Nuvens_201405_shp/DETER2014_MAIO_NUVEM_pol.shp")
cloud_201406 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2014/Nuvens_201406_shp/DETER2014_JUNHO_NUVEM_pol.shp")
cloud_201407 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2014/Nuvens_201407_shp/DETER2014_JULHO_NUVEM_pol.shp")
cloud_201408 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2014/Nuvens_201408_shp/DETER2014_AGOSTO_NUVEM_pol.shp")
cloud_201409 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2014/Nuvens_201409_shp/DETER2014_SETEMBRO_NUVEM_pol.shp")
cloud_201410 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2014/Nuvens_201410_shp/DETER2014_OUTUBRO_NUVEM_pol.shp")
cloud_201411 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2014/Nuvens_201411_shp/DETER2014_NOVEMBRO_NUVEM_pol.shp")
cloud_201412 <- st_read("D:/Dropbox/amazon_project/data/input/warnings/clouds/2014/Nuvens_201412_shp/DETER2014_DEZEMBRO_NUVEM_pol.shp")

############################################################################
#EMPILHAR TODOS OS SHAPEFILES MENSAIS DE REGIÕES SEM NUVEM -------------------------
############################################################################

cloud_names <- ls(pattern = "^cloud_\\d{6}(_[A-Z])?$")
## ─────────────────────────────────────────────────────────────
# --- Etapa 1: Carregar amazon_border e definir o CRS de referência ---
# Substitua pelo caminho correto do seu arquivo amazon_border
amazon_border_original <- st_read("D:/Dropbox/amazon_project/data/input/auxiliary/amazon_biome_border/amazon_biome_border.shp")
crs_referencia <- st_crs(amazon_border_original)
print(paste("CRS de Referência (do amazon_border original):", crs_referencia$input)) # Deve ser SIRGAS 2000

# Preparar 'amazon_border' para a operação de diferença, mantendo no CRS de referência
# Validar e, opcionalmente, unir se for composto por múltiplas feições que representam uma única região.
amazon_border_para_diferenca <- st_make_valid(amazon_border_original)
# Se 'amazon_border_original' tiver múltiplas feições que devem ser tratadas como uma só para a diferença:
# amazon_border_para_diferenca <- st_union(amazon_border_para_diferenca) %>% st_as_sf()
# Certifique-se que st_crs(amazon_border_para_diferenca) ainda é crs_referencia.



# --- Etapa 3: Processar os arquivos de nuvem ---
# Seus objetos de nuvem já estão carregados (cloud_200505, etc.)
# E cloud_names está definido

print("Iniciando o processamento dos arquivos de nuvem no CRS de Referência...")

cloud_list_processed <- lapply(cloud_names, function(obj_name) {
  message(paste("Processando:", obj_name))
  cloud_data_raw <- get(obj_name)
  
  # Sub-etapa 3.1: Checar CRS da nuvem e padronizar para o crs_referencia.
  current_cloud_crs <- st_crs(cloud_data_raw)
  if (is.na(current_cloud_crs)) {
    message(paste("--- CRS ausente para", obj_name, "-> Atribuindo CRS de Referência:", crs_referencia$input))
    st_crs(cloud_data_raw) <- crs_referencia
  } else if (current_cloud_crs != crs_referencia) {
    message(paste("--- Transformando", obj_name, "de", current_cloud_crs$input, "para CRS de Referência:", crs_referencia$input))
    cloud_data_raw <- st_transform(cloud_data_raw, crs_referencia)
  }
  # cloud_data_raw está agora no crs_referencia.
  
  cloud_data_valid <- st_make_valid(cloud_data_raw)
  
  if (nrow(cloud_data_valid) == 0 || all(st_is_empty(cloud_data_valid))) {
    message(paste("---", obj_name, "está vazio ou não contém geometrias válidas. Pulando."))
    year_month_str <- str_extract(obj_name, "\\d{6}")
    year_val <- as.integer(substr(year_month_str, 1, 4))
    month_val <- as.integer(substr(year_month_str, 5, 6))
    return(st_sf(data.frame(year = year_val, month = month_val), 
                 geometry = st_sfc(crs = crs_referencia))[0, ]) # sf vazio no crs_referencia
  }
  
  # Sub-etapa 3.2: Calcular a diferença (tudo no crs_referencia)
  clear_area_result <- tryCatch({
    st_difference(amazon_border_para_diferenca, cloud_data_valid)
  }, error = function(e) {
    message(paste("--- Erro em st_difference para", obj_name, ":", e$message))
    # Fallback: tentar com união das nuvens se amazon_border_para_diferenca for uma única geometria.
    if (nrow(amazon_border_para_diferenca) == 1 && inherits(st_geometry(amazon_border_para_diferenca), c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
      message(paste("--- Tentando st_difference com nuvens unidas para", obj_name))
      cloud_union <- st_union(st_geometry(cloud_data_valid))
      if(st_is_empty(cloud_union)) return(NULL)
      diff_sfc <- st_difference(st_geometry(amazon_border_para_diferenca), cloud_union)
      return(st_as_sf(diff_sfc)) # Converte sfc para sf
    }
    return(NULL)
  })
  
  if (is.null(clear_area_result) || nrow(clear_area_result) == 0 || all(st_is_empty(clear_area_result))) {
    message(paste("--- Nenhuma área clara resultante ou erro na diferença para", obj_name))
    year_month_str <- str_extract(obj_name, "\\d{6}")
    year_val <- as.integer(substr(year_month_str, 1, 4))
    month_val <- as.integer(substr(year_month_str, 5, 6))
    return(st_sf(data.frame(year = year_val, month = month_val), 
                 geometry = st_sfc(crs = crs_referencia))[0, ])
  }
  
  clear_area_clean <- clear_area_result %>%
    st_collection_extract("POLYGON") %>%
    filter(!st_is_empty(.))
  
  if (nrow(clear_area_clean) == 0) {
    message(paste("--- Nenhuma geometria poligonal válida restante para", obj_name, "após limpeza."))
    year_month_str <- str_extract(obj_name, "\\d{6}")
    year_val <- as.integer(substr(year_month_str, 1, 4))
    month_val <- as.integer(substr(year_month_str, 5, 6))
    return(st_sf(data.frame(year = year_val, month = month_val), 
                 geometry = st_sfc(crs = crs_referencia))[0, ])
  }
  
  # Sub-etapa 3.3: Adicionar ano/mês e selecionar colunas. O resultado já está no crs_referencia.
  year_month_str <- str_extract(obj_name, "\\d{6}")
  clear_area_clean$year <- as.integer(substr(year_month_str, 1, 4))
  clear_area_clean$month <- as.integer(substr(year_month_str, 5, 6))
  
  final_selection <- clear_area_clean %>% 
    select(year, month, any_of(st_geometry_names(clear_area_clean)))
  
  return(final_selection)
})

# Empilhe tudo num único sf.
# Renomeie para refletir que são áreas SEM nuvens e estão no CRS de referência.
areas_sem_nuvens_ref_crs <- do.call(rbind, cloud_list_processed)

if (nrow(areas_sem_nuvens_ref_crs) > 0) {
  areas_sem_nuvens_ref_crs <- areas_sem_nuvens_ref_crs %>% filter(!st_is_empty(st_geometry(.)))
}

# Confira o resultado
if (nrow(areas_sem_nuvens_ref_crs) > 0) {
  print(paste("CRS final das áreas sem nuvens:", st_crs(areas_sem_nuvens_ref_crs)$input)) # Deve ser o CRS de referência
  print("Primeiras linhas das áreas sem nuvens processadas:")
  print(head(areas_sem_nuvens_ref_crs))
  print(paste("Total de feições de áreas sem nuvens:", nrow(areas_sem_nuvens_ref_crs)))
} else {
  print("Nenhuma área sem nuvens foi processada ou todas resultaram vazias.")
}

clouds_all <- areas_sem_nuvens_ref_crs 

############################################################################
#PARA CADA ÁREA (control/target) CONTAR MESES SEM NUVEM ---------------
############################################################################
clouds_all    <- st_make_valid(clouds_all)
areas_all_a <- st_transform(areas_all, crs=crs_referencia)
areas_all_a     <- st_make_valid(areas_all_a)      # 2 polígonos grandes

# marca APENAS (área, ano, mês) que tiveram pelo menos UMA nuvem
cloud_tagged <- st_join(
  areas_all_a %>% select(area_type),
  clouds_all,
  join = st_intersects,
  left = FALSE
) %>%
  st_drop_geometry() %>%
  distinct(area_type, year, month) %>%      # <-- remove duplicados!
  mutate(cloudy = 0)

# completa os pares que faltam (viram cloudy = 0)
panel <- expand_grid(
  area_type = c("control", "target"),
  year      = 2005:2014,
  month     = 1:12
) %>%
  left_join(cloud_tagged, by = c("area_type", "year", "month")) %>%
  mutate(cloudy = replace_na(cloudy, 1))

# conta meses claros / nublados por ANO
cloud_year <- panel %>%
  group_by(area_type, year) %>%
  summarise(
    CloudyMonths = sum(cloudy),
    ClearMonths  = 12 - CloudyMonths,
    .groups = "drop"
  )

print(cloud_year)
############################################################################
#FUNÇÃO PARA CALCULAR π̂_cloud POR RAIO --------------------------------
############################################################################
make_pi_cloud <- function(warnings, fines, buf){
  full_join(warnings, fines, by = c("area_type", "year")) %>%   # W + F
    left_join(cloud_year, by = c("area_type", "year")) %>%      # + nuvem
    mutate(
      W_adj = n_avisos + (n_avisos / ClearMonths) * CloudyMonths,
      pi_cloud = n_multas / W_adj,
      buffer_ha = buf
    ) %>% 
    select(buffer_ha, area_type, year,
           n_avisos, n_multas, ClearMonths, CloudyMonths, pi_cloud)
}

pi_cloud_05 <- make_pi_cloud(avisos_count_05, multas_count_05, 0.5)
pi_cloud_1  <- make_pi_cloud(avisos_count_1 , multas_count_1 , 1)
pi_cloud_2  <- make_pi_cloud(avisos_count_2 , multas_count_2 , 2)

pi_cloud_all <- bind_rows(pi_cloud_05, pi_cloud_1, pi_cloud_2) %>% 
  arrange(buffer_ha, area_type, year)

print(pi_cloud_all)

