
#----------------------------------#
#--- INTEGRATE 
#---
#----------------------------------#
sf_use_s2(FALSE)
#---load Amazon biome boundaries---#  
amazon_bioma <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/aux/amazon_biome_border/amazon_biome_border.shp")
#terras indigenas
indigenous <- read_sf("/Users/pedrotremacoldirossi/Downloads/indigenous_area_amazon_biome/indigenous_area_amazon_biome.shp")
#unidades de conservacao
conservation <- read_sf("/Users/pedrotremacoldirossi/Downloads/conservation_units_amazon_biome")
#---load glebas federais in TLP---#  
glebas <- read_sf("/Users/pedrotremacoldirossi/Downloads/glebas_federais/i3geomap_glebas_federais.shp")
glebas <- st_intersection(glebas, amazon_bioma)
glebas <- gSimplify(methods::as(object = glebas, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
#---load states---#  
states <- read_sf("/Users/pedrotremacoldirossi/Downloads/BR_UF_2021/BR_UF_2021.shp") %>% st_make_valid()
states <- st_intersection(states, amazon_bioma)
amazon_bioma <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
#states <- sf::st_simplify(states)
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
colors <- c("gray80", "#02401B", "#F1BB7B", "#0B775E")
labels <- c("Other", "Forest", "Deforested", "Reforested")
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#florestas publicas nao designadas
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/cnfp/CNFP 2020 Shapefiles/", pattern="*.shp", recursive = TRUE)
fpnd <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/cnfp/CNFP 2020 Shapefiles/", f))
  fpnd <- rbind(fpnd, s)
}
nonFederal <- fpnd %>% filter(governo %in% c("ESTADUAL", "MUNICIPAL"))
nonFederal <- st_intersection(nonFederal, amazon_bioma)

nonFederal1 <- nonFederal[st_geometry_type(nonFederal) == "MULTIPOLYGON", ]
nonFederal2 <- nonFederal[st_geometry_type(nonFederal) == "POLYGON", ]
nonFederal3 <- nonFederal[str_detect(st_geometry_type(nonFederal), "GEOMETRYCOLLECTION"), ]
nonFederal3 <- st_collection_extract(nonFederal3)
#plot(amazon_bioma)
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(textreadr, magrittr)
# pacman::p_load_gh("trinker/pathr")
# trans_docs <- dir(
#   system.file("docs", package = "textreadr"), 
#   pattern = "^trans",
#   full.names = TRUE
# )
# pdf_doc <- "~/Downloads/glebas_TLP.pdf"
# d <- pdf_doc %>%
#   read_pdf() %>%
#   dplyr::select(text)
# d <- data.frame(unlist(strsplit(d[[1]], "\n")))
# colnames(d) <- "text"
# d <- d %>% filter(!str_detect(text, "^ITEM"))
# d$uf <- str_extract(d$text, "([:upper:])([:upper:])$")
# d$gleba_nome <- str_squish(str_remove(str_remove(d$text, "([:upper:])([:upper:])$"), "^[ ]*([0-9]+)"))
# d <- d %>% filter(!is.na(uf)) %>% dplyr::select(-text)
#plot(glebas)
glebas <- sf::st_simplify(glebas)
sf_use_s2(FALSE)

#----------------------------------#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Downloads/CleanCARShapes_robust/", pattern="*.shp", recursive = TRUE)
#---load clean CARs---#  
#----------------------------------#
car <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("/Users/pedrotremacoldirossi/Downloads/CleanCARShapes_robust/", f))
  car <- rbind(car, s)
  print(f)
}
#st_write(car, "~/car_combined.shp")
car <- read_sf("~/Documents/car_combined.shp")
#car_s <- gSimplify(methods::as(object = car, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#----------------------------------#
#---keep CAR in Amazon biome---# 
#----------------------------------#
car_1 <- car %>% filter(COD_ESTADO %in% c("AC", "AM", "RR", "AP"))
car_2 <- car %>% filter(!COD_ESTADO %in% c("AC", "AM", "RR", "AP"))
car_3 <- car %>% filter(COD_ESTADO == "PA")
registerDoParallel(cores = 80)
breaks <- seq(1, length(car_2$COD_IMOVEL), 100)
i.run <- function(j){
  v <- car_2[j:(j+100), ]
  st_intersection(amazon_bioma, v)
}
pdfs <- foreach(c=breaks[1:1000]) %dopar% i.run(c)
pdfs <- do.call(rbind, pdfs)
pdfs2 <- foreach(c=breaks[1001:2000]) %dopar% i.run(c)
pdfs2 <- do.call(rbind, pdfs2)
pdfs3 <- foreach(c=breaks[2001:3000]) %dopar% i.run(c)
pdfs3 <- do.call(rbind, pdfs3)
pdfs4 <- foreach(c=breaks[3001:4000]) %dopar% i.run(c)
pdfs4 <- do.call(rbind, pdfs4)
pdfs5 <- foreach(c=breaks[4001:4398]) %dopar% i.run(c)
pdfs5 <- do.call(rbind, pdfs5)
ps <- rbind(pdfs, pdfs2, pdfs3, pdfs4, pdfs5)
ps <- ps[!duplicated(ps), ]
breaks <- seq(1, length(car_3$COD_IMOVEL), 100)
i.run <- function(j){
  v <- car_3[j:(j+100), ]
  st_intersection(amazon_bioma, v)
}
s1 <- foreach(c=breaks[1:1000]) %dopar% i.run(c)
s1 <- do.call(rbind, s1)
s2 <- foreach(c=breaks[1001:2000]) %dopar% i.run(c)
s2 <- do.call(rbind, s2)
s3 <- foreach(c=breaks[2001:2921]) %dopar% i.run(c)
s3 <- do.call(rbind, s3)
ps <- rbind(pdfs, pdfs2, pdfs3, pdfs4, pdfs5, s1, s2, s3)
ps <- rbind(car_1, ps %>% dplyr::select(-id, -bioma))
#----------------------------------##----------------------------------#
#st_write(ps, "~/Documents/car_combined_amazonBiome2.shp")
#----------------------------------##----------------------------------#
car_amazon <- read_sf("~/Documents/car_combined_amazonBiome2.shp")
car_amazon_s <- gSimplify(methods::as(object = car_amazon, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)

states1 <- states[st_geometry_type(states) == "MULTIPOLYGON", ]
states2 <- states[st_geometry_type(states) == "POLYGON", ]
states3 <- states[str_detect(st_geometry_type(states), "GEOMETRYCOLLECTION"), ]
states3 <- st_collection_extract(states3)

tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#F1BB7B", 1), alpha("#02401B", 0.4)),
                border.col = "grey40",
                labels = c('Rural Properties', "Forgiveness-Eligible Areas")) +
  tm_shape(car_amazon_s) + tm_borders(col = "#F1BB7B", lwd = .1) +
  tm_shape(glebas) + tm_fill(col = "#02401B", alpha = 0.4, border.col = "#02401B") +
  #tm_shape(nonFederal1) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "white", legend.show = T) +
  #tm_shape(nonFederal2) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "white", legend.show = F) +
  #tm_shape(nonFederal3) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "white", legend.show = F) +
  tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)

###########################
##### ALREADY TREATED #####
###########################
#---National Property Certification System (SNCI) // Sistema Nacional de Certificacao de Imoveis---#
snci <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/titles/snci/snci_certificacoes/Imvel certificado SNCI Brasil.shp") %>% 
  st_transform(., crs = 4674) %>% # Change 4674 crs
  st_make_valid()
#---validate each polygon, drop the ones which aren't valid---#
snci$valid <- st_is_valid(snci) %>% as.numeric()
snci$codigo_imo <- as.numeric(snci$cod_imovel)
snci$year <- as.numeric(str_extract(snci$data_certi, "([0-9])([0-9])([0-9])([0-9])"))
#---parcels titled up to 2009---#  
already_treated <- snci %>% filter(year <= 2009) %>% filter(uf_municip %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO", "MT", "MA")) %>% st_intersection(amazon_bioma)



# Calculate area and tidy up
intersect_pct <- st_intersection(glebas, car_amazon) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(COD_IMOVEL, intersect_area) %>%   # only select columns needed to merge
  st_drop_geometry()  # drop geometry as we don't need it

ccar <- mutate(car_amazon, imovel_area = st_area(car_amazon))
ccar <- merge(ccar, intersect_pct, by = "COD_IMOVEL", all.x = TRUE)
library(data.table)
setDT(ccar)
ccar[, intersect_area := sum(intersect_area, na.rm = TRUE), by = COD_IMOVEL]
ccar[, overlap := as.numeric(intersect_area / imovel_area)]
ccar <- ccar[, .SD[1], by = COD_IMOVEL]
ccar$overlap <- ifelse(is.na(ccar$overlap), 0, ccar$overlap)
ccar <- st_as_sf(ccar)
ccar_clean <- ccar %>% filter(overlap == 0)
#----------------------------------#
#---CAR registered inside glebas --#
ccar_dirty <- ccar %>% filter(overlap > 0)
#----------------------------------#

#---CARs outside federal glebas in state glebas---# 
ccar_stateGlebas <- st_intersection(ccar_clean, nonFederal)
ccar_stateGlebas <- st_as_sf(ccar_stateGlebas)
#---CARs outside federal and state glebas in indigenous or reservation land---# 
ccar_clean_notStateGlebas <- ccar_clean[!ccar_clean$COD_IMOVEL %in% ccar_stateGlebas$COD_IMOVEL, ]
ccar_clean_notStateGlebas1 <- st_intersection(ccar_clean_notStateGlebas, indigenous)
ccar_clean_notStateGlebas2 <- st_intersection(ccar_clean_notStateGlebas, conservation)
ccar_clean_inReservas <- rbind.fill(ccar_clean_notStateGlebas1, ccar_clean_notStateGlebas2)
ccar_clean_inReservas <- st_as_sf(ccar_clean_inReservas)
#---CARs eligible to be in the "pure" control group---# 
ccar_controlGroup <- ccar_clean_notStateGlebas[!ccar_clean_notStateGlebas$COD_IMOVEL %in% ccar_clean_inReservas$COD_IMOVEL, ]

ccar_dirtyc <- gSimplify(methods::as(object = ccar_dirty, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
ccar_clean_inReservasc <- gSimplify(methods::as(object = ccar_clean_inReservas, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#ccar_stateGlebas1 <- ccar_stateGlebas[st_geometry_type(ccar_stateGlebas) == "MULTIPOLYGON", ]
#ccar_stateGlebas2 <- ccar_stateGlebas[st_geometry_type(ccar_stateGlebas) == "POLYGON", ]
#ccar_stateGlebas3 <- ccar_stateGlebas[str_detect(st_geometry_type(ccar_stateGlebas), "GEOMETRYCOLLECTION"), ]
#ccar_stateGlebas3 <- st_collection_extract(ccar_stateGlebas3)
#ccar_stateGlebas1c <- gSimplify(methods::as(object = ccar_stateGlebas1, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#ccar_stateGlebas2c <- gSimplify(methods::as(object = ccar_stateGlebas2, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#ccar_stateGlebas3c <- gSimplify(methods::as(object = ccar_stateGlebas3, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
ccar_controlGroupc <- gSimplify(methods::as(object = ccar_controlGroup, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
amazon_bioma <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
indigenous <- gSimplify(methods::as(object = indigenous, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
conservation <- gSimplify(methods::as(object = conservation, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)

tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#5B1A18", 1), alpha("#02401B", .7)),
                border.col = "grey40",
                labels = c('Illegal Rural Properties', "Rural Properties in Target Areas")) +
  tm_shape(ccar_dirtyc) + tm_borders(col = "#02401B", lwd = .1) +
  tm_shape(ccar_clean_inReservasc) + tm_borders(col = "#5B1A18", lwd = .1, alpha = 0.7) +
  tm_shape(glebas) + tm_fill(col = "#02401B", alpha = 0.2, border.col = "#02401B") +
  tm_shape(indigenous) + tm_fill(col = "#5B1A18", alpha = 0.2, border.col = "#5B1A18") +
  tm_shape(conservation) + tm_fill(col = "#5B1A18", alpha = 0.2, border.col = "#5B1A18") +
  #tm_shape(ccar_stateGlebas1c) + tm_borders(col = "darkblue", lwd = .1, alpha = 0.9) + 
  #tm_shape(ccar_stateGlebas2c) + tm_borders(col = "darkblue", lwd = .1, alpha = 0.9) +
  #tm_shape(ccar_stateGlebas3c) + tm_borders(col = "darkblue", lwd = .1, alpha = 0.9) +
  #tm_shape(ccar_controlGroupc) + tm_borders(col = "#F1BB7B", lwd = .1) +
  #tm_shape(glebas) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "#81A88D") +
  tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + 
  tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)

#---compute whether CAR has---# 
#---[1] partial overlap    ---#  
#---[2] full overlap       ---#  
#---[3] no overlap         ---#  

temas_ambientais_update <- read_delim("~/Library/CloudStorage/Dropbox/amazonLandPrices_project/data/raw/sicar/microdata/temas_ambientais_update.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
#temas_ambientais <- read_delim("~/Library/CloudStorage/Dropbox/amazonLandPrices_project/data/raw/sicar/microdata/temas_ambientais.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

ccar_dirty$registro_car <- ccar_dirty$COD_IMOVEL
ccar_dirty <- ccar_dirty %>% left_join(temas_ambientais_update, "registro_car")
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---WORKING EXAMPLE: Placas, PA---#

#for(muni in unique(ccar_dirty$codigo_ibge)){
i.run <- function(j){
    properties <- ccar_dirty %>% filter(codigo_ibge == j)
#properties <- ccar_dirty %>% filter(NOM_MUNICI == "Acrel√¢ndia")

#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
file_name <- list.files(path="/Volumes/ElementsMain/transitions_combined/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
f <- file_name[str_detect(file_name, "2004")]
r <- rast(paste0("/Volumes/ElementsMain/transitions_combined/", f[1]))
r <- crop(r, states %>% filter(SIGLA == properties$COD_ESTADO[1]))
p <- properties %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup()
booths_buffer = st_buffer(p, .3)
#plot(booths_buffer)
#plot(p$geometry, add = T)
rr <- crop(r, booths_buffer)
pp <- data.frame()
for(k in 1:length(properties$COD_IMOVEL)){
p1 <- terra::extract(rr, properties[k, ], xy = T)
p1 <- c(properties$COD_IMOVEL[k], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
pp <- rbind(pp, p1)
#print(k)
}
colnames(pp) <- c("COD_IMOVEL", "deforested_area_hc", "deforestation_rate")
active2004_CAR <- properties %>% filter(COD_IMOVEL %in% unique(pp %>% filter(deforestation_rate > 10) %>% .$COD_IMOVEL)) %>% left_join(pp %>% dplyr::select(COD_IMOVEL, deforested_area_hc, deforestation_rate), "COD_IMOVEL")
#----------------------------------##----------------------------------#
st_write(active2004_CAR %>% dplyr::select(geometry, COD_IMOVEL, NUM_AREA, codigo_ibge, deforestation_rate, deforested_area_hc), paste0("~/Documents/amazon/occupationCAR/active2004_CAR_", j,".shp"), append = F)
#----------------------------------##----------------------------------#
}
registerDoParallel(cores = 10)
pdfs <- foreach(c=unique(ccar_dirty$codigo_ibge)) %dopar% i.run(c)
#took 3 hours to run <<<<-------------------

#-------------------------------------------------------------------#
#---DROP 2004 CARs with deforestation above forgiveness threshold---#
file_name <- list.files(path="~/Documents/amazon/occupationCAR/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[!str_detect(file_name, "withinArea")]
for(file in file_name){
  car <- read_sf(paste0("~/Documents/amazon/occupationCAR/", file))
  car <- car %>% filter(as.numeric(dfrst__) <= 1500)
  m <- str_remove(str_extract(file, "_([0-9]+)"), "_")
#----------------------------------##----------------------------------#
st_write(car, paste0("~/Documents/amazon/occupationCAR/active2004_CAR_", m,"_withinArea.shp"), append = F)
#----------------------------------##----------------------------------#
  print(m)
}
#-------------------------------------------------------------------#
#---put together---#
file_name <- list.files(path="~/Documents/amazon/occupationCAR/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "withinArea")]
car2004 <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("~/Documents/amazon/occupationCAR/", f))
  car2004 <- rbind(car2004, s)
  print(f)
}
st_write(car2004, "~/Documents/car2004.shp")

#ccar_clean_inReservas
st_write(ccar_clean_inReservas %>% dplyr::select(geometry, COD_IMOVEL, NUM_AREA), "~/Documents/ccar_clean_inReservas.shp")

#---filter CARs in glebas that i) only appeared after 2004 or ii) had <1,500 ha deforested
CAR_notEligible <- ccar_dirty %>% filter(!COD_IMOVEL %in% car2004$COD_IMO) 
st_write(CAR_notEligible %>% dplyr::select(geometry, COD_IMOVEL, NUM_AREA), "~/Documents/CAR_notEligible.shp", append = F)
  
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
for(year in seq(2005, 2014)){  
  start.time <- Sys.time()
  file_name <- list.files(path="/Volumes/ElementsMain/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
  f <- file_name[str_detect(file_name, as.character(year))]
  r <- rast(paste0("/Volumes/ElementsMain/transitions_combined/", f[1]))

  i.run <- function(j){
    p1 <- terra::extract(r, ccar_clean_inReservas[j, ], xy = T)
    write.table(j, "/Users/pedrotremacoldirossi/Documents/placeholder/place2.txt")
    c(ccar_clean_inReservas$COD_IMOVEL[j], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
  }
  registerDoParallel(cores = 12)
  pdfs <- foreach(c=1:length(ccar_clean_inReservas$COD_IMOVEL)) %dopar% i.run(c)
  pdfs <- data.frame(do.call(rbind, pdfs))
  colnames(pdfs) <- c("COD_IMOVEL", "deforested_area_hc", "deforestation_rate")
#----------------------------------##----------------------------------#  
  write_rds(pdfs, paste0("~/Documents/CAR_inReservas_defo_", year, ".rds"))
#----------------------------------##----------------------------------#  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox/amazon_working/", pattern="*.rds", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "CAR_notEligible_defo_")]

for(file in file_name[1]){
  control <- readRDS(paste0("~/Library/CloudStorage/Dropbox/amazon_working/", file))
  control$area <- as.numeric(control$deforested_area_hc)/(as.numeric(control$deforestation_rate)/100)
  control <- control %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% group_by(COD_IMOVEL)
  control <- control %>% group_by(COD_IMOVEL, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMOVEL) == 1)
  names(control)[2:3] <- paste0(names(control)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
}  

for(file in file_name[2:10]){
data <- readRDS(paste0("~/Library/CloudStorage/Dropbox/amazon_working/", file))
data <- data %>% group_by(COD_IMOVEL, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMOVEL) == 1)
names(data)[2:3] <- paste0(names(data)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
control <- dplyr::left_join(control, data, "COD_IMOVEL")
print(file)
}  

#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox/amazon_working/", pattern="*.rds", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "CAR_notEligible_defo_")]
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
for(file in file_name[1]){
  spillover <- readRDS(paste0("~/Library/CloudStorage/Dropbox/amazon_working/", file))
  #spillover$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
  spillover <- melt(spillover, id = "COD_IMOVEL")
  spillover$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
  #control$area <- as.numeric(control$deforested_area_hc)/(as.numeric(control$deforestation_rate)/100)
  #control <- control %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% group_by(COD_IMOVEL)
  #control <- control %>% group_by(COD_IMOVEL, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMOVEL) == 1)
  #names(spillover)[2:3] <- paste0(names(spillover)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("~/Library/CloudStorage/Dropbox/amazon_working/", file))
  data <- melt(data, id = "COD_IMOVEL")
  data$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
  #data <- data %>% group_by(COD_IMOVEL, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMOVEL) == 1)
  #names(data)[2:3] <- paste0(names(data)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
  spillover <- rbind(spillover, data)
  #spillover  <- distinct(spillover)
  print(file)
}  
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
spillover[spillover == "NaN"] <- NA
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
drop_spillover <- unique(spillover %>% filter(year == "2014") %>% filter(variable == "deforested_area_hc") %>% filter(value == "0") %>% .$COD_IMOVEL)

plot(spillover %>% filter(!COD_IMOVEL %in% drop_spillover) %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% .$m, t = "l")
mean(spillover %>% filter(!COD_IMOVEL %in% drop_spillover) %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% filter(year > "2008") %>% .$m)/mean(spillover %>% filter(!COD_IMOVEL %in% drop_spillover) %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% filter(year < "2009") %>% .$m)-1

mean(spillover %>% filter(!COD_IMOVEL %in% drop_spillover) %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% filter(year > "2008") %>% .$m)
mean(spillover %>% filter(!COD_IMOVEL %in% drop_spillover) %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% filter(year < "2009") %>% .$m)


file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox/amazon_working/", pattern="*.rds", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "car2004_defo_")]
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
for(file in file_name[1]){
  eligible <- readRDS(paste0("~/Library/CloudStorage/Dropbox/amazon_working/", file))
  #spillover$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
  eligible <- melt(eligible, id = "COD_IMOVEL")
  eligible$year <- str_remove(str_extract(file, "_([0-9])([0-9])([0-9])([0-9])"), "_")
  #control$area <- as.numeric(control$deforested_area_hc)/(as.numeric(control$deforestation_rate)/100)
  #control <- control %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% group_by(COD_IMOVEL)
  #control <- control %>% group_by(COD_IMOVEL, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMOVEL) == 1)
  #names(spillover)[2:3] <- paste0(names(spillover)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("~/Library/CloudStorage/Dropbox/amazon_working/", file))
  data <- melt(data, id = "COD_IMOVEL")
  data$year <- str_remove(str_extract(file, "_([0-9])([0-9])([0-9])([0-9])"), "_")
  #data <- data %>% group_by(COD_IMOVEL, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMOVEL) == 1)
  #names(data)[2:3] <- paste0(names(data)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
  eligible <- rbind(eligible, data)
  #spillover  <- distinct(spillover)
  print(file)
}  
eligible[eligible == "NaN"] <- NA
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
plot(eligible %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% .$m, t = "l", col = "red", ylim = c(35, 70))
mean(eligible %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% filter(year > "2008") %>% .$m)/mean(eligible %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% filter(year < "2009") %>% .$m)-1

mean(eligible %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% filter(year > "2008") %>% .$m)
mean(eligible %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% filter(year < "2009") %>% .$m)


file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox/amazon_working/", pattern="*.rds", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "CAR_inReservas_defo_")]
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
for(file in file_name[1]){
  control <- readRDS(paste0("~/Library/CloudStorage/Dropbox/amazon_working/", file))
  #spillover$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
  control <- melt(control, id = "COD_IMOVEL")
  control$year <- str_remove(str_extract(file, "_([0-9])([0-9])([0-9])([0-9])"), "_")
  #control$area <- as.numeric(control$deforested_area_hc)/(as.numeric(control$deforestation_rate)/100)
  #control <- control %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% group_by(COD_IMOVEL)
  #control <- control %>% group_by(COD_IMOVEL, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMOVEL) == 1)
  #names(spillover)[2:3] <- paste0(names(spillover)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("~/Library/CloudStorage/Dropbox/amazon_working/", file))
  data <- melt(data, id = "COD_IMOVEL")
  data$year <- str_remove(str_extract(file, "_([0-9])([0-9])([0-9])([0-9])"), "_")
  #data <- data %>% group_by(COD_IMOVEL, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMOVEL) == 1)
  #names(data)[2:3] <- paste0(names(data)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
  control <- rbind(control, data)
  #spillover  <- distinct(spillover)
  print(file)
}  
control[control == "NaN"] <- NA
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
drop_control <- unique(control %>% filter(year == "2014") %>% filter(variable == "deforested_area_hc") %>% filter(value == "0") %>% .$COD_IMOVEL)

lines(control %>% filter(!COD_IMOVEL %in% drop_control) %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% .$m, t = "l")
mean(control %>% filter(!COD_IMOVEL %in% drop_control) %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% filter(year > "2008") %>% .$m)/mean(control %>% filter(!COD_IMOVEL %in% drop_control) %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% filter(year < "2009") %>% .$m)-1

mean(control %>% filter(!COD_IMOVEL %in% drop_control) %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% filter(year > "2008") %>% .$m)
mean(control %>% filter(!COD_IMOVEL %in% drop_control) %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% filter(year < "2009") %>% .$m)


#---export did 1----#
c <- control %>% filter(!COD_IMOVEL %in% drop_control)
c$treatment <- 0

e <- eligible
e$treatment <- 1

did1 <- rbind(c, e)  
did1$after <- ifelse(as.numeric(did1$year) >= 2009, 1, 0)
did1$treated <- did1$treatment*did1$after
did1$value <- as.numeric(did1$value)

write_dta(did1, "~/Documents/did1.dta")

s <- spillover %>% filter(!COD_IMOVEL %in% drop_spillover)
s$treatment <- 1

did2 <- rbind(c, s)  
did2$after <- ifelse(as.numeric(did2$year) >= 2009, 1, 0)
did2$treated <- did2$treatment*did2$after
did2$value <- as.numeric(did2$value)

write_dta(did2, "~/Documents/did2.dta")


#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#

  
  colnames(pp) <- c("COD_IMOVEL", "deforested_area_hc", "deforestation_rate")
  active2004_CAR <- properties %>% filter(COD_IMOVEL %in% unique(pp %>% filter(deforestation_rate > 10) %>% .$COD_IMOVEL)) %>% left_join(pp %>% dplyr::select(COD_IMOVEL, deforested_area_hc, deforestation_rate), "COD_IMOVEL")
  #----------------------------------##----------------------------------#
  st_write(active2004_CAR %>% dplyr::select(geometry, COD_IMOVEL, NUM_AREA, codigo_ibge, deforestation_rate, deforested_area_hc), paste0("~/Documents/amazon/occupationCAR/active2004_CAR_", j,".shp"), append = F)
  #----------------------------------##----------------------------------#




#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
f <- file_name[str_detect(file_name, "2009")]
r <- rast(paste0("/Volumes/ElementsMain/transitions_combined/", f[1]))
r <- crop(r, states %>% filter(SIGLA == properties$COD_ESTADO[1]))
rr <- crop(r, booths_buffer)
pp2 <- data.frame()
others <- properties %>% filter(!COD_IMOVEL %in% active2004_CAR$COD_IMOVEL)
for(k in 1:length(others$COD_IMOVEL)){
  p1 <- terra::extract(rr, others[k, ], xy = T)
  p1 <- c(others$COD_IMOVEL[k], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
  pp2 <- rbind(pp2, p1)
  print(k)
}
colnames(pp2) <- c("COD_IMOVEL", "deforested_area_hc", "deforestation_rate")
active2009_CAR <- properties %>% filter(COD_IMOVEL %in% unique(pp2 %>% filter(deforestation_rate > 10) %>% .$COD_IMOVEL))
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
f <- file_name[str_detect(file_name, "2017")]
r <- rast(paste0("/Volumes/ElementsMain/transitions_combined/", f[1]))
r <- crop(r, states %>% filter(SIGLA == properties$COD_ESTADO[1]))
rr <- crop(r, booths_buffer)
pp3 <- data.frame()
others <- properties %>% filter(!COD_IMOVEL %in% active2004_CAR$COD_IMOVEL) %>% filter(!COD_IMOVEL %in% active2009_CAR$COD_IMOVEL)
for(k in 1:length(others$COD_IMOVEL)){
  p1 <- terra::extract(rr, others[k, ], xy = T)
  p1 <- c(others$COD_IMOVEL[k], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
  pp3 <- rbind(pp3, p1)
  print(k)
}
colnames(pp3) <- c("COD_IMOVEL", "deforested_area_hc", "deforestation_rate")
active2017_CAR <- properties %>% filter(COD_IMOVEL %in% unique(pp3 %>% filter(deforestation_rate > 10) %>% .$COD_IMOVEL))
inactive_CAR <- properties %>% filter(!COD_IMOVEL %in% active2004_CAR$COD_IMOVEL) %>% filter(!COD_IMOVEL %in% active2009_CAR$COD_IMOVEL) %>% filter(!COD_IMOVEL %in% active2017_CAR$COD_IMOVEL)

#----------------------------------##----------------------------------#
st_write(active2004_CAR %>% dplyr::select(geometry, COD_IMOVEL, NUM_AREA, codigo_ibge), paste0("~/Documents/amazon/occupationCAR/active2004_CAR_", muni,".shp"), append = F)
st_write(active2009_CAR %>% dplyr::select(geometry, COD_IMOVEL, NUM_AREA, codigo_ibge), paste0("~/Documents/amazon/occupationCAR/active2009_CAR_", muni,".shp"), append = F)
st_write(active2017_CAR %>% dplyr::select(geometry, COD_IMOVEL, NUM_AREA, codigo_ibge), paste0("~/Documents/amazon/occupationCAR/active2017_CAR_", muni,".shp"), append = F)
st_write(inactive_CAR %>% dplyr::select(geometry, COD_IMOVEL, NUM_AREA, codigo_ibge), paste0("~/Documents/amazon/occupationCAR/inactive_CAR_", muni,".shp"), append = F)
#----------------------------------##----------------------------------#


#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
rr_p <- as.factor(rr)
ggplot() +
  tidyterra::geom_spatraster(data = rr_p, aes(fill = layer)) +
  tidyterra::scale_fill_whitebox_d(
    palette = "bl_yl_rd", na.translate = T) + theme_minimal() + #labs(title = "MAP") + 
  scale_fill_manual(values = colors, labels = labels, na.translate = F, na.value = "white") + 
  #tidyterra::geom_spatvector(data = amazon_bioma, fill = NA, col = "black") + 
  tidyterra::geom_spatvector(data = properties, fill = NA, col = "steelblue1", alpha = 0.1, lwd = 0.1) + 
  #tidyterra::geom_spatvector(data = active2004_CAR, fill = NA, col = "red", alpha = 0.1, lwd = 0.1) +
  #tidyterra::geom_spatvector(data = active2009_CAR, fill = NA, col = "green", alpha = 1, lwd = 0.1) +
  tidyterra::geom_spatvector(data = booths_buffer, fill = NA, col = "steelblue1", alpha = 0.5, lwd = 1) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=10,  family = "Palatino"),
        legend.title = element_blank(), legend.box="horizontal",  legend.position="bottom",
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.grid.major = element_blank(), 
        rect = element_blank())
#unique(p1$layer)
#length(p1$layer)*0.09
#length(p1 %>% filter(layer == 2) %>% .$layer)*0.09
#length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#---drop CARs without use
properties <- properties %>% filter(!COD_IMOVEL %in% inactive_CAR$COD_IMOVEL)
properties_archive <- properties
i.run <- function(i){
  properties %>% filter(row_number() == i) %>% 
    st_intersection(properties %>% filter(row_number() != i)) %>%
    mutate(intersect_area = st_area(.)) %>%
    dplyr::select(COD_IMOVEL, intersect_area, imovel_area, imovel_area.1, COD_IMOVEL.1, data_inscricao, data_inscricao.1) %>%   # only select columns needed to merge
    st_drop_geometry()  # drop geometry as we don't need it
}
registerDoParallel(cores = 50)
pdfs <- foreach(c=1:length(properties$COD_IMOVEL)) %dopar% i.run(c)
pdfs <- do.call(rbind, pdfs)
#------------------------------------------------------------------------------#
#---select polygons that contain other polygons
pdfs <- pdfs %>% group_by(COD_IMOVEL.1) %>% mutate(contains = if_else(as.numeric(intersect_area/imovel_area) == 1, 1, 0), contains = max(contains))
CAR_contains <- properties %>% filter(COD_IMOVEL %in% unique(pdfs %>% filter(contains == 1) %>% .$COD_IMOVEL.1))
#------------------------------------------------------------------------------#
contained <- pdfs %>% mutate(is_contained = as.numeric(intersect_area/imovel_area)) %>% filter(is_contained == 1) %>% filter(COD_IMOVEL.1 %in% CAR_contains$COD_IMOVEL)
contained$before <- ifelse(contained$data_inscricao <= contained$data_inscricao.1, 1, 0)
contained <- contained %>% group_by(COD_IMOVEL.1) %>% mutate(came_after = mean(before)) 
contained$match <- contained$COD_IMOVEL.1
#---determine total area with overlap of polygons contained in each CAR_contains
#---this only considers overlaps and adjacencies with polygons completely contained within the CAR
#---polygons that only overlap partially with the container CAR are ignored [eg in conflcit]
CAR_contains$pct_conflict <- 0
CAR_contains$pct_adjacent <- 0
for(j in CAR_contains$COD_IMOVEL){
  k <- match(j, CAR_contains$COD_IMOVEL)
  lie <- properties %>% filter(COD_IMOVEL %in% unique(contained %>% filter(COD_IMOVEL.1 == j) %>% .$COD_IMOVEL))
  
  i.run2 <- function(i){
     lie %>% filter(row_number() == i) %>% 
       st_intersection(lie %>% filter(row_number() != i)) %>%
       mutate(intersect_area = st_area(.)) %>%
       dplyr::select(COD_IMOVEL, intersect_area, imovel_area, imovel_area.1, COD_IMOVEL.1, data_inscricao, data_inscricao.1) %>%   # only select columns needed to merge
       st_drop_geometry()  # drop geometry as we don't need it
   }
  registerDoParallel(cores = 50)
  pdfs0 <- foreach(c=1:length(lie$COD_IMOVEL)) %dopar% i.run2(c)
  pdfs0 <- do.call(rbind, pdfs0)
if(length(pdfs0$COD_IMOVEL) > 0){
CAR_contains$pct_conflict[k] <- as.numeric(head(pdfs0[!duplicated(t(apply(pdfs0 %>% dplyr::select(COD_IMOVEL, COD_IMOVEL.1), 1, sort))),] %>% 
    mutate(pct_intersect = sum(as.numeric(intersect_area))) %>% .$pct_intersect, 1)/sum(unique(as.numeric(pdfs0[!duplicated(t(apply(pdfs0 %>% dplyr::select(COD_IMOVEL, COD_IMOVEL.1), 1, sort))),] %>% .$imovel_area)))*100)
} 

  lie2 <- properties %>% filter(COD_IMOVEL %in% unique(contained %>% filter(COD_IMOVEL.1 == j) %>% .$COD_IMOVEL)) %>%
    filter(!COD_IMOVEL %in% unique(pdfs0 %>% filter(as.numeric(intersect_area) > 0) %>% .$COD_IMOVEL))
if(length(lie2$COD_IMOVEL) > 0){ 
  lie2 <- lie2 %>%
    st_transform(5070) %>% #convert from lat/long to projected coord system
    mutate(length_perim = lwgeom::st_perimeter(.))
  
CAR_contains$pct_adjacent[k] <- sum(as.numeric(lie2 %>%
    st_cast('MULTILINESTRING') %>%
    st_intersection() %>%
    mutate(length_intersection = st_length(.)) %>%
    filter(length_intersection > units::as_units(0,'m')) %>%
    group_by(COD_IMOVEL) %>%
    mutate(length_intersection = sum(length_intersection),
           share_adjacent = length_intersection/length_perim) %>% filter(row_number(COD_IMOVEL) == 1) %>%
    filter(as.numeric(share_adjacent) < 1) %>% .$length_intersection))/sum(as.numeric(lie2 %>% group_by(COD_IMOVEL) %>% filter(row_number(COD_IMOVEL) == 1) %>% .$length_perim))*100
}
  print(k)
  
}
CAR_contains$match <- CAR_contains$COD_IMOVEL
contained <- contained %>% left_join(CAR_contains %>% dplyr::select(match, pct_conflict, pct_adjacent), "match")
contained$subdivision <- ifelse(contained$came_after <= 0.5 & contained$pct_conflict <= contained$pct_adjacent, 1, 0)
contained$take_over <- ifelse(contained$came_after > 0.5 & contained$pct_conflict > contained$pct_adjacent, 1, 0)
CAR_contains <- CAR_contains %>% merge(contained %>% group_by(match) %>% filter(row_number(match) == 1) %>% dplyr::select(match, subdivision, take_over), "match")
#------------------------------------------------------------------------------#
properties <- properties %>% filter(!COD_IMOVEL %in% contained$COD_IMOVEL) %>% filter(!COD_IMOVEL %in% CAR_contains$COD_IMOVEL)

#---polygons that partially enter CAR_contains
#---treat those with 90%+ invasion as contained in that CAR
i.run3 <- function(i){
properties %>% #filter(row_number() == i) %>%
    st_intersection(CAR_contains %>% filter(COD_IMOVEL == i)) %>%
      mutate(intersect_area = st_area(.)) %>%
      dplyr::select(COD_IMOVEL, intersect_area, imovel_area, imovel_area.1, COD_IMOVEL.1, data_inscricao, data_inscricao.1) %>%   # only select columns needed to merge
      st_drop_geometry()  # drop geometry as we don't need it
  }
registerDoParallel(cores = 50)
pdfs1 <- foreach(c=1:length(CAR_contains$COD_IMOVEL)) %dopar% i.run3(CAR_contains$COD_IMOVEL[c])
pdfs1 <- do.call(rbind, pdfs1)
pdfs1$match <- pdfs1$COD_IMOVEL.1

asContained <- pdfs1 %>% filter(as.numeric(intersect_area/imovel_area) > .9) %>% left_join(CAR_contains %>% dplyr::select(match, pct_conflict, pct_adjacent, subdivision, take_over), "match") %>%
      group_by(COD_IMOVEL) %>%  mutate(max_intersection = max(as.numeric(intersect_area/imovel_area))) %>% filter(as.numeric(intersect_area/imovel_area) == max_intersection) %>% 
      mutate(before = if_else(data_inscricao < data_inscricao.1, 1, 0))
#------------------------------------------------------------------------------#
properties <- properties %>% filter(!COD_IMOVEL %in% asContained$COD_IMOVEL)

#---polygons without any conflict
noOverlap_CAR <- properties %>% filter(!COD_IMOVEL %in% pdfs$COD_IMOVEL) %>% 
                 rbind(properties %>% filter(COD_IMOVEL %in% unique(pdfs %>% group_by(COD_IMOVEL) %>% 
                 mutate(pct_intersect = sum(intersect_area)/imovel_area) %>%
                 filter(row_number(COD_IMOVEL) == 1) %>% filter(as.numeric(pct_intersect) < 0.1) %>% .$COD_IMOVEL)))
#------------------------------------------------------------------------------#

#---polygons with conflict
overlap_CAR <- properties %>% filter(!COD_IMOVEL %in% noOverlap_CAR$COD_IMOVEL)
overlap_CAR$independent <- 1
#------------------------------------------------------------------------------# 
sigef <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/titles/sigef/sigef_br/Sigef Brasil.shp") %>% 
  st_transform(., crs = 4674) %>% # Change 4674 crs
  st_make_valid()
municipios <- read_csv("~/Downloads/municipios.csv")
sigef$COD <- sigef$municipio_
sigef <- sigef %>% left_join(municipios, "COD")
s <- sigef %>% filter(NOME == "Acrel‚ndia") %>% filter(status == "CERTIFICADA")
certified <- st_intersection(noOverlap_CAR, s) %>% 
  mutate(intersect_pct_sigef = as.numeric(st_area(.)/imovel_area)) %>% filter(intersect_pct_sigef > .5)
certified$independent <- 1
#------------------------------------------------------------------------------#
noOverlap_CAR <- noOverlap_CAR %>% filter(!COD_IMOVEL %in% certified$COD_IMOVEL)
#--- ---#
CAR_contains$independent <- ifelse(CAR_contains$take_over == 1, 1, NA)
CAR_contains$independent <- ifelse(CAR_contains$subdivision == 1, 0, CAR_contains$independent)
CAR_contains$imputed_id <- CAR_contains$COD_IMOVEL
#--- ---#
contained$independent <- ifelse(contained$take_over == 1, 1, NA)
contained$independent <- ifelse(contained$subdivision == 1, 0, contained$independent)
contained$imputed_id <- ifelse(!is.na(contained$independent) & contained$independent == 1, contained$COD_IMOVEL, "")
contained$imputed_id <- ifelse(!is.na(contained$independent) & contained$independent == 0, contained$COD_IMOVEL.1, contained$imputed_id)
#--- ---#
asContained$independent <- ifelse(asContained$take_over == 1, 1, NA)
asContained$independent <- ifelse(asContained$subdivision == 1, 0, asContained$independent)
asContained$imputed_id <- ifelse(!is.na(asContained$independent) & asContained$independent == 1, asContained$COD_IMOVEL, "")
asContained$imputed_id <- ifelse(!is.na(asContained$independent) & asContained$independent == 0, asContained$COD_IMOVEL.1, asContained$imputed_id)
#--- ---#
overlap_CAR$imputed_id <- overlap_CAR$COD_IMOVEL
#--- ---#
certified$imputed_id <- certified$COD_IMOVEL
#--- ---#
noOverlap_CAR$imputed_id <- ""
#--- ---#
contained <- contained %>% dplyr::select(-geometry) %>% left_join(properties_archive %>% dplyr::select(geometry, COD_IMOVEL), "COD_IMOVEL")
contained <- contained %>% st_as_sf() %>% st_transform(., crs = 4674)
asContained <- asContained %>% dplyr::select(-geometry) %>% left_join(properties_archive %>% dplyr::select(geometry, COD_IMOVEL), "COD_IMOVEL")
asContained <- asContained %>% st_as_sf() %>% st_transform(., crs = 4674)
combined <- rbind(CAR_contains %>% dplyr::select(COD_IMOVEL, geometry, imputed_id), 
                  contained %>% dplyr::select(COD_IMOVEL, geometry, imputed_id), 
                  asContained %>% dplyr::select(COD_IMOVEL, geometry, imputed_id), 
                  overlap_CAR %>% dplyr::select(COD_IMOVEL, geometry, imputed_id),
                  certified %>% dplyr::select(COD_IMOVEL, geometry, imputed_id), 
                  noOverlap_CAR %>% dplyr::select(COD_IMOVEL, geometry, imputed_id))

combined$exists <- ifelse(combined$imputed_id != "", 1, 0)
combined <- combined %>% group_by(COD_IMOVEL) %>% mutate(exists = max(exists))
combined$drop <- ifelse(combined$exists == 1 & combined$imputed_id == "", 1, 0)
combined <- combined %>% filter(drop == 0)
combined <- combined %>% group_by(imputed_id) %>% add_count()

c1 <- combined %>% filter(imputed_id != "") %>% group_by(imputed_id) %>% summarise()
c2 <- combined %>% filter(imputed_id == "")
cc <- rbind.fill(c1, c2)
cc <- st_as_sf(cc)
#------------------------------------------------------------------------------#
ggplot() +
  tidyterra::geom_spatvector(data = properties_archive, fill = NA, col = "steelblue1", alpha = 1, lwd = 0.2) + 
  #tidyterra::geom_spatvector(data = c1, fill = NA, col = "steelblue4", alpha = 1, lwd = 0.2) + 
  #tidyterra::geom_spatvector(data = c2, fill = NA, col = "steelblue", alpha = 1, lwd = 0.2) + 
  #tidyterra::geom_spatvector(data = active2004_CAR, fill = NA, col = "red", alpha = 0.1, lwd = 0.1) +
  #tidyterra::geom_spatvector(data = active2009_CAR, fill = NA, col = "green", alpha = 1, lwd = 0.1) +
  #tidyterra::geom_spatvector(data = booths_buffer, fill = NA, col = "steelblue1", alpha = 0.5, lwd = 1) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=10,  family = "Palatino"),
        legend.title = element_blank(), legend.box="horizontal",  legend.position="bottom",
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.grid.major = element_blank(), 
        rect = element_blank()) ->> a

ggplot() +
  #tidyterra::geom_spatvector(data = properties_archive, fill = NA, col = "steelblue1", alpha = 1, lwd = 0.2) + 
  tidyterra::geom_spatvector(data = c1, fill = NA, col = "steelblue4", alpha = 1, lwd = 0.4) + 
  tidyterra::geom_spatvector(data = c2, fill = NA, col = "steelblue3", alpha = 1, lwd = 0.4) + 
  #tidyterra::geom_spatvector(data = active2004_CAR, fill = NA, col = "red", alpha = 0.1, lwd = 0.1) +
  #tidyterra::geom_spatvector(data = active2009_CAR, fill = NA, col = "green", alpha = 1, lwd = 0.1) +
  #tidyterra::geom_spatvector(data = booths_buffer, fill = NA, col = "steelblue1", alpha = 0.5, lwd = 1) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=10,  family = "Palatino"),
        legend.title = element_blank(), legend.box="horizontal",  legend.position="bottom",
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.grid.major = element_blank(), 
        rect = element_blank()) ->> b

ggplot() +
  tidyterra::geom_spatvector(data = properties_archive, fill = NA, col = "white", alpha = 1, lwd = 0.1) + 
  #tidyterra::geom_spatvector(data = c1, fill = NA, col = "steelblue4", alpha = 1, lwd = 0.2) + 
  #tidyterra::geom_spatvector(data = c2, fill = NA, col = "steelblue", alpha = 1, lwd = 0.2) + 
  tidyterra::geom_spatvector(data = active2004_CAR, fill = NA, col = "#F1BB7B", alpha = 0.1, lwd = 0.4) +
  #tidyterra::geom_spatvector(data = active2009_CAR, fill = NA, col = "green", alpha = 1, lwd = 0.1) +
  #tidyterra::geom_spatvector(data = booths_buffer, fill = NA, col = "steelblue1", alpha = 0.5, lwd = 1) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=10,  family = "Palatino"),
        legend.title = element_blank(), legend.box="horizontal",  legend.position="bottom",
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.grid.major = element_blank(), 
        rect = element_blank()) ->> d

ggplot() +
  tidyterra::geom_spatvector(data = properties_archive, fill = NA, col = "white", alpha = 1, lwd = 0.1) + 
  #tidyterra::geom_spatvector(data = c1, fill = NA, col = "steelblue4", alpha = 1, lwd = 0.2) + 
  #tidyterra::geom_spatvector(data = c2, fill = NA, col = "steelblue", alpha = 1, lwd = 0.2) + 
  tidyterra::geom_spatvector(data = active2009_CAR, fill = NA, col = "#5B1A18", alpha = 0.1, lwd = 0.4) +
  #tidyterra::geom_spatvector(data = active2009_CAR, fill = NA, col = "green", alpha = 1, lwd = 0.1) +
  #tidyterra::geom_spatvector(data = booths_buffer, fill = NA, col = "steelblue1", alpha = 0.5, lwd = 1) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=10,  family = "Palatino"),
        legend.title = element_blank(), legend.box="horizontal",  legend.position="bottom",
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.grid.major = element_blank(), 
        rect = element_blank()) ->> e

grid.arrange(a, b, d, e, ncol = 2)


plot(CAR_contains$geometry)
plot(properties_archive$geometry)


plot(cc$geometry, col = "white")
plot(properties_archive$geometry, add = T) 

plot(c1$geometry, col = alpha("green", .5), add = T)
plot(c1$geometry, col = alpha("green", .5))
plot(c2$geometry, col = alpha("steelblue1", .5), add = T)
plot(contained$geometry, col = "purple", add = T)
#------------------------------------------------------------------------------#
#---6 groups:
#--- CAR_contains - independent if take over, not independent if subdivision, NA if uncertain
#--- contained - independent if take over, not independent if subdivision, NA if uncertain
#--- asContained - independent if take over, not independent if subdivision, NA if uncertain
#---overlap_CAR - independent
#---certified - independent
#---noOverlap_CAR - NA (uncertain)


plot(properties_archive$geometry)







contained %>% group_by(COD_IMOVEL.1) %>% filter(contains == 0) %>% mutate(total_conflict = sum(intersect_area))


contained$subdivision <- ifelse(contained$came_after <= 0.5, 1, 0)

contained$take_over <- ifelse(contained$came_after > 0.5, 1, 0)


#------------------------------------------------------------------------------# will need to address adjancency
#---select polygons without any overlap or only with overlaps <10% [0, 0.1]----#
#---this treats small overlap the same as no overlap---------------------------# 
noOverlap_CAR <- properties %>% filter(!COD_IMOVEL %in% pdfs$COD_IMOVEL) %>% 
              rbind(properties %>% filter(COD_IMOVEL %in% unique(pdfs %>% group_by(COD_IMOVEL) %>% 
              mutate(pct_intersect = sum(intersect_area)/imovel_area) %>%
              filter(row_number(COD_IMOVEL) == 1) %>% filter(as.numeric(pct_intersect) < 0.1) %>% .$COD_IMOVEL)))

properties <- properties %>% filter(!COD_IMOVEL %in% noOverlap_CAR$COD_IMOVEL)
#------------------------------------------------------------------------------#
#---polygons that don't overlap partially (0,1) but contain other polygons-----#
#---and the combined contained polygons make up <10% of their area-------------#
#---it's very likely these cases are dissociated from the contained polygons --#
#---thay may either be not credible (discard) or treated as separate-----------#
miniSuper_CAR <- properties %>% filter(COD_IMOVEL %in% unique(pdfs %>% group_by(COD_IMOVEL) %>% 
                 mutate(pct_intersect = sum(intersect_area)/imovel_area) %>% ungroup() %>% group_by(COD_IMOVEL.1) %>%
                 mutate(pct_intersecting = sum(intersect_area)/imovel_area.1) %>% filter(as.numeric(pct_intersecting) < 0.1) %>% 
                 filter(as.numeric(pct_intersect) == 1) %>% .$COD_IMOVEL.1))
#------------------------------------------------------------------------------#
overlaps <- pdfs %>% filter(!COD_IMOVEL %in% noOverlap_CAR$COD_IMOVEL) %>%
            filter(!COD_IMOVEL %in% miniSuper_CAR$COD_IMOVEL) %>%
            filter(as.numeric(intersect_area) > 0) %>%
            mutate(pct_intersect = intersect_area/imovel_area)    
overlaps$is_contained <- ifelse(as.numeric(overlaps$pct_intersect) == 1, 1, 0)
#------------------------------------------------------------------------------#
#---polygons with overlap [0.1, 1) but not contained in any other polygon -----#
#---and that do not contain other polygons (0, 0.1)----------------------------#
overlapsNotContained_CAR <- overlaps %>% group_by(COD_IMOVEL) %>% mutate(is_contained_max = max(is_contained)) %>% filter(is_contained_max == 0) %>% filter(row_number(COD_IMOVEL) == 1)  
overlapsNotContained_CAR$independent <- 1
#------------------------------------------------------------------------------#
overlaps_notContained_withOverlap <- overlaps %>% group_by(COD_IMOVEL) %>% 
  mutate(is_contained = max(is_contained), big_overlap = sum(big_overlap)) %>% filter(big_overlap > 0 & is_contained == 0) %>%
  filter(as.numeric(pct_intersect) >= 0.1)

overlaps_notContained_withOverlap <- overlaps_notContained_withOverlap %>% group_by(COD_IMOVEL) %>% add_count() %>% rename(i_intersect_with = n) %>% 
  ungroup() %>% group_by(COD_IMOVEL.1) %>% add_count() %>% rename(it_intersects_with = n)


overlaps$big_overlap <- ifelse(as.numeric(overlaps$pct_intersect) < 0.1 | as.numeric(overlaps$pct_intersect) == 1, 0, 1)
#------------------------------------------------------------------------------#
overlaps_contained_withoutOverlap <- overlaps %>% group_by(COD_IMOVEL) %>% 
                                      mutate(is_contained = max(is_contained),
                                             big_overlap = sum(big_overlap)) %>% filter(big_overlap == 0 & is_contained == 1) %>% filter(row_number(big_overlap) == 1)
overlaps_contained_withoutOverlap$before <- ifelse(overlaps_contained_withoutOverlap$data_inscricao <= overlaps_contained_withoutOverlap$data_inscricao.1, 1, 0)
overlaps_contained_withoutOverlap <- overlaps_contained_withoutOverlap %>% group_by(COD_IMOVEL.1) %>% mutate(came_after = mean(before))
overlaps_contained_withoutOverlap$take_over <- ifelse(overlaps_contained_withoutOverlap$came_after > 0.5, 1, 0)
overlaps_contained_withoutOverlap$subdivision <- ifelse(overlaps_contained_withoutOverlap$came_after <= 0.5, 1, 0)
#------------------------------------------------------------------------------#
overlaps_contained_withOverlap <- overlaps %>% group_by(COD_IMOVEL) %>% 
                                     mutate(is_contained = max(is_contained),
                                     big_overlap = sum(big_overlap)) %>% filter(big_overlap > 0 & is_contained == 1) %>% filter(as.numeric(pct_intersect) == 1)
overlaps_contained_withOverlap$subdivision <- 0
overlaps_contained_withOverlap$discard <- 0
for(i in unique(overlaps_contained_withOverlap$COD_IMOVEL.1)){
if(1/max(overlaps_contained_withoutOverlap %>% filter(COD_IMOVEL.1 == i) %>% count() %>% .$n, 1) < 0.1 &
   mean(overlaps_contained_withoutOverlap %>% filter(COD_IMOVEL.1 == i) %>% .$subdivision) > 0.5){
      overlaps_contained_withOverlap[overlaps_contained_withOverlap$COD_IMOVEL.1 == i, ]$subdivision <- 1
}
if(length(overlaps_contained_withoutOverlap %>% filter(COD_IMOVEL.1 == i) %>% count() %>% .$n) == 0){
      overlaps_contained_withOverlap[overlaps_contained_withOverlap$COD_IMOVEL.1 == i, ]$discard <- 1
}

}
overlaps_contained_withOverlap <- overlaps_contained_withOverlap %>% filter(discard == 0)
#------------------------------------------------------------------------------#
overlaps_notContained_withOverlap <- overlaps %>% group_by(COD_IMOVEL) %>% 
                                     mutate(is_contained = max(is_contained), big_overlap = sum(big_overlap)) %>% filter(big_overlap > 0 & is_contained == 0) %>%
                                     filter(as.numeric(pct_intersect) >= 0.1)
overlaps_notContained_withOverlap <- overlaps_notContained_withOverlap %>% group_by(COD_IMOVEL) %>% add_count() %>% rename(i_intersect_with = n) %>% 
                                     ungroup() %>% group_by(COD_IMOVEL.1) %>% add_count() %>% rename(it_intersects_with = n)
#------------------------------------------------------------------------------#
#plot(properties$geometry)
#plot(properties[properties$COD_IMOVEL %in% unique(n$COD_IMOVEL), ]$geometry, col = "darkred", add = T)
leftOver <- overlaps %>% filter(!COD_IMOVEL %in% unique(c(overlaps_contained_withOverlap$COD_IMOVEL, overlaps_notContained_withOverlap$COD_IMOVEL, overlaps_contained_withoutOverlap$COD_IMOVEL)))
leftOver$contains <- 0
leftOver$subdivision <- 0
for(i in unique(leftOver$COD_IMOVEL)){
  
  if(length(overlaps_contained_withOverlap %>% filter(COD_IMOVEL.1 == i) %>% count() %>% .$n) > 0){
    leftOver[leftOver$COD_IMOVEL == i, ]$contains <- 1
    
    if(mean(overlaps_contained_withOverlap %>% filter(COD_IMOVEL.1 == i) %>% .$subdivision) > 0.5){
      leftOver[leftOver$COD_IMOVEL == i, ]$subdivision <- 1
    }
  }
  
  if(length(overlaps_contained_withoutOverlap %>% filter(COD_IMOVEL.1 == i) %>% count() %>% .$n) > 0){
    leftOver[leftOver$COD_IMOVEL == i, ]$contains <- 1
    
    if(mean(overlaps_contained_withoutOverlap %>% filter(COD_IMOVEL.1 == i) %>% .$subdivision) > 0.5){
      leftOver[leftOver$COD_IMOVEL == i, ]$subdivision <- 1
    }
  }
  
}
superCARs <- leftOver %>% filter(contains == 1)
#------------------------------------------------------------------------------#
#---these are now basically 
leftOver <- leftOver %>% filter(contains == 0)

plot(properties$geometry)
plot(s[s$status == "CERTIFICADA",]$geometry, col = alpha("purple", 0.9), add = T)
plot(certified$geometry, col = alpha("green", 0.5), add = T)
plot(CAR_contains$geometry, add = T)
#plot(properties[properties$COD_IMOVEL %in% unique(pdfs %>% .$COD_IMOVEL), ]$geometry, col = "red", add = T)
#plot(properties[properties$COD_IMOVEL %in% unique(lie2 %>% .$COD_IMOVEL), ]$geometry, col = "darkred", add = T)
plot(noOverlap_CAR$geometry, col = alpha("steelblue1", 0.5), add = T)
plot(overlap_CAR$geometry, col = alpha("red", 0.5), add = T)

plot(properties[properties$COD_IMOVEL %in% unique(lie2 %>% filter(as.numeric(share_adjacent) < 1) %>% .$COD_IMOVEL), ]$geometry, col = "green", add = T)
plot(properties[properties$COD_IMOVEL %in% unique(contained$COD_IMOVEL), ]$geometry, col = "lightblue", add = T)
plot(properties %>% filter(!COD_IMOVEL %in% pdfs$COD_IMOVEL) %>% .$geometry, col = "yellow", add = T)
#unique(pdfs %>% group_by(COD_IMOVEL) %>% mutate(pct_intersect = sum(intersect_area)/imovel_area) %>% filter(row_number(COD_IMOVEL) == 1) %>% filter(as.numeric(pct_intersect) > 0.1) %>% .$COD_IMOVEL)


#plot(properties$geometry)
a <- properties %>% filter(row_number() == 1) %>% 
               st_intersection(properties %>% filter(row_number() != 1)) %>%
               mutate(intersect_area = st_area(.)) %>%
  dplyr::select(COD_IMOVEL, intersect_area, imovel_area) %>%   # only select columns needed to merge
  st_drop_geometry()  # drop geometry as we don't need it







p <- properties %>%
  st_transform(5070) %>% #convert from lat/long to projected coord system
  mutate(length_perim = lwgeom::st_perimeter(.))

p_overlap <- p %>%
  st_cast('MULTILINESTRING') %>%
  st_intersection() %>%
  mutate(length_intersection = st_length(.)) %>%
  filter(length_intersection > units::as_units(0,'m')) %>%
  group_by(COD_IMOVEL) %>%
  mutate(length_intersection = sum(length_intersection),
         share_adjacent = length_intersection/length_perim) %>% filter(row_number(COD_IMOVEL) == 1)


adj_pops <- function(COD_IMOVEL){
  ID_adj <- p_overlap %>%
    st_drop_geometry() %>%
    filter(map_lgl(origins, ~COD_IMOVEL %in% .x)) %>%
    unnest_longer(origins) %>%
    filter(origins != COD_IMOVEL) %>%
    pull(origins)
  
  p %>%
    st_drop_geometry() %>%
    filter(GEOID %in% ID_adj) %>%
    summarize(POP_adj = sum(POP)) %>%
    pull(POP_adj)
}

SD %>%
  mutate(POP_adj = map_dbl(GEOID, sum_adj_pops))



plot(properties$geometry)
plot(properties[1, ]$geometry, col = "steelblue1", add = T)
plot(properties[2, ]$geometry, col = "red", add = T)
plot(properties[5, ]$geometry, col = "purple", add = T)
plot(properties[80, ]$geometry, col = "green", add = T)
plot(properties[520:538, ]$geometry)
plot(properties[1080, ]$geometry, col = "green", add = T)
plot(properties[properties$COD_IMOVEL == "PA-1505650-0036307C60314248AA251F463E6E41CE", ]$geometry, type = "l", col = "yellow", add = T)


  # Calculate area and tidy up
  intersect_pct <- st_intersection(car, p) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(COD_IMOVEL, intersect_area) %>%   # only select columns needed to merge
  st_drop_geometry()  # drop geometry as we don't need it



file_name <- list.files(path="/Volumes/ElementsMain/transitions_combined/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
file_name <- file_name[str_detect(file_name, "2019")]
r <- rast(paste0("/Volumes/ElementsMain/transitions_combined/", file_name[1]))

r <- crop(r, states %>% filter(SIGLA == "RO"))
ro <- states %>% filter(SIGLA == "RO")
r <- as.factor(r)
rr <- crop(r, car)

car1 <- car[1:100, ]
plot(car$geometry)
plot(car[1, ]$geometry, col = "steelblue1", add = T)
plot(car[2, ]$geometry, col = "red", add = T)

rr1 <- crop(rr, car1)

p1 <- terra::extract(rr1, car1, xy = T)
unique(p1$layer)
length(p1$layer)
length(p1 %>% filter(layer == 2) %>% .$layer)*0.09

length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100




















ccar_clean <- gSimplify(methods::as(object = ccar_clean, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
ccar_dirty <- gSimplify(methods::as(object = ccar_dirty, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)

tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#F1BB7B", 1), alpha("#81A88D", 0.4)),
                border.col = "grey40",
                labels = c('Rural Properties', "Glebas Federais")) +
  tm_shape(ccar_clean) + tm_borders(col = "#F1BB7B", lwd = .1) +
  tm_shape(ccar_dirty) + tm_borders(col = "#5B1A18", lwd = .1) +
  tm_shape(glebas) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "#81A88D") +
  tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)



temas_ambientais_update <- read_delim("~/Library/CloudStorage/Dropbox/amazonLandPrices_project/data/raw/sicar/microdata/temas_ambientais_update.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
#temas_ambientais <- read_delim("~/Library/CloudStorage/Dropbox/amazonLandPrices_project/data/raw/sicar/microdata/temas_ambientais.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

car$registro_car <- car$COD_IMOVEL
car <- car %>% left_join(temas_ambientais_update, "registro_car")

plot(car$geometry)

file_name <- list.files(path="/Volumes/ElementsMain/transitions_combined/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
file_name <- file_name[str_detect(file_name, "2019")]
r <- rast(paste0("/Volumes/ElementsMain/transitions_combined/", file_name[1]))

r <- crop(r, states %>% filter(SIGLA == "RO"))
ro <- states %>% filter(SIGLA == "RO")
r <- as.factor(r)
rr <- crop(r, car)

car1 <- car[1:100, ]
plot(car$geometry)
plot(car[1, ]$geometry, col = "steelblue1", add = T)

rr1 <- crop(rr, car1)

p1 <- terra::extract(rr1, car1, xy = T)
unique(p1$layer)
length(p1$layer)
length(p1 %>% filter(layer == 2) %>% .$layer)*0.09

length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100



ggplot() +
  tidyterra::geom_spatraster(data = rr1, aes(fill = layer)) +
  tidyterra::scale_fill_whitebox_d(
    palette = "bl_yl_rd", na.translate = T) + theme_minimal() + labs(title = "2002") + 
  scale_fill_manual(values = colors, labels = labels, na.translate = F, na.value = "white") + 
  #tidyterra::geom_spatvector(data = amazon_bioma, fill = NA, col = "black") + 
  #tidyterra::geom_spatvector(data = ro, fill = NA, col = "white") + 
  tidyterra::geom_spatvector(data = car1, fill = NA, col = "steelblue1", lwd = 1) + 
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=10,  family = "Palatino"), text = element_text(family = "Palatino"),
        legend.title = element_blank(), legend.box="horizontal",  legend.position="bottom",
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.grid.major = element_blank(), 
        rect = element_blank()) ->> a

a
ggsave(a, filename = paste0("/Users/pedrotremacoldirossi/Documents/", "m2002", ".pdf"), device = cairo_pdf, dpi = 500,
       width = 5, # The width of the plot in inches
       height = 8, units = "in")



car %>% filter(COD_IMOVEL %in% temas_ambientais_update$registro_car)


