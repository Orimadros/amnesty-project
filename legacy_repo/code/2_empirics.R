options(scipen=999)
library('stringr') 
library(tmap)         # For creating tmap
library(tmaptools)    # For reading and processing spatial data related to tmap
library(sf)  
library(sp)
#library(rgeos)
library(terra)
library(wesanderson)
library(rmapshaper)
library(stringr)
library(extrafont)
library(timeDate)
library(stars)
library(data.table)
#library(emilfun)
library(grDevices)
library(strex)
library(stringi)
detach(package:plyr)
detach(package:dplyr)
library(plyr)
library(tikzDevice)
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
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  terra, # handle raster data
  raster, # handle raster data
  exactextractr, # fast extractions
  sf, # vector data operations
  dplyr, # data wrangling
  tidyr, # data wrangling
  data.table, # data wrangling
  prism, # download PRISM data
  tictoc, # timing codes
  tigris, # to get county sf
  tmap # for mapping
)
#----------------------------------#
#--- INTEGRATE 
#---
#----------------------------------#
sf_use_s2(FALSE)
#---load Amazon biome boundaries---#  
amazon_bioma <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/auxiliary/amazon_biome_border/amazon_biome_border.shp")
#terras indigenas
#indigenous <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/auxiliary/indigenous_area_amazon_biome/indigenous_area_amazon_biome.shp")
# #unidades de conservacao
#conservation <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/auxiliary/conservation_units_amazon_biome/")
# #control areas #DEPRACATED
#control_areas_deprecated <- st_union(st_union(indigenous), st_union(conservation))
#control_areas_deprecated <- st_collection_extract(control_areas_deprecated)
#control_areas_deprecated <- st_intersection(control_areas_deprecated, amazon_bioma)
#florestas publicas nao designadas
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/cnfp/SHP_2013/", pattern="*.shp$", recursive = TRUE)
fpnd <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/cnfp/SHP_2013/", f))
  fpnd <- rbind(fpnd, s)
}
fpnd <- st_intersection(fpnd, amazon_bioma)
#control areas
control_areas <- fpnd %>% filter(governo == "FEDERAL") %>% filter(str_detect(classe, "UC") | str_detect(classe, "TI")) #%>% filter(ano < 2010)
control_areas <- st_intersection(control_areas, amazon_bioma)
#---load glebas federais in TLP---#  
glebas_alt <- fpnd %>% filter(governo == "FEDERAL") %>% filter(str_detect(protecao, "SEM DESTINACAO")) #%>% filter(ano < 2010)
glebas_alt <- st_intersection(glebas_alt, amazon_bioma)

# glebas <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/auxiliary/glebas_federais/i3geomap_glebas_federais.shp")
# glebas <- st_intersection(glebas, amazon_bioma)
# glebas <- st_union(st_combine(glebas))
# glebas <- st_make_valid(glebas)
#---load states---#  
states <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/auxiliary/BR_UF_2021/BR_UF_2021.shp") %>% st_make_valid()
states <- st_intersection(states, amazon_bioma)
states1 <- states[st_geometry_type(states) == "MULTIPOLYGON", ]
states2 <- states[st_geometry_type(states) == "POLYGON", ]
states3 <- states[str_detect(st_geometry_type(states), "GEOMETRYCOLLECTION"), ]
states3 <- st_collection_extract(states3)
#---assign as control areas those net of target areas---#
#---we don't know precisely when non-designated glebas became rez/conser---#
#---but based on FOIA by other people, it does seem glebas almost perfectly---#
#---map program target areas---#
# A helper function that erases all of y from x: 
st_erase = function(x, y) st_difference(x, y)
#control_areas_deprecated <- st_erase(control_areas_deprecated, glebas)
#----------------------------------##----------------------------------#
# glebas1 is identical to glebas, we use glebas1 because it's
# in a better spatial format for operations
#----------------------------------##----------------------------------#
#glebas1 <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/auxiliary/glebas_federais/i3geomap_glebas_federais.shp")
#glebas1 <- st_intersection(glebas1, amazon_bioma)
#----------------------------------##----------------------------------#
#some rez/conserv intersect with glebas federais; remove them since
#they make properties not eligible for treatment
#----------------------------------##----------------------------------#
#amazon_biomac <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
#glebasc <- gSimplify(methods::as(object = glebas, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
#glebas1c <- gSimplify(methods::as(object = glebas1, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
#glebas1 <- st_erase(glebas1, st_union(control_areas_deprecated))
#glebas1_u <- st_union(glebas1)
# # # # # # # # # # # # # # # # # # # # # # # # # # # 
#glebas1 <- st_erase(glebas1, st_union(indigenous))
#glebas1 <- st_erase(glebas1, st_union(conservation))
#glebas1_u <- st_union(glebas1)
#glebas_net <- st_erase(glebas, st_union(glebas1))
   # tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
   # tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#F59077", 0.4)),
   #               border.col = "grey40",
   #               labels = c('Target areas', "Rez/cons areas")) +
   # tm_shape(glebas1_u) + tm_fill(col = "#02401B", alpha = 0.4, border.col = "#02401B") +
   # tm_borders(col = "#02401B", lwd = .5) +
   # tm_shape(control_areas_deprecated) + tm_fill(col = "#F59077", alpha = 0.4, border.col = "#F59077") +
   # tm_borders(col = "#F59077", lwd = .5) +
   # tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
   # tm_shape(states3) + tm_borders(col = "gray", lwd = 1) + 
   # tm_layout(frame = FALSE, legend.title.size = 1, legend.text.size = 1, legend.position = c("right","bottom"),
   #           legend.text.fontfamily="Times New Roman", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T) ->> a
   # 
   # tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
   #   tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#F59077", 0.4)),
   #                 border.col = "grey40",
   #                 labels = c('Target areas', "Rez/cons areas")) +
   #   tm_shape(glebas_alt) + tm_fill(col = "#02401B", alpha = 0.4, border.col = "#02401B") +
   #   tm_borders(col = "#02401B", lwd = .5) +
   #   tm_shape(control_areas) + tm_fill(col = "#F59077", alpha = 0.4, border.col = "#F59077") +
   #   #tm_borders(col = "#F59077", lwd = .5) +
   #   tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
   #   tm_shape(states3) + tm_borders(col = "gray", lwd = 1) + 
   #   tm_layout(frame = FALSE, legend.title.size = 1, legend.text.size = 1, legend.position = c("right","bottom"),
   #             legend.text.fontfamily="Times New Roman", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T) ->> b 
# 
# tmap_save(a, filename = paste0("/Users/pedrotremacoldirossi/Documents/", "areas_old", ".pdf"),
#           device = cairo_pdf,
#           width = 13, # The width of the plot in inches
#           height = 7, units = "in")
#    
# tmap_save(b, filename = paste0("/Users/pedrotremacoldirossi/Documents/", "areas_updated", ".pdf"),
#        device = cairo_pdf,
#        width = 13, # The width of the plot in inches
#        height = 7, units = "in")
#RELOAD AND RE-RERUN GLEBAS TO NET OUT CORRECT (UPDATED) CONTROL AREAS (REZ/UC)   
# glebas1 <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/auxiliary/glebas_federais/i3geomap_glebas_federais.shp")
# glebas1 <- st_intersection(glebas1, amazon_bioma)
# glebas_final <- st_erase(glebas1, st_union(control_areas))
# glebas_final <- st_union(glebas_final)
glebas_alt <- st_erase(glebas_alt, st_union(control_areas))
#glebas_alt <- st_union(glebas_alt)
# If needed: combine all states into one object
all_states <- rbind(states1, states2, states3)
glebas_alt$legend_group <- "Target Areas"
control_areas$legend_group <- "Control Areas"
all_states_proj <- st_transform(all_states, 5880)
control_areas_proj <- st_transform(control_areas, 5880)
glebas_alt_proj <- st_transform(glebas_alt, 5880)
amazon_bioma_proj <- st_transform(amazon_bioma, 5880)
all_states_plot <-st_simplify(all_states_proj, dTolerance = 100, preserveTopology = TRUE)
control_areas_plot <-st_simplify(control_areas_proj, dTolerance = 100, preserveTopology = TRUE)
glebas_alt_plot <-st_simplify(glebas_alt_proj, dTolerance = 100, preserveTopology = TRUE)
amazon_bioma_plot <-st_simplify(amazon_bioma_proj, dTolerance = 100, preserveTopology = TRUE)
glebas_alt_plot <- st_transform(glebas_alt_plot, 4326)
control_areas_plot <- st_transform(control_areas_plot, 4326)
amazon_bioma_plot <- st_transform(amazon_bioma_plot, 4326)
all_states_plot <- st_transform(all_states_plot, 4326)
# tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
#   tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#F59077", 0.4)),
#                 border.col = "grey40",
#                 labels = c("Rez/cons areas", 'Target areas')) +
#   tm_shape(control_areas) + tm_fill(col = "#02401B", alpha = 0.4, border.col = "#02401B") +
#   tm_borders(col = "#02401B", lwd = .5) +
#   tm_shape(glebas_alt) + tm_fill(col = "#F59077", alpha = 0.4, border.col = "#F59077") +
#   tm_borders(col = "#F59077", lwd = .5) +
#   tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
#   tm_shape(states3) + tm_borders(col = "gray", lwd = 1) + 
#   tm_layout(frame = FALSE, legend.title.size = 1, legend.text.size = 1, legend.position = c("right","bottom"),
#             legend.text.fontfamily="Times New Roman", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)
ggplot() +
  # Control areas as polygons with colored borders
  geom_sf(data = control_areas_plot, aes(color = legend_group), fill = alpha("#02401B", 0.2), linewidth = 0) +
  geom_sf(data = glebas_alt_plot, aes(color = legend_group), fill = alpha("#F59077", 0.9), linewidth = 0) +
  # State borders
  geom_sf(data = all_states_plot, fill = NA, color = "gray40", linewidth = 0.3) +
  # Amazon biome boundary
  geom_sf(data = amazon_bioma_plot, fill = NA, color = "black", linewidth = 0.4) +
  # Shared color legend
  scale_color_manual(name = "", 
                     values = c("Target Areas" = "#F59077", "Control Areas" = "#02401B")) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.9, 0.13),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.text = element_text(size = 16, family = "Times New Roman"),
    legend.key.size = unit(2, "lines"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank()
  ) -> plot
plot

ggsave(plot, filename = "~/Documents/g1_final.png", dpi = 300, width = 8, height = 5.5, units = "in")
# tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
#   tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#F59077", 0.4)),
#                 border.col = "grey40",
#                 labels = c('Target areas', "Rez/cons areas")) +
#   tm_shape(glebas_final) + tm_fill(col = "#02401B", alpha = 0.4, border.col = "#02401B") +
#   tm_borders(col = "#02401B", lwd = .5) +
#   tm_shape(control_areas) + tm_fill(col = "#F59077", alpha = 0.4, border.col = "#F59077") +
#   tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
#   tm_shape(states3) + tm_borders(col = "gray", lwd = 1) + 
#   tm_layout(frame = FALSE, legend.title.size = 1, legend.text.size = 1, legend.position = c("right","bottom"),
#             legend.text.fontfamily="Times New Roman", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)

#amazon_bioma <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
#states <- sf::st_simplify(states)
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
colors <- c("gray80", "#02401B", "#F1BB7B", "#0B775E")
labels <- c("Other", "Forest", "Deforested", "Reforested")
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#florestas publicas nao designadas
# file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/cnfp/SHP_2013/", pattern="*.shp$", recursive = TRUE)
# fpnd <- data.frame()
# for(f in file_name){
#   s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/cnfp/SHP_2013/", f))
#   fpnd <- rbind(fpnd, s)
# }
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#fpnd <- st_intersection(fpnd, amazon_bioma)
nonFederal <- fpnd %>% filter(governo %in% c("ESTADUAL", "MUNICIPAL", "FEDERAL/ESTADUAL"))
#nonFederal <- st_intersection(nonFederal, amazon_bioma)
#---#---#---#---#---#---#---
#federal <- fpnd %>% filter(governo == "FEDERAL")
#sum(federal %>% filter(protecao == "SEM DESTINACAO") %>% mutate(area = st_area(.)) %>% .$area)
#---#---#---#---#---#---#---
#federal <- federal %>% filter(protecao == "SEM DESTINACAO")
# nonFederal1 <- nonFederal[st_geometry_type(nonFederal) == "MULTIPOLYGON", ]
# nonFederal2 <- nonFederal[st_geometry_type(nonFederal) == "POLYGON", ]
# nonFederal3 <- nonFederal[str_detect(st_geometry_type(nonFederal), "GEOMETRYCOLLECTION"), ]
# nonFederal3 <- st_collection_extract(nonFederal3)
#----------------------------------##----------------------------------##----------------------------------##----------------------------------#
#----------------------------------##----------------------------------##----------------------------------##----------------------------------#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/processing/CleanCARShapes_robust/", pattern="*.shp", recursive = TRUE)
#---load clean CARs---#  
#---CARShapes_robust: fully "cleaned" version by Thiago ---#
#---Fully validated; not dealing with overlaps or related issues
#----------------------------------#
car <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/processing/CleanCARShapes_robust/", f))
  car <- rbind(car, s)
  print(f)
}
#st_write(car, "~/car_combined.shp")
car <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_working/amazon/car_combined.shp")
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
car_amazon <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_combined_amazonBiome2.shp")
#car_amazon_s <- gSimplify(methods::as(object = car_amazon, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
# tm_shape(car_amazon) + tm_borders(col = "#F1BB7B", lwd = .1) +
#   tm_shape(glebas1_u) + tm_fill(col = "#02401B", alpha = 0.4, border.col = "#02401B") +
#   tm_add_legend(c("fill"), col = c(alpha("#F1BB7B", 1), alpha("#02401B", 0.4)),
#                 border.col = "grey40",
#                 labels = c('Rural Properties', "Forgiveness-Eligible Areas")) +
#   tm_shape(car_amazon) + tm_borders(col = "#F1BB7B", lwd = .1) +
#   tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
#   tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
#   tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
#   tm_layout(frame = FALSE, legend.title.size = 1,
#             legend.text.size = 1, legend.position = c("right","bottom"),
#             legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)
#############################################################################################################################################################################################
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
snci$area <- as.numeric(snci$qtd_area_p)

already_treated <- snci  %>% filter(area < 100000) %>% filter(uf_municip %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO", "MT", "MA")) %>% st_intersection(amazon_bioma)
  #filter(year <= 2009)
st_write(already_treated, "~/Documents/already_treated.shp")
#load("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/code/patricio_preach_tomas_work/data/output/legal_car.Rdata")
#############################################################################################################################################################################################
#g <- st_union(glebas)
#plot(g)
#plot(glebas$geometry, col = "green", add = T)
#----------------------------------##----------------------------------#
# Calculate area and tidy up
#----------------------------------##----------------------------------#

#---drop duplicate CARs---#
car_amazon <- car_amazon %>% group_by(COD_IMOVEL) %>% filter(row_number(COD_IMOVEL) == 1) %>% ungroup()
car_amazon <- st_as_sf(car_amazon)
ccar <- mutate(car_amazon, imovel_area = st_area(car_amazon))
#----------------------------------##----------------------------------##----------------------------------##----------------------------------#
#----------------------------------##----------------------------------##----------------------------------##----------------------------------#
intersect_pct <- st_intersection(control_areas, car_amazon) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(COD_IMOVEL, intersect_area) %>%   # only select columns needed to merge
  st_drop_geometry()  # drop geometry as we don't need it
# intersect_pct <- st_intersection(indigenous, car_amazon) %>% 
#   mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
#   dplyr::select(COD_IMOVEL, intersect_area) %>%   # only select columns needed to merge
#   st_drop_geometry()  # drop geometry as we don't need it
# intersect_pct2 <- st_intersection(conservation, car_amazon) %>% 
#   mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
#   dplyr::select(COD_IMOVEL, intersect_area) %>%   # only select columns needed to merge
#   st_drop_geometry()  # drop geometry as we don't need it
# intersect_pct <- rbind(intersect_pct, intersect_pct2)
ccar_rez <- merge(ccar, intersect_pct, by = "COD_IMOVEL", all.x = TRUE)
library(data.table)
setDT(ccar_rez)
ccar_rez[, intersect_area := sum(intersect_area, na.rm = TRUE), by = COD_IMOVEL] ##will double count intersection areas, but not a problem here
ccar_rez[, overlap := as.numeric(intersect_area / imovel_area)]
ccar_rez <- ccar_rez[, .SD[1], by = COD_IMOVEL]
ccar_rez$overlap <- ifelse(is.na(ccar_rez$overlap), 0, ccar_rez$overlap)
ccar_rez <- st_as_sf(ccar_rez)

tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_shape(car_amazon) + tm_borders(col = "#5B1A18", lwd = .1) +
  tm_add_legend(c("fill"), col = c(alpha("#5B1A18", 1), alpha("#02401B", .7)),
                border.col = "grey40",
                labels = c('CARs', "Control Areas")) +
  tm_shape(control_areas) + tm_fill(col = "#02401B", alpha = 0.2, border.col = "#02401B") +
  #tm_shape(glebas) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "#81A88D") +
  tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + 
  tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)

#----------------------------------##----------------------------------#
ccar_clean_updated <- ccar_rez %>% filter(overlap*100 > 1) #overlap can be more than 100 (100%) because it's intersect area / imovel area: if imovel intersects with more than one object, will lead to >1, but not a problem here
st_write(ccar_clean_updated, "/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/ccar_clean.shp", append = F)
#----------------------------------##----------------------------------#
ccar_clean_updated <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/ccar_clean.shp")
# 
# tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
#   #tm_shape(ccar_clean) + tm_borders(col = "#5B1A18", lwd = .1) +
#   tm_add_legend(c("fill"), col = c(alpha("#5B1A18", 1), alpha("#02401B", .7)),
#                 border.col = "grey40",
#                 labels = c('Control CARs (updated)', "Control CARs (old)")) +
#   tm_shape(ccar_clean_updated) + tm_fill(col = "#02401B", alpha = 0.2, border.col = "#02401B") +
#   tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + 
#   tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
#   tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
#   tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
#   tm_layout(frame = FALSE, legend.title.size = 1,
#             legend.text.size = 1, legend.position = c("right","bottom"),
#             legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)
# #----------------------------------##----------------------------------#

#----------------------------------##----------------------------------##----------------------------------##----------------------------------#
#----------------------------------##----------------------------------##----------------------------------##----------------------------------#
ccar2 <- ccar %>% filter(!COD_IMOVEL %in% unique(ccar_clean_updated$COD_IMO))
ccar_not_in_rez <- car_amazon %>% filter(!COD_IMOVEL %in% unique(ccar_clean_updated$COD_IMO))

glebas_alt_valid <- st_make_valid(glebas_alt)
ccar_not_in_rez_valid <- st_make_valid(ccar_not_in_rez)
# # Fast spatial filter before intersection
# potential_matches <- st_intersects(glebas_alt_valid, ccar_not_in_rez_valid, sparse = TRUE)
# # Perform intersection only on pairs that intersect
# intersections_list <- lapply(seq_along(potential_matches), function(i) {
#   if (length(potential_matches[[i]]) > 0) {
#     st_intersection(glebas_alt_valid[i, ], ccar_not_in_rez_valid[potential_matches[[i]], ])
#   } else {
#     NULL
#   }
# })
# Combine all intersections
# states1 <- ccar_not_in_rez_valid[st_geometry_type(ccar_not_in_rez_valid) == "MULTIPOLYGON", ]
# states2 <- ccar_not_in_rez_valid[st_geometry_type(ccar_not_in_rez_valid) == "POLYGON", ]
# states3 <- ccar_not_in_rez_valid[str_detect(st_geometry_type(ccar_not_in_rez_valid), "GEOMETRYCOLLECTION"), ]
# states3 <- st_collection_extract(states3)
# intersect_pct <- do.call(rbind, intersections_list %>% head(10)) %>% 
#   st_as_sf()
# intersect_pct$intersect_area <- st_area(intersect_pct)
# intersect_pct <- intersect_pct %>%
#   as.data.frame() %>%
#   select(COD_IMOVEL, intersect_area)
# intersect_pct <- dplyr::bind_rows(intersections_list) %>%
#   mutate(intersect_area = st_area(.)) %>%
#   select(COD_IMOVEL, intersect_area) %>%
#   st_drop_geometry()
intersect_pct_1 <- st_intersection(glebas_alt_valid, ccar_not_in_rez_valid %>% slice(1:100000)) %>%
  dplyr::mutate(intersect_area = st_area(.)) %>%
  dplyr::select(COD_IMOVEL, intersect_area) %>%
  st_drop_geometry()
intersect_pct_2 <- st_intersection(glebas_alt_valid, ccar_not_in_rez_valid %>% slice(1000001:150000)) %>%
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(COD_IMOVEL, intersect_area) %>%
  st_drop_geometry()
intersect_pct_22 <- st_intersection(glebas_alt_valid, ccar_not_in_rez_valid %>% slice(1500001:200000)) %>%
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(COD_IMOVEL, intersect_area) %>%
  st_drop_geometry()
intersect_pct_3 <- st_intersection(glebas_alt_valid, ccar_not_in_rez_valid %>% slice(2000001:300000)) %>%
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(COD_IMOVEL, intersect_area) %>%
  st_drop_geometry()
intersect_pct_4 <- st_intersection(glebas_alt_valid, ccar_not_in_rez_valid %>% slice(3000001:400000)) %>%
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(COD_IMOVEL, intersect_area) %>%
  st_drop_geometry()
intersect_pct_5 <- st_intersection(glebas_alt_valid, ccar_not_in_rez_valid %>% slice(4000001:500000)) %>%
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(COD_IMOVEL, intersect_area) %>%
  st_drop_geometry()
intersect_pct_6 <- st_intersection(glebas_alt_valid, ccar_not_in_rez_valid %>% slice(5000001:600000)) %>%
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(COD_IMOVEL, intersect_area) %>%
  st_drop_geometry()
intersect_pct_7 <- st_intersection(glebas_alt_valid, ccar_not_in_rez_valid %>% slice(6000001:700000)) %>%
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(COD_IMOVEL, intersect_area) %>%
  st_drop_geometry()
intersect_pct_8 <- st_intersection(glebas_alt_valid, ccar_not_in_rez_valid %>% slice(7000001:length(ccar_not_in_rez_valid$COD_IMOVEL))) %>%
  mutate(intersect_area = st_area(.)) %>%
  dplyr::select(COD_IMOVEL, intersect_area) %>%
  st_drop_geometry()

intersect_pct <- rbind(intersect_pct_1, intersect_pct_2, intersect_pct_22, intersect_pct_3, intersect_pct_4, intersect_pct_5, intersect_pct_6,
                       intersect_pct_7, intersect_pct_8)

ccar_not_in_rez <- merge(ccar2, intersect_pct, by = "COD_IMOVEL", all.x = TRUE)
library(data.table)
setDT(ccar_not_in_rez)
ccar_not_in_rez[, intersect_area := sum(intersect_area, na.rm = TRUE), by = COD_IMOVEL] ##will double count intersection areas, but not a problem here
ccar_not_in_rez[, overlap := as.numeric(intersect_area / imovel_area)]
ccar_not_in_rez <- ccar_not_in_rez[, .SD[1], by = COD_IMOVEL]
ccar_not_in_rez$overlap <- ifelse(is.na(ccar_not_in_rez$overlap), 0, ccar_not_in_rez$overlap)
ccar_not_in_rez <- st_as_sf(ccar_not_in_rez)
#----------------------------------#
#---CAR registered inside glebas --#
ccar_dirty <- ccar_not_in_rez %>% filter(overlap*100 > 1)

tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_shape(ccar_clean_updated) + tm_borders(col = "#02401B", lwd = .1) +
  tm_shape(ccar_dirty) + tm_borders(col = "#5B1A18", lwd = .1) +
  #tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#5B1A18", 1), alpha("#02401B", .7)),
                border.col = "grey40",
                labels = c("Rural Properties in Target Areas", 'Rural Properties in Control Areas')) +
  tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + 
  tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)
#----------------------------------#
st_write(ccar_dirty, "/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/ccar_dirty.shp", append = F)
#----------------------------------##----------------------------------#
ccar_dirty <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/ccar_dirty.shp")
#----------------------------------##----------------------------------##----------------------------------##----------------------------------##----------------------------------#
#----------------------------------##----------------------------------##----------------------------------##----------------------------------##----------------------------------#
#----------------------------------##----------------------------------##----------------------------------##----------------------------------##----------------------------------#

#----------------------------------##----------------------------------#
#ccar_clean: any CAR with more than 5% overlap with rez/conser
#ccar_dirty: any CAR without overlap with rez/conser and overlap of >5% with gleba
#----------------------------------##----------------------------------#
ccar_clean_updated <- ccar_clean_updated %>% dplyr::select(-intrsc_, -overlap)
# #----------------------------------#
# #---CARs outside federal glebas in state glebas---# 
# #----------------------------------#
# ccar_stateGlebas <- st_intersection(ccar_clean, nonFederal)
# ccar_stateGlebas <- st_as_sf(ccar_stateGlebas)
# ccar_stateGlebas <- ccar_clean[ccar_clean$COD_IMO %in% ccar_stateGlebas$COD_IMO, ]
# #----------------------------------#
# #---CARs outside federal and state glebas in indigenous or reservation land---# 
# #----------------------------------#
# ccar_clean_notStateGlebas <- ccar_clean[!ccar_clean$COD_IMO %in% ccar_stateGlebas$COD_IMO, ]
# ccar_clean_notStateGlebas_it <- st_intersection(ccar_clean_notStateGlebas, control_areas)
# ccar_clean_inReservas <- ccar_clean_notStateGlebas[ccar_clean_notStateGlebas$COD_IMO %in% ccar_clean_notStateGlebas_it$COD_IMO, ]
# #some CARs span more than one rez/conserv area, so they get duplicated 
# #NOTE: most overlaps are with conservation areas, not rez
# ccar_clean_inReservas <- ccar_clean_inReservas %>% group_by(COD_IMO) %>% filter(row_number(COD_IMO) == 1) %>% ungroup()
# #----------------------------------##----------------------------------#
# #st_write(ccar_clean_inReservas, "/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_working/amazon/ccar_clean_inReservas.shp", append = F)
# ccar_clean_inReservas <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_working/amazon/ccar_clean_inReservas.shp")
# #----------------------------------##----------------------------------#
# ccar_clean_inReservas <- st_as_sf(ccar_clean_inReservas)
# #ccar_clean_inReservasc <- gSimplify(methods::as(object = ccar_clean_inReservas, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)

#----------------------------------##----------------------------------#
#---CARs eligible to be in the "pure" control group---# 
ccar_controlGroup <- car_amazon[!(car_amazon$COD_IMOVEL %in% ccar_clean_updated$COD_IMO | car_amazon$COD_IMOVEL %in% ccar_dirty$COD_IMO), ]
#----------------------------------##----------------------------------#
#ccar_dirtyc <- gSimplify(methods::as(object = ccar_dirty, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#ccar_clean_inReservasc <- gSimplify(methods::as(object = ccar_clean_inReservas, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#ccar_stateGlebas1 <- ccar_stateGlebas[st_geometry_type(ccar_stateGlebas) == "MULTIPOLYGON", ]
#ccar_stateGlebas2 <- ccar_stateGlebas[st_geometry_type(ccar_stateGlebas) == "POLYGON", ]
#ccar_stateGlebas3 <- ccar_stateGlebas[str_detect(st_geometry_type(ccar_stateGlebas), "GEOMETRYCOLLECTION"), ]
#ccar_stateGlebas3 <- st_collection_extract(ccar_stateGlebas3)
#ccar_stateGlebas1c <- gSimplify(methods::as(object = ccar_stateGlebas1, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#ccar_stateGlebas2c <- gSimplify(methods::as(object = ccar_stateGlebas2, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#ccar_stateGlebas3c <- gSimplify(methods::as(object = ccar_stateGlebas3, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#ccar_controlGroupc <- gSimplify(methods::as(object = ccar_controlGroup, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#amazon_biomac <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
#indigenousc <- gSimplify(methods::as(object = indigenous, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#conservationc <- gSimplify(methods::as(object = conservation, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
  tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_shape(ccar_dirty) + tm_borders(col = "#02401B", lwd = .1) +
  #tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#5B1A18", 1), alpha("#02401B", .7)),
                border.col = "grey40",
                labels = c('Ineligible Rural Properties', "Rural Properties in Target Areas")) +
  tm_shape(ccar_clean_updated) + tm_borders(col = "#5B1A18", lwd = .1, alpha = 0.7) +
  tm_shape(glebas_alt) + tm_fill(col = "#02401B", alpha = 0.2, border.col = "#02401B") +
  tm_shape(control_areas) + tm_fill(col = "#5B1A18", alpha = 0.2, border.col = "#5B1A18") +
  #tm_shape(indigenous) + tm_fill(col = "#5B1A18", alpha = 0.2, border.col = "#5B1A18") +
  #tm_shape(conservation) + tm_fill(col = "#5B1A18", alpha = 0.2, border.col = "#5B1A18") +
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
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---###---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#---compute whether CAR has---# 
#---[1] partial overlap    ---#  
#---[2] full overlap       ---#  
#---[3] no overlap         ---#  
temas_ambientais_update <- read_delim("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/sicar/microdata/temas_ambientais_update.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
#temas_ambientais <- read_delim("~/Library/CloudStorage/Dropbox/amazonLandPrices_project/data/raw/sicar/microdata/temas_ambientais.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
ccar_dirty$registro_car <- ccar_dirty$COD_IMO
ccar_dirty <- ccar_dirty %>% left_join(temas_ambientais_update, "registro_car")
# ssa <- st_intersection(ccar_dirty, control_areas)
# ssa <- st_as_sf(ssa)
# ccar_dirty[ccar_dirty$COD_IMO %in% ssa$COD_IMO, ]
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---WORKING EXAMPLE: Placas, PA---#

#---for CARS in the glebas, we implement the following polygon cleaning:
#--- (1) CARs with no conflict, leave alone
#--- (2) CARs contained inside another CAR: if deforestation of larger CAR is inside smaller CAR, drop bigger; if deforestation is outside smaller CAR, drop smaller; if in both and far away from each other, keep both
#--- (3) CARS with partial overlap: if deforestation in each CAR, keep both; if only inside intersection, keep one CAR only randomly


#---STEP 1: DROP CAR WITH NO USE EVER [up until 2019] 
i.run <- function(j){
  properties <- ccar_dirty %>% filter(codigo_ibge == j)
  #properties <- ccar_dirty %>% filter(NOM_MUN == "Santana do Araguaia")
  properties <- properties[!duplicated(properties), ]
  
  #---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
  #---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
  file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
  f <- file_name[str_detect(file_name, "2019")]
  r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", f[1]))
  #r <- crop(r, states %>% filter(SIGLA == properties$COD_ESTADO[1]))
  p <- properties %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup()
  booths_buffer = st_buffer(p, .3)
  #plot(booths_buffer)
  #plot(p$geometry, add = T)
  rr <- crop(r, booths_buffer)
  pp <- data.frame()
  for(k in 1:length(properties$COD_IMO)){
    p1 <- terra::extract(rr, properties[k, ], xy = T)
    p1 <- c(properties$COD_IMO[k], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
    pp <- rbind(pp, p1)
    #print(k)
  }
  colnames(pp) <- c("COD_IMO", "deforested_area_hc", "deforestation_rate")
  active2014_CAR <- properties %>% filter(COD_IMO %in% unique(pp %>% filter(as.numeric(deforestation_rate) > 10) %>% .$COD_IMO)) %>% left_join(pp %>% dplyr::select(COD_IMO, deforested_area_hc, deforestation_rate), "COD_IMO")
  #----------------------------------##----------------------------------#
  st_write(active2014_CAR %>% dplyr::select(geometry, COD_IMO, NUM_ARE, codigo_ibge, deforestation_rate, deforested_area_hc), paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/active2014_inGleba_CAR_", j,".shp"), append = F)
  #----------------------------------##----------------------------------#
}
# rr_p <- as.factor(rr)
# ggplot() +
#   tidyterra::geom_spatraster(data = rr_p, aes(fill = layer)) +
#   tidyterra::scale_fill_whitebox_d(
#     palette = "bl_yl_rd", na.translate = T) + theme_minimal() + #labs(title = "MAP") + 
#   scale_fill_manual(values = alpha(colors, 0.5), labels = labels, na.translate = F, na.value = "white") + 
#   tidyterra::geom_spatvector(data = properties, fill = NA, col = "steelblue4", alpha = 0.1, lwd = 0.1) + 
#   tidyterra::geom_spatvector(data = active2014_CAR, fill = NA, col = "steelblue1", alpha = 0.1, lwd = 0.2) + 
#   #tidyterra::geom_spatvector(data = booths_buffer, fill = NA, col = "steelblue1", alpha = 0.5, lwd = 1) + 
#   theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), 
#         legend.text = element_text(size=10,  family = "Palatino"), legend.title = element_blank(), 
#         legend.box="horizontal",  legend.position="bottom", legend.box.background = element_rect(colour = "black"),
#         panel.background = element_blank(), panel.grid.major = element_blank(), rect = element_blank())
registerDoParallel(cores = 10)
pdfs <- foreach(c=unique(ccar_dirty$codigo_ibge)) %dopar% i.run(c)
#------------------------------------------------------------------------------------#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "inGleba_") & str_detect(file_name, "2014") & !str_detect(file_name, "cleaned")]

car2014 <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", f))
  car2014 <- rbind(car2014, s)
  print(which(f == file_name))
}
#-------------------------------------------------------------------##-------------------------------------------------------------------#
st_write(car2014, "/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_active_2014.shp", append = F)
car2014 <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_active_2014.shp")
#-------------------------------------------------------------------##-------------------------------------------------------------------#

#------------------------------------------------------------------------------------#
#---- RECORD 2004 DEFORESTATION LEVEL IN 2004 OF CARs ACTIVE IN 2017
##### reads in: active2014_inGleba_...################################################
##### outputs:  active2014_inGleba2_...###############################################
#------------------------------------------------------------------------------------#
files <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
files <- files[str_detect(files, "inGleba") & str_detect(files, "2014") & !str_detect(files, "inGleba2") & !str_detect(files, "cleaned")]
#f2 <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_working/amazon/occupationCAR/", pattern="*.shp", recursive = TRUE)
#f2 <- f2[str_detect(f2, "inGleba2")]
#files <- files[!str_extract(files, "([0-9])([0-9])([0-9])([0-9])([0-9]+)") %in% str_extract(f2, "([0-9])([0-9])([0-9])([0-9])([0-9]+)")]
#files <- files[!str_extract(files, "_([0-9]+)") %in% str_extract(f2, "_([0-9]+)")]
i.run <- function(j){
#for(j in files){
  properties <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", j))
if(length(properties$COD_IMO) > 0){
  properties <- properties[!duplicated(properties), ]
  #properties <- ccar_dirty %>% filter(NOM_MUN == "Santana do Araguaia")
  
  #---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
  #---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
  file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
  f <- file_name[str_detect(file_name, "2004")]
  r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", f[1]))

  pp <- data.frame()
  for(k in 1:length(properties$COD_IMO)){
    p1 <- terra::extract(r, properties[k, ], xy = T)
    p1 <- c(properties$COD_IMO[k], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
    pp <- rbind(pp, p1)
    #print(k)
  }
  colnames(pp) <- c("COD_IMO", "deforested_area_hc", "deforestation_rate")
  properties <- properties %>% dplyr::select(-dfrst__, - dfrstt_)
  defo2004_CAR <- properties %>% left_join(pp %>% dplyr::select(COD_IMO, deforested_area_hc, deforestation_rate), "COD_IMO")
  #----------------------------------##----------------------------------#
  st_write(defo2004_CAR %>% dplyr::select(geometry, COD_IMO, NUM_ARE, codg_bg, deforestation_rate, deforested_area_hc), paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/active2014_inGleba2_CAR", str_extract(j, "_([0-9]+)"),".shp"), append = F)
  #----------------------------------##----------------------------------#
}
  #print(j)
}
registerDoParallel(cores = 10)
pdfs <- foreach(c=1:length(files)) %dopar% i.run(files[c])
#------------------------------------------------------------------------------------#  
#----                                   EXPORT                                   ----#
#------------------------------------------------------------------------------------#  
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "inGleba2") & str_detect(file_name, "2014") & !str_detect(file_name, "cleaned")]

car2014_all_uncleaned <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", f))
  car2014_all_uncleaned <- rbind(car2014_all_uncleaned, s)
  print(f)
}
#-------------------------------------------------------------------##-------------------------------------------------------------------#
st_write(car2014_all_uncleaned, "/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_all_uncleaned.shp", append = F)
car2014_all_uncleaned <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_all_uncleaned.shp")
#-------------------------------------------------------------------##-------------------------------------------------------------------#

#------------------------------------------------------------------------------------#
#---- SPATIAL CONFLICT RESOLUTION ALGORITHM -----------------------------------------#
##### reads in: active2014_inGleba2_...###############################################
##### outputs: active2014_inGleba_cleaned_############################################
#------------------------------------------------------------------------------------#
ff <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", pattern="*.tif", recursive = TRUE)
ff <- ff[str_detect(ff, "plot_legacy_forest")]
ff <- ff[str_detect(ff, "2004")]
r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", ff[1]))
#------------------------------------------------------------------------------------#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "inGleba2") & str_detect(file_name, "2014") & !str_detect(file_name, "cleaned")]
#fa <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
#fa <- fa[str_detect(fa, "inGleba") & str_detect(fa, "2014") & str_detect(fa, "cleaned")]
#file_name <- file_name[!str_extract(file_name, "_([0-9]+)") %in% str_extract(fa, "_([0-9]+)")]
i.run <- function(file){
#for(file in file_name){
  car <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  rm(insiders, insiders_1, insiders_2, overs, pdfs)
  car <- car[!duplicated(car$COD_IMO), ]
  ff <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", pattern="*.tif", recursive = TRUE)
  ff <- ff[str_detect(ff, "plot_legacy_forest")]
  ff <- ff[str_detect(ff, "2004")]
  r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", ff[1]))
if(length(car$COD_IMO) > 0){  
  p <- car %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup()
  booths_buffer = st_buffer(p, .3)
  rr <- crop(r, booths_buffer)
  
  #plot(car$geometry)
  i.run <- function(i){
    car %>% filter(row_number() == i) %>% 
      st_intersection(car %>% filter(row_number() != i)) %>%
      mutate(intersect_area = st_area(.)) #%>%
      #dplyr::select(COD_IMOVEL, intersect_area, imovel_area, imovel_area.1, COD_IMOVEL.1, data_inscricao, data_inscricao.1) %>%   # only select columns needed to merge
      #st_drop_geometry()  # drop geometry as we don't need it
  }
  registerDoParallel(cores = 10)
  pdfs <- foreach(c=1:length(car$COD_IMO)) %dopar% i.run(c)
  pdfs <- bind_rows(pdfs)
  pdfs$intersect_area <- as.numeric(pdfs$intersect_area)/10000
  pdfs$pct_intersect_i <- pdfs$intersect_area/pdfs$NUM_ARE
  pdfs$pct_intersect_minus_i <- pdfs$intersect_area/pdfs$NUM_ARE.1
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#  plot(car$geometry)
#  plot(car[car$COD_IMO == "MT-5106000-06BC34E3C6C846C688D17C2200D5FC14", ]$geometry, col = NA, border = "steelblue1", add = T)
#  plot(car[car$COD_IMO == "MT-5106000-14D8DEB3F9A84D29926B8EDB85CE1767", ]$geometry, col = NA, border = "red", add = T) #> 10%
#  plot(car[car$COD_IMO == "MT-5106000-9662133C0D3B4A9DBDF2E780B3E2444F", ]$geometry, col = NA, border = "purple", add = T) #> 10%
#  plot(car[car$COD_IMO == "MT-5106000-A9E90E86458942538CC3C9685970CF51", ]$geometry, col = NA, border = "green", add = T) #> 10%
#  plot(car[car$COD_IMO == "MT-5106000-BB84FCDFD19647E68741D7AA746F3287", ]$geometry, col = NA, border = "yellow", add = T) #< 10%
#  plot(car[car$COD_IMO == "MT-5106000-F4DDF87B78874598B021544793A9496D", ]$geometry, col = NA, border = "orange", add = T) #< 10%
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
  pdfs <- pdfs %>% group_by(COD_IMO) %>% mutate(max_pct_intersect = max(pct_intersect_i))
  pdfs <- pdfs %>% filter(pct_intersect_i > .1) #keeps all >10% overlaps for i 
  #------------------------------------------------------------------------------------#
  #---pdfs contain CARs that intersect at least partially at least with another CAR ---#
  #---the area in pdfs is the shape of the intersection!-------------------------------#
  #------------------------------------------------------------------------------------#    
  #--- (1) CARs with no conflict, leave alone ---#  
noConflict <- car %>% filter(!COD_IMO %in% unique(pdfs$COD_IMO)) #if i doesn't have any >10% overlap
  #------------------------------------------------------------------------------#  
  pdfs <- pdfs %>% filter(!COD_IMO %in% noConflict$COD_IMO)
  #is the shape with partial overlap?
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#  
#  plot(noConflict$geometry)
#  plot(pdfs$geometry, col = NA, border = "steelblue1", add = T)
  #---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
pdfs$drop_i <- NA
pdfs$drop_minus_i <- NA
pdfs <- st_as_sf(as.data.frame(pdfs))
options(scipen=999)

pdfs$id <- paste0(pdfs$COD_IMO, pdfs$COD_IMO.1) # GENERATE ID FOR THE PAIR WITH OVERLAP
pdfs <- pdfs[!duplicated(pdfs$id), ]
pdfs <- pdfs %>% dplyr::select(-id)

if(length(pdfs$COD_IMO) > 0){
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#    
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#    
#drop_i: whether CAR i has >80% of its deforestation in the intersection  
#drop_minus_i: whether -i has >80% of its deforestation in the intersection
#if both are 0, it means i and -i have a small % of defo in intersection 
#if both are 1, it means that a) i and -i have defo in intersection OR b) i (-i) contains -i (i) and defo is in -i (i) 
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#    
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---# 
for(i in 1:length(pdfs$COD_IMO)){
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#    
  #determine if polygon contains any subset of polygons
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#    
if(st_geometry_type(pdfs[i, ]) != "GEOMETRYCOLLECTION" & pdfs[i, ]$dfrst__ != "0" & car[car$COD_IMO == pdfs[i, ]$COD_IMO.1, ]$dfrst__[1] != "0"){
#plot(car[car$COD_IMO == "MT-5106000-06BC34E3C6C846C688D17C2200D5FC14", ]$geometry, col = NA, border = "steelblue1", lwd = 3)    
#plot(rr, add = T)
#plot(car[car$COD_IMO == "MT-5106000-06BC34E3C6C846C688D17C2200D5FC14", ]$geometry, col = NA, add = T, border = "steelblue1", lwd = 3)    
#plot(pdfs[1, ]$geometry, col = NA, add = T, border = "black", lwd = 3)
#plot(pdfs[2, ]$geometry, col = NA, add = T, border = "red", lwd = 3)    
#plot(pdfs[3, ]$geometry, col = NA, add = T, border = "purple", lwd = 3)    
    p1 <- terra::extract(rr, pdfs[i, ], xy = T)
    defo_inter <- length(p1 %>% filter(layer == 2) %>% .$layer)*0.09
   
    if(defo_inter/as.numeric(pdfs[i, ]$dfrst__)*100 >= 80){
      
      drop_i <- 1
      
    } else {
      
      drop_i <- 0
      
    }
    
    if(defo_inter/as.numeric(car[car$COD_IMO == pdfs[i, ]$COD_IMO.1, ]$dfrst__[1])*100 >= 80){
      
      drop_minus_i <- 1
      
    } else {
      
      drop_minus_i <- 0
      
    }
    
      pdfs[i, ]$drop_i <- drop_i
      pdfs[i, ]$drop_minus_i <- drop_minus_i
      #print(i)
}
      
}

pdfs$case <- ifelse(pdfs$pct_intersect_i < .9, "overlap", "")
pdfs$case <- ifelse(pdfs$pct_intersect_i >= .9, "i contained by -i", pdfs$case)
pdfs$case <- ifelse(pdfs$pct_intersect_minus_i >= .9, "i contains -i", pdfs$case)
pdfs$case <- ifelse(pdfs$pct_intersect_i >= .9 & pdfs$pct_intersect_minus_i >= .9, "overlap", pdfs$case)

pdfs$id <- paste0(pdfs$COD_IMO, pdfs$COD_IMO.1)
pdfs2 <- unique(pdfs$COD_IMO)
pdfs3 <- pdfs$id 

for(i in unique(pdfs$id)){
  pp <- pdfs %>% filter(id == i)
if(!is.na(pp$drop_i) & !is.na(pp$drop_minus_i)){
  if(pp$case == "overlap"){
    
    if(pp$drop_i == 1 & pp$drop_minus_i == 0){
      
#drop i because all its defo was in overlap with -i
      pdfs2 <- pdfs2[pdfs2 != pp$COD_IMO]
      pdfs3 <- pdfs3[pdfs3 != pp$id]
      
    }
    
    if(pp$drop_i == 0 & pp$drop_minus_i == 1){
      
#drop -i because all its defo was in overlap with i
      pdfs2 <- pdfs2[pdfs2 != pp$COD_IMO.1]
      pdfs3 <- pdfs3[pdfs3 != pp$id]
      
    }
    
  }
  if(pp$case == "i contains -i"){
    
    if(pp$drop_i == 1){
      
#drop i because all its defo was in subset -i
      pdfs2 <- pdfs2[pdfs2 != pp$COD_IMO]
      pdfs3 <- pdfs3[pdfs3 != pp$id]
      
    }
    
    if(pp$drop_i == 0 & pp$drop_minus_i == 1){
      
#drop -i because its defo belongs to i
      pdfs2 <- pdfs2[pdfs2 != pp$COD_IMO.1]
      pdfs3 <- pdfs3[pdfs3 != pp$id]
      
    }
    
  }
  if(pp$case == "i contained by -i"){
    
    if(pp$drop_i == 1 & pp$drop_minus_i == 0){
      
      #drop i because all its defo was in subset -i
      pdfs2 <- pdfs2[pdfs2 != pp$COD_IMO]
      pdfs3 <- pdfs3[pdfs3 != pp$id]
      
    }
    
    if(pp$drop_minus_i == 1){
      
      #drop -i because its defo belongs to i
      pdfs2 <- pdfs2[pdfs2 != pp$COD_IMO.1]
      pdfs3 <- pdfs3[pdfs3 != pp$id]
      
    }
    
  }
}
}

#drop shapes (based on i and -i) that should be removed
pdfs <- pdfs %>% filter(COD_IMO %in% unique(pdfs2))

#drop all overlaps that have been resolved
pdfs <- pdfs %>% filter(id %in% pdfs3)
if(length(pdfs$COD_IMO) > 0){
#---#---#---#---#---#---#---#---#---#--#
#---both 0+0 and 1+1 cases remain #----#
#---#---#---#---#---#---#---#---#---#--#
pdfs$k_i <- 1
pdfs$k_minus_i <- 0
pdfs$keep <- NA
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#  plot(car[car$COD_IMO == "MT-5106000-C65E5975CD094AFFBD41EDC0B817FEEF", ]$geometry, col = NA, border = "orange")
#  plot(car[car$COD_IMO == "MT-5106000-90C174F9D2B646A28F2A7CC527D34B99", ]$geometry, col = NA, border = "steelblue1", add = T)
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#---#---#---#---#---#---#---#---#---#--##---#---#---#---#---#---#---#---#---#--#
#---#---#--- randomly assign 0 or 1 to variable keep for 0+0 with overlap---#--#
#---#---#---#---#---#---#---#---#---#--##---#---#---#---#---#---#---#---#---#--#
for(i in 1:length(pdfs$COD_IMO)){
if(!is.na(pdfs[i, ]$drop_i) & !is.na(pdfs[i, ]$drop_minus_i)){
if(pdfs[i, ]$case == "overlap" & pdfs[i, ]$drop_i == 0 & pdfs[i, ]$drop_minus_i == 0){

  pdfs[i, ]$keep <- mapply(function(x, y) sample(seq(x, y), 1), pdfs[i, ]$k_i, pdfs[i, ]$k_minus_i)
  
}
}
}
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#has "overlap with 0+0"
overs <- pdfs %>% filter(!is.na(keep))
#has "overlap with 1+1" and "contains with 0+0"
insiders <- pdfs %>% filter(is.na(keep))
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#remove -i from i (overlap): i is modified (keep = 1) OR
#remove i from -i (overlap): -i is modified (keep = 0)
#update shape file in CAR dataframe
#------------------------------------------------------------------------------#
if(length(overs$COD_IMO) > 0){
overs$run <- 1
for(i in 1:length(overs$COD_IMO)){
if(overs[i, ]$run == 1){  
  if(overs[i, ]$keep == 1){
    #remove -i from i (overlap): i is modified
    as2 <- car[car$COD_IMO == overs[i, ]$COD_IMO.1, ]
    as2 <- st_union(st_combine(as2))
    as1 <- car[car$COD_IMO == overs[i, ]$COD_IMO, ]
    as1 <- st_make_valid(as1)
    as2 <- st_make_valid(as2)
    as0 <- st_erase(as1, as2)
    #plot(as0$geometry)
    #plot(as1$geometry, add = T, border = "green")

if(length(as0$COD_IMO) > 0){
    if(length(overs[overs$id != overs[i, ]$id & overs$COD_IMO == overs[i, ]$COD_IMO.1 & overs$COD_IMO.1 == overs[i, ]$COD_IMO, ]$COD_IMO) > 0){
#means the other pair i- and i for the current i and -i is in the sample; needs to drop that one)      
      
      overs[overs$id != overs[i, ]$id & overs$COD_IMO == overs[i, ]$COD_IMO.1 & overs$COD_IMO.1 == overs[i, ]$COD_IMO, ]$run <- 0
      
    }
    
    car[car$COD_IMO == overs[i, ]$COD_IMO, ] <- as0 
    
  }
}  
  if(overs[i, ]$keep == 0){
    #remove i from -i (overlap): -i is modified
    as2 <- car[car$COD_IMO == overs[i, ]$COD_IMO, ]
    as2 <- st_union(st_combine(as2))
    as1 <- car[car$COD_IMO == overs[i, ]$COD_IMO.1, ]
    as1 <- st_make_valid(as1)
    as2 <- st_make_valid(as2)
    
    as0 <- st_erase(as1, as2)
    if(length(as0$COD_IMO) > 0){       
    if(length(overs[overs$id != overs[i, ]$id & overs$COD_IMO == overs[i, ]$COD_IMO.1 & overs$COD_IMO.1 == overs[i, ]$COD_IMO, ]$COD_IMO) > 0){
      #means the other pair (i- and i for the current i and -i is in the sample; needs to drop that one)      
      
      overs[overs$id != overs[i, ]$id & overs$COD_IMO == overs[i, ]$COD_IMO.1 & overs$COD_IMO.1 == overs[i, ]$COD_IMO, ]$run <- 0
      
    }
    
    car[car$COD_IMO == overs[i, ]$COD_IMO.1, ] <- as0 
    
  }
}
  #print(i)
}
}
}
#------------------------------------------------------------------------------#
#overlap 0+0 and contains 1+1 
#------------------------------------------------------------------------------#
insiders <- insiders %>% filter(!is.na(drop_i) & !is.na(drop_minus_i))
insiders_1 <- insiders %>% filter(case == "overlap") #overlap 0+0 
insiders_2 <- insiders %>% filter(case != "overlap") #contains 1+1
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
if(length(insiders_1$COD_IMO) > 0){
for(i in 1:length(insiders_1$COD_IMO)){
    insiders_1[i, ]$keep <- mapply(function(x, y) sample(seq(x, y), 1), insiders_1[i, ]$k_i, insiders_1[i, ]$k_minus_i)
}
insiders_1$run <- 1
for(i in 1:length(insiders_1$COD_IMO)){
if(insiders_1[i, ]$run == 1){
  
  #keep i, delete -i
  if(insiders_1[i, ]$keep == 1){
  
    if(length(insiders_1[insiders_1$id != insiders_1[i, ]$id & insiders_1$COD_IMO == insiders_1[i, ]$COD_IMO.1 & insiders_1$COD_IMO.1 == insiders_1[i, ]$COD_IMO, ]$COD_IMO) > 0){
      #means the other pair i- and i for the current i and -i is in the sample; needs to drop that one)      
      
      insiders_1[insiders_1$id != insiders_1[i, ]$id & insiders_1$COD_IMO == insiders_1[i, ]$COD_IMO.1 & insiders_1$COD_IMO.1 == insiders_1[i, ]$COD_IMO, ]$run <- 0
      
    }
    
    car <- car %>% filter(COD_IMO != insiders_1[i, ]$COD_IMO.1)  
    
  }
  
  #keep -i, delete i
  if(insiders_1[i, ]$keep == 0){
    
    if(length(insiders_1[insiders_1$id != insiders_1[i, ]$id & insiders_1$COD_IMO == insiders_1[i, ]$COD_IMO.1 & insiders_1$COD_IMO.1 == insiders_1[i, ]$COD_IMO, ]$COD_IMO) > 0){
      #means the other pair (i- and i for the current i and -i is in the sample; needs to drop that one)      
      
      insiders_1[insiders_1$id != insiders_1[i, ]$id & insiders_1$COD_IMO == insiders_1[i, ]$COD_IMO.1 & insiders_1$COD_IMO.1 == insiders_1[i, ]$COD_IMO, ]$run <- 0
      
    }
    
    car <- car %>% filter(COD_IMO != insiders_1[i, ]$COD_IMO)  
    
  }
}
}
}
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
if(length(insiders_2$COD_IMO) > 0){
insiders_2$run <- 1
for(i in 1:length(insiders_2$COD_IMO)){
  insiders_2[i, ]$keep <- mapply(function(x, y) sample(seq(x, y), 1), insiders_2[i, ]$k_i, insiders_2[i, ]$k_minus_i)
}
for(i in 1:length(insiders_2$COD_IMO)){
  if(insiders_2[i, ]$run == 1){  
    if(insiders_2[i, ]$keep == 1){
      #remove -i from i (overlap): i is modified
      as2 <- car[car$COD_IMO == insiders_2[i, ]$COD_IMO.1, ]
      as2 <- st_union(st_combine(as2))
      as1 <- car[car$COD_IMO == insiders_2[i, ]$COD_IMO, ]
      as1 <- st_make_valid(as1)
      as2 <- st_make_valid(as2)
      
      as0 <- st_erase(as1, as2)
if(length(as0$COD_IMO) > 0){
      if(length(insiders_2[insiders_2$id != insiders_2[i, ]$id & insiders_2$COD_IMO == insiders_2[i, ]$COD_IMO.1 & insiders_2$COD_IMO.1 == insiders_2[i, ]$COD_IMO, ]$COD_IMO) > 0){
        #means the other pair (i- and i for the current i and -i is in the sample; needs to drop that one)      
        
        insiders_2[insiders_2$id != insiders_2[i, ]$id & insiders_2$COD_IMO == insiders_2[i, ]$COD_IMO.1 & insiders_2$COD_IMO.1 == insiders_2[i, ]$COD_IMO, ]$run <- 0
        
      }
      
      car[car$COD_IMO == insiders_2[i, ]$COD_IMO, ] <- as0 
      
    }
    }
    if(insiders_2[i, ]$keep == 0){
      #remove i from -i (overlap): -i is modified
      as2 <- car[car$COD_IMO == insiders_2[i, ]$COD_IMO, ]
      as2 <- st_union(st_combine(as2))
      as1 <- car[car$COD_IMO == insiders_2[i, ]$COD_IMO.1, ]
      as1 <- st_make_valid(as1)
      as2 <- st_make_valid(as2)
if(length(as0$COD_IMO) > 0){      
      as0 <- st_erase(as1, as2)
      
      if(length(insiders_2[insiders_2$id != insiders_2[i, ]$id & insiders_2$COD_IMO == insiders_2[i, ]$COD_IMO.1 & insiders_2$COD_IMO.1 == insiders_2[i, ]$COD_IMO, ]$COD_IMO) > 0){
        #means the other pair (i- and i for the current i and -i is in the sample; needs to drop that one)      
        
        insiders_2[insiders_2$id != insiders_2[i, ]$id & insiders_2$COD_IMO == insiders_2[i, ]$COD_IMO.1 & insiders_2$COD_IMO.1 == insiders_2[i, ]$COD_IMO, ]$run <- 0
        
      }
      
      car[car$COD_IMO == insiders_2[i, ]$COD_IMO.1, ] <- as0 
}
    }
    #print(i)
  }
}
}
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#bring together: noConflict, 
car_clean <- car %>% filter(COD_IMO %in% insiders_1$COD_IMO | COD_IMO %in% insiders_2$COD_IMO | COD_IMO %in% overs$COD_IMO) %>% rbind(noConflict)
#plot(car_clean$geometry)
car_clean <- st_make_valid(car_clean)
if(any(st_geometry_type(car_clean) == "GEOMETRYCOLLECTION")){
  
  car_clean_1 <- car_clean[!str_detect(st_geometry_type(car_clean), "GEOMETRYCOLLECTION"), ]
  car_clean_2 <- car_clean[str_detect(st_geometry_type(car_clean), "GEOMETRYCOLLECTION"), ]
  car_clean_2 <- st_collection_extract(car_clean_2)
  car_clean <- rbind(car_clean_1, car_clean_2)
  
}
} else {
  car_clean <- car
}
} else {
  car_clean <- car %>% rbind(noConflict)
}
#plot(car$geometry, lwd = 2)
#plot(car_clean$geometry, col = NA, border = "orange", add = T, lwd = 2)
#----------------------------------##----------------------------------#
st_write(car_clean, paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/active2014_inGleba_cleaned_CAR", str_extract(file, "_([0-9]+)"),".shp"), append = F)
#----------------------------------##----------------------------------#
print(file)
}
}
registerDoParallel(cores = 6)
pdfs <- foreach(c=1:length(file_name)) %dopar% i.run(file_name[c])
#------------------------------------------------------------------------------------#  
#----                                   EXPORT                                   ----#
#------------------------------------------------------------------------------------#  
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "inGleba") & str_detect(file_name, "2014") & str_detect(file_name, "cleaned")]

car2014_all <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", f))
  car2014_all <- rbind(car2014_all, s)
  print(f)
}
#-------------------------------------------------------------------##-------------------------------------------------------------------#
st_write(car2014_all, "/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_all_cleaned.shp", append = F)
car2014_all <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_all_cleaned.shp")
#-------------------------------------------------------------------##-------------------------------------------------------------------#

#-------------------------------------------------------------------##-------------------------------------------------------------------#
#------------------------------------------------------------------------------------##--------------------------------------------------#

#------------------------------------------------------------------------------------##--------------------------------------------------#
#----                                             CAR WITHOUT ALGO APPLIED                                                           ----#
#------------------------------------------------------------------------------------##--------------------------------------------------#

#------------------------------------------------------------------------------------#
#---- DROP CARs WITHOUT OCCUPATION IN 2004 ---#
#------------------------------------------------------------------------------------#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "inGleba2") & str_detect(file_name, "2014") & !str_detect(file_name, "cleaned")]
#fa <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_working/amazon/occupationCAR/", pattern="*.shp", recursive = TRUE)
#fa <- fa[str_detect(fa, "inGleba") & str_detect(fa, "active2004") & str_detect(fa, "cleaned")]
#file_name <- file_name[!str_extract(file_name, "_([0-9]+)") %in% str_extract(fa, "_([0-9]+)")]

i.run <- function(j){
  #for(j in file_name){  
  car <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", j))
  #properties <- ccar_dirty %>% filter(NOM_MUNICI == "Acrelândia")
  r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", ff[1]))
  #---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
  #---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
  p <- car %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup()
  booths_buffer = st_buffer(p, .3)
  #plot(booths_buffer)
  #plot(p$geometry, add = T)
  rr <- crop(r, booths_buffer)
  pp <- data.frame()
  for(k in 1:length(car$COD_IMO)){
    p1 <- terra::extract(rr, car[k, ], xy = T)
    p1 <- c(car$COD_IMO[k], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
    pp <- rbind(pp, p1)
    print(k)
  }
  colnames(pp) <- c("COD_IMO", "deforested_area_hc", "deforestation_rate")
  active2004_CAR <- car %>% filter(COD_IMO %in% unique(pp %>% filter(as.numeric(deforestation_rate) > 10) %>% .$COD_IMO)) %>% left_join(pp %>% dplyr::select(COD_IMO, deforested_area_hc, deforestation_rate), "COD_IMO")
  active2004_CAR <- active2004_CAR[!duplicated(active2004_CAR$COD_IMO), ]
  #----------------------------------##----------------------------------#
  st_write(active2004_CAR %>% dplyr::select(geometry, COD_IMO, deforestation_rate, deforested_area_hc), paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/active2004_inGleba_uncleaned_CAR", str_extract(j, "_([0-9]+)"),".shp"), append = F)
  #----------------------------------##----------------------------------#
}
registerDoParallel(cores = 12)
pdfs <- foreach(c=1:length(file_name)) %dopar% i.run(file_name[c])
#------------------------------------------------------------------------------------#  
#---  DROP 2004 CARs with deforestation above forgiveness threshold ---#
#------------------------------------------------------------------------------------#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "inGleba") & str_detect(file_name, "active2004") & str_detect(file_name, "uncleaned") & !str_detect(file_name, "within")]
#fa <- list.files(path="~/Documents/amazon/occupationCAR/", pattern="*.shp", recursive = TRUE)
#fa <- fa[str_detect(fa, "inGleba") & str_detect(fa, "active2004")]
#file_name <- file_name[!str_extract(file_name, "_([0-9]+)") %in% str_extract(fa, "_([0-9]+)")]
for(file in file_name){
  car <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  car$area <- as.numeric(car %>% st_area())/10000
  car <- car %>% filter(as.numeric(dfrst__) <= 1500 & area <= 1500)
  m <- str_remove(str_extract(file, "_([0-9]+)"), "_")
  #----------------------------------##----------------------------------#
  st_write(car, paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/active2004_inGleba_withinArea_uncleaned_CAR_", m,".shp"), append = F)
  #----------------------------------##----------------------------------#
  print(m)
}
#-------------------------------------------------------------------##-------------------------------------------------------------------#
#-------------------------------------------------------------------##-------------------------------------------------------------------#
#-------------------------------------------------------------------##-------------------------------------------------------------------#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "within") & str_detect(file_name, "2004") & str_detect(file_name, "_uncleaned")]

car2004 <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", f))
  car2004 <- rbind(car2004, s)
  print(f)
}
#-------------------------------------------------------------------##-------------------------------------------------------------------#
st_write(car2004, "/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_eligible_uncleaned.shp", append = F)
car2004_uncleaned <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_eligible_uncleaned.shp")
#-------------------------------------------------------------------##-------------------------------------------------------------------#

#------------------------------------------------------------------------------------##--------------------------------------------------#
#----                                                CAR WITH ALGO APPLIED                                                           ----#
#------------------------------------------------------------------------------------##--------------------------------------------------#

#------------------------------------------------------------------------------------#
#---- DROP CARs WITHOUT OCCUPATION IN 2004 ---#
#------------------------------------------------------------------------------------#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "inGleba") & str_detect(file_name, "2014") & str_detect(file_name, "_cleaned")]
#fa <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
#fa <- fa[str_detect(fa, "inGleba") & str_detect(fa, "active2004") & str_detect(fa, "_cleaned")]
#file_name <- file_name[!str_extract(file_name, "_([0-9]+)") %in% str_extract(fa, "_([0-9]+)")]

i.run <- function(j){
  ff <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", pattern="*.tif", recursive = TRUE)
  ff <- ff[str_detect(ff, "plot_legacy_forest")]
  ff <- ff[str_detect(ff, "2004")]
#for(j in file_name){  
  car <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", j))
if(length(car$COD_IMO) > 0){
  #car <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", "active2014_inGleba_cleaned_CAR_1100015.shp"))
  #properties <- ccar_dirty %>% filter(NOM_MUNICI == "Acrelândia")
  r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", ff[1]))
  #---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
  #---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
  p <- car %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup()
  booths_buffer = st_buffer(p, .3)
  #plot(booths_buffer)
  #plot(p$geometry, add = T)
  rr <- crop(r, booths_buffer)
  pp <- data.frame()
  for(k in 1:length(car$COD_IMO)){
    p1 <- terra::extract(rr, car[k, ], xy = T)
    p1 <- c(car$COD_IMO[k], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
    pp <- rbind(pp, p1)
    print(k)
  }
  colnames(pp) <- c("COD_IMO", "deforested_area_hc", "deforestation_rate")
  active2004_CAR <- car %>% filter(COD_IMO %in% unique(pp %>% filter(as.numeric(deforestation_rate) > 10) %>% .$COD_IMO)) %>% left_join(pp %>% dplyr::select(COD_IMO, deforested_area_hc, deforestation_rate), "COD_IMO")
  active2004_CAR <- active2004_CAR[!duplicated(active2004_CAR$COD_IMO), ]
  #----------------------------------##----------------------------------#
  st_write(active2004_CAR %>% dplyr::select(geometry, COD_IMO, deforestation_rate, deforested_area_hc), paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/active2004_inGleba_cleaned_CAR", str_extract(j, "_([0-9]+)"),".shp"), append = F)
  #----------------------------------##----------------------------------#
}
}
registerDoParallel(cores = 16)
pdfs <- foreach(c=1:length(file_name)) %dopar% i.run(file_name[c])
#------------------------------------------------------------------------------------#  
#---  DROP 2004 CARs with deforestation above forgiveness threshold ---#
#------------------------------------------------------------------------------------#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "inGleba") & str_detect(file_name, "active2004") & str_detect(file_name, "_cleaned") & !str_detect(file_name, "within")]
#fa <- list.files(path="~/Documents/amazon/occupationCAR/", pattern="*.shp", recursive = TRUE)
#fa <- fa[str_detect(fa, "inGleba") & str_detect(fa, "active2004")]
#file_name <- file_name[!str_extract(file_name, "_([0-9]+)") %in% str_extract(fa, "_([0-9]+)")]
for(file in file_name){
  car <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  car$area <- as.numeric(car %>% st_area())/10000
  car <- car %>% filter(as.numeric(dfrst__) <= 1500 & area <= 1500)
  m <- str_remove(str_extract(file, "_([0-9]+)"), "_")
  #----------------------------------##----------------------------------#
  st_write(car, paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/active2004_inGleba_withinArea_cleaned_CAR_", m,".shp"), append = F)
  #----------------------------------##----------------------------------#
  print(m)
}
#-------------------------------------------------------------------##-------------------------------------------------------------------#
#-------------------------------------------------------------------##-------------------------------------------------------------------#
#-------------------------------------------------------------------##-------------------------------------------------------------------#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "within") & str_detect(file_name, "2004") & str_detect(file_name, "_cleaned")]

car2004 <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", f))
  #s$file <- f
  car2004 <- rbind(car2004, s)
  print(f)
}
#-------------------------------------------------------------------##-------------------------------------------------------------------#
st_write(car2004, "/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_eligible_cleaned.shp", append = F)
car2004 <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_eligible_cleaned.shp")
#-------------------------------------------------------------------##-------------------------------------------------------------------#



#------------------------------------------------------------------------------------#
# COMPARE INITIAL CAR SAMPLE (ACTVE IN 2014) TO CLEANED SAMPLE AFTER REZ ALGO -------#
#------------------------------------------------------------------------------------#
clean_car_comp <- data.frame(rbind(
cbind(length(unique(car2014$COD_IMO)), length(unique(car2014_all$COD_IMO)), length(unique(car2004$COD_IMO)), length(unique(car2004_uncleaned$COD_IMO))), 
cbind(as.numeric(sum(car2014 %>% group_by(COD_IMO) %>% st_area()))/10000,
      as.numeric(sum(car2014_all %>% group_by(COD_IMO) %>% st_area()))/10000,
      as.numeric(sum(car2004 %>% group_by(COD_IMO) %>% st_area()))/10000,
      as.numeric(sum(car2004_uncleaned %>% group_by(COD_IMO) %>% st_area()))/10000), 
cbind(sum(as.numeric(car2014$dfrst__), na.rm = T),
      sum(as.numeric(car2014_all$dfrst__), na.rm = T),
      sum(as.numeric(car2004$dfrst__), na.rm = T),
      sum(as.numeric(car2004_uncleaned$dfrst__), na.rm = T)), 
cbind(as.numeric(mean(car2014 %>% group_by(COD_IMO) %>% st_area()))/10000,
      as.numeric(mean(car2014_all %>% group_by(COD_IMO) %>% st_area()))/10000,
      as.numeric(mean(car2004 %>% group_by(COD_IMO) %>% st_area()))/10000,
      as.numeric(mean(car2004_uncleaned %>% group_by(COD_IMO) %>% st_area()))/10000), 
cbind(mean(as.numeric(car2014$dfrstt_), na.rm = T),
      mean(as.numeric(car2014_all$dfrstt_), na.rm = T),
      mean(as.numeric(car2004$dfrstt_), na.rm = T),
      mean(as.numeric(car2004_uncleaned$dfrstt_), na.rm = T)), 
cbind(mean(as.numeric(car2014$dfrst__), na.rm = T), 
      mean(as.numeric(car2014_all$dfrst__), na.rm = T),
      mean(as.numeric(car2004$dfrst__), na.rm = T),
      mean(as.numeric(car2004_uncleaned$dfrst__), na.rm = T))))

clean_car_comp <- as.data.frame(lapply(clean_car_comp, function(x) if(is.numeric(x)) round(x, 1) else x))
colnames(clean_car_comp) <- c("original CAR", "cleaned CAR", "eligible CAR final sample", "eligible CAR uncleaned")
clean_car_comp$labels <- c("number of CARs", "total CAR area", "total deforested area", "average CAR area", "average deforestation rate", "average deforestation area")
clean_car_comp <- clean_car_comp %>% dplyr::select(labels, everything())
#-------------------------------------------------------------------##-------------------------------------------------------------------#
#-------------------------------------------------------------------##-------------------------------------------------------------------#
#-------------------------------------------------------------------##-------------------------------------------------------------------#
car_inelegible <- car2014_all %>% filter(!COD_IMO %in% car2004$COD_IMO)
car_eligible <- car2004

car_inelegible_uncleaned <- car2014_all_uncleaned %>% filter(!COD_IMO %in% car2004_uncleaned$COD_IMO)
car_eligible_uncleaned <- car2004_uncleaned
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
for(year in seq(2005, 2014)){  
  start.time <- Sys.time()
  file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
  f <- file_name[str_detect(file_name, as.character(year))]
  r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", f[1]))
  
  i.run <- function(j){
    p1 <- terra::extract(r, car_eligible[j, ], xy = T)
    write.table(j, "/Users/pedrotremacoldirossi/Documents/placeholder/place2.txt")
    c(car_eligible$COD_IMO[j], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
  }
  registerDoParallel(cores = 25)
  pdfs <- foreach(c=1:length(car_eligible$COD_IMO)) %dopar% i.run(c)
  pdfs <- data.frame(do.call(rbind, pdfs))
  colnames(pdfs) <- c("COD_IMO", "deforested_area_hc", "deforestation_rate")
  #----------------------------------##----------------------------------#  
  write_rds(pdfs, paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/CAR_eligible_defo_", year, ".rds"))
  #----------------------------------##----------------------------------#  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
for(year in seq(2005, 2014)){  
  start.time <- Sys.time()
  file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
  f <- file_name[str_detect(file_name, as.character(year))]
  r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", f[1]))
  
  i.run <- function(j){
    p1 <- terra::extract(r, car_eligible_uncleaned[j, ], xy = T)
    write.table(j, "/Users/pedrotremacoldirossi/Documents/placeholder/place2.txt")
    c(car_eligible_uncleaned$COD_IMO[j], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
  }
  registerDoParallel(cores = 25)
  pdfs <- foreach(c=1:length(car_eligible_uncleaned$COD_IMO)) %dopar% i.run(c)
  pdfs <- data.frame(do.call(rbind, pdfs))
  colnames(pdfs) <- c("COD_IMO", "deforested_area_hc", "deforestation_rate")
  #----------------------------------##----------------------------------#  
  write_rds(pdfs, paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/CAR_eligible_uncleaned_defo_", year, ".rds"))
  #----------------------------------##----------------------------------#  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


#-------------------------------------------------------------------##-------------------------------------------------------------------#
st_write(car_inelegible, "/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_ineligible_cleaned.shp", append = F)
car_inelegible <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_ineligible_cleaned.shp")
#-------------------------------------------------------------------##-------------------------------------------------------------------#

for(year in seq(2005, 2014)){  
  start.time <- Sys.time()
  file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
  f <- file_name[str_detect(file_name, as.character(year))]
  r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", f[1]))
  
  i.run <- function(j){
    p1 <- terra::extract(r, car_inelegible[j, ], xy = T)
    write.table(j, "/Users/pedrotremacoldirossi/Documents/placeholder/place2.txt")
    c(car_inelegible$COD_IMO[j], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
  }
  registerDoParallel(cores = 20)
  pdfs <- foreach(c=1:length(car_inelegible$COD_IMO)) %dopar% i.run(c)
  pdfs <- data.frame(do.call(rbind, pdfs))
  colnames(pdfs) <- c("COD_IMO", "deforested_area_hc", "deforestation_rate")
  #----------------------------------##----------------------------------#  
  write_rds(pdfs, paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/CAR_ineligible_defo_", year, ".rds"))
  #----------------------------------##----------------------------------#  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
for(year in seq(2005, 2014)){  
  start.time <- Sys.time()
  file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
  f <- file_name[str_detect(file_name, as.character(year))]
  r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", f[1]))
  
  i.run <- function(j){
    p1 <- terra::extract(r, car_inelegible_uncleaned[j, ], xy = T)
    write.table(j, "/Users/pedrotremacoldirossi/Documents/placeholder/place2.txt")
    c(car_inelegible_uncleaned$COD_IMO[j], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
  }
  registerDoParallel(cores = 20)
  pdfs <- foreach(c=1:length(car_inelegible_uncleaned$COD_IMO)) %dopar% i.run(c)
  pdfs <- data.frame(do.call(rbind, pdfs))
  colnames(pdfs) <- c("COD_IMO", "deforested_area_hc", "deforestation_rate")
  #----------------------------------##----------------------------------#  
  write_rds(pdfs, paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/CAR_ineligible_uncleaned_defo_", year, ".rds"))
  #----------------------------------##----------------------------------#  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#car_eligiblec <- gSimplify(methods::as(object = car_eligible, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#amazon_biomac <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
#car_inelegiblec <- gSimplify(methods::as(object = car_inelegible, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
#car2014c <- gSimplify(methods::as(object = car2014, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#indigenousc <- gSimplify(methods::as(object = indigenous, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#conservationc <- gSimplify(methods::as(object = conservation, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
bbox_new <- st_bbox(amazon_bioma) # current bounding box
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
bbox_new[3] <- bbox_new[3] + (0 * xrange) # xmax - right
bbox_new[4] <- bbox_new[4] + (0.1 * yrange) # ymax - top
bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
tm_shape(amazon_bioma, bbox = bbox_new) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#02401B", .7)),
                border.col = "grey40",
                labels = c('With Conflict')) +
  tm_shape(car2014) + tm_borders(col = "#02401B", lwd = .1, alpha = 0.2) +
  tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + 
  tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c(0.5, -0.05),
            legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)  
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#alpha("#F1BB7B", .9)
tm_shape(amazon_bioma, bbox = bbox_new) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#F1BB7B", .7), alpha("#02401B", .9)),
                border.col = "grey40",
                labels = c('Eligible', 'Ineligible')) +
  tm_shape(car_eligible_uncleaned) + tm_borders(col = "#F1BB7B", lwd = .1, alpha = 0.2) +
  tm_shape(car_inelegible_uncleaned) + tm_borders(col = "#02401B", lwd = .1) +
  tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + 
  tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c(0.8, 0.05),
            legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T) -> car_plot_1

tmap_save(car_plot_1, filename = "~/Documents/your_map1.png",
          #device = cairo_pdf, 
          dpi = 400,
          width = 6, # The width of the plot in inches
          height = 4, units = "in")
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
tm_shape(amazon_bioma, bbox = bbox_new) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#F1BB7B", .7), alpha("#02401B", .9)),
                border.col = "grey40",
                labels = c('Eligible', 'Ineligible')) +
  tm_shape(car_eligible) + tm_borders(col = "#F1BB7B", lwd = .1, alpha = 0.2) +
  tm_shape(car_inelegible) + tm_borders(col = "#02401B", lwd = .1) +
  tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + 
  tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c(0.8, 0.05),
            legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)  -> car_plot_2

tmap_save(car_plot_2, filename = "~/Documents/your_map2.png",
          #device = cairo_pdf, 
          dpi = 400,
          width = 6, # The width of the plot in inches
          height = 4, units = "in")
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.rds", recursive = F)
file_name <- file_name[str_detect(file_name, "CAR_eligible_defo_")]

for(file in file_name[1]){
  eligible <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  eligible$area <- as.numeric(eligible$deforested_area_hc)/(as.numeric(eligible$deforestation_rate)/100)
  eligible <- eligible %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% group_by(COD_IMO)
  eligible <- eligible %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMO) == 1)
  eligible <- eligible %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  names(eligible)[2:3] <- paste0(names(eligible)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  data <- data %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMO) == 1)
  names(data)[2:3] <- paste0(names(data)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
  data <- data %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  eligible <- dplyr::left_join(eligible, data, "COD_IMO")
  print(file)
}  

for(file in file_name[1]){
  eligible_long <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  eligible_long$area <- as.numeric(eligible_long$deforested_area_hc)/(as.numeric(eligible_long$deforestation_rate)/100)
  eligible_long <- eligible_long %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  eligible_long <- reshape2::melt(eligible_long, id = "COD_IMO")
  eligible_long$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  data$area <- as.numeric(data$deforested_area_hc)/(as.numeric(data$deforestation_rate)/100)
  data <- data %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  data <- reshape2::melt(data, id = "COD_IMO")
  data$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
  eligible_long <- rbind(eligible_long, data)
  print(file)
}  
eligible_long$group <- "eligible"
#---##---##---##---##---##---##---##---##---##---##---##---##
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.rds", recursive = F)
file_name <- file_name[str_detect(file_name, "CAR_ineligible_defo_")]

for(file in file_name[1]){
  inelegible <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  inelegible$area <- as.numeric(inelegible$deforested_area_hc)/(as.numeric(inelegible$deforestation_rate)/100)
  inelegible <- inelegible %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% group_by(COD_IMO)
  inelegible <- inelegible %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMO) == 1) %>% ungroup()
  inelegible <- inelegible %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  names(inelegible)[2:3] <- paste0(names(inelegible)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  data <- data %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMO) == 1) %>% ungroup()
  names(data)[2:3] <- paste0(names(data)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
  data <- data %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  inelegible <- dplyr::left_join(inelegible, data, "COD_IMO")
  print(file)
}  

for(file in file_name[1]){
  inelegible_long <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  inelegible_long$area <- as.numeric(inelegible_long$deforested_area_hc)/(as.numeric(inelegible_long$deforestation_rate)/100)
  inelegible_long <- inelegible_long %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  inelegible_long <- reshape2::melt(inelegible_long, id = "COD_IMO")
  inelegible_long$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  data$area <- as.numeric(data$deforested_area_hc)/(as.numeric(data$deforestation_rate)/100)
  data <- data %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  data <- reshape2::melt(data, id = "COD_IMO")
  data$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
  inelegible_long <- rbind(inelegible_long, data)
  print(file)
}  
inelegible_long$group <- "inelegible"
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.rds", recursive = F)
file_name <- file_name[str_detect(file_name, "CAR_eligible_uncleaned_defo_")]

for(file in file_name[1]){
  eligible_uncleaned <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  eligible_uncleaned$area <- as.numeric(eligible_uncleaned$deforested_area_hc)/(as.numeric(eligible_uncleaned$deforestation_rate)/100)
  eligible_uncleaned <- eligible_uncleaned %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% group_by(COD_IMO)
  eligible_uncleaned <- eligible_uncleaned %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMO) == 1)
  eligible_uncleaned <- eligible_uncleaned %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  names(eligible_uncleaned)[2:3] <- paste0(names(eligible_uncleaned)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  data <- data %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMO) == 1)
  names(data)[2:3] <- paste0(names(data)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
  data <- data %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  eligible_uncleaned <- dplyr::left_join(eligible_uncleaned, data, "COD_IMO")
  print(file)
}  

for(file in file_name[1]){
  eligible_uncleaned_long <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  eligible_uncleaned_long <- eligible_uncleaned_long %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  eligible_uncleaned_long <- reshape2::melt(eligible_uncleaned_long, id = "COD_IMO")
  eligible_uncleaned_long$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  data <- data %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  data <- reshape2::melt(data, id = "COD_IMO")
  data$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
  eligible_uncleaned_long <- rbind(eligible_uncleaned_long, data)
  print(file)
}  
eligible_uncleaned_long$group <- "eligible"
#---##---##---##---##---##---##---##---##---##---##---##---##
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.rds", recursive = F)
file_name <- file_name[str_detect(file_name, "CAR_ineligible_uncleaned_defo_")]

for(file in file_name[1]){
  inelegible_uncleaned <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  inelegible_uncleaned$area <- as.numeric(inelegible_uncleaned$deforested_area_hc)/(as.numeric(inelegible_uncleaned$deforestation_rate)/100)
  inelegible_uncleaned <- inelegible_uncleaned %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% group_by(COD_IMO)
  inelegible_uncleaned <- inelegible_uncleaned %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMO) == 1) %>% ungroup()
  inelegible_uncleaned <- inelegible_uncleaned %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  names(inelegible_uncleaned)[2:3] <- paste0(names(inelegible_uncleaned)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  data <- data %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMO) == 1) %>% ungroup()
  names(data)[2:3] <- paste0(names(data)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
  data <- data %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  inelegible_uncleaned <- dplyr::left_join(inelegible_uncleaned, data, "COD_IMO")
  print(file)
}  

for(file in file_name[1]){
  inelegible_uncleaned_long <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  inelegible_uncleaned_long$area <- as.numeric(inelegible_uncleaned_long$deforested_area_hc)/(as.numeric(inelegible_uncleaned_long$deforestation_rate)/100)
  inelegible_uncleaned_long <- inelegible_uncleaned_long %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  inelegible_uncleaned_long <- reshape2::melt(inelegible_uncleaned_long, id = "COD_IMO")
  inelegible_uncleaned_long$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  data$area <- as.numeric(data$deforested_area_hc)/(as.numeric(data$deforestation_rate)/100)
  data <- data %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  data <- reshape2::melt(data, id = "COD_IMO")
  data$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
  inelegible_uncleaned_long <- rbind(inelegible_uncleaned_long, data)
  print(file)
}  
inelegible_uncleaned_long$group <- "inelegible"
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#~73 million hectares have been deforested in the Amazon biome
#increase of 2 million hectares in ineligible, 0.9 million hectares in the eligible properties
#eligible properties: ~15 million hectares; not eligible: 29 million
inelegible <- inelegible %>% filter(!is.na(area) & area < 100000)

clean_car_comp2 <- data.frame(t(rbind(
  
cbind(mean(as.numeric(eligible$deforestation_rate_2005), na.rm = T),
      mean(as.numeric(eligible$deforestation_rate_2006), na.rm = T),
      mean(as.numeric(eligible$deforestation_rate_2007), na.rm = T),
      mean(as.numeric(eligible$deforestation_rate_2008), na.rm = T),
      mean(as.numeric(eligible$deforestation_rate_2009), na.rm = T),
      mean(as.numeric(eligible$deforestation_rate_2010), na.rm = T),
      mean(as.numeric(eligible$deforestation_rate_2011), na.rm = T),
      mean(as.numeric(eligible$deforestation_rate_2012), na.rm = T),
      mean(as.numeric(eligible$deforestation_rate_2013), na.rm = T),
      mean(as.numeric(eligible$deforestation_rate_2014), na.rm = T)),

cbind(mean(as.numeric(inelegible$deforestation_rate_2005), na.rm = T),
      mean(as.numeric(inelegible$deforestation_rate_2006), na.rm = T),
      mean(as.numeric(inelegible$deforestation_rate_2007), na.rm = T),
      mean(as.numeric(inelegible$deforestation_rate_2008), na.rm = T),
      mean(as.numeric(inelegible$deforestation_rate_2009), na.rm = T),
      mean(as.numeric(inelegible$deforestation_rate_2010), na.rm = T),
      mean(as.numeric(inelegible$deforestation_rate_2011), na.rm = T),
      mean(as.numeric(inelegible$deforestation_rate_2012), na.rm = T),
      mean(as.numeric(inelegible$deforestation_rate_2013), na.rm = T),
      mean(as.numeric(inelegible$deforestation_rate_2014), na.rm = T)),

cbind(sum(as.numeric(eligible$deforested_area_hc_2005), na.rm = T),
      sum(as.numeric(eligible$deforested_area_hc_2006), na.rm = T),
      sum(as.numeric(eligible$deforested_area_hc_2007), na.rm = T),
      sum(as.numeric(eligible$deforested_area_hc_2008), na.rm = T),
      sum(as.numeric(eligible$deforested_area_hc_2009), na.rm = T),
      sum(as.numeric(eligible$deforested_area_hc_2010), na.rm = T),
      sum(as.numeric(eligible$deforested_area_hc_2011), na.rm = T),
      sum(as.numeric(eligible$deforested_area_hc_2012), na.rm = T),
      sum(as.numeric(eligible$deforested_area_hc_2013), na.rm = T),
      sum(as.numeric(eligible$deforested_area_hc_2014), na.rm = T)),
      #sum(as.numeric(eligible$area), na.rm = T)),

cbind(sum(as.numeric(inelegible$deforested_area_hc_2005), na.rm = T),
      sum(as.numeric(inelegible$deforested_area_hc_2006), na.rm = T),
      sum(as.numeric(inelegible$deforested_area_hc_2007), na.rm = T),
      sum(as.numeric(inelegible$deforested_area_hc_2008), na.rm = T),
      sum(as.numeric(inelegible$deforested_area_hc_2009), na.rm = T),
      sum(as.numeric(inelegible$deforested_area_hc_2010), na.rm = T),
      sum(as.numeric(inelegible$deforested_area_hc_2011), na.rm = T),
      sum(as.numeric(inelegible$deforested_area_hc_2012), na.rm = T),
      sum(as.numeric(inelegible$deforested_area_hc_2013), na.rm = T),
      sum(as.numeric(inelegible$deforested_area_hc_2014), na.rm = T)),

cbind(mean(as.numeric(eligible_uncleaned$deforestation_rate_2005), na.rm = T),
      mean(as.numeric(eligible_uncleaned$deforestation_rate_2006), na.rm = T),
      mean(as.numeric(eligible_uncleaned$deforestation_rate_2007), na.rm = T),
      mean(as.numeric(eligible_uncleaned$deforestation_rate_2008), na.rm = T),
      mean(as.numeric(eligible_uncleaned$deforestation_rate_2009), na.rm = T),
      mean(as.numeric(eligible_uncleaned$deforestation_rate_2010), na.rm = T),
      mean(as.numeric(eligible_uncleaned$deforestation_rate_2011), na.rm = T),
      mean(as.numeric(eligible_uncleaned$deforestation_rate_2012), na.rm = T),
      mean(as.numeric(eligible_uncleaned$deforestation_rate_2013), na.rm = T),
      mean(as.numeric(eligible_uncleaned$deforestation_rate_2014), na.rm = T)),

cbind(mean(as.numeric(inelegible_uncleaned$deforestation_rate_2005), na.rm = T),
      mean(as.numeric(inelegible_uncleaned$deforestation_rate_2006), na.rm = T),
      mean(as.numeric(inelegible_uncleaned$deforestation_rate_2007), na.rm = T),
      mean(as.numeric(inelegible_uncleaned$deforestation_rate_2008), na.rm = T),
      mean(as.numeric(inelegible_uncleaned$deforestation_rate_2009), na.rm = T),
      mean(as.numeric(inelegible_uncleaned$deforestation_rate_2010), na.rm = T),
      mean(as.numeric(inelegible_uncleaned$deforestation_rate_2011), na.rm = T),
      mean(as.numeric(inelegible_uncleaned$deforestation_rate_2012), na.rm = T),
      mean(as.numeric(inelegible_uncleaned$deforestation_rate_2013), na.rm = T),
      mean(as.numeric(inelegible_uncleaned$deforestation_rate_2014), na.rm = T)),

cbind(sum(as.numeric(eligible_uncleaned$deforested_area_hc_2005), na.rm = T),
      sum(as.numeric(eligible_uncleaned$deforested_area_hc_2006), na.rm = T),
      sum(as.numeric(eligible_uncleaned$deforested_area_hc_2007), na.rm = T),
      sum(as.numeric(eligible_uncleaned$deforested_area_hc_2008), na.rm = T),
      sum(as.numeric(eligible_uncleaned$deforested_area_hc_2009), na.rm = T),
      sum(as.numeric(eligible_uncleaned$deforested_area_hc_2010), na.rm = T),
      sum(as.numeric(eligible_uncleaned$deforested_area_hc_2011), na.rm = T),
      sum(as.numeric(eligible_uncleaned$deforested_area_hc_2012), na.rm = T),
      sum(as.numeric(eligible_uncleaned$deforested_area_hc_2013), na.rm = T),
      sum(as.numeric(eligible_uncleaned$deforested_area_hc_2014), na.rm = T)),

cbind(sum(as.numeric(inelegible_uncleaned$deforested_area_hc_2005), na.rm = T),
      sum(as.numeric(inelegible_uncleaned$deforested_area_hc_2006), na.rm = T),
      sum(as.numeric(inelegible_uncleaned$deforested_area_hc_2007), na.rm = T),
      sum(as.numeric(inelegible_uncleaned$deforested_area_hc_2008), na.rm = T),
      sum(as.numeric(inelegible_uncleaned$deforested_area_hc_2009), na.rm = T),
      sum(as.numeric(inelegible_uncleaned$deforested_area_hc_2010), na.rm = T),
      sum(as.numeric(inelegible_uncleaned$deforested_area_hc_2011), na.rm = T),
      sum(as.numeric(inelegible_uncleaned$deforested_area_hc_2012), na.rm = T),
      sum(as.numeric(inelegible_uncleaned$deforested_area_hc_2013), na.rm = T),
      sum(as.numeric(inelegible_uncleaned$deforested_area_hc_2014), na.rm = T))
)))

clean_car_comp2 <- as.data.frame(lapply(clean_car_comp2, function(x) if(is.numeric(x)) round(x, 1) else x))
colnames(clean_car_comp2) <- c("defo_rate_eligible", "defo_rate_ineligible", "defo_area_eligible", "defo_area_ineligible",
                               "defo_rate_eligible_uncleaned", "defo_rate_ineligible_uncleaned", "defo_area_eligible_uncleaned", "defo_area_ineligible_uncleaned")
clean_car_comp2$year <- as.character(seq(2005, 2014))
clean_car_comp2 <- clean_car_comp2 %>% dplyr::select(year, everything())
#(sum(as.numeric(eligible$deforested_area_hc_2014), na.rm = T) - sum(as.numeric(eligible$deforested_area_hc_2005), na.rm = T))/1000000
#(sum(as.numeric(inelegible$deforested_area_hc_2014), na.rm = T)-sum(as.numeric(inelegible$deforested_area_hc_2005), na.rm = T))/1000000
#sum(as.numeric(inelegible$deforested_area_hc_2005), na.rm = T)
#sum(as.numeric(inelegible$deforested_area_hc_2014), na.rm = T)
#sum(as.numeric(inelegible$area), na.rm = T)
#-------------------------------------------------------------------##-------------------------------------------------------------------#
#-------------------------------------------------------------------##-------------------------------------------------------------------#
#ccar_clean_inReservas$registro_car <- ccar_clean_inReservas$COD_IMO
ccar_clean <- ccar_clean_updated
ccar_clean$registro_car <- ccar_clean$COD_IMO
ccar_clean_inReservas <- ccar_clean
ccar_clean_inReservas <- ccar_clean_inReservas %>% left_join(temas_ambientais_update, "registro_car")

#---STEP 1: DROP CAR WITH NO USE EVER [up until 2019]                ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ change to 2017
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
f <- file_name[str_detect(file_name, "2019")]
r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", f[1]))

i.run <- function(j){
  properties <- ccar_clean_inReservas %>% filter(codigo_ibge == j)
  #properties <- ccar_dirty %>% filter(NOM_MUNICI == "Acrelândia")
  
  #---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
  #---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
  p <- properties %>% summarise(geometry = sf::st_union(geometry)) %>% ungroup()
  booths_buffer = st_buffer(p, .3)
  rr <- crop(r, booths_buffer)
  pp <- data.frame()
  for(k in 1:length(properties$COD_IMO)){
    p1 <- terra::extract(rr, properties[k, ], xy = T)
    p1 <- c(properties$COD_IMO[k], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
    pp <- rbind(pp, p1)
    #print(k)
  }
  colnames(pp) <- c("COD_IMO", "deforested_area_hc", "deforestation_rate")
  active2014_CAR <- properties %>% filter(COD_IMO %in% unique(pp %>% filter(as.numeric(deforestation_rate) > 10) %>% .$COD_IMO)) %>% left_join(pp %>% dplyr::select(COD_IMO, deforested_area_hc, deforestation_rate), "COD_IMO")
  #----------------------------------##----------------------------------#
  st_write(active2014_CAR %>% dplyr::select(geometry, COD_IMO, NUM_ARE, codigo_ibge, deforestation_rate, deforested_area_hc), paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/active2014_inReserva_CAR_", j,".shp"), append = F)
  #----------------------------------##----------------------------------#
}
registerDoParallel(cores = 20)
pdfs <- foreach(c=unique(ccar_clean_inReservas$codigo_ibge)) %dopar% i.run(c)
#-------------------------------------------------------------------##-------------------------------------------------------------------#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "inReserva") & str_detect(file_name, "2014") & !str_detect(file_name, "cleaned")]
#fa <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
#fa <- fa[str_detect(fa, "inReserva") & str_detect(fa, "2014") & str_detect(fa, "cleaned")]
#file_name <- file_name[!str_extract(file_name, "_([0-9]+)") %in% str_extract(fa, "_([0-9]+)")]

#-------------------------------------------------------------------##-------------------------------------------------------------------#
#-------- FOR PROPERTIES IN REZ/CONSER THERE'S NO REAL DEFINITION OF PROPERTY LINES -----------------------------------------------------#
#-------- SO THE CLEANING ONLY REMOVES OVERLAPS TO AVOID DOUBLE COUNTING ----------------------------------------------------------------#
#-------------------------------------------------------------------##-------------------------------------------------------------------#
i.run <- function(file){
#for(file in file_name){
  car <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  car <- car[!duplicated(car$COD_IMO), ]
  
  if(length(car$COD_IMO) > 0){  
    i.run <- function(i){
      car %>% filter(row_number() == i) %>% 
        st_intersection(car %>% filter(row_number() != i)) %>%
        mutate(intersect_area = st_area(.))
    }
    registerDoParallel(cores = 10)
    pdfs <- foreach(c=1:length(car$COD_IMO)) %dopar% i.run(c)
    #pdfs <- do.call(rbind, pdfs[1:1000])
    pdfs <- bind_rows(pdfs)
    pdfs$intersect_area <- as.numeric(pdfs$intersect_area)/10000
    pdfs$pct_intersect_i <- pdfs$intersect_area/pdfs$NUM_ARE
    pdfs$pct_intersect_minus_i <- pdfs$intersect_area/pdfs$NUM_ARE.1
    pdfs <- pdfs %>% group_by(COD_IMO) %>% mutate(max_pct_intersect = max(pct_intersect_i))
    pdfs <- pdfs %>% filter(pct_intersect_i > .1) #keeps all >10% overlaps for i 
    #------------------------------------------------------------------------------------#
    #---pdfs contain CARs that intersect at least partially at least with another CAR ---#
    
    #--- (1) CARs with no conflict, leave alone ---#  
    noConflict <- car %>% filter(!COD_IMO %in% unique(pdfs$COD_IMO)) #if i doesn't have any >10% overlap
    #------------------------------------------------------------------------------#  
    pdfs <- pdfs %>% filter(!COD_IMO %in% noConflict$COD_IMO)
    
if(length(pdfs$COD_IMO) > 0){    
        #------------------------------------------------------------------------------#
        #------------------------------------------------------------------------------#
    
for(i in 1:length(pdfs$COD_IMO)){
      if(pdfs[i, ]$NUM_ARE >= pdfs[i, ]$NUM_ARE.1){
        
        as2 <- car[car$COD_IMO == pdfs[i, ]$COD_IMO.1, ]
        as2 <- st_union(st_combine(as2))
        as1 <- car[car$COD_IMO == pdfs[i, ]$COD_IMO, ]
        as1 <- st_make_valid(as1)
        as2 <- st_make_valid(as2)
        if(inherits(try(st_erase(as1, as2), silent = T), 'try-error') == FALSE){
        #erase smaller from larger
        as0 <- st_erase(as1, as2)
        if(length(as0$COD_IMO) > 0){
          car[car$COD_IMO == pdfs[i, ]$COD_IMO, ] <- as0 
        } 
        }
      } else {
        as2 <- car[car$COD_IMO == pdfs[i, ]$COD_IMO, ]
        as2 <- st_union(st_combine(as2))
        as1 <- car[car$COD_IMO == pdfs[i, ]$COD_IMO.1, ]
        as1 <- st_make_valid(as1)
        as2 <- st_make_valid(as2)
if(inherits(try(st_erase(as1, as2), silent = T), 'try-error') == FALSE){
        
    #if(st_geometry_type(as2) != "GEOMETRYCOLLECTION"){
        #erase larger from smaller
        as0 <- st_erase(as1, as2)
        if(length(as0$COD_IMO) > 0){
          car[car$COD_IMO == pdfs[i, ]$COD_IMO.1, ] <- as0 
        }
}
    #}
      }  
  print(i)
}
  
  car_final <- rbind(noConflict, car %>% filter(COD_IMO %in% unique(pdfs$COD_IMO)))
  
} else {
  
  car_final <- car
}
    
    car_final <- st_make_valid(car_final)
    if(any(st_geometry_type(car_final) == "GEOMETRYCOLLECTION")){
      
      car_final_1 <- car_final[!str_detect(st_geometry_type(car_final), "GEOMETRYCOLLECTION"), ]
      car_final_2 <- car_final[str_detect(st_geometry_type(car_final), "GEOMETRYCOLLECTION"), ]
      car_final_2 <- st_collection_extract(car_final_2)
      car_final <- rbind(car_final_1, car_final_2)
      
    }
    
    if(any(st_geometry_type(car_final) == "LINESTRING")){
      
      car_final <- car_final[!str_detect(st_geometry_type(car_final), "LINESTRING"), ]
      
    }
    
    if(any(st_geometry_type(car_final) == "MULTILINESTRING")){
      
      car_final <- car_final[!str_detect(st_geometry_type(car_final), "MULTILINESTRING"), ]
    }
    
    
  
    #----------------------------------##----------------------------------#
    st_write(car_final, paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/active2014_inReserva_cleaned_CAR", str_extract(file, "_([0-9]+)"),".shp"), append = F)
    #----------------------------------##----------------------------------#
   # print(file)
  }
}
registerDoParallel(cores = 10)
pdfs <- foreach(c=1:length(file_name)) %dopar% i.run(file_name[c])
#-------------------------------------------------------------------##-------------------------------------------------------------------#
#-------------------------------------------------------------------##-------------------------------------------------------------------#
#-------------------------------------------------------------------##-------------------------------------------------------------------#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.shp", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "inReserva_") & str_detect(file_name, "2014") & str_detect(file_name, "cleaned")]

control <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", f))
  control <- rbind(control, s)
  print(which(f == file_name))
}
control <- control[!duplicated(control$COD_IMO), ]
#-------------------------------------------------------------------##-------------------------------------------------------------------#
st_write(control, "/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/control_final.shp", append = F)
control_final <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/control_final.shp")
#-------------------------------------------------------------------##-------------------------------------------------------------------#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##--
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##--
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##--
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##--
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##--
#controlc <- gSimplify(methods::as(object = control, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#glebas1c <- gSimplify(methods::as(object = glebas1, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
#control_areasc <- gSimplify(methods::as(object = control_areas, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
# tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
#   tm_add_legend(c("fill"), col = c(alpha("#81A88D", .6), alpha("#5B1A18", .6), "white"),
#                 border.col = "grey40",
#                 labels = c('Treated areas', "Control areas", "Other areas")) +
#   #tm_shape(glec) + tm_fill(col = "#81A88D", alpha = 0.2, border.col = "#81A88D") +
#   tm_shape(control) + tm_fill(col = "#5B1A18", alpha = 0.6, border.col = "#5B1A18") +
#   tm_shape(glebas1) + tm_fill(col = "#81A88D", alpha = .6, border.col = "#81A88D") +
#   tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + 
#   tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
#   tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
#   tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
#   tm_layout(frame = FALSE, legend.title.size = 1,
#             legend.text.size = 1, legend.position = c("right","bottom"),
#             legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T) #-> p2
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
car2004$legend_group <- "Eligible"
car_inelegible$legend_group <- "Ineligible"
control_final$legend_group <- "Never Eligible"

car2004_proj <- st_transform(car2004, 5880)
car_ineligible_proj <- st_transform(car_inelegible, 5880)
control_final_proj <- st_transform(control_final, 5880)

car2004_plot <-st_simplify(car2004_proj, dTolerance = 100, preserveTopology = TRUE)
car_ineligible_plot <-st_simplify(car_ineligible_proj, dTolerance = 100, preserveTopology = TRUE)
control_final_plot <-st_simplify(control_final_proj, dTolerance = 100, preserveTopology = TRUE)

car2004_plot <- st_transform(car2004_plot, 4326)
car_ineligible_plot <- st_transform(car_ineligible_plot, 4326)
control_final_plot <- st_transform(control_final_plot, 4326)

ggplot() +
  # Control areas as polygons with colored borders
  geom_sf(data = car2004_plot, aes(color = legend_group), fill = alpha("#F99100", 0.6), linewidth = 0) +
  geom_sf(data = car_ineligible_plot %>% filter(NUM_ARE < 100000), aes(color = legend_group), fill = alpha("#02401B", 0.7), linewidth = 0) +
  geom_sf(data = control_final_plot %>% filter(NUM_ARE < 100000), aes(color = legend_group), fill = alpha("#5B1A18", 0.8), linewidth = 0) +
  # State borders
  geom_sf(data = all_states_plot, fill = NA, color = "gray40", linewidth = 0.3) +
  # Amazon biome boundary
  geom_sf(data = amazon_bioma_plot, fill = NA, color = "black", linewidth = 0.4) +
  # Shared color legend
  scale_color_manual(name = "", 
                     values = c("Eligible" = "#F99100", "Ineligible" = "#02401B", "Never Eligible" = "#5B1A18")) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = c(0.9, 0.13),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.text = element_text(size = 16, family = "Times New Roman"),
    legend.key.size = unit(2, "lines"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank()
  ) -> plot
plot

ggsave(plot, filename = "~/Documents/g2_final.png", dpi = 300, width = 8, height = 5.5, units = "in")

tm_shape(amazon_bioma, bbox = bbox_new) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#02401B", .7), alpha("#F1BB7B", .9), alpha("#5B1A18", .9)),
                border.col = "grey40",
                labels = c('Ineligible', 'Eligible', "Never Eligible")) +
  tm_shape(car2004) + tm_borders(col = "#F1BB7B", lwd = .1, alpha = 0.9) +
  tm_shape(car_inelegible) + tm_borders(col = "#02401B", lwd = .2, alpha = 0.9) +
  tm_shape(control_final %>% filter(NUM_ARE < 100000)) + tm_fill(col = "#5B1A18", alpha = 0.9, border.col = "#5B1A18") +
  tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + 
  tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c(0.8, 0),
            legend.text.fontfamily="Times New Roman", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)  
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
for(year in seq(2005, 2014)){  
  start.time <- Sys.time()
  file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
  f <- file_name[str_detect(file_name, as.character(year))]
  r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", f[1]))
  
  i.run <- function(j){
    p1 <- terra::extract(r, control[j, ], xy = T)
    write.table(j, "/Users/pedrotremacoldirossi/Documents/placeholder/place2.txt")
    c(control$COD_IMO[j], length(p1 %>% filter(layer == 2) %>% .$layer)*0.09, length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100)
  }
  registerDoParallel(cores = 25)
  pdfs <- foreach(c=1:length(control$COD_IMO)) %dopar% i.run(c)
  pdfs <- data.frame(do.call(rbind, pdfs))
  colnames(pdfs) <- c("COD_IMO", "deforested_area_hc", "deforestation_rate")
  #----------------------------------##----------------------------------#  
  write_rds(pdfs, paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/CAR_control_defo_", year, ".rds"))
  #----------------------------------##----------------------------------#  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", pattern="*.rds", recursive = F)
file_name <- file_name[str_detect(file_name, "CAR_control_defo_")]

for(file in file_name[1]){
  never_eligible <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  never_eligible$area <- as.numeric(never_eligible$deforested_area_hc)/(as.numeric(never_eligible$deforestation_rate)/100)
  never_eligible <- never_eligible %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% group_by(COD_IMO)
  never_eligible <- never_eligible %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc, area) %>% filter(row_number(COD_IMO) == 1)
  never_eligible <- never_eligible %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  names(never_eligible)[2:3] <- paste0(names(never_eligible)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  data$area <- as.numeric(data$deforested_area_hc)/(as.numeric(data$deforestation_rate)/100)
  data <- data %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc, area) %>% filter(row_number(COD_IMO) == 1)
  names(data)[2:4] <- paste0(names(data)[2:4], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
  data <- data %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  never_eligible <- dplyr::left_join(never_eligible, data, "COD_IMO")
  print(file)
}  

for(file in file_name[1]){
  never_eligible_long <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  never_eligible_long$area <- as.numeric(never_eligible_long$deforested_area_hc)/(as.numeric(never_eligible_long$deforestation_rate)/100)
  never_eligible_long <- never_eligible_long %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  never_eligible_long <- reshape2::melt(never_eligible_long, id = "COD_IMO")
  never_eligible_long$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
}  
for(file in file_name[2:10]){
  data <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/", file))
  data$area <- as.numeric(data$deforested_area_hc)/(as.numeric(data$deforestation_rate)/100)
  data <- data %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  data <- reshape2::melt(data, id = "COD_IMO")
  data$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
  never_eligible_long <- rbind(never_eligible_long, data)
  print(file)
}  
never_eligible_long$group <- "never eligible"
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#never_eligible <- never_eligible %>% filter(!is.na(area) & area < 50000)

clean_car_comp2 <- cbind(clean_car_comp2, t(rbind(
  cbind(mean(as.numeric(never_eligible$deforestation_rate_2005), na.rm = T),
        mean(as.numeric(never_eligible$deforestation_rate_2006), na.rm = T),
        mean(as.numeric(never_eligible$deforestation_rate_2007), na.rm = T),
        mean(as.numeric(never_eligible$deforestation_rate_2008), na.rm = T),
        mean(as.numeric(never_eligible$deforestation_rate_2009), na.rm = T),
        mean(as.numeric(never_eligible$deforestation_rate_2010), na.rm = T),
        mean(as.numeric(never_eligible$deforestation_rate_2011), na.rm = T),
        mean(as.numeric(never_eligible$deforestation_rate_2012), na.rm = T),
        mean(as.numeric(never_eligible$deforestation_rate_2013), na.rm = T),
        mean(as.numeric(never_eligible$deforestation_rate_2014), na.rm = T)),

  cbind(sum(as.numeric(never_eligible$deforested_area_hc_2005), na.rm = T),
        sum(as.numeric(never_eligible$deforested_area_hc_2006), na.rm = T),
        sum(as.numeric(never_eligible$deforested_area_hc_2007), na.rm = T),
        sum(as.numeric(never_eligible$deforested_area_hc_2008), na.rm = T),
        sum(as.numeric(never_eligible$deforested_area_hc_2009), na.rm = T),
        sum(as.numeric(never_eligible$deforested_area_hc_2010), na.rm = T),
        sum(as.numeric(never_eligible$deforested_area_hc_2011), na.rm = T),
        sum(as.numeric(never_eligible$deforested_area_hc_2012), na.rm = T),
        sum(as.numeric(never_eligible$deforested_area_hc_2013), na.rm = T),
        sum(as.numeric(never_eligible$deforested_area_hc_2014), na.rm = T)))))
colnames(clean_car_comp2)[10:11] <- c("defo_rate_never_eligible", "defo_area_never_eligible")
#(mean(as.numeric(inelegible$deforestation_rate_2008), na.rm = T)/mean(as.numeric(inelegible$deforestation_rate_2005), na.rm = T)-1)/4
#(mean(as.numeric(inelegible$deforestation_rate_2014), na.rm = T)/mean(as.numeric(inelegible$deforestation_rate_2009), na.rm = T)-1)/6
#mean(as.numeric(never_eligible$deforestation_rate_2008), na.rm = T)/mean(as.numeric(never_eligible$deforestation_rate_2005), na.rm = T)-1
#mean(as.numeric(never_eligible$deforestation_rate_2014), na.rm = T)/mean(as.numeric(never_eligible$deforestation_rate_2008), na.rm = T)-1
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#combined <- rbind(eligible_uncleaned_long, inelegible_uncleaned_long, never_eligible_long)
combined <- rbind(eligible_long, inelegible_long, never_eligible_long)
combined$value <- as.numeric(combined$value)
combined$value <- ifelse(is.nan(combined$value), NA, combined$value)
#combined <- combined %>% group_by(variable) %>% mutate(value_w = DescTools::Winsorize(value, probs=c(0.01, 0.99), na.rm = T))
#combined$value <- ifelse(combined$value == 0, NA, combined$value)
#combined_mean <- combined %>% group_by(year, group, variable) %>% mutate(v_mean = mean(value, na.rm = T), v_w_mean = mean(value_w, na.rm = T)) %>% filter(row_number(year) == 1) %>% arrange(group, variable, year)
temas_ambientais_update$COD_IMO <- temas_ambientais_update$registro_car
temas_ambientais_update <- temas_ambientais_update %>% group_by(COD_IMO) %>% filter(row_number(COD_IMO) == 1)
combined <- combined %>% left_join(temas_ambientais_update %>% dplyr::select(uf, codigo_ibge, area_do_imovel, area_rural_consolidada, cancelled, COD_IMO), "COD_IMO")

combined <- combined %>% group_by(variable, COD_IMO) %>% mutate(defo_rate_value_max = max(value, na.rm = T), defo_rate_value_min = min(value, na.rm = T))
combined$defo_rate_value_max <- ifelse(combined$variable == "deforestation_rate" & as.numeric(combined$year)<2009, combined$defo_rate_value_max, NA)
combined$defo_rate_value_min <- ifelse(combined$variable == "deforestation_rate" & as.numeric(combined$year)<2009, combined$defo_rate_value_min, NA)

combined <- combined %>% group_by(variable, COD_IMO) %>% mutate(defo_rate_value_max_after = max(value, na.rm = T), defo_rate_value_min_after = min(value, na.rm = T))
combined$defo_rate_value_max_after <- ifelse(combined$variable == "deforestation_rate" & as.numeric(combined$year)>2008, combined$defo_rate_value_max_after, NA)
combined$defo_rate_value_min_after <- ifelse(combined$variable == "deforestation_rate" & as.numeric(combined$year)>2008, combined$defo_rate_value_min_after, NA)



combined$defo_area_2005 <- ifelse(combined$variable == "deforested_area_hc" & as.numeric(combined$year)==2005, combined$value, NA)
combined$defo_area_2008 <- ifelse(combined$variable == "deforested_area_hc" & as.numeric(combined$year)==2008, combined$value, NA)

combined <- combined %>% ungroup() %>% group_by(COD_IMO) %>% mutate(defo_rate_value_max = max(defo_rate_value_max, na.rm = T), 
                                                                    defo_rate_value_min = min(defo_rate_value_min, na.rm = T),
                                                                    defo_rate_value_max_after = max(defo_rate_value_max_after, na.rm = T), 
                                                                    defo_rate_value_min_after = min(defo_rate_value_min_after, na.rm = T),
                                                                    defo_area_2005 = mean(defo_area_2005, na.rm = T),
                                                                    defo_area_2008 = mean(defo_area_2008, na.rm = T))
combined$area_final <- ifelse(combined$variable == "area", combined$value, NA)
combined <- combined %>% group_by(COD_IMO) %>% mutate(area_final = mean(area_final, na.rm = T))
#length(unique(combined %>% filter(area_do_imovel >= 50) %>% .$COD_IMO))/length(unique(combined %>% .$COD_IMO))
#sum(combined %>% filter(area_do_imovel >= 50) %>% group_by(COD_IMO) %>% filter(row_number(area_do_imovel) == 1) %>% .$area_do_imovel, na.rm = T)/sum(combined %>% group_by(COD_IMO) %>% filter(row_number(area_do_imovel) == 1) %>% .$area_do_imovel, na.rm = T)
combined$when_occupied <- ifelse(combined$value >= 10 & combined$variable == "deforestation_rate", as.numeric(combined$year), 9090)
combined <- combined %>% ungroup() %>% group_by(COD_IMO) %>% mutate(when_occupied = min(when_occupied, na.rm = T))
combined$defo_rate_2009 <- ifelse(combined$year == "2009" & combined$variable == "deforested_area_hc", combined$value, NA)
combined <- combined %>% group_by(COD_IMO) %>% mutate(defo_rate_2009 = mean(defo_rate_2009, na.rm = T))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
combined$enters_after_policy <- ifelse(combined$defo_rate_value_min < 10, 1, 0)
combined$enters_after_policy <- ifelse(combined$defo_rate_value_max_after >= 10 & combined$enters_after_policy == 1, 1, 0)

first_defor_year <- combined %>%
  filter(variable == "deforestation_rate") %>%
  group_by(COD_IMO) %>%
  summarise(first_year = min(year[value > 10], na.rm = TRUE),
            .groups = "drop") %>%
  mutate(first_year = ifelse(is.infinite(first_year), NA, first_year))
combined <- combined %>% left_join(first_defor_year, "COD_IMO")

combined$enters_after_policy <- ifelse(combined$first_year > 2008, 1, 0)
combined$eligible_second_policy <- ifelse(combined$first_year > 2008 & combined$first_year < 2012 & combined$area_do_imovel < 2500, 1, 0)
combined$ineligible_entry <- ifelse(combined$first_year > 2005, 1, 0)

combined$ineligible_entry2 <- ifelse(combined$first_year > 2005 & combined$first_year < 2012 & combined$area_do_imovel < 2500, 1, 0)


subs <- combined %>% filter(group != "eligible")
subs <- subs %>% filter(group == "inelegible") %>% filter(year == "2011") %>% filter(variable == "deforested_area_hc")
mean(subs$ineligible_entry2, na.rm = T)
mean(subs$ineligible_entry, na.rm = T)
mean(subs$eligible_second_policy, na.rm = T)

#subs$variable <- as.character(as.factor(subs$variable))
plot <- data.frame()

for(k in seq(10, 5000, 10)){

subs$dummy_reach_1500 <- ifelse(subs$variable == "deforested_area_hc" & subs$area_do_imovel <= k, 1, 0)
subs <- subs %>% group_by(COD_IMO) %>% mutate(dummy_reach_1500 = max(dummy_reach_1500, na.rm = T))
subs <- subs %>% filter(area_do_imovel < 100000)

a <- cbind(
k, 
mean(subs %>% filter(group == "inelegible") %>% filter(enters_after_policy == 0) %>% filter(variable == "deforested_area_hc") %>% filter(year == "2014") %>% .$dummy_reach_1500)*100,
mean(subs %>% filter(group == "inelegible") %>% filter(enters_after_policy == 1) %>% filter(variable == "deforested_area_hc") %>% filter(year == "2014") %>% .$dummy_reach_1500)*100,

mean(subs %>% filter(group == "never eligible") %>% filter(enters_after_policy == 0) %>% filter(variable == "deforested_area_hc") %>% filter(year == "2014") %>% .$dummy_reach_1500)*100,
mean(subs %>% filter(group == "never eligible") %>% filter(enters_after_policy == 1) %>% filter(variable == "deforested_area_hc") %>% filter(year == "2014") %>% .$dummy_reach_1500)*100,

length(subs %>% filter(group == "inelegible") %>% filter(enters_after_policy == 0) %>% filter(variable == "deforested_area_hc") %>% filter(year == "2014") %>% filter(area_do_imovel <= k) %>% .$dummy_reach_1500),
length(subs %>% filter(group == "inelegible") %>% filter(enters_after_policy == 1) %>% filter(variable == "deforested_area_hc") %>% filter(year == "2014") %>% filter(area_do_imovel <= k) %>% .$dummy_reach_1500),

length(subs %>% filter(group == "never eligible") %>% filter(enters_after_policy == 0) %>% filter(variable == "deforested_area_hc") %>% filter(year == "2014") %>% filter(area_do_imovel <= k) %>% .$dummy_reach_1500),
length(subs %>% filter(group == "never eligible") %>% filter(enters_after_policy == 1) %>% filter(variable == "deforested_area_hc") %>% filter(year == "2014") %>% filter(area_do_imovel <= k) %>% .$dummy_reach_1500)
)
plot <- rbind(plot, a)
print(k)
}

plot$ineligible <- plot$V3 - plot$V2
plot$control <- plot$V5 - plot$V4

plot(plot$ineligible, type = "l")
lines(plot$control, col = "red")
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
combined <- combined %>% group_by(variable, COD_IMO) %>% mutate(defo_rate_value_max_after2 = max(value, na.rm = T), defo_rate_value_min_after2 = min(value, na.rm = T))
combined$defo_rate_value_max_after2 <- ifelse(combined$variable == "deforestation_rate" & as.numeric(combined$year)>2011, combined$defo_rate_value_max_after2, NA)
combined$defo_rate_value_min_after2 <- ifelse(combined$variable == "deforestation_rate" & as.numeric(combined$year)>2011, combined$defo_rate_value_min_after2, NA)
combined <- combined %>% ungroup() %>% group_by(COD_IMO) %>% mutate(defo_rate_value_max_after2 = max(defo_rate_value_max_after2, na.rm = T), 
                                                                    defo_rate_value_min_after2 = min(defo_rate_value_min_after2, na.rm = T))
combined$enters_after_2011 <- ifelse(combined$defo_rate_value_min < 10, 1, 0)
combined$enters_after_2011 <- ifelse(combined$defo_rate_value_max_after2 >= 10 & combined$enters_after_2011 == 1, 1, 0)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
combined$dummy_reach_1500 <- ifelse(combined$variable == "deforested_area_hc" & combined$value >= 1500, 1, 0)
combined <- combined %>% group_by(COD_IMO) %>% mutate(dummy_reach_1500 = max(dummy_reach_1500, na.rm = T))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#combined <- combined %>% ungroup() %>% group_by(COD_IMO) %>% mutate(dummy_reach_1500 = pmin(dummy_reach_1500, na.rm = T))
#combined$reach_1500 <- ifelse(combined$reach_1500 != 9090, combined$reach_1500 - combined$when_occupied, NA)
#combined$reach_1500 <- ifelse(combined$reach_1500<0, NA, combined$reach_1500)
# combined %>% filter(group == "eligible") %>% filter(area_do_imovel >= 50 & area_do_imovel <= 1500 & value_max < 90) %>% 
#       filter(variable == "deforested_area_hc") %>% group_by(year) %>% mutate(v = sum(value, na.rm = T)) %>% filter(row_number(v) == 1) %>% 
#       dplyr::select(year, v)
# 
# combined %>% filter(group == "inelegible") %>% filter(area_do_imovel >= 50 & value_max < 90) %>%
#   filter(variable == "deforested_area_hc") %>% filter(value >= 5) %>%
#   group_by(year) %>% mutate(v = sum(value, na.rm = T)) %>% filter(row_number(v) == 1) %>% 
#   dplyr::select(year, v)
#sum value if variable == 1 & sa == . & area_do_imovel >= 50 & value_max < 90 & group == "inelegible" & y < 2009



combined %>% group_by(when_occupied) %>% filter(group ==  "never eligible") %>% filter(variable == "deforested_area_hc") %>% mutate(m = mean(reach_1500_dummy, na.rm = T)*100) %>%
  filter(row_number(m) == 1) %>% dplyr::select(when_occupied, m) %>% arrange(when_occupied) %>% filter(when_occupied != 9090)

combined %>% group_by(when_occupied) %>% filter(group ==  "inelegible") %>% filter(variable == "deforested_area_hc") %>% mutate(m = mean(reach_1500_dummy, na.rm = T)*100) %>%
  filter(row_number(m) == 1) %>% dplyr::select(when_occupied, m) %>% arrange(when_occupied) %>% filter(when_occupied != 9090)

combined %>% group_by(when_occupied) %>% filter(group ==  "eligible") %>% filter(variable == "deforested_area_hc") %>% mutate(m = mean(reach_1500_dummy, na.rm = T)*100) %>%
  filter(row_number(m) == 1) %>% dplyr::select(when_occupied, m) %>% arrange(when_occupied) %>% filter(when_occupied != 9090)


combined %>% group_by(when_occupied) %>% filter(group ==  "inelegible") %>% filter(variable == "deforested_area_hc") %>%
  mutate(r = mean(reach_1500, na.rm = T)) %>% filter(row_number(r) == 1) %>%
  filter(when_occupied != 9090) %>%
  dplyr::select(when_occupied, r) %>% arrange(when_occupied)

combined %>% group_by(COD_IMO, year) %>% mutate(defo_imovel_year = mean())

#ifelse((combined$year == "2009" & combined$variable == "deforestation_rate" & combined$value > 10 & combined$group == "inelegible") | combined$group != "inelegible", 1, 0)
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---###---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---###---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---###---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---###---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##
write_dta(combined,"~/Documents/did.dta")

#-------------------------------------------------------------------#
#-------------------------------------------------------------------#
#-------------------------------------------------------------------#
#-------------------------------------------------------------------#
# PRICE REGS
#-------------------------------------------------------------------#
library(rccmisc)
legal <- readRDS("~/Dropbox (Personal)/amazon_project/code/patricio_preach_tomas_work/data/output/parcels_NB_Lavoura/legal_parcels_all/legal_parcel_nb_lavoura_wide.rds")
eligible <- readRDS("~/Dropbox (Personal)/amazon_project/code/patricio_preach_tomas_work/data/output/parcels_NB_Lavoura/eligible_parcels_all/eligible_parcel_nb_lavoura_wide.rds")
ineligible <- readRDS("~/Dropbox (Personal)/amazon_project/code/patricio_preach_tomas_work/data/output/parcels_NB_Lavoura/ineligible_parcels_all/ineligible_parcel_nb_lavoura_wide.rds")
##PRE
less_1500_merge <- read_dta("Documents/less_1500_merge.dta")
#ineligible$less_1500 <- ifelse(ineligible$NUM_ARE < 1500, 1, 0)
ineligible <- left_join(ineligible, less_1500_merge, "COD_IMO")
ineligible$less_1500 <- ifelse(is.na(ineligible$less_1500), 0, ineligible$less_1500)
ineligible <- left_join(ineligible, subs %>% dplyr::select(COD_IMO, eligible_second_policy), "COD_IMO")
less_1500 <- ineligible %>% group_by(region_id) %>% mutate(less_1500 = mean(less_1500, na.rm = T), eligible_second_policy = mean(eligible_second_policy, na.rm = T)) %>% filter(row_number(less_1500) == 1) %>% dplyr::select(region_id, less_1500, eligible_second_policy)
less_1500$eligible_second_policy <- ifelse(is.nan(less_1500$eligible_second_policy), NA, less_1500$eligible_second_policy)
write_dta(less_1500, "~/Documents/less_1500.dta")
########


#p1 <- legal %>% filter(year < 2010)  %>% group_by(region_id) %>% mutate(legal_area = sum(area, na.rm = T)) %>% add_count() %>% filter(row_number(legal_area) == 1) %>% dplyr::select(legal_area, n)
p1 <- legal  %>% group_by(region_id) %>% mutate(legal_area = sum(area, na.rm = T)) %>% add_count() %>% filter(row_number(legal_area) == 1) %>% dplyr::select(legal_area, n)
p2 <- eligible %>% group_by(region_id) %>% mutate(eligible_area = sum(area, na.rm = T)) %>% add_count() %>% filter(row_number(eligible_area) == 1) %>% dplyr::select(eligible_area, n)
p3 <- ineligible %>% group_by(region_id) %>% mutate(ineligible_area = sum(NUM_ARE, na.rm = T)) %>% add_count() %>% filter(row_number(ineligible_area) == 1) %>% dplyr::select(ineligible_area, n)

colnames(p1)[3] <- "legal_count"
colnames(p2)[3] <- "eligible_count"
colnames(p3)[3] <- "ineligible_count"

shares <- left_join(left_join(p1, p2, "region_id"), p3, "region_id")
shares[is.na(shares)] <- 0

shares$eligible_share <- shares$eligible_area/psum(shares$legal_area, shares$eligible_area, shares$ineligible_area, na.rm = T)
shares$ineligible_share <- shares$ineligible_area/psum(shares$legal_area, shares$eligible_area, shares$ineligible_area, na.rm = T)

shares$eligible_share2 <- shares$eligible_count/(shares$eligible_count+shares$ineligible_count+shares$legal_count)
shares$ineligible_share2 <- shares$ineligible_count/(shares$eligible_count+shares$ineligible_count+shares$legal_count)

e <- eligible %>% group_by(region_id) %>% filter(row_number(region_id) == 1) 
i <- ineligible %>% group_by(region_id) %>% filter(row_number(region_id) == 1) 
l <- legal %>% group_by(region_id) %>% filter(row_number(region_id) == 1) 

shares <- left_join(shares, l[, c(2, 1686:1701)], "region_id")

#region_id's to input with FNP: 120, 127, 126, 77, 128, 122, 131
#problematic region_id's: 124 [x], 119 [?], 77
shares[shares$region_id == 77, ]$price_2002_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2002"][1]
shares[shares$region_id == 77, ]$price_2003_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2003"][1]
shares[shares$region_id == 77, ]$price_2004_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2004"][1]
shares[shares$region_id == 77, ]$price_2005_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2005"][1]
shares[shares$region_id == 77, ]$price_2006_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2006"][1]
shares[shares$region_id == 77, ]$price_2007_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2007"][1]
shares[shares$region_id == 77, ]$price_2008_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2008"][1]
shares[shares$region_id == 77, ]$price_2009_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2009"][1]
shares[shares$region_id == 77, ]$price_2010_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2010"][1]
shares[shares$region_id == 77, ]$price_2011_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2011"][1]
shares[shares$region_id == 77, ]$price_2012_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2012"][1]
shares[shares$region_id == 77, ]$price_2013_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2013"][1]
shares[shares$region_id == 77, ]$price_2014_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2014"][1]
shares[shares$region_id == 77, ]$price_2015_lavoura <- legal[legal$region_id == 77, "preco_cerrado_proximo_a_belem_brasilia_a_leste_2015"][1]

shares[shares$region_id == 119, ]$price_2002_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2002"][1]
shares[shares$region_id == 119, ]$price_2003_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2003"][1]
shares[shares$region_id == 119, ]$price_2004_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2004"][1]
shares[shares$region_id == 119, ]$price_2005_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2005"][1]
shares[shares$region_id == 119, ]$price_2006_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2006"][1]
shares[shares$region_id == 119, ]$price_2007_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2007"][1]
shares[shares$region_id == 119, ]$price_2008_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2008"][1]
shares[shares$region_id == 119, ]$price_2009_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2009"][1]
shares[shares$region_id == 119, ]$price_2010_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2010"][1]
shares[shares$region_id == 119, ]$price_2011_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2011"][1]
shares[shares$region_id == 119, ]$price_2012_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2012"][1]
shares[shares$region_id == 119, ]$price_2013_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2013"][1]
shares[shares$region_id == 119, ]$price_2014_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2014"][1]
shares[shares$region_id == 119, ]$price_2015_lavoura <- legal[legal$region_id == 119, "preco_pastagem_nativa_em_varzea_monte_alegre_alenquer_oriximina_2015"][1]

shares[shares$region_id == 120, ]$price_2002_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2002"][1]
shares[shares$region_id == 120, ]$price_2003_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2003"][1]
shares[shares$region_id == 120, ]$price_2004_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2004"][1]
shares[shares$region_id == 120, ]$price_2005_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2005"][1]
shares[shares$region_id == 120, ]$price_2006_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2006"][1]
shares[shares$region_id == 120, ]$price_2007_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2007"][1]
shares[shares$region_id == 120, ]$price_2008_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2008"][1]
shares[shares$region_id == 120, ]$price_2009_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2009"][1]
shares[shares$region_id == 120, ]$price_2010_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2010"][1]
shares[shares$region_id == 120, ]$price_2011_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2011"][1]
shares[shares$region_id == 120, ]$price_2012_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2012"][1]
shares[shares$region_id == 120, ]$price_2013_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2013"][1]
shares[shares$region_id == 120, ]$price_2014_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2014"][1]
shares[shares$region_id == 120, ]$price_2015_lavoura <- legal[legal$region_id == 120, "preco_pastagem_formada_de_alto_suporte_redencao_2015"][1]

shares[shares$region_id == 121, ]$price_2002_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2002"][1]
shares[shares$region_id == 121, ]$price_2003_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2003"][1]
shares[shares$region_id == 121, ]$price_2004_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2004"][1]
shares[shares$region_id == 121, ]$price_2005_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2005"][1]
shares[shares$region_id == 121, ]$price_2006_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2006"][1]
shares[shares$region_id == 121, ]$price_2007_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2007"][1]
shares[shares$region_id == 121, ]$price_2008_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2008"][1]
shares[shares$region_id == 121, ]$price_2009_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2009"][1]
shares[shares$region_id == 121, ]$price_2010_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2010"][1]
shares[shares$region_id == 121, ]$price_2011_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2011"][1]
shares[shares$region_id == 121, ]$price_2012_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2012"][1]
shares[shares$region_id == 121, ]$price_2013_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2013"][1]
shares[shares$region_id == 121, ]$price_2014_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2014"][1]
shares[shares$region_id == 121, ]$price_2015_lavoura <- legal[legal$region_id == 121, "preco_mata_de_facil_acesso_2015"][1]

shares[shares$region_id == 122, ]$price_2002_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2002"][1]
shares[shares$region_id == 122, ]$price_2003_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2003"][1]
shares[shares$region_id == 122, ]$price_2004_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2004"][1]
shares[shares$region_id == 122, ]$price_2005_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2005"][1]
shares[shares$region_id == 122, ]$price_2006_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2006"][1]
shares[shares$region_id == 122, ]$price_2007_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2007"][1]
shares[shares$region_id == 122, ]$price_2008_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2008"][1]
shares[shares$region_id == 122, ]$price_2009_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2009"][1]
shares[shares$region_id == 122, ]$price_2010_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2010"][1]
shares[shares$region_id == 122, ]$price_2011_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2011"][1]
shares[shares$region_id == 122, ]$price_2012_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2012"][1]
shares[shares$region_id == 122, ]$price_2013_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2013"][1]
shares[shares$region_id == 122, ]$price_2014_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2014"][1]
shares[shares$region_id == 122, ]$price_2015_lavoura <- legal[legal$region_id == 122, "preco_pastagem_nativa_com_acesso_marajo_2015"][1]

shares[shares$region_id == 124, ]$price_2002_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2002"][1]
shares[shares$region_id == 124, ]$price_2003_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2003"][1]
shares[shares$region_id == 124, ]$price_2004_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2004"][1]
shares[shares$region_id == 124, ]$price_2005_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2005"][1]
shares[shares$region_id == 124, ]$price_2006_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2006"][1]
shares[shares$region_id == 124, ]$price_2007_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2007"][1]
shares[shares$region_id == 124, ]$price_2008_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2008"][1]
shares[shares$region_id == 124, ]$price_2009_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2009"][1]
shares[shares$region_id == 124, ]$price_2010_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2010"][1]
shares[shares$region_id == 124, ]$price_2011_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2011"][1]
shares[shares$region_id == 124, ]$price_2012_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2012"][1]
shares[shares$region_id == 124, ]$price_2013_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2013"][1]
shares[shares$region_id == 124, ]$price_2014_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2014"][1]
shares[shares$region_id == 124, ]$price_2015_lavoura <- legal[legal$region_id == 124, "preco_pastagem_formada_vilhena_pimenta_bueno_2015"][1]
#region id 118 not found
shares[shares$region_id == 125, ]$price_2002_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2002"][1]
shares[shares$region_id == 125, ]$price_2003_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2003"][1]
shares[shares$region_id == 125, ]$price_2004_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2004"][1]
shares[shares$region_id == 125, ]$price_2005_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2005"][1]
shares[shares$region_id == 125, ]$price_2006_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2006"][1]
shares[shares$region_id == 125, ]$price_2007_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2007"][1]
shares[shares$region_id == 125, ]$price_2008_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2008"][1]
shares[shares$region_id == 125, ]$price_2009_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2009"][1]
shares[shares$region_id == 125, ]$price_2010_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2010"][1]
shares[shares$region_id == 125, ]$price_2011_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2011"][1]
shares[shares$region_id == 125, ]$price_2012_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2012"][1]
shares[shares$region_id == 125, ]$price_2013_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2013"][1]
shares[shares$region_id == 125, ]$price_2014_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2014"][1]
shares[shares$region_id == 125, ]$price_2015_lavoura <- legal[legal$region_id == 125, "preco_pastagem_formada_porto_velho_2015"][1]

shares[shares$region_id == 126, ]$price_2002_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2002"][1]
shares[shares$region_id == 126, ]$price_2003_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2003"][1]
shares[shares$region_id == 126, ]$price_2004_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2004"][1]
shares[shares$region_id == 126, ]$price_2005_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2005"][1]
shares[shares$region_id == 126, ]$price_2006_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2006"][1]
shares[shares$region_id == 126, ]$price_2007_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2007"][1]
shares[shares$region_id == 126, ]$price_2008_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2008"][1]
shares[shares$region_id == 126, ]$price_2009_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2009"][1]
shares[shares$region_id == 126, ]$price_2010_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2010"][1]
shares[shares$region_id == 126, ]$price_2011_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2011"][1]
shares[shares$region_id == 126, ]$price_2012_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2012"][1]
shares[shares$region_id == 126, ]$price_2013_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2013"][1]
shares[shares$region_id == 126, ]$price_2014_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2014"][1]
shares[shares$region_id == 126, ]$price_2015_lavoura <- legal[legal$region_id == 126, "preco_pastagem_formada_no_asfalto_rio_branco_2015"][1]

shares[shares$region_id == 127, ]$price_2002_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2002"][1]
shares[shares$region_id == 127, ]$price_2003_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2003"][1]
shares[shares$region_id == 127, ]$price_2004_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2004"][1]
shares[shares$region_id == 127, ]$price_2005_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2005"][1]
shares[shares$region_id == 127, ]$price_2006_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2006"][1]
shares[shares$region_id == 127, ]$price_2007_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2007"][1]
shares[shares$region_id == 127, ]$price_2008_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2008"][1]
shares[shares$region_id == 127, ]$price_2009_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2009"][1]
shares[shares$region_id == 127, ]$price_2010_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2010"][1]
shares[shares$region_id == 127, ]$price_2011_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2011"][1]
shares[shares$region_id == 127, ]$price_2012_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2012"][1]
shares[shares$region_id == 127, ]$price_2013_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2013"][1]
shares[shares$region_id == 127, ]$price_2014_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2014"][1]
shares[shares$region_id == 127, ]$price_2015_lavoura <- legal[legal$region_id == 127, "preco_pastagem_formada_de_facil_acesso_cruzeiro_do_sul_tarauaca_2015"][1]
#region id 115 not found
shares[shares$region_id == 128, ]$price_2002_lavoura <- legal[legal$region_id == 128, "preco_mata_2002"][1]
shares[shares$region_id == 128, ]$price_2003_lavoura <- legal[legal$region_id == 128, "preco_mata_2003"][1]
shares[shares$region_id == 128, ]$price_2004_lavoura <- legal[legal$region_id == 128, "preco_mata_2004"][1]
shares[shares$region_id == 128, ]$price_2005_lavoura <- legal[legal$region_id == 128, "preco_mata_2005"][1]
shares[shares$region_id == 128, ]$price_2006_lavoura <- legal[legal$region_id == 128, "preco_mata_2006"][1]
shares[shares$region_id == 128, ]$price_2007_lavoura <- legal[legal$region_id == 128, "preco_mata_2007"][1]
shares[shares$region_id == 128, ]$price_2008_lavoura <- legal[legal$region_id == 128, "preco_mata_2008"][1]
shares[shares$region_id == 128, ]$price_2009_lavoura <- legal[legal$region_id == 128, "preco_mata_2009"][1]
shares[shares$region_id == 128, ]$price_2010_lavoura <- legal[legal$region_id == 128, "preco_mata_2010"][1]
shares[shares$region_id == 128, ]$price_2011_lavoura <- legal[legal$region_id == 128, "preco_mata_2011"][1]
shares[shares$region_id == 128, ]$price_2012_lavoura <- legal[legal$region_id == 128, "preco_mata_2012"][1]
shares[shares$region_id == 128, ]$price_2013_lavoura <- legal[legal$region_id == 128, "preco_mata_2013"][1]
shares[shares$region_id == 128, ]$price_2014_lavoura <- legal[legal$region_id == 128, "preco_mata_2014"][1]
shares[shares$region_id == 128, ]$price_2015_lavoura <- legal[legal$region_id == 128, "preco_mata_2015"][1]

shares[shares$region_id == 129, ]$price_2002_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2002"][1]
shares[shares$region_id == 129, ]$price_2003_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2003"][1]
shares[shares$region_id == 129, ]$price_2004_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2004"][1]
shares[shares$region_id == 129, ]$price_2005_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2005"][1]
shares[shares$region_id == 129, ]$price_2006_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2006"][1]
shares[shares$region_id == 129, ]$price_2007_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2007"][1]
shares[shares$region_id == 129, ]$price_2008_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2008"][1]
shares[shares$region_id == 129, ]$price_2009_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2009"][1]
shares[shares$region_id == 129, ]$price_2010_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2010"][1]
shares[shares$region_id == 129, ]$price_2011_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2011"][1]
shares[shares$region_id == 129, ]$price_2012_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2012"][1]
shares[shares$region_id == 129, ]$price_2013_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2013"][1]
shares[shares$region_id == 129, ]$price_2014_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2014"][1]
shares[shares$region_id == 129, ]$price_2015_lavoura <- legal[legal$region_id == 129, "preco_terra_agricola_de_baixa_produtividade_em_varzea_2015"][1]

shares[shares$region_id == 130, ]$price_2002_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2002"][1]
shares[shares$region_id == 130, ]$price_2003_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2003"][1]
shares[shares$region_id == 130, ]$price_2004_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2004"][1]
shares[shares$region_id == 130, ]$price_2005_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2005"][1]
shares[shares$region_id == 130, ]$price_2006_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2006"][1]
shares[shares$region_id == 130, ]$price_2007_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2007"][1]
shares[shares$region_id == 130, ]$price_2008_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2008"][1]
shares[shares$region_id == 130, ]$price_2009_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2009"][1]
shares[shares$region_id == 130, ]$price_2010_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2010"][1]
shares[shares$region_id == 130, ]$price_2011_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2011"][1]
shares[shares$region_id == 130, ]$price_2012_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2012"][1]
shares[shares$region_id == 130, ]$price_2013_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2013"][1]
shares[shares$region_id == 130, ]$price_2014_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2014"][1]
shares[shares$region_id == 130, ]$price_2015_lavoura <- legal[legal$region_id == 130, "preco_pastagem_nativa_em_varzea_2015"][1]

shares[shares$region_id == 131, ]$price_2002_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2002"][1]
shares[shares$region_id == 131, ]$price_2003_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2003"][1]
shares[shares$region_id == 131, ]$price_2004_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2004"][1]
shares[shares$region_id == 131, ]$price_2005_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2005"][1]
shares[shares$region_id == 131, ]$price_2006_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2006"][1]
shares[shares$region_id == 131, ]$price_2007_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2007"][1]
shares[shares$region_id == 131, ]$price_2008_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2008"][1]
shares[shares$region_id == 131, ]$price_2009_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2009"][1]
shares[shares$region_id == 131, ]$price_2010_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2010"][1]
shares[shares$region_id == 131, ]$price_2011_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2011"][1]
shares[shares$region_id == 131, ]$price_2012_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2012"][1]
shares[shares$region_id == 131, ]$price_2013_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2013"][1]
shares[shares$region_id == 131, ]$price_2014_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2014"][1]
shares[shares$region_id == 131, ]$price_2015_lavoura  <- legal[legal$region_id == 131, "preco_pastagem_formada_de_alto_suporte_2015"][1]
# for(k in 1:length(shares$region_id)){
#   if(is.na(shares$price_2002_lavoura[1])){
#     legal[185, !is.na(legal[185, ])]
#   }
# }
#sum(legal$area)/sum(shares$legal_area)
#length(unique(legal$cod_imovel))
#length(unique(legal_car$cod_imovel))
#sum(legal_car$area)
shares %>% dplyr::select(-legal_area, -eligible_area, -ineligible_area)

shares$total_area <- shares$legal_area + shares$eligible_area + shares$ineligible_area
shares$total_count <- shares$eligible_count+shares$ineligible_count+shares$legal_count

prices_reg <- shares %>% dplyr::select(-legal_area, -eligible_area, -ineligible_area) %>%
  pivot_longer(
    cols = starts_with("price_"),
    names_to = "year",
    values_to = "price_lavoura"
  ) %>%
  mutate(
    year = as.integer(gsub("price_(\\d{4})_lavoura", "\\1", year))
  )

prices_reg <- left_join(prices_reg, yearly_average_price_region, c("region_id", "year"))
prices_reg$price_north <- ifelse(is.na(prices_reg$price_north), prices_reg$price_lavoura, prices_reg$price_north)
prices_reg <- left_join(prices_reg, output %>% select(year, region_id, share_eligible_turnover, share_ineligible_turnover), c("region_id", "year"))

write_dta(prices_reg, "~/Documents/prices_reg.dta")
#

write.csv(shares, "~/Documents/shares5.csv")






a <- legal %>% group_by(region_id) %>% filter(row_number(region_id) == 1) %>% dplyr::select(region_id, region_name) 


















sum(ifelse(combined$year == "2005" & combined$group == "inelegible" & combined$variable == "deforestation_rate" & combined$value > 10, 1, 0), na.rm = T)



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
#st_write(car2004, "~/Documents/car_eligible_cleaned.shp")
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
car2004 <- read_sf("~/Documents/car_eligible_cleaned.shp")
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
 





#ccar_clean_inReservas
#st_write(ccar_clean_inReservas %>% dplyr::select(geometry, COD_IMOVEL, NUM_AREA), "~/Documents/ccar_clean_inReservas.shp")
#ccar_clean_inReservas <- read_sf("~/Documents/ccar_clean_inReservas.shp")
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---filter CARs in glebas that i) only appeared after 2004 or ii) had >1,500 ha deforested
CAR_notEligible <- ccar_dirty %>% filter(!COD_IMOVEL %in% car2004$COD_IMO) 
#st_write(CAR_notEligible %>% dplyr::select(geometry, COD_IMOVEL, NUM_AREA), "~/Documents/CAR_notEligible.shp", append = F)
CAR_notEligible <- read_sf("~/Documents/CAR_notEligible.shp")
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
car2004c <- gSimplify(methods::as(object = car2004, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
car2004_allc <- gSimplify(methods::as(object = car2004_all, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
CAR_notEligiblec <- gSimplify(methods::as(object = CAR_notEligible, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
amazon_biomac <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)

# for(k in 1:length(glebas$id)){
#   is <- st_difference(indigenous[1, ], glebas[k, ])
#   
#   for(j in 2:length(indigenous$id)){
#     
#     is <- st_difference(indigenous[j, ], glebas[k, ])
#     
#   }
# }
# out <- indigenous[1, ]
# for(k in 2:length(glebas$id)){
# sindigenous <- ms_erase(indigenous[1, ], glebas[k, ])
# out <- rbind(out, sindigenous)
# print(k)
# }
# indigenous <- ms_erase(indigenous, glebas)
# conservation <- st_difference(conservation, glebas)
glebasc <- gSimplify(methods::as(object = glebas, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
indigenousc <- gSimplify(methods::as(object = indigenous, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
conservationc <- gSimplify(methods::as(object = conservation, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)

tm_shape(amazon_biomac) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#5B1A18", .8), alpha("#02401B", .5), alpha("#F1BB7B", .9)),
                border.col = "grey40",
                labels = c('Now-ineligible', "Eligible", "Never-eligible")) +
  tm_shape(car2004c) + tm_borders(col = "#02401B", lwd = .1, alpha = 0.5) +
  tm_shape(car2004_allc) + tm_borders(col = "#5B1A18", lwd = .1, alpha = 0.8) +
  #tm_shape(ccar_clean_inReservasc) + tm_borders(col = "#F1BB7B", lwd = .1, alpha = 0.9) +
  #tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + 
  #tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  #tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
  #tm_shape(amazon_biomac) + tm_borders(col = "black", lwd = 1) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T) #-> p1


tm_shape(amazon_biomac) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#5B1A18", 1), alpha("#81A88D", .5)),
                border.col = "grey40",
                labels = c('Treated areas', "Control areas")) +
  tm_shape(indigenousc) + tm_fill(col = "#81A88D", alpha = 0.2, border.col = "#81A88D") +
  tm_shape(conservationc) + tm_fill(col = "#81A88D", alpha = 0.2, border.col = "#81A88D") +
  tm_shape(glebasc) + tm_fill(col = "#5B1A18", alpha = 1, border.col = "#5B1A18") +
  tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + 
  tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(states3) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(amazon_biomac) + tm_borders(col = "black", lwd = 1) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T) #-> p2

ggsave(p1, filename = paste0("/Users/pedrotremacoldirossi/Documents/", "p1", ".pdf"), device = cairo_pdf, dpi = 500,
       width = 8, # The width of the plot in inches
       height = 5, units = "in")
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
file_name <- file_name[!str_detect(file_name, "2004")]
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

plot(control %>% filter(!COD_IMOVEL %in% drop_control) %>% group_by(year) %>% filter(variable == "deforestation_rate") %>% mutate(m = mean(as.numeric(value), na.rm = T)) %>% filter(row_number(m) == 1) %>% .$m, t = "l")

#---export did 1----#
c <- control %>% filter(!COD_IMOVEL %in% drop_control)
c$treatment <- 0

e <- eligible
e$treatment <- 1

did1 <- rbind(c, e)  
did1$after <- ifelse(as.numeric(did1$year) >= 2009, 1, 0)
did1$treated <- did1$treatment*did1$after
did1$value <- as.numeric(did1$value)

write_dta(did1, "~/Documents/did1_new.dta")

s <- spillover %>% filter(!COD_IMOVEL %in% drop_spillover)
s$treatment <- 1

did2 <- rbind(c, s)  
did2$after <- ifelse(as.numeric(did2$year) >= 2009, 1, 0)
did2$treated <- did2$treatment*did2$after
did2$value <- as.numeric(did2$value)

write_dta(did2, "~/Documents/did2_new.dta")

did1 <- read_dta("~/Documents/did1_new.dta")
did2 <- read_dta("~/Documents/did2_new.dta")

temas_ambientais_update$COD_IMOVEL <- temas_ambientais_update$registro_car

did1 <- did1 %>% left_join(temas_ambientais_update %>% dplyr::select(uf, codigo_ibge, area_do_imovel, area_rural_consolidada, COD_IMOVEL), "COD_IMOVEL")
did2 <- did2 %>% left_join(temas_ambientais_update %>% dplyr::select(uf, codigo_ibge, area_do_imovel, area_rural_consolidada, COD_IMOVEL), "COD_IMOVEL")

did1$value <- ifelse(did1$value == "NaN", NA, did1$value)
did1 <- did1 %>% group_by(variable, year) %>% mutate(value_w = winsorize(value, c(0.01, 0.99), na.rm = T))
write_dta(did1, "~/Documents/did1_new.dta")

did2$value <- ifelse(did2$value == "NaN", NA, did2$value)
did2 <- did2 %>% group_by(variable, year) %>% mutate(value_w = winsorize(value, c(0.01, 0.99), na.rm = T))
write_dta(did2, "~/Documents/did2_new.dta")


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
s <- sigef %>% filter(NOME == "Acrel?ndia") %>% filter(status == "CERTIFICADA")
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
file_name <- file_name[str_detect(file_name, "2004")]
r <- rast(paste0("/Volumes/ElementsMain/transitions_combined/", file_name[1]))

r <- crop(r, states %>% filter(SIGLA == "RO"))
ro <- states %>% filter(SIGLA == "RO")
r <- as.factor(r)

#car1 <- car %>% filter(COD_IMO %in% c("RO-1100098-25AD2D26D8F64F02A0305AE852451741", "RO-1100098-7723066A1C0E49EE9A587A11D3996E15"))
#car1 <- car[1:100, ]
car1 <- car %>% filter(COD_IMO %in% c("RO-1100098-25AD2D26D8F64F02A0305AE852451741", "RO-1100098-7723066A1C0E49EE9A587A11D3996E15", "RO-1100098-0059D938F76A41EBA57C0AECBB28E0C6", "RO-1100098-C07DB49294124450A75AA1CF8021AC48"))
car1 <- car %>% filter(COD_IMO %in% c("RO-1100098-1F1B681BF4FC449AAC60F9DE8D379E02", "RO-1100098-0560FDDE177447B0A0F84F23E14C21A4"))





#car1 <- ccar_dirty %>% filter(COD_IMOVEL %in% a$COD_IMO)
plot(car1$geometry)
#plot(car[1, ]$geometry, col = "steelblue1", add = T)
rr <- crop(r, car1)
rr1 <- crop(rr, car1)

p1 <- terra::extract(rr1, car1, xy = T)
unique(p1$layer)
length(p1$layer)
length(p1 %>% filter(layer == 2) %>% .$layer)*0.09

length(p1 %>% filter(layer == 2) %>% .$layer)/length(p1 %>% filter(layer != 0) %>% .$layer)*100

#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----
#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----
#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----
ggplot() +
  tidyterra::geom_spatraster(data = rr1, aes(fill = layer), maxcell = 105000) +
  tidyterra::scale_fill_whitebox_d(
    palette = "bl_yl_rd", na.translate = T) + theme_minimal() + labs(title = "Blue contains White") + 
  scale_fill_manual(values = colors, labels = labels, na.translate = F, na.value = "white") + 
  #tidyterra::geom_spatvector(data = amazon_bioma, fill = NA, col = "black") + 
  #tidyterra::geom_spatvector(data = ro, fill = NA, col = "white") + 
  tidyterra::geom_spatvector(data = car1, fill = NA, col = c("steelblue1"), lwd = 1) + 
  #tidyterra::geom_spatvector(data = car1, fill = NA, col = c("white", "steelblue1", "brown", "white"), lwd = 1) + 
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=10,  family = "Palatino"), text = element_text(family = "Palatino"),
        legend.title = element_blank(), legend.box="horizontal",  legend.position="bottom",
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.grid.major = element_blank(), 
        rect = element_blank()) ->> b

#blue bigger than brown, so blue contains brown. two white polygons intersection is accidental, so should just net areas.

b
ggsave(b, filename = paste0("/Users/pedrotremacoldirossi/Documents/", "i_contains_minus_i", ".pdf"), device = cairo_pdf, dpi = 2000,
       width = 5, # The width of the plot in inches
       height = 8, units = "in")

key <- "AIzaSyD6uPjP2nQCGW3hxvS23Ter6RFyKK9XRgk"
register_google(key = key)

car1 <- as(car1, "Spatial")
car2 <- fortify(car1)

lisbon_satellite <- get_map(c(p1$x[1], p1$y[1]), maptype="satellite", source="google", api_key = key, zoom = 14)
#car2 <- spTransform(car1$geometry, CRS("+proj=longlat +datum=WGS84"))


ggmap(lisbon_satellite) + 
  geom_polygon(data = car2, aes(x = long, y = lat, group = group), col = c("steelblue1"), fill = NA) + 
  theme(axis.text.x = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=10,  family = "Palatino"), text = element_text(family = "Palatino"),
        legend.title = element_blank(), legend.box="horizontal",  legend.position="bottom",
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.grid.major = element_blank(), 
        rect = element_blank())


#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----
#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----
#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----


car %>% filter(COD_IMOVEL %in% temas_ambientais_update$registro_car)


