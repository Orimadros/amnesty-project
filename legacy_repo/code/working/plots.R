library(ggplot2)      # For plotting
library(tidycensus)   # For downloading Census data
library(tmap)         # For creating tmap
library(tmaptools)    # For reading and processing spatial data related to tmap
library(dplyr)        # For data wrangling
library(sf)  
library(sp)
library(rgeos)
library(wesanderson)
library(rmapshaper)
library(stringr)
library(extrafont)

wes_palette("Cavalcanti1") 
wes_palette("Rushmore") 
wes_palette("GrandBudapest1")[3]

amazon_bioma <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/aux/amazon_biome_border/amazon_biome_border.shp")
#terras indigenas
indigenous <- read_sf("/Users/pedrotremacoldirossi/Downloads/indigenous_area_amazon_biome/indigenous_area_amazon_biome.shp")
#unidades de conservacao
conservation <- read_sf("/Users/pedrotremacoldirossi/Downloads/conservation_units_amazon_biome")
#florestas publicas nao designadas
file_name <- list.files(path="/Users/pedrotremacoldirossi/Downloads/CNFP/", pattern="*.shp", recursive = TRUE)
fpnd <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("/Users/pedrotremacoldirossi/Downloads/CNFP/", f))
  fpnd <- rbind(fpnd, s)
}
sf_use_s2(FALSE)
fpnd_bioma <- st_intersection(fpnd, amazon_bioma)
fpnd_bioma1 <- fpnd_bioma[st_geometry_type(fpnd_bioma) == "MULTIPOLYGON", ]
fpnd_bioma2 <- fpnd_bioma[st_geometry_type(fpnd_bioma) == "POLYGON", ]
fpnd_bioma3 <- fpnd_bioma[str_detect(st_geometry_type(fpnd_bioma), "GEOMETRYCOLLECTION"), ]
fpnd_bioma3 <- st_collection_extract(fpnd_bioma3)

amazon_bioma <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
indigenous <- gSimplify(methods::as(object = indigenous, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
conservation <- gSimplify(methods::as(object = conservation, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
fpnd_bioma1 <- gSimplify(methods::as(object = fpnd_bioma1, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
fpnd_bioma2 <- gSimplify(methods::as(object = fpnd_bioma2, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
fpnd_bioma3 <- gSimplify(methods::as(object = fpnd_bioma3, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)

#st_geometry_type(fpnd_bioma1)
m <- tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#0B775E", 0.4), alpha("#81A88D", 0.4), "#F1BB7B", "#5B1A18"),
                border.col = "grey40", #size = bubble_sizes,
                labels = c('Indigenous', "Conservation", "Undesignated", "Properties (no overlap)", "Properties (overlap)")) +
  tm_shape(indigenous) + tm_fill(col = "#02401B", alpha = 0.4, border.col = "white", legend.show = T) + 
  tm_shape(conservation) + tm_fill(col = "#0B775E", alpha = 0.4, border.col = "white", legend.show = T) + 
  tm_shape(fpnd_bioma1) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "white", legend.show = T) +
  tm_shape(fpnd_bioma2) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "white", legend.show = F) +
  tm_shape(fpnd_bioma3) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "white", legend.show = F) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = .8, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino",
            legend.bg.color = "white",
            #legend.digits = 5,
            legend.bg.alpha = 1, legend.show = T)
##############################                ############################################################                ##############################
# file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/sicar/shapefiles/", pattern="*.shp", recursive = TRUE)
# file_name <- file_name[str_detect(file_name, "AREA_IMOVEL")]
# car <- data.frame()
# for(f in 1:200){
#   s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/sicar/shapefiles/", file_name[f]))
#   car <- rbind(car, s)
#   print(f)
# }
# car1 <- data.frame()
# for(f in 201:400){
#   s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/sicar/shapefiles/", file_name[f]))
#   car1 <- rbind(car1, s)
#   print(f)
# }
# car2 <- data.frame()
# for(f in 401:600){
#   s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/sicar/shapefiles/", file_name[f]))
#   car2 <- rbind(car2, s)
#   print(f)
# }
# car3 <- data.frame()
# for(f in 601:800){
#   s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/sicar/shapefiles/", file_name[f]))
#   car3 <- rbind(car3, s)
#   print(f)
# }
# car4 <- data.frame()
# for(f in 801:1100){
#   s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/sicar/shapefiles/", file_name[f]))
#   car4 <- rbind(car4, s)
#   print(f)
# }
# car5 <- data.frame()
# for(f in 1101:length(file_name)){
#   s <- read_sf(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/sicar/shapefiles/", file_name[f]))
#   car5 <- rbind(car5, s)
#   print(f)
# }
# car <- rbind(car, car1, car2, car3, car4, car5)
# st_write(car, "~/car_combined.shp")
car <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/cleaned/sicar/car_combined.shp")

indigenous <- read_sf("/Users/pedrotremacoldirossi/Downloads/indigenous_area_amazon_biome/indigenous_area_amazon_biome.shp")
conservation <- read_sf("/Users/pedrotremacoldirossi/Downloads/conservation_units_amazon_biome")
fpnd_bioma <- st_collection_extract(fpnd_bioma)
p <- data.table::rbindlist(list(indigenous, conservation, fpnd_bioma),
                           use.names = TRUE, fill = TRUE, idcol = NULL)
p <- st_as_sf(p) 

# Calculate area and tidy up
intersect_pct <- st_intersection(car, p) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(COD_IMOVEL, intersect_area) %>%   # only select columns needed to merge
  st_drop_geometry()  # drop geometry as we don't need it

ccar <- mutate(car, imovel_area = st_area(car))
ccar <- merge(ccar, intersect_pct, by = "COD_IMOVEL", all.x = TRUE)
library(data.table)
setDT(ccar)
ccar[, intersect_area := sum(intersect_area, na.rm = TRUE), by = COD_IMOVEL]
ccar[, overlap := as.numeric(intersect_area / imovel_area)]
ccar <- ccar[, .SD[1], by = COD_IMOVEL]
#ccar <- ccar %>% group_by(COD_IMOVEL) %>% mutate(intersect_area = sum(intersect_area, na.rm = T)) %>% ungroup()
#ccar <- ccar %>% mutate(overlap = as.numeric(intersect_area/imovel_area)) %>% group_by(COD_IMOVEL) %>% filter(row_number(COD_IMOVEL) == 1)
ccar$overlap <- ifelse(is.na(ccar$overlap), 0, ccar$overlap)
ccar <- st_as_sf(ccar)
ccar_clean <- ccar %>% filter(overlap == 0)
ccar_dirty <- ccar %>% filter(overlap > 0)

ccar_clean <- gSimplify(methods::as(object = ccar_clean, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
ccar_dirty <- gSimplify(methods::as(object = ccar_dirty, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)

m <- m + tm_shape(ccar_clean) + tm_borders(col = "#F1BB7B", lwd = .1)
m <- m + tm_shape(ccar_dirty) + tm_borders(col = "#5B1A18", lwd = .1)
m

tmap_save(m, "~/Documents/my_map.png", dpi = 300)
tmap_save(m, "~/Documents/my_map500.pdf", dpi = 300)
