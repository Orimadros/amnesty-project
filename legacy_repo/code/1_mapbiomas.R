library('stringr') 
library(tmap)         # For creating tmap
library(tmaptools)    # For reading and processing spatial data related to tmap
library(sf)  
library(sp)
library(rgeos)
library(wesanderson)
library(rmapshaper)
library(stringr)
library(extrafont)
library(timeDate)
library(stars)
library(data.table)
library(emilfun)
library(grDevices)
library(strex)
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

#---break up raster into data.table grids [1985-986]---# [X]
for(year in 1985:1986){
grey <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/mapbiomas/brasil_coverage_", year, ".tif"))

amazon_sf <-
  #--- get Amazon biome boundary ---
  read_sf("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/aux/amazon_biome_border/amazon_biome_border.shp") %>%
  #--- sp to sf ---# if needed
  #st_as_sf() %>%
  #--- transform using the CRS of the Mapbiomas raster  ---#
  st_transform(terra::crs(grey))
#--- crop to Amazon biome portion---#
grey <- terra::crop(grey, amazon_sf)

#e <- extent(-73.98327, -43.39941, -16.66204, 5.269697)

#--- horizontal grids: 31 [74-43]---#
#--- vertical grids: 23 [-17-6]---#
#--- total grids/year: 713---#
i.run <- function(h){
  for(v in seq(-17, 5)){
    
    if(v < 0){
      vv <- v+0.00009
    } else {
      vv <- v-0.00009
    }
    e <- extent(h+0.00009, h+1, vv, v+1)
    #--- converting to a data.frame ---#
    grey1_df <- as.data.frame(terra::crop(grey, e), xy = TRUE) 
    saveRDS(grey1_df, file = paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/grids/grid_", h, "_", h+1, "_", v, "_", v+1, "_", year, ".rds"))
    #print(v)
  }
}

registerDoParallel(cores = 8)
pdfs <- foreach(c=-74:-44) %dopar% i.run(c)

}

file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/grids/", pattern="*.rds", recursive = TRUE)
file_name <- unique(str_remove(str_remove(file_name, "grid_"), "_([0-9])([0-9])([0-9])([0-9]).rds$"))

#---generate legacy forest [1985-986]---# [X]
i.run <- function(k){
#for(k in file_name){
a <- readRDS(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/grids/grid_", k, "_1985.rds"))
b <- readRDS(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/grids/grid_", k, "_1986.rds"))
a$x <- as.character(a$x)
a$y <- as.character(a$y)
b$x <- as.character(b$x)
b$y <- as.character(b$y)

a <- as.data.table(a)
b <- as.data.table(b)

result <- a[b, on = .(x, y), nomatch = 0L]
result$cover <- ifelse(result$brasil_coverage_1985 %in% c(1, 3, 4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 13) & 
                       result$brasil_coverage_1986 %in% c(1, 3, 4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 13), "forest", "not forest")

result <- result[, .(x, y, cover)]
saveRDS(result[, .(x, y, cover)], file = paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/grid_", k, "_legacy.rds"))
}
registerDoParallel(cores = 8)
pdfs <- foreach(c=1:length(file_name)) %dopar% i.run(file_name[c])

#---put legacy together, export as full raster---# [X]
file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", pattern="*.rds", recursive = TRUE)

ras <- readRDS(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[1]))
ras$value <- ifelse(ras$cover == "forest", 1, 0)
ras <- ras[, .(x, y, value)]
ras <- rasterFromXYZ(ras)  #Convert first two columns as lon-lat and third as value                
crs(ras) <- CRS('+init=EPSG:4326')
i <- str_remove(file_name[1], ".rds")
terra::writeRaster(ras, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", i, ".tif"), filetype = "GTiff", overwrite = TRUE)
rm(ras)

file_name <- file_name[2:length(file_name)]
i.run <- function(k){
a <- readRDS(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", k))
i <- str_remove(k, ".rds")
a$value <- ifelse(a$cover == "forest", 1, 0)
a <- a[, .(x, y, value)]
a <- rasterFromXYZ(a)  #Convert first two columns as lon-lat and third as value                
crs(a) <- CRS('+init=EPSG:4326')
#ras <- merge(ras, a)
#rm(a)
#print(k)
terra::writeRaster(a, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", i, ".tif"), filetype = "GTiff", overwrite = TRUE)
}
registerDoParallel(cores = 15)
pdfs <- foreach(c=1:length(file_name)) %dopar% i.run(file_name[c])

file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", pattern="*.tif", recursive = TRUE)
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[1]))
for(k in 2:153){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/legacy_1-153.tif"), filetype = "GTiff", overwrite = TRUE)
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[154]))
for(k in 155:250){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/legacy_154-250.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[251]))
for(k in 252:350){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/legacy_251-350.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[351]))
for(k in 352:500){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/legacy_351-500.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[501]))
for(k in 502:600){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/legacy_501-600.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[601]))
for(k in 602:650){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/legacy_601-650.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[651]))
for(k in 652:713){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/legacy_651-713.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()

file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/", pattern="*.tif", recursive = TRUE)
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/", file_name[1]))
for(k in 2:length(file_name)){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/legacy_forest.tif"), filetype = "GTiff", overwrite = TRUE)

#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---plot legacy forest---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
file_name <- list.files(path="~/Downloads/legacy/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[!str_detect(file_name, "^plot")]

i.run <- function(k){  
  r <- raster(rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", k)))
  cropped_raster <- crop(r, amazon_bioma)
  final_raster <- mask(cropped_raster, amazon_bioma)
  if(!all(is.na(values(final_raster)))){
    terra::writeRaster(final_raster, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/plot_", k), filetype = "GTiff", overwrite = TRUE)
    #print(k)
  }
}
registerDoParallel(cores = 30)
pdfs <- foreach(c=1:length(file_name)) %dopar% i.run(file_name[c])
file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "plot")]
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[1]))
for(k in 2:73){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/plot_legacy_1-73.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[74]))
for(k in 75:150){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/plot_legacy_74-150.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[151]))
for(k in 152:200){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/plot_legacy_151-200.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[201]))
for(k in 202:270){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/plot_legacy_201-270.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[271]))
for(k in 272:350){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/plot_legacy_271-350.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[351]))
for(k in 352:422){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/plot_legacy_351-422.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()

file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "plot")]
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/", file_name[1]))
for(k in 2:length(file_name)){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/plot_legacy_forest.tif"), filetype = "GTiff", overwrite = TRUE)
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#file_name <- list.files(path="~/Downloads/legacy_merged/", pattern="*.tif", recursive = TRUE)
#file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
#r <- rast(paste0("~/Downloads/legacy_merged/", file_name[1]))
#plot(r, col = c("white", "#02401B"), box = FALSE, legend = FALSE, axes = FALSE)
#amazon_bioma <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/aux/amazon_biome_border/amazon_biome_border.shp")
#amazon_bioma <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
#terra::plot(amazon_bioma, border="black", add = TRUE, col = NA, lwd = 1.5)

#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---
#---break up raster into data.table grids---# [X]
for(year in 1987:2020){
  grey <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/mapbiomas/brasil_coverage_", year, ".tif"))
  
  amazon_sf <-
    #--- get Amazon biome boundary ---
    read_sf("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/aux/amazon_biome_border/amazon_biome_border.shp") %>%
    #--- sp to sf ---# if needed
    #st_as_sf() %>%
    #--- transform using the CRS of the Mapbiomas raster  ---#
    st_transform(terra::crs(grey))
  #--- crop to Amazon biom portion---#
  grey <- terra::crop(grey, amazon_sf)
  
  #e <- extent(-73.98327, -43.39941, -16.66204, 5.269697)
  
  #--- horizontal grids: 31 [74-43]---#
  #--- vertical grids: 23 [-17-6]---#
  #--- total grids/year: 713---#
  i.run <- function(h){
    for(v in seq(-17, 5)){
      
      if(v < 0){
        vv <- v+0.00009
      } else {
        vv <- v-0.00009
      }
      e <- extent(h+0.00009, h+1, vv, v+1)
      #--- converting to a data.frame ---#
      grey1_df <- as.data.frame(terra::crop(grey, e), xy = TRUE) 
      saveRDS(grey1_df, file = paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/grids/grid_", h, "_", h+1, "_", v, "_", v+1, "_", year, ".rds"))
      #print(v)
    }
  }
  
  registerDoParallel(cores = 8)
  pdfs <- foreach(c=-74:-44) %dopar% i.run(c)
  
}
#---generate forest and human cover---# [X]
file_name <- list.files(path="~/Downloads/grids/", pattern="*.rds", recursive = TRUE)
file_name <- file_name[!str_detect(file_name, "1985") & !str_detect(file_name, "1986")]
#file_name <- file_name[!str_detect(file_name, "1986") & !str_detect(file_name, "1987") & !str_detect(file_name, "1988") & !str_detect(file_name, "1989") & !str_detect(file_name, "1990")]
file_name <- file_name[!str_detect(file_name, "2021") & !str_detect(file_name, "2022")]

#---generate pixels in Amazon biome---# [X]
file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy_merged/", file_name[1]))
amazon_sf <-
  #--- get Amazon biome boundary ---
  read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/aux/amazon_biome_border/amazon_biome_border.shp") %>%
  #--- transform using the CRS of the Mapbiomas raster  ---#
  st_transform(terra::crs(r))
#--- crop to Amazon biom portion---#
r <- terra::crop(r, amazon_sf)
i.run <- function(h){
    for(v in seq(-17, 5)){
      
      if(v < 0){
        vv <- v+0.00009
      } else {
        vv <- v-0.00009
      }
      e <- extent(h+0.00009, h+1, vv, v+1)
      #--- converting to a data.frame ---#
      grey1_df <- as.data.frame(terra::crop(r, e), xy = TRUE) 
      if(length(grey1_df$x) > 0){
      saveRDS(grey1_df, file = paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/grids_bioma/grid_", h, "_", h+1, "_", v, "_", v+1, ".rds"))
      }
      #print(v)
    }
  }
registerDoParallel(cores = 12)
pdfs <- foreach(c=-74:-44) %dopar% i.run(c)

#---create land cover for all pixels and all grids---# [X]
for(y in 1987:2020){
f <- file_name[str_detect(file_name, as.character(y))]  
i.run <- function(k){
  #for(k in file_name){
  a <- readRDS(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/grids/", k))
  a$x <- as.character(a$x)
  a$y <- as.character(a$y)
  # b$x <- as.character(b$x)
  # b$y <- as.character(b$y)
  # 
  #a <- as.data.table(a)
  #b <- as.data.table(b)
  names(a)[3] <- "brasil_coverage"
  #result <- a[b, on = .(x, y), nomatch = 0L]
  a$cover <- ifelse(a$brasil_coverage %in% c(1, 3, 4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 13), "forest", "")
  a$cover <- ifelse(a$brasil_coverage %in% c(14, 15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21, 24, 30), "human", a$cover)
  
  a <- a %>% dplyr::select(x, y, cover)
  
  i <- str_remove(str_remove(k, "grid_"), ".rds$")
  
  saveRDS(a, file = paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions/grid_", i, "_transitions.rds"))
}
registerDoParallel(cores = 10)
pdfs <- foreach(c=1:length(f)) %dopar% i.run(f[c])
}

#---integrate pixel by pixel for all years with land cover---# [~]
#---compute deforested, reforested transitions, export as raster by year---#
#---if area always been forest (since legacy), remains as forest (1)---#
#---if area becomes deforested, remains deforested (2) until and if changes to reforested (3)---#
#---legacy pixels that start with human cover are coded as 0, just pixels with cover not in {forest, human}---#
#---compute back-to-back pixel cover changes (measure of noiseness in pixel---#
#---!!!! uses C++ to compute transitions, must load in Rcpp external function---#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions/", pattern="*.rds", recursive = TRUE)
file_name <- file_name[!str_detect(file_name, "2022")]
f <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/grids_bioma/", pattern="*.rds", recursive = TRUE) #list of Amazon biome pixels
file_name <- file_name[str_remove(str_remove(file_name, "_([0-9])([0-9])([0-9])([0-9])_transitions.rds"), "grid_") %in% str_remove(str_remove(f, ".rds"), "grid_")]
f <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", pattern="*.rds", recursive = TRUE)  
file_name <- file_name[!str_remove(str_remove(file_name, "_([0-9])([0-9])([0-9])([0-9])_transitions.rds"), "grid_") %in% str_remove(str_remove(f, "_cover.rds"), "grid_")]
grids <- unique(str_remove(str_remove(file_name, "grid_"), "_([0-9])([0-9])([0-9])([0-9])_transitions.rds$"))

i.run <- function(y){
  f <- file_name[str_detect(file_name, as.character(y))]  
  #i <- which(y == stata_file_number$grids)
  a <- readRDS(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions/", f[1]))
  names(a)[3] <- paste0("cover_1987")
  f <- f[2:length(f)]
  
for(k in f){    
    b <- readRDS(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions/", k))
    b <- b %>% dplyr::select(cover)
    names(b) <- paste0("cover_", str_extract(k, "([0-9])([0-9])([0-9])([0-9])"))
    a <- cbind(a, b)
    #print(k)
}
  b <- readRDS(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", str_replace(str_remove(k, "_([0-9])([0-9])([0-9])([0-9])"), "transitions", "legacy")))
  b <- b %>% dplyr::select(cover)
  a <- cbind(a, b)
  t <- a %>% dplyr::select(-x, -y) %>% dplyr::select(cover, everything())
  
  count_value_changes_optimized <- function(df) {
    # Calculate the differences between adjacent columns
    diffs <- df[, -ncol(df)] != df[, -1]
    
    # Sum the differences row-wise
    counts <- rowSums(diffs)
    
    return(counts)
  }
  a$n_pixel_change_back_to_back <- count_value_changes_optimized(t)
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---  
  Rcpp::sourceCpp("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/code/final/aux/deforestation_rules.cpp")
  t[, 2] <- ifelse(t[, 1] == "forest" & t[, 2] == "human", "deforested", t[, 2])
  t <- as.data.frame(apply_deforestation_rules_cpp(as.matrix(t)))
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---  

for(j in 2:35){
  i <- j + 1985
  x <- cbind(a %>% dplyr::select(x, y), t[, j])
  names(x)[3] <- "cover"
  x$value <- ifelse(x$cover == "forest", 1, 0)
  x$value <- ifelse(x$cover == "deforested", 2, x$value)
  x$value <- ifelse(x$cover == "reforested", 3, x$value)
  x <- x %>% dplyr::select(x, y, value)
  
  x <- rasterFromXYZ(x)  #Convert first two columns as lon-lat and third as value                
  crs(x) <- CRS('+init=EPSG:4326')
  terra::writeRaster(x, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", gsub("_([0-9])([0-9])([0-9])([0-9])_transitions.*", "", k), "_", i, ".tif"), filetype = "GTiff", overwrite = TRUE)
}
  
  saveRDS(a %>% dplyr::select(x, y, n_pixel_change_back_to_back), file = paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", gsub("_([0-9])([0-9])([0-9])([0-9])_transitions.*", "", k), "_cover.rds"))
}
registerDoParallel(cores = 4)
pdfs <- foreach(c=1:length(grids)) %dopar% i.run(grids[c])


#---plot legacy forest---#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", pattern="*.tif", recursive = TRUE)
f <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/legacy/", pattern="*.tif", recursive = TRUE)
f <- str_remove(str_remove(f[str_detect(f, "plot")], "plot_grid_"), "_legacy.tif")
file_name <- file_name[str_remove(str_remove(file_name, "grid_"), "_([0-9]+).tif") %in% f]
#f <- file_name

for(y in seq(1987, 2020)){
f <- file_name[str_detect(file_name, as.character(y))]
#---                    ---#
amazon_bioma <- read_sf("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/aux/amazon_biome_border/amazon_biome_border.shp")
#---                    ---#
i.run <- function(k){  
  r <- raster(rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", k)))
  cropped_raster <- crop(r, amazon_bioma)
  final_raster <- mask(cropped_raster, amazon_bioma)
  if(!all(is.na(values(final_raster)))){
    terra::writeRaster(final_raster, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/plot_", k), filetype = "GTiff", overwrite = TRUE)
  }
}
registerDoParallel(cores = 20)
pdfs <- foreach(c=1:length(f)) %dopar% i.run(f[c])
}

f <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", pattern="*.tif", recursive = TRUE)
f <- f[str_detect(f, "plot")]
f <- f[!str_detect(f, "legacy")]
#bring y to 1992
for(y in seq(2013, 2019)){
file_name <- f[str_detect(f, as.character(y))]
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[1]))
for(k in 2:73){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/plot_legacy_", y, "_1-73.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[74]))
for(k in 75:150){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/plot_legacy_", y, "_74-150.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[151]))
for(k in 152:200){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/plot_legacy_", y, "_151-200.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[201]))
for(k in 202:270){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/plot_legacy_", y, "_201-270.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[271]))
for(k in 272:350){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/plot_legacy_", y, "_271-350.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[351]))
for(k in 352:422){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/plot_legacy_", y, "_351-422.tif"), filetype = "GTiff", overwrite = TRUE)
rm(a, r)
gc()
}

f <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", pattern="*.tif", recursive = TRUE)
f <- f[str_detect(f, "plot_legacy")]
for(y in 2016:2018){
file_name <- f[str_detect(f, as.character(y))]

r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[1]))
for(k in 2:length(file_name)){
  a <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[k]))
  r <- merge(r, a)
  print(k)
}
terra::writeRaster(r, paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/plot_legacy_forest_", y, ".tif"), filetype = "GTiff", overwrite = TRUE)
rm(r)
}
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#DEFORESTATION STATS
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
#---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---##---#
sf_use_s2(FALSE)
options(scipen=999)
# A helper function that erases all of y from x: 
st_erase = function(x, y) st_difference(x, y)
#---load states---#  
states <- read_sf("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/auxiliary/BR_UF_2021/BR_UF_2021.shp") %>% st_make_valid()
amazon_bioma <- read_sf("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/auxiliary/amazon_biome_border/amazon_biome_border.shp")
states <- st_intersection(states, amazon_bioma)
states1 <- states[st_geometry_type(states) == "MULTIPOLYGON", ]
states2 <- states[st_geometry_type(states) == "POLYGON", ]
states3 <- states[str_detect(st_geometry_type(states), "GEOMETRYCOLLECTION"), ]
states3 <- st_collection_extract(states3)

glebas1 <- read_sf("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/auxiliary/glebas_federais/i3geomap_glebas_federais.shp")
glebas1 <- st_intersection(glebas1, amazon_bioma)
# Function to remove overlapping areas systematically with spatial indexing
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
glebas1 <- remove_overlaps_optimized(glebas1)

#----FEDERAL GLEBAS IN AMAZON BIOME AREA: 115,827,209 hectares----#
#----GLEBAS FEDERAIS WITHOUT INDIGENOUS REZ AND UC: 70,668,189----#
sum(glebas1 %>% st_area())/10000

#florestas publicas nao designadas
file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/cnfp/SHP_2013/", pattern="*.shp$", recursive = TRUE)
fpnd <- data.frame()
for(f in file_name){
  s <- read_sf(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/cnfp/SHP_2013/", f))
  fpnd <- rbind(fpnd, s)
}
fpnd <- st_intersection(fpnd, amazon_bioma)
# if (!require("pacman")) install.packages("pacman")
#  pacman::p_load(textreadr, magrittr)
#  pacman::p_load_gh("trinker/pathr")
#  trans_docs <- dir(
#    system.file("docs", package = "textreadr"), 
#    pattern = "^trans",
#    full.names = TRUE
#  )
# pdf_doc <- "~/Downloads/glebas_TLP.pdf"
#  d <- pdf_doc %>%
#    textreadr::read_pdf() %>%
#    dplyr::select(text)
#  d <- data.frame(unlist(strsplit(d[[1]], "\n")))
#  colnames(d) <- "text"
#  d <- d %>% filter(!str_detect(text, "^ITEM"))
#  d$uf <- str_extract(d$text, "([:upper:])([:upper:])$")
#  d$gleba_nome <- str_squish(str_remove(str_remove(d$text, "([:upper:])([:upper:])$"), "^[ ]*([0-9]+)"))
#  d <- d %>% filter(!is.na(uf)) %>% dplyr::select(-text)
glebas <- sf::st_simplify(glebas)
sf_use_s2(FALSE)
# file_name <- list.files(path="/Users/pedrotremacoldirossi/Downloads/cnfp2022", pattern="*.shp$", recursive = TRUE)
# fpnd <- data.frame()
# for(f in file_name){
#   s <- read_sf(paste0("/Users/pedrotremacoldirossi/Downloads/cnfp2022/", f))
#   fpnd <- rbind(fpnd, s)
# }
target_areas <- fpnd %>% filter(tipo == "TIPO B") %>% filter(governo == "FEDERAL")
#federal <- fpnd %>% filter(governo == "FEDERAL")
control <- fpnd %>% filter(governo == "FEDERAL") %>% filter(str_detect(classe, "UC") | str_detect(classe, "TI")) %>% filter(ano < 2010)
control <- st_intersection(control, amazon_bioma)

tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  #tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#F59077", 0.4)),
   #             border.col = "grey40",
    #            labels = c('Federal FPND', "Federal Glebas (net rez/uc)")) +
  tm_shape(control) + tm_fill(col = "#02401B", alpha = 0.6, border.col = "#02401B") +
  tm_shape(target_areas %>% filter(ano < 2015)) + tm_fill(col = "#F59077", alpha = 0.6, border.col = "#F59077") +
  #tm_shape(glebas_net) + tm_borders(col = "#02401B", lwd = .5) +
  #tm_shape(glebas_net) + tm_fill(col = "#F59077", alpha = 0.2, border.col = "#F59077") +
  #tm_shape(glebas_net2) + tm_fill(col = "#F59077", alpha = 0.4, border.col = "#F59077") +
  tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(states3) + tm_borders(col = "gray", lwd = 1) + 
  tm_layout(frame = FALSE, legend.title.size = 1, legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Times New Roman", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)

#----FLORESTAS PUBLICAS NAO DESIGNADAS (2013 REFERENCE YEAR)----#
#----NON-FEDERAL: 44,179,757 (57%)------------------------------#
#----FEDERAL: 33,403,024 (43%)----------------------------------#
#----ALL FPND: 77,582,781---------------------------------------#
#---------------------------------------------------------------#
nonFederal <- fpnd %>% filter(tipo == "TIPO B") %>% filter(governo %in% c("ESTADUAL", "MUNICIPAL", "FEDERAL/ESTADUAL"))
nonFederal <- remove_overlaps_optimized(nonFederal %>% filter(protecao == "SEM DESTINACAO"))
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
sum(nonFederal %>% mutate(area = st_area(.)) %>% .$area)/10000
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#federal <- fpnd %>% filter(governo == "FEDERAL")
#federal <- remove_overlaps_optimized(fpnd %>% filter(protecao == "SEM DESTINACAO"))
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
sum(target_areas %>% mutate(area = st_area(.)) %>% .$area)/10000
#sum(federal %>% filter(protecao == "SEM DESTINACAO") %>% mutate(area = st_area(.)) %>% .$area)/10000
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#terras indigenas
#indigenous <- read_sf("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/auxiliary/indigenous_area_amazon_biome/indigenous_area_amazon_biome.shp")
#indigenous <- st_intersection(indigenous, amazon_bioma)
#indigenous <- remove_overlaps_optimized(indigenous)
#unidades de conservacao
#conservation <- read_sf("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/auxiliary/conservation_units_amazon_biome")
#conservation <- conservation %>% filter(esfera == "federal") %>% filter(as.numeric(ano_cria) < 2010)
#conservation <- st_intersection(conservation, amazon_bioma)
#conservation <- remove_overlaps_optimized(conservation)
#control_areas <- st_union(st_union(indigenous), st_union(conservation))
#control_areas <- remove_overlaps_optimized(control_areas)
#control_areas2 <- st_as_sf(control_areas)
#control_areas2 <- st_collection_extract(control_areas2)
#sum(indigenous %>% st_area())
#sum(conservation %>% st_area())
#control_areas %>% st_area()
glebas_net <- st_erase(glebas1, st_union(control))
#glebas_net <- st_erase(glebas1, st_union(indigenous))
glebas_net <- remove_overlaps_optimized(glebas_net)
#glebas_net <- st_erase(glebas_net, st_union(conservation))
#glebas_net <- remove_overlaps_optimized(glebas_net)
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
sum(glebas_net %>% st_area())/10000
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
glebas_net2 <- glebas_net[str_detect(st_geometry_type(glebas_net), "GEOMETRYCOLLECTION"), ]
glebas_net2 <- st_collection_extract(glebas_net2)
glebas_net <- glebas_net[!str_detect(st_geometry_type(glebas_net), "GEOMETRYCOLLECTION"), ]

tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#F59077", 0.4)),
                border.col = "grey40",
                labels = c('Federal FPND', "Federal Glebas (net rez/uc)")) +
  tm_shape(target_areas) + tm_fill(col = "#02401B", alpha = 0.6, border.col = "#02401B") +
  #tm_shape(glebas_net) + tm_borders(col = "#02401B", lwd = .5) +
  tm_shape(glebas_net) + tm_fill(col = "#F59077", alpha = 0.2, border.col = "#F59077") +
  #tm_shape(glebas_net2) + tm_fill(col = "#F59077", alpha = 0.4, border.col = "#F59077") +
  tm_shape(states1) + tm_borders(col = "gray", lwd = 1) + tm_shape(states2) + tm_borders(col = "gray", lwd = 1) +
  tm_shape(states3) + tm_borders(col = "gray", lwd = 1) + 
  tm_layout(frame = FALSE, legend.title.size = 1, legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Times New Roman", legend.bg.color = "white", legend.bg.alpha = 1, legend.show = T)

#glebas_net <- st_erase(glebas_net, st_union(conservation))
glebas1_net_u <- st_union(glebas_net)
glebas_leftover <- st_erase(glebas, glebas1_net_u)
#control_areas <- st_union(st_union(indigenous), st_union(conservation))
#control_areas <- remove_overlaps_optimized(control_areas)
#control_areas <- st_collection_extract(control_areas)
year <- seq(2005, 2014)
table_amazon_biome <- data.frame()
table_glebas_biome <- data.frame()
table_glebas_net_biome <- data.frame()
table_control_areas <- data.frame()
table_car_ineligible_biome <- data.frame()
table_car_eligible_biome <- data.frame()

for(y in year){
file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "plot_legacy")]
file_name <- file_name[str_detect(file_name, as.character(y))]

forest <- data.frame()

#system.time(
for(k in 1:6){
#p1 <- as.data.frame(rast(paste0("/Volumes/ElementsMain/transitions_combined/", file_name[k])))
p1 <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[k]))

total_cells <- ncell(p1$layer)
forest_cells <- global(p1, fun = function(x) sum(x == 1, na.rm = T))*0.09
deforest_cells <- global(p1, fun = function(x) sum(x == 2, na.rm = T))*0.09
reforest_cells <- global(p1, fun = function(x) sum(x == 3, na.rm = T))*0.09

#forest <- rbind(forest, cbind(length(p1$layer), sum(p1 %>% filter(layer == 1) %>% .$layer)))
forest <- rbind(forest, cbind(total_cells, forest_cells, deforest_cells, reforest_cells))
print(k)
}
#)

n_pixels <- sum(forest[, 1])
forest_area_hc <- sum(forest[, 2])
deforested_area_hc <- sum(forest[, 3])
reforested_area_hc <- sum(forest[, 4])

table_amazon_biome <- rbind(table_amazon_biome, cbind(y, n_pixels, forest_area_hc, deforested_area_hc, reforested_area_hc))
}
colnames(table_amazon_biome) <- c("year", "n_pixels", "forest_area_hc", "deforested_area_hc", "reforested_area_hc")

for(y in year){
start.time <- Sys.time()
file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "plot_legacy")]
file_name <- file_name[str_detect(file_name, as.character(y))]
i.run0 <- function(j){
  p1 <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[j]))
  
  i.run <- function(i){ 
    cropped <- terra::extract(p1, glebas1[i, ], xy = T)
    c(length(cropped %>% filter(layer == 1) %>% .$layer)*0.09, length(cropped %>% filter(layer == 2) %>% .$layer)*0.09, length(cropped %>% filter(layer == 3) %>% .$layer)*0.09)
  }
  registerDoParallel(cores = 4)
  pdfs <- foreach(c=1:length(glebas1$GID0)) %dopar% i.run(c)
  pp <- data.frame(do.call(rbind, pdfs))
  
  c(sum(pp[, 1]), sum(pp[, 2]), sum(pp[, 3]))
}
registerDoParallel(cores = 3)
glebas_stats <- foreach(c=1:6) %dopar% i.run0(c)
glebas_stats <- data.frame(do.call(rbind, glebas_stats))

table_glebas_biome <- rbind(table_glebas_biome, cbind(y, sum(glebas_stats[, 1]), sum(glebas_stats[, 2]), sum(glebas_stats[, 3]))) 

print(y)
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)
}
colnames(table_glebas_biome) <- c("year", "forest_area_hc", "deforested_area_hc", "reforested_area_hc")

for(y in year){
  start.time <- Sys.time()
  file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy")]
  file_name <- file_name[str_detect(file_name, as.character(y))]
  i.run0 <- function(j){
    p1 <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[j]))
    
    i.run <- function(i){ 
      cropped <- terra::extract(p1, control_areas2[i, ], xy = T)
      c(length(cropped %>% filter(layer == 1) %>% .$layer)*0.09, length(cropped %>% filter(layer == 2) %>% .$layer)*0.09, length(cropped %>% filter(layer == 3) %>% .$layer)*0.09)
    }
    registerDoParallel(cores = 3)
    pdfs <- foreach(c=1:length(control_areas2$x)) %dopar% i.run(c)
    pp <- data.frame(do.call(rbind, pdfs))
    
    c(sum(pp[, 1]), sum(pp[, 2]), sum(pp[, 3]))
  }
  registerDoParallel(cores = 2)
  glebas_stats <- foreach(c=1:6) %dopar% i.run0(c)
  glebas_stats <- data.frame(do.call(rbind, glebas_stats))
  
  table_control_areas <- rbind(table_control_areas, cbind(y, sum(glebas_stats[, 1]), sum(glebas_stats[, 2]), sum(glebas_stats[, 3]))) 
  
  print(y)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
colnames(table_control_areas) <- c("year", "forest_area_hc", "deforested_area_hc", "reforested_area_hc")

for(y in year){
  start.time <- Sys.time()
  file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy")]
  file_name <- file_name[str_detect(file_name, as.character(y))]
  i.run0 <- function(j){
    p1 <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[j]))
    
    i.run <- function(i){ 
      cropped <- terra::extract(p1, glebas_net[i, ], xy = T)
      c(length(cropped %>% filter(layer == 1) %>% .$layer)*0.09, length(cropped %>% filter(layer == 2) %>% .$layer)*0.09, length(cropped %>% filter(layer == 3) %>% .$layer)*0.09)
    }
    registerDoParallel(cores = 3)
    pdfs <- foreach(c=1:length(glebas_net$GID0)) %dopar% i.run(c)
    pp <- data.frame(do.call(rbind, pdfs))
    
    c(sum(pp[, 1]), sum(pp[, 2]), sum(pp[, 3]))
  }
  registerDoParallel(cores = 3)
  glebas_stats <- foreach(c=1:6) %dopar% i.run0(c)
  glebas_stats <- data.frame(do.call(rbind, glebas_stats))
  
  table_glebas_net_biome <- rbind(table_glebas_net_biome, cbind(y, sum(glebas_stats[, 1]), sum(glebas_stats[, 2]), sum(glebas_stats[, 3]))) 
  
  print(y)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
colnames(table_glebas_net_biome) <- c("year", "forest_area_hc", "deforested_area_hc", "reforested_area_hc")

#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---##---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---##---#---#---#---#
car_eligible <- read_sf("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazon_working/amazon/car_eligible_cleaned.shp")
car_ineligible <- read_sf("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazon_working/amazon/car_all_cleaned.shp")
car_ineligible <- car_ineligible %>% filter(!COD_IMO %in% car_eligible$COD_IMO)
car_ineligible <- car_ineligible %>% mutate(area = st_area(.)) %>% filter(as.numeric(area)/10000 < 100000)

for(y in year){
  start.time <- Sys.time()
  file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy")]
  file_name <- file_name[str_detect(file_name, as.character(y))]
  i.run0 <- function(j){
    p1 <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[j]))
    
    i.run <- function(i){ 
      cropped <- terra::extract(p1, car_ineligible[i, ], xy = T)
      c(length(cropped %>% filter(layer == 1) %>% .$layer)*0.09, length(cropped %>% filter(layer == 2) %>% .$layer)*0.09, length(cropped %>% filter(layer == 3) %>% .$layer)*0.09)
    }
    registerDoParallel(cores = 3)
    pdfs <- foreach(c=1:length(car_ineligible$COD_IMO)) %dopar% i.run(c)
    pp <- data.frame(do.call(rbind, pdfs))
    
    c(sum(pp[, 1]), sum(pp[, 2]), sum(pp[, 3]))
  }
  registerDoParallel(cores = 3)
  car_ineligible_stats <- foreach(c=1:6) %dopar% i.run0(c)
  car_ineligible_stats <- data.frame(do.call(rbind, car_ineligible_stats))
  
  table_car_ineligible_biome <- rbind(table_car_ineligible_biome, cbind(y, sum(car_ineligible_stats[, 1]), sum(car_ineligible_stats[, 2]), sum(car_ineligible_stats[, 3]))) 
  
  print(y)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
colnames(table_car_ineligible_biome) <- c("year", "forest_area_hc", "deforested_area_hc", "reforested_area_hc")

for(y in year){
  start.time <- Sys.time()
  file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy")]
  file_name <- file_name[str_detect(file_name, as.character(y))]
  i.run0 <- function(j){
    p1 <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[j]))
    
    i.run <- function(i){ 
      cropped <- terra::extract(p1, car_eligible[i, ], xy = T)
      c(length(cropped %>% filter(layer == 1) %>% .$layer)*0.09, length(cropped %>% filter(layer == 2) %>% .$layer)*0.09, length(cropped %>% filter(layer == 3) %>% .$layer)*0.09)
    }
    registerDoParallel(cores = 3)
    pdfs <- foreach(c=1:length(car_eligible$COD_IMO)) %dopar% i.run(c)
    pp <- data.frame(do.call(rbind, pdfs))
    
    c(sum(pp[, 1]), sum(pp[, 2]), sum(pp[, 3]))
  }
  registerDoParallel(cores = 3)
  car_eligible_stats <- foreach(c=1:6) %dopar% i.run0(c)
  car_eligible_stats <- data.frame(do.call(rbind, car_eligible_stats))
  
  table_car_eligible_biome <- rbind(table_car_eligible_biome, cbind(y, sum(car_eligible_stats[, 1]), sum(car_eligible_stats[, 2]), sum(car_eligible_stats[, 3]))) 
  
  print(y)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}
colnames(table_car_eligible_biome) <- c("year", "forest_area_hc", "deforested_area_hc", "reforested_area_hc")
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---##---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---##---#---#---#---#


#---##---##---##---##---##---##---##---##---##---##---##---##---
#LOAD official mapbiomas amazon biome forest cover (won't perfectly match because in our definition we consider "outras formacoes nao florestais")
#---##---##---##---##---##---##---##---##---##---##---##---##---
library(readxl)
mapbiomas_amazon_official <- read_excel("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazon_project/miseEnPlace/mapbiomas_amazon_official.xlsx")

plot1 <- mapbiomas_amazon_official %>% left_join(table_amazon_biome, "year") %>% filter(!is.na(n_pixels))

cols2 <- c("Mapabiomas"="#02401B", "Data"=alpha("#97A79C", 0.5))
  ggplot() +
  ylim(350, 370) + #+
  geom_line(aes(x = plot1$year, y = plot1$forest/1000000, colour = "Mapabiomas"), size=2, alpha=1) + 
  geom_line(aes(x = plot1$year, y = plot1$forest_area_hc/1000000, colour = "Data"), size=2, alpha=.5) + 
  #geom_vline(xintercept = 2009, linetype="dashed", color = "gray", size=1.5) + 
  scale_x_continuous(limits = c(2005, 2014), breaks = seq(2005, 2014), labels = c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014")) + 
  guides(color = guide_legend(override.aes = list(size = 3), ncol = 1)) + 
  theme_minimal() + labs(title = "Amazon Biome Vegetation Coverage", subtitle = "(millions of hectares)", x = NULL, y = NULL) +
  geom_abline(intercept =0 , slope = 1, color = "gray", alpha = 0.5, size = 1.5) +  
    theme(text = element_text(family = "Times New Roman", size = 25), axis.title.x=element_text(colour="black"), axis.text = element_text(colour = "black"), 
          axis.title.y=element_text(colour="black", angle = 0, vjust = 0.5), plot.title = element_text(face="bold", hjust = -.1, size = 25, vjust = 1.5),
          plot.subtitle = element_text(face="italic", hjust = -.05, size = 20),
          panel.grid.major = element_line(color = "gray96", linetype = "longdash", size = 1), legend.position=c(.8,.9), panel.grid.minor = element_blank(),
          legend.title = element_blank(), legend.spacing.y = unit(0, "mm"), 
          legend.background = element_rect(fill = "gray98", colour = "white"), legend.key.size = unit(2,"line")) + scale_color_manual(name="Bar",values=cols2)
sum(table_glebas_biome[1, -1])/sum(table_amazon_biome[1, -c(1:2)])*100
  
  ggplot() +
    ylim(40, 45) + #+
    geom_line(aes(x = table_amazon_biome$year, y = table_glebas_biome$deforested_area_hc/table_amazon_biome$deforested_area_hc*100), colour = "#F1BB7B", size=2, alpha=1) + 
    scale_x_continuous(limits = c(2005, 2014), breaks = seq(2005, 2014), labels = c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014")) + 
    guides(color = guide_legend(override.aes = list(size = 3), ncol = 1)) + 
    theme_minimal() + labs(title = "% Amazon Deforestation in Glebas", x = NULL, y = NULL) +
    geom_abline(intercept =0 , slope = 1, color = "gray", alpha = 0.5, size = 1.5) +  
    theme(text = element_text(family = "Times New Roman", size = 25), axis.title.x=element_text(colour="black"), axis.text = element_text(colour = "black"), 
          axis.title.y=element_text(colour="black", angle = 0, vjust = 0.5), plot.title = element_text(face="bold", hjust = -.1, size = 25, vjust = 1.5),
          plot.subtitle = element_text(face="italic", hjust = -.05, size = 20), panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray96", linetype = "longdash", size = 1), legend.title = element_blank(),
          #panel.border = element_rect(colour = "black", fill=NA),
          #aspect.ratio = 1,
          legend.background = element_blank(), 
          legend.box.background = element_rect(colour = "black")) #+ scale_color_manual(name="Bar",values=cols2)
############################################################################ END  ############################################################################ END
############################################################################ END  ############################################################################ END
############################################################################ END  ############################################################################ END
#---plot pixel transitions---#
amazon_bioma <- read_sf("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/raw/aux/amazon_biome_border/amazon_biome_border.shp")
states <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/auxiliary/BR_UF_2021/BR_UF_2021.shp") %>% st_make_valid()
states <- st_intersection(states, amazon_bioma)
states <- sf::st_simplify(states)
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
colors <- c("gray80", "#02401B", "#F1BB7B", "#0B775E")
labels <- c("Other", "Forest", "Deforested", "Reforested")
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
year <- c(2020)
table_amazon_biome <- data.frame()

# total cells (per layer) and forest cells (value == 1) in hectares assuming 30m pixels (0.09 ha)
total_cells_per_layer <- rep(ncell(p1), nlyr(p1))               # same for each layer
forest_cells_per_layer <- global(p1 == 1, "sum", na.rm = TRUE)  # data.frame with one row, nlyr columns
forest_ha_per_layer    <- as.numeric(forest_cells_per_layer[1, ]) * 0.09
# deforested cells (value == 2)
deforest_cells <- global(p1 == 2, "sum", na.rm = TRUE)
deforest_ha    <- as.numeric(deforest_cells[1, ]) * 0.09

# reforested cells (value == 3)
reforest_cells <- global(p1 == 3, "sum", na.rm = TRUE)
reforest_ha    <- as.numeric(reforest_cells[1, ]) * 0.09

cbind(total_cells, forest_ha_per_layer, deforest_ha, reforest_ha)

for(y in year){
  file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", pattern="*.tif", recursive = TRUE)
  file_name <- file_name[str_detect(file_name, "plot_legacy")]
  file_name <- file_name[str_detect(file_name, as.character(y))]
  
  forest <- data.frame()
  
  #system.time(
  for(k in 1:6){
    #p1 <- as.data.frame(rast(paste0("/Volumes/ElementsMain/transitions_combined/", file_name[k])))
    p1 <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/transitions_combined/", file_name[k]))
    
    total_cells <- ncell(p1$layer)
    forest_cells <- global(p1, fun = function(x) sum(x == 1, na.rm = T))*0.09
    deforest_cells <- global(p1, fun = function(x) sum(x == 2, na.rm = T))*0.09
    reforest_cells <- global(p1, fun = function(x) sum(x == 3, na.rm = T))*0.09
    
    #forest <- rbind(forest, cbind(length(p1$layer), sum(p1 %>% filter(layer == 1) %>% .$layer)))
    forest <- rbind(forest, cbind(total_cells, forest_cells, deforest_cells, reforest_cells))
    print(k)
  }
  #)
  
  n_pixels <- sum(forest[, 1])
  forest_area_hc <- sum(forest[, 2])
  deforested_area_hc <- sum(forest[, 3])
  reforested_area_hc <- sum(forest[, 4])
  
  table_amazon_biome <- rbind(table_amazon_biome, cbind(y, n_pixels, forest_area_hc, deforested_area_hc, reforested_area_hc))
}
colnames(table_amazon_biome) <- c("year", "n_pixels", "forest_area_hc", "deforested_area_hc", "reforested_area_hc")





file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/processing/legacy_merged/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
r <- rast(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/processing/legacy_merged/", file_name[1]))
r <- as.factor(r)

ggplot() +
  tidyterra::geom_spatraster(data = r, aes(fill = layer), maxcell=600000) +
  tidyterra::scale_fill_whitebox_d(palette = "bl_yl_rd", na.translate = T) + theme_minimal() + #labs(title = "MAP") + 
  scale_fill_manual(values = colors, labels = labels, na.translate = F, na.value = "white") + 
  tidyterra::geom_spatvector(data = amazon_bioma, fill = NA, col = "black") + tidyterra::geom_spatvector(data = states, fill = NA, col = "white") + 
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), legend.text = element_text(size=10,  family = "Palatino"),
        legend.title = element_blank(), legend.box="horizontal",  legend.position="bottom",
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.grid.major = element_blank(), rect = element_blank()) ->> a

ggsave(a, filename = paste0("/Users/pedrotremacoldirossi/Documents/", "am", ".pdf"), device = cairo_pdf, dpi = 2000,
       width = 7, # The width of the plot in inches
       height = 7, units = "in")

# value = 1: forest // value = 2: deforested // value = 3: reforested // value = 0: others
file_name <- list.files(path="/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "plot_legacy_forest")]
file_name <- file_name[str_detect(file_name, "2020")]
r <- rast(paste0("/Users/pedrotremacoldirossi/Library/CloudStorage/Dropbox-Personal/amazonLandPrices_project/data/processing/transitions_combined/", file_name[1]))
r <- as.factor(r)

ggplot() +
  tidyterra::geom_spatraster(data = r, aes(fill = layer), maxcell=600000) +
  tidyterra::scale_fill_whitebox_d(
    palette = "bl_yl_rd", na.translate = T) + theme_minimal() + #labs(title = "MAP") + 
  scale_fill_manual(values = colors, labels = labels, na.translate = F, na.value = "white") + 
  tidyterra::geom_spatvector(data = amazon_bioma, fill = NA, col = "black") + tidyterra::geom_spatvector(data = states, fill = NA, col = "white") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size=10,  family = "Palatino"),
        legend.title = element_blank(), legend.box="horizontal",  legend.position="bottom",
        legend.box.background = element_rect(colour = "black"),
        panel.background = element_blank(), panel.grid.major = element_blank(), 
        rect = element_blank()) ->> a

ggsave(a, filename = paste0("/Users/pedrotremacoldirossi/Documents/", "am4", ".pdf"), device = cairo_pdf, dpi = 2000,
       width = 7, # The width of the plot in inches
       height = 7, units = "in")


a

ggsave(a, filename = paste0("/Users/pedrotremacoldirossi/Documents/", "am", ".pdf"), device = cairo_pdf, dpi = 500,
       width = 7, # The width of the plot in inches
       height = 7, units = "in")


theme(axis.title.x=element_text(size=20, colour="black", family = "Palatino"), 
      title = element_text(size = 20), axis.text=element_text(size = 20, colour = "black", family = "Palatino"), 
      axis.title.y=element_text(size=20, colour="black",  family = "Palatino"), text = element_text(family = "Palatino"), legend.box="vertical", 
      legend.margin=margin(), panel.grid.major = element_blank(), legend.text = element_text(size=20),  legend.position="none")



  geom_sf(amazon_bioma, aes(col = )) + tm_borders(col = "black", lwd = 1) +
  theme(text = element_text(family = "Palatino", size = 25),
        panel.grid.major = element_blank(), legend.position = "none", panel.grid.minor = element_blank(),
        legend.title = element_blank(), legend.spacing.y = unit(0, "mm"), 
        legend.background = element_blank(), legend.key.size = unit(2,"line"),
        legend.box.background = element_rect(colour = "black"))  

  
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino",
            legend.bg.color = "white",
            #legend.digits = 5,
            legend.bg.alpha = 1, legend.show = T)
  #tm_borders(col = "black", lwd = 1) +
  geom_spatvector(data = a, fill = NA) +
  tm_add_legend(c("fill"), col = c("white", "#02401B", "#F1BB7B", "#0B775E"),
                border.col = "grey40", #size = bubble_sizes,
                labels = c('Indigenous', "Conservation", "Undesignated")) +
  #tm_shape(indigenous) + tm_fill(col = "#02401B", alpha = 0.4, border.col = "white", legend.show = T) + 
  #tm_shape(conservation) + tm_fill(col = "#0B775E", alpha = 0.4, border.col = "white", legend.show = T) + 
  #tm_shape(fpnd_bioma1) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "white", legend.show = T) +
  #tm_shape(fpnd_bioma2) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "white", legend.show = F) +
  #tm_shape(fpnd_bioma3) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "white", legend.show = F) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino",
            legend.bg.color = "white",
            #legend.digits = 5,
            legend.bg.alpha = 1, legend.show = T)




amazon_bioma <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/aux/amazon_biome_border/amazon_biome_border.shp")
amazon_bioma <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
#a <- terra::vect(amazon_bioma)
terra::plot(amazon_bioma, border="black", add = TRUE, col = NA, lwd = 1.5)
####################################                    ####################################                      ####################################
####################################                    ####################################                      ####################################
####################################                    ####################################                      ####################################






# #---combine pixels [1987-2020]---# 
# registerDoParallel(cores = 15)
# i.run <- function(k){
# #for(k in file_name){  
#   a <- readRDS(paste0("/Volumes/ElementsMain/transitions/grid_", k, "_", "1987", "_transitions.rds"))
#   names(a)[3] <- paste0("cover_", "1987")
#   a <- as.data.table(a)
#   
#   for(y in years){
#   b <- readRDS(paste0("/Volumes/ElementsMain/transitions/grid_", k, "_", y, "_transitions.rds"))
#   names(b)[3] <- paste0("cover_", y)
#   #b <- as.data.table(b)
#   a <- a[b, on = .(x, y), nomatch = 0L]
#   #print(y)
#   }
#   b <- readRDS(paste0("~/Downloads/legacy/grid_", k, "_legacy.rds"))
#   names(b)[3] <- "cover_legacy"
#   a <- a[b, on = .(x, y), nomatch = 0L]
#   
#   saveRDS(a, file = paste0("/Volumes/ElementsMain/transitions_combined/grid_", k, "_combined.rds"))
#   #print(k)
# }
# pdfs <- foreach(c=1:100) %dopar% i.run(file_name[c])



file_name <- list.files(path="~/Downloads/legacy/", pattern="*.tif", recursive = TRUE)
file_name <- file_name[str_detect(file_name, "plot")]
r <- rast(paste0("~/Downloads/legacy/grid_-68_-67_-12_-11_legacy.tif"))
plot(r)




terra::plot(amazon_bioma, border="blue", col = NA, lwd = 2)
terra::plot(final_raster, box = FALSE, legend = FALSE, add = TRUE)

# Load the raster file and convert it
raster_file <- raster("path_to_your_raster_file.tif")
raster_stars <- st_as_stars(r)

# Create the base plot
ggplot() +
  # Add the raster layer
  geom_stars(data = raster_stars) +
  # Add the shapefile layer
  geom_sf(data = amazon_bioma, color = "blue", fill = NA) +
  # Customizations
  labs(title = "Raster and Shapefile Overlay", 
       x = "Longitude", y = "Latitude") +
  theme_minimal()


r <- aggregate(r, fact = 4, fun = "max")
plot(r)

agg_r <- aggregate(r, fact=2, fun=mean)
par(mfrow=c(1,2))
plot(r, main='Original Raster')
plot(agg_r, main='Aggregated Raster')

r_layer <- raster(r)
polys <- rasterToPolygons(r_layer, dissolve=FALSE)
# Dissolve adjacent polygons with the same value
dissolved <- gUnaryUnion(polys, id=polys@data[,1])
# Convert dissolved polygons back to raster
result_raster <- rasterize(dissolved, r, field=names(dissolved@data)[1])

# Plotting
par(mfrow=c(2,2))
plot(r, main='Original Raster')
plot(agg_r, main='Processed Raster')
plot(result_raster, main='Processed Raster')

#make categorical
r2 <- terra::as.factor(r)
r <- as(r, "SpatialGridDataFrame")
levelplot(r2, col.regions = c("white", "#02401B"), att='layer')

r2 <- terra::as.factor(r)
map_r <- tm_shape(r2, projection = 4326, raster.downsample = F) + tm_raster()

map_r <- gSimplify(methods::as(object = r, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)


amazon_bioma <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/aux/amazon_biome_border/amazon_biome_border.shp")
r_shape <- as.polygons(r2)
r_shape <- sf::st_as_sf(r_shape)

r_shape_comb <- r_shape %>% 
  group_by(layer) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

r_shape_comb <- gSimplify(methods::as(object = r_shape_comb, Class = "Spatial"), tol=0.1, topologyPreserve=TRUE)
tm_shape(r_shape_comb) + tm_borders(col = "black", lwd = 1)



fpnd_bioma <- st_intersection(fpnd, amazon_bioma)
amazon_bioma <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)

tm_shape(r_shape) + tm_borders(col = "black", lwd = 1)
tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) + tm_shape(r_shape) + tm_borders(col = "#02401B", lwd = 1)



m <- tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) +
  tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#0B775E", 0.4), alpha("#81A88D", 0.4)),
                border.col = "grey40", #size = bubble_sizes,
                labels = c('Indigenous', "Conservation", "Undesignated")) +
  #tm_shape(indigenous) + tm_fill(col = "#02401B", alpha = 0.4, border.col = "white", legend.show = T) + 
  #tm_shape(conservation) + tm_fill(col = "#0B775E", alpha = 0.4, border.col = "white", legend.show = T) + 
  #tm_shape(fpnd_bioma1) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "white", legend.show = T) +
  #tm_shape(fpnd_bioma2) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "white", legend.show = F) +
  #tm_shape(fpnd_bioma3) + tm_fill(col = "#81A88D", alpha = 0.4, border.col = "white", legend.show = F) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino",
            legend.bg.color = "white",
            #legend.digits = 5,
            legend.bg.alpha = 1, legend.show = T)


#---break up raster into data.table grids---#
for(year in 1987:2020){
  grey <- rast(paste0("/Users/pedrotremacoldirossi/Downloads/mapbiomas/brasil_coverage_", year, ".tif"))
  
  amazon_sf <-
    #--- get Amazon biome boundary ---
    read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazonLandPrices_project/data/raw/aux/amazon_biome_border/amazon_biome_border.shp") %>%
    #--- sp to sf ---# if needed
    #st_as_sf() %>%
    #--- transform using the CRS of the Mapbiomas raster  ---#
    st_transform(terra::crs(grey))
  #--- crop to Amazon biom portion---#
  grey <- terra::crop(grey, amazon_sf)
  
  #e <- extent(-73.98327, -43.39941, -16.66204, 5.269697)
  
  #--- horizontal grids: 31 [74-43]---#
  #--- vertical grids: 23 [-17-6]---#
  #--- total grids/year: 713---#
  i.run <- function(h){
    for(v in seq(-17, 5)){
      
      if(v < 0){
        vv <- v+0.00009
      } else {
        vv <- v-0.00009
      }
      e <- extent(h+0.00009, h+1, vv, v+1)
      #--- converting to a data.frame ---#
      grey1_df <- as.data.frame(terra::crop(grey, e), xy = TRUE) 
      saveRDS(grey1_df, file = paste0("~/Downloads/grids/grid_", h, "_", h+1, "_", v, "_", v+1, "_", year, ".rds"))
      #print(v)
    }
  }
  
  registerDoParallel(cores = 8)
  pdfs <- foreach(c=-74:-44) %dopar% i.run(c)
  
}
#---generate forest---#
file_name <- list.files(path="~/Downloads/grids/", pattern="*.rds", recursive = TRUE)
file_name <- file_name[!str_detect(file_name, "1985") & !str_detect(file_name, "1986")]
file_name <- file_name[str_detect(file_name, "1991") | str_detect(file_name, "1992")]

i.run <- function(k){
  #for(k in file_name){
  a <- readRDS(paste0("~/Downloads/grids/", k))
  a$x <- as.character(a$x)
  a$y <- as.character(a$y)
  # b$x <- as.character(b$x)
  # b$y <- as.character(b$y)
  # 
  a <- as.data.table(a)
  #b <- as.data.table(b)
  
  #result <- a[b, on = .(x, y), nomatch = 0L]
  a$cover <- ifelse(a$brasil_coverage_1985 %in% c(1, 3, 4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 13) & 
                    a$brasil_coverage_1986 %in% c(1, 3, 4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 13), "forest", "not forest")
  
  a <- a[, .(x, y, cover)]
  
  i <- str_remove(str_remove(k, "grid_"), ".rds$")
  
  saveRDS(a[, .(x, y, cover)], file = paste0("~/Downloads/transitions/grid_", i, "_transitions.rds"))
}
registerDoParallel(cores = 20)
pdfs <- foreach(c=1:length(file_name)) %dopar% i.run(file_name[c])











dfr 

dfr2 <- rasterFromXYZ(r)  #Convert first two columns as lon-lat and third as value                
crs(dfr2) <- CRS('+init=EPSG:4326')
dfr2
plot(dfr2)

s <- terra::sprc(dfr , dfr2)
m <- merge(dfr , dfr2)
plot(m)


min(as.numeric(r$x))

plot(dfr)

g <- terra::crop(grey, c(-64.99994, -64.00011, -3.000014, -1.999919))
plot(g)

#---legacy forest: natural in 1985 and 1986---#
c(1, 3, 4, 5, 6, 49, 10, 11, 12, 32, 29, 50, 13)



#---legacy forest: natural in 1985 and 1986---#
c(14, 15, 18, 19, 39, 20, 40, 62, 41, 36, 46, 47, 35, 48, 9, 21, 24, 30)







#d$x <- as.character(d$x)
#d$y <- as.character(d$y)
#dd <- d %>% filter(str_detect(y, "^3.560")) %>% filter(str_detect(x, "^-62.728"))
#-62.72850
#3.560158


#0.0003 ~ 30 meters




for(h in seq(-74, -44)){
print(h)
for(v in seq(-17, 5)){

if(v < 0){
  vv <- v+0.00009
} else {
  vv <- v-0.00009
}
  e <- extent(h+0.00009, h+1, vv, v+1)
  #--- converting to a data.frame ---#
  grey1_df <- as.data.frame(terra::crop(grey, e), xy = TRUE) # this works with Raster* objects as well
  saveRDS(grey1_df, file = paste0("~/Downloads/grids/grid_", h, "_", h+1, "_", v, "_", v+1, "_2022.rds"))
  print(v)
}
}



grey1 <- terra::crop(grey, e)
#--- converting to a data.frame ---#
grey1_df <- as.data.frame(terra::crop(grey, e), xy = TRUE) # this works with Raster* objects as well


#--- take a look ---#
head(IA_cdl_df)

values <- terra::values(grey)


plot(grey)
grey <- setMinMax(grey)












library(readr)
legend <- read_delim("~/Downloads/Codigos-da-legenda-colecao-8.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(rasterVis)
#define color palette
cpal <- legend$Color
#make categorical
r2 <- terra::as.factor(grey)

levelplot(r2,col.regions=cpal,att='ID')

#For deforestation, the pixel has to persist as Natural at least two years before the change and persist as Anthropic at least one year after the conversion (1987-2019). 
#For reforestation, the pixel has to persist as Anthropic at least two years before and as Natural at least three years after (1987-2018).

library(ggmap)
mapMilan <- get_map(location=c(-56.614457271180804,-3.035321038967455), zoom=19, maptype = "hybrid")
ggmap(mapMilan)

#maptype = c("terrain", "satellite", "roadmap", "hybrid", "toner", "watercolor"),

#Visualize the map
ggmap(mapMilan)
library(mapsapi)

key <- "AIzaSyD93NKlMUghzuzglWSO5siLzYtuND2yP2k"
register_google("AIzaSyD6uPjP2nQCGW3hxvS23Ter6RFyKK9XRgk")

####################################                    ####################################                      ####################################

grey <- terra::rast("/Users/pedrotremacoldirossi/Downloads/brasil_coverage_2022.tif")
plot(grey)

grey <- grey > 30

plot(grey)

extract(grey)

terra::extract(grey, points) 


# calculate and save the min and max values of the raster to the raster object
grey <- setMinMax(grey)

# view raster attributes
grey





rasterToPoints(grey)
imported_raster=raster(grey)
read_file<-readTIFF("/Users/pedrotremacoldirossi/Downloads/brasil_coverage_2022.tif")

plot(grey)
plotRGB(rgb)
