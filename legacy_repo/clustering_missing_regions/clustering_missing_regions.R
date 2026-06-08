# --- here bootstrap (auto-added) ---
suppressPackageStartupMessages({
  if (!requireNamespace("here", quietly = TRUE)) {
    stop("Package 'here' is required. Install it with install.packages('here').")
  }
})

.local_script_path <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grepl("^--file=", args)]
  if (length(file_arg) == 0) {
    NA_character_
  } else {
    normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)
  }
})

.repo_root <- here::here()
if (!is.na(.local_script_path)) {
  # Tomas pipeline scripts were authored relative to code/patricio_preach_tomas_work.
  if (grepl("/code/patricio_preach_tomas_work/code/", .local_script_path, fixed = TRUE)) {
    setwd(file.path(.repo_root, "code", "patricio_preach_tomas_work"))
  } else {
    setwd(.repo_root)
  }
}
rm(.local_script_path, .repo_root)
# --- end here bootstrap ---

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

pasta_saida <- "D:/Dropbox/amazon_project/clustering_missing_regions"

#----- BELÉM ---------

municipios_sf <- st_read("D:/Dropbox/amazon_project/data/input/auxiliary/municipalities_amazon_biome/municipalities_amazon_biome.shp")
         
municipios_selecionados_sf <- municipios_sf %>%
filter(nome %in% c("Belém", "Abaetetuba", "São Miguel do Guamá","Barcarena","Acará","Igarapé-Miri","Ananindeua","Marituba","Benevides","Santa Bárbara do Pará"
                   ,"Santa Izabel do Pará","Castanhal", "Inhangapi", "Santo Antônio do Tauá", "São Domingos do Capim", "Igarapé-Açu", "Santa Maria do Pará", "Irituia", "Concórdia do Pará","Bujaru", "Tomé-Açu"
                   ,"Irituia","Moju","Aurora do Pará","Ourém","Ipixuna do Pará","Tailândia","Bonito"))

plot(st_geometry(municipios_selecionados_sf),
     col = factor(municipios_selecionados_sf$nome))


cluster_belem_sf <- municipios_selecionados_sf %>%
  summarise(
    geometry = st_union(geometry)
      )

st_write(
  obj = cluster_belem_sf,
  dsn = file.path(pasta_saida, "cluster_belem.shp"),
  driver = "ESRI Shapefile", 
  delete_dsn = TRUE
)
#--------------ILHAS

municipios_selecionados_sf <- municipios_sf %>%
  filter(nome %in% c("Afuá","Chaves","Cachoeira do Arari","Anajás","Muaná","Breves","Ponta de Pedras","Santa Cruz do Arari","Soure",
                     "Salvaterra","São Sebastião da Boa Vista","Curralinho"))

plot(st_geometry(municipios_selecionados_sf),
     col = factor(municipios_selecionados_sf$nome))


cluster_ilha_sf <- municipios_selecionados_sf %>%
  summarise(
    geometry = st_union(geometry)
  )

st_write(
  obj = cluster_ilha_sf,
  dsn = file.path(pasta_saida, "cluster_ilha.shp"),
  driver = "ESRI Shapefile", 
  delete_dsn = TRUE
)

#-------- BAIXO AMAZONAS

municipios_selecionados_sf <- municipios_sf %>%
  filter(nome %in% c("Iranduba","Manaus","Itacoatiara","Parintins","Rio Preto da Eva", "Itapiranga", "SãoSebastião do Uatumã","Boa Vista do Ramos",
                     "Barreirinha","Careiro da Várzea","Silves","Urucurituba"))

plot(st_geometry(municipios_selecionados_sf),
     col = factor(municipios_selecionados_sf$nome))


cluster_baixo_amazonas_sf <- municipios_selecionados_sf %>%
  summarise(
    geometry = st_union(geometry)
  )

st_write(
  obj = cluster_baixo_amazonas_sf,
  dsn = file.path(pasta_saida, "cluster_baixo_amazonas.shp"),
  driver = "ESRI Shapefile", 
  delete_dsn = TRUE
)