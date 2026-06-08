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
library(geobr)
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
library(readxl)
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
library(stringi)
options(scipen=999)

#---load Amazon biome boundaries---#  
amazon_bioma <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/auxiliary/amazon_biome_border/amazon_biome_border.shp")

#---requests for land title under Programa Terra Legal---#  
terra_legal <- read_delim("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/terralegal/DadosTerraLegal.csv", delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), trim_ws = TRUE)
terra_legal$municipio <- tolower(stri_trans_general(terra_legal$municipio, "Latin-ASCII"))
terra_legal$nome_requerente <- str_squish(stri_trans_general(terra_legal$nome_requerente, "Latin-ASCII"))
terra_legal$first_name <- tolower(stri_trans_general(gsub(" .*", "", terra_legal$nome_requerente), "Latin-ASCII"))
terra_legal$ha_m <- gsub("\\..*", "", as.character(terra_legal$area_do_imovel))
terra_legal$ha_m2 <- str_extract(as.character(terra_legal$area_do_imovel), "([0-9]+)[\\.]*([0-9])")
terra_legal <- terra_legal %>% group_by(nome_requerente, municipio, area_do_imovel) %>% filter(row_number(nome_requerente) == 1)
terra_legal <- terra_legal %>% filter(numero_processo != "00000.000000/0000-00")
terra_legal <- terra_legal %>% group_by(numero_processo) %>% add_count() %>% filter(n == 1) %>% ungroup() %>% dplyr::select(-n)
terra_legal$year_request <- str_extract(gsub(".*\\/", "", terra_legal$numero_processo), "^([0-9])([0-9])([0-9])([0-9])")

#---PLOTS---#
#---Terra Legal aggregates---#
library(ggtext)

terra_legal  %>% group_by(year_request) %>% count() %>% filter(as.numeric(year_request) > 2008 & as.numeric(year_request) < 2019) %>% 
  ggplot() + 
  geom_bar(aes(x = year_request, y = n, colour = "vec2"), fill = "#385D22", alpha = 0.9, stat = "identity", position = "dodge", color = "white", width = 1) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(breaks = seq(0, 30000, 10000), limit = c(0,30000), labels = c("0","10,000","20,000", "")) +
  coord_cartesian(ylim = c(0, 30000), clip = "off") +
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text = element_text(size=35, colour="black"),
    axis.title.x = element_text(size=35, colour="black"),
    axis.title.y = element_text(size=35, colour="black"),
    panel.grid.major = element_blank(),
    legend.position = "none"#,
    #plot.title = element_text(face = "bold")
  )  ->> a

ggsave(a, filename = paste0("/Users/pedrotremacoldirossi/Documents/", "policy1_requests", ".pdf"),
       device = cairo_pdf,
       width = 13, # The width of the plot in inches
       height = 7, units = "in")

terra_legal  %>% group_by(year_request) %>% mutate(n = sum(as.numeric(ha_m))) %>% filter(as.numeric(year_request) > 2008 & as.numeric(year_request) < 2016) %>% 
  filter(row_number(n) == 1) %>%
  ggplot() + 
  geom_bar(aes(x = year_request, y = n, colour = "vec2"), fill = "#385D22", alpha = 0.9, stat = "identity", position = "dodge", color = "white", width = 1) +
  labs(x = NULL, y = NULL) +
  scale_y_continuous(breaks = seq(0, 4500000, 1000000), limit = c(0,4500000), labels = c("0","1,000","2,000","3,000", "4,000")) +
  theme_minimal() + #geom_hline(yintercept=0.62, linetype="solid", color = "blue4", size = 2) +geom_bar(width = .7, stat = "identity", position = "dodge", colour="black") + 
  theme(axis.title.x=element_text(size=35, colour="black", family = "Times New Roman"), 
        title = element_text(size = 35), axis.text=element_text(size = 35, colour = "black", family = "Times New Roman"), 
        axis.title.y=element_text(size=35, colour="black",  family = "Times New Roman"), text = element_text(family = "Times New Roman"), legend.box="vertical", 
        legend.margin=margin(), panel.grid.major = element_blank(), legend.text = element_text(size=35),  legend.position="none") +
  scale_colour_manual("", labels = c("HL Zeros", "HL Positive"), values = alpha(c("black"), 0.8)) ->> b 
  #+ 
  #geom_richtext(aes(x= 3, y=30000, label= "**Amnesty Requests**"), hjust = 1, text.colour = "black",
   #             size=10, col = 'white',fill='white', family = "Palatino") + coord_cartesian(ylim = c(0, 30000), clip = "off") ->> b

ggsave(b, filename = paste0("/Users/pedrotremacoldirossi/Documents/", "policy1_area", ".pdf"),
       device = cairo_pdf,
       width = 13, # The width of the plot in inches
       height = 7, units = "in")



#---Programa Terra Legal shape files--------------------------------------------------------#  
#---CONTAINS SHAPES OF ALL PROPERTIES SEEKING TITLING, NOT ONLY THOSE THROUGH THE PROGRAM---#  
#---THESE ARE LIKELY AUDITED AREAS, SO MANY PROPERTIES SEEKING TITLING WON'T BE HERE YET ---#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/terralegal/shapes/", pattern="*.csv", recursive = TRUE)
tlp_shapes <- data.frame()
for(k in file_name){
  d <- read_csv(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/terralegal/shapes/", k), col_types = cols(codigo_imovel = col_character()))
  d <- data.frame(d)
  tlp_shapes <- rbind.fill(tlp_shapes, d)
  print(k)
}
uf_uf_id <- read_excel("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/auxiliary/uf_uf_id.xlsx")
muni_crosswalk <- read_csv("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/miseEnPlace/Munic_Micro_Meso_Region_Codes.csv", locale = locale(encoding = "ISO-8859-1"))

tlp_shapes <- tlp_shapes %>% left_join(uf_uf_id, "uf_id")
tlp_shapes$nome_requerente <- str_squish(stri_trans_general(tlp_shapes$detentor_nome, "Latin-ASCII"))
tlp_shapes$ha_m <- gsub("\\..*", "", as.character(tlp_shapes$area_ha))

muni_crosswalk$municipio_id <- muni_crosswalk$munic_code
tlp_shapes <- tlp_shapes %>% left_join(muni_crosswalk, "municipio_id")
tlp_shapes$municipio <- tolower(stri_trans_general(tlp_shapes$munic, "Latin-ASCII"))
colnames(tlp_shapes)[1] <- "geometry"

#---TLP applications will be under "particular" and managed by SERFAL (still false +)----#
tlp_shapes <- tlp_shapes %>% filter(natureza == "Particular") %>% filter(orgao_publico == "Serfal")
tlp_shapes <- tlp_shapes %>% filter(!is.na(nome_requerente))

#---perfect matches (full name, uf, xxxtotal area (without decimals))---#  
#terra_legal_shp <- terra_legal %>% merge(tlp_shapes %>% filter(!is.na(nome_requerente)), c("nome_requerente", "uf", "ha_m"))
terra_legal_shp <- terra_legal %>% merge(tlp_shapes %>% filter(!is.na(nome_requerente)), c("nome_requerente", "municipio"))
terra_legal_shp <- st_as_sf(terra_legal_shp, wkt = 'geometry', crs = crs(amazon_bioma))
sf_use_s2(FALSE)
uf <- read_state()
uf <- st_intersection(uf, amazon_bioma)
terra_legal_shp <- st_intersection(terra_legal_shp, amazon_bioma)
#amazon_bioma <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)
#uf <- gSimplify(methods::as(object = uf, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)

terra_legal_shp <- terra_legal_shp %>% group_by(nome_requerente, numero_processo, art) %>%
  slice_max(area_ha, with_ties = FALSE) %>%
  ungroup()
                             
tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) + 
tm_shape(terra_legal_shp) + tm_borders(col = "#F1BB7B", lwd = .1) + 
tm_shape(uf) + tm_borders(col = "gray", lwd = .3) +
tm_layout(frame = FALSE, legend.title.size = 1,
          legend.text.size = 1, legend.position = c("right","bottom"),
          legend.text.fontfamily="Palatino",
          legend.bg.color = "white",
            #legend.digits = 5,
          legend.bg.alpha = 1, legend.show = T) #+ tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#0B775E", 0.4), alpha("#81A88D", 0.4)),
                                                #               border.col = "grey40", #size = bubble_sizes,
                                                #                labels = c('Indigenous', "Conservation", "Undesignated"))

#### OUTPUT: terra_legal_shp [terra legal requests -> online map shapes]

#---remaining unmatched---#  
terra_legal <- terra_legal %>% filter(!numero_processo %in% terra_legal_shp$numero_processo)

####PIPELINE 1: TERRA LEGAL {NAME, MUNICIPALITY, AREA} -> SNCR {COD IMOVEL} -> SIGEF {POLYGON}
####PIPELINE 2: TERRA LEGAL {NAME, MUNICIPALITY, AREA} -> SNCR {COD IMOVEL} -> SNCI {POLYGON}

####PIPELINE 3: TERRA LEGAL {NAME, MUNICIPALITY, AREA} -> CAFIR {COD IMOVEL} -> SIGEF {POLYGON}
####PIPELINE 4: TERRA LEGAL {NAME, MUNICIPALITY, AREA} -> CAFIR {COD IMOVEL} -> SIGEF {POLYGON}

#---PIPELINE 1---#  

#---load sncr municipal files---#  
#---SNCR DOES NOT HAVE SHAPES, BUT HAS COD IMOVEL TO MERGE TO OTHER DATASETS WITH SHAPES ----#
file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/titles/sncr/", pattern="*.csv", recursive = TRUE)
sncr <- data.frame()
for(k in file_name){
  d <- read_delim(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/titles/sncr/", k), delim = ";", escape_double = FALSE, locale = locale(), trim_ws = TRUE)
  sncr <- rbind(sncr, d)
  print(k)
}

sncr$area_ha <- as.numeric(str_replace(str_remove_all(sncr$`ÁREA TOTAL`, "\\."), ",", "."))
sncr$ha_m <- gsub("\\..*", "", as.character(sncr$area_ha))
sncr$municipio <- tolower(stri_trans_general(sncr$MUNICÍPIO, "Latin-ASCII"))
sncr$first_name <- tolower(stri_trans_general(gsub(" .*", "", sncr$TITULAR), "Latin-ASCII"))
sncr <- sncr %>% filter(!str_detect(`ÁREA TOTAL`, "^\\,"))
sncr$ha_m2 <- str_extract(str_replace(str_remove_all(sncr$`ÁREA TOTAL`, "\\."), ",", "."), "([0-9]+)\\.([0-9])")
sncr$ha_m2 <- ifelse(str_detect(sncr$ha_m2, "\\.0"), gsub("\\..*", "", as.character(sncr$ha_m2)), sncr$ha_m2)

#---all remaining terra legal are in the SNCR---#  
#length(unique(terra_legal$numero_processo))/length(unique(terra_legal$numero_processo))
terra_legal_sncr <- terra_legal %>% merge(sncr, c("first_name", "ha_m2", "municipio")) %>% arrange(numero_processo)
terra_legal_sncr$codigo_imo <- terra_legal_sncr$`CÓDIGO DO IMOVEL`
#---remaining unmatched---#  
terra_legal <- terra_legal %>% filter(!numero_processo %in% terra_legal_sncr$numero_processo)

#---data on private properties in SNCI and Sigef as titled and regular private properties---#
#---Land Management System (Sigef) // Sistema de Gestao Fundiaria---#
#---TERRA LEGAL SHOULD BE IN SIGEF---#
#---BUT IT'S NOT -> IT'S IN THE SNCR AND CAFIR---#
sigef <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/titles/sigef/sigef_br/Sigef\ Brasil.shp") %>% 
  st_transform(., crs = 4674) %>% # Change 4674 crs
  st_make_valid()
#---validate each polygon, drop the ones which aren't valid---#
sigef$valid <- st_is_valid(sigef) %>% as.numeric()
sigef$codigo_imo <- as.numeric(sigef$codigo_imo)

terra_legal_sncr_sigef <- terra_legal_sncr %>% merge(sigef, "codigo_imo")
#---remaining unmatched---# 
terra_legal_sncr <- terra_legal_sncr %>% filter(!numero_processo %in% terra_legal_sncr_sigef$numero_processo)

terra_legal_sncr_sigef <- st_as_sf(terra_legal_sncr_sigef)

tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) + 
  tm_shape(terra_legal_shp) + tm_borders(col = "#F1BB7B", lwd = .1) + 
  tm_shape(terra_legal_sncr_sigef) + tm_borders(col = "#5B1A18", lwd = .1) + 
  tm_shape(uf) + tm_borders(col = "gray", lwd = .3) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino",
            legend.bg.color = "white",
            #legend.digits = 5,
            legend.bg.alpha = 1, legend.show = T) #+ tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#0B775E", 0.4), alpha("#81A88D", 0.4)),
#               border.col = "grey40", #size = bubble_sizes,
#                labels = c('Indigenous', "Conservation", "Undesignated"))

#---PIPELINE 2---#  

#---National Property Certification System (SNCI) // Sistema Nacional de Certificacao de Imoveis---#
snci <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/titles/snci/snci_certificacoes/Imvel\ certificado\ SNCI\ Brasil.shp") %>% 
  st_transform(., crs = 4674) %>% # Change 4674 crs
  st_make_valid()
#---validate each polygon, drop the ones which aren't valid---#
snci$valid <- st_is_valid(snci) %>% as.numeric()
snci$codigo_imo <- as.numeric(snci$cod_imovel)

terra_legal_sncr_snci <- terra_legal_sncr %>% merge(snci, "codigo_imo")

#---remaining unmatched---# 
terra_legal_sncr <- terra_legal_sncr %>% filter(!numero_processo %in% terra_legal_sncr_snci$numero_processo)

terra_legal_sncr_snci <- st_as_sf(terra_legal_sncr_snci)

tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) + 
  tm_shape(terra_legal_shp) + tm_borders(col = "#F1BB7B", lwd = .1) + 
  tm_shape(terra_legal_sncr_sigef) + tm_borders(col = "#5B1A18", lwd = .1) + 
  tm_shape(terra_legal_sncr_snci) + tm_borders(col = "#5B1A18", lwd = .1) + 
  tm_shape(uf) + tm_borders(col = "gray", lwd = .3) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino",
            legend.bg.color = "white",
            #legend.digits = 5,
            legend.bg.alpha = 1, legend.show = T) #+ tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#0B775E", 0.4), alpha("#81A88D", 0.4)),
#               border.col = "grey40", #size = bubble_sizes,
#                labels = c('Indigenous', "Conservation", "Undesignated"))


#---PIPELINE 3---# 
tlp_shapes_unmatched <- tlp_shapes %>% filter(!id %in% terra_legal_shp$id)
tlp_shapes_unmatched$area_do_imovel <- as.character(tlp_shapes_unmatched$area_ha)
terra_legal$area_do_imovel <- as.character(terra_legal$area_do_imovel)

terra_legal_shp2 <- terra_legal %>% merge(tlp_shapes_unmatched, c("area_do_imovel", "municipio"))

terra_legal_shp2 <- st_as_sf(terra_legal_shp2, wkt = 'geometry', crs = crs(amazon_bioma))

tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) + 
  tm_shape(terra_legal_shp) + tm_borders(col = "#F1BB7B", lwd = .1) + 
  tm_shape(terra_legal_shp2) + tm_borders(col = "#02401B", lwd = .1) + 
  tm_shape(terra_legal_sncr_sigef) + tm_borders(col = "#5B1A18", lwd = .1) + 
  tm_shape(terra_legal_sncr_snci) + tm_borders(col = "#5B1A18", lwd = .1) + 
  tm_shape(uf) + tm_borders(col = "gray", lwd = .3) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino",
            legend.bg.color = "white",
            #legend.digits = 5,
            legend.bg.alpha = 1, legend.show = T) #+ tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#0B775E", 0.4), alpha("#81A88D", 0.4)),
#               border.col = "grey40", #size = bubble_sizes,
#                labels = c('Indigenous', "Conservation", "Undesignated"))



#---remaining unmatched---#  
terra_legal <- terra_legal %>% filter(!numero_processo %in% terra_legal_shp2$numero_processo)
#---UP TO PIPELINE 3, spatial assignment of terra legal applications are basically perfect---# 
#---let's compile those first----------------------------------------------------------------# 
##PUTTING TOGETHER
terra_legal_shp <- rbind.fill(terra_legal_shp, terra_legal_sncr_sigef %>% filter(!codigo_imo %in% terra_legal_shp$codigo_imo))
terra_legal_shp <- rbind.fill(terra_legal_shp, terra_legal_sncr_snci %>% filter(!codigo_imo %in% terra_legal_shp$codigo_imo))
terra_legal_shp <- rbind.fill(terra_legal_shp, terra_legal_shp2 %>% filter(!codigo_imovel %in% terra_legal_shp$codigo_imo))
#---DONE----------------------------------------------------------------# 

#---TAKE UP ANALYSIS--------------------------------------------------------#  
car_eligible <- read_sf("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/intermediate/car_eligible_cleaned.shp")

# Read your shape files
dataset_A <- car_eligible
dataset_B <- terra_legal_shp

dataset_B <- st_sf(dataset_B, sf_column_name = "geometry")
dataset_B <- st_as_sf(dataset_B, wkt = 'geometry', crs = crs(amazon_bioma))
# Ensure CRS alignment explicitly
dataset_B <- st_transform(dataset_B, st_crs(dataset_A))

# Set overlap threshold (modifiable)
overlap_threshold <- 0.9

# Spatial join to filter candidate matches quickly
joined_indices <- st_intersects(dataset_A, dataset_B)

# Function to calculate overlap ratio
calculate_overlap <- function(poly_a, polys_b, threshold) {
  intersection <- st_intersection(poly_a, polys_b)
  if (nrow(intersection) == 0) return(FALSE)
  intersection_area <- sum(st_area(intersection))
  poly_a_area <- st_area(poly_a)
  as.numeric(intersection_area / poly_a_area) >= threshold
}

# Check overlaps and identify matches
matches <- sapply(seq_along(joined_indices), function(i) {
  indices_b <- joined_indices[[i]]
  if (length(indices_b) == 0) return(FALSE)
  calculate_overlap(dataset_A[i, ], dataset_B[indices_b, ], overlap_threshold)
})

# Filter matched polygons
matched_dataset <- dataset_A[matches, ]
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
#---Establish year of occupation going back to 1989---#######
for(year in seq(1989, 2008)){  
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
  write_rds(pdfs, paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/CAR_eligible_defo_", year, ".rds"))
  #----------------------------------##----------------------------------#  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
}

file_name <- list.files(path="/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/", pattern="*.rds", recursive = F)
file_name <- file_name[str_detect(file_name, "CAR_eligible_defo_")]

for(file in file_name[1]){
  eligible <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/", file))
  eligible$area <- as.numeric(eligible$deforested_area_hc)/(as.numeric(eligible$deforestation_rate)/100)
  eligible <- eligible %>% mutate_all(~ifelse(is.nan(.), NA, .)) %>% group_by(COD_IMO)
  eligible <- eligible %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMO) == 1)
  eligible <- eligible %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  names(eligible)[2:3] <- paste0(names(eligible)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
}  
for(file in file_name[2:20]){
  data <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/", file))
  data <- data %>% group_by(COD_IMO, deforestation_rate, deforested_area_hc) %>% filter(row_number(COD_IMO) == 1)
  names(data)[2:3] <- paste0(names(data)[2:3], "_", str_extract(file, "([0-9])([0-9])([0-9])([0-9])"))
  data <- data %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  eligible <- dplyr::left_join(eligible, data, "COD_IMO")
  print(file)
}  

for(file in file_name[1]){
  eligible_long <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/", file))
  eligible_long$area <- as.numeric(eligible_long$deforested_area_hc)/(as.numeric(eligible_long$deforestation_rate)/100)
  eligible_long <- eligible_long %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  eligible_long <- reshape2::melt(eligible_long, id = "COD_IMO")
  eligible_long$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
}  
for(file in file_name[2:20]){
  data <- readRDS(paste0("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/", file))
  data$area <- as.numeric(data$deforested_area_hc)/(as.numeric(data$deforestation_rate)/100)
  data <- data %>% group_by(COD_IMO) %>% add_count() %>% filter(n == 1)
  data <- reshape2::melt(data, id = "COD_IMO")
  data$year <- str_extract(file, "([0-9])([0-9])([0-9])([0-9])")
  eligible_long <- rbind(eligible_long, data)
  print(file)
}  
eligible_long$group <- "eligible"
#---##---##---##---##---##---##---##---##---##---##---##---##
temas_ambientais_update <- read_delim("/Users/pedrotremacoldirossi/Dropbox (Personal)/amazon_project/data/input/sicar/microdata/temas_ambientais_update.csv", delim = ",", escape_double = FALSE, trim_ws = TRUE)
temas_ambientais_update$COD_IMO <- temas_ambientais_update$registro_car
temas_ambientais_update <- temas_ambientais_update %>% group_by(COD_IMO) %>% filter(row_number(COD_IMO) == 1)
eligible_long <- eligible_long %>% left_join(temas_ambientais_update %>% dplyr::select(uf, codigo_ibge, area_do_imovel, area_rural_consolidada, cancelled, COD_IMO), "COD_IMO")

eligible_long$when_occupied <- ifelse(eligible_long$value >= 10 & eligible_long$variable == "deforestation_rate", as.numeric(eligible_long$year), 9090)
eligible_long <- eligible_long %>% ungroup() %>% group_by(COD_IMO) %>% mutate(when_occupied = min(when_occupied, na.rm = T))
eligible_long$when_occupied <- 2008 - eligible_long$when_occupied
eligible_long$defo_rate_2008 <- ifelse(eligible_long$variable == "deforestation_rate" & as.numeric(eligible_long$year)==2008, as.numeric(eligible_long$value), NA)
eligible_long <- eligible_long %>% ungroup() %>% group_by(COD_IMO) %>% mutate(defo_rate_2008 = mean(defo_rate_2008, na.rm = T))

data <- eligible_long %>% filter(variable == "n") %>% filter(year == 2008)
data$applies <- ifelse(data$COD_IMO %in% matched_dataset$COD_IMO, 1, 0)
#---##---##---##---##---##---##---##---##---##---##---##---##
#---##---##---##---##---##---##---##---##---##---##---##---##
# Read your shape files
dataset_A <- car_eligible
dataset_B <- terra_legal_shp %>% filter(status_processo == "Titulado")

dataset_B <- st_sf(dataset_B, sf_column_name = "geometry")
dataset_B <- st_as_sf(dataset_B, wkt = 'geometry', crs = crs(amazon_bioma))
# Ensure CRS alignment explicitly
dataset_B <- st_transform(dataset_B, st_crs(dataset_A))
# Spatial join to filter candidate matches quickly
joined_indices <- st_intersects(dataset_A, dataset_B)
# Check overlaps and identify matches
matches <- sapply(seq_along(joined_indices), function(i) {
  indices_b <- joined_indices[[i]]
  if (length(indices_b) == 0) return(FALSE)
  calculate_overlap(dataset_A[i, ], dataset_B[indices_b, ], overlap_threshold)
})
# Filter matched polygons
matched_dataset2 <- dataset_A[matches, ]

data$receives <- ifelse(data$COD_IMO %in% matched_dataset2$COD_IMO, 1, 0)

write_dta(data,"~/Documents/takeup.dta")






#---PIPELINE 4---# 
muni_crosswalk <- read_csv("~/Downloads/Munic_Micro_Meso_Region_Codes.csv", locale = locale(encoding = "ISO-8859-1"))

 
muni_crosswalk$muni_nome <- tolower(stri_trans_general(muni_crosswalk$munic, "Latin-ASCII"))
terra_legal$muni_nome <- tolower(stri_trans_general(terra_legal$municipio, "Latin-ASCII"))


#---Cafir---#
#---FIRST batch subsets data based on CAFIRs that have a sncr code---#
file_name <- list.files(path="~/Downloads/cafir/", pattern="*.txt", recursive = TRUE)
cafir <- data.frame()
for(k in file_name){
  d <- read_delim(paste0("~/Downloads/cafir/", k), delim = "$", escape_double = FALSE, col_names = FALSE, 
                  locale = locale(encoding = "ISO-8859-1"), 
                  trim_ws = TRUE)
  d <- data.frame(d)
  cafir <- rbind(cafir, d)
  print(k)
}
setDT(cafir)  # Convert 'cafir' to a data.table for faster processing

cafir_w_incra_cod <- cafir[stringr::str_length(stringr::str_squish(substr(cafir[[1]], 1, 30))) == 30]
cafir_w_incra_cod[, nirf_cod := substr(X1, 1, 8)]
cafir_w_incra_cod[, area_hec := as.numeric(gsub("^(.{8})(.)", "\\1.\\2", substr(X1, 9, 17)))]
cafir_w_incra_cod[, incra_cod := substr(X1, 18, 30)]
cafir_w_incra_cod[, X1 := stringi::stri_sub(X1, 31)]
cafir_w_incra_cod[, imovel_nome := sapply(stringr::str_split(X1, "\\s{3,}"), `[`, 1)]
last_20_chars_squished <- stringr::str_squish(substr(cafir_w_incra_cod[[1]], nchar(cafir_w_incra_cod[[1]]) - 20, nchar(cafir_w_incra_cod[[1]])))
cafir_w_incra_cod[, cep := substr(last_20_chars_squished, 1, 8)]
cafir_w_incra_cod[, data_atualizacao_cadastro := substr(last_20_chars_squished, 9, 16)]
cafir_w_incra_cod[, isento := substr(last_20_chars_squished, 17, 19)]
cafir_w_incra_cod[, codigo_sncr := substr(last_20_chars_squished, 20, 20)]
cafir_w_incra_cod[, X1 := stringr::str_trim(stringi::stri_sub(X1, 1, nchar(X1) - 20))]
cafir_w_incra_cod[, uf := sub(".*\\s{3,}(.{2}).*", "\\1", X1)]
cafir_w_incra_cod[, muni_nome := sub(".*\\s{3,}..(.*)", "\\1", X1)]
remove_pattern <- "[\\(\\)\\{\\}\\*\\?\\\\]"
cafir_w_incra_cod[, X1 := stringr::str_remove_all(X1, remove_pattern)]
cafir_w_incra_cod[, imovel_nome := stringr::str_remove_all(imovel_nome, remove_pattern)]
cafir_w_incra_cod[, imovel_nome := stringi::stri_trans_general(imovel_nome, "Latin-ASCII")]
cafir_w_incra_cod[, X1 := stringi::stri_trans_general(X1, "Latin-ASCII")]
cafir_w_incra_cod[, X1 := stringr::str_remove_all(X1, remove_pattern)]
cafir_w_incra_cod[, imovel_nome := stringr::str_remove_all(imovel_nome, remove_pattern)]
cafir_w_incra_cod[, X1 := gsub(paste0("\\b", imovel_nome, "\\b"), "", X1, perl = TRUE), by = 1:nrow(cafir_w_incra_cod)]
cafir_w_incra_cod[, text1 := sapply(strsplit(X1, "\\s{3,}"), function(parts) paste(head(parts, -1), collapse = "   "))]
cafir_w_incra_cod$imovel_address <- str_squish(cafir_w_incra_cod$text1)
cafir_w_incra_cod[, c("X1", "text1") := NULL]

cafir_w_incra_cod$muni_nome <- tolower(stri_trans_general(cafir_w_incra_cod$muni_nome, "Latin-ASCII"))
cafir_w_incra_cod$ha_m2 <- str_extract(as.character(cafir_w_incra_cod$area_hec), "([0-9]+)[\\.]*[([0-9])]*")
cafir_w_incra_cod$codigo_imo <- as.numeric(cafir_w_incra_cod$incra_cod)

terra_legal_cafir_sigef <- terra_legal %>% merge(cafir_w_incra_cod, c("ha_m2", "muni_nome")) %>% 
                           arrange(numero_processo) %>% merge(sigef, "codigo_imo") %>% 
                           group_by(incra_cod, nome_requerente) %>% filter(row_number(incra_cod) == 1)

terra_legal_cafir_sigef <- st_as_sf(terra_legal_cafir_sigef)

tm_shape(amazon_bioma) + tm_borders(col = "black", lwd = 1) + 
  tm_shape(terra_legal_shp) + tm_borders(col = "#F1BB7B", lwd = .1) + 
  tm_shape(terra_legal_shp2) + tm_borders(col = "#02401B", lwd = .1) + 
  tm_shape(terra_legal_cafir_sigef) + tm_borders(col = "#02401B", lwd = .1) + 
  tm_shape(terra_legal_sncr_sigef) + tm_borders(col = "#5B1A18", lwd = .1) + 
  tm_shape(terra_legal_sncr_snci) + tm_borders(col = "#5B1A18", lwd = .1) + 
  tm_shape(uf) + tm_borders(col = "gray", lwd = .3) +
  tm_layout(frame = FALSE, legend.title.size = 1,
            legend.text.size = 1, legend.position = c("right","bottom"),
            legend.text.fontfamily="Palatino",
            legend.bg.color = "white",
            #legend.digits = 5,
            legend.bg.alpha = 1, legend.show = T) #+ tm_add_legend(c("fill"), col = c(alpha("#02401B", 0.4), alpha("#0B775E", 0.4), alpha("#81A88D", 0.4)),
#               border.col = "grey40", #size = bubble_sizes,
#                labels = c('Indigenous', "Conservation", "Undesignated"))



#---remaining unmatched---#  
terra_legal <- terra_legal %>% filter(!numero_processo %in% terra_legal_cafir_sigef$numero_processo)


terra_legal_ro <- terra_legal %>% filter(uf == "RO")








terra_legal_titulado <- terra_legal %>% filter(status_processo == "Titulado")




terra_legal_sncr %>% filter(codigo_imo %in% as.numeric(cafir_w_incra_cod$incra_cod))





cafir_in_sigef <- cafir_w_incra_cod %>% filter(incra_cod %in% sigef$codigo_imo)


nchar(cafir_w_incra_cod$incra_cod)
nchar(sigef$codigo_imo)


 
sf_use_s2(FALSE)

snci <- st_intersection(snci, amazon_bioma)

snci_in_cafir <- snci %>% filter(cod_imovel %in% cafir_w_incra_cod$incra_cod)
#---length(snci_in_cafir$num_proces)/length(snci$num_proces): 0.67%


shape_names <- unique(tlp_shapes %>% filter(!is.na(detentor_nome)) %>% .$detentor_nome)

#---terra legal registry with boundaries matched---#  
t <- terra_legal %>% filter(nome_requerente %in% shape_names) %>% mutate(match = 1) %>% rbind(terra_legal %>% filter(!nome_requerente %in% shape_names) %>% mutate(match = 0))

tlp_shapes %>% filter(!is.na(detentor_nome)) %>% filter(detentor_nome %in% unique(terra_legal$nome_requerente))

t3 <- tlp_shapes %>% filter(is.na(detentor_nome))

t2 <- tlp_shapes %>% filter(!is.na(codigo_imovel))

 
test <- st_as_sf(parcelageo_apAS_WKT, wkt = 'WKT')
 


sncr_example <- read_delim("~/Downloads/Imoveis_1100015.csv", delim = ";", escape_double = FALSE, locale = locale(), trim_ws = TRUE)

JANESCLEIA

c <- cafir %>% filter(str_detect(X1, regex("roalta floresta", ignore_case = T)))


snci <- gSimplify(methods::as(object = snci, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
types <- vapply(sf::st_geometry(snci), function(x) {
  class(x)[2]
}, "")
types
snci1 <- snci[st_geometry_type(snci) == "MULTIPOLYGON", ]
snci2 <- snci[st_geometry_type(snci) == "POLYGON", ]
snci3 <- snci[str_detect(st_geometry_type(snci), "GEOMETRYCOLLECTION"), ]
snci3 <- st_collection_extract(snci3)

snci1 <- gSimplify(methods::as(object = snci1, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
snci2 <- gSimplify(methods::as(object = snci2, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)
snci3 <- gSimplify(methods::as(object = snci3, Class = "Spatial" ), tol=0.01, topologyPreserve=TRUE)




amazon_bioma <- gSimplify(methods::as(object = amazon_bioma, Class = "Spatial"), tol=0.01, topologyPreserve=TRUE)

plot(snci1)
plot(snci2, add = TRUE)
plot(snci3, add = TRUE)
plot(amazon_bioma, add = TRUE)

s <- snci %>% filter(str_detect(num_proces, "56422"))



c <- cafir %>% filter(str_detect(d, "CHACARA SAO CRISTOVAO"))

sigef %>% filter(str_detect(nome_area, "CHACARA SAO CRISTOVAO"))

ss <- sigef %>% filter(str_detect(parcela_co, "a632"))


assentamento <- read_sf("~/Downloads/Assentamento Brasil/Assentamento Brasil.shp") #%>% 
  #st_transform(., crs = 4674) %>% # Change 4674 crs
  #st_make_valid()



parcelageo_apAS_WKT <- read_csv("~/Downloads/parcelageo_apAS_WKT.csv", col_types = cols(codigo_imovel = col_character()))

crs(snci$geometry)
test <- st_as_sf(parcelageo_apAS_WKT, wkt = 'WKT')

plot.locationsSp_HARV <- SpatialPointsDataFrame(parcelageo_apAS_WKT$WKT,
                                                plot.locations_HARV,    #the R object to convert
                                                proj4string = CRS(snci$geometry))   # assign a CRS 


plot(test$WKT)


