setwd(dir_wd)

# section 8: create output datatable ----------------
if(fp$OUTPUT_DATA){
  
  
  setwd(dir_wd) 
  
  # section 8.1: load relevant data-sets -----
  # section 8.1.1: load municipal CAR variables 
  sicar_vars_exp <- fread(paste0(dir_wd, "data/cleaned/sicar_overlap_variables_exapanded_s2.csv")) %>% copy() %>% 
    # <- fread(paste0(dir_wd, "data/cleaned/sicar_overlap_variables.csv"))   %>% copy() %>% 
    # .[, car_area_notcancelled := NULL ] %>% 
    .[, municipio := as.numeric(municipio)] %>% 
    .[year!="FULL"] %>% 
    .[, year := as.numeric(year)]
  
  # section 8.1.2: create municipal overlap variables for CONFLICTING cars
  car_vars_ordered_out <- fread( paste0(dir_wd, "data/cleaned/CAR_overlap_variables_reftarget_areas.csv")) %>% 
    .[, muni := substr(carid_reference, 4, 10)]
  
  # double check
  nrow(car_vars_ordered_out[year(data_inscricao_reference)>=year(data_inscricao_target)])==nrow(car_vars_ordered_out)
  
  # section 8.1.2: create municipal overlap variables for all cars 
  # NOTICE: ( this data-set is an unorganized version of 8.1.2 with duplicates from many versions): HANDLE WITH CARE
  car_vars <- fread( paste0(dir_wd, "data/cleaned/CAR_overlap_variables_final.csv")) %>% 
    .[, muni := substr(COD_IMOVEL, 4, 10)] 
  
  # section 8.2: construct variables ------
  
  # section 8.2.1:  number of conflicts with overlaps by bucket of intersection per year =-----------
  conflict_buckets <- car_vars_ordered_out %>% copy() %>% 
    # exclude self intersections 
    .[carid_target!=carid_reference] %>% 
    # drop duplicate intersections 
    .[order(cleaning_method)] %>%  
    .[, GRP:=.GRP, .(carid_reference,carid_target)] %>% 
    .[!duplicated(GRP)] %>% 
    # compute intersection area as share of reference CAR area
    .[, int_perc := 100*(int_area / int_area_reference)] %>% 
    # cnstruct the buckets
    .[, bucket := 33] %>% 
    .[int_perc>33, bucket := 66] %>% 
    .[int_perc>66, bucket := 99] %>% 
    .[int_perc==100, bucket := 100] %>% 
    # get reference CAR year 
    .[, year:=year(data_inscricao_reference )] %>% 
    # number of CARs per bucket-muni-reference year
    .[,  .N, .(bucket, muni, year) ] %>% 
    # wide on buckets 
    dcast.data.table(formula = muni+year~bucket, value.var = "N") %>% 
    # fill missings 
    setnafill(cols=c("33", "66", "99", "100"), fill=0) %>% 
    # rename 
    rename_columns(c("33", "66", "99", "100", "muni"), c("n_ovarlaps_33", "n_ovarlaps_66", "n_ovarlaps_99", "n_ovarlaps_100", "municipio")) %>% 
    .[, n_overlaps_sum_buckets := n_ovarlaps_33 + n_ovarlaps_66 + n_ovarlaps_99+ n_ovarlaps_100 ] %>% 
    setnafill(cols=c("n_overlaps_sum_buckets"), fill=0) %>% 
    .[, municipio:=as.numeric(municipio) ] %>% 
    .[, year:=as.numeric(year) ]
  
  # cod_imovel MISSING DATA "c('RO-1100296-E1BD940799EA4DF4A996C34FBF080AD8', 'RO-1100320-1780631FEDE94132A599CADD8500D0A7', 'AM-1301852-8384B042D51A42BCB066DD39C2395FD5', 'AM-1303569-A2E56857E867463D8A273E722FA0C275', 'TO-1707207-1126B3288DEB4A7A9CEF14B866710EDD', 'TO-1707207-92A0CC1EB892497AB3001B44887AA55E', 'TO-1711100-64FFA12AE567400DB8F5FA0164C760A8', 'MA-2108108-72F0720E390449A8BCB1980B00624873', 'MA-2110005-B3399C25C1D54A399CD0AA2680F9704A', 'MA-2113009-F9AF6856E1FE4052831A09F919FF7C03', 'MT-5100250-AE8AE3EC4869421A9E82E2A706118EEA', 'MT-5101852-D9B4E495899A4A759A51F1853A557879', 'MT-5102793-E5EBF717FD8C484F8458E162DA3FB9CB', 'MT-5105002-E8A01D3EB1964658BF63D3621845452D', 'MT-5105176-73F74251DB0A4EE5973CF518B80D666E')"
  
  # section 8.2.2: unique CARs by muni & year ----------
  n_cars <- car_vars %>% copy() %>%  
    # get all of the CARs (with dates) in one column with their year of entry ### CHECK TO SEE IF WE PICK UP THOSE WITHOUT SELF INTERSECTION 
    .[, .(COD_IMOVEL.1,data_inscricao.1)] %>% 
    rename_columns(c("COD_IMOVEL.1", "data_inscricao.1"), c("COD_IMOVEL", "data_inscricao")) %>% 
    rbind(car_vars[, .(COD_IMOVEL,data_inscricao)], .) %>% 
    # drop duplicated cars 
    .[!duplicated(COD_IMOVEL)] %>% 
    .[, year := year(data_inscricao)] %>% 
    .[, muni := substr(COD_IMOVEL, 4, 10)] %>% 
    .[, .(COD_IMOVEL, year, muni)] %>% 
    .[, one  := 1] %>% 
    .[!is.na(year)] %>% 
    .[year<2023] %>% 
    # wide year
    dcast.data.table(formula = COD_IMOVEL+muni~year, value.var = "one") %>% 
    setnafill(x = ., cols =paste0(c(2014:2022)), fill=0) %>% 
    # long with zeros rather than no obs
    melt.data.table(id.vars = c("COD_IMOVEL", "muni")) %>% 
    .[, year := as.integer(as.character(variable))] %>% 
    # actual year the CAR was created
    .[, actual_year := max(year*value), COD_IMOVEL] %>% 
    # if the current year is equal to or greater than the actual year, register a 1
    .[, indic := 0] %>% 
    .[actual_year<=year, indic := 1] %>% 
    # drop duplicate years for good measure
    .[, GRP:= .GRP, .(year, COD_IMOVEL)] %>% .[!duplicated(GRP)] %>% .[, GRP:=NULL ] %>% 
    # sum up the indicators per year
    .[, sum(indic), .(muni, year)] %>%
    # final wrangling
    rename_columns(c("V1", "muni"), c("n_unique_cars", "municipio")) %>% 
    .[, municipio:=as.numeric(municipio) ] %>% 
    .[, year:=as.numeric(year) ]
  
  # section 8.2.3:  unique CARs by muni & year WITH AN INTERSECTION
  cars_intersection <- car_vars_ordered_out %>% copy() %>%
    # only keep actual intersections 
    .[carid_target != carid_reference] %>%
    # get the reference& target cars and years 
    .[, .(
      carid_reference,
      carid_target,
      year_reference = year(data_inscricao_reference),
      year_target = year(data_inscricao_target)
    )]
  
  
  YEAR <- 2015
  for(YEAR in c(2014:2022)){
    
    message_with_lines(YEAR)
    
    conflicting_cars_this_or_past_years <- cars_intersection %>% copy() %>%
      # only select conflicts that exist this year
      .[ (year_reference<=YEAR & year_target<=YEAR)] %>% 
      .[, .(carid_reference, carid_target )]
    
    tmp <- conflicting_cars_this_or_past_years %>% copy() %>% 
      .[, index:=1:.N] %>% 
      # create a column with all CARs, regardless of target/reference
      melt.data.table(id.vars = "index") %>% 
      # only keep unique CARs 
      .[, .(unique(value))] %>% 
      .[, muni := substr(V1, 4, 10)] %>% 
      .[, year := YEAR] %>% 
      # number of unique CARs that have at least one conflict 
      .[, .N, .(muni, year)]
    
    if(YEAR==2014){
      
      n_unique_conflicting_cars <- tmp %>% copy() 
      
    }else{
      
      n_unique_conflicting_cars <- tmp %>% copy() %>% rbind(      n_unique_conflicting_cars, .)
      
    }
  }
  
  
  n_unique_conflicting_cars %<>% 
    rename_columns(c("N", "muni"), c("n_unique_conflicting_cars", "municipio")) %>% 
    .[, municipio:=as.numeric(municipio) ] %>% 
    .[, year:=as.numeric(year) ]
  
  # section 8.3:  consolidate output ------
  
  out <- 
    merge(conflict_buckets, n_cars, c("year", "municipio"), all=T) %>%
    merge(., n_unique_conflicting_cars, c("year", "municipio"), all=T) %>%
    merge(sicar_vars_exp, c("year", "municipio"), all=T) %>% 
    .[!is.na(year)] %>%  
    setnafill(cols = c("n_new_CARs_microdata", "n_overlaps_sum_buckets",  "n_ovarlaps_33", "n_ovarlaps_66", "n_ovarlaps_99", "n_ovarlaps_100", "n_unique_cars", "n_unique_conflicting_cars"), fill=0)       %>% 
    .[year<2023] %>% 
    setnafill(x = ., fill=0, 
              cols = c("car_union_area", 
                       "car_area_intersect_indi",
                       "car_area_intersect_conserve",
                       "car_area_intersect_forestA",
                       "car_area_intersect_forestB",
                       "car_area_intersect_forestC",
                       "car_area_cancelled", 
                       "car_area_notcancelled", 
                       "car_area_ca_notca_intersections", 
                       "muni_area", 
                       "intersect_forestAB", 
                       "intersect_forestAC",
                       "intersect_forestBC",
                       "intersect_forestALL",
                       "car_union_area_in_muni") 
    ) %>% 
    .[order(municipio, year)]
  
  out %>%   
    .[municipio %in% municipalities_amazon] %>% 
    fwrite(x = ., file = paste0(dir_wd, "data/cleaned/municipal_level/muni_year_intersections.csv"))
  
  out %>% 
    names() %>% 
    return_in_vector_format()
  
  variable_desc <- c(
    'year' = "Year",
    'municipio' = "IBGE 7-digit Municipality",
    'n_ovarlaps_33' = "Number of unique (AB=BA) new overlaps/conflicts which intersect with at most 33% of a reference CAR area.",
    'n_ovarlaps_66' = "Number of unique (AB=BA) new overlaps/conflicts which intersect with (33%, 66%] of a reference CAR area.",
    'n_ovarlaps_99' = "Number of unique (AB=BA) new overlaps/conflicts which intersect with (66%, 100%)  of a reference CAR area.",
    'n_ovarlaps_100'= "Number of unique (AB=BA) new overlaps/conflicts which intersect with 100%  of a reference CAR area.",
    'n_ovarlaps_sum_buckets'= "Summation of n_ovarlaps variables for all buckets by municipality, year",
    'n_new_CARs_microdata'= "Number of new unique reference CARs present in the municipality this year. Includes CARs with any status (e.g. active, cancelled, etc). Sourced from the micro-data.",
    'n_CARs_microdata'= "Number of unique CARs present in the municipality this year. Includes CARs with any status (e.g. active, cancelled, etc). Sourced from the micro-data.",
    'n_unique_cars'= "Number of unique CARs present in the municipality each year. Includes CARs with any status (e.g. active, cancelled, etc). [Technical note: Should contain the CARs which did not self-intersect.]",
    'n_unique_conflicting_cars'= "Number of unique CARs present in the municipality this year which have at least one conflict by 2022-12-31. Includes CARs with any status (e.g. active, cancelled, etc). [Technical note: Does not contain the CAR self-intersections. CARs which were not successfully self-intersected (<1% of sample) may have conflict which we cannot measure.)]",
    # 'n_unique_cars_ever'= "Number of unique CARs present in the municipality by 2022-12-31. Includes CARs with any status (e.g. active, cancelled, etc). Should not contain the CARs which did not self-intersect.",
    # 'n_unique_conflicting_cars_ever'= "Number of unique CARs present in the municipality this year which have at least one conflict by 2022-12-31. Includes CARs with any status (e.g. active, cancelled, etc). Does not contain the CAR self-intersections.",
    'car_union_area' = "Area (m2) of 'Union of the of all CARs in the municipality' this year.",
    'car_union_area_in_muni' = "Area (m2) of 'Union of the of all CARs in the municipality', intersected with municipal boundires, this year. [Technical note: car_union_area_in_muni should equal car_union_area, unless CARs extend beyond municipal boundries.]",
    'car_area_intersect_indi'= "Area (m2) of the intersection between the 'Union of the of all CARs in the municipality' and indigenous land this year.",
    'car_area_intersect_conserve'= "Area (m2) of the intersection between the 'Union of the of all CARs in the municipality' and conservation land this year.",
    'car_area_intersect_forestA'= "Area (m2) of the intersection between the 'Union of the of all CARs in the municipality' and forested land (type A) this year.",
    'car_area_intersect_forestB'= "Area (m2) of the intersection between the 'Union of the of all CARs in the municipality' and forested land (type B) this year.",
    'car_area_intersect_forestC'= "Area (m2) of the intersection between the 'Union of the of all CARs in the municipality' and forested land (type C) this year.",
    'intersect_forestALL'= "Area (m2) of the union of forested lands assigned to each municipality, year. [Technical note: Variable created to test for overlaps between forested regions and overlap with municipal boundries.]",
    'intersect_forestAB'= "Area (m2) of the union of forested lands (type = A and type = B) assigned to each municipality, year. [Technical note: Variable created to test for overlaps between different forest types and overlap with municipal boundries.]",
    'intersect_forestAC'= "Area (m2) of the union of forested lands (type = A and type = C) assigned to each municipality, year. [Technical note: Variable created to test for overlaps between different forest types and overlap with municipal boundries.]",
    'intersect_forestBC'= "Area (m2) of the union of forested lands (type = B and type = C) assigned to each municipality, year. [Technical note: Variable created to test for overlaps between different forest types and overlap with municipal boundries.]",
    'car_area_cancelled'= "Area (m2) of the 'Union of all cancelled CARs in the municipality' this year.",
    'car_area_notcancelled'= "Area (m2) of the 'Union of all non-cancelled CARs in the municipality' this year.",
    'car_area_ca_notca_intersections'= "Area (m2) of the intersection between the 'Union of all non-cancelled CARs in the municipality' with 'Union of all cancelled CARs in the municipality`, this year.",
    'muni_area' = "(Computed) Area (m2) of the municipality."
  ) %>% as.data.table(keep.rownames = T) %>% 
    rename_columns(c("rn", "."), c("variable", "desc"))
  
  writexl::write_xlsx(list("muni-year data" = out, "variable_desc"=variable_desc), path = "data/cleaned/municipal_level/muni_year_intersections.xlsx")
  
}

# section 8.4: generate documentation  -----------------

if(GENERATE_DOCUMENTATION){
  
  # 1) prep for mapping --------
  library(geobr)
  library(classInt)
  library(ggplot2)
  
  muni <- read_municipality() %>% 
    st_transform(4674) 
  
  states_sf <- geobr::read_state() %>%
    .[which(.$abbrev_state%in%names(states)), ] %>% 
    st_transform(4674)  
  
  
  # section 8.4.1: create mapping functions -------------
  
  plot_main_cloropleths <- function(data, 
                                    dir = "output/generating_documentation/",
                                    variable, 
                                    breaks=c(-1, 0, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000), 
                                    filename,
                                    label=bquote(km^2), 
                                    areakm2=TRUE, 
                                    DPI=300)  {
    
    # data = muni_year_intersections
    # variable = "car_union_area"
    # breaks=c(-1, 0, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000)
    # variable="car_area_intersect_indi_rr"
    # breaks=c(-1, 0, 1, 5, 10, 25, 50, 75, 95, 100, 10000)
    # label=bquote(km^2)
    # areakm2=TRUE
    # dir = "output/generating_documentation/"
    # filename <- "car_union_area_all_map.pdf"
    
    # # 1) prep for mapping --------
    # library(geobr)
    # library(classInt)
    # library(ggplot2)
    # 
    # muni <- read_municipality() %>% 
    #   st_transform(4674) 
    # 
    # states_sf <- geobr::read_state() %>%
    #   .[which(.$abbrev_state%in%names(states)), ] %>% 
    #   st_transform(4674)  
    
    # 2) amount of CAR claimed land --------
    
    if(areakm2){
      plotting_microdata_output <- data %>% copy() %>% 
        rename_columns(c(variable), c("variable")) %>% 
        .[, VAR := variable/(1000**2) ]  %>% 
        .[, .(municipio,  VAR, year)] 
    }else{
      plotting_microdata_output <- data %>% copy() %>% 
        rename_columns(c(variable), c("variable")) %>% 
        .[, VAR := variable ]  %>% 
        .[, .(municipio,  VAR, year)] 
    }
    
    plotting_microdata_output %<>% 
      dplyr::inner_join(muni, ., by = c("code_muni" = "municipio") ) 
    
    labels <- format_with_commas(breaks[-1])
    plotting_microdata_output$V1_cat <- cut(plotting_microdata_output$VAR, breaks=breaks, labels=labels, include.lowest=TRUE)
    
    p <- plotting_microdata_output %>% 
      ggplot(data=.) +
      geom_sf(aes(fill=V1_cat), color= "grey50", size=.15, na.value="grey20") +
      facet_wrap(~year) + 
      scale_fill_brewer(palette = "RdYlBu", direction=-1, label, na.value="grey20") + 
      theme_minimal() + 
      theme( 
        text = element_text(size = 20), 
        axis.text = element_blank()) + 
      geom_sf(data = states_sf, color = "black", fill = NA)
    
    ggsave(
      plot = p,
      filename = paste0(dir, filename, ".pdf"),
      device = cairo_pdf,
      width = 9,
      height = 9,
      scale = 1, 
      dpi = DPI
    )
    
    ggsave(
      plot = p,
      filename = paste0(dir, filename, ".jpeg"),
      device = jpeg,
      width = 9,
      height = 9,
      scale = 1, 
      dpi = DPI*2, 
      bg="white"
    )
    
    return(p)
    
  }
  
  # section 8.4A: MUNICIPAL LEVEL DATA -----
  
  muni_year_intersections <- paste0(dir_wd, "data/cleaned/municipal_level/muni_year_intersections.csv") %>% 
    fread() %>% 
    .[, car_area_intersect_forest := car_area_intersect_forestA + car_area_intersect_forestB + car_area_intersect_forestC] %>%   
    .[, car_area_intersect_any := car_area_intersect_forest + car_area_intersect_conserve + car_area_intersect_indi] %>% 
    .[, car_area_intersect_indi_rr := round(100*(car_area_intersect_indi/(car_union_area+1)), 1) ] %>% 
    .[, car_area_intersect_conserve_rr := round(100*(car_area_intersect_conserve/(car_union_area+1)), 1) ] %>% 
    .[, car_area_intersect_any_forest_rr := round(100*(car_area_intersect_forest/(car_union_area+1)), 1) ] %>% 
    .[, car_area_intersect_forestA_rr := round(100*(car_area_intersect_forestA/(car_union_area+1)), 1) ] %>% 
    .[, car_area_intersect_forestB_rr := round(100*(car_area_intersect_forestB/(car_union_area+1)), 1) ] %>% 
    .[, car_area_intersect_forestC_rr := round(100*(car_area_intersect_forestC/(car_union_area+1)), 1) ] %>% 
    .[, car_area_intersect_any_rr := round(100*(car_area_intersect_any/(car_union_area+1)), 1) ] %>% 
    .[, car_area_cancelled_over_noncancelled := round(100*car_area_cancelled/(car_area_notcancelled+1))] %>% 
    # new 
    .[, car_area_ca_notca_intersections_share  := round(100*car_area_ca_notca_intersections  / (car_union_area +1))  ] %>% 
    .[, car_area_over_car_area_in_muni := round(100*car_union_area  / (car_union_area_in_muni+1))  ] %>% 
    .[, car_area_in_muni_share := round(100*car_union_area_in_muni/ (muni_area +1))  ] 
  
  # section 8.4A.1: create main plots -----
  CREATE_MAIN_PLOTS_MUNIYEAR1 <- TRUE # ALREADY RUN
  if(CREATE_MAIN_PLOTS_MUNIYEAR1){
    
    figures_list <- list()
    
    # section 8.4A.2: municipal CAR area -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_union_area", 
                          filename="car_union_area", 
                          breaks=c(-1, 0, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000))
    
    
    figures_list$car_union_area <- data.table(
      variable = c("car_union_area"),
      description = c("This figure presents the area, in square km, of the spatial union of all CARs in each municipal CAR shapefile.
                    The union is taken to account for overlaps, which are quite common. 
                    We include all CARs and each year builds on the previous."), 
      in_folder = c("Yes"))
    
    
    # section 8.4A.3: municipal CAR area - indigenous -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_intersect_indi", 
                          filename="car_area_intersect_indi", 
                          breaks=c(-1, 0, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000))
    
    figures_list$car_area_intersect_indi <- data.table(
      variable = c("car_area_intersect_indi"),
      description = c("This figure presents the area, in square km, of the intersection between indigenous lands and the spatial union of all CARs in each municipal CAR shapefile.
                    The union is taken to account for overlaps, which are quite common. 
                    The union of of all CARs in each municipal CAR shapefile has been shown to be
                    larger than it's intersection with municipal borders, suggesting overlaps with boundries.
                    We include all CARs and each year builds on the previous. 
                    Indigenous land does not vary with time."), 
      in_folder = c("Yes"))
    
    # section 8.4A.4: municipal CAR area - conservation -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_intersect_conserve", 
                          filename="car_area_intersect_conserve",
                          breaks=c(-1, 0, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000))
    
    figures_list$car_area_intersect_conserve <- data.table(
      variable = c("car_area_intersect_conserve"),
      description = c("This figure presents the area, in square km, of the intersection between conservation lands and the spatial union of all CARs in each municipal CAR shapefile.
                    The union is taken to account for overlaps, which are quite common. 
                    The union of of all CARs in each municipal CAR shapefile has been shown to be
                    larger than it's intersection with municipal borders, suggesting overlaps with boundries.
                    We include all CARs and each year builds on the previous. 
                    Conservation land does not vary with time."), 
      in_folder = c("Yes"))
    
    # section 8.4A.5: municipal CAR area - forest A -------------
    
    plot_main_cloropleths(data=., 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_intersect_forestA", 
                          filename="car_area_intersect_forestA", 
                          breaks=c(-1, 0, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000))
    
    figures_list$car_area_intersect_forestA <- data.table(
      variable = c("car_area_intersect_forestA"),
      description = c("This figure presents the area, in square km, of the intersection between forested land (type A) and the spatial union of all CARs in each municipal CAR shapefile.
                    The union is taken to account for overlaps, which are quite common. 
                    The union of of all CARs in each municipal CAR shapefile has been shown to be
                    larger than it's intersection with municipal borders, suggesting overlaps with boundries.
                    We include all CARs and each year builds on the previous. 
                    Forested land does not vary with time."), 
      in_folder = c("Yes"))
    
    # Section 8.4A.6: municipal CAR area - B -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_intersect_forestB", 
                          filename="car_area_intersect_forestB", 
                          breaks=c(-1, 0, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000))
    
    figures_list$car_area_intersect_forestB <- data.table(
      variable = c("car_area_intersect_forestB"),
      description = c("This figure presents the area, in square km, of the intersection between forested land (type B) and the spatial union of all CARs in each municipal CAR shapefile.
                    The union is taken to account for overlaps, which are quite common. 
                    The union of of all CARs in each municipal CAR shapefile has been shown to be
                    larger than it's intersection with municipal borders, suggesting overlaps with boundries.
                    We include all CARs and each year builds on the previous. 
                    Forested land does not vary with time."), 
      in_folder = c("Yes"))
    # Section 8.4A.6: municipal CAR area - C -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_intersect_forestC", 
                          filename="car_area_intersect_forestC", 
                          breaks=c(-1, 0, 100, 250, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000))
    
    figures_list$car_area_intersect_forestC <- data.table(
      variable = c("car_area_intersect_forestC"),
      description = c("This figure presents the area, in square km, of the intersection between forested land (type C) and the spatial union of all CARs in each municipal CAR shapefile.
                    The union is taken to account for overlaps, which are quite common.
                    The union of of all CARs in each municipal CAR shapefile has been shown to be
                    larger than it's intersection with municipal borders, suggesting overlaps with boundries.
                    We include all CARs and each year builds on the previous. 
                    Forested land does not vary with time."), 
      in_folder = c("Yes"))
    
    # Section 8.4A.7: municipal CAR area - all relative rate -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_intersect_any_rr", 
                          filename="car_area_intersect_any_rr", 
                          breaks=c(-1, 0, 1, 5, 10, 25, 50, 75, 95, 100, 10000), 
                          label = "%", areakm2=FALSE)
    
    
    figures_list$car_area_intersect_any_rr <- data.table(
      variable = c("car_area_intersect_any_rr"),
      description = c("This figure presents the relative ratio of: the summation of
    individual intersections between forested, indigenous and conservation lands with the spatial union of all CARs over the
    area of the spatial union of all CARs in each municipal CAR shapefile.
    
    The objective is to understand whether these ever overlap.
    
    The union is taken to account for overlaps, which are quite common. 
    The union of of all CARs in each municipal CAR shapefile has been shown to be 
    larger than it's intersection with municipal borders, suggesting overlaps with boundries.
    We include all CARs and each year builds on the previous. 
    Forested, conservation and indigenous lands may overlap and do not vary with time.
    We add 1 to the denominator to avoid missing data as a funtion of zero-division."), 
      in_folder = c("Yes"))
    
    # Section 8.4A.8: municipal CAR area - indigenous relative rate -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_intersect_indi_rr", 
                          filename="car_area_intersect_indi_rr", 
                          breaks=c(-1, 0, 1, 5, 10, 25, 50, 75, 95, 100, 10000), 
                          label = "%", areakm2=FALSE)
    
    figures_list$car_area_intersect_any_rr <- data.table(
      variable = c("car_area_intersect_any_rr"),
      description = c("This figure presents the relative ratio of the intersection 
      between indigenous lands and spatial union of all CARs over the spatial 
      union of all CARs in each municipal CAR shapefile.
      
      The objective is to understand how much overlap there is at the municipal 
      level between CARs and indigenous land. 
      
      The union is taken to account for overlaps, which are quite common. 
    The union of of all CARs in each municipal CAR shapefile has been shown to be 
    larger than it's intersection with municipal borders, suggesting overlaps with boundries.
    We include all CARs and each year builds on the previous. 
    Forested, conservation and indigenous lands may overlap and do not vary with time.
    We add 1 to the denominator to avoid missing data as a funtion of zero-division."), 
      in_folder = c("Yes"))
    
    # Section 8.4A.9: municipal CAR area - conservation relative rate -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_intersect_conserve_rr", 
                          filename="car_area_intersect_conserve_rr", 
                          breaks=c(-1, 0, 1, 5, 10, 25, 50, 75, 95, 100, 10000), 
                          label = "%", areakm2=FALSE)
    
    figures_list$car_area_intersect_conserve_rr <- data.table(
      variable = c("car_area_intersect_conserve_rr"),
      description = c("This figure presents the relative ratio of the intersection 
      between conservation lands and spatial union of all CARs over the spatial 
      union of all CARs in each municipal CAR shapefile.
      
      The objective is to understand how much overlap there is at the municipal 
      level between CARs and conservation land. 
      
      The union is taken to account for overlaps, which are quite common. 
    The union of of all CARs in each municipal CAR shapefile has been shown to be 
    larger than it's intersection with municipal borders, suggesting overlaps with boundries.
    We include all CARs and each year builds on the previous. 
    Forested, conservation and indigenous lands may overlap and do not vary with time.
    We add 1 to the denominator to avoid missing data as a funtion of zero-division."), 
      in_folder = c("Yes"))
    
    # Section 8.4A.10: municipal CAR area - forest relative rate -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_intersect_any_forest_rr", 
                          filename="car_area_intersect_any_forest_rr", 
                          breaks=c(-1, 0,  1, 5, 10, 25, 50, 95, 100, 10000), 
                          label = "%", areakm2=FALSE)
    
    figures_list$car_area_intersect_any_forest_rr <- data.table(
      variable = c("car_area_intersect_any_forest_rr"),
      description = c("This figure presents the relative ratio of the intersection 
      between forested lands and spatial union of all CARs over the spatial 
      union of all CARs in each municipal CAR shapefile. We sum the individual 
      intersections for each forest type.
      
      The objective is to understand how much overlap there is at the municipal 
      level between CARs and forested land. The figure also suggests whether 
      different forested types overlap themselves. 
      
      The union is taken to account for overlaps, which are quite common. 
    The union of of all CARs in each municipal CAR shapefile has been shown to be 
    larger than it's intersection with municipal borders, suggesting overlaps with boundries.
    We include all CARs and each year builds on the previous. 
    Forested, conservation and indigenous lands may overlap and do not vary with time.
    We add 1 to the denominator to avoid missing data as a funtion of zero-division."), 
      in_folder = c("Yes"))
    
    # Section 8.4A.11: municipal CAR area - forest relative rate -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_intersect_forestA_rr", 
                          filename="car_area_intersect_forestA_rr", 
                          breaks=c(-1, 0,  1, 5, 10, 25, 50, 95, 100, 10000), 
                          label = "%", areakm2=FALSE)
    
    figures_list$car_area_intersect_forestA_rr <- data.table(
      variable = c("car_area_intersect_forestA_rr"),
      description = c("This figure presents the relative ratio of the intersection 
      between forested lands (type A) and spatial union of all CARs over the spatial 
      union of all CARs in each municipal CAR shapefile.
      
      The objective is to understand how much overlap there is at the municipal 
      level between CARs and forested land (type A). 
      
      The union is taken to account for overlaps, which are quite common. 
    The union of of all CARs in each municipal CAR shapefile has been shown to be 
    larger than it's intersection with municipal borders, suggesting overlaps with boundries.
    We include all CARs and each year builds on the previous. 
    Forested, conservation and indigenous lands may overlap and do not vary with time.
    We add 1 to the denominator to avoid missing data as a funtion of zero-division."), 
      in_folder = c("Yes"))
    
    # Section 8.4A.12: municipal CAR area - forest relative rate -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_intersect_forestB_rr", 
                          filename="car_area_intersect_forestB_rr", 
                          breaks=c(-1, 0,  1, 5, 10, 25, 50, 95, 100, 10000), 
                          label = "%", areakm2=FALSE)
    
    figures_list$car_area_intersect_forestB_rr <- data.table(
      variable = c("car_area_intersect_forestB_rr"),
      description = c("This figure presents the relative ratio of the intersection 
      between forested lands (type B) and spatial union of all CARs over the spatial 
      union of all CARs in each municipal CAR shapefile.
      
      The objective is to understand how much overlap there is at the municipal 
      level between CARs and forested land (type C). 
      
      The union is taken to account for overlaps, which are quite common. 
    The union of of all CARs in each municipal CAR shapefile has been shown to be 
    larger than it's intersection with municipal borders, suggesting overlaps with boundries.
    We include all CARs and each year builds on the previous. 
    Forested, conservation and indigenous lands may overlap and do not vary with time.
    We add 1 to the denominator to avoid missing data as a funtion of zero-division."), 
      in_folder = c("Yes"))
    
    
    # Section 8.4A.13: municipal CAR area - forest relative rate -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_intersect_forestC_rr", 
                          filename="car_area_intersect_forestC_rr", 
                          breaks=c(-1, 0,  1, 5, 10, 25, 50, 95, 100, 10000), 
                          label = "%", areakm2=FALSE)
    
    figures_list$car_area_intersect_forestC_rr <- data.table(
      variable = c("car_area_intersect_forestC_rr"),
      description = c("This figure presents the relative ratio of the intersection 
      between forested lands (type C) and spatial union of all CARs over the spatial 
      union of all CARs in each municipal CAR shapefile.
      
      The objective is to understand how much overlap there is at the municipal 
      level between CARs and forested land (type C). 
      
      The union is taken to account for overlaps, which are quite common. 
    The union of of all CARs in each municipal CAR shapefile has been shown to be 
    larger than it's intersection with municipal borders, suggesting overlaps with boundries.
    We include all CARs and each year builds on the previous. 
    Forested, conservation and indigenous lands may overlap and do not vary with time.
    We add 1 to the denominator to avoid missing data as a funtion of zero-division."), 
      in_folder = c("Yes"))
    
    
    # Section 8.4A.14: municipal CAR area - cancelled over non cancelled -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_cancelled_over_noncancelled", 
                          filename="car_area_cancelled_over_noncancelled", 
                          breaks=c(-1, 0,  1, 5, 10, 25, 50, 95, 100, 200), 
                          label = bquote(CA/CA^c), areakm2=FALSE)
    
    figures_list$car_area_cancelled_over_noncancelled <- data.table(
      variable = c("car_area_cancelled_over_noncancelled"),
      description = c("This figure presents the relative ratio of the spatial union 
      of cancelled over non-cancelled CARs over the spatial in each municipal CAR shapefile.
      
      The objective is to understand whether there are any patterns to cancellations 
      and what the rates of cancellation are like when compared to non-cancelled CARs. 
      This figure differs from car_area_ca_notca_intersections_share in that we are comparing sizes, 
      not evaluating overlaps.
      
      The union is taken to account for overlaps, which are quite common. 
      The union of of all CARs in each municipal CAR shapefile has been shown to be 
      larger than it's intersection with municipal borders, suggesting overlaps with boundries.
      We include all CARs and each year builds on the previous. 
      CAR cancellation dates are unknown, we can only observe the state of CAR 
      cancelations as of the date of data collection (Sep 2023).
                      
      We add 1 to the denominator to avoid missing data as a funtion of zero-division."), 
      in_folder = c("Check"))
    
    # Section 8.4A.15: municipal CAR area - (cancelled Intersect non cancelled) over total CAR area -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_ca_notca_intersections_share", 
                          filename="car_area_ca_notca_intersections_share", 
                          breaks=c(-1, 0,  1, 5, 10, 25, 50, 95, 100), 
                          label = "%", areakm2=FALSE)
    
    figures_list$car_area_ca_notca_intersections_share <- data.table(
      variable = c("car_area_ca_notca_intersections_share"),
      description = c("This figure presents the ratio of the intersection between 
      cancelled and non-cancelled CARs to the total area of CARs in the municipality shapefile.
      
      The objective is to understand whether there are any patterns to overlaps between 
      Cancelled and non-cancelled CARs.
      
      The union is taken to account for overlaps, which are quite common. 
      The union of of all CARs in each municipal CAR shapefile has been shown to be 
      larger than it's intersection with municipal borders, suggesting overlaps with boundries.
      We include all CARs and each year builds on the previous. 
      CAR cancellation dates are unknown, we can only observe the state of CAR 
      cancelations as of the date of data collection (Sep 2023).
                      
      We add 1 to the denominator to avoid missing data as a funtion of zero-division."), 
      in_folder = c("No"))
    
    
    # Section 8.4A.16: municipal CAR area - (cancelled Intersect non cancelled) over total CAR area -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_over_car_area_in_muni", 
                          filename="car_area_over_car_area_in_muni", 
                          breaks=c(-1, 0, 95, 100, 110, 125, 150, 200, 500, 100000), 
                          label = "%", areakm2=FALSE)
    
    
    figures_list$car_area_over_car_area_in_muni <- data.table(
      variable = c("car_area_over_car_area_in_muni"),
      description = c("This figure presents the ratio of the union of CARs in the
      municipality shapefile over the union of CARs within the municipalities borders.
      
      The objective is to understand whether CARs extend beyond municipal borders.
      
      The union is taken to account for overlaps, which are quite common. 
      The union of of all CARs in each municipal CAR shapefile has been shown to be 
      larger than it's intersection with municipal borders, suggesting overlaps with boundries.
      We include all CARs and each year builds on the previous. 
      CAR cancellation dates are unknown, we can only observe the state of CAR 
      cancelations as of the date of data collection (Sep 2023).
                      
      We add 1 to the denominator to avoid missing data as a funtion of zero-division."), 
      in_folder = c("Yes"))
    
    # Section 8.4A.17: municipal CAR area - (cancelled Intersect non cancelled) over total CAR area -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="car_area_in_muni_share", 
                          filename="car_area_in_muni_share", 
                          breaks=c(-1, 0, 1, 5, 10, 25, 50, 75, 90, 100), 
                          label = "%", areakm2=FALSE)
    
    
    
    figures_list$car_area_over_car_area_in_muni <- data.table(
      variable = c("car_area_over_car_area_in_muni"),
      description = c("This figure presents the ratio of the area of the union of CARs within 
      the municipalities borders over the area of the municipality.
      
      The objective is to understand how much of each municipality has been claimed over time. 
      
      The union is taken to account for overlaps, which are quite common. 
      We include all CARs and each year builds on the previous. 

      We add 1 to the denominator to avoid missing data as a funtion of zero-division."), 
      in_folder = c("Yes"))
    
    # Section 8.4A.18: municipal area -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="muni_area", 
                          filename="muni_area", 
                          breaks=c(-1, 0, 100, 500, 1000, 2500, 5000, 10000, 25000, 50000, 100000, 500000), 
                          areakm2=TRUE)
    
    figures_list$car_area_over_car_area_in_muni <- data.table(
      variable = c("car_area_over_car_area_in_muni"),
      description = c("This figure presents the area, in square km, of municipalities. 
      
      The objective is to identify any potential issues with the underlying data."), 
      in_folder = c("Yes"))
    
    
  }
  
  # section 8.4A.2: identify remaining issues ----
  
  
  muni_year_intersections[year>2017&muni_area==0]
  
  muni_year_intersections <- paste0(dir_wd, "data/cleaned/municipal_level/muni_year_intersections.csv") %>% 
    fread() %>% 
    .[, bucket_perc33 := round(100*(n_ovarlaps_33/(n_overlaps_sum_buckets+1))) ] %>% 
    .[, bucket_perc66 := round(100*(n_ovarlaps_66/(n_overlaps_sum_buckets+1))) ] %>% 
    .[, bucket_perc99 := round(100*(n_ovarlaps_99/(n_overlaps_sum_buckets+1))) ] %>% 
    .[, bucket_perc100 := round(100*(n_ovarlaps_100/(n_overlaps_sum_buckets+1))) ] %>% 
    .[, n_unique_conflicting_ratio := round(100*(n_unique_conflicting_cars /(n_unique_cars +1))) ]  
  
  # section 8.4B:  CAR LEVEL DATA AGG TO MUNI-YEAR -----
  CREATE_MAIN_PLOTS_MUNIYEAR2 <- TRUE # ALREADY RUN
  if(CREATE_MAIN_PLOTS_MUNIYEAR2){
    
    # define helper fn
    get_angle <- function(slope){
      
      O <- slope
      H <- sqrt(O**2 + 1)
      return(atan(O/H)*180/pi)
    }
    
    # Section 8.4B.1: testing number of unique CARs --------------
    exclude_outlier <- c(1100205, 1504752 )
    text_slope <- summary(lm(n_unique_cars ~n_CARs_microdata, muni_year_intersections[!municipio%in%exclude_outlier]))$coefficients[2,1]
    text_angle <- get_angle(text_slope)
    
    muni_year_intersections %>% 
      ggplot(aes(x=n_unique_cars, y=n_CARs_microdata, label=municipio)) + 
      geom_text(size=2, angle=text_angle) +
      ylab("N. unique CARs (Microdata)") + 
      xlab("N. unique CARs (Shapefiles)")  + 
      theme_minimal() + 
      theme( 
        text = element_text(size = 20), 
        axis.text = element_blank())
    
    ggsave(
      plot = last_plot(),
      filename = paste0(dir_wd, "output/generating_documentation/", "unique_car_text_plot.pdf"),
      device = cairo_pdf,
      width = 12,
      height = 9,
      scale = 1, 
      dpi = 300
    )
    
    # checking information.  
    muni_year_intersections[car_union_area==0&n_CARs_microdata!=0]
    muni_year_intersections[municipio==1100205]
    sicar_vars_exp <- fread(paste0(dir_wd, "data/cleaned/sicar_overlap_variables_exapanded.csv")) %>% copy() 
    sicar_vars_exp[is.na(car_union_area)&n_CARs_microdata!=0]
    paste0("Municipalities: ", 1100205, " and ", 1504752 , " have data on CARs in the micro-data, but no avaiable shapes in the shapefiles.")
    # checked==zero cars this year 
    
    
    # section 8.4B.2: number of unique CARs -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="n_unique_cars", 
                          filename="n_unique_cars_all", 
                          breaks=c(-1, 0, 10, 100, 500, 1000, 2500, 5000, 10000, 20000), 
                          label="Count", 
                          areakm2 = F
    )
    
    # section 8.4B.2: number of unique new CARs -------------
    
    muni_year_intersections[,.N, n_new_CARs_microdata][order(n_new_CARs_microdata)]
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="n_new_CARs_microdata", 
                          filename="n_new_CARs_microdata_all", 
                          breaks=c(-1, 0, 10, 100, 500, 1000, 2500, 5000, 10000), 
                          label="Count", 
                          areakm2 = F
    )
    
    # section 8.4B.3: municipal CAR area -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="n_CARs_microdata", 
                          filename="n_CARs_microdata_all", 
                          breaks=c(-1, 0, 10, 100, 500, 1000, 2500, 5000, 10000, 20000), 
                          label="Count", 
                          areakm2 = F
                          
    )
    
    muni_year_intersections[car_area_notcancelled==0 & car_union_area!=0][, unique(municipio)]
    
    # section 8.4B.3: number of conflicting CARs over number of carts -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="n_unique_conflicting_ratio", 
                          filename="n_unique_conflicting_ratio", 
                          breaks=c(-1, 0,  1, 5, 10, 25, 50, 95, 100), 
                          label="%", 
                          areakm2 = F
                          
    )
    
    # section 8.4B.3: municipal CAR area -------------
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="bucket_perc33", 
                          filename="bucket_perc33", 
                          breaks=c(-1, 0,  1, 5, 10, 25, 50, 95, 100), 
                          label = '%', areakm2 = FALSE)
    
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="bucket_perc66", 
                          filename="bucket_perc66", 
                          breaks=c(-1, 0,  1, 5, 10, 25, 50, 95, 100), 
                          label = '%', areakm2 = FALSE)
    
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="bucket_perc99", 
                          filename="bucket_perc99", 
                          breaks=c(-1, 0,  1, 5, 10, 25, 50, 95, 100), 
                          label = '%', areakm2 = FALSE)
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="bucket_perc100", 
                          filename="bucket_perc100", 
                          breaks=c(-1, 0, 1, 2, 3, 4, 5, 10, 15, 25), 
                          label = '%', areakm2 = FALSE)
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="n_overlaps_sum_buckets", 
                          filename="n_overlaps_sum_buckets", 
                          breaks=c(-1, 0, 10, 100, 500, 1000, 10000, 25000), 
                          label = '%', areakm2 = FALSE)
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="n_unique_conflicting_ratio", 
                          filename="n_unique_conflicting_ratio", 
                          breaks=c(-1, 0,  1, 5, 10, 25, 50, 95, 100), 
                          label="%", 
                          areakm2 = F)
    
    
    plot_main_cloropleths(data=muni_year_intersections, 
                          dir = paste0(dir_wd, "output/generating_documentation/"),
                          variable="n_unique_conflicting_ratio", 
                          filename="n_unique_conflicting_ratio", 
                          breaks=c(-1, 0,  1, 5, 10, 25, 50, 95, 100), 
                          label="%", 
                          areakm2 = F)
    
  }
  
  # section 8.4C: identifying potential issues ------------
  
  fread("data/cleaned/sicar_area_imovel_combined.csv")
  
  overlaps <- fread(paste0(dir_wd, "data/cleaned/CAR_overlap_variables_final.csv"))
  # section 8.4C.1: share of municipalities with self intersections -------------
  
  
  
}


# section 7.5: second round of checks -------------------
if(fp$CHECKS){
  
  # load the ordered and unordered data-sets 
  car_vars <-   fread(
    paste0(dir_wd, "data/cleaned/CAR_overlap_variables.csv"))
  
  car_vars_ordered <- fread(paste0(dir_wd, "data/cleaned/CAR_overlap_variables_reftarget.csv"))
  
  # get the CARs that self intersect 
  self_intersecting <- car_vars_ordered %>% 
    .[carid_reference==carid_target] %>% 
    .[, unique(carid_reference)]
  
  # get the CARs that intersect with others 
  intersecting_others <- car_vars_ordered %>% 
    .[carid_reference!=carid_target] %>% 
    .[, .(carid_reference, carid_target)] %>% 
    .[, index := 1:.N] %>% 
    melt.data.table(data = ., id.vars = "index") %>% 
    .[, unique(value)]
  
  
  #1) what share of self intersecting CARs intersect with others? -----
  
  num <- self_intersecting %>% .[.%in%intersecting_others] %>% uniqueN()
  denom <- self_intersecting %>% uniqueN()
  
  paste0(round((num*100)/denom), "% of self intersecting CARs intersect with others.") %>% message_with_lines()
  
  #2) what share of  CARs which intersect with others are self intersecting? -----
  
  num <- intersecting_others %>% .[.%in%self_intersecting] %>% uniqueN()
  denom <- intersecting_others %>% uniqueN()
  
  paste0(round((num*100)/denom), "% of CARs with conflicts intersect with themselves as well.") %>% message_with_lines()
  
  # 3) which are the CARs which do not self intersect? 
  do_not_self_intersect <- intersecting_others %>% .[!.%in%self_intersecting] 
  
  #4) which municipalities are they from? 
  munis_w_non_self_intersecting <- do_not_self_intersect %>% 
    substr(., 4, 10) %>% 
    unique()
  
  #5) are these municipalities the same ones where we have missing data for the final dataset? 
  
  NEED_TO_RUN_CODE_UNTIL_END_BEFORE_RUNNING_THIS <- T
  if(NEED_TO_RUN_CODE_UNTIL_END_BEFORE_RUNNING_THIS){
    
    muni_year_intersections <- fread("data/cleaned/municipal_level/muni_year_intersections.csv")  
    
    # 5.1) identify municipalities with missing or suspicious data ----
    
    matches_over_non_self_int <- function(x=munis_w_non_self_intersecting, other_munis){
      
      num <- munis_w_non_self_intersecting %>% .[.%in%other_munis] %>% uniqueN()
      denom <- munis_w_non_self_intersecting %>% uniqueN()
      
      paste0(round((num*100)/denom), "% of munis in non-self intersecting data within these munis as well.") %>% message_with_lines()
      
    }
    
    # 5.1.1) CAR Union area = 0
    car_union_area_zero <- muni_year_intersections %>% copy() %>% 
      .[car_union_area==0&n_unique_cars !=0] 
    
    fwrite(car_union_area_zero, "data/processing/identifying_errors/car_union_area_zero.csv")
    
    car_union_area_zero[, .(municipio, year)]
    
    # matches_over_non_self_int(other_munis = car_union_area_zero[, unique(municipio )])
    
    # 5.1.2) non-cancelled CAR Union area = 0
    car_area_notcancelled_zero <- muni_year_intersections %>% copy() %>% 
      .[car_area_notcancelled==0&n_unique_cars!=0]
    
    car_area_notcancelled_zero[, .(municipio, year)][order(municipio, year)]
    
    fwrite(car_area_notcancelled_zero, "data/processing/identifying_errors/car_area_notcancelled_zero.csv")
    
    # matches_over_non_self_int(other_munis = car_area_notcancelled_zero[, unique(municipio )])
    
    # 5.1.3) cancelled CAR Union area = 0
    car_area_cancelled_zero <- muni_year_intersections %>% copy() %>% 
      .[car_area_cancelled==0&n_unique_cars!=0]
    
    fwrite(car_area_cancelled_zero, "data/processing/identifying_errors/car_area_cancelled_zero.csv")
    
    car_area_cancelled_zero[, .(municipio, year)] # a lot of bugs on this front
    
    # matches_over_non_self_int(other_munis = car_area_cancelled_zero[, unique(municipio )])
    
    # 5.1.4) cancelled CAR Union area = MISSING
    car_union_area_missing <- muni_year_intersections %>% copy() %>% 
      .[is.na(car_union_area)]
    # matches_over_non_self_int(other_munis = car_area_cancelled_missing[, unique(municipio )])
    
    car_union_area_missing[year!=2023][, .(municipio, year)] 
    
    fwrite(car_union_area_missing, "data/processing/identifying_errors/car_union_area_missing.csv")
    
    # 5.1.5) non-cancelled CAR Union area MISSING
    car_area_notcancelled_missing <- muni_year_intersections %>% copy() %>% 
      .[is.na(car_area_notcancelled)]
    # matches_over_non_self_int(other_munis = car_area_notcancelled_missing[, unique(municipio )])
    
    fwrite(car_area_notcancelled_missing, "data/processing/identifying_errors/car_area_notcancelled_missing.csv")    
    
    # 5.1.6) cancelled CAR Union area =MISSING
    car_area_cancelled_missing <- muni_year_intersections %>% copy() %>% 
      .[is.na(car_area_cancelled)]
    # matches_over_non_self_int(other_munis = car_area_cancelled_missing[, unique(municipio )])
    
    fwrite(car_area_cancelled_missing, "data/processing/identifying_errors/car_area_cancelled_missing.csv")    
    
    
    # 5.1.8) zero unique cars 
    n_unique_cars_zero <- muni_year_intersections %>% copy() %>% 
      .[n_unique_cars==0] # weird
    
    fwrite(n_unique_cars_zero, "data/processing/identifying_errors/n_unique_cars_zero.csv")    
    
    
    n_unique_cars_zero %>% .[, uniqueN(municipio)]
    n_unique_cars_zero %>% .[, .N, year]
    n_unique_cars_zero %>% .[year<2023 & car_union_area>0, uniqueN(municipio)]
    
    print("There are 14 unique municipalities where the unique number of CARs is zero yet the area of the union of CARs >0.")
    
    # matches_over_non_self_int(other_munis = n_unique_cars_zero[, unique(municipio )])
    
    # 5.1.9) zero n_unique_conflicting_cars 
    n_unique_conflicting_cars_zero <- muni_year_intersections %>% copy() %>% 
      .[n_unique_conflicting_cars ==0]
    # matches_over_non_self_int(other_munis = n_unique_conflicting_cars_zero[, unique(municipio )])
    
    
    fwrite(n_unique_conflicting_cars_zero, "data/processing/identifying_errors/n_unique_conflicting_cars_zero.csv")    
    
    
    
    
    
    
    
    
    # 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  }
  
  
  # 6) -----------------
  
  data <- paste0(dir_wd, "data/cleaned/municipal_level/muni_year_intersections.csv") %>% 
    fread()
  
  
  # check to see which car union areas are zero -----
  data %>% 
    .[car_union_area]
  
  
  # ====================
  # not really; the only thing in common is the n_unique_cars == zero problem
  
  
  # 6) now, lets check out the other issue of missing municipality data s
  
  # 6.1) select municipalities have zero or missing CAR intersection data 
  muni_year_intersections <- fread("data/cleaned/municipal_level/muni_year_intersections.csv")  
  
  # 6.2.1) only consder those with zero
  munis_with_zero_union <- muni_year_intersections %>%
    .[car_union_area==0]
  
  # 6.2.2) how many? 
  munis_with_zero_union %>% 
    .[, uniqueN(municipio)]
  message_with_lines("Only 4 municipalities with 0 car union area")
  
  # 6.2.3) only look at those with 0s
  munis_with_zero_union %>% 
    .[car_union_area==0] 
  message_with_lines("Municipalities with 0 car union area had an issue with the st_area command. Easy to fix.")
  
  # 6.3.1) only look at those with missings
  munis_with_missing_union <- muni_year_intersections %>% 
    .[is.na(car_union_area)]
  
  # 6.3.2) how many? 
  munis_with_missing_union %>% 
    .[, uniqueN(municipio)]
  message_with_lines("Only 18 municipalities with missing car union area")
  
  # 6.3.3) check year distribution 
  munis_with_missing_union %>% 
    .[, .N, year]
  message_with_lines("Municipalities with missing car union area all appear AFTER 2014.")
  
  # 6.3.3) check year distribution 
  munis_with_missing_union %>% 
    .[, .N, year]
  message_with_lines("Municipalities with missing car union area all appear AFTER 2014.")
  
  
  
}