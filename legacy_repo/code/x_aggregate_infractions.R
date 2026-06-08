########################################################
# filename: aggregate_multa_data.R
# author: thiago alckmin april 2024
# desc: aggreagte infractions 
########################################################


# section 0: setup ----------

rm(list = ls())
library(data.table)
library(magrittr)
library(stringi)

source("https://raw.githubusercontent.com/Thiago-Alckmin/helper_functions_scripts/main/helper_functions_simple.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/Thiago-Alckmin/helper_functions_scripts/main/helper_functions_analysis.R", encoding = "UTF-8")


dir_wd <- "C:/Users/alckm/Pedro Dropbox/alckmin.thiago@gmail.com/amazonLandPrices_project/"

# section 1: load & save the data -------------

if(FALSE){
states <- c("AC", "AP", "AM", "MA", "MT", "PA", "RO", "RR", "TO", "MS", "GO")


for(STATE in states){
  
  message_with_lines(STATE)
  
  tmp <- paste0(
    "https://dadosabertos.ibama.gov.br/dados/SICAFI/",
    STATE,
    "/Quantidade/multasDistribuidasBensTutelados.csv"
  ) %>% 
  fread() 
  
  if(STATE==states[1]){
    multas <- tmp
  }else{
    multas <- tmp %>% rbind(multas)
  }
    
  
}

setwd(dir_wd)

fwrite(multas, "data/cleaned/infractions/multas_raw.csv")

}

# section 2: identify individuals with multiple infractions; use date to enumerate it -------------

if(FALSE){
multas <- fread("data/cleaned/infractions/multas_raw.csv")

TODAYSDATE <- 20240419
multas %>% names() %>% return_in_vector_format()

multas_clean <- multas %>% copy() %>% 
  .[, c("day", "month", "year") := tstrsplit(`Data Auto`, "/")] %>% 
  .[year == '0997', year := '1997'] %>% 
  .[, date_num := as.numeric(paste0(year, month, day))] %>% 
  rename_columns(
    c(
      'Nº AI',
      'Data Auto',
      'Nome ou Razão Social',
      'CPF/CNPJ',
      'UF',
      'Município',
      'Tipo Auto',
      'Tipo Infração',
      'Enquadramento Legal',
      'Valor do Auto',
      'Moeda',
      'Situação Débito',
      'Última Atualização Relatório'
    )
    ,
    c(
      'infraction_id',
      'infraction_date',
      'name_firname',
      'cpf_cnpj',
      'uf',
      'muni',
      'penalty_type',
      'infraction_type',
      'legal_framing',
      'penalty_fee',
      'penalty_currency',
      'status',
      'last_report_update'
    )) %>% 
  .[order(date_num), infraction_number := 1:.N, cpf_cnpj] %>% 
  .[cpf_cnpj!=""] %>% 
  .[date_num<TODAYSDATE] %>% 
  .[, nome_municipio := stri_trans_general(muni, "Latin-ASCII")] %>% 
  .[, year := as.numeric(year)]

# merge in with municipality data 
setwd(dir_wd)

# get the muni data 
muni_year_intersections <- fread("data/cleaned/municipal_level/muni_year_intersections.csv") 

# append muni names
indigenous_area <- fread("data/cleaned/municipios_brasileiros_tse.csv") %>%
  .[, .(codigo_ibge, nome_municipio)] %>%
  merge(
    x = muni_year_intersections,
    y = .,
    by.x = "municipio",
    by.y = "codigo_ibge",
    all.y = T,
    all.x = T
  ) %>% 
  .[!is.na(year)] %>% 
  .[, .(year, municipio, car_area_intersect_indi, muni_area, nome_municipio)] %>% 
  .[, nome_municipio := stri_trans_general(nome_municipio, "Latin-ASCII")] %>% 
  .[, year := year + 1]

# merge in the indigenous area one 

merge(indigenous_area, multas_clean, by = c("nome_municipio", "year"))

fwrite(multas, "data/cleaned/infractions/multas_clean_indi.csv")

}


# --------------------------------------------

if(FALSE){
  
auto_infracao <- fread("https://dadosabertos.ibama.gov.br/dados/SIFISC/auto_infracao/auto_infracao/auto_infracao.csv")

}