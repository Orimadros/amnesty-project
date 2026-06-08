library(readr)
library(RSelenium)
library(wdman)
library(tidyr)
library(tidyverse)
library(downloader)
library(sjmisc)
library(fastmatch)
library(doParallel)
library(stringr)
library(lubridate)
library(stringi)
library(dplyr)
library(readxl)
library(haven)
library(Metrics)
library(bigmemory)
library(biganalytics)
library(bigtabulate)
library(Rcpp)
library(data.table)
library(ff)
library(ffbase)
library(tabulizer)
library(edgar)
library(qpdf)
library(edgarWebR)
library(fuzzyjoin)
registerDoParallel(cores = 10)
str_between <- function(string, start, end) {
  stringr::str_extract(string,
                       stringr::regex(str_c(start, '(.*?)', end, collapse = ''), ignore_case = T)) %>%
    stringr::str_replace(start, "") %>%
    stringr::str_replace(end, "")
}
str_before <- function(string, pattern) {
  stringr::str_extract(string, stringr::str_c(".+?(?=", pattern, ")"))
}
str_between_n <- function (string, start, end, n) {
  stringr::str_extract_all(string,
                           stringr::str_c(start, "(.*?)", end, collapse = "")) %>%
    purrr::map(~.x %>% str_between(start, end) %>% .[n]) %>%
    unlist()
}
str_between_all <- function(string, start, end) {
  str_extract_all(string, str_c(start, '(.*?)', end, collapse = '')) %>%
    purrr::map(~ .x %>% str_between(start, end))
}
str_after <- function(string, pattern) {
  str_extract(string, str_c("(?<=", pattern, ").*$"))
}
#######     #######     #######     #######     #######     #######     #######     #######
selServ <- selenium(
  port = 1423L,
  version = 'latest',
  chromever = '110.0.5481.30', # set to available
)
remDr <- remoteDriver(
  remoteServerAddr = 'localhost',
  port = 1423L,
  browserName = 'firefox'
)
remDr$open()
#######     #######     #######     #######     #######     #######     #######     #######
#SECTION 1
url <- "http://car.sedam.ro.gov.br/#/site/consultar"
remDr$navigate(url)

terra_legal_ro$car <- 0

for(name in 1:length(terra_legal_ro$nome_requerente)){


webElem <- remDr$findElement(using = "xpath", "/html/body/div[2]/article/div/div[2]/div/form/div[3]/div[1]/input")
webElem$clearElement()
webElem$sendKeysToElement(list(terra_legal_ro$nome_requerente[name]))

button <- remDr$findElement(using = "xpath", "/html/body/div[2]/article/div/div[2]/div/div[1]/button")
button$clickElement()
Sys.sleep(1)
c <- remDr$getPageSource()[[1]]


if(str_detect(c, "RO-")){
  terra_legal_ro$car[name] <- 1
}
print(name)
}


gsub(".*</td><td>", "", strsplit(str_squish(str_remove_all(str_remove_all(gsub(".*Área", "", gsub("gRepeat: page in pages.*", "", c)), '</td><td class=\"'), '\">')), "ng-binding")[[1]])



strsplit(gsub(".*Área", "", gsub("btAcoes.*", "", c)), "ng-binding")
strsplit(str_squish(str_remove_all(str_remove_all(gsub(".*Área", "", gsub("btAcoes.*", "", c)), '</td><td class=\"'), '\">')), "ng-binding")


data <- data.frame(unlist(strsplit(c, "http://museu.in.gov.br/fi/web/dou/dou/-")))
data <- data.frame(data[str_detect(data$unlist.strsplit.c...http...museu.in.gov.br.fi.web.dou.dou....., "/document_library"), ])
data$year <- as.numeric(str_extract(str_extract(data$data.str_detect.data.unlist.strsplit.c...http...museu.in.gov.br.fi.web.dou.dou......., '\">([1-2])([0-9])([0-9])([0-9])'), "([0-9])([0-9])([0-9])([0-9])"))
colnames(data)[1] <- "url"

url <- "http://museu.in.gov.br/fi/web/dou/dou/-/document_library/kcmautn6AnNs/view/271525?_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs_navigation=home&_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs_curFolder=&_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs_deltaFolder=&_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs_deltaEntry=75&_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs_orderByCol=modifiedDate&_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs_orderByType=asc&p_r_p_resetCur=false&_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs_curEntry=2"
remDr$navigate(url)
c <- remDr$getPageSource()[[1]]

data2 <- data.frame(unlist(strsplit(c, "http://museu.in.gov.br/fi/web/dou/dou/-")))
data2 <- data.frame(data2[str_detect(data2$unlist.strsplit.c...http...museu.in.gov.br.fi.web.dou.dou....., "/document_library"), ])
data2$year <- as.numeric(str_extract(str_extract(data2$data2.str_detect.data2.unlist.strsplit.c...http...museu.in.gov.br.fi.web.dou.dou......., '\">([1-2])([0-9])([0-9])([0-9])'), "([0-9])([0-9])([0-9])([0-9])"))
data2 <- data2 %>% arrange(!is.na(year))
colnames(data2)[1] <- "url"

data <- data %>% rbind(data2) %>% filter(!is.na(year)) %>% arrange(year)
data$url <- paste0("http://museu.in.gov.br/fi/web/dou/dou/-", data$url)
data$url <- gsub('\">([0-9]+).*', "", data$url)

for(k in 1:length(data$url)){
  
  remDr$navigate(data$url[k])
  Sys.sleep(10)
  c <- remDr$getPageSource()[[1]]
  
  l <- unlist(str_between_all(c, "http://museu.in.gov.br/fi/web/dou/dou/-/document_library/", '\"'))
  l <- l[str_detect(l, "curEntry=([0-9]+)$")]
  l <- str_replace_all(l, "&amp;", "&")  
  l <- paste0("http://museu.in.gov.br/fi/web/dou/dou/-/document_library/", gsub("deltaEntry=.*", "", l), gsub(".*orderByCol=", "", l))
  l <- unique(l)
  
  pages <- l[!str_detect(l, "=1$")] 
  
  link0 <- paste0("http://museu.in.gov.br/documents/", gsub("&amp;download=true.*", "", unlist(strsplit(c, "http://museu.in.gov.br/documents/"))))
  link0 <- data.frame(link0[nchar(link0) < 1000])
  colnames(link0)[1] <- "url"
  
  if(length(pages) > 0){
    for(i in pages){
      
      remDr$navigate(i)
      Sys.sleep(10)
      c <- remDr$getPageSource()[[1]]
      
      link <- paste0("http://museu.in.gov.br/documents/", gsub("&amp;download=true.*", "", unlist(strsplit(c, "http://museu.in.gov.br/documents/"))))
      link <- data.frame(link[nchar(link) < 1000])
      colnames(link)[1] <- "url"
      
      link0 <- rbind(link0, link)
    }
    
  }
  
  link0 <- link0 %>% filter(!str_detect(url, "log"))
  if(length(link0$url) == length(distinct(link0)$url)){
    
    for(j in 1:length(link0$url)){
      
      if(j%%20 == 0){
        remDr$navigate(data$url[k])
      }
      
      perror <- tryCatch({
        download(paste0(link0$url[j], ".pdf"), mode = "a")
      }, error = function(e) e)
      
      if(inherits(perror, "error")){
        
        tryCatch({
          download(paste0(link0$url[j], ".pdf"), paste0("~/Documents/pdf/pdf_dou_daily_1808-2001/dou_sec1_", str_between(link0$url[j], "DO_1_", ".pdf") , ".pdf"), mode = "a")
        }, error = function(e) e)
        
      }
      
    }
  }
}
#######     #######     #######     #######     #######     #######     #######     #######
#######     #######     #######     #######     #######     #######     #######     #######
#SECTION 2
url <- "http://museu.in.gov.br/fi/web/dou/dou/-/document_library/kcmautn6AnNs/view/271528?_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs_redirect=http%3A%2F%2Fmuseu.in.gov.br%2Ffi%2Fweb%2Fdou%2Fdou%3Fp_p_id%3Dcom_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs%26p_p_lifecycle%3D0%26p_p_state%3Dnormal%26p_p_mode%3Dview%26_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs_mvcRenderCommandName%3D%252Fdocument_library%252Fview"
remDr$navigate(url)
c <- remDr$getPageSource()[[1]]

data <- data.frame(unlist(strsplit(c, "http://museu.in.gov.br/fi/web/dou/dou/-")))
data <- data.frame(data[str_detect(data$unlist.strsplit.c...http...museu.in.gov.br.fi.web.dou.dou....., "/document_library"), ])
data$year <- as.numeric(str_extract(str_extract(data$data.str_detect.data.unlist.strsplit.c...http...museu.in.gov.br.fi.web.dou.dou......., '\">([1-2])([0-9])([0-9])([0-9])'), "([0-9])([0-9])([0-9])([0-9])"))
colnames(data)[1] <- "url"
data <- data %>% filter(!is.na(year)) %>% arrange(year)
data$url <- paste0("http://museu.in.gov.br/fi/web/dou/dou/-", data$url)
data$url <- gsub('\">([0-9]+).*', "", data$url)

for(k in 1:length(data$url)){
  
  remDr$navigate(data$url[k])
  Sys.sleep(10)
  c <- remDr$getPageSource()[[1]]
  
  l <- unlist(str_between_all(c, "http://museu.in.gov.br/fi/web/dou/dou/-/document_library/", '\"'))
  l <- l[str_detect(l, "curEntry=([0-9]+)$")]
  l <- str_replace_all(l, "&amp;", "&")  
  l <- paste0("http://museu.in.gov.br/fi/web/dou/dou/-/document_library/", gsub("deltaEntry=.*", "", l), gsub(".*orderByCol=", "", l))
  l <- unique(l)
  
  pages <- l[!str_detect(l, "=1$")] 
  
  link0 <- paste0("http://museu.in.gov.br/documents/", gsub("&amp;download=true.*", "", unlist(strsplit(c, "http://museu.in.gov.br/documents/"))))
  link0 <- data.frame(link0[nchar(link0) < 1000])
  colnames(link0)[1] <- "url"
  
  if(length(pages) > 0){
    for(i in pages){
      
      remDr$navigate(i)
      Sys.sleep(10)
      c <- remDr$getPageSource()[[1]]
      
      link <- paste0("http://museu.in.gov.br/documents/", gsub("&amp;download=true.*", "", unlist(strsplit(c, "http://museu.in.gov.br/documents/"))))
      link <- data.frame(link[nchar(link) < 1000])
      colnames(link)[1] <- "url"
      
      link0 <- rbind(link0, link)
    }
    
  }
  
  link0 <- link0 %>% filter(!str_detect(url, "log"))
  if(length(link0$url) == length(distinct(link0)$url)){
    
    for(j in 1:length(link0$url)){
      
      if(j%%20 == 0){
        remDr$navigate(data$url[k])
      }
      
      perror <- tryCatch({
        download(paste0(link0$url[j], ".pdf"), mode = "a")
      }, error = function(e) e)
      
      if(inherits(perror, "error")){
        
        tryCatch({
          download(paste0(link0$url[j], ".pdf"), paste0("~/Documents/pdf/pdf_dou_daily_1808-2001/dou_sec2_", str_between(link0$url[j], "DO_2_", ".pdf") , ".pdf"), mode = "a")
        }, error = function(e) e)
        
      }
      
    }
  }
}
#######     #######     #######     #######     #######     #######     #######     #######
#######     #######     #######     #######     #######     #######     #######     #######
#SECTION 3
url <- "http://museu.in.gov.br/fi/web/dou/dou/-/document_library/kcmautn6AnNs/view/271531?_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs_redirect=http%3A%2F%2Fmuseu.in.gov.br%2Ffi%2Fweb%2Fdou%2Fdou%3Fp_p_id%3Dcom_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs%26p_p_lifecycle%3D0%26p_p_state%3Dnormal%26p_p_mode%3Dview%26_com_liferay_document_library_web_portlet_DLPortlet_INSTANCE_kcmautn6AnNs_mvcRenderCommandName%3D%252Fdocument_library%252Fview"
remDr$navigate(url)
c <- remDr$getPageSource()[[1]]

data <- data.frame(unlist(strsplit(c, "http://museu.in.gov.br/fi/web/dou/dou/-")))
data <- data.frame(data[str_detect(data$unlist.strsplit.c...http...museu.in.gov.br.fi.web.dou.dou....., "/document_library"), ])
data$year <- as.numeric(str_extract(str_extract(data$data.str_detect.data.unlist.strsplit.c...http...museu.in.gov.br.fi.web.dou.dou......., '\">([1-2])([0-9])([0-9])([0-9])'), "([0-9])([0-9])([0-9])([0-9])"))
colnames(data)[1] <- "url"
data <- data %>% filter(!is.na(year)) %>% arrange(year)
data$url <- paste0("http://museu.in.gov.br/fi/web/dou/dou/-", data$url)
data$url <- gsub('\">([0-9]+).*', "", data$url)

for(k in 1:length(data$url)){
  
  remDr$navigate(data$url[k])
  Sys.sleep(10)
  c <- remDr$getPageSource()[[1]]
  
  l <- unlist(str_between_all(c, "http://museu.in.gov.br/fi/web/dou/dou/-/document_library/", '\"'))
  l <- l[str_detect(l, "curEntry=([0-9]+)$")]
  l <- str_replace_all(l, "&amp;", "&")  
  l <- paste0("http://museu.in.gov.br/fi/web/dou/dou/-/document_library/", gsub("deltaEntry=.*", "", l), gsub(".*orderByCol=", "", l))
  l <- unique(l)
  
  pages <- l[!str_detect(l, "=1$")] 
  
  link0 <- paste0("http://museu.in.gov.br/documents/", gsub("&amp;download=true.*", "", unlist(strsplit(c, "http://museu.in.gov.br/documents/"))))
  link0 <- data.frame(link0[nchar(link0) < 1000])
  colnames(link0)[1] <- "url"
  
  if(length(pages) > 0){
    for(i in pages){
      
      remDr$navigate(i)
      Sys.sleep(10)
      c <- remDr$getPageSource()[[1]]
      
      link <- paste0("http://museu.in.gov.br/documents/", gsub("&amp;download=true.*", "", unlist(strsplit(c, "http://museu.in.gov.br/documents/"))))
      link <- data.frame(link[nchar(link) < 1000])
      colnames(link)[1] <- "url"
      
      link0 <- rbind(link0, link)
    }
    
  }
  
  link0 <- link0 %>% filter(!str_detect(url, "log"))
  if(length(link0$url) == length(distinct(link0)$url)){
    
    for(j in 1:length(link0$url)){
      
      if(j%%20 == 0){
        remDr$navigate(data$url[k])
      }
      
      perror <- tryCatch({
        download(paste0(link0$url[j], ".pdf"), mode = "a")
      }, error = function(e) e)
      
      if(inherits(perror, "error")){
        
        tryCatch({
          download(paste0(link0$url[j], ".pdf"), paste0("~/Documents/pdf/pdf_dou_daily_1808-2001/dou_sec3_", str_between(link0$url[j], "DO_3_", ".pdf") , ".pdf"), mode = "a")
        }, error = function(e) e)
        
      }
      
    }
  }
}
#######     #######     #######     #######     #######     #######     #######     #######
#######     #######     #######     #######     #######     #######     #######     #######
files <- list.files(path="/Volumes/T7\ Shield/pdf_dou_daily_1808-2001", pattern="*.pdf", ignore.case = T, full.names=T, recursive=FALSE)

do1 <- data.frame(files[str_detect(files, "sec1")])
do2 <- data.frame(files[str_detect(files, "sec2")])
do3 <- data.frame(files[str_detect(files, "sec3")])
colnames(do1) <- "file"
colnames(do2) <- "file"
colnames(do3) <- "file"

do1$date <- str_extract(do1$file, "([0-9])([0-9])([0-9])([0-9])([0-9])([0-9])([0-9])([0-9])")
do2$date <- str_extract(do2$file, "([0-9])([0-9])([0-9])([0-9])([0-9])([0-9])([0-9])([0-9])")
do3$date <- str_extract(do3$file, "([0-9])([0-9])([0-9])([0-9])([0-9])([0-9])([0-9])([0-9])")
#do <- full_join(do3, full_join(do2, do1, "date", relationship = "many-to-many"), "date", relationship = "many-to-many")

dates <- sort(unique(c(do1$date, do2$date, do3$date)))

library(pdftools)
#20000 ~ 30000 / end
for(k in 1:length(dates)){
  
  fs <- do1 %>% filter(date == dates[k]) %>% rbind(do2 %>% filter(date == dates[k])) %>% rbind(do3 %>% filter(date == dates[k])) %>% arrange(file)
  
  perror <- tryCatch({
    pdf_combine(fs$file, output = paste0("/Users/pedrotremacoldirossi/Documents/pdf/pdf_dou_complete_daily_1808-2001/", as.character(as.Date(dates[k], "%Y%m%d")), "_complete.pdf"))
  }, error = function(e) e)
  
  if(inherits(perror, "error")){
    
    tryCatch({
      pdf_combine(fs$file, output = paste0("/Users/pedrotremacoldirossi/Documents/pdf/pdf_dou_complete_daily_1808-2001/", as.character(as.Date(dates[k], "%Y%m%d")), "_complete.pdf"))
    }, error = function(e) e)
    
  }
  #pdf_combine(fs$file, output = paste0("/Users/pedrotremacoldirossi/Documents/pdf/pdf_dou_complete_daily_1808-2001/", as.character(as.Date(dates[k], "%Y%m%d")), "_complete.pdf"))
  print(dates[k])
}


files <- list.files(path="/Users/pedrotremacoldirossi/Documents/pdf/pdf_dou_complete_daily_1808-2001/", pattern="*.pdf", ignore.case = T, full.names=T, recursive=FALSE)

for(k in 1:length(files)){
  
  pdf_compress(files[k], output = paste0("/Volumes/T7\ Shield/pdf_dou_complete_daily_1808-2001/", gsub(".*2001//", "", files[k])))
  print(k)
}


#pdf_compress("/Users/pedrotremacoldirossi/Documents/pdf/pdf_dou_complete_daily_1808-2001/2001-09-10_complete.pdf", output = "/Volumes/T7\ Shield/pdf_dou_complete_daily_1808-2001/2001-09-10_complete.pdf")
#pdf_compress("/Users/pedrotremacoldirossi/Documents/pdf/pdf_dou_complete_daily_1808-2001/1997-05-09_complete.pdf")
#tools::compactPDF("/Users/pedrotremacoldirossi/Documents/pdf/pdf_dou_complete_daily_1808-2001/2001-09-13_complete.pdf")



