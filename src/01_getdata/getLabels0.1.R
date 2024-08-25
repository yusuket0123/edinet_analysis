### get label string data
### libraries
library(RCurl)
library(tidyverse)
library(jsonlite)
library(XML)
library(xml2)
library(rvest)
library(lubridate)
library(XBRL)

get_label <- function(path_zipdata, year){
  
  zipfile <- path_zipdata[1]
  
  unzip(zipfile = zipfile,
        exdir = path_zip)
  
  path_xbrl <- dir(str_c(path_zip, "/XBRL/Publicdoc"), 
                   pattern = "\\.xbrl", 
                   full.names = T) %>% 
    str_subset("ifrs", negate = TRUE)
  path_xbrl = path_xbrl[stringi::stri_detect_regex(path_xbrl, "jpcrp030000")] # 企業内容等の開示に関する内閣府令 第三号様式 有価証券報告書  (jpcrp030000-asr)
  # https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww.fsa.go.jp%2Fsearch%2F20211109%2F1e_ElementList.xlsx&wdOrigin=BROWSELINK

  parsed <- xbrlParse(path_xbrl)
  
  schema <- xbrlGetSchemaName(parsed)
  
  xbrlFree(parsed)
  
  ##
  
  path_pubdoc <- dirname(path_xbrl)
  
  parsed_Sch <- xbrlParse(str_c(path_pubdoc, "/", schema))
  
  linkbase <- xbrlGetLinkbaseNames(parsed_Sch)
  
  xbrlFree(parsed_Sch)
  
  linkbase %>%
    str_subset(pattern = "jppfs") %>%　#★jpcrp
    str_subset(pattern = "lab.xml") %>%
    read_xml() %>%
    write_xml(file = str_c("Data/label/jppfs_label_", year, ".xml"))
  
  path_label <- dir("Data/label", as.character(year), full.names = TRUE) %>% str_subset(., "xml")
  
  parsed_label <- xbrlParse(path_label)
  
  labels <- xbrlProcessLabels(parsed_label)
  
  xbrlFree(parsed_label)
  
  ##
  
  labels %>%
    as_tibble() %>%
    mutate_at("labelString", str_conv, encoding = "UTF-8") %>%
    mutate_if(is.factor, as.character)  %>%
    slice(str_which(.$labelRole, "verbose", negate = T)) %>%
    slice(str_which(.$labelRole, "2003/role/label$")) %>% 
    select(elementId, labelString) %>%
    write_excel_csv(path = str_c("Data/label/JGAAP_label_", year, ".csv"))
  
  unlink(str_c(path_zip, "/XBRL"), recursive = TRUE)
  
}
