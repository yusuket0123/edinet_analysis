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
  
  parsed <- xbrlParse(path_xbrl)
  
  schema <- xbrlGetSchemaName(parsed)
  
  xbrlFree(parsed)
  
  ##
  
  path_pubdoc <- dirname(path_xbrl)
  
  parsed_Sch <- xbrlParse(str_c(path_pubdoc, "/", schema))
  
  linkbase <- xbrlGetLinkbaseNames(parsed_Sch)
  
  xbrlFree(parsed_Sch)
  
  linkbase %>%
    str_subset(pattern = "jppfs") %>%
    str_subset(pattern = "lab.xml") %>%
    read_xml() %>%
    write_xml(file = str_c("Data/label/jppfs_label_", year, ".xml"))
  
  path_label <- dir("data/label", as.character(year), full.names = TRUE) %>%
    str_subset("jppfs") # jgaapとかも入ってくる可能性あるのでjppfs（日本会計基準に順序）を指定
  
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
    write_excel_csv(path = str_c("Data/label/jppfs_label_", year, ".csv"))
  
  unlink(str_c(path_zip, "/XBRL"), recursive = TRUE)
  
}
