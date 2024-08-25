### Construct functions to extract data
### libraries
library(RCurl)
library(tidyverse)
library(jsonlite)
library(XML)
library(xml2)
library(rvest)
library(lubridate)
library(XBRL)


### get docID from unzipped file
get_docID <- function(zip, path_zip){
  
  zip %>% 
    str_remove(pattern = path_zip) %>% 
    str_remove(pattern = "/") %>% 
    str_remove(pattern = ".zip") %>% 
    return()
}


### get fact from file
get_fact <- function(path_xbrl){
  
  parsed <- xbrlParse(path_xbrl)
  
  fact <- xbrlProcessFacts(parsed)
  
  xbrlFree(parsed)
  
  ##
  
  fact %>% 
    as_tibble() %>% 
    mutate_at("fact", str_conv, encoding = "UTF-8") %>% 
    mutate_if(is.factor, as.character) %>% 
    return()
}

### get contextId from file
get_context <- function(path_xbrl){
  
  parsed <- xbrlParse(path_xbrl)
  
  context <- xbrlProcessContexts(parsed)
  
  xbrlFree(parsed)
  
  context %>% 
    as_tibble() %>% 
    mutate_if(is.factor, as.character) %>% 
    return()
}

### get releId from file
get_definition <- function(path_xbrl, fs_name){
  
  path_pubdoc <- dirname(path_xbrl)
  
  path_def <- 
    dir(path_pubdoc, full.names = TRUE) %>% 
    str_subset(pattern = "def.xml$") %>% 
    str_subset(pattern = "ifrs", negate = TRUE)
  
  parsed_def <- xbrlParse(path_def)
  
  definition <- xbrlProcessArcs(parsed_def, arcType = "definition")
  
  xbrlFree(parsed_def)
  
  definition %>% 
    as_tibble() %>% 
    mutate_all(as.character) %>% 
    rename("elementId" = toElementId) %>% 
    slice(str_which(.$roleId, str_c(fs_name, collapse = "|"))) %>% 
    slice(str_which(.$roleId, "jppfs")) %>% 
    select(elementId, roleId) %>% 
    return()
}

### get accounting standard from file
get_ac_standard <- function(joined_fact){
  
  joined_fact %>% 
    slice(str_which(.$elementId, "AccountingStandardsDEI")) %>% 
    purrr::pluck("fact") %>% 
    return()
}

### join fact + context + labelstring
join_fcl <- function(joined_fact, joined_context, label){
  
  joined_fact %>% 
    left_join(joined_context, by = "contextId") %>% 
    left_join(label, by = "elementId") %>% 
    return()
}

### return num of rows having info of financial statement
is_consolidated <- function(joined_fcl){
  joined_fcl %>% 
    slice(str_which(.$elementId, "jppfs")) %>%
    slice(str_which(.$contextId, "NonConsolidated", negate = T)) %>%
    nrow() %>% 
    return()
}

### clean up joined data 
tidy_joined_data <- function(joined_data){
  
  joined_data %>% 
    select(labelString, fact, startDate, endDate, elementId, contextId, roleId) %>% 
    mutate_at("elementId", str_remove, pattern = "jppfs_cor_") %>% 
    separate(col = roleId, into = c("garbage", "roleId"), sep = "rol_") %>% 
    select(-garbage) %>% 
    mutate(roleId = if_else(is.na(.$roleId) & str_detect(.$elementId, "SGA"), 
                            true = "StatementOfIncome", false = .$roleId)) %>% 
    return()
}
