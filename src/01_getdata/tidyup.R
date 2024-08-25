### tidy up data 
### libraries
library(RCurl)
library(tidyverse)
library(jsonlite)
library(XML)
library(xml2)
library(rvest)
library(lubridate)
library(XBRL)

source("src/01_getdata/defineFuncExtractData.R")

tidyup_data <- function(path_zipdata, path_label, path_tidyup, start, end){
  
  print(str_c("start!", now(), sep = ", "))
  
  i <- as.integer(start - 1L)
  
  fs_name <- c("BalanceSheet", "StatementOfIncome", "StatementOfCashFlows",
               "StatementOfComprehensiveIncome", "StatementOfChangesInEquity")
  
  path_zip <- dirname(path_zipdata) %>% unique()
  
  filing_year <- 
    path_zip %>%
    str_split("/", simplify = TRUE) %>% 
    str_subset("filing$") %>% 
    str_sub(1L, 4L)
  
  ## label
  label <- 
    dir(path_label, filing_year, full.names = TRUE) %>% 
    str_subset("\\.csv$") %>% 
    read_csv()
  
  for(zip in path_zipdata[start:end]) {
    
    unzip(zipfile = zip,
          exdir = path_zip)
    
    i <- i + 1L
    
    docID <- get_docID(zip, path_zip)
    
    path_xbrl <- 
      dir(str_c(path_zip, "/XBRL/Publicdoc"), pattern = "\\.xbrl", full.names = T) %>% 
      str_subset("ifrs", negate = TRUE)
    
    ## fact
    joined_fact <- get_fact(path_xbrl) # func. is defined in "defineFuncExtractData.R"
    
    ## context
    joined_context <- get_context(path_xbrl) # func. is defined in "defineFuncExtractData.R"
    
    ## accounting_standard
    ac_standard <- get_ac_standard(joined_fact) # func. is defined in "defineFuncExtractData.R"
    
    
    if(ac_standard == "Japan GAAP"){
      
      joined_fcl <- join_fcl(joined_fact, joined_context, label) # func. is defined in "defineFuncExtractData.R"
      
      ## consolidated financial statement or NOT
      if(is_consolidated(joined_fcl)){
        
        ## definition
        joined_definition <- 
          get_definition(path_xbrl, fs_name) %>% 
          slice(str_which(.$roleId, "Consolidated"))
        
        
        joined_data <- 
          joined_fcl %>% 
          left_join(joined_definition, by = "elementId") %>% 
          slice(str_which(.$elementId, "jppfs")) %>% 
          slice(str_which(.$contextId, "NonConsolidated", negate = TRUE)) 
        
      } else {
        
        ## definition
        joined_definition <- 
          get_definition(path_xbrl, fs_name) %>% 
          slice(str_which(.$roleId, "Consolidated", negate = TRUE))  # remove consolidated
        
        
        joined_data <- 
          joined_fcl %>% 
          left_join(joined_definition , by = "elementId") %>%   
          slice(str_which(.$elementId, "jppfs"))
        
      }
      
      joined_tidy_data <- tidy_joined_data(joined_data)
      
      NA_flag <-
        joined_tidy_data %>%
        filter(is.na(labelString)) %>% 
        nrow()
      
      if(NA_flag == 0) {
        
        # save data
        joined_tidy_data %>% 
          write_excel_csv(path = str_c(path_tidyup, "/", docID, ".csv"))
        
      } else {
        
        stop(str_c("No.", i, " there are NA rows in labelString!"))
        
      }
      # JGAAP finished
      
    } else if(ac_standard == "IFRS"){
      
      # save as "IFRS_**.txt"
      tibble(
        docID = docID,
        ac_standard = "IFRS"
      ) %>% 
        write_tsv(path = str_c(path_tidyup, "/", "IFRS_", docID, ".txt"))
      
    } else if(ac_standard == "US GAAP"){
      
      # save as "USGAAP_**.txt"
      tibble(
        docID = docID,
        ac_standard = "US GAAP"
      ) %>% 
        write_tsv(path = str_c(path_tidyup, "/", "USGAAP_", docID, ".txt"))
      
    }
    
    ## clean up
    unlink(str_c(path_zip, "/XBRL"), recursive = TRUE)
    unlink("xbrl.Cache", recursive = TRUE)
    rm(list = ls(pattern = "path_xbrl|^joined|^docID$|^ac_standard$|NA_flag"))
    invisible(gc()) ; invisible(gc())
    
    print(str_c(i, now(), sep = ", "))
    
    # repeat
  }
  
}

debug = function(){
  ### temp
  zipfile = "./data/2024-01-14_dl/2023-11-21_filing/zip/S100SC5K.zip"
  path_zip = "./data/2024-01-14_dl/2023-11-21_filing/zip"

  unzip(zipfile = zipfile, exdir = path_zip)
  path_xbrl <- 
    dir(str_c(path_zip, "/XBRL/Publicdoc"), pattern = "\\.xbrl", full.names = T) %>% 
    str_subset("ifrs", negate = TRUE)
  
  temp = read_xml(path_xbrl)
  xml_find_all(temp, xpath = '//jppfs_cor:CallLoansCAFND[@contextRef = "CurrentYearInstant_NonConsolidatedMember"]') %>% xml_text()
  xml_find_all(temp, xpath = '//jpcrp_cor:ManagementAnalysisOfFinancialPositionOperatingResultsAndCashFlowsTextBlock') %>% xml_text()
}

