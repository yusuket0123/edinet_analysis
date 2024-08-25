### get zip file of binarydata for financial reports

### libraries
library(RCurl)
library(tidyverse)
library(jsonlite)
library(XML)
library(rvest)
library(lubridate)
library(XBRL)

get_zip_via_EDINET <- function(data_tidy, docID, path_zip,
                               start, end) {
  
  url_api = "https://api.edinet-fsa.go.jp/api/v2/documents/"
  
  url_type = "?type=1" # 小類内容取得のためtype=1
  
  key = "&Subscription-Key=eb10a3c6b0a649a4b86980603cd8687e" # subscription key
  
  i <- as.integer(start - 1)
  
  for(id in docID[start:end]) {
    
    ## 過剰アクセス注意 ##
    
    Sys.sleep(0.3)
    
    ##                ##
    
    i <- i + 1
    
    get_binary <- getBinaryURL(str_c(url_api, id, url_type, key))
    
    writeBin(get_binary, str_c(path_zip, "/", id, ".zip"))    
    
    if(file.exists(str_c(path_zip, "/downloaded_XBRL.csv"))) {
      
      data_tidy %>% 
        filter(docID == id) %>% 
        write_excel_csv(path = str_c(path_zip, "/downloaded_XBRL.csv"),
                        append = TRUE)
      
    } else {
      
      data_tidy %>% 
        filter(docID == id) %>% 
        write_excel_csv(path = str_c(path_zip, "/downloaded_XBRL.csv"),
                        append = FALSE)
      
    }
    
    print(str_c(i, id, now(), sep = ", "))
    
    rm(get_binary)
    
  }
  
  rm(list = ls(pattern = "^url|^id$"))
  
}
