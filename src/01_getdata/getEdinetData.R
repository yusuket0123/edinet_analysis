#### edinet からデータ取得
### libraries
library(RCurl)
library(tidyverse)
library(jsonlite)
library(XML)
library(xml2)
library(rvest)
library(lubridate)
library(XBRL)

### calling functions in other files
source("src/01_getdata/createdir.R")
source("src/01_getdata/getDocID.R")
source("src/01_getdata/getzip.R")
source("src/01_getdata/getLabels.R")
source("src/01_getdata/tidyup.R")

### creating a folder to preserve data extracted from edinet
date_filing = "2022-04-01" # GETメソッドの出力のsubmitDateTimeを指定 ★ここを365日分回す必要？
date_end = "2022-06-30"
path_filing = make_path_filing(date_filing, date_end)
path_label = str_c("data/label")
#path_tidyup = str_c(path_filing, "/tidyup")
create_file_structure(date_filing, date_end, path_filing, path_label)

### getting docID through edinet API
data_tidy = get_json(date_filing = date_filing, date_end = date_end)
write.csv(data_tidy, paste0(path_filing, "/", "docID.csv"))#★ここまで 
### get zip file of binarydata for financial reports　　　
#data_tidy <- 
#  read_csv(dir(path_filing, ".csv", full.names = TRUE))  # 前記事のdocID.csvを読み込む

docID <-
  data_tidy %>% 
  purrr::pluck("docID")

path_zip = str_c(path_filing, "/zip")

get_zip_via_EDINET(data_tidy, docID, path_zip,
                   start = 1L, end = length(docID))

### get data of labels in financial reports
year <- 2024
path_zipdata <- dir(path_zip, pattern = ".zip", full.names = TRUE)
get_label(path_zipdata, year)

### tidy up data
tidyup_data(path_zipdata, path_label, path_tidyup, start = 1L, end = length(path_zipdata))


debug = function(){
  date_filing = "2024-06-28" # GETメソッドの出力のsubmitDateTimeを指定 ★ここを365日分回す必要？
  date_end = "2024-06-30"
}