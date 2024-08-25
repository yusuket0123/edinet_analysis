#### quantitative data
### libraries
library(RCurl)
library(tidyverse)
library(jsonlite)
library(XML)
library(xml2)
library(rvest)
library(lubridate)
library(XBRL)
library(rvest)

### calling functions in other files
source("src/01_getdata/createdir.R")
source("src/01_getdata/getDocID.R")
source("src/01_getdata/getzip.R")
source("src/01_getdata/tidyup.R")
source("src/01_getdata/getQuantitativedata.R")


### defining func.
addcol = function(data, c, n, list_elem){
  v = list_elem[[c]][[n]]$value
  if(length(v) == 0 | is.null(v)){
    v = "0"
  }
  combi = data %>% dplyr::mutate(., !!n := v)
  return(combi)
}

make_row = function(path_zip, docID, edinetCode){
  list_elem = getDatas(doc_id = docID, path_zip = path_zip, edinetCode = edinetCode)
  results = tibble::tibble(docID = docID, edinetCode = edinetCode)
  for (c in seq_along(list_elem)) {
    list_elem_by_category = list_elem[[c]]
    for (idx in seq_along(list_elem_by_category)) {
      n = names(list_elem_by_category)[[idx]]
      results = addcol(results, c, n, list_elem)
    }
  }
  return(results)
}

make_df_quantitative = function(path_zip){
  df_dwnld =read.csv(paste(path_zip, "downloaded_XBRL.csv", sep = "\\"))  # get_quantitativedata.R→getzip.R→get_zip_edinetで保存したdownloaded_XBRLを参照
  
  #t = df_dwnld %>% slice(1:3)
  #getDatas(doc_id = df_dwnld$docID[[2]], edinetCode = df_dwnld$edinetCode[[2]], path_zip = path_zip)
  #make_row(docID = df_dwnld$docID[[3]], edinetCode = df_dwnld$edinetCode[[3]], path_zip = path_zip)
  tmp = purrr::map( 
    seq(nrow(df_dwnld)), 
    ~ make_row(docID = df_dwnld$docID[[.x]], edinetCode = df_dwnld$edinetCode[[.x]], path_zip = path_zip)
  )
  results = bind_rows(tmp)
  results = results %>%
    dplyr::rowwise() %>%
    dplyr::mutate(count_business = count_segment(segment)[[1]],
                  count_country = count_segment(segment)[[2]]
    ) %>%
    dplyr::select(-segment)
  return(results)
}

### データの掃き出し（リファクタリング前）
output = function(){
  ### 出力
  ## FY2023
  path_zip = "data\\2024-05-20_dl2023-04-01_to_2023-06-30\\zip"　#zipfileのpathを指定
  results_FY2023 = make_df_quantitative(path_zip = path_zip)
  write.csv(results_FY2023, "data\\2024-05-20_dl2023-04-01_to_2023-06-30\\data_tidy_quantitative_FY2023.csv")
  
  ## FY2022
  path_zip = "data\\2024-01-29_dl\\zip"　#zipfileのpathを指定
  results_FY2022 = make_df_quantitative(path_zip = path_zip)
  write.csv(results_FY2022, "data\\2024-01-29_dl\\data_tidy_quantitative_FY2022.csv")
  
  ## FY2016
  path_zip = "data\\2024-07-15_dl2016-04-01_to_2016-06-30\\zip"　#zipfileのpathを指定
  results_FY2016 = make_df_quantitative(path_zip = path_zip)
  View(results_FY2016)
  write.csv(results_FY2016, "data\\2024-07-15_dl2016-04-01_to_2016-06-30\\data_tidy_quantitative_FY2016.csv")
  
  ## FY2017
  path_zip = "data\\2024-07-15_dl2017-04-01_to_2017-06-30\\zip"　#zipfileのpathを指定
  results_FY2017 = make_df_quantitative(path_zip = path_zip)
  write.csv(results_FY2017, "data\\2024-07-15_dl2017-04-01_to_2017-06-30\\data_tidy_quantitative_FY2017.csv")
  
  ## FY2018
  path_zip = "data\\2024-07-14_dl2018-04-01_to_2018-06-30\\zip"　#zipfileのpathを指定
  results_FY2018 = make_df_quantitative(path_zip = path_zip)
  write.csv(results_FY2018, "data\\2024-07-14_dl2018-04-01_to_2018-06-30\\data_tidy_quantitative_FY2018.csv")
  
  ## FY2019
  path_zip = "data\\2024-07-14_dl2019-04-01_to_2019-06-30\\zip"　#zipfileのpathを指定
  results_FY2019 = make_df_quantitative(path_zip = path_zip)
  write.csv(results_FY2019, "data\\2024-07-14_dl2019-04-01_to_2019-06-30\\data_tidy_quantitative_FY2019.csv")
  
  ## FY2020
  path_zip = "data\\2024-07-14_dl2020-04-01_to_2020-06-30\\zip"　#zipfileのpathを指定
  results_FY2020 = make_df_quantitative(path_zip = path_zip)
  write.csv(results_FY2020, "data\\2024-07-14_dl2020-04-01_to_2020-06-30\\data_tidy_quantitative_FY2020.csv")
  
  ## FY2021
  path_zip = "data\\2024-07-14_dl2021-04-01_to_2021-06-30\\zip"　#zipfileのpathを指定
  results_FY2021 = make_df_quantitative(path_zip = path_zip)
  write.csv(results_FY2021, "data\\2024-07-14_dl2021-04-01_to_2021-06-30\\data_tidy_quantitative_FY2021.csv")
  
  # FY2024
  path_zip = "data\\2024-07-13_dl2024-04-01_to_2024-06-30\\zip"　#zipfileのpathを指定
  results_FY2024 = make_df_quantitative(path_zip = path_zip)
  write.csv(results_FY2024, "data\\2024-07-13_dl2024-04-01_to_2024-06-30\\data_tidy_quantitative_FY2024.csv")
}
