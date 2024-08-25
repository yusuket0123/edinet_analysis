### getting docID through edinet API

### libraries
library(RCurl)
library(tidyverse)
library(jsonlite)
library(XML)
library(rvest)
library(lubridate)
library(XBRL)

get_json = function(date_filing, date_end) { # ★おかしい
  
  url_api = "https://api.edinet-fsa.go.jp/api/v2/documents.json?date="
  
  url_type = "&type=2" # 提出書類一覧及びメタデータを取得する。本記事の目的はdocIDの取得であるからこちらを指定する。
  
  key = "&Subscription-Key=eb10a3c6b0a649a4b86980603cd8687e" # subscription key
  
  s = as.Date(date_filing)
  e = as.Date(date_end)
    
  days = as.character(seq(from = s, to = e, by = "day")) # 文字列変換しないとdate型が5桁の数字になってしまう
  
  data_tidy = tibble::tibble()
  
  for (d in days) {

    get_res = httpGET(str_c(url_api, d, url_type, key)) ### get response　# GETメソッドの出力のsubmitDateTimeを指定
    
    data_raw = fromJSON(get_res) # transform json data into list
    
    data_flag = # metadata > resultset > countの情報を取得
      data_raw %>% 
      purrr::pluck("metadata") %>% 
      purrr::pluck("resultset") %>% 
      purrr::pluck("count")
    
    if(is.null(data_flag) == TRUE || data_flag == 0){
      print(paste(d, ": no data is extracted through api"))
      #data_tidy = tibble::tibble()
    } else{
      data_tidy_d =       
        data_raw %>% 
        purrr::pluck(2) %>% # extract "results"
        as_tibble() %>%
        dplyr::mutate(uploaded_date = d) %>% # date列を追加★ 
        slice(str_which(.$docDescription, "有価証券報告書")) %>% 
        slice(str_which(.$docDescription, "訂正", negate = TRUE)) %>% # negate: 否定条件
        filter(!is.na(.$secCode)) %>% # 上場企業(seccode)を持っている企業のみ抽出
        select(uploaded_date, docID, edinetCode, filerName, secCode, periodEnd) %>% 
        mutate_at("filerName", str_remove, pattern = "株式会社") %>% # remove "株式会社"
        mutate_at("filerName", str_squish) %>% # remove whitespace
        distinct() %>%
        dplyr::mutate_all(as.character)
      
      data_tidy = data_tidy %>% dplyr::bind_rows(data_tidy_d)  
      print(paste0(d, ": ", nrow(data_tidy_d), " cases"))
      #browser()
      # path_file = str_c("data/", date, "_dl/docID.csv") 　#★
      
      
      #if(nrow(data_tidy) == 0){
      #  print("skip")
      #} 
      #else if(!file.exists(path_file)) {
      #  data_tidy %>% 
      #    write_excel_csv(path_file)
      #} else {
      #  temp = read.csv(path_file, header = TRUE) # 既存のdata_tidyを読み込み★
      #  temp %>% 
      #    dplyr::bind_rows(., data_tidy %>% dplyr::mutate(secCode = as.integer(.$secCode))) %>%
      #    write_excel_csv(path_file)
      #}
    }
  }
 
  ## API key word : docID
  return(data_tidy)
  rm(list = ls(pattern = "^e|^d|^s|^url|get_res|^data"))
}

debug = function(){
  date_filing = as.Date("2024-03-31")
  date_end = as.Date("2024-07-13")
  days = seq(from = date_filing, to = date_end, by = "day")
  }
