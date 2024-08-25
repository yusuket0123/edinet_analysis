#### edinet からデータ取得
### zip fileとdocID.csv格納済の前提での処理

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
source("src/01_getdata/getQualitativedata.R")



## define function for cleaning text data with html format
clean_text = function(text_data){
  if(!is.na(text_data)){
    str = rvest::html_text(rvest::read_html(text_data))
    str_out = gsub("\\n.", " ", str) 
  } else {
    str_out = NA_character_
  }
  return(str_out)
}

## define function for creating a folder to preserve data extracted from edinet
create_tidyup_df = function(start_date, end_date){
  s <- as.Date(start_date)  # 開始日
  e <- as.Date(end_date)    # 終了日
  
  date_list <- seq.Date(s, e, by = "day")  # 日単位でリストを生成
  
  data_tidy = tibble::tibble() %>% 
    tibble::add_column(uploaded_date = character(), docID = character(), edinetCode = character(), filerName = character(), secCode = character(), periodEnd = character())
  for (d in 1:length(date_list)) {
    day = date_list[d]
    print(day)
    date_filing = day
    ### getting docID through edinet API
    temp = get_json(date_filing = date_filing, date = today())
    #browser()
    if(nrow(temp) != 0){
      temp = temp %>% dplyr::mutate(uploaded_date = as.character(.$uploaded_date))
      data_tidy = data_tidy %>% dplyr::bind_rows(., temp)
    }
    print(paste(day, "is completed"))
  }
  return(data_tidy)
}

## define function for creating dataframe with textdata
create_tidyup_df_w_text = function(data_tidy, dwnld_date, start_date, end_date){
  ### get zip file of binarydata for financial reports
  docID = data_tidy %>% purrr::pluck("docID") # 前記事のdocID.csvを読み込む
  edinetCode = data_tidy %>% purrr::pluck("edinetCode") 
  
  ### create folders
  path_label = str_c("data/label")
  path_root = str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date)
  path_zip = str_c(path_root, "/zip")
  year = stringr::str_split(start_date, pattern = "-")[[1]][1]
  #if(!file.exists(path_root)) {dir.create(path_root)}
  #if(!file.exists(path_zip)) {dir.create(path_zip)}
  #get_zip_via_EDINET(data_tidy, docID, path_zip, start = 1, end = length(docID))
  
  df_text = tibble::tibble(
    docID = character(),
    info_ManagementAnalysis_raw = character(),
    info_BusinessPolicy_raw = character()
  )
  
  print(paste("# of companies subject to the download is", length(docID)))
  pb <- txtProgressBar(min = 0, max = length(docID), style = 3)
  #i = 1
  for (i in 1:length(docID)) {
    d = docID[i]
    e = edinetCode[i]
    list_text = getTextdata(doc_id = d, path_zip = path_zip, edinetCode = e, year)
    new_row = tibble::tibble(
      docID = d,
      info_ManagementAnalysis_raw = list_text[["info_ManagementAnalysis"]],
      info_BusinessPolicy_raw = list_text[["info_BusinessPolicy"]]
    )
    df_text = df_text %>% 
      dplyr::bind_rows(., new_row)
    
    setTxtProgressBar(pb, i) 
  }
  
  close(pb)
  
  data_tidy_with_text = data_tidy %>% 
    dplyr::left_join(., df_text, id = "docID") %>%
    dplyr::mutate(
      info_ManagementAnalysis_neat = unlist(purrr::map(.$info_ManagementAnalysis_raw, clean_text)),
      info_BusinessPolicy_neat = unlist(purrr::map(.$info_BusinessPolicy_raw, clean_text))
    )
  return(data_tidy_with_text)
}

## define func. for execution
main = function(dwnld_date, start_date, end_date){
  path_root = str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date)
  data_tidy = read.csv(paste0(path_root, "/docID.csv"))
  #data_tidy = create_tidyup_df(dwnld_date = dwnld_date, start_date = start_date, end_date = end_date)
  data_w_text = create_tidyup_df_w_text(data_tidy = data_tidy, dwnld_date = dwnld_date, start_date = start_date, end_date = end_date)
  return(data_w_text)
}

### データの掃き出し（リファクタリング前）
output = function(){
  # write.csv(data_tidy_with_text, paste(path_root, "data_tidy_with_text_2023-04-01_to_2023-06-30.csv", sep = "/"))
  
  ### 出力
  #df_use_FY2021 = main(start_date = "2022-04-01", end_date = "2022-06-30")
  #df_use_FY2022 = main(start_date = "2023-04-01", end_date = "2023-06-30")
  ## 2024
  dwnld_date = "2024-07-13"
  start_date = "2024-04-01"
  end_date = "2024-06-30"
  data_w_text_2024 = main(dwnld_date = dwnld_date, start_date = start_date, end_date = end_date)
  write.csv(data_w_text_2024,paste0(str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date), "/data_w_text_2024.csv"))
  
  ## 2023
  dwnld_date = "2024-05-20"
  start_date = "2023-04-01"
  end_date = "2023-06-30"
  data_w_text_2023 = main(dwnld_date = dwnld_date, start_date = start_date, end_date = end_date)
  write.csv(data_w_text_2023, paste0(str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date), "/data_w_text_2023.csv"))
  
  ## 2022
  dwnld_date = "2024-05-20"
  start_date = "2022-04-01"
  end_date = "2022-06-30"
  data_w_text_2022 = main(dwnld_date = dwnld_date, start_date = start_date, end_date = end_date)
  write.csv(data_w_text_2022, paste0(str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date), "/data_w_text_2022.csv"))
  
  
  ## 2021
  dwnld_date = "2024-07-14"
  start_date = "2021-04-01"
  end_date = "2021-06-30"
  data_w_text_2021 = main(dwnld_date = dwnld_date, start_date = start_date, end_date = end_date)
  write.csv(data_w_text_2021, paste0(str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date), "/data_w_text_2021.csv"))
  
  ## 2020
  dwnld_date = "2024-07-14"
  start_date = "2020-04-01"
  end_date = "2020-06-30"
  data_w_text_2020 = main(dwnld_date = dwnld_date, start_date = start_date, end_date = end_date)
  write.csv(data_w_text_2020, paste0(str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date), "/data_w_text_2020.csv"))
  
  ## 2019
  dwnld_date = "2024-07-14"
  start_date = "2019-04-01"
  end_date = "2019-06-30"
  data_w_text_2019 = main(dwnld_date = dwnld_date, start_date = start_date, end_date = end_date)
  write.csv(data_w_text_2019, paste0(str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date), "/data_w_text_2019.csv"))
  
  ## 2018
  dwnld_date = "2024-07-14"
  start_date = "2018-04-01"
  end_date = "2018-06-30"
  data_w_text_2018 = main(dwnld_date = dwnld_date, start_date = start_date, end_date = end_date)
  write.csv(data_w_text_2018, paste0(str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date), "/data_w_text_2018.csv"))
  
  ## 2017
  dwnld_date = "2024-07-15"
  start_date = "2017-04-01"
  end_date = "2017-06-30"
  data_w_text_2017 = main(dwnld_date = dwnld_date, start_date = start_date, end_date = end_date)
  write.csv(data_w_text_2017, paste0(str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date), "/data_w_text_2017.csv"))
  
  ## 2016
  dwnld_date = "2024-07-15"
  start_date = "2016-04-01"
  end_date = "2016-06-30"
  data_w_text_2016 = main(dwnld_date = dwnld_date, start_date = start_date, end_date = end_date)
  write.csv(data_w_text_2016, paste0(str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date), "/data_w_text_2016.csv"))
  
}

debug = function(){
  date_filing = "2023-11-21" # GETメソッドの出力のsubmitDateTimeを指定 ★ここを365日分回す必要？
  path_filing = make_path_filing(date_filing)
  path_label = str_c("data/label")
  make_path_tidyup =  make_path_filing(date_filing)
  create_file_structure(date_filing, path_filing, path_label)
  
  ### getting docID through edinet API
  get_json(date_filing = date_filing, path_filing = path_filing, date = today(), registered_date = date_filing)
  ### for文はここまででよさそう★
  
  ### get zip file of binarydata for financial reports
  data_tidy <- 
    read_csv(dir(path_filing, ".csv", full.names = TRUE))  # 前記事のdocID.csvを読み込む
  
  docID <-
    data_tidy %>% 
    purrr::pluck("docID")
  
  path_zip = str_c(path_filing, "/zip") # path_fillingは汎用的にする必要あり★
  
  get_zip_via_EDINET(data_tidy, docID, path_zip,
                     start = 1L, end = length(docID))
  
  ### get text data in financial reports
  path_zipdata <- dir(path_zip, pattern = ".zip", full.names = TRUE)
  # stringi::stri_extract_last_regex(path_zipdata, "[A-Z0-9]{8}(?=\\.zip)")
  
  df_text = tibble::tibble(
    docID = character(),
    info_ManagementAnalysis_raw = character(),
    info_BusinessPolicy_raw = character()
  )
  for (i in docID) {
    list_text = getTextdata(doc_id = i, path_zip = path_zip)
    new_row = tibble::tibble(
      docID = i,
      info_ManagementAnalysis_raw = list_text[["info_ManagementAnalysis"]],
      info_BusinessPolicy_raw = list_text[["info_BusinessPolicy"]]
    )
    df_text = df_text %>% 
      dplyr::bind_rows(., new_row)
  }
  data_tidy_with_text = data_tidy %>% dplyr::left_join(., df_text, id = "docID")
}
