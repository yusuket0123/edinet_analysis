#### edinet からデータ取得
### CollectQualitativedata.Rの後続処理
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
library(RMeCab)

### calling functions in other files
source("src/01_getdata/getQualitativedata.R")

### 文章と極性辞書をマッチングする関数
count_match = function(text, word_list){
  if(!is.na(text)){
    kaiseki = RMeCab::RMeCabC(text) # 形態素ごとに文章を区切る
    temp = unlist(kaiseki)
    out = length(intersect(temp, word_list))
  } else {
    out = NA_integer_
  }
  return(out)
}

get_polDic = function(){
  # 日本語極性辞書の読み込み：https://estrellita.hatenablog.com/entry/2018/09/02/004829
  polDic_noun = read_delim("http://www.cl.ecei.tohoku.ac.jp/resources/sent_lex/pn.csv.m3.120408.trim",
                           delim="\t", 
                           col_names = FALSE, 
                           locale = locale(encoding = 'UTF8')
  ) %>%
    dplyr::rename(word = X1, sentiment = X2, vp = X3) %>%
    dplyr::select(word, sentiment) %>%
    dplyr::filter((.$sentiment == "p")|(.$sentiment == "n"))
  
  polDic_verb = read_delim("http://www.cl.ecei.tohoku.ac.jp/resources/sent_lex/wago.121808.pn", 
                           delim="\t", 
                           col_names = FALSE, 
                           locale = locale(encoding = 'UTF8')
  ) %>%
    dplyr::rename(word = X2, sentiment = X1) %>%
    dplyr::select(word, sentiment) %>%
    dplyr::mutate(sentiment = case_when( # sentiment列のユニーク値は"ネガ（経験）" "ネガ（評価）" "ポジ（経験）" "ポジ（評価）"
      stringi::stri_detect_regex(.$sentiment, "^ネガ.*") == TRUE ~ "n",
      stringi::stri_detect_regex(.$sentiment, "^ポジ.*") == TRUE ~ "n"
    )
    )
  
  polDic_maseter = dplyr::bind_rows(polDic_noun, polDic_verb)
  return(polDic_maseter)
}

main = function(data_fs_w_text){  ### edinetからデータ取り込んだ後に処理
  polDic_maseter = get_polDic()
  polDic_maseter_positive = polDic_maseter %>% dplyr::filter(., sentiment == "p")
  polDic_maseter_negative = polDic_maseter %>% dplyr::filter(., sentiment == "n")
  df_out = data_fs_w_text %>%
    dplyr::mutate(
      count_positive = unlist(purrr::map(.$info_ManagementAnalysis_neat, ~ count_match(.x, polDic_maseter_positive$word))),
      count_negative = unlist(purrr::map(.$info_ManagementAnalysis_neat, ~ count_match(.x, polDic_maseter_negative$word))),
    ) %>%
    dplyr::mutate(tone = (.$count_positive - .$count_negative) / (.$count_positive + .$count_negative) # tone算出
    ) %>%
    dplyr::select(-c("info_ManagementAnalysis_raw", "info_BusinessPolicy_raw", "info_ManagementAnalysis_neat", "info_BusinessPolicy_neat")) # 文字データが重すぎるため省略
  return(df_out)
}

debug = function(){
  df_out = main(data_fs_w_text = data_tidy_with_text)
  df_out = df_out %>% dplyr::filter(!is.na(.$count_positive))
  df_out = df_out[!duplicated(df_out),]
  write.csv(df_out, paste(path_root, "data_count_FY2023.csv", sep = "/"))
}

### データの取得・吐き出し（リファクタリング前）
output = function(){
  ### 2024
  dwnld_date = "2024-07-13"
  start_date = "2024-04-01"
  end_date = "2024-06-30"
  path_root = str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date)
  df_use = read.csv(paste0(path_root, "/data_w_text_2024.csv"))
  df_out = main(data_fs_w_text = df_use)
  df_out = df_out %>% dplyr::filter(!is.na(.$count_positive))
  df_out = df_out[!duplicated(df_out),]
  write.csv(df_out, paste(path_root, "data_count_FY2024.csv", sep = "/"))
  
  ### 2023
  dwnld_date = "2024-05-20"
  start_date = "2023-04-01"
  end_date = "2023-06-30"
  path_root = str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date)
  df_use = read.csv(paste0(path_root, "/data_w_text_2023.csv"))
  df_out = main(data_fs_w_text = df_use)
  df_out = df_out %>% dplyr::filter(!is.na(.$count_positive))
  df_out = df_out[!duplicated(df_out),]
  write.csv(df_out, paste(path_root, "data_count_FY2023.csv", sep = "/"))
  
  ### 2022
  dwnld_date = "2024-05-20"
  start_date = "2022-04-01"
  end_date = "2022-06-30"
  path_root = str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date)
  df_use = read.csv(paste0(path_root, "/data_w_text_2022.csv"))
  df_out = main(data_fs_w_text = df_use)
  df_out = df_out %>% dplyr::filter(!is.na(.$count_positive))
  df_out = df_out[!duplicated(df_out),]
  write.csv(df_out, paste(path_root, "data_count_FY2022.csv", sep = "/"))
  
  ### 2021
  dwnld_date = "2024-07-14"
  start_date = "2021-04-01"
  end_date = "2021-06-30"
  path_root = str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date)
  df_use = read.csv(paste0(path_root, "/data_w_text_2021.csv"))
  df_out = main(data_fs_w_text = df_use)
  df_out = df_out %>% dplyr::filter(!is.na(.$count_positive))
  df_out = df_out[!duplicated(df_out),]
  write.csv(df_out, paste(path_root, "data_count_FY2021.csv", sep = "/"))
  
  ### 2020
  dwnld_date = "2024-07-14"
  start_date = "2020-04-01"
  end_date = "2020-06-30"
  path_root = str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date)
  df_use = read.csv(paste0(path_root, "/data_w_text_2020.csv"))
  df_out = main(data_fs_w_text = df_use)
  df_out = df_out %>% dplyr::filter(!is.na(.$count_positive))
  df_out = df_out[!duplicated(df_out),]
  write.csv(df_out, paste(path_root, "data_count_FY2020.csv", sep = "/"))
  
  ### 2019
  dwnld_date = "2024-07-14"
  start_date = "2019-04-01"
  end_date = "2019-06-30"
  path_root = str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date)
  df_use = read.csv(paste0(path_root, "/data_w_text_2019.csv"))
  df_out = main(data_fs_w_text = df_use)
  df_out = df_out %>% dplyr::filter(!is.na(.$count_positive))
  df_out = df_out[!duplicated(df_out),]
  write.csv(df_out, paste(path_root, "data_count_FY2019.csv", sep = "/"))
  
  ### 2018
  dwnld_date = "2024-07-14"
  start_date = "2018-04-01"
  end_date = "2018-06-30"
  path_root = str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date)
  df_use = read.csv(paste0(path_root, "/data_w_text_2018.csv"))
  df_out = main(data_fs_w_text = df_use)
  df_out = df_out %>% dplyr::filter(!is.na(.$count_positive))
  df_out = df_out[!duplicated(df_out),]
  write.csv(df_out, paste(path_root, "data_count_FY2018.csv", sep = "/"))
  
  ### 2017
  dwnld_date = "2024-07-15"
  start_date = "2017-04-01"
  end_date = "2017-06-30"
  path_root = str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date)
  df_use = read.csv(paste0(path_root, "/data_w_text_2017.csv"))
  df_out = main(data_fs_w_text = df_use)
  df_out = df_out %>% dplyr::filter(!is.na(.$count_positive))
  df_out = df_out[!duplicated(df_out),]
  write.csv(df_out, paste(path_root, "data_count_FY2017.csv", sep = "/"))
  
  df_use = read.csv(paste0(path_root, "/data_count_FY2017.csv"))
  
  
  ### 2016
  dwnld_date = "2024-07-15"
  start_date = "2016-04-01"
  end_date = "2016-06-30"
  path_root = str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date)
  df_use = read.csv(paste0(path_root, "/data_w_text_2016.csv"))
  df_out = main(data_fs_w_text = df_use)
  df_out = df_out %>% dplyr::filter(!is.na(.$count_positive))
  df_out = df_out[!duplicated(df_out),]
  write.csv(df_out, paste(path_root, "data_count_FY2016.csv", sep = "/"))
  
}
