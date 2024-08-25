### 分析用のlongデータ作成
source("src/01_getdata/CollectQualitativedata.R") 
source("src/02_combine/combi_text_polarityDic.R") # data_count_FYYYYYデータを取得

### データ読み込み
read_cnt_data = function(dwnld_date, start_date, end_date){
  path_root = str_c("data/", dwnld_date, "_dl", start_date, "_to_", end_date)
  year = stringr::str_split(start_date, pattern = "-")[[1]][1]
  fname = paste0("data_count_FY", year, ".csv")
  data = read.csv(paste(path_root, fname, sep = "/"))
  return(data)
}

list_dwnld = list(
  list(dwnld_date = "2024-07-13", start_date = "2024-04-01", end_date = "2024-06-30"),  ### 2024
  list(dwnld_date = "2024-05-20", start_date = "2023-04-01", end_date = "2023-06-30"),  ### 2023
  list(dwnld_date = "2024-05-20", start_date = "2022-04-01", end_date = "2022-06-30"),  ### 2022
  list(dwnld_date = "2024-07-14", start_date = "2021-04-01", end_date = "2021-06-30"),  ### 2021
  list(dwnld_date = "2024-07-14", start_date = "2020-04-01", end_date = "2020-06-30"),  ### 2020
  list(dwnld_date = "2024-07-14", start_date = "2019-04-01", end_date = "2019-06-30"),  ### 2019
  list(dwnld_date = "2024-07-14", start_date = "2018-04-01", end_date = "2018-06-30"),  ### 2018
  list(dwnld_date = "2024-07-15", start_date = "2017-04-01", end_date = "2017-06-30"),  ### 2017
  list(dwnld_date = "2024-07-15", start_date = "2016-04-01", end_date = "2016-06-30")  ### 2016
)

for (idx in 1:length(list_dwnld)) {
  print(idx)
  a = list_dwnld[[idx]]$dwnld_date
  b = list_dwnld[[idx]]$start_date
  c = list_dwnld[[idx]]$end_date
  year = stringr::str_split(b, pattern = "-")[[1]][1]
  data = read_cnt_data(dwnld_date = a, start_date = b, end_date = c)
  obj_name = paste0("results_tone_FY", year)
  assign(obj_name, data)
  ### 必要に応じてresults_FYYYYYもデータ読み込みする
}

### 
## FY2023
results_FY2023 = read.csv("data\\2024-05-20_dl2023-04-01_to_2023-06-30\\data_tidy_quantitative_FY2023.csv")
## FY2022
results_FY2022 = read.csv("data\\2024-05-20_dl2022-04-01_to_2022-06-30\\data_tidy_quantitative_FY2022.csv")
## FY2016
results_FY2016 = read.csv("data\\2024-07-15_dl2016-04-01_to_2016-06-30\\data_tidy_quantitative_FY2016.csv")
## FY2017
results_FY2017 = read.csv("data\\2024-07-15_dl2017-04-01_to_2017-06-30\\data_tidy_quantitative_FY2017.csv")
## FY2018
results_FY2018 = read.csv("data\\2024-07-14_dl2018-04-01_to_2018-06-30\\data_tidy_quantitative_FY2018.csv")
## FY2019
results_FY2019 = read.csv("data\\2024-07-14_dl2019-04-01_to_2019-06-30\\data_tidy_quantitative_FY2019.csv")
## FY2020
results_FY2020 = read.csv("data\\2024-07-14_dl2020-04-01_to_2020-06-30\\data_tidy_quantitative_FY2020.csv")
## FY2021
results_FY2021 = read.csv("data\\2024-07-14_dl2021-04-01_to_2021-06-30\\data_tidy_quantitative_FY2021.csv")
# FY2024
results_FY2024 = read.csv("data\\2024-07-13_dl2024-04-01_to_2024-06-30\\data_tidy_quantitative_FY2024.csv")


### データ突合
for (idx in 1:length(list_dwnld)) {
  print(idx)
  a = list_dwnld[[idx]]$dwnld_date
  b = list_dwnld[[idx]]$start_date
  c = list_dwnld[[idx]]$end_date
  year = stringr::str_split(b, pattern = "-")[[1]][1]
  print(year)
  obj_res = paste0("results_FY", year)
  data_res = get(obj_res)
  obj_tone = paste0("results_tone_FY", year)
  data_tone = get(obj_tone) %>%
    dplyr::select(c("docID", "count_positive", "count_negative", "tone"))
  data_use = data_res %>% 
    dplyr::left_join(., data_tone, by = "docID")
  obj_name = paste0("data_use", year)
  assign(obj_name, data_use)
}

### long型のデータ作成
for (idx in 1:length(list_dwnld)){
  print(idx)
  b = list_dwnld[[idx]]$start_date
  year = stringr::str_split(b, pattern = "-")[[1]][1]
  obj_name = paste0("data_use", year)
  df_add = get(obj_name) %>% dplyr::mutate(fyear = year) %>% dplyr::mutate_all(as.character)
  if(year == "2024"){
    df_use_long = df_add
  }else{
    df_use_long = df_use_long %>%
      dplyr::bind_rows(., df_add)
  }
}

### 各変数の調整
df_use_long2 = df_use_long %>%
  dplyr::mutate(roa = (as.numeric(extordnryinc) - as.numeric(extordnryloss)) / as.numeric(totalassets), # ROA = 経常利益/総資産
                ret = log10(1+as.numeric(eps)), # 株式リターンの代理変数(配当性向かけた方がいい？)
                acc = (as.numeric(ordnryinc) - as.numeric(oprtcf)) / as.numeric(totalassets), # 会計発生高 = {（経常利益）−（営業CF）} /（総資産）
                size = log10(as.numeric(eps) * as.numeric(per) * as.numeric(num_share)), # 時価総額 ※株価の代わりにEPS(一株当たり純利益)*PER(price earning ratio)     
                mtb = (as.numeric(size) + as.numeric(liability)) / as.numeric(totalassets), # 時価簿価比={（株式時価総額）＋（負債）} /（総資産）
                nbseg = log(1 + as.integer(count_business)), # 事業セグメント近似
                ngseg = log(1 + as.integer(count_country)), # 地域（国）セグメント近似
                si = (as.numeric(extordnryinc) + as.numeric(extordnryloss)) / as.numeric(totalassets)
  )

summarise_data = df_use_long2 %>%
  dplyr::group_by(edinetCode) %>%
  dplyr::summarise(roavol = var(roa), #9年のボラ ※元研究は5年のボラ
                   retvol = var(ret) #9年のボラ ※元研究は5年のボラ
  ) %>%
  dplyr::ungroup()

df_spd = read.csv("data/marketdata/speeda/df_spd.csv")

df_use_long3 = df_use_long2 %>%
  dplyr::left_join(., summarise_data, by = "edinetCode") %>%
  dplyr::left_join(., df_spd, by = "edinetCode") %>%
  dplyr::mutate(
    id_code_year = paste(edinetCode, fyear, sep = "_"),
    age = as.integer(listingyears_spd - (2024 - as.integer(fyear)))
                ) %>%
  dplyr::mutate(age = dplyr::if_else(age < 0, 0, as.double(age)))

df_use_long3 = df_use_long3[!duplicated(df_use_long3[["id_code_year"]]),] ## edinetCodeとfyearが重複する行を削除

