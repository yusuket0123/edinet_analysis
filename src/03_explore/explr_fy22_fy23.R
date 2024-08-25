### explanatory analysis on data

library(magrittr)
library(ggplot2)


codelist =  read.csv("data/EdinetcodeDlInfo.csv", fileEncoding = "Shift-JIS")

df_FY23 = read.csv("data/2024-05-20_dl2023-04-01_to_2023-06-30/data_count_FY2023.csv")
df_FY22 = read.csv("data/2024-01-29_dl/data_count_FY2022_rev.csv")

df_FY23_for_join = df_FY23 %>%
  dplyr::select(., c("filerName", "secCode","count_positive", "count_negative", "tone")) %>%
  dplyr::mutate(fyear = 2023)
df_FY22_for_join = df_FY22 %>%
  dplyr::select(., c("secCode","count_positive", "count_negative", "tone")) %>%
  dplyr::mutate(fyear = 2022)
codelist_for_join = codelist %>%
  dplyr::select(c("提出者業種", "証券コード", "資本金")) %>%
  dplyr::rename(category = 提出者業種,
                secCode = 証券コード,
                capital = 資本金) %>%
  dplyr::mutate(secCode = as.integer(.$secCode),
                capital_ln = log10(.$capital)
                )
df_join = df_FY23_for_join %>% 
  dplyr::full_join(., df_FY22_for_join, by = "secCode") %>%
  dplyr::rename_with(., ~ gsub("\\.x$", "_23", .)) %>%
  dplyr::rename_with(., ~ gsub("\\.y$", "_22", .)) %>%
  dplyr::left_join(., codelist_for_join, by = "secCode")

unique(df_join$category)

df_plt = df_join %>%
  dplyr::filter((category == "石油・石炭製品")|(category == "食料品")|(category == "銀行業"))
df_plt = df_join %>%
  dplyr::filter(category == "銀行業")

ggplot2::ggplot(df_plt, aes(x = tone_22, y = tone_23, color = category)) +
  geom_point()

### 財務指標
df_other_FY23 = read.csv("data/2024-05-20_dl2023-04-01_to_2023-06-30/data_tidy_quantitative_FY2023.csv")
df_other_FY22 = read.csv("data/2024-01-29_dl/data_tidy_quantitative_FY2022.csv")

df_text = 
  dplyr::bind_rows(df_FY23 %>% dplyr::mutate(fyear = "2023"), 
                   df_FY22 %>% dplyr::mutate(fyear = "2022")
                   ) %>%
  dplyr::mutate(id_code_year = paste(edinetCode, fyear, sep = "_"))

df_other = 
  dplyr::bind_rows(df_other_FY23 %>% dplyr::mutate(fyear = "2023"), 
                   df_other_FY22 %>% dplyr::mutate(fyear = "2022")
                   )

df_other = df_other %>% 
  dplyr::mutate(roa = .$extordnryinc / .$totalassets, # ROA = 経常利益/総資産
                ret = .$eps, # 株式リターンの代理変数(配当性向かけた方がいい？)
                acc = (.$ordnryinc - .$oprtcf) / .$totalassets, # 会計発生高 = {（経常利益）−（営業CF）} /（総資産）
                size = log10(.$eps * .$per * .$num_share), # 時価総額 ※株価の代わりにEPS(一株当たり純利益)*PER(price earning ratio)     
                mtb = (.$size + .$liability) / .$totalassets, # 時価簿価比={（株式時価総額）＋（負債）} /（総資産）
                nbseg = log(1 + .$count_business), # 事業セグメント近似
                ngseg = log(1 + .$count_country), # 地域（国）セグメント近似
                si = (.$extordnryinc + .$extordnryloss) / .$totalasset
                )
summarise_data = df_other %>%
  dplyr::group_by(edinetCode) %>%
  dplyr::summarise(roavol = max(roa) - min(roa), #2年のボラ ※元研究は5年のボラ
                retvol = max(ret) - min(ret) #2年のボラ ※元研究は5年のボラ
  ) %>%
  dplyr::ungroup()
df_other_v2 = df_other %>%
  dplyr::left_join(., summarise_data, by = "edinetCode") %>%
  dplyr::mutate(id_code_year = paste(edinetCode, fyear, sep = "_"))

df_ana = df_text %>%
  dplyr::left_join(., df_other_v2, by = "id_code_year") %>%
  dplyr::arrange(fyear.x) %>%
  dplyr::mutate(lead_roa = dplyr::lead(as.numeric(roa), n = 1))
df_lm = df_ana %>%
  dplyr::select(lead_roa, tone, roa, ret, acc, size, mtb, retvol, roavol, nbseg, ngseg, si)
df_lm =  na.omit(df_lm) %>%
  dplyr::mutate(
    size = dplyr::case_when(is.infinite(size) ~ 0, TRUE ~ size),
    mtb = dplyr::case_when(is.infinite(mtb) ~ 0, TRUE ~ mtb)
  )
res_simple_lm = lm(lead_roa ~ tone + roa + ret + acc + size + mtb + retvol + roavol + nbseg + ngseg + si,
                   data = df_lm)
summary(res_simple_lm)

debug = function(){
  df_FY22 = df_FY22 %>%
    dplyr::mutate(tone = (.$count_positive - .$count_negative) / (.$count_positive + .$count_negative))
  
}