#### sentiment analysis

source("src/02_combine/make_data_ana_tone.R") 

### setup
idx_var = c("edinetCode", "fyear")
dep_var = "tone"
exp_var = c("roa" , "ret" , "acc" , "size", "mtb" , "retvol" , "roavol" , "nbseg" , "ngseg" , "age" , "si")

### pooled ols
df_ana = df_use_long3 %>% 
  dplyr::select_at(c(idx_var, dep_var, exp_var)) %>% # パネルデータとして使用する列に絞る
  dplyr::mutate_at(vars(c(exp_var,dep_var)), as.numeric)
columns_with_missing_values = colnames(df_ana)[colSums(is.na(df_ana)) > 0]　# 欠損値の列を取得

# imputation
df_ana_imp = df_ana
for (e in columns_with_missing_values) {
  print(e)
  med = median(df_ana[[e]], na.rm = TRUE)
  df_ana_imp = df_ana_imp %>%
    dplyr::mutate(!!sym(e) := dplyr::case_when(
      is.na(!!sym(e)) | is.infinite(!!sym(e)) | is.nan(!!sym(e)) == TRUE ~ med,
      TRUE ~ !!sym(e)
    )
    )
}

panel_data = plm::pdata.frame(df_ana_imp, index = c("edinetCode", "fyear"))


### tone 推定
# Pooled OLS モデルを推定
fml = c(paste(dep_var, paste(exp_var, collapse = "+"), sep = "~"))
res_pooled_ols = lfe::felm(as.formula(fml), data = panel_data)

# Li. 2010. plooled ols + year dummy
fml = c(paste(
  paste(dep_var, paste(exp_var, collapse = "+"), sep = "~"),
  "fyear",
  sep = "+"
))
res_li2010 = lfe::felm(as.formula(fml), data = panel_data)

# 固定効果モデル:year, 企業
fml = c(paste(
    paste(dep_var, paste(exp_var, collapse = "+"), sep = "~"),
    paste(idx_var, collapse = "+"),
    sep = "|"
    ))
res_lsdv = lfe::felm(as.formula(fml), data = panel_data)

writexl::write_xlsx(
  list(
    tidy_pooled_ols = broom::tidy(res_pooled_ols),
    glance_pooled_ols = broom::glance(res_pooled_ols),
    tidy_li2010 = broom::tidy(res_li2010),
    glance_li2010 = broom::glance(res_li2010),
    tidy_lsdv = broom::tidy(res_lsdv),
    glance_lsdv = broom::glance(res_lsdv)
  ),
  path = "res/est/res_est_dep_tone.xlsx"
  )

panel_data_tone_adj = panel_data %>%
  dplyr::mutate(tone_adj = as.vector(res_lsdv$residuals),
                tone_est = as.vector(res_lsdv$fitted.values)
                ) %>%
  dplyr::group_by(edinetCode) %>%
  dplyr::mutate(roa_lead = lead(roa, order_by = fyear, default = NA)) %>%
  dplyr::ungroup()

### 業績推定
exp_var = c("tone_adj", "roa" , "ret" , "acc" , "mtb" , "nbseg" , "ngseg" , "age" , "si") # retvol, retは年ダミーと線形従属するため除外

# Pooled OLS モデルを推定
fml = c(paste("roa_lead", paste(exp_var, collapse = "+"), sep = "~"))
res_pooled_ols = lfe::felm(as.formula(fml), data = panel_data_tone_adj)


# Li. 2010. plooled ols + year dummy
fml = c(paste(
  paste("roa_lead", paste(exp_var, collapse = "+"), sep = "~"),
  "fyear",
  sep = "+"
))
res_li2010 = lfe::felm(as.formula(fml), data = panel_data_tone_adj)

# 固定効果モデル:year, 企業
fml = c(paste(
  paste("roa_lead", paste(exp_var, collapse = "+"), sep = "~"),
  paste(idx_var, collapse = "+"),
  sep = "|"
))
res_fe = lfe::felm(as.formula(fml), data = panel_data_tone_adj)


# GMM推定
# データの準備
model_data = panel_data_tone_adj %>%
  dplyr::filter(!is.na(roa_lead)) %>%
  dplyr::mutate(intercept = 1)
# model定義
fml_1 = c(paste(
  paste("roa_lead", paste(exp_var, collapse = "+"), sep = "~"),
  "fyear",
  sep = "+"
))
fml_2 = c(paste(
  paste0("~", paste(exp_var, collapse = "+")),
  "fyear",
  sep = "+"
))
model_gmm_lm = momentfit::momentModel(
  as.formula(fml_1),
  as.formula(fml_2),
  data = model_data
)
fit_gmm_lm = momentfit::gmmFit(model_gmm_lm)
res_gmm = momentfit::summary(fit_gmm_lm)

writexl::write_xlsx(
  list(
    tidy_pooledols = broom::tidy(res_pooled_ols),
    glance_pooledols = broom::glance(res_pooled_ols),
    tidy_li2010 = broom::tidy(res_li2010),
    glance_li2010 = broom::glance(res_li2010),
    tidy_fe = broom::tidy(res_fe),
    glance_fe = broom::glance(res_fe),
    tidy_gmm = tibble::tibble(res_gmm@coef)
  ),
  path = "res/est/res_est_dep_roa_lead.xlsx"
)


debug = function(){
  for (i in 1:nrow(tmp)) {
    for (c in 1:9) {
      if(tmp[i,c]>1){
        print(paste0("row: ", i, " ",  rownames(tmp)[i], "; col: ", c, " " , colnames(tmp)[c], "; value: ", tmp[i,c]))
      }
    }
  }
  tmp = table(index(panel_data), useNA = "ifany")
  tmp = df_use_long3 %>% dplyr::filter(edinetCode == "E00610" & fyear == "2020")
  colnames(df_use_long3)
  
  # データフレーム内の欠損値を含む行を特定
  rows_with_missing_values <- which(rowSums(is.na(panel_data)) > 0)
  # データセット内の欠損値を持つ列名のリストを取得
  
  ### descriptive stat.
  mean(panel_data_tone_adj$tone_adj)
  psych::describe(panel_data_tone_adj)
  cols = c("roa", "ret", "acc", "size", "mtb", "retvol", "roavol", "nbseg", "ngseg", "age", "si", "tone", "tone_adj", "tone_est")
  df_corr = panel_data_tone_adj %>% 
    dplyr::select(cols) %>% 
    dplyr::mutate_all(as.numeric)
  
  ### plt
  df_plt = panel_data_tone_adj %>% dplyr::select(c("tone", "tone_est"))
  ggplot(df_plt, aes(x = tone, y = tone_est)) + 
    geom_point()
  cormat = cor(df_corr, use = "pairwise.complete.obs")
  corrplot::corrplot(cormat)
  write.csv(cormat, "res/desc/cormat_imp.csv")
  # PCAの実行
  pca_result <- prcomp(df_plt, center = TRUE, scale. = TRUE)
  summary(pca_result)
  # 主成分の寄与率
  pca_result$rotation
  pca_var <- pca_result$sdev^2
  pca_var_ratio <- pca_var / sum(pca_var)
  
  # 主成分プロット
  plot(pca_result, type = "l")
}
