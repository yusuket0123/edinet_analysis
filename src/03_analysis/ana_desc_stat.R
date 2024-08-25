### descriptive stats.

source("src/01_combine/make_data_ana_tone.R") 

cols = c("roa", "ret", "acc", "size", "mtb", "retvol", "roavol", "nbseg", "ngseg", "age", "si", "tone")
df_corr = df_use_long3 %>% 
  dplyr::select(cols) %>% 
  dplyr::mutate_all(as.numeric)
df_corr = df_corr[!apply(df_corr, 1, function(row) any(is.infinite(row))), ]
cormat = cor(df_corr, use = "pairwise.complete.obs")
corrplot::corrplot(cormat)
write.csv(cormat, "res/desc/cormat.csv")

desc = psych::describe(df_corr)
write.csv(desc, "res/desc/desc.csv")

df_plt = df_corr %>% dplyr::select(c("tone", "age"))
ggplot(df_plt, aes(x = tone, y = age)) + 
  geom_point()
