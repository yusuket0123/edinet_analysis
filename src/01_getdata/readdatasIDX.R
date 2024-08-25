#### market index data for causal impact

folder_path = "data/marketidx/"
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# CSVファイルを一括して読み込む
data_list <- lapply(file_list, function(file){
  data = read.csv(file)
  file_name <- gsub("\\.csv$", "", basename(file)) 
  data$idx = file_name
  return(data)
})

df_idx = dplyr::bind_rows(data_list)
