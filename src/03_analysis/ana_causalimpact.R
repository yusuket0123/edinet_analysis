### causal impact
library(CausalImpact)
source("src/02_combine/make_data_ana_tone.R") 

### defineing functions
## causal impact
ana_ci = function(df_input, pre_start, threshold, post_end, cov = "yes"){
  if (nrow(df_input) <= 3) {
    warning("Not enough data points for CausalImpact analysis")
    return(NULL)
  }
  
  df_use_ci = df_input %>%
    dplyr::mutate(date = as.Date(.$date),
                  price = as.numeric(.$price))
  # Split the data into pre and post intervention periods
  data_pre <- df_use_ci %>%
    filter(date < as.Date(threshold), date >= as.Date(pre_start))# & condition
  
  data_post <- df_use_ci %>%
    filter(date >= as.Date(threshold), date <= as.Date(post_end))# & condition
  
  if(cov == "yes"){ # 共変量(tone_adj)を含まない場合
    data_pre = data_pre %>% 
      dplyr::filter(!is.na(ave_tone_adj)) %>% # tone_adjを算出した時点に限る
      dplyr::select(price, ave_tone_adj, min_tone_adj, max_tone_adj)
    data_post = data_post %>% 
      dplyr::filter(!is.na(ave_tone_adj)) %>%
      dplyr::select(price, ave_tone_adj, min_tone_adj, max_tone_adj)
    n_pre <- nrow(data_pre)
    n_post <- nrow(data_post)
    data_combined = rbind(data_pre, data_post)  # Combine pre and post data into a single time series
  }else if(cov == "no"){# 共変量(tone_adj)を含まない場合
    data_pre = data_pre %>% pull(price)
    data_post = data_post %>% pull(price)
    n_pre <- length(data_pre)
    n_post <- length(data_post)
    data_combined = c(data_pre, data_post)
  }else{
    message("input 'yes' or 'no'")
  }
  
  # Define the pre and post periods for the CausalImpact analysis
  pre.period <- c(1, n_pre)
  post.period <- c(n_pre + 1, n_pre + n_post)
  #pre.period <- c(as.Date(pre_start), as.Date(pre_end))
  #post.period <- c(as.Date(post_start), as.Date(post_end))
    
  # Run CausalImpact analysis if there's sufficient data
  if (n_post > 0) {
    # Run the Causal Impact analysis
    impact <- CausalImpact::CausalImpact(data_combined, pre.period, post.period, model.args = list(nseasons = 12))
    return(impact)
  } else {
    return(NULL)
  }
}

## Define a function to extract data from CausalImpact
extract_causalimpact_data <- function(impact) {
  if (!is.null(impact)) {
    # Extract the time series data
    plot_data <- impact$series
    
    # Create a data frame for plotting
    data.frame(
      time = 1:nrow(plot_data),
      observed = plot_data$response,
      predicted = plot_data$point.pred,
      lower = plot_data$point.pred.lower,
      upper = plot_data$point.pred.upper,
      diff = plot_data$response - plot_data$point.pred,
      stringsAsFactors = FALSE
    )
  } else {
    # Return an empty data frame if impact is NULL
    data.frame(
      date = as.Date(character()),
      observed = numeric(),
      predicted = numeric(),
      lower = numeric(),
      upper = numeric(),
      stringsAsFactors = FALSE
    )
  }
}

## Combine individual plot data into a single data frame
make_df_plt = function(df, with_cov = "yes"){
  df_out = df %>%
    tidyr::unnest(plot_df) %>%
    dplyr::mutate(ticker = factor(ticker))
  if(with_cov == "yes"){
    df_out = df_out %>% dplyr::rename_with(~paste0(.x, "_with_cov"), -c("ticker", "time"))
  }else if(with_cov == "no"){
    df_out = df_out %>% dplyr::rename_with(~paste0(.x, "_without_cov"), -c("ticker", "time"))
  }else{
    message("please specify either 'yes' or 'no' in the augument 'with_cov'")
    return(NULL)
  }
  return(df_out)
}

## plotting
plt_causalimpact = function(combined_plot_df){
  ggplot(combined_plot_df, aes(x = time)) +
    geom_line(aes(y = observed_with_cov, color = "observed"), size = 1) +
    geom_line(aes(y = predicted_with_cov, color = "predicted"), size = 1, linetype = "dashed", alpha = 0.7) +
    geom_ribbon(aes(ymin = lower_with_cov, ymax = upper_with_cov, fill = "with_cov"), alpha = 0.2) +
    geom_ribbon(aes(ymin = lower_without_cov, ymax = upper_without_cov, fill = "without_cov"), alpha = 0.2) +
    labs(title = "CausalImpact Analysis for All Individuals",
         x = "Time",
         y = "Value",
         color = "category",
         fill = "category") +
    theme_minimal()# +
  #scale_color_manual(values = RColorBrewer::brewer.pal(n = length(unique(combined_plot_df$ticker)), name = "Set1")) +
  #scale_fill_manual(values = RColorBrewer::brewer.pal(n = length(unique(combined_plot_df$ticker)), name = "Set1"))
}

# Plotting
plt_causalimpact_diff = function(y_values, title){
  df_plt = combined_plot_df %>%
    dplyr::select(c("time", y_values)) %>%
    tidyr::pivot_longer(., cols = y_values, names_to = "legend", values_to = "value")
  
  ggplot(df_plt, aes(x = time)) +
    geom_line(aes(y = value, color = legend)) +
    labs(title = title,
         x = "Time",
         y = "Value",
         color = "category",
         fill = "category") +
    theme_minimal() +
    geom_vline(xintercept = 90, color = "black", linetype = "dashed")
}

plt_causalimpact_jgbs = function(df_plt, y_val, title){
  ggplot(df_plt, aes(x = time)) +
    geom_line(aes_string(y = y_val, color = "ticker")) +
    labs(title = title,
         x = "Time",
         y = "Value",
         color = "category",
         fill = "category") +
    theme_minimal() +
    geom_vline(xintercept = 90, color = "black", linetype = "dashed")
}

### output summary datas for saving as xlsx
output_res_ci_xlsx = function(res_with_cov, res_without_cov){
  list_res = list()
  for(i in seq_along(res_with_cov$ticker)){
    list_res[[paste0("with_", res_with_cov$ticker[[i]])]] = res_with_cov$impact[[i]]$summary
    list_res[[paste0("without_", res_without_cov$ticker[[i]])]] = res_without_cov$impact[[i]]$summary
  }
  return(list_res)
}

### proccessing datas
# Extract data from the CausalImpact object
path = str_c("data/marketdata/blmbrg")
data_mrkt = read.csv(paste(path, "historicaldata_market.csv", sep = "/"))
df_use_ci = data_mrkt %>%
  dplyr::mutate(date = format(as.Date(.$date, format = "%m/%d/%Y"), "%Y-%m-01"),
                price = case_when(stringi::stri_detect_regex(.$price, ".*k") == TRUE ~ as.numeric(stringi::stri_replace_all(.$price, regex = "k", replacement = ""))*1000,
                                  TRUE ~ as.numeric(.$price)
                                  )
                ) %>%
  dplyr::select(-"type") %>%
  dplyr::filter(ticker != "bbgworld")

df_filter_date_range =  df_use_ci %>%
  group_by(ticker) %>%
  summarize(min_date = min(date), max_date = max(date)) %>%
  summarize(min_date = max(min_date), max_date = min(max_date))

#impact = ana_ci(df_use_ci)
#plot_df = extract_causalimpact_data(impact)
panel_data_tone_adj_byyear = panel_data_tone_adj %>% 
  dplyr::select(fyear, tone_adj) %>%
  dplyr::group_by(fyear) %>%
  dplyr::summarize(ave_tone_adj = mean(tone_adj, na.rm = TRUE),
                   min_tone_adj = min(tone_adj, na.rm = TRUE),
                   max_tone_adj = max(tone_adj, na.rm = TRUE),
                   sd_tone_adj = sd(tone_adj, na.rm = TRUE)
                   )

results_common = df_use_ci %>%
  dplyr::group_by(ticker) %>%
  dplyr::filter(date >= df_filter_date_range$min_date, date <= df_filter_date_range$max_date) %>%
  dplyr::mutate(year = as.factor(year(date))) %>%
  dplyr::left_join(., panel_data_tone_adj_byyear, by = c("year" = "fyear")) 

results_with_cov = results_common %>%
  do(impact = ana_ci(., pre_start = "2016-09-21", threshold = "2024-03-19", post_end = df_filter_date_range$max_date, cov = "yes")) %>%
  dplyr::mutate(plot_df = list(extract_causalimpact_data(impact)))
results_without_cov = results_common %>%
  do(impact = ana_ci(., pre_start = "2016-09-21", threshold = "2024-03-19", post_end = df_filter_date_range$max_date, cov = "no")) %>%
  dplyr::mutate(plot_df = list(extract_causalimpact_data(impact)))

# save results as xlsx file
list_res = output_res_ci_xlsx(results_with_cov, results_without_cov)
writexl::write_xlsx(list_res, path = "res/causalimpact/res_ci.xlsx")
names(list_res)

# Combine individual plot data into a single data frame
combined_plot_df = results_with_cov %>%
  make_df_plt(., with_cov = "yes") %>%
  dplyr::left_join(.,
                   results_without_cov %>% make_df_plt(., with_cov = "no"),
                   by = c("ticker", "time")
                   ) %>%
  dplyr::mutate(LineType = ifelse(is.na(observed_with_cov), "Predicted_with_cov", "Observed_with_cov"),
                diff_predicted_cov = predicted_with_cov - predicted_without_cov,
                diff_impact_cov = diff_with_cov - diff_without_cov,
                band_with_cov = upper_with_cov - lower_with_cov,
                band_without_cov = upper_without_cov - lower_without_cov
                ) %>% 
  dplyr::mutate(diff_band_cov = band_with_cov - band_without_cov) 


# Optionally, create a new column to differentiate between the observed and predicted lines
### plt JGB
combined_plot_df = combined_plot_df %>%
  dplyr::filter(grepl(pattern = "jgby", ticker, ignore.case = FALSE)) %>%
  dplyr::mutate(ticker = factor(ticker, 
                                   levels = paste0("jgby", 
                                                   sort(as.integer(unlist(stringi::stri_extract_all_regex(unique(combined_plot_df$ticker), "\\d+"))))
                                   )))

### plotting
plt_causalimpact_jgbs(df_plt = combined_plot_df, y_val = "diff_with_cov", title = "共変量有の予測値-観測値差（JGB）")
plt_causalimpact_jgbs(df_plt = combined_plot_df, y_val = "observed_with_cov", title = "観測値（JGB）")

#plt_causalimpact_diff(y_value = "diff_predicted_cov", title = "共変量有-無の予測値の差（JGB）")
#plt_causalimpact_diff(y_value = "diff_impact_cov", title = "共変量有-無の予測値-観測値差（JGB）")
#plt_causalimpact_diff(y_value = "diff_with_cov", title = "共変量有の予測値-観測値差（JGB）")
#plt_causalimpact_diff(y_value = "diff_without_cov", title = "共変量無の予測値-観測値差（JGB）")
#plt_causalimpact_diff(y_value = "predicted_with_cov", title = "共変量有の予測値（JGB）")
#plt_causalimpact_diff(y_value = "predicted_without_cov", title = "共変量無の予測値（JGB）")

### plt nikkei
combined_plot_df = combined_plot_df %>%
  dplyr::filter(grepl(pattern = "nikkei", ticker, ignore.case = FALSE))
plt_causalimpact_diff(y_values = "diff_predicted_cov", title = "共変量有-無の予測値の差（JGB）")
plt_causalimpact_diff(y_values = "diff_impact_cov", title = "共変量有-無の予測値-観測値差（JGB）")
plt_causalimpact_diff(y_values = c("diff_without_cov", "diff_with_cov"), title = "共変量無の予測値-観測値差（nikkei225）")
plt_causalimpact_diff(y_values = c("predicted_with_cov", "predicted_without_cov"), title = "共変量無の予測値（nikkei225）")
plt_causalimpact_diff(y_values = c("band_with_cov", "band_without_cov"), title = "共変量無の予測値（nikkei225）")
results_with_cov$impact

### plotting
plot((results_with_cov %>% dplyr::filter(ticker == "jgby3"))$impact[[1]])
plot((results_without_cov %>% dplyr::filter(ticker == "jgby3"))$impact[[1]])

plot((results_with_cov %>% dplyr::filter(ticker == "jgby3"))$impact[[1]]$model$bsts.model, "coefficients")
plot((results_with_cov %>% dplyr::filter(ticker == "jgby2"))$impact[[1]]$model$bsts.model, "components")
a = (results_with_cov %>% dplyr::filter(ticker == "jgby3"))$impact[[1]]$model$bsts.model
names(a)

debug = function(){
  ## Define the extraction function
  extract_summary <- function(impact) {
    if (!is.null(impact)) {
      summary_impact <- impact$summary
      
      # Extract summary statistics
      data.frame(
        average_effect = summary_impact$AbsEffect,
        cumulative_effect = sum(summary_impact$AbsEffect),
        average_effect_ci_lower = summary_impact$AbsEffect.lower,
        average_effect_ci_upper = summary_impact$AbsEffect.upper,
        cumulative_effect_ci_lower = sum(summary_impact$AbsEffect.lower),
        cumulative_effect_ci_upper = sum(summary_impact$AbsEffect.upper),
        stringsAsFactors = FALSE  # Ensure character columns are not converted to factors
      )
    } else {
      return(data.frame(
        average_effect = NA,
        cumulative_effect = NA,
        average_effect_ci_lower = NA,
        average_effect_ci_upper = NA,
        cumulative_effect_ci_lower = NA,
        cumulative_effect_ci_upper = NA,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Extract summaries for all individual results
  summaries <- results %>%
    rowwise() %>%
    dplyr::mutate(summary = list(extract_summary(impact))) %>%
    unnest(cols = c(summary))
  
  # Extract and combine summary statistics
  aggregate_results <- summaries %>%
    summarize(
      average_effect = mean(average_effect),
      cumulative_effect = mean(cumulative_effect),
      average_effect_ci_lower = mean(average_effect_ci_lower),
      average_effect_ci_upper = mean(average_effect_ci_upper),
      cumulative_effect_ci_lower = mean(cumulative_effect_ci_lower),
      cumulative_effect_ci_upper = mean(cumulative_effect_ci_upper)
    )
  
  results_without_cov = df_use_ci %>%
    dplyr::group_by(ticker) %>%
    dplyr::filter(date >= df_filter_date_range$min_date, date <= df_filter_date_range$max_date) %>%
    dplyr::mutate(year = as.factor(year(date))) %>%
    dplyr::left_join(., panel_data_tone_adj_byyear, by = c("year" = "fyear")) %>%
    do(impact = ana_ci(., pre_start = "2016-09-21", threshold = "2024-03-19", post_end = df_filter_date_range$max_date, cov = "no")) %>%
    dplyr::mutate(plot_df = list(extract_causalimpact_data(impact)))
  
  results_with_cov = df_use_ci %>%
    dplyr::group_by(ticker) %>%
    dplyr::filter(date >= df_filter_date_range$min_date, date <= df_filter_date_range$max_date) %>%
    dplyr::mutate(year = as.factor(year(date))) %>%
    dplyr::left_join(., panel_data_tone_adj_byyear, by = c("year" = "fyear")) %>%
    do(impact = ana_ci(., pre_start = "2016-09-21", threshold = "2024-03-19", post_end = df_filter_date_range$max_date, cov = "yes")) %>%
    dplyr::mutate(plot_df = list(extract_causalimpact_data(impact)))
  tmp = results_with_cov %>% dplyr::filter(ticker == "dowjones")
  ana_ci(df_input = tmp, pre_start = "2016-09-21", threshold = "2024-03-19", post_end = df_filter_date_range$max_date, cov = "yes")
  results_with_cov
}

summary(lm(tone_adj ~ fyear, panel_data_tone_adj))
write.csv(panel_data_tone_adj_byyear, "res/desc/res_desc_tone_adj_byyear.csv")

a = lm(price ~ ave_tone_adj + min_tone_adj + max_tone_adj + sd_tone_adj + as.factor(date), 
   results_common %>% dplyr::filter(grepl(pattern = "jgby1", ticker, ignore.case = FALSE)))
broom::tidy(a)

data_reg_jgby = results_common %>% dplyr::filter(grepl(pattern = "jgby", ticker, ignore.case = FALSE))
reg_jgby = lm(price ~ ave_tone_adj + min_tone_adj + max_tone_adj + as.factor(date)+ as.factor(ticker), data_reg)
data_reg_nikkei = results_common %>% dplyr::filter(grepl(pattern = "nikkei", ticker, ignore.case = FALSE))
reg_nikkei = lm(price ~ ave_tone_adj + min_tone_adj + max_tone_adj + as.factor(date)+ as.factor(ticker), data_reg)

writexl::write_xlsx(
  list(
    tidy_jgby = broom::tidy(reg_jgby),
    glance_jgby = broom::glance(reg_jgby),
    tidy_nikkei = broom::tidy(reg_nikkei),
    glance_nikkei = broom::glance(reg_nikkei)
  ),
  path = "res/causalimpact/res_est_dep_index_exp_tone_adj.xlsx"
)
