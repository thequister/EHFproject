library(here)
source(here::here('2_code', '1_libraries_and_settings_global.R'))
here::i_am("2_code/Social_Insurance/1_correlation_plot.R")

gr_clean <- read.csv(here("3_cleaned_data", "general_retail_clean.csv"))

corr_data_wrk <- gr_clean %>%
  select(replacement_2024_ui_1, replacement_2024_ui_2, avg_wba, max_ui_weeks, worksite) %>%
  unique()

corr_data_res <- gr_clean %>%
  select(WG_TANF, WG_TANF_Benefit, residence) %>%
  unique()

corr_data <- left_join(corr_data_wrk, corr_data_res, by = c("worksite" = "residence")) %>%
  select(-worksite)

pairs.panels(corr_data, ellipses = F, lm = T)

