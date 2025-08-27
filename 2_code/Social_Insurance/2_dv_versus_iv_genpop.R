library(here)
source(here::here('2_code', '1_libraries_and_settings_global.R'))
here::i_am("2_code/Social_Insurance/2_dv_versus_iv_genpop.R")

gr_clean <- read.csv(here("3_cleaned_data", "general_retail_clean.csv"))

gr_plot <- gr_clean %>%
  select(st_directed, WG_TANF_Benefit, cashfood, ehf_support_both_num, ehf_donate_both_num) %>%
  pivot_longer(cols = c("st_directed", "WG_TANF_Benefit", "cashfood"), names_to = "iv_vars", values_to = "iv_vals") %>%
  pivot_longer(cols = c("ehf_support_both_num", "ehf_donate_both_num"), names_to = "dv_vars", values_to = "dv_vals")

ggplot(gr_plot) +
  geom_boxplot(aes(group = dv_vals, y = iv_vals)) +
  facet_grid(rows = vars(iv_vars), cols = vars(dv_vars), scales = "free")



