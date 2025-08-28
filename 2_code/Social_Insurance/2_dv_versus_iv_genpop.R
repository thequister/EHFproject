library(here)
source(here::here('2_code', '1_libraries_and_settings_global.R'))
here::i_am("2_code/Social_Insurance/2_dv_versus_iv_genpop.R")

gr_clean <- read.csv(here("3_cleaned_data", "general_retail_clean.csv"))

gr_plot <- gr_clean %>%
  select(st_directed, WG_TANF_Benefit, cashfood, ehf_support_both_num, ehf_donate_both_num) %>%
  pivot_longer(cols = c("ehf_support_both_num", "ehf_donate_both_num"), names_to = "dv_vars", values_to = "dv_vals")

p_supp_st <- ggplot(gr_clean) +
  geom_point(aes(y = ehf_support_both_num, x = st_directed)) +
  geom_rug(aes(x = st_directed), alpha=0.1, linewidth = 2, col = "steelblue") +
  labs(x = "State Welfare", title = "EHF Support Level by State Welfare", y = "") +
  scale_y_continuous(labels = c("Not at all supportive", "Moderately supportive", "Very supportive"), 
                     breaks = c(0, 0.5, 1)) +
  theme(axis.text.y = element_text(angle = 45, vjust = -1.5))

p_supp_st <- ggMarginal(p, size=4, type = "histogram", margin = 'y')

ggsave(here(""))

