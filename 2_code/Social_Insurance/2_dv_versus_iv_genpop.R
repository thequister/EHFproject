library(here)
source(here::here('2_code', '1_libraries_and_settings_global.R'))
here::i_am("2_code/Social_Insurance/2_dv_versus_iv_genpop.R")

gr_clean <- read.csv(here("3_cleaned_data", "general_retail_clean.csv")) %>%
  mutate()

gr_plot <- gr_clean %>%
  select(st_directed, WG_TANF_Benefit, cashfood, ehf_support_both_num, ehf_donate_both_num) %>%
  pivot_longer(cols = c("ehf_support_both_num", "ehf_donate_both_num"), names_to = "dv_vars", values_to = "dv_vals")


p_supp_st <- ggplot(gr_clean) +
  geom_point(aes(y = ehf_support_both_num, x = st_directed)) +
  geom_smooth(aes(y = ehf_support_both_num, x = st_directed), method = "lm", se = FALSE) +
  labs(x = "State safety net generosity", title = "EHF support Level by State safety net generosity", y = "") +
  scale_y_continuous(
    labels = c("Not at all supportive", "Moderately supportive", "Very supportive"), 
    breaks = c(0, 0.5, 1),
  ) +
  scale_x_continuous(expand = c(0, 0)) +  # Also remove padding on x-axis
  coord_cartesian(ylim = c(-0.1, 1.1)) +  # ← Zoom in/out by adjusting these values
  theme(
    axis.text.y = element_text(angle = 45, vjust = -1.5),
    plot.margin = margin(5, 5, 5, 5, "pt")  # Reduce plot margins (top, right, bottom, left)
  )

p_supp_st <- ggMarginal(p_supp_st, size=5, alpha = 0.3, type = "histogram", margins = "both", 
                        fill = "slateblue")

ggsave(here("4_output", "plots", "socins_gr_supp_st_directed.pdf"))

p_don_st <- ggplot(gr_clean) +
  geom_point(aes(y = as.numeric(ehf_donate_both_num), x = st_directed)) +
  geom_smooth(aes(y = as.numeric(ehf_donate_both_num), x = st_directed), method = "lm", se = FALSE) +
  labs(x = "State safety net generosity", title = "Willingness to donate to EHF by State safety net generosity", y = "") +
  scale_y_continuous(
    labels = c("Would not donate", "Would donate"),
    breaks = c(0, 1),
    limits = c(0, 1)
  ) +
  coord_cartesian(ylim = c(-0.1, 1.1)) +  # ← Zoom in/out by adjusting these values
  theme(
    axis.text.y = element_text(angle = 45, vjust = -1.5),
    plot.margin = margin(5, 5, 5, 5, "pt")
  )

p_don_st <- ggMarginal(p_don_st, size=5, alpha = 0.3, type = "histogram", margins = "both", 
                        fill = "slateblue")

ggsave(here("4_output", "plots", "socins_gr_donate_st_directed.pdf"))


