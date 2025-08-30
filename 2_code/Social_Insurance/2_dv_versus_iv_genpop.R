#library(here)
#source(here::here('2_code', '1_libraries_and_settings_global.R'))
#here::i_am("2_code/Social_Insurance/2_dv_versus_iv_genpop.R")

gr_clean <- read.csv(here("3_cleaned_data", "general_retail_clean.csv")) %>%
  mutate()

p_supp_st <- ggplot(gr_clean) +
  geom_point(aes(y = ehf_support_both_num, x = st_directed*1000), size = 5, shape=1) +
  geom_smooth(aes(y = ehf_support_both_num, x = st_directed*1000), method = "lm", se = FALSE) +
  labs(x = "State safety net generosity (USD)", title = "EHF support level by State generosity", y = "") +
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

ggsave(filename = 'ui_plot_gr_supp.png',
       plot = p_supp_st,
       path = here('4_output', 'plots'))

p_don_st <- ggplot(gr_clean) +
  geom_point(aes(y = as.numeric(ehf_donate_new_num), x = st_directed*1000), size = 5, shape=1) +
  geom_smooth(aes(y = as.numeric(ehf_donate_new_num), x = st_directed*1000), method = "lm", se = FALSE) +
  labs(x = "State safety net generosity (USD)", title = "Willingness to donate to EHF by State generosity", y = "") +
  scale_y_continuous(
    labels = c("Would not donate", "One time <$20", 
               "One time >$20", "Recurring <$20", "Recurring >$20"),
    breaks = c(0, .25, .5, .75, 1),
    limits = c(0, 1)
  ) +
  coord_cartesian(ylim = c(-0.1, 1.1)) +  # ← Zoom in/out by adjusting these values
  theme(
    axis.text.y = element_text(angle = 45, vjust = -1.5),
    plot.margin = margin(5, 5, 5, 5, "pt")
  )

p_don_st <- ggMarginal(p_don_st, size=5, alpha = 0.3, type = "histogram", margins = "both", 
                        fill = "slateblue")

ggsave(filename = 'ui_plot_gr_don.png',
       plot = p_don_st,
       path = here('4_output', 'plots'))