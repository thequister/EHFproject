#source(here::here('2_code', 'general_retail', '1_libraries_and_settings_gr.R'))
#source("2_data_format.R")
# 

genpop <- read_csv(here("3_cleaned_data", "general_retail_clean.csv"))


# Overall answers plot

round(sum(genpop$acs_weight_trim[genpop$ehf_own_comp=="Yes"])) #200
round(sum(genpop$acs_weight_trim[genpop$ehf_own_comp!="Yes"])) #804

#round(sum(genpop$acs_weight_trim[genpop$emerg_assist_benefits=="Yes"])) #230
#round(sum(genpop$acs_weight_trim[genpop$emerg_assist_benefits!="Yes"])) #775


gr_pl3.2 <- genpop %>%
  mutate(eab_yes = if_else(
    emerg_assist_benefits == "Yes",
    "claims EHF (n = 200)",
    "doesn't claim EHF (n = 804)"
  ),
  eab_yes = factor(eab_yes)
  ) |> 
  select(ehf_offer_thd:ehf_offer_costco, acs_weight_trim, eab_yes) %>%
  pivot_longer(cols = c(ehf_offer_thd:ehf_offer_costco), 
               names_to = "Q", 
               values_to = "ans")
gr_pl3.2_w <- gr_pl3.2 |> 
  as_survey_design(ids = 1, weights = acs_weight_trim) 

firms_summary <- gr_pl3.2_w |> 
  group_by(eab_yes, Q, ans) |>
  summarize(
    prop = survey_mean(vartype = "ci", na.rm = TRUE),
    .groups = "drop") |> 
  mutate(Q = case_match(Q, "ehf_offer_costco" ~ "Costco", 
                        "ehf_offer_disn" ~ "Disney",
                        "ehf_offer_kohls" ~ "Kohl's",
                        "ehf_offer_stb" ~ "Starbucks",
                        "ehf_offer_thd" ~ "Home Depot",
                        "ehf_offer_wal" ~ "Walmart"))

my_colors <- RColorBrewer::brewer.pal(3, "Set2")

ehf_firm_beliefs <- ggplot(firms_summary, aes(x = Q, y = prop, fill = ans)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(
    aes(ymin = prop_low, ymax = prop_upp),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  facet_grid(.~eab_yes) +
  scale_fill_manual(values = my_colors) +
  labs(
    #title = "Beliefs about EHFs at major retailers, by own EHF status",
    y = "",
    x = ""
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(title = NULL))

ggsave(ehf_firm_beliefs, 
       filename = here::here("4_output", "plots", "genpop_ehf_firm_beliefs.pdf")
)

# Support

# EHF Support Plot
gr_pl2.1 <- genpop %>%
  select(ehf_support_exist, ehf_support_new, acs_weight_trim) %>%
  pivot_longer(cols = c(ehf_support_exist:ehf_support_new), 
               names_to = "Q", 
               values_to = "ans")

gr_pl2.1_w <- gr_pl2.1 |> 
  as_survey_design(ids = 1, weights = acs_weight_trim) 


support_sum <-  gr_pl2.1_w |> 
  group_by(Q, ans) %>%
  filter(!is.na(ans)) |> 
  summarize(
    prop = survey_mean(vartype = "ci", na.rm = TRUE),
    .groups = "drop") |> 
  mutate(Q = case_match(Q, 
                        "ehf_support_exist" ~ "claims EHF (N = 200)", 
                        "ehf_support_new" ~ "doesn't claim EHF (N = 804)"))

my_colors <- RColorBrewer::brewer.pal(2, "Set2")

ehf_support_genpop <- ggplot(support_sum, aes(x = ans, y = prop, fill = Q)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(
    aes(ymin = prop_low, ymax = prop_upp),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  facet_grid(.~Q) +
  scale_fill_manual(values = my_colors) +
  labs(
    #title = "Support for introduction of EHF, by own EHF status",
    y = "Proportion",
    x = ""
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" 
  )

ggsave(ehf_support_genpop, 
       filename = here::here("4_output", "plots", "support_gp_w.pdf")
       )

tt<-svyttest(I(ans == "Extremely supportive") ~ Q, design = gr_pl2.1_w) # for pvalue in paper


# Control

gr_pl2.2 <- genpop %>%
  select(ehf_wrk_new, acs_weight_trim) %>%
  mutate(ehf_wrk_new =  
           factor(ehf_wrk_new, 
                  levels = 
                    c("Only management should control the fund",
                      "Management should control the fund with worker input",
                      "Workers and management should share control equally",
                      "Workers should control the fund with management input",
                      "Only workers should control the fund"),
                  ordered= TRUE)
         ) %>% 
  pivot_longer(cols = c(ehf_wrk_new), 
               names_to = "Q", 
               values_to = "ans")

gr_pl2.2_w <- gr_pl2.2 |> 
  as_survey_design(ids = 1, weights = acs_weight_trim) 

control_sum <-  gr_pl2.2_w |> 
  group_by(Q, ans) %>% 
  filter(!is.na(ans)) |> 
  summarize(
    prop = survey_mean(vartype = "ci", na.rm = TRUE),
    .groups = "drop") %>% 
  mutate(cntrllev = factor(c("full mgmt", 
                              "more mgmt", 
                              "equal", 
                              "more worker", 
                              "full worker"),
                      levels = c("full mgmt", 
                                 "more mgmt", 
                                 "equal", 
                                 "more worker", 
                                 "full worker"),
                      ordered=T)
           )

genpop_ehf_cntrl <- ggplot(control_sum,
  aes(x = cntrllev, y = prop)) +
  geom_col(position = position_dodge(width = 0.9), 
           width = 0.7,
           alpha = 0.75, fill = "grey") +
  geom_errorbar(
    aes(ymin = prop_low, ymax = prop_upp),
    width = 0.2,
    position = position_dodge(width = 0.9)
  ) +
  labs(#title = "Preferred worker control over EHF" 
       #subtitle = "N = 804, weighted estimates)"
       ) +
  scale_x_discrete(breaks = 
                     c("full mgmt", 
                       "equal", 
                       "full worker")) +
  ylab("Proportion") +
  xlab("")
  #theme(legend.position = "none",  # Hide the legend if not necessary
   #     axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(plot = genpop_ehf_cntrl, 
       filename =here::here("4_output","plots", "genpop_ehfcontrol.pdf"))



#Amenities

genpop$hire_benefits_pto_num <- as.ordered(genpop$hire_benefits_pto_num)
genpop$hire_benefits_health_num <- as.ordered(genpop$hire_benefits_health_num)
genpop$hire_benefits_retire_num <- as.ordered(genpop$hire_benefits_retire_num)
genpop$hire_benefits_parent_num <- as.ordered(genpop$hire_benefits_parent_num)
genpop$hire_benefits_union_num <- as.ordered(genpop$hire_benefits_union_num)
genpop$hire_benefits_emerg_num <- as.ordered(genpop$hire_benefits_emerg_num)
genpop$hire_benefits_tuition_num <- as.ordered(genpop$hire_benefits_tuition_num)

# Reshape the data
long_df <- genpop %>%
  pivot_longer(cols = c(hire_benefits_pto_num, hire_benefits_health_num, hire_benefits_retire_num,
                        hire_benefits_parent_num, hire_benefits_union_num, hire_benefits_emerg_num, 
                        hire_benefits_tuition_num),
               names_to = "benefit_type", values_to = "level")

# Calculate proportions of level "1" for each benefit_type
proportions_uw <- long_df %>%
  group_by(benefit_type) %>%
  summarise(prop = mean(level == "1"), .groups = "drop") %>%
  arrange(desc(prop))  # Sort by descending order of proportion

proportions_w <- long_df %>%
  group_by(benefit_type) %>%
  summarise(prop = weighted.mean(level == "1", acs_weight_trim), .groups = "drop") %>%
  arrange(desc(prop))  # Sort by descending order of proportion

# Use the ordered proportions to sort the factor levels in the plot
long_df$benefit_type <- factor(long_df$benefit_type, levels = proportions_uw$benefit_type)
long_df$benefit_type_w <- factor(long_df$benefit_type, levels = proportions_w$benefit_type)

# Custom labels for the x-axis
labels <- c(hire_benefits_pto_num = "Paid time off",
            hire_benefits_health_num = "Health insurance",
            hire_benefits_retire_num = "Retirement lan",
            hire_benefits_parent_num = "Paid family leave",
            hire_benefits_union_num = "Union representation",
            hire_benefits_emerg_num = "Emergency cash support",
            hire_benefits_tuition_num = "Tuition assistance")
legend_labels <- c('1' = 'Very', '0.5' = 'Somewhat', '0' = 'None') 
colors <- RColorBrewer::brewer.pal(3, "Set2")

# Plot
amenity_plot_gp <- ggplot(long_df, aes(x = benefit_type, fill = level)) +
  geom_bar(position = "stack") +
  scale_x_discrete(labels = labels) +
  #scale_fill_discrete(name = "", 
  #                    labels = legend_labels)+
  scale_fill_manual(values = colors, name = "", labels = legend_labels) +
  labs(
    #title = "Level of interest in new job with longer commute but better amenities",
    x = "Amenity",
    y = "Count", 
    fill = "Interest") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for readability


amenity_plot_w_gp <- ggplot(long_df, aes(x = benefit_type_w, fill = level)) +
  geom_bar(aes(weight = acs_weight_trim), position = "stack") +
  scale_x_discrete(labels = labels) +
  #scale_fill_discrete(name = "", 
  #                    labels = legend_labels)+
  scale_fill_manual(values = colors, name = "", labels = legend_labels) +
  labs(
    #title = "Interest in new job with longer commute but better amenities",
    x = "Amenity",
    y = "Count", 
    fill = "Interest") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for readability

ggsave(filename = here::here("4_output", "plots", "amenities_gp.pdf"),
       plot = amenity_plot_gp)
ggsave(filename = here::here("4_output", "plots", "amenities_gp_wgt.pdf"),
       plot = amenity_plot_w_gp)




