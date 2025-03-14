#source(here::here('2_code', 'general_retail', '1_libraries_and_settings_gr.R'))
#source("2_data_format.R")
# 

genpop <- read_csv(here("3_cleaned_data", "general_retail_clean.csv"))

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
labels <- c(hire_benefits_pto_num = "Paid Time Off",
            hire_benefits_health_num = "Health Insurance",
            hire_benefits_retire_num = "Retirement Plan",
            hire_benefits_parent_num = "Paid Family Leave",
            hire_benefits_union_num = "Union Representation",
            hire_benefits_emerg_num = "Emergency Cash Support",
            hire_benefits_tuition_num = "Tuition Assistance")
legend_labels <- c('1' = 'Serious', '0.5' = 'Some', '0' = 'None') 
colors <- RColorBrewer::brewer.pal(3, "Set2")

# Plot
amenity_plot_gp <- ggplot(long_df, aes(x = benefit_type, fill = level)) +
  geom_bar(position = "stack") +
  scale_x_discrete(labels = labels) +
  #scale_fill_discrete(name = "", 
  #                    labels = legend_labels)+
  scale_fill_manual(values = colors, name = "", labels = legend_labels) +
  labs(
    title = "Level of interest in new job with longer commute but better amenities",
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
    title = "Level of interest in new job with longer commute but better amenities",
    x = "Amenity",
    y = "Count", 
    fill = "Interest") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for readability

ggsave(filename = here::here("4_output", "plots", "amenities_gp.pdf"),
       plot = amenity_plot_gp)
ggsave(filename = here::here("4_output", "plots", "amenities_gp_wgt.pdf"),
       plot = amenity_plot_w_gp)




aware_reg_list_wmt<-svyglm(ehf_aware_pretr ~
                    age_clean +
                    male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = wmt_wgt_f,
                  family = quasibinomial)

know_reg_wmt <- svyglm(ehf_other_recip=="Yes" ~
                     age_clean +
                     male +
                     main_job +
                     tenure_num +
                     nonwhite +
                     fulltime +
                     hourly+
                     college, 
                   design = wmt_wgt_f,
                  family = quasibinomial)

applied_reg_wmt <- svyglm(ehf_applied =="Yes" ~
                        age_clean +
                        male +
                        main_job +
                        tenure_num +
                        nonwhite +
                        fulltime +
                        hourly+
                        college, 
                      design = wmt_wgt_f,
                  family = quasibinomial)


received_reg_wmt <- svyglm(ehf_received_all =="Yes" ~
                         age_clean +
                         male +
                         main_job +
                         tenure_num +
                         nonwhite +
                         fulltime +
                         hourly+
                         college, 
                       design = wmt_wgt_f,
                       maxit = 50,
                  family = quasibinomial)

donate_reg_wmt <- svyglm(ehf_donation=="Yes" ~
                       age_clean +
                       male +
                       main_job +
                       tenure_num +
                       nonwhite +
                       fulltime +
                       hourly+
                       college, 
                     design = wmt_wgt_f,
                     family = quasibinomial)


#sg_mods<-stargazer(aware_reg_list, know_reg, applied_reg, received_reg,donate_reg,
#          title = "Weighted logistic regression of EHF awareness \\& engagement",
#          covariate.labels = c("age", "male", "main job",
          #                      "tenure: 6-12mos", "  1-2 yrs.", 
          #                      "  2-3 yrs.", "  3+ yrs.",
          #                      "nonwhite", "full-time", "hourly",
          #                      "BA/BS+"),
          # dep.var.caption = "",
          # dep.var.labels = c("awareness", "know recipient", "applied", 
          #   "received", "donated"),
          # header = FALSE, no.space = T, digits = 2,
          # label = "tab:awareness-model"
          # )

engage.models_wmt <- list(aware_reg_list_wmt, know_reg_wmt, applied_reg_wmt,
    received_reg_wmt, donate_reg_wmt)

names(engage.models_wmt) <- c("awareness", "know recipient", "applied", 
    "received", "donated")

coef_maps <- c("age_clean" = "age",
               "maleTRUE" = "male",
               "main_jobTRUE" = "main job",
               "tenure_num" = "tenure",
               "nonwhiteTRUE" = "nonwhite",
               "fulltimeTRUE" = "full time",
               "hourlyTRUE" = "hourly",
               "collegeTRUE" = "college")

gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1))
note2 <- "Standard errors in parentheses."


model_print_aware_wmt<- modelsummary::modelsummary( engage.models_wmt,
                               #shape = "rbind",
                               coef_map = coef_maps,
                               gof_map = gm,
                               #vcov = "robust",
                               #add_rows = rows,
                               title = "Weighted logistic regression of EHF awareness and engagement \\label{tab:awareness-model}",
                               output = "kableExtra",
                               notes = list(note2),
                               stars = c('*' = .05, '**' = .01),
                               threeparttable = TRUE
)


## WMT DEI weights

aware_reg_list_wmt_dei<-svyglm(ehf_aware_pretr ~
                             age_clean +
                             male +
                             main_job +
                             tenure_num +
                             nonwhite +
                             fulltime +
                             hourly+
                             college, 
                           design = wmt_wgt_dei,
                           family = quasibinomial)

know_reg_wmt_dei <- svyglm(ehf_other_recip=="Yes" ~
                         age_clean +
                         male +
                         main_job +
                         tenure_num +
                         nonwhite +
                         fulltime +
                         hourly+
                         college, 
                       design = wmt_wgt_dei,
                       family = quasibinomial)

applied_reg_wmt_dei <- svyglm(ehf_applied =="Yes" ~
                            age_clean +
                            male +
                            main_job +
                            tenure_num +
                            nonwhite +
                            fulltime +
                            hourly+
                            college, 
                          design = wmt_wgt_dei,
                          family = quasibinomial)


received_reg_wmt_dei <- svyglm(ehf_received_all =="Yes" ~
                             age_clean +
                             male +
                             main_job +
                             tenure_num +
                             nonwhite +
                             fulltime +
                             hourly+
                             college, 
                           design = wmt_wgt_dei,
                           maxit = 50,
                           family = quasibinomial)

donate_reg_wmt_dei <- svyglm(ehf_donation=="Yes" ~
                           age_clean +
                           male +
                           main_job +
                           tenure_num +
                           nonwhite +
                           fulltime +
                           hourly+
                           college, 
                         design = wmt_wgt_dei,
                         family = quasibinomial)



engage.models_wmt_dei <- list(aware_reg_list_wmt_dei, know_reg_wmt_dei, applied_reg_wmt_dei,
                          received_reg_wmt_dei, donate_reg_wmt_dei)

names(engage.models_wmt_dei) <- c("awareness", "know recipient", "applied", 
                              "received", "donated")

coef_maps <- c("age_clean" = "age",
               "maleTRUE" = "male",
               "main_jobTRUE" = "main job",
               "tenure_num" = "tenure",
               "nonwhiteTRUE" = "nonwhite",
               "fulltimeTRUE" = "full time",
               "hourlyTRUE" = "hourly",
               "collegeTRUE" = "college")

gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1))
note2 <- "Standard errors in parentheses."


model_print_aware_wmt_dei<- modelsummary::modelsummary( engage.models_wmt_dei,
                                                    #shape = "rbind",
                                                    coef_map = coef_maps,
                                                    gof_map = gm,
                                                    #vcov = "robust",
                                                    #add_rows = rows,
                                                    title = "Weighted logistic regression of EHF awareness and engagement \\label{tab:awareness-model}",
                                                    output = "kableExtra",
                                                    notes = list(note2),
                                                    stars = c('*' = .05, '**' = .01),
                                                    threeparttable = TRUE
)



