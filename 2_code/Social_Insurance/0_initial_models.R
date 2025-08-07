library(here)
source(here::here('2_code', '1_libraries_and_settings_global.R'))
here::i_am("2_code/Social_Insurance/0_initial_models.R")

## Read in the data
gr_clean <- read.csv(here("3_cleaned_data", "general_retail_clean.csv"))

### General Population Models --------

## EHF Support
summary(with(gr_clean, lm(ehf_support_new_num ~ replacement_2024_ui + recipiency_2024_ui + WG_TANF)))

ehf_supp_mod <- with(gr_clean, lm(ehf_support_new_num ~ replacement_2024_ui + recipiency_2024_ui + WG_TANF
                                  + income + home_ownership +
                    other_welfare + ideology_conlib_num + age_clean + male +
                    college + practice_religion_num + nonwhite))

with(gr_clean, lm(ehf_support_new_num ~ replacement_2024_ui + replacement_2024_ui:practice_religion_num + 
                            replacement_2024_ui:ideology_conlib_num +
                            income + home_ownership +
                            other_welfare + ideology_conlib_num + age_clean + male +
                            college + practice_religion_num + nonwhite))

modelsummary(ehf_supp_mod,
             title = "Support for Unemployment Insurance",
             vcov = "robust",
             threeparttable=TRUE,
             stars = c('*' = .05, '**' = .01),
             escape = FALSE
)

## EHF Donation
gr_clean$ehf_donate_new_fac <- factor(gr_clean$ehf_donate_new, levels =
                                        c("I would not donate at all",
                                          "A one-time donation of less than $20",
                                          "A one-time donation of more than $20",
                                          "A regular donation of less than $20",
                                          "A regular donation of more than $20"),
                                      ordered = T)

gr_clean$ehf_donate_new_num <- (as.numeric(gr_clean$ehf_donate_new_fac) - 1)/4

summary(with(gr_clean, lm(ehf_donate_new_num ~ replacement_2024_ui)))

summary(with(gr_clean, lm(ehf_donate_new_num ~ replacement_2024_ui + income + home_ownership +
                            other_welfare + ideology_conlib_num + age_clean + male +
                            college + practice_religion_num + nonwhite)))

summary(with(gr_clean, lm(ehf_donate_new_num ~ replacement_2024_ui + replacement_2024_ui:practice_religion_num + 
                            replacement_2024_ui:ideology_conlib_num +
                            income + home_ownership +
                            other_welfare + ideology_conlib_num + age_clean + male +
                            college + practice_religion_num + nonwhite)))
