#library(here)
#source(here::here('2_code', '1_libraries_and_settings_global.R'))
#here::i_am("2_code/Social_Insurance/2_hierarchical_models.R")

gr_clean <- read.csv(here("3_cleaned_data", "general_retail_clean.csv"))

## EHF Support Models
model_1 <- lm(ehf_support_both_num ~ st_directed + age_clean + nonwhite +
                male + college + tenure_num + fulltime + hourly + main_job + has_ehf,
              data = gr_clean)

model_2 <- lm(ehf_support_both_num ~ st_directed + age_clean + nonwhite +
                male + college + tenure_num + fulltime + hourly + main_job + 
                religious + conservative + income_num + has_ehf,
              data = gr_clean)

rob_1_alt_supp <- coeftest(model_1, vcov. = vcovCL(model_1, cluster = gr_clean$residence))
rob_2_alt_supp <- coeftest(model_2, vcov. = vcovCL(model_2, cluster = gr_clean$residence))

hier_1_supp <- with(gr_clean, lmer(ehf_support_both_num ~ st_directed + age_clean + nonwhite +
                                     male + college + tenure_num + fulltime + hourly + main_job +
                                     (other_welfare == "Yes") +
                                     religious + conservative + income_num + has_ehf + (1|residence)))

hier_2_supp <- with(gr_clean, lmer(ehf_support_both_num ~ st_directed + age_clean + nonwhite +
                                     male + college + tenure_num + fulltime + hourly + main_job + 
                                     religious + conservative + income_num + (other_welfare == "Yes") +
                                     conservative:st_directed +
                                     has_ehf + (1 + conservative|residence)))

hier_3_supp <- with(gr_clean, lmer(ehf_support_both_num ~ st_directed + age_clean + nonwhite +
                                     male + college + tenure_num + fulltime + hourly + main_job +
                                     income_num + (other_welfare == "Yes") + religious + conservative +
                                     home_ownership + home_ownership:hpi_5year +
                                     has_ehf + (1 + hpi_5year|residence)))


supp_mods <- list(rob_1_alt_supp, rob_2_alt_supp, hier_1_supp, hier_2_supp, hier_3_supp)

names(supp_mods) <- c("Model A", "Model B", "Model C", "Model D", "Model E")

coef_maps <- c(
  "st_directed" = "State safety net generosity",
  "has_ehfTRUE" = "Has an EHF",
  "conservativeTRUE" = "Conservative", 
  'st_directed:conservativeTRUE' = "Generosity x Conservative",
  "home_ownershipTRUE" = "Home owner",
  "home_ownershipFALSE:hpi_5year" = "Not a home owner x house appreciation", 
  "home_ownershipTRUE:hpi_5year" = "Home owner x house appreciation",
  "SD (Intercept residence)"= "Var: intercept",
  "SD (Observations)" = "Var: residual",
  "SD (hpi_5year residence)" = "Var: house appreciation",
  "SD (conservativeTRUE residence)" = "Var: conservative"
  )

AIC <- str_sub(as.character(sapply(list(rob_1_alt_supp, rob_2_alt_supp, hier_1_supp, 
                                        hier_2_supp, hier_3_supp), 
                                   function(x) AIC(x))), , 3)

rows<-tribble(
  ~"",~"",~"",~"",~"",~"",
  "Model Type", "Cluster-robust", "Cluster-robust", "HLM", "HML","HLM", 
  "AIC", AIC[1], AIC[2], AIC[3], AIC[4], AIC[5])
attr(rows, 'position') <- c(19)

note1 <- "Additional covariates include age, gender race, job tenure, hourly and full time status, college degree, main job, income, religiosity."
#note2 <- "Robust standard errors in parentheses."

gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0))

mod_tab_supp <-modelsummary(supp_mods,
                                   #shape = term + response ~ statistic,
                                   coef_map = coef_maps,
                                   title = "Support for EHF among retail workers by state level safety net generosity models. There is no detected state level variation.  \\label{tab:tab-genpop-ui-models}",
                                   gof_map = gm,
                                   add_rows = rows,
                                   notes = list(note1),
                                   output = "kableExtra",
                                   threeparttable=TRUE, 
                                   #stars = c('*' = .05, '**' = .01),
                                   escape = FALSE
)



## EHF Donation Models
model_1 <- lm(ehf_donate_new_num ~ st_directed + age_clean + nonwhite +
                male + college + tenure_num + fulltime + hourly + main_job,
              data = gr_clean)

model_2 <- lm(ehf_donate_new_num ~ st_directed + age_clean + nonwhite +
                male + college + tenure_num + fulltime + hourly + main_job + 
                religious + conservative + income_num,
              data = gr_clean)

rob_1_alt_don <- coeftest(model_1, vcov. = vcovCL(model_1, cluster = gr_clean$residence))
rob_2_alt_don <- coeftest(model_2, vcov. = vcovCL(model_2, cluster = gr_clean$residence))

hier_1_don <- with(gr_clean, lmer(ehf_donate_new_num ~ st_directed + age_clean + nonwhite +
                                    male + college + tenure_num + fulltime + hourly + main_job + 
                                    (other_welfare == "Yes") +
                                    religious + conservative + income_num + (1|residence)))

hier_2_don <- with(gr_clean, lmer(ehf_donate_new_num ~ st_directed + age_clean + nonwhite +
                                    male + college + tenure_num + fulltime + hourly + main_job + 
                                    religious + conservative + income_num + conservative:st_directed +
                                    (other_welfare == "Yes") +
                                    (1 + conservative|residence)))

hier_3_don <- with(gr_clean, lmer(ehf_donate_new_num ~ st_directed + age_clean + nonwhite +
                                    male + college + tenure_num + fulltime + hourly + main_job +
                                    income_num + (other_welfare == "Yes") + religious + conservative +
                                    home_ownership + home_ownership:hpi_5year +
                                    (1 + hpi_5year|residence)))

don_mods <- list(rob_1_alt_don, rob_2_alt_don, hier_1_don, hier_2_don, hier_3_don)

names(don_mods) <- c("Model A", "Model B", "Model C", "Model D", "Model E")

coef_maps <- c(
  "st_directed" = "State safety net generosity",
  "has_ehfTRUE" = "Has an EHF",
  "conservativeTRUE" = "Conservative", 
  'st_directed:conservativeTRUE' = "Generosity x Conservative",
  "home_ownershipTRUE" = "Home owner",
  "home_ownershipFALSE:hpi_5year" = "Not a home owner x house appreciation", 
  "home_ownershipTRUE:hpi_5year" = "Home owner x house appreciation",
  "SD (Intercept residence)"= "Var: intercept",
  "SD (Observations)" = "Var: residual",
  "SD (hpi_5year residence)" = "Var: house appreciation",
  "SD (conservativeTRUE residence)" = "Var: conservative"
)

AIC <- str_sub(as.character(sapply(list(rob_1_alt_don, rob_2_alt_don, hier_1_don, 
                                        hier_2_don, hier_3_don), 
                                   function(x) AIC(x))), , 3)

rows<-tribble(
  ~"",~"",~"",~"",~"",~"",
  "Model Type", "Cluster-robust", "Cluster-robust", "HLM", "HML","HLM", 
  "AIC", AIC[1], AIC[2], AIC[3], AIC[4], AIC[5])
attr(rows, 'position') <- c(19)

note1 <- "Additional covariates include age, gender race, job tenure, hourly and full time status, college degree, main job, income, religiosity."
#note2 <- "Robust standard errors in parentheses."

gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0))

mod_tab_don <-modelsummary(supp_mods,
                            #shape = term + response ~ statistic,
                            coef_map = coef_maps,
                            title = "Willingness to donate to EHF among retail workers who do not have an EHF, by state level safety net generosity models. There is no detected state level variation.  \\label{tab:tab-genpop-ui-models}",
                            gof_map = gm,
                            add_rows = rows,
                            notes = list(note1),
                            output = "kableExtra",
                            threeparttable=TRUE, 
                            #stars = c('*' = .05, '**' = .01),
                            escape = FALSE
)
