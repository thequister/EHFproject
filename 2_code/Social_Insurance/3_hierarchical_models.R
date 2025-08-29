library(here)
source(here::here('2_code', '1_libraries_and_settings_global.R'))
here::i_am("2_code/Social_Insurance/2_hierarchical_models.R")

gr_clean <- read.csv(here("3_cleaned_data", "general_retail_clean.csv"))

model_1 <- lm(ehf_support_both_num ~ st_directed + age_clean + nonwhite +
                male + college + tenure_num + fulltime + hourly + main_job,
              data = gr_clean)

model_2 <- lm(ehf_support_both_num ~ st_directed + age_clean + nonwhite +
                male + college + tenure_num + fulltime + hourly + main_job + 
                religious + conservative + income_num,
              data = gr_clean)

# Apply cluster-robust standard errors (vcovCL) and 
# print the coeftest result (which looks like a summary table)
rob_1_alt <- coeftest(model_1, vcov. = vcovCL(model_1, cluster = gr_clean$residence))
rob_2_alt <- coeftest(model_2, vcov. = vcovCL(model_2, cluster = gr_clean$residence))

hier_1 <- with(gr_clean, lmer(ehf_support_both_num ~ st_directed + age_clean + nonwhite +
                                male + college + tenure_num + fulltime + hourly + main_job + 
                                religious + conservative + income_num + (1|residence)))

hier_2 <- with(gr_clean, lmer(ehf_support_both_num ~ st_directed + age_clean + nonwhite +
                                male + college + tenure_num + fulltime + hourly + main_job + 
                                religious + conservative + income_num + conservative:st_directed +
                                (1 + conservative|residence)))

hier_3 <- with(gr_clean, lmer(ehf_support_both_num ~ st_directed + age_clean + nonwhite +
                                male + college + tenure_num + fulltime + hourly + main_job +
                                income_num + (other_welfare == "Yes") + religious + conservative +
                                home_ownership + home_ownership:hpi_5year +
                                (1 + hpi_5year|residence)))

htmlreg(
  l = list(rob_1_alt, rob_2_alt, hier_1, hier_2, hier_3),
  stars = numeric(0),    #No p-value stars
  digits = 2,
  padding = 20, #Add space around columns (you may need to adjust this via trial-and-error)
  omit.coef = c('age_clean|nonwhiteTRUE|maleTRUE|collegeTRUE|tenure_num|fulltimeTRUE|hourlyTRUE|main_jobYes|income_num|other_welfare == "Yes"TRUE|religiousTRUE|(Intercept)'), 
  custom.model.names = c("Model A", "Model B", "Model C", "Model D", "Model E"), 
  custom.coef.names = c("State safety net generosity", "Conservative", "Home owner", 
                        "Not a home owner x house appreciation", "Home owner x house appreciation", "Generosity x conservative"),
  reorder.coef = c(1, 2, 6, 3, 4, 5), #Put intercept at bottom of table
  include.loglik = FALSE, #Omit log-likelihood
  include.bic = FALSE,    #Omit BIC
  include.aic = FALSE,    #Omit BIC
  include.groups = FALSE, #Omit group size
  include.rsq = FALSE,    # Omit R-squared
  include.adjrs = FALSE,  # Omit adjusted R-squared
  include.nobs = FALSE,   # Omit number of observations
  include.deviance = FALSE,
  custom.gof.names = c("Var: Intercept", "Var: Residual", "Var: house appreciation",
                       "Cov: Intercept x house appreciation", "Var: Conservative", "Cov: Intercept x conservative"), # Rename variance component rows
  #reorder.gof = c(3, 5, 6, 4, 2, 1),
  custom.gof.rows = list(Observations = sapply(list(rob_1_alt, rob_2_alt, hier_1, hier_2, hier_3), 
                                               function(x) nobs(x)), 
                         AIC = sapply(list(rob_1_alt, rob_2_alt, hier_1, hier_2, hier_3), 
                                               function(x) AIC(x))),
  caption = "Robust (A, B) and Hierarchical (C, D, E) linear models predicting variation in EHF support based on State safety net generosity",
  caption.above = TRUE, #Move caption above table
  inner.rules = 1, #Include line rule before model-level output
  outer.rules = 1 , #Include line rules around table
  custom.note = "Preregistered covariates omited for brevity when not part of the hierarchical model structure."
)

summary(hier_1)
