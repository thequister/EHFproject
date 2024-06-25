#source("1_libraries_and_settings.R")
#source("2_data_format.R")


#unweighted regressions
ee_lm_uw <- lm(emergency_expense_num ~ HDTreatment, 
               data = THD_comp_uw)
ee_lm_uw_int <- lm(emergency_expense_num ~ HDTreatment*EHF_aware_list, 
                   data = THD_comp_uw)
ee_lm_uw_c <- lm(emergency_expense_num ~ 
                   HDTreatment*EHF_aware_list + rk_age + male +
                   main_job + tenure_num + nonwhite + fulltime +
                   hourly+ college, data = THD_comp_uw)
ee_ol_uw_int <- MASS::polr(emergency_expense~ HDTreatment*EHF_aware_list,
                           data = THD_comp_uw)

ee.models.uw <- list(ee_lm_uw, ee_lm_uw_int, ee_lm_uw_c)
names(ee.models.uw) <- c("Base", "Pre-exposure", "Covariates")

coef_maps <- c("HDTreatmenttxt" = "Text treatment",
               "HDTreatmentvid" = "Video treatment",
               "EHF_aware_listTRUE" = "Pre-exposed",
               "HDTreatmenttxt:EHF_aware_listTRUE" = "Text x pre-exposed",
               "HDTreatmentvid:EHF_aware_listTRUE" = "Video x pre-exposed")

rows<-tribble(
  ~"term", ~"Base", ~"Preexposure",  ~"Covariates",
  "Covariates?", "No", "No", "Yes")
attr(rows, 'position') <- c(12)

note2 <- "Robust standard errors in parentheses. Covariates include age, gender race, job tenure, hourly status, full time status, college degree, and main job."
gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2),
  list("raw" = "F", "clean" = "$F$", "fmt" = 2))


model_print_uw<- modelsummary( ee.models.uw,
                                     coef_map = coef_maps,
                                     gof_map = gm,
                                     vcov = "robust",
                                     add_rows = rows,
                                     title = "Ability to meet $400 emergency expense; OLS estimates \\label{tab:tab-finsec}",
                                     notes = list(note2),
                                     output = "kableExtra",
                                     threeparttable=TRUE,
                                     stars = c('*' = .05, '**' = .01)
)




#weighted regressions
ee_reg <- svyglm(emergency_expense_num ~ HDTreatment, 
                   design = THD_comp)

ee_reg_int <- svyglm(emergency_expense_num ~ HDTreatment*EHF_aware_list, 
                   design = THD_comp)

ee_reg_c <- svyglm(emergency_expense_num ~ HDTreatment*EHF_aware_list +
                   rk_age + male + main_job + tenure_num + 
                   nonwhite + fulltime +
                       hourly+
                       college, 
                     design = THD_comp)
ee.models.w <- list(ee_reg, ee_reg_int, ee_reg_c)
names(ee.models.w) <- c("Base", "Pre-exposure", "Covariates")

model_print_weighted_insecurity<- modelsummary( ee.models.w,
              coef_map = coef_maps,
              gof_map = c("nobs", "r.squared", "rmse"),
              vcov = "robust",
              add_rows = rows,
              title = "Ability to meet $400 emergency expense; weighted OLS estimates",
              notes = list(note2),
              output = "kableExtra",
              threeparttable = TRUE,
              stars = c('*' = .05, '**' = .01)
)

