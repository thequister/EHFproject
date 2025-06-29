#source("1_libraries_and_settings.R")
#source("2_data_format.R")

#unweighted regressions
ee_lm_wmt <- lm(emergency_expense_num ~ treatment_bin, 
               data = wmt.hq)
ee_lm_wmt_int <- lm(emergency_expense_num ~ treatment_bin*ehf_aware_pretr, 
                   data = wmt.hq)
ee_lm_wmt_c <- lm(emergency_expense_num ~ 
                    treatment_bin*ehf_aware_pretr +age_clean +
                    male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, data = wmt.hq)
ee_ol_uw_int_wmt <- MASS::polr(as.factor(emergency_expense_num)~ treatment_bin*ehf_aware_pretr,
                           data = wmt.hq)

ee.models.wmt <- list(ee_lm_wmt, ee_lm_wmt_int, ee_lm_wmt_c)
names(ee.models.wmt) <- c("Base", "Pre-exposure", "Covariates")

coef_maps <- c(
  "treatment_binTRUE" = "Treated",
  "ehf_aware_pretrTRUE" = "Pre-exposed",
  "treatment_binTRUE:ehf_aware_pretrTRUE" = "Treated x pre-exposed")

rows<-tribble(
  ~"term", ~"Base", ~"Preexposure",  ~"Covariates",
  "Covariates?", "No", "No", "Yes")
attr(rows, 'position') <- c(7)

note2 <- "Robust standard errors in parentheses. Covariates include age, gender race, job tenure, hourly status, full time status, college degree, and main job."
gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2),
  list("raw" = "F", "clean" = "F", "fmt" = 2))


model_print_sec_wmt<- modelsummary( ee.models.wmt,
                                     coef_map = coef_maps,
                                     gof_map = gm,
                                     vcov = "robust",
                                     #add_rows = rows,
                                     title = "OLS regression estimates of treatment effects on subjective financial security (Walmart sample) \\label{tab:tab-finsec-wmt}",
                                     notes = list(note2),
                                     output = "kableExtra",
                                     threeparttable=TRUE,
                                     stars = c('*' = .05, '**' = .01),
                                    escape = FALSE
)




