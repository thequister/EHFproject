#source("1_libraries_and_settings.R")
#source("2_data_format.R")

#still need HQ sample analysis

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


received_reg_wmt <- svyglm(ehf_received =="Yes" ~
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


received_reg_wmt_dei <- svyglm(ehf_received =="Yes" ~
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



