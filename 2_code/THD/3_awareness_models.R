#source("1_libraries_and_settings.R")
#source("2_data_format.R")

aware_reg_list<-svyglm(EHF_aware_list ~
                    rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_comp,
                  family = quasibinomial)

aware_reg_simp <- svyglm(EHF_aware_simp ~
                    rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_comp,
                  family = quasibinomial)

know_reg <- svyglm(HF_know=="Yes" ~
                    rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_comp,
                  family = quasibinomial)

applied_reg <- svyglm(HF_applied =="Yes" ~
                    rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_comp,
                  family = quasibinomial)


received_reg <- svyglm(HF_received =="Yes" ~
                    rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_comp,
                  family = quasibinomial)

received_reg_trt <- svyglm(HF_received =="Yes" ~
                         rk_age + male +
                         main_job +
                         tenure_num +
                         nonwhite +
                         fulltime +
                         hourly+
                         college, 
                       design = THD_comp,
                       family = quasibinomial,
                       subset = HDTreatment != "cntrl")

donate_reg <- svyglm(HF_donate=="Yes" ~
                    rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_comp,
                  family = quasibinomial)


donate_reg_trt <- svyglm(HF_donate=="Yes" ~
                       rk_age + male +
                       main_job +
                       tenure_num +
                       nonwhite +
                       fulltime +
                       hourly+
                       college, 
                     design = THD_comp,
                     family = quasibinomial,
                     subset = HDTreatment != "cntrl")


mfx_aware_thd <- marginaleffects::avg_slopes(aware_reg_list, variables = c("tenure_num", "male"), type = "response")


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

engage.models <- list(aware_reg_list, know_reg, applied_reg,
    received_reg, donate_reg)

names(engage.models) <- c("awareness", "know recipient", "applied", 
    "received", "donated")

coef_maps <- c("rk_age" = "age",
               "maleTRUE" = "male",
               "main_jobTRUE" = "main job",
               "tenure_num" = "tenure",
               "nonwhiteTRUE" = "nonwhite",
               "fulltimeTRUE" = "full time",
               "hourlyTRUE" = "hourly",
               "collegeTRUE" = "college")

gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 0))
note2 <- "Standard errors in parentheses."


model_print<- modelsummary( engage.models,
                               #shape = "rbind",
                               coef_map = coef_maps,
                               gof_map = gm,
                               #vcov = "robust",
                               #add_rows = rows,
                               title = "Weighted logistic regression of Home Depot EHF awareness and engagement \\label{tab:awareness-model}",
                               output = "kableExtra",
                               notes = list(note2),
                               stars = c('*' = .05, '**' = .01),
                               threeparttable = TRUE,
                            escape = FALSE
)


## Among those aware

THD_sub <- subset(THD_comp, EHF_aware_list == TRUE)


know_reg_aware <- svyglm(HF_know=="Yes" ~
                    rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_sub,
                  family = quasibinomial)


donate_reg_aware <- svyglm(HF_donate=="Yes" ~
                    rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_sub,
                  family = quasibinomial)




