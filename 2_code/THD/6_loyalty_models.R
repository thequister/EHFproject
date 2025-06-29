#source("1_libraries_and_settings.R")
#source("2_data_format.R")


#unweighted models
ai_thd_uw <- lm(attachment_index  ~ HDTreatment, 
                     data = THD_comp_uw)
ai_int_thd_uw <- lm(attachment_index  ~ HDTreatment*EHF_aware_list, 
                         data = THD_comp_uw)
ai_c_thd_uw <- lm(attachment_index  ~ 
                         HDTreatment*EHF_aware_list + rk_age + male +
                         main_job + tenure_num + nonwhite + fulltime +
                         hourly+ college, data = THD_comp_uw)

wrkloyal_lm_uw <- lm(wrk_loyal_num ~ HDTreatment, 
               data = THD_comp_uw)
wrkloyal_lm_int_uw <- lm(wrk_loyal_num ~ HDTreatment*EHF_aware_list, 
                   data = THD_comp_uw)
wrkloyal_lm_c_uw <- lm(wrk_loyal_num ~ 
                   HDTreatment*EHF_aware_list + rk_age + male +
                   main_job + tenure_num + nonwhite + fulltime +
                   hourly+ college, data = THD_comp_uw)
wrkloyal_ol_int_uw <- MASS::polr(wrk_loyal~ HDTreatment*EHF_aware_list,
                           data = THD_comp_uw)

wl.models.uw <- list(wrkloyal_lm_uw, wrkloyal_lm_int_uw, wrkloyal_lm_c_uw)
names(wl.models.uw) <- c("Base", "Pre-exposure", "Covariates")

employal_lm_uw <- lm(emp_loyal_num ~ HDTreatment, 
                     data = THD_comp_uw)
employal_lm_int_uw <- lm(emp_loyal_num ~ HDTreatment*EHF_aware_list, 
                         data = THD_comp_uw)
employal_lm_c_uw <- lm(emp_loyal_num ~ 
                         HDTreatment*EHF_aware_list + rk_age + male +
                         main_job + tenure_num + nonwhite + fulltime +
                         hourly+ college, data = THD_comp_uw)
employal_ol_int_uw <- MASS::polr(emp_loyal~ HDTreatment*EHF_aware_list,
                                 data = THD_comp_uw)

el.models.uw <- list(employal_lm_uw, employal_lm_int_uw, employal_lm_c_uw)
names(el.models.uw) <- c("Base", "Pre-exposure", "Covariates")

empreco_lm_uw <- lm(emp_reco_num ~ HDTreatment, 
                     data = THD_comp_uw)
empreco_lm_int_uw <- lm(emp_reco_num ~ HDTreatment*EHF_aware_list, 
                         data = THD_comp_uw)
empreco_lm_c_uw <- lm(emp_reco_num ~ 
                         HDTreatment*EHF_aware_list + rk_age + male +
                         main_job + tenure_num + nonwhite + fulltime +
                         hourly+ college, data = THD_comp_uw)
empreco_ol_int_uw <- MASS::polr(emp_reco~ HDTreatment*EHF_aware_list,
                                 data = THD_comp_uw)

er.models.uw <- list(empreco_lm_uw, empreco_lm_int_uw, empreco_lm_c_uw)
names(el.models.uw) <- c("Base", "Pre-exposure", "Covariates")


coef_maps <- c("HDTreatmenttxt" = "Text treatment",
               "HDTreatmentvid" = "Video treatment",
               "EHF_aware_listTRUE" = "Pre-exposed",
               "HDTreatmenttxt:EHF_aware_listTRUE" = "Text x pre-exposed",
               "HDTreatmentvid:EHF_aware_listTRUE" = "Video x pre-exposed")

rows<-tribble(
  ~"term", ~"Base", ~"Preexposure",  ~"Full",
  "Covariates?", "No", "No", "Yes")
attr(rows, 'position') <- c(41)

note1 <- "Robust standard errors in parentheses. Covariates include age, gender race, job tenure, hourly status, full time status, college degree, and main job."

gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2),
  list("raw" = "F", "clean" = "$F$", "fmt" = 2))

panels <- list(
  "Outcome: co-worker loyalty" = wl.models.uw,
  "Outcome: employer loyalty" = el.models.uw,
  "Outcome: recommend employer" = el.models.uw
)

ai_mods <- list(
  "Base" = ai_thd_uw, 
  "Pre-exposure" = ai_int_thd_uw, 
  "Covariates" = ai_c_thd_uw)

model_print_thd_uw_attachment<- modelsummary( ai_mods,
                                     #shape = term + statistic ~ model,
                                     coef_map = coef_maps,
                                     gof_map = gm,
                                     vcov = "robust",
                                     #add_rows = rows,
                                     title = "OLS regression estimates of treatment effects on job attachment at the Home Depot \\label{tab:tab-aithd-models}",
                                     output = "kableExtra",
                                     notes = list(note1),
                                     stars = c('*' = .05, '**' = .01),
                                     threeparttable=TRUE,
                                     escape = FALSE
)


THDattach_eff<-Effect(c("HDTreatment", "EHF_aware_list"), 
                      ai_c_thd_uw,
                      vcov = sandwich::vcovHC(ai_c_thd_uw),     
                      se = TRUE)

THDattach_eff_plot <- plot(THDattach_eff,
                          main = NULL,
                          #main = "Treament effect on Home Depot job attachment\n by pre-exposure status",
                          xlab = "Experimental condition",
                          axes = list(
                            y = list(type = "response",
                                     lab="predicted value")),
                          ylim = c(-1.2, 1),
                          lattice=list(strip=list(factor.names=FALSE)),
                          colors = grey(0.5)
)

THDattach_eff_plot$condlevels$EHF_aware_list<-c("unaware", "pre-exposed")








model_print_uw_loyal<- modelsummary( panels,
                               shape = "rbind",
                               coef_map = coef_maps,
                               gof_map = gm,
                               vcov = "robust",
                               #add_rows = rows,
                               title = "Coworker and employer attachment, OLS regression \\label{tab:tab-loyalty-models}",
                               output = "kableExtra",
                               notes = list(note1),
                               stars = c('*' = .05, '**' = .01),
                               threeparttable=TRUE,
                               escape = FALSE
)


wrkloyal_eff<-Effect(c("HDTreatment", "EHF_aware_list"), 
    wrkloyal_lm_int_uw,
    vcov = sandwich::vcovHC(wrkloyal_lm_int_uw),     
    se = TRUE)

wrkloyal_eff_plot <- plot(wrkloyal_eff,
     main = "Treament effect on coworker loyalty\n by pre-exposure status",
     xlab = "Experimental condition",
     axes = list(
       y = list(type = "response",
                lab="predicted value")),
     ylim = c(.2, .9),
     lattice=list(strip=list(factor.names=FALSE)),
     colors = grey(0.5)
)

wrkloyal_eff_plot$condlevels$EHF_aware_list<-c("unaware", "aware")

employal_eff<-Effect(c("HDTreatment", "EHF_aware_list"), 
    employal_lm_int_uw,
    vcov = sandwich::vcovHC(employal_lm_int_uw),     
    se = TRUE)

employal_eff_plot <- plot(employal_eff,
     main = "Treament effect on employer loyalty\n by pre-exposure status",
     xlab = "Experimental condition",
     axes = list(
       y = list(type = "response",
                lab="predicted value")),
     ylim = c(.2, .9),
     lattice=list(strip=list(factor.names=FALSE)),
     colors = grey(0.5)
)

employal_eff_plot$condlevels$EHF_aware_list<-c("unaware", "aware")


png(here::here("4_output","plots", "wrkloyal_eff.png"))
print(wrkloyal_eff_plot)
dev.off()

png(here::here("4_output","plots", "employal_eff.png"))
print(employal_eff_plot)
dev.off()


#weighted models
wrkloyal_reg_b<-svyglm(wrk_loyal_num ~
                    HDTreatment, 
                  design = THD_comp)

wrkloyal_reg_b_int<-svyglm(wrk_loyal_num ~
                    HDTreatment*EHF_aware_list +
                      tenure_num, 
                  design = THD_comp)

wrkloyal_reg_c<-svyglm(wrk_loyal_num ~
                    HDTreatment*EHF_aware_list + rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_comp)  # fitted values outside [1,4] 

wrkloyal_reg_polr<-svy_vglm(wrk_loyal ~
                    HDTreatment*EHF_aware_list+ rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_comp,
                  family=propodds())

employal_reg_b<-svyglm(emp_loyal_num ~
                    HDTreatment, 
                  design = THD_comp)

employal_reg_b_int<-svyglm(emp_loyal_num ~
                    HDTreatment*EHF_aware_list, 
                  design = THD_comp)


employal_reg_c<-svyglm(emp_loyal_num ~
                    HDTreatment*EHF_aware_list + rk_age +
                      male + main_job + tenure_num +
                    nonwhite + fulltime +
                    hourly+ college, 
                  design = THD_comp)

employal_reg_polr<-svy_vglm(emp_loyal ~
                    HDTreatment*EHF_aware_list+ rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_comp,
                  family=propodds())


empreco_reg_b<-svyglm(emp_reco_num ~
                    HDTreatment, 
                  design = THD_comp)

empreco_reg_b_int<-svyglm(emp_reco_num ~
                    HDTreatment*EHF_aware_list +
                      tenure_fac, 
                  design = THD_comp)

empreco_reg_c<-svyglm(emp_reco_num ~
                    HDTreatment*EHF_aware_list +rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_comp)

empreco_reg_polr<-svy_vglm(emp_reco ~
                    HDTreatment*EHF_aware_list +rk_age + male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_comp,
                  family=propodds())

#wrkloyal_reg_polr etc. won't work with stargazer til I fix it.


# model_print_w_loyal <- stargazer(wrkloyal_reg_b, wrkloyal_reg_b_int, wrkloyal_reg_c, 
#                            employal_reg_b, employal_reg_b_int, employal_reg_c,
#                            empreco_reg_b, empreco_reg_b_int, empreco_reg_c,
#           title = "Weighted OLS regression of treatments on attachment to coworkers \\& employer",
#           covariate.labels = c("TRT: text", "TRT: vid", "age", "male", "main job",
#                                "tenure: 6-12mos", "  1-2 yrs.", 
#                                "  2-3 yrs.", "  3+ yrs.",
#                                "nonwhite", "full-time", "hourly",
#                                "BA/BS+"),
#           dep.var.caption = "",
#           dep.var.labels = c("coworker\n loyalty",
#                              "employer\n loyalty",
#                              "recommend\n employer"),
#           model.numbers = F,
#           label="tab:tab-loyalty-models",
#           no.space = T,
#           digits = 2,
#          # column.labels = c("OLS", "OLS", "Ordered logit"),
#           header = FALSE
#          #notes = c("All results use raking weights to the Facebook-reported gender/age distribution for the recuitment target population.")
#           )
