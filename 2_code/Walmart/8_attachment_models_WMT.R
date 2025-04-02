#source("1_libraries_and_settings.R")
#source("2_data_format.R")

# attachment index
##full sample
attach_lm_wmt <- lm(attachment_index ~ treatment_bin,
   data = wmt)

attach_lm_int_wmt <- lm(attachment_index ~ treatment_bin*ehf_aware_pretr,
                 data = wmt)

attach_lm_c_wmt <- lm(attachment_index ~ 
                        treatment_bin*ehf_aware_pretr + 
                        age_clean +
                        male +
                        main_job +
                        tenure_num +
                        nonwhite +
                        fulltime +
                        hourly+
                        college, 
                      data = wmt)

attach_lm_cfull_wmt <- lm(attachment_index ~ 
                        treatment_full*ehf_aware_pretr + 
                        age_clean +
                        male +
                        main_job +
                        tenure_num +
                        nonwhite +
                        fulltime +
                        hourly+
                        college, 
                      data = wmt)

##HQ sample  #question about whether to include placebo

attach_lm_wmt_hq<- update(attach_lm_wmt, .~., data = wmt.hq)
attach_lm_int_wmt_hq <- update(attach_lm_int_wmt, .~., data = wmt.hq)
attach_lm_c_wmt_hq <- update(attach_lm_c_wmt, .~., data = wmt.hq)
attach_lm_cfull_wmt_hq <- update(attach_lm_cfull_wmt, .~., data = wmt.hq)

#report HQ sample results based on PAP
# attachment.models.wmt.hq <- list(attach_lm_wmt_hq, attach_lm_int_wmt_hq, 
#                           attach_lm_c_wmt_hq,attach_lm_cfull_wmt_hq)
# names(attachment.models.wmt.hq) <- c("Base", "Pre-exposure", "Covariates", "Detailed")

attachment.models.wmt.hq <- list(attach_lm_wmt_hq, attach_lm_int_wmt_hq, 
                                 attach_lm_c_wmt_hq)
names(attachment.models.wmt.hq) <- c("Base", "Pre-exposure", "Covariates")

# coef_maps <- c(
#   "treatment_binTRUE" = "Treated",
#   "treatment_binTRUE:ehf_aware_pretrTRUE" = "Treated x pre-exposed",
#   "treatment_fullvid0" = "Placebo",
#   "treatment_fullvidChar" = "Charity treatment",
#   "treatment_fullvidSolid" = "Solidarity treatment",
#   "ehf_aware_pretrTRUE" = "Pre-exposed",
#   "treatment_fullvid0:ehf_aware_pretrTRUE" = "Placebo x pre-exposed",
#   "treatment_fullvidChar:ehf_aware_pretrTRUE" = "Charity x pre-exposed",
#   "treatment_fullvidSolid:ehf_aware_pretrTRUE" = "Solidarity x pre-exposed")
# 
#   
# rows<-tribble(
#   ~"term", ~"Base", ~"Preexposure",  ~"Full", ~"Detailed",
#   "Covariates?", "No", "No", "Yes", "Yes")
# attr(rows, 'position') <- c(19)

coef_maps <- c(
  "treatment_binTRUE" = "Treated",
  "ehf_aware_pretrTRUE" = "Pre-exposed",
  "treatment_binTRUE:ehf_aware_pretrTRUE" = "Treated x pre-exposed"
  )

rows<-tribble(
  ~"term", ~"Base", ~"Preexposure",  ~"Covariates",
  "Covariates?", "No", "No", "Yes")
attr(rows, 'position') <- c(7)


note1 <- "Robust standard errors in parentheses. Covariates include age, gender, race, job tenure, hourly status, full time status, college degree, and main job.  High-quality respondents only."

gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2),
  list("raw" = "F", "clean" = "$F$", "fmt" = 2))


model_print_attachment_hq<- modelsummary::modelsummary( attachment.models.wmt.hq,
                                     #shape = "rbind",
                                     coef_map = coef_maps,
                                     gof_map = gm,
                                     vcov = "robust",
                                     add_rows = rows,
                                     title = "Job attachment, OLS regression \\label{tab:tab-attachment-models-wmt}",
                                     output = "kableExtra",
                                     notes = list(note1),
                                     stars = c('*' = .05, '**' = .01),
                                     threeparttable=TRUE,
                                     escape = FALSE
)

### subcomponents of attachment index

# new job
##full sample
nj_lm_wmt <- lm(new_job_num ~ treatment_bin,
                    data = wmt)

nj_lm_int_wmt <- lm(new_job_num ~ treatment_bin*ehf_aware_pretr,
                        data = wmt)

nj_lm_c_wmt <- lm(new_job_num ~ 
                        treatment_bin*ehf_aware_pretr + 
                        age_clean +
                        male +
                        main_job +
                        tenure_num +
                        nonwhite +
                        fulltime +
                        hourly+
                        college, 
                      data = wmt)

nj_lm_cfull_wmt <- lm(new_job_num ~ 
                            treatment_full*ehf_aware_pretr + 
                            age_clean +
                            male +
                            main_job +
                            tenure_num +
                            nonwhite +
                            fulltime +
                            hourly+
                            college, 
                          data = wmt)

##HQ sample

nj_lm_wmt_hq<- update(nj_lm_wmt, .~., data = wmt.hq)
nj_lm_int_wmt_hq <- update(nj_lm_int_wmt, .~., data = wmt.hq)
nj_lm_c_wmt_hq <- update(nj_lm_c_wmt, .~., data = wmt.hq)
nj_lm_cfull_wmt.hq <- update(nj_lm_cfull_wmt, .~., data = wmt.hq)


# loyalty to coworkers
wrkloyal_lm_wmt <- lm(wrk_loyal_num ~ treatment_bin, 
               data = wmt)
wrkloyal_lm_int_wmt <- lm(wrk_loyal_num ~ treatment_bin*ehf_aware_pretr, 
                   data = wmt)
wrkloyal_lm_c_wmt <- lm(wrk_loyal_num ~ 
                         treatment_bin*ehf_aware_pretr + 
                         age_clean +
                         male +
                     main_job +
                     tenure_num +
                     nonwhite +
                     fulltime +
                     hourly+
                     college, 
                   data = wmt)
wrkloyal_ol_int_uw_wmt <- MASS::polr(as.ordered(wrk_loyal_num)~ treatment_bin*ehf_aware_pretr,
                           data = wmt)

wl.models.wmt.bin <- list(wrkloyal_lm_wmt, wrkloyal_lm_int_wmt, wrkloyal_lm_c_wmt)
names(wl.models.wmt.bin) <- c("Base", "Pre-exposure", "Covariates")

employal_lm_wmt <- lm(emp_loyal_num ~ treatment_bin, 
                      data = wmt)
employal_lm_int_wmt <- lm(emp_loyal_num ~ treatment_bin*ehf_aware_pretr, 
                          data = wmt)
employal_lm_c_wmt <- lm(emp_loyal_num ~ 
                          treatment_bin*ehf_aware_pretr + 
                          age_clean +
                          male +
                          main_job +
                          tenure_num +
                          nonwhite +
                          fulltime +
                          hourly+
                          college, 
                        data = wmt)
employal_ol_int_uw_wmt <- MASS::polr(as.ordered(emp_loyal_num)~ treatment_bin*ehf_aware_pretr,
                                 data = wmt.hq)

# el.models.uw <- list(employal_lm_uw, employal_lm_int_uw, employal_lm_c_uw)
# names(el.models.uw) <- c("Base", "Pre-exposure", "Covariates")
# 
# empreco_lm_uw <- lm(emp_reco_num ~ treatment_bin, 
#                      data = wmt.hq)
# empreco_lm_int_uw <- lm(emp_reco_num ~ treatment_bin*ehf_aware_pretr, 
#                          data = wmt.hq)
# empreco_lm_c_uw <- lm(emp_reco_num ~ 
#                         treatment_bin*ehf_aware_pretr+
#                         age_clean +
#                         male +
#                         main_job +
#                         tenure_num +
#                         nonwhite +
#                         fulltime +
#                         hourly+
#                         college, 
#                       data = wmt.hq)
# empreco_ol_int_uw <- MASS::polr(as.ordered(emp_reco)~ treatment_bin*ehf_aware_pretr,
#                                  data = wmt.hq)
# 
# er.models.uw <- list(empreco_lm_uw, empreco_lm_int_uw, empreco_lm_c_uw)
# names(el.models.uw) <- c("Base", "Pre-exposure", "Covariates")


# coef_maps <- c("HDTreatmenttxt" = "Text treatment",
#                "HDTreatmentvid" = "Video treatment",
#                "EHF_aware_listTRUE" = "Pre-exposed",
#                "HDTreatmenttxt:EHF_aware_listTRUE" = "Text x pre-exposed",
#                "HDTreatmentvid:EHF_aware_listTRUE" = "Video x pre-exposed")
# 
# rows<-tribble(
#   ~"term", ~"Base", ~"Preexposure",  ~"Full",
#   "Covariates?", "No", "No", "Yes")
# attr(rows, 'position') <- c(41)
# 
# note1 <- "Robust standard errors in parentheses. Covariates include age, gender race, job tenure, hourly status, full time status, college degree, and main job."
# 
# gm <- list(
#   list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
#   list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2),
#   list("raw" = "F", "clean" = "$F$", "fmt" = 2))
# 
# panels <- list(
#   "Outcome: co-worker loyalty" = wl.models.uw,
#   "Outcome: employer loyalty" = el.models.uw,
#   "Outcome: recommend employer" = el.models.uw
# )
# 
# model_print_uw_loyal<- modelsummary( panels,
#                                shape = "rbind",
#                                coef_map = coef_maps,
#                                gof_map = gm,
#                                vcov = "robust",
#                                #add_rows = rows,
#                                title = "Coworker and employer attachment, OLS regression \\label{tab:tab-loyalty-models}",
#                                output = "kableExtra",
#                                notes = list(note1),
#                                stars = c('*' = .05, '**' = .01),
#                                threeparttable=TRUE
# )
# 
# 
# wrkloyal_eff<-Effect(c("HDTreatment", "EHF_aware_list"), 
#     wrkloyal_lm_int_uw,
#     vcov = sandwich::vcovHC(wrkloyal_lm_int_uw),     
#     se = TRUE)
# 
# wrkloyal_eff_plot <- plot(wrkloyal_eff,
#      main = "Treament effect on coworker loyalty\n by pre-exposure status",
#      xlab = "Experimental condition",
#      axes = list(
#        y = list(type = "response",
#                 lab="predicted value")),
#      ylim = c(.2, .9),
#      lattice=list(strip=list(factor.names=FALSE)),
#      colors = grey(0.5)
# )
# 
# wrkloyal_eff_plot$condlevels$EHF_aware_list<-c("unaware", "aware")
# 
# employal_eff<-Effect(c("HDTreatment", "EHF_aware_list"), 
#     employal_lm_int_uw,
#     vcov = sandwich::vcovHC(employal_lm_int_uw),     
#     se = TRUE)
# 
# employal_eff_plot <- plot(employal_eff,
#      main = "Treament effect on employer loyalty\n by pre-exposure status",
#      xlab = "Experimental condition",
#      axes = list(
#        y = list(type = "response",
#                 lab="predicted value")),
#      ylim = c(.2, .9),
#      lattice=list(strip=list(factor.names=FALSE)),
#      colors = grey(0.5)
# )
# 
# employal_eff_plot$condlevels$EHF_aware_list<-c("unaware", "aware")
# 
# 
# png(here::here("output","plots", "wrkloyal_eff.png"))
# print(wrkloyal_eff_plot)
# dev.off()
# 
# png(here::here("output","plots", "employal_eff.png"))
# print(employal_eff_plot)
# dev.off()
# 

#weighted models
# wrkloyal_reg_b<-svyglm(wrk_loyal_num ~
#                     HDTreatment, 
#                   design = THD_comp)
# 
# wrkloyal_reg_b_int<-svyglm(wrk_loyal_num ~
#                     HDTreatment*EHF_aware_list +
#                       tenure_num, 
#                   design = THD_comp)
# 
# wrkloyal_reg_c<-svyglm(wrk_loyal_num ~
#                     HDTreatment*EHF_aware_list + rk_age + male +
#                     main_job +
#                     tenure_num +
#                     nonwhite +
#                     fulltime +
#                     hourly+
#                     college, 
#                   design = THD_comp)  # fitted values outside [1,4] 
# 
# wrkloyal_reg_polr<-svy_vglm(wrk_loyal ~
#                     HDTreatment*EHF_aware_list+ rk_age + male +
#                     main_job +
#                     tenure_num +
#                     nonwhite +
#                     fulltime +
#                     hourly+
#                     college, 
#                   design = THD_comp,
#                   family=propodds())
# 
# employal_reg_b<-svyglm(emp_loyal_num ~
#                     HDTreatment, 
#                   design = THD_comp)
# 
# employal_reg_b_int<-svyglm(emp_loyal_num ~
#                     HDTreatment*EHF_aware_list, 
#                   design = THD_comp)
# 
# 
# employal_reg_c<-svyglm(emp_loyal_num ~
#                     HDTreatment*EHF_aware_list + rk_age +
#                       male + main_job + tenure_num +
#                     nonwhite + fulltime +
#                     hourly+ college, 
#                   design = THD_comp)
# 
# employal_reg_polr<-svy_vglm(emp_loyal ~
#                     HDTreatment*EHF_aware_list+ rk_age + male +
#                     main_job +
#                     tenure_num +
#                     nonwhite +
#                     fulltime +
#                     hourly+
#                     college, 
#                   design = THD_comp,
#                   family=propodds())
# 
# 
# empreco_reg_b<-svyglm(emp_reco_num ~
#                     HDTreatment, 
#                   design = THD_comp)
# 
# empreco_reg_b_int<-svyglm(emp_reco_num ~
#                     HDTreatment*EHF_aware_list +
#                       tenure_fac, 
#                   design = THD_comp)
# 
# empreco_reg_c<-svyglm(emp_reco_num ~
#                     HDTreatment*EHF_aware_list +rk_age + male +
#                     main_job +
#                     tenure_num +
#                     nonwhite +
#                     fulltime +
#                     hourly+
#                     college, 
#                   design = THD_comp)
# 
# empreco_reg_polr<-svy_vglm(emp_reco ~
#                     HDTreatment*EHF_aware_list +rk_age + male +
#                     main_job +
#                     tenure_num +
#                     nonwhite +
#                     fulltime +
#                     hourly+
#                     college, 
#                   design = THD_comp,
#                   family=propodds())

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
