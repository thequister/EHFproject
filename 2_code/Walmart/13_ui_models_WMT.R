#OLS


ui_lm_wmt <- lm(govt_responsib_unemp_num ~ treatment_bin, 
               data = wmt.hq)
ui_lm_int_wmt <- lm(govt_responsib_unemp_num ~ treatment_bin*ehf_aware_pretr, 
                    data = wmt.hq)
ui_lm_c_wmt <- lm(govt_responsib_unemp_num ~ 
                   treatment_bin+ 
                   age_clean + 
                   male +
                   main_job +
                   tenure_num +
                   nonwhite +
                   fulltime +
                   hourly+
                   college,
                 data = wmt.hq)
ui_lm_c_int_wmt <- lm(govt_responsib_unemp_num ~ 
                   treatment_bin*ehf_aware_pretr+ 
                   age_clean + 
                   male +
                   main_job +
                   tenure_num +
                   nonwhite +
                   fulltime +
                   hourly+
                   college,
                 data = wmt.hq)


ui_ol_int_wmt <- MASS::polr(ordered(govt_responsib_unemp_num) ~treatment_bin*ehf_aware_pretr,
                           data = wmt.hq, Hess = TRUE)


hard_lm_wmt <- lm(govt_responsib_hardship_num ~ treatment_bin, 
                data = wmt.hq)
hard_lm_int_wmt <- lm(govt_responsib_hardship_num ~ treatment_bin*ehf_aware_pretr, 
                    data = wmt.hq)
hard_lm_c_wmt <- lm(govt_responsib_hardship_num ~ 
                    treatment_bin+ 
                    age_clean + 
                    male +
                    main_job +
                    tenure_num +
                    nonwhite +
                    fulltime +
                    hourly+
                    college,
                  data = wmt.hq)

hard_lm_c_int_wmt <- lm(govt_responsib_hardship_num ~ 
                        treatment_bin*ehf_aware_pretr+ 
                        age_clean + 
                        male +
                        main_job +
                        tenure_num +
                        nonwhite +
                        fulltime +
                        hourly+
                        college,
                      data = wmt.hq)


hard_ol_int_wmt <- MASS::polr(ordered(govt_responsib_hardship_num) ~treatment_bin*ehf_aware_pretr,
                            data = wmt.hq, Hess = TRUE)




ui.hard.models.wmt <- list(
  ui_lm_wmt,
  ui_lm_int_wmt,
  ui_lm_c_int_wmt,
  hard_lm_wmt,
  hard_lm_int_wmt,
  hard_lm_c_int_wmt)
names(ui.hard.models.wmt) <- c("UI", "UI", "UI", 
                         "Emergency", "Emergency", "Emergency")

coef_maps <- c(
  "treatment_binTRUE" = "Treated",
  "ehf_aware_pretrTRUE" = "Pre-exposed",
  "treatment_binTRUE:ehf_aware_pretrTRUE" = "Treated x pre-exposed")

rows<-tribble(
  ~"term", ~"Base", ~"Preexposure",  ~"Covariates",~"Base", ~"Preexposure",  ~"Covariates",
  "Covariates?", "No", "No", "Yes","No", "No", "Yes")
attr(rows, 'position') <- c(7)

note1 <- "Robust standard errors in parentheses. Covariates include age, gender race, job tenure, hourly status, full time status, college degree, and main job."


gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2),
  list("raw" = "F", "clean" = "$F$", "fmt" = 2))

ui_hard_tab_wmt<-modelsummary(ui.hard.models.wmt,
             coef_map = coef_maps,
            title = "Support for unemployment insurance and emergency government support  \\label{tab:tab-ui-hard-wmt}",
            gof_map = gm,
            vcov = "robust",
            add_rows = rows, 
            notes = list(note1),
            threeparttable=TRUE,
            stars = c('*' = .05, '**' = .01),
            escape = FALSE
)

# ui_eff<-Effect(c("HDTreatment", "EHF_aware_list"), 
#     ui_lm_int_uw,
#     vcov = sandwich::vcovHC(ui_lm_int_uw),     
#     se = TRUE)
# 
# ui_eff_plot <- plot(ui_eff,
#      main = "Treament effect on UI support\n by pre-exposure status",
#      xlab = "Experimental condition",
#      axes = list(
#        y = list(type = "response",
#                 lab="predicted value")),
#      ylim = c(.2, .9),
#      lattice=list(strip=list(factor.names=FALSE)),
#      colors = grey(0.5)
# )
# 
# ui_eff_plot$condlevels$EHF_aware_list<-c("unaware", "aware")
# 
# png(here::here("output","plots", "ui_eff.png"))
# print(ui_eff_plot)
# dev.off()
# 
# 
# # Placebos
# 
# pension_lm_int_uw <- lm(gov_pension_num ~ HDTreatment*EHF_aware_list, 
#                    data = THD_comp_uw)
# cc_lm_int_uw <- lm(gov_childcare_num ~ HDTreatment*EHF_aware_list, 
#                    data = THD_comp_uw)
# placebo.models.uw <- list(pension_lm_int_uw, cc_lm_int_uw)
# names(placebo.models.uw) <- c("Pension", "Childcare")
# 
# 
# placebo_tab<-modelsummary(placebo.models.uw,
#              coef_map = coef_maps,
#             title = "Support for other social policies, OLS regression  \\label{tab:tab-placebo}",
#             gof_map = gm,
#             vcov = "robust",
#             notes = list("Robust standard errors in parentheses."),
#             threeparttable = TRUE,
#             stars = c('*' = .05, '**' = .01)
# )
# 
# 
# #weighted models
# ui_reg_b<-svyglm(gov_ui_num ~
#                     HDTreatment, 
#                   design = THD_comp)
# 
# ui_reg_b_int<-svyglm(gov_ui_num ~
#                     HDTreatment*EHF_aware_list, 
#                   design = THD_comp)
# 
# ui_reg_c<-svyglm(gov_ui_num ~
#                     HDTreatment*EHF_aware_list +age + male +
#                     main_job +
#                     tenure_fac +
#                     nonwhite +
#                     fulltime +
#                     hourly+
#                     college , 
#                   design = THD_comp)
# #ordered logit
# ui_reg_polr<-svy_vglm(gov_ui ~
#                     HDTreatment*EHF_aware_list 
#                     +age + male +
#                     main_job +
#                     tenure_fac +
#                     nonwhite +
#                     fulltime +
#                     hourly+
#                     college, 
#                   design = THD_comp,
#                   family=propodds())
# 
# # stargazer(ui_reg_b, ui_reg_c,
# #           title = "Weighted OLS regression on support for UI",
# #           covariate.labels = c("TRT: text", "TRT: vid", "age", "male", "main job",
# #                                "tenure: 6-12mos", "  1-2 yrs.", 
# #                                "  2-3 yrs.", "  3+ yrs.",
# #                                "nonwhite", "full-time", "hourly",
# #                                "BA/BS+"),
# #           dep.var.caption = "",
# #           dep.var.labels = "support for UI (1-4)",
# #           model.numbers = F,
# #           no.space = T,
# #           digits = 2,
# #          # column.labels = c("OLS", "OLS", "Ordered logit"),
# #           header = FALSE,
# #          label = "tab:tab-ui"
# #          #notes = c("All results use raking weights to the Facebook-reported gender/age distribution for the recuitment target population.")
# #           )
# 
# 
# pension_reg_b<-svyglm(as.numeric(gov_pension_num) ~
#                     HDTreatment, 
#                   design = THD_comp)
# 
# pension_reg_b_int<-svyglm(as.numeric(gov_pension_num) ~
#                     HDTreatment*EHF_aware_list, 
#                   design = THD_comp)
# 
# 
# pension_reg_c<-svyglm(as.numeric(gov_pension_num) ~
#                     HDTreatment +age + male +
#                     main_job +
#                     tenure_fac +
#                     nonwhite +
#                     fulltime +
#                     hourly+
#                     college , 
#                   design = THD_comp)
# 
# cc_reg_b<-svyglm(as.numeric(gov_childcare_num) ~
#                     HDTreatment, 
#                   design = THD_comp)
# 
# cc_reg_b<-svyglm(as.numeric(gov_childcare_num) ~
#                     HDTreatment*EHF_aware_list, 
#                   design = THD_comp)
# 
# cc_reg_c<-svyglm(as.numeric(gov_childcare_num) ~
#                     HDTreatment +age + male +
#                     main_job +
#                     tenure_fac +
#                     nonwhite +
#                     fulltime +
#                     hourly+
#                     college , 
#                   design = THD_comp)
