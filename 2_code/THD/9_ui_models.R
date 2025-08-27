#OLS


ui_lm_uw <- lm_robust(gov_ui_num ~ HDTreatment, 
               data = THD_comp_uw)
ui_lm_int_uw <- lm_robust(gov_ui_num ~ HDTreatment*EHF_aware_list, 
                   data = THD_comp_uw)
ui_ols_int_uw <- lm(gov_ui_num ~ HDTreatment*EHF_aware_list, 
                          data = THD_comp_uw)
ui_lm_c_pregreg_uw <- lm_robust(gov_ui_num ~ 
                   HDTreatment*EHF_aware_list + rk_age + male +
                   main_job + tenure_num + nonwhite + fulltime +
                   hourly+ college, data = THD_comp_uw)

ui_lmer_uw <- lme4::lmer(gov_ui_num ~ HDTreatment + 
                           HDTreatment:st_directed +
                           + (1 + HDTreatment| state_lv), 
               data = THD_comp_uw)  #no evidence of state-level random slope on treatment (singular variance)


ui_lm_c_expand_uw <- update(ui_lm_c_pregreg_uw, .~. + other_welfare + 
                             income_num + 
                             religious +
                             ideology_conservative +
                              st_directed + 
                              as.numeric(home_ownership):hpi_5year, 
                            clusters = state_lv)

 
ui_lm_c_uiwks_uw <- update(ui_lm_c_expand_uw, . ~. -st_directed + I(max_ui_weeks<26))
ui_lm_c_uiwba_uw <- update(ui_lm_c_pregreg_uw, . ~. + avg_wba)
ui_lm_c_uiwba_uw <- update(ui_lm_c_pregreg_uw, . ~. + replacement_2021_ui_1)
ui_lm_c_uitanf_uw <- update(ui_lm_c_expand_uw, . ~. - st_directed + WG_TANF)
ui_lm_c_uitanfb_uw <- update(ui_lm_c_expand_uw, . ~. - st_directed + WG_TANF_Benefit)
ui_lm_c_uicf_uw <- update(ui_lm_c_expand_uw, . ~. - st_directed + cashfood)


ui_ol_int_uw <- MASS::polr(gov_ui~ HDTreatment*EHF_aware_list,
                           data = THD_comp_uw, Hess = TRUE)

ui.models.uw <- list(ui_lm_uw, ui_lm_int_uw, ui_lm_c_pregreg_uw, ui_lm_c_expand_uw)
names(ui.models.uw) <- c("Base", "Pre-exposure", "Pre-registered", "Expanded")

coef_maps <- c("HDTreatmenttxt" = "Text treatment",
               "HDTreatmentvid" = "Video treatment",
               "EHF_aware_listTRUE" = "Pre-exposed",
               "HDTreatmenttxt:EHF_aware_listTRUE" = "Text x pre-exposed",
               "HDTreatmentvid:EHF_aware_listTRUE" = "Video x pre-exposed",
               "other_welfareTRUE" = "Past welfare",
               #"cohabitTRUE" = "co-habiting",
               "income_num" = "HH income",
               "religiousTRUE" = "Religiosity",
               "st_directed" = "State safety net generosity",
               "ideology_conservative" = "Conservative",
               "as.numeric(home_ownership):hpi_5year" = "house appreciation")

rows<-tribble(
  ~"term", ~"Base", ~"Preexposure",  ~"Pre-registered", ~"Expanded",
  "Std. Errors", "robust", "robust", "robust", "state-clustered")
attr(rows, 'position') <- c(24)

note1 <- "Standard errors in parentheses.\n Pre-registered covariates include age, gender race, job tenure, hourly status, full time status, college degree, and main job."

gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1))

ui_tab<-modelsummary(ui.models.uw,
             coef_map = coef_maps,
            title = "Government support for the unemployed (Home Depot) \\label{tab:tab-ui}",
            gof_map = gm,
           # vcov = "robust",
            add_rows = rows, 
            notes = list(note1),
            threeparttable=TRUE,
            stars = c('*' = .1, '**' = .05, "***" = 0.01),
            escape = FALSE
)

ui_eff<-Effect(c("HDTreatment", "EHF_aware_list"), 
    ui_ols_int_uw,
    vcov = sandwich::vcovHC(ui_ols_int_uw),     
    se = TRUE)

ui_eff_plot <- plot(ui_eff,
     main = "Treament effect on UI support\n by EHF pre-exposure status",
     xlab = "Experimental condition",
     axes = list(
       y = list(type = "response",
                lab="predicted value")),
     ylim = c(.2, .9),
     lattice=list(strip=list(factor.names=FALSE)),
     colors = grey(0.5)
)

ui_eff_plot$condlevels$EHF_aware_list<-c("unaware", "aware")

png(here::here("4_output","plots", "ui_eff.png"))
print(ui_eff_plot)
dev.off()


# Placebos

pension_lm_int_uw <- lm(gov_pension_num ~ HDTreatment*EHF_aware_list, 
                   data = THD_comp_uw)
cc_lm_int_uw <- lm(gov_childcare_num ~ HDTreatment*EHF_aware_list, 
                   data = THD_comp_uw)
placebo.models.uw <- list(pension_lm_int_uw, cc_lm_int_uw)
names(placebo.models.uw) <- c("Pension", "Childcare")

pension_lm_c_pregreg_uw <- lm_robust(gov_pension_num ~ 
                                  HDTreatment*EHF_aware_list + rk_age + male +
                                  main_job + tenure_num + nonwhite + fulltime +
                                  hourly+ college, data = THD_comp_uw)


pension_lm_c_expand_uw <- update(pension_lm_int_uw, .~. + other_welfare + 
                              cohabit + 
                              income_num + 
                              religious +
                              ideology_conservative +
                              as.numeric(home_ownership):hpi_5year)
pension_lm_c_pensionwks_uw <- update(pension_lm_c_pregreg_uw, . ~. + max_ui_weeks)
pension_lm_c_pensionwba_uw <- update(pension_lm_c_pregreg_uw, . ~. + avg_wba)
pension_lm_c_pensionwba_uw <- update(pension_lm_c_pregreg_uw, . ~. + replacement_2021_ui_1)
pension_lm_c_pensiontanf_uw <- update(pension_lm_c_pregreg_uw, . ~. + WG_TANF)
pension_lm_c_pensiontanfb_uw <- update(pension_lm_c_pregreg_uw, . ~. + WG_TANF_Benefit)
pension_lm_c_pensionstd_uw <- update(pension_lm_c_pregreg_uw, . ~. + st_directed)
pension_lm_c_pensioncf_uw <- update(pension_lm_c_pregreg_uw, . ~. + cashfood)




placebo_tab<-modelsummary(placebo.models.uw,
             coef_map = coef_maps,
            title = "Support for other social policies, OLS regression  \\label{tab:tab-placebo}",
            gof_map = gm,
            vcov = "robust",
            notes = list("Robust standard errors in parentheses."),
            threeparttable = TRUE,
            stars = c('*' = .05, '**' = .01)
)


#weighted models
ui_reg_b<-svyglm(gov_ui_num ~
                    HDTreatment, 
                  design = THD_comp)

ui_reg_b_int<-svyglm(gov_ui_num ~
                    HDTreatment*EHF_aware_list, 
                  design = THD_comp)

ui_reg_c<-svyglm(gov_ui_num ~
                    HDTreatment*EHF_aware_list +age + male +
                    main_job +
                    tenure_fac +
                    nonwhite +
                    fulltime +
                    hourly+
                    college , 
                  design = THD_comp)
#ordered logit
ui_reg_polr<-svy_vglm(gov_ui ~
                    HDTreatment*EHF_aware_list 
                    +age + male +
                    main_job +
                    tenure_fac +
                    nonwhite +
                    fulltime +
                    hourly+
                    college, 
                  design = THD_comp,
                  family=propodds())

# stargazer(ui_reg_b, ui_reg_c,
#           title = "Weighted OLS regression on support for UI",
#           covariate.labels = c("TRT: text", "TRT: vid", "age", "male", "main job",
#                                "tenure: 6-12mos", "  1-2 yrs.", 
#                                "  2-3 yrs.", "  3+ yrs.",
#                                "nonwhite", "full-time", "hourly",
#                                "BA/BS+"),
#           dep.var.caption = "",
#           dep.var.labels = "support for UI (1-4)",
#           model.numbers = F,
#           no.space = T,
#           digits = 2,
#          # column.labels = c("OLS", "OLS", "Ordered logit"),
#           header = FALSE,
#          label = "tab:tab-ui"
#          #notes = c("All results use raking weights to the Facebook-reported gender/age distribution for the recuitment target population.")
#           )


pension_reg_b<-svyglm(as.numeric(gov_pension_num) ~
                    HDTreatment, 
                  design = THD_comp)

pension_reg_b_int<-svyglm(as.numeric(gov_pension_num) ~
                    HDTreatment*EHF_aware_list, 
                  design = THD_comp)


pension_reg_c<-svyglm(as.numeric(gov_pension_num) ~
                    HDTreatment +age + male +
                    main_job +
                    tenure_fac +
                    nonwhite +
                    fulltime +
                    hourly+
                    college , 
                  design = THD_comp)

cc_reg_b<-svyglm(as.numeric(gov_childcare_num) ~
                    HDTreatment, 
                  design = THD_comp)

cc_reg_b<-svyglm(as.numeric(gov_childcare_num) ~
                    HDTreatment*EHF_aware_list, 
                  design = THD_comp)

cc_reg_c<-svyglm(as.numeric(gov_childcare_num) ~
                    HDTreatment +age + male +
                    main_job +
                    tenure_fac +
                    nonwhite +
                    fulltime +
                    hourly+
                    college , 
                  design = THD_comp)
