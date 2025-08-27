#source("1_libraries_and_settings.R")
#source("2_data_format.R")

d_wmt_ols <-lm_robust(ehf_donation_post  ~ treatment_bin, data=wmt.hq)
d_wmt_lme <- lme4::glmer(ehf_donation_post ~ treatment_bin + 
                          treatment_bin:st_directed +
                          (1 + treatment_bin| residence),
                         family = binomial,
                        data=wmt.hq)
d_wmt_st_int <-lm_robust(ehf_donation_post  ~ treatment_bin*st_directed, 
                         cluster = residence,
                         data=wmt.hq)
d_wmt_int <-lm_robust(ehf_donation_post  ~ treatment_bin*ehf_aware_pretr, data=wmt.hq)
d_wmt_pregreg <- lm_robust(ehf_donation_post ~ treatment_bin*ehf_aware_pretr +
                                  age_clean +
                                  male +
                                  main_job +
                                  tenure_num +
                                  nonwhite +
                                  fulltime +
                                  hourly+
                                  college,
                                data = wmt.hq)

d_wmt_expand <- lm_robust(ehf_donation_post ~ 
                            treatment_bin*ehf_aware_pretr +
                         age_clean +
                         male +
                         main_job +
                         tenure_num +
                         nonwhite +
                         fulltime +
                         hourly+
                         college +
                         welfare + 
                         income_num + 
                         religious +
                         ideology_conservative +
                         st_directed + 
                         as.numeric(home_ownership):hpi_5year, 
                       clusters = residence,
                       data = wmt.hq)


d_mods<-list(d_wmt_ols, d_wmt_st_int, d_wmt_int, d_wmt_pregreg, d_wmt_expand)
names(d_mods)<-c("Base", "State Generosity", "Pre-exposure", "Pre-registered", "Expanded")

coef_maps <- c("treatment_binTRUE" = "Treated",
               "ehf_aware_pretrTRUE" = "Pre-exposed",
               "treatment_binTRUE:ehf_aware_pretrTRUE" = "Treated x pre-exposed",
               "st_directed" = "State safety net generosity",
               "treatment_binTRUE:st_directed" = "Treated x generosity",
               "welfareTRUE" = "Past welfare",
               "income_num" = "HH income",
               "religiousTRUE" = "Religiosity",
               "ideology_conservative" = "Conservative",
               "as.numeric(home_ownership):hpi_5year" = "house appreciation")

rows<-tribble(
  ~"term", ~"Base", ~"State", ~"Pre-exposed",  ~"Pre-registered", ~"Expanded",
  "Std Errors", "robust", "state-clustered", "robust", "robust", "state-clustered")
attr(rows, 'position') <- c(21)
gm <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = 0),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1))
note1 <- "Standard errors in parentheses.\n Pre-registerd covariates include age, gender race, job tenure, hourly status, full time status, college degree, and main job."


donation_tab_wmt<-modelsummary(d_mods,
                               coef_map = coef_maps,
                               title = "ACNT donation willingness (Walmart sample) \\label{tab:tab-donate-wmt}",
                                       gof_map = gm,
                                       # vcov = "robust",
                                       add_rows = rows, 
                                       notes = list(note1),
                                       threeparttable=TRUE,
                                       stars = c('*' = .1, '**' = .05, "***" = 0.01),
                                       escape = FALSE
)


d_wmt <-glm(ehf_donation_post ~ treatment_bin, family=binomial, data=wmt.hq)
d_int_wmt <-glm(ehf_donation_post ~ treatment_bin*ehf_aware_pretr, family=binomial, data=wmt.hq)

d_c_wmt <-glm(wmt.hq$donate == "YES I would like to learn how to donate" ~ treatment_bin +
                age_clean + 
                male +
                main_job +
                tenure_num +
                nonwhite +
                fulltime +
                #    hourly+ perfect separation
                college, family=binomial, data=wmt.hq)
d_firth <-logistf::logistf(donate == "YES I would like to learn how to donate" ~ treatment_bin,
                           data = wmt.hq)
d_firth_cov <-logistf::logistf(donate == "YES I would like to learn how to donate" ~ treatment_full,
                           data = wmt.hq)






d_mods_app<-list(d_wmt_ols, d_wmt, d_firth)
names(d_mods_app) <-c("LPM", "logit", "Firth")
donation_tab_app <- modelsummary(d_mods_app,
                               #shape = term + response ~ statistic,
                               coef_map = c("treatment_binTRUE" = "Treated"),
                               title = "Regression of ACNT donation on treatment  \\label{tab:tab-donate-models-wmt-app}",
                               #vcov = "robust",
                               gof_map = gm,
                               #add_rows = rows,
                               notes = "Robust standard errors for OLS model",
                               #threeparttable=TRUE, 
                               stars = c('+' = 0.1, '*' = .05, '**' = .01),
                               escape = FALSE
)



donation_summary <- wmt.hq  |>
  mutate(
    donate_bin = donate=="YES I would like to learn how to donate"
  ) |> 
  group_by(treatment_bin)  |> 
  summarise(
    prop = mean(donate_bin),
    n = n(),
    successes = sum(donate_bin),
    ci_lower = binom.test(sum(donate_bin), n)$conf.int[1],
    ci_upper = binom.test(sum(donate_bin), n)$conf.int[2]
  )

pt.wmt<-prop.test(x= donation_summary$successes, n = donation_summary$n)
ft.wmt<-table(wmt.hq$donate=="YES I would like to learn how to donate" , wmt.hq$treatment_bin)
ft.wmt<-fisher.test(ft.wmt)

d_plot <-ggplot(donation_summary, aes(x = as.factor(treatment_bin), y = prop, color = as.factor(treatment_bin))) +
  geom_point(position = position_dodge(width = 0.25), size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(0.25)) +
  labs(x = "", y = "Proportion", title = "ACNT donation willingness") +
  scale_color_brewer(palette = "Dark2", name = "Treatment Group") +
  scale_x_discrete(labels = c("FALSE" = "Untreated", "TRUE" = "Treated"))+
  theme_minimal() + 
  theme(legend.position = "none") 

ggsave(d_plot, filename=here::here("4_output", "plots", "donate_wmt.png"))

donate_dp<-interpretCI::propCI(n1=donation_summary$n[1],
                               n2=donation_summary$n[2],
                               p1=donation_summary$prop[1],
                               p2=donation_summary$prop[2])

