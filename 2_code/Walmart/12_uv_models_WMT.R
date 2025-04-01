#source("1_libraries_and_settings.R")
#source("2_data_format.R")

#unweighted models
wmt.hq$union_elec <- relevel(as.factor(wmt.hq$union_elec), 
                                  ref= "For the union")

uv_wmt_mnl <- nnet::multinom(union_elec ~ treatment_bin,
                            Hess=TRUE, model = TRUE,
                            trace = FALSE,
                            data = wmt.hq)

uv_wmt_mnl_c <- nnet::multinom(union_elec ~ treatment_bin+
                                 age_clean +
                                 male +
                                 main_job +
                                 tenure_num +
                                 nonwhite +
                                 fulltime +
                                 hourly+
                                 college,
                               Hess=TRUE, model = TRUE,
                               trace = FALSE,
                               data = wmt.hq)


uv_wmt_mnl_int <- nnet::multinom(union_elec ~ treatment_bin*ehf_aware_pretr,
                            Hess=TRUE, model = TRUE,
                            trace = FALSE,
                            data = wmt.hq)

uv_wmt_mnl_int_c <- nnet::multinom(union_elec ~ treatment_bin*ehf_aware_pretr+
                                age_clean +
                                male +
                                main_job +
                                tenure_num +
                                nonwhite +
                                fulltime +
                                hourly+
                                college,
                                Hess=TRUE, model = TRUE,
                                trace = FALSE,
                                data = wmt.hq)

uv.wmt.models.hq<-list( uv_wmt_mnl, uv_wmt_mnl_c, uv_wmt_mnl_int, uv_wmt_mnl_int_c)
names(uv.wmt.models.hq) <- c("Base", "Covariates", "Pre-exposure", "Pre-exposure +\n covariates")
coef_maps <- c(
               "treatment_binTRUE" = "Treated",
               "ehf_aware_pretrTRUE" = "Pre-exposed",
               "treatment_binTRUE:ehf_aware_pretrTRUE" = "Treated x pre-exposed")

rows<-tribble(
 ~"",~"",~"",~"",~"",~"",~"",~"",~"",~"",
  # ~"term", ~"",~"Base",~"", ~"Covariates",~"", ~"Preexposure",~"",  ~"Pre-covariates",~"",
  "Covariates?", "","No","", "Yes","", "No","", "Yes","")
attr(rows, 'position') <- c(9)

note1 <- "Reference category is 'For the union'. Covariates include age, gender race, job tenure, hourly and full time status, college degree, and main job."
#note2 <- "Robust standard errors in parentheses."


gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 0))

multinom_tab_wmt<-modelsummary(uv.wmt.models.hq,
             shape = term + response ~ statistic,
             coef_map = coef_maps,
            title = "Multinomial logistic regression of union support \\label{tab:tab-uv-models-wmt}",
            gof_map = gm,
            #add_rows = rows,
            notes = list(note1),
            output = "kableExtra",
            threeparttable=TRUE, 
            stars = c('*' = .05, '**' = .01),
            escape = FALSE
)


eff_probs_uv_wmt<-marginaleffects::avg_comparisons(uv_wmt_mnl_c, variables = "treatment_bin")

wmt_mnl_uv_interp <-ggplot(eff_probs_uv_wmt, aes(x = group, y = estimate)) +
  geom_point() +  # Adds the dots for estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +  # Adds the whiskers for confidence intervals
  labs(title = "Average Treatment Effects",
       x = "Union vote",
       y = "Change in predicted probability")
  
# eff_probs<-Effect(c("treatment_bin"), uv_wmt_mnl_c , se = TRUE)
# 
# mnl_eff_plot <- plot(eff_probs,
#      main = "Treament effect on union vote\n by pre-exposure status",
#      xlab = "Experimental condition",
#      axes = list(
#        y = list(type = "probability",
#                 lab="predicted probability")),
#      lattice=list(strip=list(factor.names=FALSE)),
#      colors = grey(0.5)
# )
# 
# mnl_eff_plot$condlevels$EHF_aware_list<-c("unaware", "aware")
# 
# png(here::here("output","plots", "mnl_eff.png"))
# print(mnl_eff_plot)
# dev.off()
# 
# 
# #mnl_eff_plot

# coworker support model
union_supp_pct<-lm(union_coworkers_6 ~ treatment_bin,
   data = wmt.hq)

union_supp_pct_c<-lm(union_coworkers_6 ~ treatment_bin + 
                           age_clean + 
                           male +
                           main_job +
                           tenure_num +
                           nonwhite +
                           fulltime +
                           hourly+
                           college,
                         data = wmt.hq)


union_supp_pct_int<-lm(union_coworkers_6 ~ treatment_bin*ehf_aware_pretr,
   data = wmt.hq)

union_supp_pct_int_c<-lm(union_coworkers_6 ~ treatment_bin*ehf_aware_pretr + 
                       age_clean + 
                       male +
                       main_job +
                       tenure_num +
                       nonwhite +
                       fulltime +
                       hourly+
                       college,
                     data = wmt.hq)

union_supp_pct_models <- list(union_supp_pct,union_supp_pct_c,union_supp_pct_int)
names(union_supp_pct_models) <- c("base", "covariates", "pre-exposed")
note1 <- "Robust standard errors in parentheses. Covariates include age, gender race, job tenure, hourly and full time status, college degree, and main job."
#note2 <- ""


gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2),
  list("raw" = "F", "clean" = "F", "fmt" = 2))

union_supp_pct_tab_wmt<-modelsummary(union_supp_pct_models, 
                               coef_map = coef_maps,
                               title = "OLS regression of expected union support among co-workers \\label{tab:tab-usupp-models-wmt}",
                               vcov = "robust",
                               gof_map = gm,
                               output = "kableExtra",
                               #add_rows = rows,
                               notes = list(note1),
                               threeparttable=TRUE, 
                               stars = c('*' = .05, '**' = .01),
                               escape = FALSE
)



# out.tab<-summary(uv_multinom_c)

# rownames(out.tab$coeftable) <- c("(Intercept):For", "(Intercept):Against",
#                        "TRT:txt:For", "TRT:txt:Against",
#                        "TRT:vid:For", "TRT:vid:Against",
#                        "age:For", "age:Against", "male:For", "male:Against",
#                        "main job:For", "main job:Against",
#                        "tenure: 6-12mos:For", "tenure: 6-12mos:Against",
#                        "1-2 yrs.:For","1-2 yrs.:Against", "2-3 yrs.:For", "2-3 yrs.:Against",
#                        "3+ yrs.:For","3+ yrs.:Against",
#                        "nonwhite:For", "nonwhite:Against",
#                        "fulltime:For", "fulltime:Against",
#                        #"hourly:For", "hourly:Against",
#                        "college:For", "college:Against")
                               
# Checking estimates by comparing For union vs DK
#uv_fornot<- svy_vglm(
#    formula = I(union_vote=="For the union") ~ HDTreatment,
#    design = subset(THD_comp, union_vote != "Against the union"),
#   family=binomialff)
#```

# ```{r tab-uv, echo=FALSE, results='asis'}

# stargazer(out.tab$coeftable,
#           digits = 2,
#           title = "Multinomial logistic regression on support for unionization",
#           notes = "Reference category is DK.",
#           header=FALSE,
#           no.space = TRUE,
#           notes.align = "l",
#           notes.append=TRUE,
#           label = "tab:tab-uv"
#           )
# ```