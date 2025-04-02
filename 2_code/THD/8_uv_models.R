#source("1_libraries_and_settings.R")
#source("2_data_format.R")

#unweighted models
THD_comp_uw$union_vote <- relevel(THD_comp_uw$union_vote, 
                                  ref= "For the union")

uv_mnl_uw <- nnet::multinom(union_vote ~ HDTreatment,
                            Hess=TRUE, model = TRUE,
                            trace = FALSE,
                            data = THD_comp_uw)

uv_mnl_int_uw <- nnet::multinom(union_vote ~ HDTreatment*EHF_aware_list,
                            Hess=TRUE, model = TRUE,
                            trace = FALSE,
                            data = THD_comp_uw)

uv_mnl_c_uw <- nnet::multinom(union_vote ~ HDTreatment*EHF_aware_list +
                                  rk_age + male + main_job + tenure_num +
                                  nonwhite + fulltime + college,
                                Hess=TRUE, model = TRUE,
                                trace = FALSE,
                                data = THD_comp_uw)

uv.models.uw<-list( uv_mnl_uw, uv_mnl_int_uw, uv_mnl_c_uw)
names(uv.models.uw) <- c("Base", "Pre-exposure", "Covariates")
coef_maps <- c("HDTreatmenttxt" = "Text treatment",
               "HDTreatmentvid" = "Video treatment",
               "EHF_aware_listTRUE" = "Pre-exposed",
               "HDTreatmenttxt:EHF_aware_listTRUE" = "Text x pre-exposed",
               "HDTreatmentvid:EHF_aware_listTRUE" = "Video x pre-exposed")

rows<-tribble(
  ~"term", ~"Base", ~"Preexposure",  ~"Covariates",
  "Covariates?", "No", "No", "Yes")
attr(rows, 'position') <- c(12)

note1 <- "Reference category is 'For the union'. Covariates include age, gender race, job tenure, full time status, college degree, and main job.  Hourly worker indicator was excluded due to perfect separation."
#note2 <- "Robust standard errors in parentheses."


gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 0))

multinom_tab<-modelsummary(uv.models.uw,
             #shape = term + y.level ~ statistic,
             shape = term + response ~ statistic,
             coef_map = coef_maps,
            title = "Multinomial logistic regression of union support \\label{tab:tab-uv}",
            gof_map = gm,
            notes = list(note1),
            threeparttable=TRUE, 
            stars = c('*' = .05, '**' = .01),
            escape = FALSE
)

eff_probs<-Effect(c("HDTreatment", "EHF_aware_list"), uv_mnl_int_uw, se = TRUE)

mnl_eff_plot <- plot(eff_probs,
     main = "Treament effect on union vote\n by pre-exposure status",
     xlab = "Experimental condition",
     axes = list(
       y = list(type = "probability",
                lab="predicted probability")),
     lattice=list(strip=list(factor.names=FALSE)),
     colors = grey(0.5)
)

mnl_eff_plot$condlevels$EHF_aware_list<-c("unaware", "aware")

png(here::here("4_output","plots", "mnl_eff.png"))
print(mnl_eff_plot)
dev.off()


#mnl_eff_plot

#weighted union vote models
uv_multinom_b<- svy_vglm(
    formula = union_vote ~ HDTreatment,
    design = THD_comp, 
    family = multinomial(refLevel = "Not sure"))

uv_multinom_b_int<- svy_vglm(
    formula = union_vote ~ HDTreatment*EHF_aware_list + tenure_num,
    design = THD_comp, 
    family = multinomial(refLevel = "Not sure"))


uv_multinom_c<- svy_vglm(
    formula = union_vote ~ HDTreatment +
      rk_age + male + main_job + tenure_num +
      nonwhite + fulltime + college, #hourly removed b/c perfect sep
    design = THD_comp, 
    family = multinomial(refLevel = "Not sure"))

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