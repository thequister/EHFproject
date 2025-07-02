#source("1_libraries_and_settings.R")
#source("2_data_format.R")


wmt.hq <- wmt.hq |> 
  mutate(followup = !is.na(age_f),
    union_vote_f = relevel(as.factor(union_vote_f),
      ref= "For the union"),
    uv_consistent =  union_vote_f == union_elec
    )

p_followup <- mean(wmt.hq$followup == TRUE)

# response

followup.basebin<-glm(followup ~ treatment_bin,
  family=binomial, data=wmt.hq)

followup.basefull<-glm(followup ~ treatment_full,
  family=binomial, data=wmt.hq)


your_data_sub <- wmt.hq %>%
  select(treatment_bin, followup, age_clean ,
                male ,
                main_job ,
                tenure_num ,
                nonwhite,
                fulltime,
                hourly,
                college)
followup.covint <- glm(followup ~ treatment_bin* ., 
  family=binomial, data=your_data_sub)

fu_select<-list(followup.basebin, followup.basefull )

names(fu_select) <- c("base", "full treatment")
note1 <- "Standard errors in parentheses."
logit_tab_followup<-modelsummary(fu_select,
             #shape = term + response ~ statistic,
             coef_map = c("treatment_binTRUE" = "Treated",
              "treatment_fullvid0" = "placebo",
              "treatment_fullvidChar" = "charity video",
              "treatment_fullvidSolid" = "solidarity video"),
            title = "Logistic regression selection into Walmart followup survey \\label{tab:tab-fu-wmt}",
            gof_map = gm,
            #add_rows = rows,
            notes = list(note1),
            output = "kableExtra",
            threeparttable=TRUE, 
            stars = c('$^+$' = 0.1, '*' = .05, '**' = .01),
            escape = FALSE
)



#Predict probability of selection into follow-up
wmt.hq <- wmt.hq %>%
  mutate(
    ps = predict(followup.covint, type = "response"),
    ipw = if_else(followup == TRUE, 1 / ps, NA_real_), #weights as big as 13!
    ipw_stab = if_else(followup == 1, p_followup / ps, NA_real_)
    )


followup_data <- wmt.hq %>%
  filter(followup == TRUE)

# union vote followup

uv_wmt_f_mnl <- nnet::multinom(union_vote_f ~ treatment_bin,
                            Hess=TRUE, model = TRUE,
                            trace = FALSE,
                            data = followup_data)

uv_wmt_f_mnl_w <- nnet::multinom(union_vote_f ~ treatment_bin,
                            Hess=TRUE, model = TRUE,
                            trace = FALSE,
                            weights = ipw_stab,
                            data = followup_data)

fumnl<-list(uv_wmt_f_mnl,uv_wmt_f_mnl_w)
names(fumnl) <- c("base", "IPW-weighted")
note1 <- "Reference category is 'For the union'.  Standard errors in parentheses."
multinom_tab_followup<-modelsummary(fumnl,
             shape = term + response ~ statistic,
             coef_map = c("treatment_binTRUE" = "Treated"),
            title = "Multinomial logistic regression of union support in Walmart followup survey \\label{tab:tab-uv-models-fu-wmt}",
            gof_map = gm,
            #add_rows = rows,
            notes = list(note1),
            output = "kableExtra",
            threeparttable=TRUE, 
            stars = c('$^+$' = 0.1, '*' = .05, '**' = .01),
            escape = FALSE
)





# consistent response
