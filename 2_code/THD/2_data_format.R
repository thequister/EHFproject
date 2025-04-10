#source("1_libraries_and_settings.R")
#library(here)
#source(here::here('2_code', 'THD', '1_libraries_and_settings.R'))
#here::i_am("1_code/THD/2_data_format.R")

set.seed(123)
THD_comp_uw<-read_csv(here("0_raw_data", "THD", "THD_completed.csv")) %>%
  mutate(emergency_expense = factor(Q3.4, levels = 
                                           c("I am certain I could not come up with $400",
                                             "I could probably not come up with $400",
                                             "I could probably come up with $400",
                                             "I am certain I could come up with the full $400"),
                                    ordered= TRUE
                                    ),
         emergency_expense_num = 
           (as.numeric(emergency_expense) - min(as.numeric(emergency_expense)))/
           (max(as.numeric(emergency_expense))- min(as.numeric(emergency_expense))), 
         emergency_expense_bin = ifelse(  #binary expense variable; T if can cover expense 
           grepl(" not ", Q3.4),
           FALSE,
           TRUE),
         bills = factor(Q6.15),
         EHF_aware_list = grepl("Cash", Q2.15, fixed=T), #asked of all respondents
         EHF_aware_simp = as.factor(Q3.10),  #asked only of control group
         HF_know = factor(Q3.12), # VARNAMES HERE DIFFERENT FROM ACNT
         HF_applied = factor(Q3.13),
         HF_mgr = factor(Q3.16),
         HF_received = factor(Q3.19),
         HF_donate = factor(Q3.24),
         nonwhite = Q6.3 != "White",
         main_job = Q2.7 == "Yes",
         tenure_fac = factor(Q2.9, levels = 
                               c("Less than 6 months", "At least 6 months but less than 1 year",
                                 "At least 1 year but less than 2 years",
                                 "At least 2 years but less than 3 years",
                                 "3 or more years")
                             ),
         tenure_fac_h = C(ordered(tenure_fac),contr.helmert),
         tenure_ord_num = unclass(tenure_fac_h),
         tenure_num = case_match(Q2.9, "Less than 6 months" ~ 0,
          "At least 6 months but less than 1 year" ~ 6,
          "At least 1 year but less than 2 years" ~ 12,
          "At least 2 years but less than 3 years" ~ 24,
          "3 or more years" ~ 36),
         fulltime = Q2.10 == "Regular full-time",
         hourly = Q2.13 == "Yes",
         ed = factor(Q6.5, levels = 
                       c("No degree or diploma earned",
                         "High school diploma/GED",
                         "Some college",
                         "Associate's degree",
                         "Bachelor's degree",
                         "Master's degree/Advanced degree")),
         ed_h = C(ordered(ed), contr.helmert),
         college = ed %in% c(
                         "Bachelor's degree",
                         "Master's degree/Advanced degree"),
         male = Q6.2=="Male",
         wrk_loyal = factor(Q3.5, levels = 
                               c("No loyalty at all", "Only a little loyalty",
                                 "Some loyalty","A lot of loyalty"),
                             ordered = TRUE),
         wrk_loyal_num = (as.numeric(wrk_loyal) - min(as.numeric(wrk_loyal), na.rm=T))/
           (max(as.numeric(wrk_loyal), na.rm=T)- min(as.numeric(wrk_loyal), na.rm=T)),
         emp_loyal = factor(Q3.6, levels = 
                               c("No loyalty at all", "Only a little loyalty",
                                 "Some loyalty","A lot of loyalty"),
                             ordered = TRUE),
         emp_loyal_num = (as.numeric(emp_loyal) - min(as.numeric(emp_loyal), na.rm=T))/
           (max(as.numeric(emp_loyal), na.rm=T)- min(as.numeric(emp_loyal), na.rm=T)),
         union_vote = factor(Q3.7),
         emp_reco = factor(Q3.8, levels = 
                               c("Definitely would not recommend", "Might not recommend",
                                 "Not sure", "Might recommend",
                                 "Certainly would recommend"),
                             ordered = TRUE),
         emp_reco_num = (as.numeric(emp_reco) - min(as.numeric(emp_reco), na.rm=T))/
           (max(as.numeric(emp_reco), na.rm=T)- min(as.numeric(emp_reco), na.rm=T)),
         #emp_reco_bin = emp
         gov_pension = factor(Q3.9_1, levels =
                           c("No responsibility", "A little responsibility",
                             "Some responsibility", "A lot of responsibility"),
                         ordered=TRUE),
         gov_pension_num = (as.numeric(gov_pension) - min(as.numeric(gov_pension), na.rm=T))/ # different from ACNT
           (max(as.numeric(gov_pension), na.rm=T)- min(as.numeric(gov_pension), na.rm=T)),
         gov_ui = factor(Q3.9_2, levels =
                           c("No responsibility", "A little responsibility",
                             "Some responsibility", "A lot of responsibility"),
                         ordered=TRUE),
         gov_ui_num = (as.numeric(gov_ui) - min(as.numeric(gov_ui), na.rm=T))/
           (max(as.numeric(gov_ui), na.rm=T)- min(as.numeric(gov_ui), na.rm=T)),
         gov_childcare = factor(Q3.9_3, levels =
                           c("No responsibility", "A little responsibility",
                             "Some responsibility", "A lot of responsibility"),
                         ordered=TRUE),
         gov_childcare_num = (as.numeric(gov_childcare) - min(as.numeric(gov_childcare), na.rm=T))/
           (max(as.numeric(gov_childcare), na.rm=T)- min(as.numeric(gov_childcare), na.rm=T)),
         home_ownership = factor(Q6.13),
         healthcare = (Q2.16 == "Yes")
         ) 

pca_att_dt2 <- THD_comp_uw  |> 
  select(emp_loyal_num, wrk_loyal_num, emp_reco_num)  
  #mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm = TRUE)))) #use this for mean imputation
  
library(tidymodels)  
rec <- recipe(~ emp_loyal_num + wrk_loyal_num +emp_reco_num, 
              data = pca_att_dt2)  |> 
  step_impute_knn(all_predictors(), neighbors = 5) |> 
  prep(data = pca_att_dt2)

pca_att_dt_knn<-bake(rec, new_data = NULL)   

#pca_pr <- prcomp(pca_att_dt2, scale = T, center=T)
pca_pr_knn <- prcomp(pca_att_dt_knn, scale = T, center=T)
#pca_pr_knn$rotation # 1PC is negatively loading on all items so reverse scaling

THD_comp_uw$attachment_index <- -pca_pr_knn$x[,1]

THD_comp <- THD_comp_uw %>%
  srvyr::as_survey_design(ids = 1, weights = rk_wgt)
