library(here)
source(here::here('2_code', '1_libraries_and_settings_global.R'))
here::i_am("2_code/Walmart/2_data_format_ACNT.R")

ACNT_uw <- read_csv(here("0_raw_data", "ACNT", "ACNT_full.csv"))

ACNT_uw <- ACNT_uw %>%
#  mutate_at(bin_vars, ~case_match(., "Yes" ~ 1, "No" ~ 0)) %>%
  mutate(
    treatment_framing = case_when(
    treatment_full == "vidSolid" ~ "solidarity",
    treatment_full == "vidChar" ~ "charity",
    TRUE ~ "cntrl"),
    tenure_fac = factor(employ_period, levels = 
                          c("Less than 6 months", "At least 6 months but less than 1 year",
                          "At least 1 year but less than 2 years",
                          "At least 2 years but less than 3 years",
                          "3 or more years")),
  tenure_fac_h = C(ordered(tenure_fac),contr.helmert), 
  tenure_ord_num = unclass(tenure_fac_h),
  tenure_num = case_match(employ_period, "Less than 6 months" ~ 0,
                          "At least 6 months but less than 1 year" ~ 6,
                          "At least 1 year but less than 2 years" ~ 12,
                          "At least 2 years but less than 3 years" ~ 24,
                          "3 or more years" ~ 36),
  num_jobs_fac = factor(num_jobs, levels = c("1", "2", "3 or more")), 
  num_jobs_fac_h = C(ordered(num_jobs_fac),contr.helmert),
  multiple_jobs = (num_jobs != "1"), # binary var indicating multiple jobs
  fulltime = employ_status == "Regular full-time",
  hourly = pay_type == "Hourly wage",
  pph = ifelse(pph > 151, NA, pph), #remove values that are over 151 hourly
  ehf_aware_pretr = emerg_assist_benefits == "Yes", #binary of whether they knew about emergency cash pre-treatment
  health_job = health_job == "I get health coverage through my job at ${e://Field/EmployerName}", # health ins through job
  emergency_expense = factor(expense, levels = 
                         c("I am certain I could not come up with $400",
                           "I could probably not come up with $400",
                           "I could probably come up with $400",
                           "I am certain I could come up with the full $400"),
                       ordered= TRUE), 
  emergency_expense_num = 
    (as.numeric(emergency_expense) - min(as.numeric(emergency_expense)))/
    (max(as.numeric(emergency_expense))- min(as.numeric(emergency_expense))), 
  emergency_expense_bin = ifelse(  #binary variable; T if can cover expense 
    grepl(" not ", expense),
    FALSE,
    TRUE),
  new_job = fct_rev(factor(new_job, levels = 
                         c("Extremely likely",
                         "Very likely",
                         "Somewhat likely",
                         "Not very likely", 
                         "Not at all likely"),
                     ordered= TRUE)), 
  new_job_num = 
    (as.numeric(new_job) - min(as.numeric(new_job)))/
    (max(as.numeric(new_job))- min(as.numeric(new_job))), 
  new_job_bin = ifelse(  #binary variable; T if more than somewhat 
    grepl("Not ", new_job),
    FALSE,
    TRUE),
  wrk_loyal = fct_rev(factor(loyal_workers, levels = 
                     c("A lot of loyalty",
                       "Some loyalty",
                       "Only a little loyalty",
                       "No loyalty at all"),
                   ordered= TRUE)), 
  wrk_loyal_num = 
    (as.numeric(wrk_loyal) - min(as.numeric(wrk_loyal)))/
    (max(as.numeric(wrk_loyal))- min(as.numeric(wrk_loyal))), 
  wrk_loyal_bin = ifelse(  #binary variable; T if some/a lot 
    wrk_loyal %in% c("A lot of loyalty", "Some loyalty"),
    TRUE,
    FALSE),
  emp_loyal = fct_rev(factor(loyal_comp, levels = 
                           c("A lot of loyalty",
                             "Some loyalty",
                             "Only a little loyalty",
                             "No loyalty at all"),
                         ordered= TRUE)), 
  emp_loyal_num = 
    (as.numeric(emp_loyal) - min(as.numeric(emp_loyal)))/
    (max(as.numeric(emp_loyal))- min(as.numeric(emp_loyal))), 
  emp_loyal_bin = ifelse(  #binary variable; T if some/a lot 
    emp_loyal %in% c("A lot of loyalty", "Some loyalty"),
    TRUE,
    FALSE),
  union_vote_agg = ifelse(union_elec == "Not sure", union_unsure, union_elec),
  union_vote = fct_rev(factor(union_vote_agg, levels = 
                     c("For the union",
                       "Leaning toward voting for the union",
                       "Not sure",
                       "Leaning against voting for the union", 
                       "Against the union"),
                   ordered= TRUE)), 
  union_vote_num = 
    (as.numeric(union_vote) - min(as.numeric(union_vote)))/
    (max(as.numeric(union_vote))- min(as.numeric(union_vote))), 
  union_vote_bin = ifelse(  #binary variable; T if leans towards union 
    union_vote %in% c("For the union", "Leaning toward voting for the union"),
    TRUE,
    FALSE),
  emp_reco = fct_rev(factor(recommend, levels = 
                            c("Certainly would recommend",
                              "Might recommend",
                              "Not sure",
                              "Might not recommend", 
                              "Definitely would not recommend"),
                          ordered= TRUE)), 
  emp_reco_num = 
    (as.numeric(emp_reco) - min(as.numeric(emp_reco)))/
    (max(as.numeric(emp_reco))- min(as.numeric(emp_reco))), 
  emp_reco_bin = ifelse(  #binary variable; T if leans recommending
    emp_reco %in% c("Certainly would recommend", "Might recommend"),
    TRUE,
    FALSE),
  ehf_reason_bin = ifelse(is.na(ehf_reason), 0, 1),
  ehf_time = factor(ehf_time, levels = 
                      c("Don’t know",
                        "Less than 2 hours",
                        "2 to 6 hours",
                        "6 to 12 hours",
                        "More than 12 hours"),
                    ordered = T),
  ehf_time_num = case_match(ehf_time, "Less than 2 hours" ~ 0,
                        "2 to 6 hours" ~ 2,
                        "6 to 12 hours" ~ 6,
                        "More than 12 hours" ~ 12),
  emergency_bank = grepl("Borrowed money from a bank", emergency_aid),
  emergency_cc = grepl("Used a credit card", emergency_aid),
  emergency_pawn = grepl("Borrowed money using a pawnshop, payday lender, or title lender", emergency_aid),
  emergency_public = grepl("Public assistance (SNAP, food stamps, cash welfare)", emergency_aid),
  emergency_unemp = grepl("Unemployment insurance", emergency_aid),
  emergency_disab = grepl("Disability insurance or worker’s compensation", emergency_aid),
  emergency_fam = grepl("Family or friends", emergency_aid),
  emergency_church = grepl("Church, synagogue, mosque or other religious community", emergency_aid),
  emergency_other = grepl("Other", emergency_aid),
  emergency_sum = rowSums(dplyr::across(emergency_bank:emergency_other)),
  ehf_coverage = fct_rev(factor(ehf_coverage, levels = 
                       c("It covered all my emergency needs",
                         "It covered more than ½ but not all of my emergency needs",
                         "It covered about ½ of my emergency needs",
                         "It made some difference but covered less than ½ of my emergency needs",
                         "It did not make much difference for my emergency needs"),
                     ordered = T)),
  ehf_coverage_num = 
    (as.numeric(ehf_coverage) - min(as.numeric(ehf_coverage)))/
    (max(as.numeric(ehf_coverage))- min(as.numeric(ehf_coverage))), 
  ehf_coverage_bin = ifelse(  #binary variable; T if at least half covered
    recommend %in% c("It covered all my emergency needs",
                     "It covered more than ½ but not all of my emergency needs",
                     "It covered about ½ of my emergency needs"),
    TRUE,
    FALSE),
  ehf_delay = factor(ehf_delay, levels = 
                      c("Less than 48 hours",
                        "Between 2 and 7 days",
                        "Between 1 and 2 weeks",
                        "More than 2 weeks"),
                    ordered = T),
  ehf_delay_num = case_match(ehf_delay, "Less than 48 hours" ~ 0,
                         "Between 2 and 7 days" ~ 2,
                         "Between 1 and 2 weeks" ~ 7,
                         "More than 2 weeks" ~ 14),
  ehf_received_all = case_when(ehf_received == "Yes" ~ T, 
                               ehf_exist == "No" ~ NA, 
                               .default = F),
  app_time_unemp = factor(app_time, levels = 
                      c("I did not fill out the paperwork for the claim",
                        "Don’t know",
                        "Less than 2 hours",
                        "2 to 6 hours",
                        "6 to 12 hours",
                        "More than 12 hours"),
                    ordered = T),
  app_time_unemp_num = case_match(app_time_unemp, "Less than 2 hours" ~ 0,
                            "2 to 6 hours" ~ 2,
                            "6 to 12 hours" ~ 6,
                            "More than 12 hours" ~ 12),
  delay_unemp = factor(delay, levels = 
                       c("Claim was rejected/never got money",
                         "Between 2 and 5 days",
                         "Between 5 days and a week",
                         "Between 1 and 2 weeks",
                         "More than 2 weeks"),
                     ordered = T),
  delay_unemp_num = case_match(delay_unemp, "Between 2 and 5 days" ~ 2,
                         "Between 5 days and a week" ~ 5,
                             "Between 1 and 2 weeks" ~ 7,
                             "More than 2 weeks" ~ 14)) %>%
  mutate(dplyr::across(c(hire_benefits_pto:hire_benefits_tuition), ~case_match(., 
                                                                        "Not seriously" ~ 0,
                                                                        "Somewhat seriously" ~ 0.5,
                                                                        "Very seriously" ~ 1),
                .names = "{.col}_num")) %>%
  mutate(ehf_hire_bin = (hire_benefits_emerg != "Not seriously")) |>
  rowwise() |>  # This ensures that the operations are performed row by row
  mutate(ehf_hire_relative = hire_benefits_emerg_num - mean(
    dplyr::c_across(c(hire_benefits_pto_num, hire_benefits_health_num, 
      hire_benefits_retire_num, hire_benefits_parent_num, 
      hire_benefits_union_num,
      hire_benefits_tuition_num)), na.rm=T),
  #ehf_hire_relative =   #old code; replaced by JSA
   # hire_benefits_emerg_num/rowSums(across(hire_benefits_pto_num:hire_benefits_tuition_num)), #what percentage of overall score
  ideology_answered = ifelse(ideology == "Haven’t thought much about this", NA, ideology),
  ideology_conlib = fct_rev(factor(ideology_answered, 
                           levels = 
                             c("Extremely liberal",
                               "Liberal",
                               "Moderate",
                               "Conservative",
                               "Extremely conservative"),
                           ordered = T)),
  ideology_conlib_num = ifelse(is.na(ideology_conlib), NA,
    (as.numeric(ideology_conlib) - 1)/4), 
  ideology = factor(ideology),
  vote_lik_fix_flag = (RecordedDate <= "2024-11-13 09:22:16"),
  vote_lik_fix = case_when(RecordedDate > "2024-11-13 09:22:16" ~ vote_lik_post_elec,
                           !is.na(vote_pres_post_elec) ~ "I'm sure I voted", 
                           .default = "I did not vote (in the election this November)"),
  voted = (vote_lik_fix == "I'm sure I voted"),
  dplyr::across(c(govt_responsib_elder:govt_responsib_hardship),
                ~fct_rev(factor(., levels = c(
                  "A lot of responsibility",
                  "Some responsibility",
                  "A little responsibility",
                  "No responsibility"
                ), ordered = T))),
  dplyr::across(c(govt_responsib_elder:govt_responsib_hardship), 
        ~(as.numeric(.))/4,
      .names = "{.col}_num"),
  dplyr::across(c(govt_responsib_elder:govt_responsib_hardship), ~(. != "No responsibility"),
         .names = "{.col}_bin"), # coded binary T if assigned at least some responsibility
  ed = factor(educ, levels = 
                c("No degree or diploma earned",
                  "High school diploma/GED",
                  "Some college",
                  "Associate's degree",
                  "Bachelor's degree",
                  "Advanced degree (JD, Masters, PhD, etc)")), # included as ed due to THD code
  college = ed %in% c(
    "Bachelor's degree",
    "Advanced degree (JD, Masters, PhD, etc)"),
  male = gender == "Man",
  nonwhite = ethn_race != "White",
  practice_religion = fct_rev(factor(practice_religion, levels =
                               c("At least once per week",
                                 "Once a week",
                                 "Once or twice a month",
                                 "A few times a year",
                                 "Never"),
                             ordered = T)),
  practice_religion_num = 
    (as.numeric(practice_religion) - 1)/4,
  practice_religion_bin = practice_religion != "Never", # binary, T if practice religion at all
  identify_religion_bin = religion != "Nothing in particular", # binary T if identifies with any religion
  home_ownership = rent == "Own",
  healthcare = (health_ins == "Yes"),
  income = factor(income, levels = 
                    c("Prefer not to state", "$150,000 or more per year",
                      "At least $100,000 but less than $150,000 per year",
                      "At least 75,000 but less than $100,000 per year",
                      "At least $50,000 but less than $75,000 per year",
                      "At least $35,000 but less than $50,000 per year",
                      "At least $25,000 but less than $35,000 per year",
                      "At least $15,000 but less than $25,000 per year",
                      "Less than $15,000 per year"),
                  ordered = T),
  income_num = case_match(income,
                          "$150,000 or more per year" ~ 200,
                          "At least $100,000 but less than $150,000 per year" ~ 125,
                          "At least 75,000 but less than $100,000 per year" ~ 87.5,
                          "At least $50,000 but less than $75,000 per year" ~ 62.5,
                          "At least $35,000 but less than $50,000 per year" ~ 42.5,
                          "At least $25,000 but less than $35,000 per year" ~ 30,
                          "At least $15,000 but less than $25,000 per year" ~ 20,
                          "Less than $15,000 per year"~ 7.5, .default = NA),
  )

## Create the attachment index
pca_att_dt <- ACNT_uw %>%
  select(emp_loyal_num, wrk_loyal_num, emp_reco_num, new_job_num) %>%
  mutate(new_job_num = 1 - new_job_num)

#pca_res <- principal(pca_att_dt, cor = "poly", 
#                     rotate = "none", nfactors = 4)
#pca_att_dt2 <- pca_att_dt %>%
#  mutate(new_job_num = 1+4*new_job_num,
#         emp_reco_num = 1+4*emp_reco_num,
#         wrk_loyal_num = 1+3*wrk_loyal_num,
#         emp_loyal_num = 1+3*emp_loyal_num)
#test<-ordPens::ordPCA(pca_att_dt2, p = 1, lambda = 0.5, maxit = 100,
#             constr = rep(TRUE,ncol(pca_att_dt2)),
#             CV = FALSE)  #produces results almost identical to prcomp

pca_pr <- prcomp(pca_att_dt, scale = T, center=T)
fa_pr <- psych::fa(pca_att_dt, nfactors = 1, scores = "regression")

ACNT_uw$attachment_index <- as.numeric(fa_pr$scores)
ACNT_uw$attachment_index_pca <- pca_pr$x[,1]

## Ammendments
ACNT_uw <- ACNT_uw %>%
  mutate(no_ideology = ideology == "Haven’t thought much about this", 
         ideology_conlib_num_0 = ifelse(is.na(ideology_conlib), -1,
                                        (as.numeric(ideology_conlib) - 1)/6), 
         religious = (religion != "Nothing in particular"),
         ideology_conservative = ifelse(ideology_conlib %in% c("Extremely conservative", "Conservative",  "Slightly Conservative"),1,0),
         welfare = (other_welfare=="Yes")
         )

ACNT_uw$ehf_donation_num <- ACNT_uw$ehf_donation == "Yes"
ACNT_uw$ehf_donation_post <- ACNT_uw$donate == "YES I would like to learn how to donate"

write_csv(ACNT_uw, here("3_cleaned_data", "ACNT_clean_main.csv"))


