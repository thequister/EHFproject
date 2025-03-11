library(here)
source(here::here('2_code', 'general_retail', '1_libraries_and_settings_gr.R'))
here::i_am("2_code/general_retail/2_data_format_gr.R")

gr <- read_csv(here("0_raw_data", "general_retail", "general_retail_purged.csv"))

# bin_vars <- c("main_job", "manager", "ehf_received", "ehf_donation", 
#               "unemp_benefits")

gr <- gr %>%
#  mutate_at(bin_vars, ~case_match(., "Yes" ~ 1, "No" ~ 0)) %>%
  mutate(tenure_fac = factor(employ_period, levels = 
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
  ehf_aware_pretr = emerg_assist_benefits == "Yes", #binary of whether their employer offers EHF (pick from several) 
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
  mutate(across(c(hire_benefits_pto:hire_benefits_tuition), ~case_match(., 
                                                                        "Not seriously" ~ 0,
                                                                        "Somewhat seriously" ~ 0.5,
                                                                        "Very seriously" ~ 1),
                .names = "{.col}_num")) %>%
  mutate(ehf_hire_bin = (hire_benefits_emerg != "Not seriously"),
  ehf_hire_relative = 
    hire_benefits_emerg_num - mean(
      c_across(c(hire_benefits_pto_num, hire_benefits_health_num, 
                 hire_benefits_retire_num, hire_benefits_parent_num, 
                 hire_benefits_union_num,
                 hire_benefits_tuition_num)), na.rm=T),
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
    (as.numeric(ideology_conlib) - min(as.numeric(ideology_conlib), na.rm = T))/
    (max(as.numeric(ideology_conlib), na.rm = T)- min(as.numeric(ideology_conlib), na.rm = T))), 
  ideology = factor(ideology),
  voted_bin = (voted == "Yes"),
  across(c(govt_responsib_elder:govt_responsib_hardship),
                ~fct_rev(factor(., levels = c(
                  "A lot of responsibility",
                  "Some responsibility",
                  "A little responsibility",
                  "No responsibility"
                ), ordered = T))),
  across(c(govt_responsib_elder:govt_responsib_hardship), 
         ~(as.numeric(.) - min(as.numeric(.)))/
           (max(as.numeric(.))- min(as.numeric(.))),
         .names = "{.col}_num"),
  across(c(govt_responsib_elder:govt_responsib_hardship), ~(. != "No responsibility"),
         .names = "{.col}_bin"), # coded binary T if assigned at least some responsibility
  ed = factor(educ, levels = 
                c("No degree or diploma earned",
                  "High school diploma/GED",
                  "Some college",
                  "Associate's degree",
                  "Bachelor's degree",
                  "Advanced degree (JD, Masters, PhD, etc)")), # included as ed due to THD code
  ed_h = C(ordered(ed), contr.helmert),
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
    (as.numeric(practice_religion) - min(as.numeric(practice_religion)))/
    (max(as.numeric(practice_religion))- min(as.numeric(practice_religion))),
  practice_religion_bin = practice_religion != "Never", # binary, T if practice religion at all
  identify_religion_bin = religion != "Nothing in particular", # binary T if identifies with any religion
  home_ownership = factor(rent),
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
                          "$150,000 or more per year" ~ 150,
                          "At least $100,000 but less than $150,000 per year" ~ 100,
                          "At least 75,000 but less than $100,000 per year" ~ 75,
                          "At least $50,000 but less than $75,000 per year" ~ 50,
                          "At least $35,000 but less than $50,000 per year" ~ 35,
                          "At least $25,000 but less than $35,000 per year" ~ 25,
                          "At least $15,000 but less than $25,000 per year" ~ 15,
                          "Less than $15,000 per year"~ 0, .default = NA),
  across(c(ehf_offer_thd:ehf_offer_costco), ~(. == "Offers an EHF"),
         .names = "{.col}_bin"),
  ehf_support_new = factor(ehf_support_new, levels = 
                               c("Not at all supportive",
                                 "Moderately supportive",
                                 "Extremely supportive"),
                             ordered= TRUE), 
  ehf_support_new_num = 
    (as.numeric(ehf_support_new) - min(as.numeric(ehf_support_new), na.rm = T))/
    (max(as.numeric(ehf_support_new), na.rm = T)- min(as.numeric(ehf_support_new), na.rm = T)), 
  ehf_support_new_bin = ifelse(  #binary variable; T if moderate/extreme
    ehf_support_new %in% c("Moderately supportive", "Extremely supportive"),
    TRUE,
    FALSE),
  ehf_wrk_new = factor(ehf_type_new, levels = 
                             c("Only management should control the fund",
                               "Management should control the fund with worker input",
                               "Workers and management should share control equally",
                               "Workers should control the fund with management input",
                               "Only workers should control the fund"),
                           ordered= TRUE), 
  ehf_wrk_new_num = 
    (as.numeric(ehf_wrk_new) - min(as.numeric(ehf_wrk_new), na.rm = T))/
    (max(as.numeric(ehf_wrk_new), na.rm = T)- min(as.numeric(ehf_wrk_new), na.rm = T)), 
  ehf_wrk_new_bin = ifelse(  #binary variable; T if more worker than management
    ehf_wrk_new %in% c("Workers should control the fund with management input", "Only workers should control the fund"),
    TRUE,
    FALSE),
  ehf_support_exist = factor(ehf_support_exist, levels = 
                             c("Not at all supportive",
                               "Moderately supportive",
                               "Extremely supportive"),
                           ordered= TRUE), 
  ehf_support_exist_num = 
    (as.numeric(ehf_support_exist) - min(as.numeric(ehf_support_exist), na.rm = T))/
    (max(as.numeric(ehf_support_exist), na.rm = T)- min(as.numeric(ehf_support_exist), na.rm = T)), 
  ehf_support_exist_bin = ifelse(  #binary variable; T if moderate/extreme
    ehf_support_exist %in% c("Moderately supportive", "Extremely supportive"),
    TRUE,
    FALSE),
  ehf_wrk_exist = factor(ehf_type_exist, levels = 
                         c("Only management controls the fund",
                           "Management control the fund with worker input",
                           "Workers and management share control equally",
                           "Workers control the fund with management input",
                           "Only workers control the fund"),
                       ordered= TRUE), 
  ehf_wrk_exist_num = 
    (as.numeric(ehf_wrk_exist) - min(as.numeric(ehf_wrk_exist), na.rm = T))/
    (max(as.numeric(ehf_wrk_exist), na.rm = T)- min(as.numeric(ehf_wrk_exist), na.rm = T)), 
  ehf_wrk_exist_bin = ifelse(  #binary variable; T if more worker than management
    ehf_wrk_exist %in% c("Workers control the fund with management input", "Only workers control the fund"),
    TRUE,
    FALSE),
  )

pca_att_dt <- gr %>%
  select(emp_loyal_num, wrk_loyal_num, emp_reco_num, new_job_num) %>%
  mutate(new_job_num = 1 - new_job_num)

pca_pr <- prcomp(pca_att_dt, scale = T, center=T)

gr$attachment_index <- pca_pr$x[,1]

write.csv(gr, here("3_cleaned_data", "general_retail_clean.csv"))

