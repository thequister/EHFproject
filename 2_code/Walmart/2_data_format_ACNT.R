library(here)
source(here::here('2_code', 'Walmart', '1_libraries_and_settings_ACNT.R'))
here::i_am("2_code/Walmart/2_data_format_ACNT.R")

ACNT_uw <- read_csv(here("0_raw_data", "ACNT", "ACNT_clean.csv"))

# bin_vars <- c("main_job", "manager", "ehf_received", "ehf_donation", 
#               "unemp_benefits")

ACNT_uw <- ACNT_uw %>%
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
  new_job = factor(new_job, levels = 
                         c("Extremely likely",
                         "Very likely",
                         "Somewhat likely",
                         "Not very likely", 
                         "Not at all likely"),
                     ordered= TRUE), 
  new_job_num = 
    (as.numeric(new_job) - min(as.numeric(new_job)))/
    (max(as.numeric(new_job))- min(as.numeric(new_job))), 
  new_job_bin = ifelse(  #binary variable; T if more than somewhat 
    grepl("Not ", new_job),
    FALSE,
    TRUE),
  loyal_workers = factor(loyal_workers, levels = 
                     c("A lot of loyalty",
                       "Some loyalty",
                       "Only a little loyalty",
                       "No loyalty at all"),
                   ordered= TRUE), 
  loyal_workers_num = 
    (as.numeric(loyal_workers) - min(as.numeric(loyal_workers)))/
    (max(as.numeric(loyal_workers))- min(as.numeric(loyal_workers))), 
  loyal_workers_bin = ifelse(  #binary variable; T if some/a lot 
    loyal_workers %in% c("A lot of loyalty", "Some loyalty"),
    FALSE,
    TRUE),
  loyal_comp = factor(loyal_comp, levels = 
                           c("A lot of loyalty",
                             "Some loyalty",
                             "Only a little loyalty",
                             "No loyalty at all"),
                         ordered= TRUE), 
  loyal_comp_num = 
    (as.numeric(loyal_comp) - min(as.numeric(loyal_comp)))/
    (max(as.numeric(loyal_comp))- min(as.numeric(loyal_comp))), 
  loyal_comp_bin = ifelse(  #binary variable; T if some/a lot 
    loyal_comp %in% c("A lot of loyalty", "Some loyalty"),
    FALSE,
    TRUE),
  union_vote_agg = ifelse(union_elec == "Not sure", union_unsure, union_elec),
  union_vote_agg = factor(union_vote_agg, levels = 
                     c("For the union",
                       "Leaning toward voting for the union",
                       "Not sure",
                       "Leaning against voting for the union", 
                       "Against the union"),
                   ordered= TRUE), 
  union_vote_agg_num = 
    (as.numeric(union_vote_agg) - min(as.numeric(union_vote_agg)))/
    (max(as.numeric(union_vote_agg))- min(as.numeric(union_vote_agg))), 
  union_vote_agg_bin = ifelse(  #binary variable; T if leans towards union 
    union_vote_agg %in% c("For the union", "Leaning toward voting for the union"),
    FALSE,
    TRUE),
  recommend = factor(recommend, levels = 
                            c("Certainly would recommend",
                              "Might recommend",
                              "Not sure",
                              "Might not recommend", 
                              "Definitely would not recommend"),
                          ordered= TRUE), 
  recommend_num = 
    (as.numeric(recommend) - min(as.numeric(recommend)))/
    (max(as.numeric(recommend))- min(as.numeric(recommend))), 
  recommend_bin = ifelse(  #binary variable; T if leans recommending
    recommend %in% c("Certainly would recommend", "Might recommend"),
    FALSE,
    TRUE),
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
  ehf_coverage = factor(ehf_coverage, levels = 
                       c("It covered all my emergency needs",
                         "It covered more than ½ but not all of my emergency needs",
                         "It covered about ½ of my emergency needs",
                         "It made some difference but covered less than ½ of my emergency needs",
                         "It did not make much difference for my emergency needs"),
                     ordered = T),
  ehf_coverage_num = 
    (as.numeric(ehf_coverage) - min(as.numeric(ehf_coverage)))/
    (max(as.numeric(ehf_coverage))- min(as.numeric(ehf_coverage))), 
  ehf_coverage_bin = ifelse(  #binary variable; T if at least half covered
    recommend %in% c("It covered all my emergency needs",
                     "It covered more than ½ but not all of my emergency needs",
                     "It covered about ½ of my emergency needs"),
    FALSE,
    TRUE),
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
  app_time = factor(app_time, levels = 
                      c("I did not fill out the paperwork for the claim",
                        "Don’t know",
                        "Less than 2 hours",
                        "2 to 6 hours",
                        "6 to 12 hours",
                        "More than 12 hours"),
                    ordered = T),
  app_time_num = case_match(app_time, "Less than 2 hours" ~ 0,
                            "2 to 6 hours" ~ 2,
                            "6 to 12 hours" ~ 6,
                            "More than 12 hours" ~ 12),
  delay = factor(delay, levels = 
                       c("Claim was rejected/never got money",
                         "Between 2 and 5 days",
                         "Between 5 days and a week",
                         "Between 1 and 2 weeks",
                         "More than 2 weeks"),
                     ordered = T),
  delay_num = case_match(delay, "Between 2 and 5 days" ~ 2,
                         "Between 5 days and a week" ~ 5,
                             "Between 1 and 2 weeks" ~ 7,
                             "More than 2 weeks" ~ 14)) %>%
  mutate(across(c(hire_benefits_pto:hire_benefits_tuition), ~case_match(., 
                                                                        "Not seriously" ~ 0,
                                                                        "Somewhat seriously" ~ 1,
                                                                        "Very seriously" ~ 2),
                .names = "{.col}_num")) %>%
  mutate(ehf_hire_bin = (hire_benefits_emerg != "Not seriously"),
  ehf_hire_relative = 
    hire_benefits_emerg_num/rowSums(across(hire_benefits_pto_num:hire_benefits_tuition_num)), #what percentage of overall score
  ideology_answered = ifelse(ideology == "Haven’t thought much about this", NA, ideology),
  ideology_conlib = factor(ideology_answered, 
                           levels = 
                             c("Extremely liberal",
                               "Liberal",
                               "Moderate",
                               "Conservative",
                               "Extremely conservative"),
                           ordered = T),
  ideology_conlib_num = ifelse(is.na(ideology_conlib), NA,
    (as.numeric(ideology_conlib) - min(as.numeric(ideology_conlib), na.rm = T))/
    (max(as.numeric(ideology_conlib), na.rm = T)- min(as.numeric(ideology_conlib), na.rm = T))), 
  ideology = factor(ideology),
  vote_lik_fac = factor(vote_lik_post_elec),
  vote_lik_fix_flag = (vote_lik_post_elec %in% c("4", "5")),
  voted = (vote_lik_post_elec == "I'm sure I voted"),
  across(c(govt_responsib_elder:govt_responsib_hardship),
                ~factor(., levels = c(
                  "A lot of responsibility",
                  "Some responsibility",
                  "A little responsibility",
                  "No responsibility"
                ), ordered = T)),
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
                  "Advanced degree (JD, Masters, PhD, etc)")),
  ed_h = C(ordered(ed), contr.helmert),
  college = ed %in% c(
    "Bachelor's degree",
    "Master's degree/Advanced degree"),
  male = gender == "Male",
  nonwhite = ethn_race != "White",
  practice_religion = factor(practice_religion, levels =
                               c("At least once per week",
                                 "Once a week",
                                 "Once or twice a month",
                                 "A few times a year",
                                 "Never"),
                             ordered = T),
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
  )
  

