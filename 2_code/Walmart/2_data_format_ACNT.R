library(here)
source(here::here('2_code', 'Walmart', '1_libraries_and_settings_ACNT.R'))
here::i_am("2_code/Walmart/2_data_format_ACNT.R")

ACNT_uw <- read_csv(here("0_raw_data", "ACNT", "ACNT_clean.csv"))

bin_vars <- c("main_job", "manager", "ehf_received", "ehf_donation", 
              "unemp_benefits")

ACNT_uw <- ACNT_uw %>%
  mutate_at(bin_vars, ~case_match(., "Yes" ~ 1, "No" ~ 0)) %>%
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
  emergency_expense_bin = ifelse(  #binary expense variable; T if can cover expense 
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
  new_job_bin = ifelse(  #binary expense variable; T if more than somewhat 
    grepl("Not ", new_job),
    FALSE,
    TRUE),
  )
  

