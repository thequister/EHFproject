library(here)
source(here::here('2_code', 'Walmart', '1_libraries_and_settings_ACNT.R'))
here::i_am("2_code/Walmart/3_follow_up_clean.R")

fup <- read_survey(here("0_raw_data", "ACNT", "ACNT_followup_qualtrics_raw_120125.csv"))
ACNT_clean <- read_csv(here("0_raw_data", "ACNT", "ACNT_clean_main.csv"))

# Remove preview obs, set treatment, remove unnecessary data, remove values not in clean set
fup <- fup %>%
  mutate(treatment_full = case_when(HDTreatment==0 ~ "vid0",
                                    HDTreatment==1 ~ "vidChar",
                                    HDTreatment==2 ~ "vidSolid", 
                                    HDTreatment==3 ~ "ctrl")) %>%
  filter(`Login ID` %in% ACNT_clean$`Login ID`) %>%
  filter(!is.na(HDTreatment)) %>%
  select(-StartDate:-Progress, -RecipientFirstName, -RecipientLastName, -ExternalReference:-UserLanguage, -HDTreatment)

# Make values equivalent to main survey
fup <- fup %>%
  mutate(birthyear_clean = case_match(birthyear_f, 
                                      1 ~ 2001, 
                                      63 ~ 1963,
                                      73 ~ 1973,
                                      79 ~ 1979, .default = birthyear_f),
         age_f = 2025 - birthyear_clean,
         male_f = gender_f == "Male",
         ed_f = factor(educ_f, levels = 
                         c("No degree or diploma earned",
                           "High school diploma/GED",
                           "Some college",
                           "Associate's degree",
                           "Bachelor's degree",
                           "Advanced degree (JD, Masters, PhD, etc)")),
         college_f = ed_f %in% c(
           "Bachelor's degree",
           "Advanced degree (JD, Masters, PhD, etc)"),
         emergency_expense_f = factor(expense_f, levels = 
                                      c("I am certain I could not come up with $400",
                                        "I could probably not come up with $400",
                                        "I could probably come up with $400",
                                        "I am certain I could come up with the full $400"),
                                    ordered= TRUE), 
         emergency_expense_num_f = 
           (as.numeric(emergency_expense_f) - min(as.numeric(emergency_expense_f)))/
           (max(as.numeric(emergency_expense_f))- min(as.numeric(emergency_expense_f))), 
         emergency_expense_bin_f = ifelse(  #binary variable; T if can cover expense 
           grepl(" not ", expense_f),
           FALSE,
           TRUE),
         new_job_f = fct_rev(factor(new_job_f, levels = 
                            c("Extremely likely",
                              "Very likely",
                              "Somewhat likely",
                              "Not very likely", 
                              "Not at all likely"),
                          ordered= TRUE)), 
         new_job_num_f = 
           (as.numeric(new_job_f) - min(as.numeric(new_job_f)))/
           (max(as.numeric(new_job_f))- min(as.numeric(new_job_f))), 
         new_job_bin_f = ifelse(  #binary variable; T if more than somewhat 
           grepl("Not ", new_job_f),
           FALSE,
           TRUE),
         wrk_loyal_f = fct_rev(factor(loyal_workers_f, levels = 
                              c("A lot of loyalty",
                                "Some loyalty",
                                "Only a little loyalty",
                                "No loyalty at all"),
                            ordered= TRUE)), 
         wrk_loyal_num_f = 
           (as.numeric(wrk_loyal_f) - min(as.numeric(wrk_loyal_f)))/
           (max(as.numeric(wrk_loyal_f))- min(as.numeric(wrk_loyal_f))), 
         wrk_loyal_bin_f = ifelse(  #binary variable; T if some/a lot 
           wrk_loyal_f %in% c("A lot of loyalty", "Some loyalty"),
           TRUE,
           FALSE),
         emp_loyal_f = factor(loyal_comp_f, levels = 
                              c("A lot of loyalty",
                                "Some loyalty",
                                "Only a little loyalty",
                                "No loyalty at all"),
                            ordered= TRUE), 
         emp_loyal_num_f = 
           (as.numeric(emp_loyal_f) - min(as.numeric(emp_loyal_f)))/
           (max(as.numeric(emp_loyal_f))- min(as.numeric(emp_loyal_f))), 
         emp_loyal_bin_f = ifelse(  #binary variable; T if some/a lot 
           emp_loyal_f %in% c("A lot of loyalty", "Some loyalty"),
           TRUE,
           FALSE),
         union_vote_f = fct_rev(factor(union_elec_f, levels = 
                               c("For the union",
                                 "Not sure", 
                                 "Against the union"),
                             ordered= TRUE)), 
         union_vote_bin_f = ifelse(  #binary variable; T if leans towards union 
           union_vote_f == "For the union",
           TRUE,
           FALSE),
         emp_reco_f = fct_rev(factor(recommend_f, levels = 
                             c("Certainly would recommend",
                               "Might recommend",
                               "Not sure",
                               "Might not recommend", 
                               "Definitely would not recommend"),
                           ordered= TRUE)), 
         emp_reco_num_f = 
           (as.numeric(emp_reco_f) - min(as.numeric(emp_reco_f)))/
           (max(as.numeric(emp_reco_f))- min(as.numeric(emp_reco_f))), 
         emp_reco_bin_f = ifelse(  #binary variable; T if leans recommending
           emp_reco_f %in% c("Certainly would recommend", "Might recommend"),
           TRUE,
           FALSE))

# Join onto main survey and save
fup_j <- left_join(ACNT_clean, fup, by = c("email" = "RecipientEmail")) 
# Joined by email because Login ID has non-unique values

# # ## Add comparisons to joined data
fup_j <- fup_j %>%
  mutate(age_match = age == age_f,
         ed_match = ed == ed_f,
         male_match = male == male_f,
         tot_match = (age_match + ed_match + male_match)/3)

# Filter out those with a match under 2/3 from the non-joined set
verified_emails <- na.rm(fup_j$email[fup_j$tot_match >= .5])

fup <- fup %>%
  filter(RecipientEmail %in% verified_emails)

fup_j <- left_join(ACNT_clean, fup, by = c("email" = "RecipientEmail")) 
 
# non_match <- fup_j %>%
#   filter(tot_match < 1) %>%
#   select(email, birthyear, birthyear_f, ed, ed_f, male, male_f, tot_match, quality)
# 
# write_csv(non_match, here("3_cleaned_data", "ACNT_fup_nonmatch.csv"))
# 
# # Generate emails
# email_list <- data.frame(email = c(unique(ACNT_clean$email)), nonmatch_flag = rep(0, length(unique(ACNT_clean$email))))
# email_list_fp <- data.frame(email = unique(fup$RecipientEmail), nonmatch_flag = ifelse(unique(fup$RecipientEmail) %in% non_match$email, 1, 0))
# email_list <- rbind(email_list, email_list_fp)
# 
# write_csv(email_list, here("3_cleaned_data", "ACNT_award_email_list.csv"))

# Save as clean follow-up
# write_csv(fup, here("3_cleaned_data", "ACNT_followup_clean_draft.csv"))

# Remove unnecessary columns
fup_j <- fup_j %>%
  select(-Finished.x:-ResponseId.x, -Finished.y:-ResponseId.y, -email, -`Login ID.x`, -`Login ID.y`, 
         -utm_source, -dupe_IPlatlong, -union_vote_agg, -ideology_answered, -treatment_full.y) %>%
  rename(treatment_full = "treatment_full.x")

write_csv(fup_j, here("3_cleaned_data", "ACNT_clean.csv"))
