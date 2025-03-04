library(here)
source(here::here('2_code', 'Walmart', '1_libraries_and_settings_ACNT.R'))
here::i_am("2_code/Walmart/11_balance_check_WMT.R")

ACNT_clean <- read_csv(here("3_cleaned_data", "ACNT_clean.csv"))


demos <- ACNT_clean %>%
  select(multiple_jobs, fulltime, hourly, ehf_aware_pretr, 
         voted, college, male, nonwhite, practice_religion_bin, 
         identify_religion_bin, healthcare, treatment_full) %>%
  group_by(treatment_full) %>%
  summarize(across(multiple_jobs:healthcare, mean))

ACNT_clean %>%
  select(ideology_conlib, home_ownership, treatment_full) %>%
  mutate(n = 1) %>%
  pivot_wider(names_from = ideology_conlib, values_from = n)

# ideology_conlib,home_ownership, pph, 
#   mutate(manager = manager == "Yes", member_union = member_union == "Yes") %>%