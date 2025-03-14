library(here)
source(here::here('2_code', 'Walmart', '1_libraries_and_settings_ACNT.R'))
here::i_am("2_code/Walmart/11_balance_check_WMT.R")

ACNT_clean <- read_csv(here("3_cleaned_data", "ACNT_clean.csv"))

# Create initial balance table
demos <- ACNT_clean %>%
  select(multiple_jobs, fulltime, ehf_aware_pretr, 
         voted, college, male, nonwhite, practice_religion_bin, 
         identify_religion_bin, healthcare, ideology_conlib, home_ownership, pph, manager, member_union, treatment_full) %>%
  mutate(across(c(multiple_jobs:home_ownership, manager, member_union), factor)) %>%
  mutate(ideology_conlib = fct_rev(factor(ideology_conlib, 
                                          levels = 
                                            c("Extremely liberal",
                                              "Liberal",
                                              "Moderate",
                                              "Conservative",
                                              "Extremely conservative"),
                                          ordered = T)))
# please look at cobalt::bal.tab( )


summary(estimatr::lm_robust(treatment_full == "vid0"  ~., data=demos))
summary(estimatr::lm_robust(treatment_full == "vidChar"  ~., data=demos))
summary(estimatr::lm_robust(treatment_full == "vidSolid"  ~., data=demos))

datasummary_balance(~ treatment_full, demos, output = here("4_output", "ACNT_balance_tab.html"))

# demos <- ACNT_clean %>%
#   select(multiple_jobs, hourly, fulltime, ehf_aware_pretr, 
#          voted, college, male, nonwhite, practice_religion_bin, 
#          identify_religion_bin, healthcare, home_ownership, manager, member_union, treatment_full) %>%
#   na.omit()
# 
# sparse <- polr(as.factor(treatment_full) ~ 1, demos)
# full <- polr(as.factor(treatment_full) ~ ., demos)
# 
# summary(full)
# 
# anova(full, sparse)
# 
# a <- stepAIC(full)
