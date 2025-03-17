###
#created on: 12/30/24
#last updated: 03/16/25
#updated by: TNt
###
# Purpose: This code outputs a balance table for the ACNT survey.
# Output:
## balance_tab_ACNT: kableExtra object to be used by other code
## ACNT_balance_tab.html: html version of the above table
###

library(here)
source(here::here('2_code', 'Walmart', '1_libraries_and_settings_ACNT.R'))
here::i_am("2_code/Walmart/11_balance_check_WMT.R")

ACNT_clean <- read_csv(here("3_cleaned_data", "ACNT_clean.csv"))

# Create initial balance table
ACNT_clean <- ACNT_clean %>%
  mutate(ideology_conlib = fct_rev(factor(ideology_conlib, 
                                          levels = 
                                            c("Extremely liberal",
                                              "Liberal",
                                              "Moderate",
                                              "Conservative",
                                              "Extremely conservative"),
                                          ordered = T)))

### Balance Table -------------------------------------------------------------

balCovs <- subset(ACNT_clean, select = c(multiple_jobs, fulltime, ehf_aware_pretr, 
                                         voted, college, male, nonwhite, practice_religion_bin, 
                                         identify_religion_bin, healthcare, home_ownership, manager, member_union, pph, ideology_conlib)) %>%
  mutate(ideology_conlib = as.numeric(fct_rev(factor(ideology_conlib, 
                                          levels = 
                                            c("Extremely liberal",
                                              "Liberal",
                                              "Moderate",
                                              "Conservative",
                                              "Extremely conservative"),
                                          ordered = T)))) %>%
  mutate(manager = as.numeric(manager == "Yes"),
         home_ownership = as.numeric(home_ownership == "Own"),
         member_union = as.numeric(member_union == "Yes")) %>%
  mutate(across(multiple_jobs:member_union, ~ as.numeric(.x)))


balCovsBin <- subset(ACNT_clean, select = c(multiple_jobs, fulltime, ehf_aware_pretr, 
                                     voted, college, male, nonwhite, practice_religion_bin, 
                                     identify_religion_bin, healthcare, home_ownership, manager, member_union)) %>%
  mutate(manager = as.numeric(manager == "Yes"),
         home_ownership = as.numeric(home_ownership == "Own"),
         member_union = as.numeric(member_union == "Yes")) %>%
  mutate(across(multiple_jobs:member_union, ~ as.numeric(.x)))

tab <- cobalt::bal.tab(balCovs, treat = ACNT_clean$treatment_bin, 
               stats = c("mean.diffs", "ks.statistics"))

ind <- (ACNT_clean$treatment_bin == 1)

Bin_t <- apply(balCovsBin, 2, function(a) 
{return(t.test(a[ind], a[-ind])$p.value)})

Bin_t <- round(append(Bin_t, t.test(ACNT_clean$pph[ind], ACNT_clean$pph[-ind])$p.value), digits = 4)

Bin_t <- round(append(Bin_t, t.test(as.numeric(ACNT_clean$ideology_conlib[ind]), as.numeric(ACNT_clean$ideology_conlib[-ind]))$p.value), digits = 4)

# Variable names
varnames <- c("Multiple Jobs", "Fulltime", "EHF Pre-treat Awareness", "Voted24", "College", 
              "Male", "Nonwhite", "Practice Religion", "Identify with Religion", "Healthcare", 
              "Home Owner", "Manager", "Union Member", "Pay per hour", "Ideology (Numeric)")

balance.data <- data.frame(varnames, 
                           tab$Balance[1][c(1:12, 14, 16, 18), ], 
                           round(tab$Balance[2][c(1:12, 14, 16, 18), ], digits = 4), 
                           round(tab$Balance[3][c(1:12, 14, 16, 18), ], digits = 4), 
                           Bin_t)

names(balance.data) <- c("Variables", "Type", "Standardized Mean Difference", "KS Test Statistic",
                         "T-Test P-Value")

balance_tab_ACNT <- kableExtra::kable(balance.data, format.args = list(big.mark = ","),
                  digits = 4,
                  align = c("l", "l", rep("r", 3)),
                  caption = "ACNT Balance Table") %>%
  kableExtra::kable_styling(full_width = F, position = "center") %>%
  kableExtra::row_spec(0, bold = T)

cat(knitr::kable(balance_tab_ACNT, format = 'markdown'), file = "4_output/ACNT_balance_tab.md")

kableExtra::save_kable(balance_tab_ACNT, file = "4_output/ACNT_balance_tab", self_contained = T)

