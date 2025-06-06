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

#library(here)
#source(here::here('2_code', '1_libraries_and_settings_global.R'))
#here::i_am("2_code/Walmart/11_balance_check_WMT.R")

#ACNT_clean <- read_csv(here("3_cleaned_data", "ACNT_clean.csv"))

ACNT_clean <- wmt.hq

### Balance Table -------------------------------------------------------------

balCovs <- subset(ACNT_clean, 
  select = c(multiple_jobs, fulltime, ehf_aware_pretr, 
    voted, college, male, nonwhite, practice_religion_bin, 
    identify_religion_bin, healthcare, home_ownership, manager, 
    member_union, pph, ideology_conlib, tenure_num)) %>%
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


balCovsBin <- subset(ACNT_clean, 
  select = c(multiple_jobs, fulltime, ehf_aware_pretr, 
    voted, college, male, nonwhite, practice_religion_bin, 
    identify_religion_bin, healthcare, home_ownership, 
    manager, member_union)) %>%
  mutate(manager = as.numeric(manager == "Yes"),
         home_ownership = as.numeric(home_ownership == "Own"),
         member_union = as.numeric(member_union == "Yes")) %>%
  mutate(across(multiple_jobs:member_union, ~ as.numeric(.x)))

tab <- cobalt::bal.tab(balCovs, treat = ACNT_clean$treatment_bin, 
               stats = c("mean.diffs", "ks.statistics"), continuous = "std", binary = "std")

# ind <- (ACNT_clean$treatment_bin == 1)

# Bin_t <- apply(balCovsBin, 2, function(a) 
# {return(t.test(a[ind], a[-ind])$p.value)})

# Bin_t <- round(append(Bin_t, t.test(ACNT_clean$pph[ind], ACNT_clean$pph[-ind])$p.value), digits = 3)

# Bin_t <- round(append(Bin_t, t.test(as.numeric(ACNT_clean$ideology_conlib[ind]), as.numeric(ACNT_clean$ideology_conlib[-ind]))$p.value), digits = 3)


# Bin_t <- round(append(Bin_t, t.test(as.numeric(ACNT_clean$tenure_num[ind]), as.numeric(ACNT_clean$tenure_num[-ind]))$p.value), digits = 3)


# # Variable names
# varnames <- c("Multiple Jobs", "Fulltime", "EHF Pre-treat Awareness", 
#   "Voted24", "College", "Male", "Nonwhite", "Practice Religion", 
#   "Identify with Religion", "Healthcare", "Home Owner", "Manager", 
#   "Union Member", "Pay per hour", "Ideology (Numeric)", "Tenure")

# balance.data <- data.frame(varnames, 
#                            tab$Balance[1][c(1:12, 14, 16, 18, 20), ], 
#                            round(tab$Balance[2][c(1:12, 14, 16, 18, 20), ], digits = 3), 
#                            round(tab$Balance[3][c(1:12, 14, 16, 18, 20), ], digits = 3), 
#                            Bin_t)

# names(balance.data) <- c("Variables", "Type", "Standardized Mean Difference", "KS Test Statistic",
#                          "T-Test P-Value")

# balance_tab_ACNT <- kableExtra::kable(balance.data, format.args = list(big.mark = ","),
#                   digits = 4,
#                   align = c("l", "l", rep("r", 3)),
#                   caption = "ACNT Balance Table") %>%
#   kableExtra::kable_styling(full_width = F, position = "center") %>%
#   kableExtra::row_spec(0, bold = T)

# cat(knitr::kable(balance_tab_ACNT, format = 'markdown'), file = "4_output/ACNT_balance_tab.md")

# kableExtra::save_kable(balance_tab_ACNT, file = "4_output/ACNT_balance_tab", self_contained = T)

### Descriptives Table

# Unweighted
tbl_sum_WMT_1<- 
  ACNT_clean |> 
  select(treatment_full, ehf_aware_pretr, rk_age, male, main_job, 
         tenure_fac, nonwhite, fulltime, hourly, college, 
         ehf_prior_know, ehf_applied,
         ehf_received, ehf_donation) |> 
  gtsummary::tbl_summary(by = treatment_full,
                         missing_text = "(missing/not gathered)",
                         label = list( 
                           ehf_aware_pretr ~ "EHF awareness (pre-treatment)", 
                           rk_age ~ "age", male ~ "male",
                           main_job ~ "main job", tenure_fac ~ "job tenure",
                           nonwhite ~ "PoC/nonwhite", fulltime ~ "full time",
                           hourly ~ "hourly", college ~ "college degree",
                           ehf_prior_know ~ "EHF awareness (post-treatment)", 
                           ehf_applied ~"applied to EHF", 
                           ehf_received ~ "received EHF grant", 
                           ehf_donation ~"donated to EHF"
                         )
  ) |>  
  gtsummary::modify_caption("**Sample summary statistics by treatment group**") |> 
  gtsummary::bold_labels() |> 
  gtsummary::as_kable_extra()

tbl_sum_WMT_1


tbl_sum_WMT_2<- 
  ACNT_clean |> 
  select(treatment_full, wrk_loyal, emp_loyal, emp_reco, 
         union_vote, govt_responsib_hardship, govt_responsib_elder, govt_responsib_unemp) |>
  gtsummary::tbl_summary(by = treatment_full,
                         missing_text = "(missing/not gathered)",
                         label = list(
                           wrk_loyal ~"loyalty to coworkers", emp_loyal~"loyalty to employer",
                           emp_reco~"recommend employer", union_vote ~"union vote", 
                           govt_responsib_hardship ~"hardship support", govt_responsib_elder~"pension support", govt_responsib_unemp~"UI support"
                         )
  ) |> 
  gtsummary::modify_caption("*summary table (cont'd)*") |> 
  gtsummary::bold_labels() |> 
  gtsummary::as_kable_extra()

tbl_sum_WMT_2


# Weighted
ACNT_clean_w <- ACNT_clean %>%
  srvyr::as_survey_design(ids = 1, weights = rk_wgt_trim)

tbl_sum_WMT_1w<- 
  ACNT_clean_w |> 
  select(treatment_full, ehf_aware_pretr, rk_age, male, main_job, 
         tenure_fac, nonwhite, fulltime, hourly, college, ehf_prior_know, ehf_applied,
         ehf_received, ehf_donation) |> 
  gtsummary::tbl_svysummary(by = treatment_full,
                         missing_text = "(missing/not gathered)",
                         label = list( 
                           ehf_aware_pretr ~ "EHF awareness (pre-treatment)", 
                           rk_age ~ "age", male ~ "male",
                           main_job ~ "main job", tenure_fac ~ "job tenure",
                           nonwhite ~ "PoC/nonwhite", fulltime ~ "full time",
                           hourly ~ "hourly", college ~ "college degree",
                           ehf_prior_know ~ "EHF awareness (post-treatment)", ehf_applied ~"applied to EHF", 
                           ehf_received ~ "received EHF grant", ehf_donation ~"donated to EHF"
                         )
  ) |>  
  gtsummary::modify_caption("**Sample summary statistics by treatment group**") |> 
  gtsummary::bold_labels() |> 
  gtsummary::as_kable_extra()

tbl_sum_WMT_1w


tbl_sum_WMT_2w<- 
  ACNT_clean_w |> 
  select(treatment_full, wrk_loyal, emp_loyal, emp_reco, 
         union_vote, govt_responsib_hardship, govt_responsib_elder, govt_responsib_unemp) |>
  gtsummary::tbl_svysummary(by = treatment_full,
                         missing_text = "(missing/not gathered)",
                         label = list(
                           wrk_loyal ~"loyalty to coworkers", emp_loyal~"loyalty to employer",
                           emp_reco~"recommend employer", union_vote ~"union vote", 
                           govt_responsib_hardship ~"hardship support", govt_responsib_elder~"pension support", govt_responsib_unemp~"UI support"
                         )
  ) |> 
  gtsummary::modify_caption("*summary table (cont'd)*") |> 
  gtsummary::bold_labels() |> 
  gtsummary::as_kable_extra()

tbl_sum_WMT_2w

