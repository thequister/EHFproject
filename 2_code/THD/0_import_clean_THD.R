###
#created on:
#last updated: 8/14/25
#updated by: TNT
###
# Purpose: This code loads raw qualtrics data from the October 2021 THD survey
## it then does a first pass at cleaning the data and removing PII, creating treatment IDs,
## and generating the subsets of data to conform with those described in the 
## pre-analysis plan.  It then calculates raking weights
# Output:
## THD_clean.csv: raw dataset removing PII, respondents <18 y/o
## THD_completed.csv: dataset of all respondents >17 y/o who completed most/all of survey, PII removed and raking weights included 
###

library(MASS)
library(tidyverse)
library(codebook)
library(survey)
#devtools::install_github("pewresearch/pewmethods", build_vignettes = TRUE)
library(pewmethods)
library(qualtRics)
library(excluder)
library(naniar)
ggplot2::theme_set(ggplot2::theme_bw())
library(here)
library(readxl)
library(haven)
library(cdlTools)

here()

#setting up data
qual_raw <- read_survey(here("0_raw_data", "THD", "THD_qualtrics_raw_103121.csv"))
qual_dedupe <- qual_raw[!duplicated(qual_raw$Q7.1, incomparables = NA), ] %>% #removing duplicate email addresses
  exclude_location(include_na = TRUE) %>% #excluding IP addresses outside the USA
  mark_duplicates(dupl_ip=T, dupl_location = F) %>% #mark duplicate IP addresses
  rename(IP_dupe = exclusion_duplicates) %>%
  mark_duplicates(dupl_ip=F, dupl_location = T) %>% #mark duplicate lat/long coords
  rename(Loc_dupe = exclusion_duplicates) %>%
  mutate(dupe_IPlatlong = ifelse(IP_dupe=="duplicated" & Loc_dupe=="duplicated",1,0)) %>% #ID obs w/ duplicate IP & location
  mutate(HDTreatment = case_when(HDTreatment==0 ~ "cntrl",
                                 HDTreatment==1 ~ "txt",
                                 HDTreatment==2 ~ "vid")) %>%  #encoding EHF treatment indicator
  mutate(COVIDTreatment = case_when(COVIDTreatment==0 ~ "trump",
                                    COVIDTreatment==1 ~ "biden")) %>%  #encoding COVID policy treatment indicator
  mutate(completion_subgroup = case_when(  # encoding levels of survey completion
    (Q2.1 != "Yes" | is.na(Q2.1)) ~ 1,  # 1: Haven't worked for THD
    (Progress <= 25 & is.na(Q2.16)) ~ 2, # 2: Quit before EHF treatment
    (Progress <= 25 & !is.na(Q2.16) & is.na(Q3.4)) ~ 3,  # 3: Quit before EHF DVs
    (Progress > 25 & Finished == F & is.na(Q5.12)) ~ 4, # 4: Quit before COVID treatment",
    (Progress > 25 & Finished == F & (
      (COVIDTreatment== "trump" & is.na(Q5.17)) | 
        (COVIDTreatment== "biden" & is.na(Q5.18))
      )) ~ 5,                            # 5: Quit before COVID DVs
    (Finished == F & Progress > 75) ~ 5.5,  # 5.5: Got far but quit before end
    (Finished == T & is.na(Q7.1)) ~ 6,  # 6: completed w/o giving email
    (Finished == T & !is.na(Q7.1)) ~ 7  # 7: completed & gave email
    )  
    ) %>%
  rename(duration = `Duration (in seconds)`)
to_numeric <- c("Q2.14", "Q3.3_First Click", "Q3.3_Last Click", "Q3.3_Page Submit", 
               "Q3.3_Click Count", "Q3.20", "Q3.25", "Q6.4")
for (var_name in to_numeric) {
  qual_dedupe[, var_name] <- as.numeric(getElement(qual_dedupe, var_name))
}

THD <- mutate(qual_dedupe, age = 2021 - Q6.4) %>% 
  filter(age>17 | is.na(age))  %>% #removing those who report age less than 18
  select(-StartDate:-Progress, -Finished:-UserLanguage, -Q7.1:-MaxGrantAmt) # removing PII and useless info
write.csv(THD, 
          file = here("0_raw_data", "THD", "THD_clean.csv"),
          row.names = FALSE)  #dataset purged of all PII, irrelevant info 

# creating raking weights for ~completed surveys
THD$rk_age <- THD$age  #creating age variable for use in survey weight calcs
THD$rk_age[THD$age >99]<-NA #Removing erroneous age values
THD$rk_gender <- NA   #creating gender variable for use in survey weight calcs
THD$rk_gender[THD$Q6.2=="Male"]<-0
THD$rk_gender[THD$Q6.2=="Female"]<-1

# imputing missing age and gender variables for weight calcs
THD_impute<-impute_vars(THD[THD$completion_subgroup>5,], 
                        to_impute = c("rk_age", "rk_gender", "Q4.2", 
                                      "Q4.6", "Q2.10", "duration", "Q4.7",
                                      "Q5.5"),
                        seed = 76)

THD_complete <- mutate(THD_impute, 
              rk_age_cat = cut(rk_age, 
                            breaks=c(-Inf, 17, 29, 39, 49, 55, Inf),
                            labels=c("<18","18-29","30-39","40-49","50-55","56-65+")
                            )
              ) %>%
  mutate(rk_gender = case_when(
    rk_gender==0 ~ "male",
    rk_gender==1 ~ "female")) %>%
  mutate(rk_gender_age = interaction(rk_gender, rk_age_cat, sep = ":")) %>%
  droplevels()

## target distribution from to FB marginals 
fb_thd_dems <- read_csv(here("0_raw_data", "THD", "THD_audience.csv")) %>%
  mutate(targeting = NULL, location = NULL)
thd_tr = sum(fb_thd_dems$potential_reach)
fb_thd_dems <- mutate(fb_thd_dems, 
                      Freq = potential_reach/thd_tr*100,
                      rk_gender_age = as.factor(paste(gender,age, sep=":")))
rk_targets<-list(tibble(rk_gender_age = fb_thd_dems$rk_gender_age, 
                        Freq = fb_thd_dems$Freq))

THD_complete <- THD_complete %>%
  mutate(rk_wgt = rake_survey(THD_complete, pop_margins = rk_targets))

## Add UI Data

state_ui <- read_csv(here('3_cleaned_data', 'state_year_benefits.csv'))

# Brookings Data
brookings <- state_ui %>%
  filter(year == "2021") %>%
  select(state, cashfood, cashfood_rs, st_directed)

THD_complete <- left_join(THD_complete, brookings, by = c("Q2.6" = "state"))

# UI Weeks
ui_weeks <- state_ui %>%
  filter(year == 2021) %>%
  select(state, max_ui_weeks)

THD_complete <- left_join(THD_complete, ui_weeks, by = c("Q2.5" = "state"))

# Replacement, Recipiency, hpi
rep <- state_ui %>%
  filter(year == 2021) %>%
  select(hpi_5year, replacement_ui_1, replacement_ui_2, recipiency_ui, avg_wba, state)

THD_complete <- left_join(THD_complete, rep, by = c("Q2.5" = "state"))

# TANF Generosity
tanf_gen <- state_ui %>%
  filter(year == 2016) %>%
  select(state, WG_TANF, WG_TANF_Benefit)

THD_complete <- left_join(THD_complete, tanf_gen, by = c("Q2.6" = "state"))

write.csv(THD_complete,   # dataset of survey (near-)completers with raking weights  
          file = here("0_raw_data", "THD", "THD_completed.csv"),
          row.names = FALSE)
