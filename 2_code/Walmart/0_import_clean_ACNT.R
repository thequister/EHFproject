###
#created on: 12/30/24
#last updated: 12/30/24
#updated by: TNt
###
# Purpose: This code loads raw qualtrics data from the Fall 2024 Walmart survey
## it then does a first pass at cleaning the data and removing PII, creating treatment IDs,
## and generating the subsets of data to conform with those described in the 
## pre-analysis plan.  It then calculates raking weights
# Output:
## Walmart_clean.csv: raw dataset removing PII, respondents <18 y/o
## Walmart_completed.csv: dataset of all respondents >17 y/o who completed most/all of survey, PII removed and raking weights included 
###

library(MASS)
library(tidyverse)
library(codebook)
library(survey)
# devtools::install_github("pewresearch/pewmethods", build_vignettes = TRUE)
library(pewmethods)
library(qualtRics)
library(excluder)
library(naniar)
ggplot2::theme_set(ggplot2::theme_bw())
library(here)
here()

renv::restore()

qual_raw <- read_survey(here("0_raw_data", "ACNT", "ACNT_qualtrics_raw_191224.csv"))

qual_dedupe <- qual_raw[!duplicated(qual_raw$email, incomparables = NA), ] %>% #removing duplicate email addresses
  exclude_location(include_na = TRUE) %>% #excluding IP addresses outside the USA
  mark_duplicates(dupl_ip=T, dupl_location = F) %>% #mark duplicate IP addresses
  rename(IP_dupe = exclusion_duplicates) %>%
  mark_duplicates(dupl_ip=F, dupl_location = T) %>% #mark duplicate lat/long coords
  rename(Loc_dupe = exclusion_duplicates) %>%
  mutate(dupe_IPlatlong = ifelse(IP_dupe=="duplicated" & Loc_dupe=="duplicated",1,0)) %>%
  mutate(treatment_full = case_when(HDTreatment==0 ~ "vid0",
                                    HDTreatment==1 ~ "vidChar",
                                    HDTreatment==2 ~ "vidSolid", 
                                    HDTreatment==3 ~ "ctrl")) %>%
  mutate(treatment_vid = treatment_full!="ctrl") %>% #were the respondents shown a video
  mutate(treatment_vid_type = case_when(HDTreatment==0 ~ "vid0",
                                        HDTreatment==1 ~ "vidChar",
                                        HDTreatment==2 ~ "vidSolid", 
                                        HDTreatment==3 ~ NA)) %>%
  mutate(completion_subgroup = case_when(  # encoding levels of survey completion
    (past_work != "Yes" | is.na(past_work)) ~ 1,  # 1: Haven't worked for Walmart
    ((Progress <= 25 & is.na(health_ins)) | wal_relation == "Exit survey") ~ 2, # 2: Quit before EHF treatment
    (Progress <= 45 & !is.na(health_ins) & is.na(expense)) ~ 3,  # 3: Quit before EHF DVs
    (Progress <= 45 & !is.na(expense) & is.na(recommend)) ~ 3.5, # 3.5: Quit during EHF DVs
    (Finished == F & Progress > 25 & !is.na(recommend)) ~ 4,  # 4: Quit after responding to ACNT outcome questions but before completing the survey
    (Finished == T & is.na(email)) ~ 5,  # 5: completed w/o giving email
    (Finished == T & !is.na(email)) ~ 6  # 6: completed & gave email
  )  
  ) %>%
  rename(duration = `Duration (in seconds)`)

Walmart <- mutate(qual_dedupe, age = 2024 - birthyear) %>%
  filter(completion_subgroup %in% c(5, 6)) %>% # Take only completed surveys
  filter(age>17) %>% # Filter out minors
  mutate(quality = ifelse((age %in% c(18:80)) & attention == "A little of the time", "high", "low")) # set quality, sensible age responses + correct attention check
