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
  mutate(quality = ifelse((age %in% c(18:80)) & attention == "A little of the time", "high", "low")) %>% # set quality, sensible age responses + correct attention check
  select(-StartDate:-Progress, -RecipientLastName:-UserLanguage, -`Q182_First Click`:-`Q182_Click Count`,
         -EmployerName, -EHFName, -charity_treat:-EHFNameAbbr, -ExtendText, -ExtendTextFinal)

write.csv(Walmart, 
          file = here("0_raw_data", "ACNT", "ACNT_clean.csv"),
          row.names = FALSE)  #dataset purged of all PII, irrelevant info

# creating raking weights for ~completed surveys
Walmart$rk_age <- Walmart$age  #creating age variable for use in survey weight calcs
Walmart$rk_age[Walmart$age >80]<-NA #Removing erroneous age values

Walmart$rk_gender <- NA   #creating gender variable for use in survey weight calcs
Walmart$rk_gender[Walmart$gender=="Man"]<-0
Walmart$rk_gender[Walmart$gender=="Woman"]<-1

Walmart$rk_educ <- case_when(Walmart$educ %in% c("Associate's degree", "Advanced degree (JD, Masters, PhD, etc)", "Bachelor's degree", "Some college") ~ 1, 
                              Walmart$educ %in% c("No degree or diploma earned", "High school diploma/GED") ~ 0)

# imputing missing age and gender variables for weight calcs
Walmart_impute<-impute_vars(Walmart[Walmart$completion_subgroup>=5,], 
                        to_impute = c("rk_age", "rk_gender"),
                        seed = 76)

Walmart_complete <- mutate(Walmart_impute, 
                       rk_age_cat = cut(rk_age, 
                                        breaks=c(-Inf, 45, Inf),
                                        labels=c("18-45","45-65+")
                       )
) %>%
  mutate(rk_gender = case_when(
    rk_gender==0 ~ "male",
    rk_gender==1 ~ "female")) %>%
  mutate(rk_educ = case_when(
    rk_educ==0 ~ "NC",
    rk_educ==1 ~ "C")) %>%
  mutate(rk_gender_age_educ = interaction(rk_gender, rk_age_cat, rk_educ, sep = ":")) %>%
  droplevels()

## target distribution from to FB marginals 
fb_Walmart_dems <- read_csv(here("0_raw_data", "ACNT", "Walmart_audience.csv")) %>%
  mutate(targeting = NULL, location = NULL, min_audience = NULL) %>%
  rename(educ = "education")
Walmart_tr = sum(fb_Walmart_dems$max_audience)
fb_Walmart_dems <- mutate(fb_Walmart_dems, 
                      Freq = max_audience/Walmart_tr*100,
                      rk_gender_age_educ = as.factor(paste(gender,age,educ, sep=":")))
rk_targets<-list(tibble(rk_gender_age_educ = fb_Walmart_dems$rk_gender_age_educ, 
                        Freq = fb_Walmart_dems$Freq))

Walmart_complete <- Walmart_complete %>%
  mutate(rk_wgt_og = rake_survey(Walmart_complete, pop_margins = rk_targets)) %>% # original produced weights
  mutate(rk_wgt_trim = case_when(rk_wgt_og <= 0.5 ~ 0.5, 
                                 rk_wgt_og >= 2 ~ 2, 
                                 .default = rk_wgt_og)) # Weights trimmed according to PAP

write.csv(Walmart_complete,   # dataset of survey (near-)completers with raking weights  
          file = here("0_raw_data", "ACNT", "THD_completed.csv"),
          row.names = FALSE)

# Summary statistics plots and numbers
mean(qual_dedupe$duration)
median(qual_dedupe$duration)
sd(qual_dedupe$duration)

ggplot(qual_dedupe) +
  geom_boxplot(aes(x = as.factor(completion_subgroup), y = duration), outlier.shape = NA) +
  coord_cartesian(xlim =NULL, ylim = c(0, 2500), expand = TRUE, default = FALSE, clip = "on")

sum(qual_dedupe$completion_subgroup %in% c(5, 6))

ggplot(Walmart) +
  geom_bar(aes(x = quality, fill = treatment_full))

ggplot(Walmart) +
  geom_boxplot(aes(x = quality, y = duration), outlier.shape = NA) +
  coord_cartesian(xlim =NULL, ylim = c(0, 2500), expand = TRUE, default = FALSE, clip = "on")

ggplot(Walmart) +
  geom_boxplot(aes(x = as.factor(treatment_full), y = duration), outlier.shape = NA) +
  coord_cartesian(xlim =NULL, ylim = c(0, 2500), expand = TRUE, default = FALSE, clip = "on")
