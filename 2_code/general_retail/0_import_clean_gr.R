###
#created on: 12/30/24
#last updated: 12/30/24
#updated by: TNt
###
# Purpose: This code loads raw qualtrics data from the Winter 2025 General Retail survey
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
library(readxl)
library(naniar)
ggplot2::theme_set(ggplot2::theme_bw())
library(here)
here()

renv::restore()

qual_raw <- read_survey(here("0_raw_data", "general_retail", "general_retail_qualtrics_raw_250225.csv"))
# flagged_aid <- data.frame(aid = 0, reason = NA)

qual_dedupe <- qual_raw %>% #removing duplicate email addresses
  exclude_location(include_na = TRUE) %>% #excluding IP addresses outside the USA
  mark_duplicates(dupl_ip=T, dupl_location = F) %>% #mark duplicate IP addresses
  rename(IP_dupe = exclusion_duplicates) %>%
  mark_duplicates(dupl_ip=F, dupl_location = T) %>% #mark duplicate lat/long coords
  rename(Loc_dupe = exclusion_duplicates) %>%
  mutate(dupe_IPlatlong = ifelse(IP_dupe=="duplicated" & Loc_dupe=="duplicated",1,0)) %>%
  mutate(completion_subgroup = case_when(  # encoding levels of survey completion
    (retail_confirm != "Yes" | is.na(retail_confirm)) ~ 1,  # 1: Haven't worked for Walmart
    ((Progress <= 25 & is.na(health_ins)) | emp_relationship == "Exit survey") ~ 2, # 2: Quit before EHF treatment
    (Progress <= 45 & !is.na(health_ins) & is.na(expense)) ~ 3,  # 3: Quit before EHF DVs
    (Progress <= 45 & !is.na(expense) & is.na(recommend)) ~ 3.5, # 3.5: Quit during EHF DVs
    (Finished == F & Progress > 25 & !is.na(recommend)) ~ 4,  # 4: Quit after responding to ACNT outcome questions but before completing the survey
    (Finished == T & !is.na(income)) ~ 5
  )  
  ) %>%
  rename(duration = `Duration (in seconds)`)

# flagged_aid <- data.frame(aid = unique(qual_raw$aid)[!(unique(qual_raw$aid) %in% unique(qual_dedupe$aid))],
#                           reason = "IP outside US")

irreg_emp <- c("Crockpot burial", "your retail employer", "i want too proceed with it", "I worked at  retail store packing stuff.", "It was not retail.  It was a private contractor.",  
               "etc etc", "F-ur-life", "SALES COMPANY", "None", 
               "Walmart was all about making money for the managers and for corporate. The most of the employees were not treated good. I was told that it was a better place to work when Sam Walton was alive and ran the company.", 
               "I sell things online as a hobby on Mercari's Android app", "Completed")

gr <- qual_dedupe %>%
  mutate(qual_dedupe, age = 2025 - birthyear) %>%
  filter(completion_subgroup == 5) %>% # Take only completed surveys
  filter(age>17) %>% # Filter out minors
  mutate(quality = ifelse((age %in% c(18:85)), "high", "low")) %>% # set quality, sensible age responses + sensible pph responses
  select(-StartDate:-Progress, -RecipientLastName:-UserLanguage)

# flagged_aid <- rbind(flagged_aid, data.frame(aid = gr$aid[gr$duration > 1980 | gr$duration < 270], reason = "Irregular time to completion."))
# flagged_aid <- rbind(flagged_aid, data_frame(aid = gr$aid[gr$comp_name %in% irreg_emp], 
#            reason = "One or more non-sensical/disqualifying responses."))

gr <- gr %>% # correct age, duration, emp_name values
  filter(duration <= 1980 & duration >= 270) %>%
  filter(!(comp_name %in% irreg_emp)) %>%
  mutate(age_corrected = case_match(birthyear, 
                                    42 ~ 1,
                                    71 ~ 1,
                                    72 ~ 1,
                                    1002 ~ 1,
                                    1066 ~ 1,
                                    1083 ~ 1,
                                    .default = 0)) %>%
  mutate(age_clean = case_match(birthyear, 
                                42 ~ 83,
                                71 ~ 54,
                                72 ~ 53,
                                1002 ~ 23,
                                1066 ~ 59,
                                1083 ~ 42,
                                .default = age))

gr <- gr %>% # correct pph values
  mutate(pph_corrected = ifelse(pph >= 100, 1, 0)) %>%
  mutate(pph_clean = ifelse(pph >= 100, NA, pph))

gr <- gr %>%
  rename(ehf_offer_thd = "ehf_comp_offer_1", 
         ehf_offer_wal = "ehf_comp_offer_2", 
         ehf_offer_stb = "ehf_comp_offer_3", 
         ehf_offer_disn = "ehf_comp_offer_4", 
         ehf_offer_kohls = "ehf_comp_offer_5", 
         ehf_offer_costco = "ehf_comp_offer_6") %>%
  select(-Finished:-ResponseId)

accepted_aids <- gr$aid

#which(!(flagged_aid$aid %in% flagged_aid_old$aid))
# flagged_aid$round <- rep(1, nrow(flagged_aid))
# flagged_aid$round[c(34,  35,  36, 116, 117, 118, 119, 120, 121, 122, 123, 124)] <- 2
# 
# write.csv(flagged_aid, 
#           file = here("0_raw_data", "general_retail", "flagged_aid.csv"),
#           row.names = FALSE)  #dataset purged of all PII, irrelevant info
# 
# write.csv(accepted_aids,
#           file = here("0_raw_data", "general_retail", "accepted_aid.csv"),
#           row.names = FALSE)  #dataset purged of all PII, irrelevant info

# mean(gr$gender == "Man")
# c(mean(gr$age %in% c(18:37)), mean(gr$age %in% c(38:57)), mean(gr$age %in% c(58:100)))

## Add UI Data

# Replacement and Recipiency
recipiency_2024 <- read_csv(here("0_raw_data", "UI", "recipiency_2024.csv"))

recipiency_2024$state_name <- state.name[match(recipiency_2024$State,state.abb)]
recipiency_2024$state_name[recipiency_2024$State == "DC"] <- "District of Columbia"
recipiency_2024$state_name[recipiency_2024$State == "PR"] <- "Puerto Rico"

recipiency_2024 <- select(recipiency_2024, -Year, -State)

names(recipiency_2024)[1] <- "recipiency_2024_ui"

gr <- left_join(gr, recipiency_2024, by = c("worksite" = "state_name"))

ui_files <- c("ui_replacement_1.csv", 
             "ui_replacement_2.csv", 
             "ui_replacement_3.csv", 
             "ui_replacement_4.csv")

replacement <- c()

for(i in 1:4){
  replacement <- rbind(replacement, read_csv(here("0_raw_data", "UI", ui_files[i])) %>%
                         filter(Year == 2024) %>%
                         select(State, `Replacement Ratio 1`)) 
}

replacement$state_name <- state.name[match(replacement$State,state.abb)]
replacement$state_name[replacement$State == "DC"] <- "District of Columbia"

replacement <- select(replacement, -State) 

names(replacement)[1] <- "replacement_2024_ui"

gr <- left_join(gr, replacement, by = c("worksite" = "state_name"))

# TANF Generosity
tanf_gen <- read_excel(here("0_raw_data", "UI", "TANF Codebook and Data_updated July 25 2022.xlsx"), 
                                                          sheet = "Data") %>%
  filter(year == "2016") %>%
  select(State, WG_TANF)

gr <- left_join(gr, tanf_gen, by = c("residence" = "State"))

# Housing price index
hpi <- read_excel("1_secondary_data/hpi_at_state.xlsx", 
                           skip = 5) %>%
  select(State, Year, `HPI with 2000 base`) %>%
  filter(Year %in% c(2024, 2019)) %>%
  pivot_wider(values_from = `HPI with 2000 base`, names_from = Year,
              names_glue = "y_{Year}") %>%
  mutate(hpi_5year = y_2024 - y_2019) %>%
  select(State, hpi_5year)

gr <- left_join(gr, hpi, by = c("residence" = "State"))

write.csv(gr, 
          file = here("0_raw_data", "general_retail", "general_retail_purged.csv"),
          row.names = FALSE)  #dataset purged of all PII, irrelevant info
