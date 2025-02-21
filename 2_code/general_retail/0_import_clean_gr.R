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
library(naniar)
ggplot2::theme_set(ggplot2::theme_bw())
library(here)
here()

renv::restore()

qual_raw <- read_survey(here("0_raw_data", "general_retail", "general_retail_qualtrics_raw_210225.csv"))
flagged_aid <- data.frame(aid = 0, reason = NA)

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

# accepted_aids <- gr$aid

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

write.csv(gr, 
          file = here("0_raw_data", "general_retail", "general_retail_purged.csv"),
          row.names = FALSE)  #dataset purged of all PII, irrelevant info

# # creating raking weights for ~completed surveys
# Walmart$rk_age <- Walmart$age  #creating age variable for use in survey weight calcs
# Walmart$rk_age[Walmart$age >80]<-NA #Removing erroneous age values
# 
# Walmart$rk_gender <- NA   #creating gender variable for use in survey weight calcs
# Walmart$rk_gender[Walmart$gender=="Man"]<-1
# Walmart$rk_gender[Walmart$gender!="Man"]<-0
# 
# Walmart$rk_educ <- case_when(Walmart$educ %in% c("Associate's degree", "Advanced degree (JD, Masters, PhD, etc)", "Bachelor's degree", "Some college") ~ 1, 
#                               Walmart$educ %in% c("No degree or diploma earned", "High school diploma/GED") ~ 0)
# 
# # imputing missing age and gender variables for weight calcs
# Walmart_complete <- mutate(Walmart[Walmart$completion_subgroup>=5,], 
#                       rk_age_dei = cut(rk_age, 
#                                                    breaks=c(-Inf, 24, 34, 44, 54, 64, Inf),
#                                                    labels=c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")),
#                        rk_age = cut(rk_age, 
#                                         breaks=c(-Inf, 45, Inf),
#                                         labels=c("18-45","45-65+")))
#                        
# Walmart_complete$rk_age[is.na(Walmart_complete$rk_age)] <- "18-45"
# Walmart_complete$rk_age_dei[is.na(Walmart_complete$rk_age_dei)] <- "35-44"
#                        
# Walmart_complete <- Walmart_complete %>%
#   mutate(
#     rk_gender = case_when(
#     rk_gender==1 ~ "male",
#     rk_gender==0 ~ "female")) %>%
#   mutate(rk_educ = case_when(
#     rk_educ==0 ~ "NC",
#     rk_educ==1 ~ "C")) %>%
#   mutate(rk_gender_age = interaction(rk_gender, rk_age, sep = ":")) %>%
#   mutate(rk_gender_educ = interaction(rk_gender, rk_educ, sep = ":")) %>%
#   mutate(rk_age_educ = interaction(rk_age, rk_educ, sep = ":")) %>%
#   mutate(across(c(rk_gender:rk_age_educ), factor)) %>%
#   droplevels()
# 
# ## Raking from FB audience
# fb_Walmart_dems <- read_csv(here("0_raw_data", "ACNT", "Walmart_audience.csv")) %>%
#   mutate(targeting = NULL, location = NULL, min_audience = NULL) %>%
#   rename(educ = "education") %>%
#   mutate(across(c(educ, age, gender), factor))
# 
# # Walmart_tr = sum(fb_Walmart_dems$max_audience)
# # fb_Walmart_dems <- mutate(fb_Walmart_dems, 
# #                       Freq = max_audience/Walmart_tr*100,
# #                       rk_gender_age = as.factor(paste(gender,age, sep=":")),
# #                       rk_gender_educ = as.factor(paste(gender,educ, sep=":")),
# #                       rk_age_educ = as.factor(paste(age,educ, sep=":")))
# # 
# # rk_targets<-list(tibble(rk_gender_age = fb_Walmart_dems$rk_gender_age,
# #                         rk_gender_educ = fb_Walmart_dems$rk_gender_educ,
# #                         rk_age_educ = fb_Walmart_dems$rk_age_educ,
# #                         Freq = fb_Walmart_dems$Freq))
# 
# rk_targets <- create_raking_targets(fb_Walmart_dems, 
#                                     vars = c("educ", "age", "gender", "gender:age", "gender:educ", "age:educ"),
#                                     wt = "max_audience")
# 
# Walmart_complete <- Walmart_complete %>%
#   mutate(rk_wgt_og = rake_survey(Walmart_complete, pop_margins = rk_targets)) %>% # original produced weights
#   mutate(rk_wgt_trim = case_when(rk_wgt_og <= 0.5 ~ 0.5, 
#                                  rk_wgt_og >= 2 ~ 2, 
#                                  .default = rk_wgt_og)) # Weights trimmed according to PAP
# 
# ## Raking from Walmart DEI file
# 
# rk_targets <- list(tibble(rk_gender = factor(c("female", "male")), Freq = c(47.59, 52.41)), 
#                    tibble(rk_age_dei = factor(c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")),
#                           Freq = c(28.89, 20, 16.22, 14.31, 14, 6.54)))
# 
# Walmart_complete <- Walmart_complete %>%
#   mutate(rk_dei_og = rake_survey(Walmart_complete, pop_margins = rk_targets)) %>% # original produced weights
#   mutate(rk_dei_trim = case_when(rk_dei_og <= 0.5 ~ 0.5, 
#                                  rk_dei_og >= 2 ~ 2, 
#                                  .default = rk_dei_og)) # Weights trimmed according to PAP
# 
# write.csv(Walmart_complete,   # dataset of survey (near-)completers with raking weights  
#           file = here("0_raw_data", "ACNT", "ACNT_full.csv"),
#           row.names = FALSE)
# 
# # # Summary statistics plots and numbers
# # mean(qual_dedupe$duration)
# # median(qual_dedupe$duration)
# # sd(qual_dedupe$duration)
# # 
# # ggplot(qual_dedupe) +
# #   geom_boxplot(aes(x = as.factor(completion_subgroup), y = duration), outlier.shape = NA) +
# #   coord_cartesian(xlim =NULL, ylim = c(0, 2500), expand = TRUE, default = FALSE, clip = "on")
# # 
# # sum(qual_dedupe$completion_subgroup %in% c(5, 6))
# # 
# # ggplot(Walmart) +
# #   geom_bar(aes(x = quality, fill = treatment_full))
# # 
# # ggplot(Walmart) +
# #   geom_boxplot(aes(x = quality, y = duration), outlier.shape = NA) +
# #   coord_cartesian(xlim =NULL, ylim = c(0, 2500), expand = TRUE, default = FALSE, clip = "on")
# # 
# # ggplot(Walmart) +
# #   geom_boxplot(aes(x = as.factor(treatment_full), y = duration), outlier.shape = NA) +
# #   coord_cartesian(xlim =NULL, ylim = c(0, 2500), expand = TRUE, default = FALSE, clip = "on")
# 
# # ggplot(Walmart_complete, aes(x = rk_wgt_trim)) +
# #   geom_histogram(binwidth = 0.05)
# # 
# # wgt_og_df <- unique(data.frame(Walmart_complete$rk_age, Walmart_complete$rk_educ, Walmart_complete$rk_gender, Walmart_complete$rk_wgt_og))
# # 
# # 
# # ggplot(Walmart_complete, aes(x = rk_dei_trim)) +
# #   geom_histogram(binwidth = 0.05)
# # 
# # wgt_og_df <- unique(data.frame(Walmart_complete$rk_age_dei, Walmart_complete$rk_gender, Walmart_complete$rk_dei_og))
