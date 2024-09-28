## Analysis of walmart survey pretest
## Conducted on Prolific using Qualtrics, Sep 2024

library(here)
library(tidyverse)
library(readr)

pretest <- read_csv(here("0_raw_data", "Pretesting/EHF+Survey+-+Walmart+-+Pretest_September+23,+2024_10.23", "EHF Survey - Walmart - Pretest_September 23, 2024_10.23.csv"))


# Generalized treatment
pretest %>%
  rename(duration = `Duration (in seconds)`, treatment = HDTreatment) %>%
  select(duration, treatment, Q199, Q204, Q200) %>%
  filter(!row_number() %in% c(1, 2)) %>%
  mutate(treatment = case_match(treatment, c("0", "1", "2") ~ "Treatment", "3" ~ "Control"), 
         duration = as.numeric(duration)) %>%
  group_by(treatment) %>%
  summarize(mean_dur = mean(duration), median_dur = median(duration))

# Conditional treatment
pretest %>%
  rename(duration = `Duration (in seconds)`, treatment = HDTreatment) %>%
  select(duration, treatment, Q199, Q204, Q200) %>%
  filter(!row_number() %in% c(1, 2)) %>%
  mutate(treatment = case_match(treatment,"0" ~ "Plain Treatment", "1" ~ "Charity Treatment", "2" ~ "Solidarity Treatment",  "3" ~ "Control"), 
         duration = as.numeric(duration)) %>%
  group_by(treatment) %>%
  summarize(mean_dur = mean(duration), median_dur = median(duration))
