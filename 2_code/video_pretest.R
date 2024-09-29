## Analysis of video pretest
## Conducted on Prolific using Qualtrics, Sep 2024

library(here)
library(tidyverse)
library(readr)

vid <- read_csv(here("0_raw_data", "Pretesting", "EHF+Survey+-+Walmart+-+Pretest+-+Video_September+23,+2024_08.59", "EHF Survey - Walmart - Pretest - Video_September 23, 2024_08.59.csv"))

vid <- vid %>%
  rename(age = Q7, description= Q199, duration = `Duration (in seconds)`, RealVid = VidTreatment) %>%
  select(age, description, duration, RealVid) %>%
  filter(!row_number() %in% c(1, 2)) %>%
  mutate(age = as.numeric(age), duration = as.numeric(duration), RealVid = case_match(RealVid, "0" ~ "Home Depot", "1" ~ "Walmart"))

vid %>% group_by(RealVid) %>%
  summarize(mean_dur = mean(duration), mean_age = mean(age), sd_age = sd(age), sd_dur = sd(duration),
            median_dur = median(duration), median_age = median(age))

vid %>% group_by(RealVid) %>%
  summarize(mean_age = mean(age), sd_age = sd(age), median_age = median(age), min_age = min(age), max_age = max(age))

print(vid[order(vid$RealVid), ])
