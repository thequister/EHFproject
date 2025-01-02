#library(here)
#source(here::here('2_code', 'Walmart', '1_libraries_and_settings_ACNT.R'))
#here::i_am("2_code/Walmart/2_data_format_ACNT.R")

ACNT_uw <- read_csv(here("0_raw_data", "ACNT", "ACNT_clean.csv"))

ACNT_uw <- ACNT_uw %>%
  mutate()