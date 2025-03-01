library(here)
source(here::here('2_code', 'Walmart', '1_libraries_and_settings_ACNT.R'))
here::i_am("2_code/Walmart/9_balance_check_WMT.R")

ACNT_clean <- read_csv(here("3_cleaned_data", "ACNT_clean.csv"))

