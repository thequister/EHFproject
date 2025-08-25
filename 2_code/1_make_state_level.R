library(here)
source(here::here('2_code', '1_libraries_and_settings_global.R'))
here::i_am("2_code/1_make_state_level.R")

# Brookings Data
library(haven)

state_ui <- read_dta(here("0_raw_data", "UI", "sna_v1_1", "sna_styr_s_v2_1.dta")) %>%
  slice(-1) %>%
  mutate(state = fips(state,to='Name'))

# UI Weeks
ui_weeks <- read_csv(here("0_raw_data", "UI", "ui_weeks.txt")) %>%
  rename("max_ui_weeks" = "Maximum number of weeks of benefits available") %>%
  select(-`Unemployment (3-month average)`) %>%
  mutate(max_ui_weeks = as.numeric(str_sub(max_ui_weeks, 1, 2)))

state_ui <- left_join(state_ui, ui_weeks, by = c("state" = "State"))

# Replacement and Recipiency
recipiency <- read_csv(here("0_raw_data", "UI", "recipiency_2024.csv"))
recipiency <- rbind(recipiency, read_csv(here("0_raw_data", "UI", "recipiency_2021.csv")))


recipiency$state_name <- state.name[match(recipiency$State,state.abb)]
recipiency$state_name[recipiency$State == "DC"] <- "District of Columbia"
recipiency$state_name[recipiency$State == "PR"] <- "Puerto Rico"

recipiency <- select(recipiency, -State)

names(recipiency)[2] <- "recipiency_ui"

state_ui <- full_join(state_ui, recipiency, by = c("year" = "Year", "state" = "state_name"))

ui_files <- c("ui_replacement_1.csv", 
              "ui_replacement_2.csv", 
              "ui_replacement_3.csv", 
              "ui_replacement_4.csv")

replacement <- c()

for(i in 1:4){
  replacement <- rbind(replacement, read_csv(here("0_raw_data", "UI", ui_files[i])))
}

replacement$state_name <- state.name[match(replacement$State,state.abb)]
replacement$state_name[replacement$State == "DC"] <- "District of Columbia"

replacement <- replacement %>%
  select(-State) %>%
  mutate(`Average WBA` = as.double(str_sub(`Average WBA`, 2, ))) %>%
  rename("replacement_ui_1" = "Replacement Ratio 1", 
         "replacement_ui_2" = "Replacement Ratio 2",
         "avg_wba" = "Average WBA")

state_ui <- full_join(state_ui, replacement, by = c("year" = "Year", "state" = "state_name")) %>%
  filter(!is.na(state))

# TANF Generosity
tanf_gen <- read_excel(here("0_raw_data", "UI", "TANF Codebook and Data_updated July 25 2022.xlsx"), 
                       sheet = "Data")

state_ui <- full_join(state_ui, tanf_gen, by = c("state" = "State", "year"))

# Housing price index
hpi <- read_excel(here("1_secondary_data", "hpi_at_state.xlsx"), 
                  skip = 5) %>%
  group_by(State) %>%
  mutate(hpi_5year = `HPI with 2000 base` - lag(`HPI with 2000 base`, 5))

state_ui <- full_join(state_ui, hpi, by = c("state" = "State", "year" = "Year"))

# Remove unnecessary vars
state_ui <- state_ui %>%
  select(-Sample, -state_abbrev, -FIPS, -`Valid cases`, -state_fips, -Abbreviation)

write.csv(state_ui, here('3_cleaned_data', 'state_year_benefits.csv'))

