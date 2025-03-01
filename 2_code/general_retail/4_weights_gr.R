library(here)
source(here::here('2_code', 'general_retail', '1_libraries_and_settings_gr.R'))
here::i_am("2_code/general_retail/4_weights_gr.R")

gr_clean <- readxl::read_xlsx(here("3_cleaned_data", "general_retail_clean.xlsx"))

gr_table <- gr_clean %>%
  mutate(age_bin = case_match(age_clean, 
                              c(18:37) ~ "18-37", 
                              c(38:57) ~ "38-57", 
                              c(58:77) ~ "58-77", 
                              c(78:110) ~ "78+")) %>%
  mutate(age_bin_brief = case_match(age_clean, 
                              c(18:40) ~ "40-",
                              c(40:110) ~ "40+")) %>%
  mutate(educ_bin = case_match(educ,
                               c("No degree or diploma earned") ~ "No HS",
                               c("High school diploma/GED", "Some college") ~ "HS", 
                               "Bachelor's degree" ~ "College", 
                               .default = "Postgrad")) %>%
  group_by(age_bin_brief, nonwhite, college, male) %>%
  summarize(obs = n())
