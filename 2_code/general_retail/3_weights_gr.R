library(here)
source(here::here('2_code', 'general_retail', '1_libraries_and_settings_gr.R'))
here::i_am("2_code/general_retail/3_weights_gr.R")

gr_clean <- readxl::read_xlsx(here("3_cleaned_data", "general_retail_clean.xlsx"))
gr_acs <- read_excel("0_raw_data/general_retail/general_retail_ACS.xlsx", 
                     col_names = FALSE, skip = 11)

# Clean up ACS data
gr_acs <- gr_acs %>%
  slice(-3) %>%
  select(-1:-2)

gr_acs <- data_frame(total = as.vector(as.matrix(gr_acs)), male = rep(c(T, F), 8), 
           nonwhite = rep(c(F, F, F, F, T, T, T, T), 2), college = rep(c(F, F, T, T), 4), 
           age_bin_brief = c(rep("40-", 8), rep("40+", 8)))

# Create relevant age variable
gr_clean <- gr_clean %>%
  mutate(age_bin_brief = case_match(age_clean, 
                                    c(18:40) ~ "40-",
                                    c(40:110) ~ "40+"))

# Create table grouping observed values
gr_table <- gr_clean %>%
  group_by(age_bin_brief, nonwhite, college, male) %>%
  summarize(obs = n())

# Create table with acs weights
weight_table <- full_join(gr_acs, gr_table) %>%
  mutate(obs_prop = obs/sum(obs), acs_prop = total/sum(total)) %>%
  mutate(acs_weight = acs_prop/obs_prop) %>%
  select(-c(total, obs, obs_prop, acs_prop))

# Join to main survey
gr_clean <- left_join(gr_clean, weight_table)

# Save
write.xlsx(gr_clean, here("3_cleaned_data", "general_retail_clean.xlsx"))