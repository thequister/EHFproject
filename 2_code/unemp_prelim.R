library(here)
source(here::here('2_code', '1_libraries_and_settings_global.R'))

ACNT <- read_csv(here("3_cleaned_data", "ACNT_clean.csv")) %>%
  select(unemp_benefits, other_welfare, ehf_aware_pretr, rk_wgt_trim, 
         registered, ideology, party, voted, ideology_conlib_num) %>%
  mutate(og = "ACNT") %>%
  rename(wgt = rk_wgt_trim)

gr <- read_csv(here("3_cleaned_data", "general_retail_clean.csv")) %>%
  select(unemp_benefits, other_welfare, ehf_aware_pretr, ehf_support_exist, 
         ehf_support_new, ehf_support_new_num, acs_weight_trim, registered, ideology, voted, party, 
         vote_choice, ideology_conlib_num) %>%
  mutate(og = "General Retail", 
         new = ifelse(is.na(ehf_support_exist), "Support for a new EHF (Gen retail)", "Support for existing EHF (Gen retail)"), 
         supp = ifelse(is.na(ehf_support_exist), ehf_support_new, ehf_support_exist), 
         voted = voted == "Yes") %>%
  rename(wgt = acs_weight_trim)

THD <- read_csv(here("3_cleaned_data", "THD_clean.csv")) %>%
  select(Q4.7, Q4.10, EHF_aware_list, rk_wgt, Q5.2, Q5.3, Q5.5, Q5.6) %>%
  rename(unemp_benefits = Q4.7, other_welfare = Q4.10, ehf_aware_pretr = EHF_aware_list, 
         wgt = rk_wgt, registered = Q5.2, voted = Q5.5, vote_choice_20 = Q5.6, 
         ideology = Q5.3) %>%
  mutate(og = "THD", voted = voted == "Yes")

wf <- bind_rows(ACNT, gr, THD)

## Correlations
cor(as.numeric(ACNT$unemp_benefits == "Yes"), as.numeric(ACNT$ehf_aware_pretr)) # Some relationship here
cor(as.numeric(gr$unemp_benefits == "Yes"), as.numeric(gr$ehf_aware_pretr))
cor(as.numeric(THD$Q4.7 == "Yes"), as.numeric(THD$EHF_aware_list))

cor.test(as.numeric(ACNT$unemp_benefits == "Yes"), as.numeric(ACNT$ehf_aware_pretr))
cor.test(as.numeric(gr$unemp_benefits == "Yes"), as.numeric(gr$ehf_aware_pretr))
cor.test(as.numeric(THD$Q4.7 == "Yes"), as.numeric(THD$EHF_aware_list))

cor(as.numeric(ACNT$other_welfare == "Yes"), as.numeric(ACNT$ehf_aware_pretr)) # No relationship
cor(as.numeric(gr$other_welfare == "Yes"), as.numeric(gr$ehf_aware_pretr))
cor(as.numeric(THD$Q4.10 == "Yes"), as.numeric(THD$EHF_aware_list), use = "complete.obs")

cor.test(as.numeric(ACNT$other_welfare == "Yes"), as.numeric(ACNT$ehf_aware_pretr))
cor.test(as.numeric(gr$other_welfare == "Yes"), as.numeric(gr$ehf_aware_pretr))
cor.test(as.numeric(THD$Q4.10 == "Yes"), as.numeric(THD$EHF_aware_list))

cor(as.numeric(gr$unemp_benefits == "Yes"), as.numeric(gr$ehf_support_exist_num), use = "complete.obs") # Maybe something to look at?
cor(as.numeric(gr$unemp_benefits == "Yes"), as.numeric(gr$ehf_support_new_num), use = "complete.obs")
cor(as.numeric(gr$other_welfare == "Yes"), as.numeric(gr$ehf_support_exist_num), use = "complete.obs")
cor(as.numeric(gr$other_welfare == "Yes"), as.numeric(gr$ehf_support_new_num), use = "complete.obs")

cor.test(as.numeric(gr$unemp_benefits == "Yes"), as.numeric(gr$ehf_support_exist_num), use = "complete.obs")
cor.test(as.numeric(gr$unemp_benefits == "Yes"), as.numeric(gr$ehf_support_new_num), use = "complete.obs")
cor.test(as.numeric(gr$other_welfare == "Yes"), as.numeric(gr$ehf_support_exist_num), use = "complete.obs")
cor.test(as.numeric(gr$other_welfare == "Yes"), as.numeric(gr$ehf_support_new_num), use = "complete.obs")


## Plots: benefits and awareness
ggplot(wf) +
  geom_bar(aes(x = unemp_benefits, fill = ehf_aware_pretr), position = "fill") +
  facet_grid(cols = vars(og))

ggplot(filter(wf, og == "General Retail")) +
  geom_bar(aes(x = unemp_benefits, fill = supp), position = "fill") +
  facet_grid(cols = vars(new))
  
ggplot(filter(wf, og == "General Retail" & other_welfare != "Don't know")) +
  geom_bar(aes(x = other_welfare, fill = supp), position = "fill") +
  facet_grid(cols = vars(new))

### Plots: politics and awareness
ggplot(wf) +
  geom_bar(aes(x = voted, fill = ehf_aware_pretr), position = "fill") +
  facet_grid(cols = vars(og)) #nothing interesting

ggplot(wf) +
  geom_bar(aes(x = (registered == "Yes"), fill = ehf_aware_pretr), position = "fill") +
  facet_grid(cols = vars(og)) #nothing interesting

ggplot(wf) +
  geom_bar(aes(x = ehf_aware_pretr, fill = ideology), position = "fill") +
  facet_grid(cols = vars(og))

glm(ehf_aware_pretr ~ voted + registered + ideology_conlib_num + party, ACNT, family = "binomial")

summary(glm(ehf_aware_pretr ~ voted + registered + ideology_conlib_num + party, THD, family = "binomial"))

summary(lm(ehf_support_new_num ~ (registered == "Yes") + ideology_conlib_num + (party == "Something else"), gr))

summary(lm(attachment_index ~ party + registered + ideology_conlib_num, ACNT))

## Use of welfare
weighted.mean(ACNT$unemp_benefits == "Yes", ACNT$wgt, na.rm = T) # 7.3%
weighted.mean(gr$unemp_benefits == "Yes", gr$wgt, na.rm = T) # 7.4%
weighted.mean(THD$unemp_benefits == "Yes", THD$wgt, na.rm = T) # 16.5%

weighted.mean(ACNT$other_welfare == "Yes", ACNT$wgt) # 34.5%
weighted.mean(gr$other_welfare == "Yes", gr$wgt) # 31.3%
weighted.mean(THD$other_welfare == "Yes", THD$wgt, na.rm = T) # 19.5%

weighted.mean(ACNT$other_welfare == "Yes" | ACNT$unemp_benefits == "Yes", ACNT$wgt) # 36.7%
weighted.mean(gr$other_welfare == "Yes"| gr$unemp_benefits == "Yes", gr$wgt) # 35.2%
weighted.mean(THD$other_welfare == "Yes"| THD$unemp_benefits == "Yes", THD$wgt, na.rm = T) # 30.9%






