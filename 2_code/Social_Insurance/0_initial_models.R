library(here)
library(sjPlot)
source(here::here('2_code', '1_libraries_and_settings_global.R'))
here::i_am("2_code/Social_Insurance/0_initial_models.R")

## Read in the data
gr_clean <- read.csv(here("3_cleaned_data", "general_retail_clean.csv")) %>%
  mutate(no_ideology = is.na(ideology_answered), 
         ideology_conlib_num_0 = (1 - no_ideology)*replace_na(ideology_conlib_num, 0))

wal <- read.csv(here("3_cleaned_data", "ACNT_clean.csv")) %>%
  mutate(no_ideology = ideology == "Haven’t thought much about this", 
         ideology_conlib_num_0 = (1 - no_ideology)*replace_na(ideology_conlib_num, 0))


thd <- read.csv(here("3_cleaned_data", "THD_clean.csv")) %>% 
  mutate(no_ideology = Q5.3 == "Haven’t thought much about this")


gr_clean$ehf_donate_exist_num <- gr_clean$ehf_donate_exist == "Yes"
gr_clean$ehf_donate_new_num <- gr_clean$ehf_donate_new == "Yes"

wal$ehf_donation_num <- wal$ehf_donation == "Yes"
wal$ehf_donation_post <- wal$donate == "YES I would like to learn how to donate"

# THD additional data format

thd$ehf_donation_num <- thd$Q3.12 == "Yes"

thd <- thd %>%
  mutate(income = factor(Q6.14, levels = 
                           c("Prefer not to state", "$150,000 or more per year",
                             "At least $100,000 but less than $150,000 per year",
                             "At least 75,000 but less than $100,000 per year",
                             "At least $50,000 but less than $75,000 per year",
                             "At least $35,000 but less than $50,000 per year",
                             "At least $25,000 but less than $35,000 per year",
                             "At least $15,000 but less than $25,000 per year",
                             "Less than $15,000 per year"),
                         ordered = T),
         income_num = case_match(income,
                                 "$150,000 or more per year" ~ 150,
                                 "At least $100,000 but less than $150,000 per year" ~ 100,
                                 "At least 75,000 but less than $100,000 per year" ~ 75,
                                 "At least $50,000 but less than $75,000 per year" ~ 50,
                                 "At least $35,000 but less than $50,000 per year" ~ 35,
                                 "At least $25,000 but less than $35,000 per year" ~ 25,
                                 "At least $15,000 but less than $25,000 per year" ~ 15,
                                 "Less than $15,000 per year"~ 0, .default = NA), 
         other_welfare = grepl("Unemployment insurance", Q3.18) | 
           grepl("Public assistance (SNAP, food stamps, cash welfare)", Q3.18) |
           grepl("Disability insurance or worker’s compensation", Q3.18), 
         ideology_conlib = fct_rev(factor(Q5.3, 
                                          levels = 
                                            c("Extremely liberal",
                                              "Liberal",
                                              "Slightly Liberal", 
                                              "Moderate",
                                              "Slightly Conservative",
                                              "Conservative",
                                              "Extremely conservative"),
                                          ordered = T)),
         ideology_conlib_num = ifelse(is.na(ideology_conlib), NA,
                                      (as.numeric(ideology_conlib) - 1)/4), 
         age_clean = case_match(age, 
                                2021 ~ NA,
                                328 ~ 58,
                                121 ~ NA,
                                .default = age), 
         practice_religion = fct_rev(factor(Q6.6, levels =
                                              c("At least once per week",
                                                "Once a week",
                                                "Once or twice a month",
                                                "A few times a year",
                                                "Seldom",
                                                "Never"),
                                            ordered = T)),
         practice_religion_num = 
           (as.numeric(practice_religion) - 1)/5)

note2 <- "Standard errors in parentheses. Errors clustered at the state level"

### General Population Models --------

create_mods_gr_EHF <- function(gr_data){
  dv <- with(gr_data, list(
              ehf_support_exist_num, 
              ehf_donate_exist_num))
  
  iv <- with(gr_data, list(
                replacement_2024_ui, 
                recipiency_2024_ui, 
                WG_TANF))
  
  dv_names_main <- c("support for existing EHF", "willingness to donate to existing EHF")
  iv_names_main <- c("replacement rate", "recipiency rate", "tanf generosity")
  
  dv_names_file <- c("ehf_supp_exist", "ehf_don_exist")
  iv_names_file <- c("_repl", "_recip", "_tanf")
  
  modsums_out <- list()
  
  for(i in 1:length(dv)){
    for(j in 1:length(iv)){
      ## Without covs
      gr_ncov <- with(gr_data, lm(dv[[i]] ~ iv[[j]]))
      
      ## With covs
      gr_cov <- with(gr_data, lm(dv[[i]] ~ iv[[j]]
                                  + income_num + (home_ownership == "Own") + hpi_5year +
                                    (other_welfare == "Yes") + no_ideology + ideology_conlib_num_0 + age_clean + male +
                                    college + practice_religion_num + nonwhite + tenure_num))
      
      ## With interaction
      gr_int <- with(gr_data, lm(dv[[i]] ~ iv[[j]] + iv[[j]]:practice_religion_num + 
                                         no_ideology + ideology_conlib_num_0 +
                                         hpi_5year*(home_ownership == "Own") +
                                         income_num + (other_welfare == "Yes") + age_clean + male +
                                         college + practice_religion_num + nonwhite + tenure_num))
      
      coef_maps <- c("iv[[j]]" = iv_names_main[j],
                     "income_num" = "income",
                     'home_ownership == "Own"TRUE' = "home owner",
                     "hpi_5year" = "five year hpi",
                     'other_welfare == "Yes"TRUE' = "welfare recipient",
                     "practice_religion_num" = "religiosity",
                     "maleTRUE" = "male",
                     "nonwhiteTRUE" = "nonwhite",
                     "no_ideologyTRUE" = "not thought about ideology",
                     "ideology_conlib_num_0" = "ideology [responded]",
                     "collegeTRUE" = "college",
                     "tenure_num" = "job tenure",
                     "age_clean" = "age",
                     "iv[[j]]:practice_religion_num" = paste(iv_names_main[j], "x religiosity"), 
                     #"iv[[j]]:ideology_conlib_num" = paste(iv_names_main[j], "x ideology"), 
                     'hpi_5year:home_ownership == "Own"TRUE' = "home owner x hpi"
                     )
      
      support.models_gr <- list(gr_ncov, gr_cov, gr_int)
      
      names(support.models_gr) <- c("No Covariates", "Covariates", "With Interaction")
      
      modsums_out <- modelsummary::modelsummary(support.models_gr,
                                 coef_map = coef_maps,
                                 vcov = ~residence,
                                 #add_rows = rows,
                                 title = paste("OLS regression of", dv_names_main[[i]], "on", iv_names_main[[j]]),
                                 output = "html",
                                 notes = list(note2),
                                 stars = c('*' = .05, '**' = .01))
      
      save_kable(modsums_out, here("4_output", "SocialInsurancePoliticsPaper", 
                                   paste("reg_tab_gr_", dv_names_file[i], iv_names_file[j], ".html", sep = "")))
    }
  }
}

create_mods_gr_noEHF <- function(gr_data){
  dv <- with(gr_data, list(
    ehf_support_new_num, 
    ehf_donate_new_num))
  
  iv <- with(gr_data, list(
    replacement_2024_ui, 
    recipiency_2024_ui, 
    WG_TANF))
  
  dv_names_main <- c("support for new EHF", "willingness to donate to a new EHF")
  iv_names_main <- c("replacement rate", "recipiency rate", "tanf generosity")
  
  dv_names_file <- c("ehf_supp_new", "ehf_don_new")
  iv_names_file <- c("_repl", "_recip", "_tanf")
  
  modsums_out <- list()
  
  for(i in 1:length(dv)){
    for(j in 1:length(iv)){
      ## Without covs
      gr_ncov <- with(gr_data, lm(dv[[i]] ~ iv[[j]]))
      
      ## With covs
      gr_cov <- with(gr_data, lm(dv[[i]] ~ iv[[j]]
                                 + income_num + (home_ownership == "Own") + hpi_5year +
                                   (other_welfare == "Yes") + no_ideology + ideology_conlib_num_0 + age_clean + male +
                                   college + practice_religion_num + nonwhite + tenure_num))
      
      ## With interaction
      gr_int <- with(gr_data, lm(dv[[i]] ~ iv[[j]] + iv[[j]]:practice_religion_num + 
                                   no_ideology + ideology_conlib_num_0 +
                                   hpi_5year*(home_ownership == "Own") +
                                   income_num + (other_welfare == "Yes") + age_clean + male +
                                   college + practice_religion_num + nonwhite + tenure_num))
      
      
      coef_maps <- c("iv[[j]]" = iv_names_main[j],
                     "income_num" = "income",
                     'home_ownership == "Own"TRUE' = "home owner",
                     "hpi_5year" = "five year hpi",
                     'other_welfare == "Yes"TRUE' = "welfare recipient",
                     "practice_religion_num" = "religiosity",
                     "no_ideologyTRUE" = "not thought about ideology",
                     "ideology_conlib_num_0" = "ideology [responded]",
                     "maleTRUE" = "male",
                     "nonwhiteTRUE" = "nonwhite",
                     "collegeTRUE" = "college", 
                     "tenure_num" = "job tenure",
                     "age_clean" = "age",
                     "iv[[j]]:practice_religion_num" = paste(iv_names_main[j], "x religiosity"), 
                     #"iv[[j]]:ideology_conlib_num" = paste(iv_names_main[j], "x ideology"), 
                     'hpi_5year:home_ownership == "Own"TRUE' = "home owner x hpi"
      )
      
      support.models_gr <- list(gr_ncov, gr_cov, gr_int)
      
      names(support.models_gr) <- c("No Covariates", "Covariates", "With Interaction")
      
      modsums_out <- modelsummary::modelsummary(support.models_gr,
                                                coef_map = coef_maps,
                                                vcov = ~residence,
                                                #add_rows = rows,
                                                title = paste("OLS regression of", dv_names_main[[i]], "on", iv_names_main[[j]]),
                                                output = "html",
                                                notes = list(note2),
                                                stars = c('*' = .05, '**' = .01))
      
      save_kable(modsums_out, here("4_output", "SocialInsurancePoliticsPaper", 
                                   paste("reg_tab_gr_", dv_names_file[i], iv_names_file[j], ".html", sep = "")))
    }
  }
}

create_mods_gr_EHF(gr_clean)

create_mods_gr_noEHF(gr_clean)

### Walmart Models --------

create_mods_walmart <- function(wal_data){
  dv <- with(wal_data, list(
    ehf_aware_pretr, 
    ehf_donation_num,
    ehf_donation_post
    ))
  
  iv <- with(wal_data, list(
    replacement_2024_ui, 
    recipiency_2024_ui, 
    WG_TANF))
  
  dv_names_main <- c("awareness of EHF", "past donation to the EHF", "post survey donation")
  iv_names_main <- c("replacement rate", "recipiency rate", "tanf generosity")
  
  dv_names_file <- c("ehf_aware", "ehf_don_past", "ehf_don_new")
  iv_names_file <- c("_repl", "_recip", "_tanf")
  
  modsums_out <- list()
  
  for(i in 1:length(dv)){
    for(j in 1:length(iv)){
      ## Without covs
      wal_ncov <- with(wal_data, lm(dv[[i]] ~ iv[[j]]))
      
      if(i != 3){
      
      ## With covs
      wal_cov <- with(wal_data, lm(dv[[i]] ~ iv[[j]]
                                  + income_num + (home_ownership == "Own") + hpi_5year +
                                    (other_welfare == "Yes") + no_ideology + ideology_conlib_num_0 + age_clean + male +
                                    college + practice_religion_num + nonwhite + tenure_num))
      
      ## With interaction
      wal_int <- with(wal_data, lm(dv[[i]] ~ iv[[j]] + iv[[j]]:practice_religion_num + 
                                    hpi_5year*(home_ownership == "Own") +
                                    income_num + (other_welfare == "Yes") + 
                                    no_ideology + ideology_conlib_num_0 + age_clean + male +
                                    college + practice_religion_num + nonwhite + tenure_num))
      
      }
      
      if(i == 3){
        
        ## With covs
        wal_cov <- with(wal_data, lm(dv[[i]] ~ iv[[j]]
                                     + income_num + (home_ownership == "Own") + hpi_5year +
                                       (other_welfare == "Yes") + no_ideology + ideology_conlib_num_0 + age_clean + male +
                                       college + practice_religion_num + nonwhite + tenure_num))
        
        ## With interaction
        wal_int <- with(wal_data, lm(dv[[i]] ~ iv[[j]] + iv[[j]]:practice_religion_num + 
                                       hpi_5year*(home_ownership == "Own") +
                                       income_num + (other_welfare == "Yes") + 
                                       no_ideology + ideology_conlib_num_0 + age_clean + male +
                                       college + practice_religion_num + nonwhite + tenure_num))
        
      }
      
      coef_maps <- c("iv[[j]]" = iv_names_main[j],
                     "income_num" = "income",
                     'home_ownership == "Own"TRUE' = "home owner",
                     "hpi_5year" = "five year hpi",
                     'other_welfare == "Yes"TRUE' = "welfare recipient",                     
                     "no_ideologyTRUE" = "not thought about ideology",
                     "ideology_conlib_num_0" = "ideology [responded]",
                     "practice_religion_num" = "religiosity",
                     "maleTRUE" = "male",
                     "nonwhiteTRUE" = "nonwhite",
                     "collegeTRUE" = "college", 
                     "tenure_num" = "job tenure",
                     "age_clean" = "age",
                     "iv[[j]]:practice_religion_num" = paste(iv_names_main[j], "x religiosity"), 
                     "iv[[j]]:ideology_conlib_num" = paste(iv_names_main[j], "x ideology"), 
                     'hpi_5year:home_ownership == "Own"TRUE' = "home owner x hpi"
      )
      
      support.models_gr <- list(wal_ncov, wal_cov, wal_int)
      
      names(support.models_gr) <- c("No Covariates", "Covariates", "With Interaction")
      
      modsums_out <- modelsummary::modelsummary(support.models_gr,
                                                coef_map = coef_maps,
                                                vcov = ~residence,
                                                #add_rows = rows,
                                                title = paste("OLS regression of", dv_names_main[[i]], "on", iv_names_main[[j]]),
                                                output = "html",
                                                notes = list(note2),
                                                stars = c('*' = .05, '**' = .01))
      
      save_kable(modsums_out, here("4_output", "SocialInsurancePoliticsPaper", 
                                   paste("reg_tab_wal_", dv_names_file[i], iv_names_file[j], ".html", sep = "")))
    }
  }
}

create_mods_walmart(wal)

### THD Models --------

create_mods_thd <- function(thd_data){
  dv <- with(thd_data, list(
    EHF_aware_list, 
    ehf_donation_num
  ))
  
  iv <- with(thd_data, list(
    replacement_2021_ui, 
    recipiency_2021_ui, 
    WG_TANF))
  
  dv_names_main <- c("awareness of EHF", "past donation to the EHF")
  iv_names_main <- c("replacement rate", "recipiency rate", "tanf generosity")
  
  dv_names_file <- c("ehf_supp", "ehf_don_past", "ehf_don_new")
  iv_names_file <- c("_repl", "_recip", "_tanf")
  
  modsums_out <- list()
  
  for(i in 1:length(dv)){
    for(j in 1:length(iv)){
      ## Without covs
      thd_ncov <- with(thd_data, lm(dv[[i]] ~ iv[[j]]))
      
      ## With covs
      thd_cov <- with(thd_data, lm(dv[[i]] ~ iv[[j]]
                                   + income_num + (home_ownership == "Own") + hpi_5year +
                                     (other_welfare == "Yes") + ideology_conlib_num + age_clean + male +
                                     college + practice_religion_num + nonwhite))
      
      ## With interaction
      thd_int <- with(thd_data, lm(dv[[i]] ~ iv[[j]] + iv[[j]]:practice_religion_num + 
                                     iv[[j]]:ideology_conlib_num + hpi_5year*(home_ownership == "Own") +
                                     income_num + (other_welfare == "Yes") + 
                                     ideology_conlib_num + age_clean + male +
                                     college + practice_religion_num + nonwhite))
      
      coef_maps <- c("iv[[j]]" = iv_names_main[j],
                     "income_num" = "income",
                     'home_ownership == "Own"TRUE' = "home owner",
                     "hpi_5year" = "five year hpi",
                     'other_welfare == "Yes"TRUE' = "welfare recipient",
                     "ideology_conlib_num" = "ideology",
                     "practice_religion_num" = "religiosity",
                     "maleTRUE" = "male",
                     "nonwhiteTRUE" = "nonwhite",
                     "collegeTRUE" = "college", 
                     "age_clean" = "age",
                     "iv[[j]]:practice_religion_num" = paste(iv_names_main[j], "x religiosity"), 
                     "iv[[j]]:ideology_conlib_num" = paste(iv_names_main[j], "x ideology"), 
                     'hpi_5year:home_ownership == "Own"TRUE' = "home owner x hpi"
      )
      
      support.models_gr <- list(thd_ncov, thd_cov, thd_int)
      
      names(support.models_gr) <- c("No Covariates", "Covariates", "With Interaction")
      
      modsums_out <- modelsummary::modelsummary(support.models_gr,
                                                coef_map = coef_maps,
                                                vcov = ~Q2.6,
                                                #add_rows = rows,
                                                title = paste("OLS regression of", dv_names_main[[i]], "on", iv_names_main[[j]]),
                                                output = "html",
                                                notes = list(note2),
                                                stars = c('*' = .05, '**' = .01))
      
      save_kable(modsums_out, here("4_output", "SocialInsurancePoliticsPaper", 
                                   paste("reg_tab_thd_", dv_names_file[i], iv_names_file[j], ".html", sep = "")))
    }
  }
}

create_mods_thd(thd)
