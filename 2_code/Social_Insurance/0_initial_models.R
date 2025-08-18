library(here)
source(here::here('2_code', '1_libraries_and_settings_global.R'))
here::i_am("2_code/Social_Insurance/0_initial_models.R")

## Read in the data
gr_clean <- read.csv(here("3_cleaned_data", "general_retail_clean.csv"))

wal <- read.csv(here("3_cleaned_data", "ACNT_clean.csv"))

thd <- read.csv(here("3_cleaned_data", "THD_clean.csv"))

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
                                  + income_num + home_ownership + hpi_5year +
                                    (other_welfare == "Yes") + no_ideology + ideology_conlib_num_0 + age_clean + male +
                                    college + practice_religion_num + nonwhite + tenure_num))
      
      ## With interaction
      gr_int <- with(gr_data, lm(dv[[i]] ~ iv[[j]] + iv[[j]]:practice_religion_num + 
                                         no_ideology + ideology_conlib_num_0 +
                                         hpi_5year*home_ownership +
                                         income_num + (other_welfare == "Yes") + age_clean + male +
                                         college + practice_religion_num + nonwhite + tenure_num))
      
      coef_maps <- c("iv[[j]]" = iv_names_main[j],
                     "income_num" = "income",
                     'home_ownershipTRUE' = "home owner",
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
                     'hpi_5year:home_ownershipTRUE' = "home owner x hpi"
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
                                 + income_num + home_ownership + hpi_5year +
                                   (other_welfare == "Yes") + no_ideology + ideology_conlib_num_0 + age_clean + male +
                                   college + practice_religion_num + nonwhite + tenure_num))
      
      ## With interaction
      gr_int <- with(gr_data, lm(dv[[i]] ~ iv[[j]] + iv[[j]]:practice_religion_num + 
                                   no_ideology + ideology_conlib_num_0 +
                                   hpi_5year*home_ownership +
                                   income_num + (other_welfare == "Yes") + age_clean + male +
                                   college + practice_religion_num + nonwhite + tenure_num))
      
      coef_maps <- c("iv[[j]]" = iv_names_main[j],
                     "income_num" = "income",
                     'home_ownershipTRUE' = "home owner",
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
                     'hpi_5year:home_ownershipTRUE' = "home owner x hpi"
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
                                  + income_num + home_ownership + hpi_5year +
                                    (other_welfare == "Yes") + no_ideology + ideology_conlib_num_0 + age_clean + male +
                                    college + practice_religion_num + nonwhite + tenure_num))
      
      ## With interaction
      wal_int <- with(wal_data, lm(dv[[i]] ~ iv[[j]] + iv[[j]]:practice_religion_num + 
                                    hpi_5year*home_ownership +
                                    income_num + (other_welfare == "Yes") + 
                                    no_ideology + ideology_conlib_num_0 + age_clean + male +
                                    college + practice_religion_num + nonwhite + tenure_num))
      
      }
      
      if(i == 3){
        
        ## With covs
        wal_cov <- with(wal_data, lm(dv[[i]] ~ iv[[j]]
                                     + income_num + home_ownership + hpi_5year +
                                       (other_welfare == "Yes") + no_ideology + ideology_conlib_num_0 + age_clean + male +
                                       college + practice_religion_num + nonwhite + tenure_num + treatment_placebo))
        
        ## With interaction
        wal_int <- with(wal_data, lm(dv[[i]] ~ iv[[j]] + iv[[j]]:practice_religion_num + 
                                       hpi_5year*home_ownership +
                                       income_num + (other_welfare == "Yes") + 
                                       no_ideology + ideology_conlib_num_0 + age_clean + male +
                                       college + practice_religion_num + nonwhite + tenure_num + treatment_placebo))
        
      }
      
      coef_maps <- c("iv[[j]]" = iv_names_main[j],
                     "income_num" = "income",
                     'home_ownershipTRUE' = "home owner",
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
                     "treatment_placeboplacebo" = "placebo group",
                     "treatment_placebotreatment" = "treatment group",
                     "iv[[j]]:practice_religion_num" = paste(iv_names_main[j], "x religiosity"), 
                     "iv[[j]]:ideology_conlib_num" = paste(iv_names_main[j], "x ideology"), 
                     'hpi_5year:home_ownershipTRUE' = "home owner x hpi"
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
  
  dv_names_file <- c("ehf_aware", "ehf_don_past")
  iv_names_file <- c("_repl", "_recip", "_tanf")
  
  modsums_out <- list()
  
  for(i in 1:length(dv)){
    for(j in 1:length(iv)){
      ## Without covs
      thd_ncov <- with(thd_data, lm(dv[[i]] ~ iv[[j]]))
      
      ## With covs
      thd_cov <- with(thd_data, lm(dv[[i]] ~ iv[[j]]
                                   + income_num + home_ownership + hpi_5year +
                                     (other_welfare == "Yes") + no_ideology + ideology_conlib_num_0 + age_clean + male +
                                     college + practice_religion_num + nonwhite + tenure_num +
                                     kids + cohabit + treated))
      
      ## With interaction
      thd_int <- with(thd_data, lm(dv[[i]] ~ iv[[j]] + iv[[j]]:practice_religion_num + 
                                     hpi_5year*home_ownership +
                                     income_num + (other_welfare == "Yes") + 
                                     no_ideology + ideology_conlib_num_0 + age_clean + male +
                                     college + practice_religion_num + nonwhite + tenure_num +
                                     kids + cohabit + treated))
      
      coef_maps <- c("iv[[j]]" = iv_names_main[j],
                     "income_num" = "income",
                     'home_ownershipTRUE' = "home owner",
                     "hpi_5year" = "five year hpi",
                     'other_welfare == "Yes"TRUE' = "welfare recipient",                     
                     "no_ideologyTRUE" = "not thought about ideology",
                     "ideology_conlib_num_0" = "ideology [responded]",
                     "practice_religion_num" = "religiosity",
                     "tenure_num" = "job tenure",
                     "maleTRUE" = "male",
                     "nonwhiteTRUE" = "nonwhite",
                     "collegeTRUE" = "college", 
                     "kidsTRUE" = "has children",
                     "cohabitTRUE" = "cohabiting",
                     "age_clean" = "age",
                     "treatedTRUE" = "treatment group",
                     "iv[[j]]:practice_religion_num" = paste(iv_names_main[j], "x religiosity"), 
                     "iv[[j]]:ideology_conlib_num" = paste(iv_names_main[j], "x ideology"), 
                     'hpi_5year:home_ownershipTRUE' = "home owner x hpi"
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
