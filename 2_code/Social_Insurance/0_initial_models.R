library(here)
source(here::here('2_code', '1_libraries_and_settings_global.R'))
here::i_am("2_code/Social_Insurance/0_initial_models.R")

## Read in the data
gr_clean <- read.csv(here("3_cleaned_data", "general_retail_clean.csv"))

gr_clean$ehf_donate_exist_num <- gr_clean$ehf_donate_exist == "Yes"
note2 <- "Standard errors in parentheses."

### General Population Models --------

create_mods_gr <- function(gr_data){
  dv <- with(gr_data, list(
              ehf_support_exist_num, 
              ehf_donate_exist_num))
  
  iv <- with(gr_data, list(
                replacement_2024_ui, 
                recipiency_2024_ui, 
                WG_TANF))
  
  dv_names_main <- c("support for new EHF", "willingness to donate to a new EHF")
  iv_names_main <- c("replacement rate", "recipiency rate", "tanf generosity")
  
  modsums_out <- list()
  
  for(i in 1:length(dv)){
    for(j in 1:length(iv)){
      ## Without covs
      gr_ncov <- with(gr_data, lm(dv[[i]] ~ iv[[j]]))
      
      ## With covs
      gr_cov <- with(gr_clean, lm(dv[[i]] ~ iv[[j]]
                                  + income_num + (home_ownership == "Own") + hpi_5year +
                                    (other_welfare == "Yes") + ideology_conlib_num + age_clean + male +
                                    college + practice_religion_num + nonwhite))
      
      ## With interaction
      gr_int <- with(gr_clean, lm(dv[[i]] ~ iv[[j]] + iv[[j]]:practice_religion_num + 
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
                     'hpi_5year:home_ownership == "Own"TRUE' = "home ownership x hpi"
                     )
      
      support.models_gr <- list(gr_ncov, gr_cov, gr_int)
      
      names(support.models_gr) <- c("No Covariates", "Covariates", "With Interaction")
      
      modsums_out <- append(modsums_out, modelsummary::modelsummary(support.models_gr,
                                 coef_map = coef_maps,
                                 vcov = ~residence,
                                 #add_rows = rows,
                                 title = paste("OLS regression of", dv_names_main[[i]], "on", iv_names_main[[j]]),
                                 output = "html",
                                 notes = list(note2),
                                 stars = c('*' = .05, '**' = .01)))
    }
  }
  modsums_out
}

