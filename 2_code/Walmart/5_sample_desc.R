#Basic output for appendices descriptives and appendices

library(MASS)
library(tidyverse)
library(codebook)
library(survey)
library(srvyr)
# devtools::install_github("pewresearch/pewmethods", build_vignettes = TRUE)
library(pewmethods)
library(qualtRics)
library(excluder)
library(naniar)
ggplot2::theme_set(ggplot2::theme_bw())
library(here)
library(estimatr)
here()

#datasets
wmt <- read_csv(here("3_cleaned_data", "ACNT_clean.csv"))
wmt.hq<-wmt |> 
  filter(quality == "high")
wmt.hqnp<-wmt |> 
  filter(quality == "high", treatment_full != "vid0")  #appears that placebo failed

wmt_wgt_f <- wmt %>%
  as_survey_design(ids = 1, weights = rk_wgt_trim)
wmt_wgt_dei <- wmt %>%
  as_survey_design(ids = 1, weights = rk_dei_trim)

#duration and treatment

duration.table <- wmt |> 
  group_by(treatment_full) |>
  summarize(
    mean_duration = mean(duration),
    median_duration = median(duration),
    sd_duration = sd(duration),
    attach_mean = mean(attachment_index),
    uv_notno = sum(union_vote != "Against the union")/n(),
    n = n()
  )

duration.table.hq <- wmt |>
  filter(quality == "high") |> 
  group_by(treatment_full) |> 
  summarize(
    mean_duration = mean(duration),
    median_duration = median(duration),
    sd_duration = sd(duration),
    attach_mean = mean(attachment_index),
    uv_notno = sum(union_vote != "Against the union")/n(),
    n = n()
  )



summary(lm(log(duration)~ treatment_full, data=wmt))


#correlate pre-treatment awareness with manipulation check
#among control respondents

wmt |> filter(treatment_bin==F) |>
  select(ehf_exist, ehf_aware_pretr)  |> 
  mutate(ehf_exist = as.numeric(ehf_exist=="Yes")) |> 
  cor(method = "pearson")

#Manipulation check
mc_bin <-lm_robust(ehf_exist=="Yes" ~ treatment_bin, 
                   data = wmt, se_type = "HC3")
mc_bin_pe <-lm_robust(ehf_exist=="Yes" ~ treatment_bin*ehf_aware_pretr, 
                   data = wmt, se_type = "HC3")
mc_full <-lm_robust(ehf_exist=="Yes" ~ treatment_full, 
                    data = wmt, se_type = "HC3")
mc_full_pe <-lm_robust(ehf_exist=="Yes" ~ treatment_full*ehf_aware_pretr, 
                      data = wmt, se_type = "HC3")


mc_bin_hq <-lm_robust(ehf_exist=="Yes" ~ treatment_bin, 
                   data = subset(wmt, quality =="high"), 
                   se_type = "HC3")
mc_full_hq <-lm_robust(ehf_exist=="Yes" ~ treatment_full, 
                    data = subset(wmt, quality =="high"),
                    se_type = "HC3")
mc_bin_hq_pe <-lm_robust(ehf_exist=="Yes" ~ treatment_bin*ehf_aware_pretr, 
                      data = subset(wmt, quality =="high"), 
                      se_type = "HC3")
mc_full_hq_pe <-lm_robust(ehf_exist=="Yes" ~ treatment_full*ehf_aware_pretr, 
                       data = subset(wmt, quality =="high"),
                       se_type = "HC3")


mc_all <- list(mc_bin, mc_full, mc_bin_pe, mc_full_pe, 
               mc_bin_hq, mc_full_hq, mc_bin_hq_pe, mc_full_hq_pe)
names(mc_all) <-  rep(c("binary", "detailed "),4 )

coef_maps <- c("treatment_binTRUE" = "treated",
               "treatment_fullvid0" = "placebo",
               "treatment_fullvidChar" = "charity treatment",
               "treatment_fullvidSolid" = "solidarity treatment",
               "treatment_binTRUE:ehf_aware_pretrTRUE" = "treat x pre-exposed",
               "treatment_fullvid0:ehf_aware_pretrTRUE" = "placebo x pre-exposed",
               "treatment_fullvidChar:ehf_aware_pretrTRUE" = "charity x pre-exposed",
               "treatment_fullvidSolid:ehf_aware_pretrTRUE" = "solidarity x pre-exposed"
               )

gm <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0))
note2 <- "Robust standard errors in parentheses."


extra_row <- data.frame(
  term =  "Sample",
  one = "full",
  two = "full",
  three = "full",
  four = "full",
  five = "high quality",
  six = "high quality",
  seven = "high quality",
  eight = "high quality"
)
  
attr(extra_row, "position") <- 17
model_print<- modelsummary::modelsummary( mc_all,
                            #shape = "rbind",
                            coef_map = coef_maps,
                            gof_map = gm,
                            #vcov = "robust",
                            add_rows = extra_row,
                            title = "OLS regression of treatment on correctly reporting a Walmart EHF (manipulation check) \\label{tab:awareness-model}",
                            output = "kableExtra",
                            notes = list(note2),
                            stars = c('*' = .05, '**' = .01)
)


### Sample descriptives

### Balance tests