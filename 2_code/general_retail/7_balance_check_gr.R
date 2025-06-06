#source(here::here('2_code', 'general_retail', '2_data_format_gr.R'))
#source(here::here('2_code', 'general_retail', '3_weights_gr.R'))

### Descriptives Table

# Unweighted
tbl_sum_genpop_1<- 
  genpop |> 
  select(ehf_aware_pretr, age_clean, male, main_job, 
         tenure_fac, nonwhite, fulltime, hourly, college, member_union,
         ehf_prior_know, emergency_expense, new_job, attachment_index) |> 
  gtsummary::tbl_summary(missing_text = "(missing/not gathered)",
                         label = list( 
                           ehf_aware_pretr ~ "EHF awareness (list)", 
                           male ~ "male",
                           main_job ~ "main job", 
                           tenure_fac ~ "job tenure",
                           nonwhite ~ "PoC/nonwhite", 
                           fulltime ~ "full time",
                           hourly ~ "hourly", 
                           college ~ "college degree", 
                           member_union ~ "union member",
                           ehf_prior_know ~ "EHF awareness (direct)", 
                           emergency_expense ~ "deal with emergency expense", 
                           new_job ~ "accept new job", 
                           #ehf_offer_percent ~ "accuracy of ehf ident", 
                           attachment_index ~ "attachment index"
                         )
  ) |>  
  gtsummary::modify_caption("**Sample summary statistics**") |> 
  gtsummary::bold_labels() |> 
  gtsummary::as_kable_extra()

tbl_sum_genpop_1


tbl_sum_genpop_2<- 
  genpop |> 
  select(wrk_loyal, emp_loyal, emp_reco, 
         union_vote, govt_responsib_hardship, govt_responsib_elder, 
         govt_responsib_unemp) |>
  gtsummary::tbl_summary(missing_text = "(missing/not gathered)",
                         label = list(
                           wrk_loyal ~"loyalty to coworkers", emp_loyal~"loyalty to employer",
                           emp_reco~"recommend employer", union_vote ~"union vote", 
                           govt_responsib_hardship ~"hardship support", govt_responsib_elder~"pension support", govt_responsib_unemp~"UI support"
                         )
  ) |> 
  gtsummary::modify_caption("*summary table (cont'd)*") |> 
  gtsummary::bold_labels() |> 
  gtsummary::as_kable_extra()

tbl_sum_genpop_2


# Weighted
#gr_w <- gr_clean %>%
#  srvyr::as_survey_design(ids = 1, weights = acs_weight_trim)

tbl_sum_genpop_1w<- 
  genpop_w |> 
  select(ehf_aware_pretr, age_clean, male, main_job, 
         tenure_fac, nonwhite, fulltime, hourly, college, member_union,
         ehf_prior_know, emergency_expense, new_job, attachment_index) |> 
  gtsummary::tbl_svysummary(missing_text = "(missing/not gathered)",
                         label = list( 
                           ehf_aware_pretr ~ "EHF awareness (list)", 
                           male ~ "male",
                           main_job ~ "main job", tenure_fac ~ "job tenure",
                           nonwhite ~ "PoC/nonwhite", fulltime ~ "full time",
                           hourly ~ "hourly", college ~ "college degree", 
                           member_union ~ "union member",
                           ehf_prior_know ~ "EHF awareness (direct)",
                           emergency_expense ~ "deal with emergency expense", 
                           new_job ~ "accept new job", 
                           #ehf_offer_percent ~ "accuracy of ehf ident", 
                           attachment_index ~ "attachment index"
                         )
  ) |>  
  gtsummary::modify_caption("**Sample summary statistics**") |> 
  gtsummary::bold_labels() |> 
  gtsummary::as_kable_extra()

tbl_sum_genpop_1w

tbl_sum_genpop_2w<- 
  genpop_w |> 
  select(wrk_loyal, emp_loyal, emp_reco, 
         union_vote, govt_responsib_hardship, govt_responsib_elder, govt_responsib_unemp) |>
  gtsummary::tbl_svysummary(missing_text = "(missing/not gathered)",
                         label = list(
                           wrk_loyal ~"loyalty to coworkers", emp_loyal~"loyalty to employer",
                           emp_reco~"recommend employer", union_vote ~"union vote", 
                           govt_responsib_hardship ~"hardship support", govt_responsib_elder~"pension support", govt_responsib_unemp~"UI support"
                         )
  ) |> 
  gtsummary::modify_caption("*summary table (cont'd)*") |> 
  gtsummary::bold_labels() |> 
  gtsummary::as_kable_extra()

tbl_sum_genpop_2w