genpop <- genpop |>
  mutate(union_elec = relevel(factor(union_elec), ref = "For the union"))


genpop_svy <- svydesign(ids = ~1, weights = ~acs_weight_trim, data = genpop) |>
  as.svrepdesign(type = "bootstrap", replicates = 500)

# ── Study 1: OLS on fin sec, attachment index and MNL on union vote ────────────────

finsec_gp_base <- svyglm(
  emergency_expense_num ~ ehf_aware_pretr,
  design = genpop_svy
)

finsec_gp_cov <- svyglm(
  emergency_expense_num ~ ehf_aware_pretr + tenure_num + male + age_clean +
    hourly + fulltime + nonwhite + college + main_job,
  design = genpop_svy
)


# Attachment: OLS
attach_gp_base <- svyglm(
  attachment_index ~ ehf_aware_pretr,
  design = genpop_svy
)

attach_gp_cov <- svyglm(
  attachment_index ~ ehf_aware_pretr + tenure_num + male + age_clean +
    hourly + fulltime + nonwhite + college + main_job,
  design = genpop_svy
)

# Union vote: multinomial logit
uv_gp_base <- svymultinom(
  union_elec ~ ehf_aware_pretr,
  design = genpop_svy
)

uv_gp_cntrl <- svymultinom(
  union_elec ~ ehf_wrk_new_num,
  design = genpop_svy
)

uv_gp_cntrl_cov <- svymultinom(
  union_elec ~ ehf_wrk_new_num + tenure_num + male + age_clean +
    hourly + fulltime + nonwhite + college + main_job,
  design = genpop_svy
)


uv_gp_cov <- svymultinom(
  union_elec ~ ehf_aware_pretr + tenure_num + male + age_clean +
    hourly + fulltime + nonwhite + college + main_job,
  design = genpop_svy
)

# ── Table output ──────────────────────────────────────────────────────────

coef_map_gp <- c(
  "ehf_aware_pretrTRUE" = "EHF aware",
  "ehf_wrk_new_num"     = "EHF control (pro-worker)",
  "tenure_num"          = "Tenure",
  "maleTRUE"            = "Male",
  "age_clean"           = "Age",
  "hourlyTRUE"          = "Hourly",
  "fulltimeTRUE"        = "Full-time",
  "nonwhiteTRUE"        = "Nonwhite",
  "collegeTRUE"         = "College",
  "main_jobYes"            = "Main job"
)

gm_ols <- list(
  list("raw" = "nobs",      "clean" = "$N$",   "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2)
)

gm_mnl <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "aic",  "clean" = "AIC", "fmt" = 0)
)

# Attachment table
genpop_ja_ols_tab <-modelsummary(
  list("Fin. Sec. 1" = finsec_gp_base,
       "Fin. Sec. 2" = finsec_gp_cov,
       "Job Attach. 1" = attach_gp_base, 
       "Job Attach. 2" = attach_gp_cov),
  coef_map       = coef_map_gp,
  gof_map        = gm_ols,
  vcov           = "robust",
  stars          = c('$^+$' = .1, '*' = .05, '**' = .01),
  title          = "Weighted OLS estimates: EHF awareness, financial security, and job attachment, Study 1 \\label{tab:tab-gp-attachment}",
  notes          = "Robust standard errors in parentheses.",
  output         = "kableExtra",
  #output = "flextable",
  threeparttable = TRUE,
  escape         = TRUE
)

# Union vote table

coef_map_gp_mnl <- c(
  "Against the union.ehf_aware_pretrTRUE" = "EHF aware (Against)",
  "Not sure.ehf_aware_pretrTRUE"          = "EHF aware (Not sure)",
  "Against the union.ehf_wrk_new_num"     = "Worker EHF control (Against)",
  "Not sure.ehf_wrk_new_num"     = "Worker EHF control (Not sure)"
)

n_mnl1 <- sum(!is.na(genpop$union_elec) & !is.na(genpop$ehf_aware_pretr))
n_mnl2  <- sum(complete.cases(genpop[, c("union_elec", "ehf_aware_pretr",
                                        "tenure_num", "male", "age_clean",
                                        "hourly", "fulltime", "nonwhite",
                                        "college", "main_job")]))
n_mnl3 <- sum(!is.na(genpop$union_elec) & !is.na(genpop$ehf_wrk_new_num))
n_mnl4  <- sum(complete.cases(genpop[, c("union_elec", "ehf_wrk_new_num",
                                        "tenure_num", "male", "age_clean",
                                        "hourly", "fulltime", "nonwhite",
                                        "college", "main_job")]))


rows_gp_mnl <- data.frame(
  term         = c("Covariates", "$N"),
  "(1)"         = c("No",n_mnl1),
  "(2)"   = c("Yes",n_mnl2),
  "(3)"         = c("No",n_mnl3),
  "(4)"   = c("Yes",n_mnl4)
)
attr(rows_gp_mnl, "position") <- nrow(coef_map_gp_mnl) + 1

genpop_uv_mnl_tab<-modelsummary(
  list("(1)" = uv_gp_base, "(2)" = uv_gp_cov, "(3)" = uv_gp_cntrl, 
       "(4)" = uv_gp_cntrl_cov),
  coef_map       = coef_map_gp_mnl,
  gof_map        = gm_mnl,
  add_rows       = rows_gp_mnl,
  stars          = c('$^+$' = .1, '*' = .05, '**' = .01),
  title          = "Weighted multinomial logit: EHF awareness and union vote intention, Study 1 \\label{tab:tab-gp-uv}",
  notes          = "Reference category is 'For the union.' Bootstrap standard errors in parentheses.",
  output         = "kableExtra",
  #output = "flextable",
  threeparttable = TRUE,
  escape         = TRUE
)