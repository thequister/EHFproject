# ── Load and prepare WERN data ─────────────────────────────────────────────

wern <- read.csv(here("3_cleaned_data", "wern_analysis_v3.0.csv"))

ind_simp <- c("healthcare", "hospitality", "retail", "telecom", "warehousing")

wern$industry_screen <- as.factor(wern$industry_screen)

wern <- wern |>
  mutate(
    union_exposed = if_else(
      union_rep_num == 0 & (union_ever_num == 1 | union_fam_num == 1), 1, 0),
    age           = 2022 - age_num,
    age_cat       = cut(age, breaks = c(17, 34, 54, 100),
                        labels = c("18-34", "35-54", "55+")),
    union_vote    = fct_relevel(
      fct_recode(as.factor(union_support_num),
                 DK = "-77", Yes = "1", No = "0"),
      "Yes"),   # Yes as reference category
    college       = as.factor(highest_degree_num > 4),
    female        = as.factor(gender_num == 2),
    union_rep_yes = as.factor(union_rep_num == 1),
    white         = as.factor(race == "White"),
    gop           = as.factor(pol_pid3_num == 1),
    hourly        = as.factor(pay_type_num %in% c(1, 2)),
    industry_simp = as.factor(case_match(
      industry_screen,
      "Health care"                                        ~ ind_simp[1],
      "Hospitality (food service, hotel, etc.)"            ~ ind_simp[2],
      "Retail"                                             ~ ind_simp[3],
      "Telecommunications"                                 ~ ind_simp[4],
      "Warehousing (such as at a fulfillment center)"      ~ ind_simp[5]))
  )

# ── Subset to retail employed non-managers ────────────────────────────────

wern_retail <- wern |>
  filter(
    industry_simp == "retail",
    employed_num  == 1,
    worker_num    == 0
  )

# ── Construct attachment index following Study 3 approach ─────────────────
# Scale each component to [0,1] then run single-factor FA
# find_job_num is reverse-scaled (higher = more likely to quit = less attached)

wern_retail <- wern_retail |>
  mutate(
    find_job_clean = na_if(find_job_num, -77),
    job_sat_01  = (job_sat_num  - min(job_sat_num,  na.rm = TRUE)) /
      (max(job_sat_num,  na.rm = TRUE) - min(job_sat_num,  na.rm = TRUE)),
    job_reco_01 = (job_reco_num - min(job_reco_num, na.rm = TRUE)) /
      (max(job_reco_num, na.rm = TRUE) - min(job_reco_num, na.rm = TRUE)),
    job_fwd_01  = (job_fwd_num  - min(job_fwd_num,  na.rm = TRUE)) /
      (max(job_fwd_num,  na.rm = TRUE) - min(job_fwd_num,  na.rm = TRUE)),
    find_job_01 = 1 - (find_job_clean - min(find_job_clean, na.rm = TRUE)) /
      (max(find_job_clean, na.rm = TRUE) - min(find_job_clean, na.rm = TRUE))
  )
pca_att_wern <- wern_retail |>
  select(job_sat_01, job_reco_01, job_fwd_01, find_job_01)

fa_wern <- psych::fa(pca_att_wern, nfactors = 1, scores = "regression")  #job search loads weakly.

pca_att_wern_3 <- wern_retail |>
  select(job_sat_01, job_reco_01, job_fwd_01)

fa_wern_3 <- psych::fa(pca_att_wern_3, nfactors = 1, scores = "regression")

#wern_retail$attachment_index <- as.numeric(fa_wern$scores)
wern_retail$attachment_index_3 <- as.numeric(fa_wern_3$scores)
# ── Survey design object ──────────────────────────────────────────────────

wern_svy <- svydesign(ids = ~1, weights = ~rk_wgt_ind, data = wern_retail) |>
  as.svrepdesign(type = "bootstrap", replicates = 500) |>
  update(
    union_vote = fct_relevel(union_vote, "Yes")
  )

# ── Weighted union vote distribution table ────────────────────────────────

wern_svy_srvyr <- wern_retail |>
  as_survey_design(ids = 1, weights = rk_wgt_ind)

wern_uv_dist <- wern_svy_srvyr |>
  filter(!is.na(union_vote)) |>
  group_by(union_vote) |>
  summarise(
    n    = unweighted(n()),
    prop = survey_mean(na.rm = TRUE)
  )

wern_uv_dist_tab <- wern_uv_dist |>
  kbl(
    col.names = c("Response", "N (unweighted)", "Proportion", "SE"),
    digits    = 3,
    caption   = "Weighted distribution of union vote intentions, 2022 WERN retail sample \\label{tab:tab-wern-uv-dist}",
    booktabs  = TRUE
  ) |>
  kable_styling(latex_options = c("hold_position"))

# ── Attachment index regressions ──────────────────────────────────────────

attach_wern_base <- svyglm(
  attachment_index_3 ~ benefits_emergency_cash,
  design = wern_svy
)

attach_wern_cov <- svyglm(
  attachment_index_3 ~ benefits_emergency_cash + tenure_num + female +
    age + hourly + college + white,
  design = wern_svy
)

# ── Union vote multinomial regressions ────────────────────────────────────

uv_wern_base <- svymultinom(
  union_vote ~ benefits_emergency_cash,
  design = wern_svy
)

uv_wern_cov <- svymultinom(
  union_vote ~ benefits_emergency_cash + tenure_num + female +
    age + hourly + college + white,
  design = wern_svy
)

# ── Attachment table ──────────────────────────────────────────────────────

coef_map_wern <- c(
  "benefits_emergency_cash" = "EHF aware",
  "tenure_num"              = "Tenure",
  "femaleTRUE"              = "Female",
  "age"                     = "Age",
  "hourlyTRUE"              = "Hourly",
  "collegeTRUE"             = "College",
  "whiteTRUE"               = "White"
)

gm_ols <- list(
  list("raw" = "nobs",      "clean" = "$N$",   "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2)
)

wern_ja_ols_tab <- modelsummary(
  list("Base" = attach_wern_base, "Covariates" = attach_wern_cov),
  coef_map       = coef_map_wern,
  gof_map        = gm_ols,
  vcov           = "robust",
  stars          = c('$^+$' = .1, '*' = .05, '**' = .01),
  title          = "OLS estimates: EHF awareness and job attachment, WERN retail sample \\label{tab:tab-wern-attachment}",
  notes          = "Robust standard errors in parentheses. Outcome is job attachment index.",
  output         = "kableExtra",
  #threeparttable = TRUE,
  escape         = TRUE
)

# ── Union vote table (manual extraction as with genpop MNL) ───────────────

coef_map_wern_mnl <- c(
  "No.benefits_emergency_cash" = "EHF aware (Against)",
  "DK.benefits_emergency_cash" = "EHF aware (Not sure)"
)

gm_mnl <- list(
  list("raw" = "nobs", "clean" = "$N$", "fmt" = 0),
  list("raw" = "aic",  "clean" = "AIC", "fmt" = 0)
)

n_wern_base <- sum(complete.cases(wern_retail[, c("union_vote", 
                                                  "benefits_emergency_cash")]))
n_wern_cov  <- sum(complete.cases(wern_retail[, c("union_vote",
                                                  "benefits_emergency_cash",
                                                  "tenure_num", "female",
                                                  "age", "hourly",
                                                  "college", "white")]))

rows_wern_mnl <- data.frame(
  term         = c("Covariates", "$N$"),
  "Base"         = c("No",n_wern_base),
  "Covariates"   = c("Yes",n_wern_cov)
)
attr(rows_wern_mnl, "position") <- nrow(coef_map_wern_mnl) + 1

wern_uv_mnl_tab <- modelsummary(
  list("Base" = uv_wern_base, "Covariates" = uv_wern_cov),
  coef_map       = coef_map_wern_mnl,
  gof_map        = gm_mnl,
  add_rows       = rows_wern_mnl,
  stars          = c('$^+$' = .1, '*' = .05, '**' = .01),
  title          = "Weighted multinomial logit: EHF awareness and union vote intention, WERN \\label{tab:tab-wern-uv}",
  notes          = "Reference category is 'For the union.' Bootstrap standard errors in parentheses.",
  output         = "kableExtra",
  threeparttable = TRUE,
  escape         = FALSE
)
