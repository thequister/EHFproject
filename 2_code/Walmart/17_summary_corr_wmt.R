#wmt.hq

#----------------------------------------------------------
# 1. Variable set and clean working dataset
#----------------------------------------------------------
vars <- c(
  "treatment_bin",
  "ehf_aware_pretr",
  "ehf_other_recip",
  "ehf_applied",
  "ehf_received_all",
  "ehf_donation",
  "emergency_expense_num",
  "new_job_num",
  "wrk_loyal_num",
  "emp_loyal_num",
  "emp_reco_num",
  "union_for",    
  "union_against",
  "union_coworkers_6",
  "age_clean",
  "male",
  "main_job",
  "tenure_num",
  "nonwhite",
  "fulltime",
  "hourly",
  "college"
)

df_cor <- wmt.hq %>%
  mutate(
    ehf_applied     = if_else(ehf_applied == "Yes", 1, 0, missing = NA_real_),
    ehf_other_recip = if_else(ehf_other_recip == "Yes", 1, 0, missing = NA_real_),
    ehf_donation    = if_else(ehf_donation == "Yes", 1, 0, missing = NA_real_),
    union_for       = if_else(union_elec == "For the union", 1, 0, missing = NA_real_),
    # assuming union_elec has "Against the union"; if not, tweak this line:
    union_against   = if_else(union_elec == "Against the union", 1, 0, missing = NA_real_),
    main_job        = if_else(main_job == "Yes", 1, 0, missing = NA_real_)
  ) %>%
  select(all_of(vars))

#----------------------------------------------------------
# 2. Variable labels
#----------------------------------------------------------
var_labels <- c(
  treatment_bin       = "Treated",
  ehf_aware_pretr     = "Aware of HF",
  ehf_other_recip     = "Knows beneficiary",
  ehf_applied         = "Applied to HF",
  ehf_received_all    = "Received grant",
  ehf_donation        = "Donated",
  emergency_expense_num = "Emergency expense ability",
  new_job_num         = "Quit intention",
  wrk_loyal_num       = "Co-worker loyalty",
  emp_loyal_num       = "Employer loyalty",
  emp_reco_num        = "Employer recommend",
  union_for           = "Union: for",
  union_against       = "Union: against",
  union_coworkers_6   = "E[Union vote]",
  age_clean           = "Age",
  male                = "Male",
  main_job            = "Main job",
  tenure_num          = "Tenure (months)",
  nonwhite            = "Nonwhite",
  fulltime            = "Full-time",
  hourly              = "Hourly",
  college             = "College degree"
)

#==========================================================
# A) DESCRIPTIVE STATISTICS TABLE
#==========================================================
means    <- sapply(df_cor, function(x) mean(x, na.rm = TRUE))
medians  <- sapply(df_cor, function(x) median(x, na.rm = TRUE))
sds      <- sapply(df_cor, function(x) sd(x, na.rm = TRUE))
valid_n  <- sapply(df_cor, function(x) sum(!is.na(x)))

desc_df <- tibble(
  var_name = names(means),
  Variable = var_labels[var_name],
  Mean     = round(means,   2),
  Median   = round(medians, 2),
  SD       = round(sds,     2),
  N        = valid_n
) %>%
  dplyr::select(Variable, Mean, Median, SD, N)

# indices for job-attachment rows
job_attach_vars       <- c("new_job_num", "wrk_loyal_num", "emp_loyal_num", "emp_reco_num")
job_attach_labels     <- var_labels[job_attach_vars]
job_attach_idx_desc   <- which(desc_df$Variable %in% job_attach_labels)

wmt_desc_tab <- desc_df %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    digits   = 2,
    align    = c("l", rep("r", 4)),
    caption = "Descriptive statistics \\label{tab:wmt_desc}",
    escape   = FALSE
  ) %>%
  add_header_above(c(" " = 1, "Descriptive statistics" = 4)) %>%
  row_spec(job_attach_idx_desc, background = "gray!15") %>%
  group_rows("Job attachment index", min(job_attach_idx_desc), max(job_attach_idx_desc)) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 9
  )

#==========================================================
# B) CORRELATION TABLES (split across 3 pages)
#   Part 1 ends with emergency_expense_num
#   Part 2 ends with union_coworkers_6
#==========================================================

cor_mat <- cor(df_cor, use = "pairwise.complete.obs")

cor_lower <- cor_mat
cor_lower[upper.tri(cor_lower)] <- NA
diag(cor_lower) <- 1

# Apply labels to rows/cols
clean_names <- var_labels[colnames(cor_lower)]
colnames(cor_lower) <- clean_names
rownames(cor_lower) <- clean_names

cor_df <- as.data.frame(round(cor_lower, 2))
cor_df[is.na(cor_df)] <- ""  # blank upper triangle
cor_df <- cor_df %>%
  tibble::rownames_to_column("Variable")

# job-attachment row indices in correlation table
job_attach_idx_cor <- which(cor_df$Variable %in% job_attach_labels)

# ---- FUNCTION to wrap long column headers using \makecell ----
# (needs \usepackage{makecell} in YAML header-includes)
wrap_names_makecell <- function(x, width = 12) {
  vapply(
    x,
    function(z) {
      lines <- strwrap(z, width = width)
      paste0("\\makecell[l]{", paste(lines, collapse = " \\\\ "), "}")
    },
    character(1)
  )
}

# ---- define split points in terms of df_cor variable order ----
idx_emerg <- which(names(df_cor) == "emergency_expense_num")
idx_unionc <- which(names(df_cor) == "union_coworkers_6")
n_vars <- ncol(df_cor)

# Sanity: make sure we actually found them
# (if either is length 0, you'll want to check the names)

# Part 1: Variable + correlations up through emergency_expense_num
part1_idx <- 1:idx_emerg
cor_df_part1 <- cor_df[, c(1, 1 + part1_idx)]

# Part 2: Variable + correlations from just after emergency_expense_num
#         through union_coworkers_6
part2_idx <- (idx_emerg + 1):idx_unionc
cor_df_part2 <- cor_df[, c(1, 1 + part2_idx)]

# Part 3: Variable + correlations after union_coworkers_6, if any
if (idx_unionc < n_vars) {
  part3_idx <- (idx_unionc + 1):n_vars
  cor_df_part3 <- cor_df[, c(1, 1 + part3_idx)]
} else {
  cor_df_part3 <- NULL
}

# ---- helper to build one correlation table (with title and wrapping) ----
make_cor_table <- function(df_part, title, job_idx) {
  if (is.null(df_part)) return(NULL)
  
  # wrap headers except the first column
  colnames(df_part) <- c(
    "Variable",
    wrap_names_makecell(colnames(df_part)[-1], width = 12)
  )
  
  tab <- df_part %>%
    kbl(
      format   = "latex",
      booktabs = TRUE,
      digits   = 2,
      align    = c("l", rep("r", ncol(df_part) - 1)),
      caption = "Correlation matrix with treatment, key outcomes, and covariates \\label{tab:wmt_corr}",
      escape   = FALSE,  # allow \makecell and \\ in headers
      na       = ""
    ) %>%
    add_header_above(
      setNames(
        c(1, ncol(df_part) - 1),
        c(" ", title)
      )
    )
  
  # Shade / group job-attachment rows if present
  if (length(job_idx) > 0) {
    tab <- tab %>%
      row_spec(job_idx, background = "gray!15") %>%
      group_rows(
        "Job attachment index",
        min(job_idx),
        max(job_idx)
      )
  }
  
  tab %>%
    kable_styling(
      latex_options = c("hold_position", "scale_down"),
      font_size = 7
    ) %>%
    landscape()
}

wmt_cor_tab_1 <- make_cor_table(
  df_part = cor_df_part1,
  title   = "Correlations (Part 1)",
  job_idx = job_attach_idx_cor
)

wmt_cor_tab_2 <- make_cor_table(
  df_part = cor_df_part2,
  title   = "Correlations (Part 2)",
  job_idx = job_attach_idx_cor
)

wmt_cor_tab_3 <- make_cor_table(
  df_part = cor_df_part3,
  title   = "Correlations (Part 3)",
  job_idx = job_attach_idx_cor
)


# #wmt.hq
# 
# #----------------------------------------------------------
# # 1. Variable set and clean working dataset
# #----------------------------------------------------------
# vars <- c(
#   "treatment_bin",
#   "ehf_aware_pretr",
#   "ehf_other_recip",
#   "ehf_applied",
#   "ehf_received_all",
#   "ehf_donation",
#   "emergency_expense_num",
#   "new_job_num",
#   "wrk_loyal_num",
#   "emp_loyal_num",
#   "emp_reco_num",
#   "union_for",    
#   "union_against",
#   "union_coworkers_6",
#   "age_clean",
#   "male",
#   "main_job",
#   "tenure_num",
#   "nonwhite",
#   "fulltime",
#   "hourly",
#   "college"
# )
# 
# df_cor <- wmt.hq %>%
#   mutate(
#     ehf_applied   = if_else(ehf_applied =="Yes", 1, 0, missing = NA_real_),
#     #ehf_received_all = if_else(ehf_received_all =="Yes", 1, 0, missing = NA_real_),
#     ehf_other_recip = if_else(ehf_other_recip=="Yes", 1, 0, missing = NA_real_),
#     ehf_donation = if_else(ehf_donation=="Yes", 1, 0, missing = NA_real_),
#     union_for  = if_else(union_elec  == "For the union", 1, 0, missing = NA_real_),
#     union_against = if_else(union_against  == "For the union", 1, 0, missing = NA_real_),
#     main_job  = if_else(main_job  == "Yes", 1, 0, missing = NA_real_)
#   ) %>%
#   select(all_of(vars))
# 
# #----------------------------------------------------------
# # 2. Variable labels
# #----------------------------------------------------------
# var_labels <- c(
#   treatment_bin       = "Treated",
#   ehf_aware_pretr      = "Aware of HF",
#   ehf_other_recip             = "Knows beneficiary",
#   ehf_applied          = "Applied to HF",
#   ehf_received_all         = "Received grant",
#   ehf_donation = "Donated",
#   emergency_expense_num = "Emergency expense ability",
#   new_job_num = "Quit intention",
#   wrk_loyal_num       = "Co-worker loyalty",
#   emp_loyal_num       = "Employer loyalty",
#   emp_reco_num        = "Employer recommend",
#   union_for             = "Union: for",
#   union_against         = "Union: against",
#   union_coworkers_6 =  "E[Union vote]",
#   age_clean           = "Age",
#   male                = "Male",
#   main_job            = "Main job",
#   tenure_num          = "Tenure (months)",
#   nonwhite            = "Nonwhite",
#   fulltime            = "Full-time",
#   hourly              = "Hourly",
#   college             = "College degree"
# )
# 
# wrap_names <- function(x, width = 12) {
#   str_replace_all(x, sprintf("(.{%d})", width), "\\1\\\\newline ")
# }
# 
# #==========================================================
# # A) DESCRIPTIVE STATISTICS TABLE
# #==========================================================
# means    <- sapply(df_cor, function(x) mean(x, na.rm = TRUE))
# medians  <- sapply(df_cor, function(x) median(x, na.rm = TRUE))
# sds      <- sapply(df_cor, function(x) sd(x, na.rm = TRUE))
# valid_n  <- sapply(df_cor, function(x) sum(!is.na(x)))
# 
# desc_df <- tibble(
#   var_name = names(means),
#   Variable = var_labels[var_name],
#   Mean     = round(means,   2),
#   Median   = round(medians, 2),
#   SD       = round(sds,     2),
#   N        = valid_n
# ) %>%
#   select(Variable, Mean, Median, SD, N)
# 
# # indices for job-attachment rows (same variables as before)
# job_attach_vars   <- c("new_job_num", "wrk_loyal_num", "emp_loyal_num", "emp_reco_num")
# job_attach_labels <- var_labels[job_attach_vars]
# job_attach_idx_desc <- which(desc_df$Variable %in% job_attach_labels)
# 
# wmt_desc_tab <- desc_df %>%
#   kbl(
#     format   = "latex",
#     booktabs = TRUE,
#     digits   = 2,
#     align    = c("l", rep("r", 4)),
#     escape   = FALSE
#   ) %>%
#   add_header_above(c(" " = 1, "Descriptive statistics" = 4)) %>%
#   row_spec(job_attach_idx_desc, background = "gray!15") %>%
#   group_rows("Job attachment index", min(job_attach_idx_desc), max(job_attach_idx_desc)) %>%
#   kable_styling(
#     latex_options = c("hold_position"),
#     font_size = 9
#   )
# 
# #==========================================================
# # B) CORRELATION TABLE (wrapped headers + safe group_rows)
# #==========================================================
# 
# cor_mat <- cor(df_cor, use = "pairwise.complete.obs")
# 
# cor_lower <- cor_mat
# cor_lower[upper.tri(cor_lower)] <- NA
# diag(cor_lower) <- 1
# 
# # Apply labels to rows/cols
# clean_names <- var_labels[colnames(cor_lower)]
# colnames(cor_lower) <- clean_names
# rownames(cor_lower) <- clean_names
# 
# cor_df <- as.data.frame(round(cor_lower, 2))
# cor_df[is.na(cor_df)] <- ""  # blank upper triangle
# cor_df <- cor_df %>%
#   tibble::rownames_to_column("Variable")
# 
# job_attach_idx_cor <- which(cor_df$Variable %in% job_attach_labels)
# 
# # ---- FUNCTION to wrap long column headers using \makecell ----
# wrap_names <- function(x, width = 12) {
#   vapply(
#     x,
#     function(z) {
#       lines <- strwrap(z, width = width)
#       paste0("\\makecell[l]{", paste(lines, collapse = " \\\\ "), "}")
#     },
#     character(1)
#   )
# }
# 
# # Apply wrapping to all *correlation* column names before kbl
# # (keep the "Variable" column header as-is)
# colnames(cor_df) <- c(
#   "Variable",
#   wrap_names(colnames(cor_df)[-1], width = 10)
# )
# 
# n_corr <- ncol(df_cor)
# 
# # Build the base table
# wmt_cor_tab <- cor_df %>%
#   kbl(
#     format   = "latex",
#     booktabs = TRUE,
#     digits   = 2,
#     align    = c("l", rep("r", ncol(cor_df) - 1)),
#     escape   = FALSE,   # CRITICAL so \makecell and \\ are interpreted
#     na       = ""
#   ) %>%
#   add_header_above(
#     c(" " = 1, "Correlations" = ncol(cor_df) - 1)
#   )
# 
# # Conditionally shade and group job-attachment rows (only if they exist)
# if (length(job_attach_idx_cor) > 0) {
#   thd_cor_tab <- thd_cor_tab %>%
#     row_spec(job_attach_idx_cor, background = "gray!15") %>%
#     group_rows(
#       "Job attachment index",
#       min(job_attach_idx_cor),
#       max(job_attach_idx_cor)
#     )
# }
# 
# # Final styling + landscape
# wmt_cor_tab <- wmt_cor_tab %>%
#   kable_styling(
#     latex_options = c("hold_position", "scale_down"),
#     font_size = 7
#   ) %>%
#   landscape()
