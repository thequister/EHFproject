#----------------------------------------------------------
# 0. Construct bill_num_rev
#----------------------------------------------------------
THD_comp_uw$bill_num_rev <- (max(as.numeric(THD_comp_uw$bills), na.rm = TRUE) -
                               as.numeric(THD_comp_uw$bills)) /
  (max(as.numeric(THD_comp_uw$bills), na.rm = TRUE) -
     min(as.numeric(THD_comp_uw$bills), na.rm = TRUE))


vars <- c(
  "vid",
  "txt",
  "EHF_aware_list",
  "HF_know",
  "HF_applied",
  "HF_received",
  "HF_donate",
  "emergency_expense_num",
  "bill_num_rev",
  "wrk_loyal_num",
  "emp_loyal_num",
  "emp_reco_num",
  "union_for",
  "union_against",
  "rk_age",
  "male",
  "main_job",
  "tenure_num",
  "nonwhite",
  "fulltime",
  "hourly",
  "college"
)

df_cor <- THD_comp_uw %>%
  mutate(
    HF_know       = if_else(HF_know     == "Yes", 1, 0, missing = NA_real_),
    HF_applied    = if_else(HF_applied  == "Yes", 1, 0, missing = NA_real_),
    HF_received   = if_else(HF_received == "Yes", 1, 0, missing = NA_real_),
    HF_donate     = if_else(HF_donate   == "Yes", 1, 0, missing = NA_real_),
    union_for     = if_else(union_vote  == "For the union",     1, 0, missing = NA_real_),
    union_against = if_else(union_vote  == "Against the union", 1, 0, missing = NA_real_),
    vid           = if_else(HDTreatment == "vid", 1, 0, missing = NA_real_),
    txt           = if_else(HDTreatment == "txt", 1, 0, missing = NA_real_)
  ) %>%
  select(all_of(vars))

#----------------------------------------------------------
# 2. Variable labels (now includes union_for / union_against)
#----------------------------------------------------------
var_labels <- c(
  vid                   = "Video treatment",
  txt                   = "Text treatment",
  EHF_aware_list        = "Aware of HF",
  HF_know               = "Knows beneficiary",
  HF_applied            = "Applied to HF",
  HF_received           = "Received grant",
  HF_donate             = "Donated",
  emergency_expense_num = "Emergency expense ability",
  bill_num_rev          = "Ease of paying bills",
  wrk_loyal_num         = "Co-worker loyalty",
  emp_loyal_num         = "Employer loyalty",
  emp_reco_num          = "Employer recommend",
  union_for             = "Union: for",
  union_against         = "Union: against",
  rk_age                = "Age",
  male                  = "Male",
  main_job              = "Main job",
  tenure_num            = "Tenure (mth)",
  nonwhite              = "Nonwhite",
  fulltime              = "Full-time",
  hourly                = "Hourly",
  college               = "College degree"
)

#==========================================================
# A) DESCRIPTIVE STATISTICS TABLE
#   (mean, median, SD, valid N) including both union dummies
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
job_attach_vars      <- c("wrk_loyal_num", "emp_loyal_num", "emp_reco_num")
job_attach_labels    <- var_labels[job_attach_vars]
job_attach_idx_desc  <- which(desc_df$Variable %in% job_attach_labels)

thd_desc_tab <- desc_df %>%
  kbl(
    format   = "latex",
    booktabs = TRUE,
    digits   = 2,
    caption = "Descriptive statistics \\label{tab:thd_desc}",
    align    = c("l", rep("r", 4)),
    escape   = FALSE
  ) %>%
  add_header_above(c(" " = 1, "Descriptive statistics" = 4)) %>%
  row_spec(job_attach_idx_desc, background = "gray!15") %>%
  group_rows("Job attachment index", min(job_attach_idx_desc), max(job_attach_idx_desc)) %>%
  kable_styling(
    latex_options = c("hold_position"),
    font_size = 9
  )

cor_mat <- cor(df_cor, use = "pairwise.complete.obs")

cor_lower <- cor_mat
cor_lower[upper.tri(cor_lower)] <- NA
diag(cor_lower) <- 1

# Apply pretty labels to rows/cols
clean_names <- var_labels[colnames(cor_lower)]
colnames(cor_lower) <- clean_names
rownames(cor_lower) <- clean_names

cor_df <- as.data.frame(round(cor_lower, 2))
cor_df[is.na(cor_df)] <- ""  # blank upper triangle
cor_df <- cor_df %>%
  tibble::rownames_to_column("Variable")

# job-attachment row indices in correlation table
job_attach_idx_cor <- which(cor_df$Variable %in% job_attach_labels)

# ---- helper to wrap long headers via \makecell ----
# (needs \usepackage{makecell} in YAML header-includes)
wrap_names <- function(x, width = 12) {
  vapply(
    x,
    function(z) {
      lines <- strwrap(z, width = width)
      paste0("\\makecell[l]{", paste(lines, collapse = " \\\\ "), "}")
    },
    character(1)
  )
}

# ---- split columns after union_against ----
# position of union_against in df_cor (1-based, excluding "Variable")
split_pos <- which(names(df_cor) == "union_for")

# part 1: Variable + correlations up through union_against
cor_df_part1 <- cor_df[, c(1, 1 + seq_len(split_pos-1))]

# part 2: Variable + correlations after union_against
if (split_pos < ncol(df_cor)) {
  cor_df_part2 <- cor_df[, c(1, 1 + seq(split_pos, ncol(df_cor)))]
} else {
  cor_df_part2 <- NULL
}

# ---- helper to build one correlation table ----
make_cor_table <- function(df_part, title, job_idx) {
  if (is.null(df_part)) return(NULL)
  
  # wrap headers except the first column
  colnames(df_part) <- c(
    "Variable",
    wrap_names(colnames(df_part)[-1], width = 12)
  )
  
  tab <- df_part %>%
    kbl(
      format   = "latex",
      booktabs = TRUE,
      digits   = 2,
      align    = c("l", rep("r", ncol(df_part) - 1)),
      caption = "Correlation matrix with treatment, key outcomes, and covariates \\label{tab:thd_corr}",
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

thd_cor_tab_1 <- make_cor_table(
  df_part = cor_df_part1,
  title   = "Correlations (Part 1)",
  job_idx = job_attach_idx_cor
)

thd_cor_tab_2 <- make_cor_table(
  df_part = cor_df_part2,
  title   = "Correlations (Part 2)",
  job_idx = job_attach_idx_cor
)





# #THD_comp_uw
# 
# #----------------------------------------------------------
# # 0. Construct bill_num_rev
# #----------------------------------------------------------
# THD_comp_uw$bill_num_rev <- (max(as.numeric(THD_comp_uw$bills), na.rm = TRUE) -
#                                as.numeric(THD_comp_uw$bills)) /
#   (max(as.numeric(THD_comp_uw$bills), na.rm = TRUE) -
#      min(as.numeric(THD_comp_uw$bills), na.rm = TRUE))
# 
# #----------------------------------------------------------
# # 1. Variable set and clean working dataset
# #----------------------------------------------------------
# vars <- c(
#   "vid",
#   "txt",
#   "EHF_aware_list",
#   "HF_know",
#   "HF_applied",
#   "HF_received",
#   "HF_donate",
#   "emergency_expense_num",
#   "bill_num_rev",
#   "wrk_loyal_num",
#   "emp_loyal_num",
#   "emp_reco_num",
#   "union_vote",
#   "rk_age",
#   "male",
#   "main_job",
#   "tenure_num",
#   "nonwhite",
#   "fulltime",
#   "hourly",
#   "college"
# )
# 
# df_cor <- THD_comp_uw %>%
#   mutate(
#     HF_know     = if_else(HF_know     == "Yes", 1, 0, missing = NA_real_),
#     HF_applied  = if_else(HF_applied  == "Yes", 1, 0, missing = NA_real_),
#     HF_received = if_else(HF_received == "Yes", 1, 0, missing = NA_real_),
#     HF_donate = if_else(HF_donate=="Yes", 1, 0, missing = NA_real_),
#     union_vote  = if_else(union_vote  == "For the union", 1, 0, missing = NA_real_),
#     vid         = if_else(HDTreatment == "vid", 1, 0, missing = NA_real_),
#     txt         = if_else(HDTreatment == "txt", 1, 0, missing = NA_real_)
#   ) %>%
#   select(all_of(vars))
# 
# #----------------------------------------------------------
# # 2. Variable labels
# #----------------------------------------------------------
# var_labels <- c(
#   vid                 = "Video treatment",
#   txt                 = "Text treatment",
#   EHF_aware_list      = "Aware of HF",
#   HF_know             = "Knows beneficiary",
#   HF_applied          = "Applied to HF",
#   HF_received         = "Received grant",
#   HF_donate           = "Donated",
#   emergency_expense_num = "Emergency expense ability",
#   bill_num_rev        = "Ease of paying bills",
#   wrk_loyal_num       = "Co-worker loyalty",
#   emp_loyal_num       = "Employer loyalty",
#   emp_reco_num        = "Employer recommend",
#   union_vote          = "Vote for union",
#   rk_age              = "Age",
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
# job_attach_vars   <- c("wrk_loyal_num", "emp_loyal_num", "emp_reco_num")
# job_attach_labels <- var_labels[job_attach_vars]
# job_attach_idx_desc <- which(desc_df$Variable %in% job_attach_labels)
# 
# thd_desc_tab <- desc_df %>%
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
# # labels for job-attachment rows (already defined earlier)
# # job_attach_vars   <- c("wrk_loyal_num", "emp_loyal_num", "emp_reco_num")
# # job_attach_labels <- var_labels[job_attach_vars]
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
# thd_cor_tab <- cor_df %>%
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
# thd_cor_tab <- thd_cor_tab %>%
#   kable_styling(
#     latex_options = c("hold_position", "scale_down"),
#     font_size = 7
#   ) %>%
#   landscape()
