library(here)
source(here::here('2_code', 'general_retail', '1_libraries_and_settings_gr.R'))
here::i_am("2_code/general_retail/4_codebook_gr.R")

gr_clean <- read.csv(here("3_cleaned_data", "general_retail_clean.csv"))

# Create codebook
attr(gr_clean$main_job, "description") <- "Is your ${e://Field/EmployerName} job your main paying job?" 
attr(gr_clean$employ_period, "description") <- "How long have you worked with ${e://Field/EmployerName}?"
attr(gr_clean$manager, "description") <- "Are you a manager at ${e://Field/EmployerName}?"

ben_names <- names(select(gr_clean, vac_days_benefits:tuition_benefits))
for(i in which(colnames(gr_clean) %in% ben_names)){
  attr(gr_clean[[i]], "description") <- "Breakdown of responses to question: Employers sometimes create programs or offer benefits for their workers. Does ${e://Field/EmployerName} offer any of the following to you?"
}

attr(gr_clean$health_ins, "description") <- "Do you now have any type of health plan or medical insurance?"
attr(gr_clean$health_job, "description") <- "Do you get health coverage through your job with ${e://Field/EmployerName} or do you get it in some other way? Asked only if answer to health insurance question is positive."
attr(gr_clean$expense, "description") <- "How confident are you that you could come up with $400 if an unexpected need arose within the next month?"
attr(gr_clean$new_job, "description") <- "Taking everything into consideration, how likely is it you will make a genuine effort to find a new job within the next 3 months?"
attr(gr_clean$loyal_workers, "description") <- "How much loyalty would you say you feel toward other workers at ${e://Field/EmployerName}?"
attr(gr_clean$loyal_comp, "description") <- "How much loyalty would you say you feel toward ${e://Field/EmployerName} as a company?"
attr(gr_clean$union_elec, "description") <- "If an election were held today to decide whether employees like you should be represented by a union at ${e://Field/EmployerName}, would you vote for the union or against the union?"
attr(gr_clean$union_unsure, "description") <- "Are you leaning one way or another about voting for unionization? Asked only if Not Sure is selected in the previous question."
attr(gr_clean$union_coworkers_6, "description") <- "If an election were held today to decide whether employees like you should be represented by a union at ${e://Field/EmployerName}, what percent of your co-workers do you think would vote for the union? Please use the slider below to indicate your response."
attr(gr_clean$recommend, "description") <- "If a friend asked you about working at ${e://Field/EmployerName} doing a job like yours, how likely is it that you would recommend ${e://Field/EmployerName} as a place to work?"
attr(gr_clean$ehf_donate_exist, "description") <- "Have you ever donated to the Employee Hardship Fund?"
attr(gr_clean$ehf_donate_amount, "description") <- "For your latest donation, how much did you donate to the Employee Hardship Fund at your retail employer?"
attr(gr_clean$emergency, "description") <- "In the last 12 months, have any of the following events happened to you, causing unmanageable expenses or affecting your ability to work (see survey for specifics)?"
attr(gr_clean$app_time, "description") <- "How much time did you spend on your unemployment insurance claim, including gathering information, assembling materials, and completing the paperwork?"
attr(gr_clean$delay, "description") <- "Once you submitted your application, about how long did it take for you to receive your money?"


hire_ben_names <- names(select(gr_clean, hire_benefits_pto:hire_benefits_tuition))
for(i in which(colnames(gr_clean) %in% hire_ben_names)){
  attr(gr_clean[which(colnames(gr_clean) == "i")], "description") <- "Breakdown of responses to question: Imagine another retailer was hiring, paying the same wage you are earning at Walmart. The new job has a commute that is 10 minutes longer but has better benefits. How seriously would you consider this offer, if the better benefits were:"
}

attr(gr_clean$attention, "description") <- "Attention check question. Not everyone pays close attention in surveys. For this question, please select “A little of the time”"

govt_resp_names <- names(select(gr_clean, govt_responsib_elder:govt_responsib_hardship))
for(i in which(colnames(gr_clean) %in% govt_resp_names)){
  attr(gr_clean[[i]], "description") <- "Breakdown of responses to question: People have different views on the responsibilities of government. How much responsibility do you think the US government should have for ensuring  a reasonable standard of living for each of the following groups?"
}

attr(gr_clean$gender_3_TEXT, "description") <- "Text answers to those who responded `Prefer to self-describe:` in the gender field."
attr(gr_clean$bias, "description") <- "Do you feel like this survey was biased in any way? Please tell us!"
attr(gr_clean$IP_dupe, "description") <- "Flag for duplicate IP addresses created using the excluder package." 
attr(gr_clean$Loc_dupe, "description") <- "Flag for duplicate locations created using the excluder package.."

# Below are all variables that have undergone any data wrangling
attr(gr_clean$age, "description") <- "Recoded from birthyear by subtracting birthyear from 2025."
attr(gr_clean$quality, "description") <- "High quality respondents versus low-quality respondents. Low-quality respondents are those who either incorrectly responded to the attention check while completing the survey, or failed to accurately provide specified numerical variables (age, pph) in a sensical manner."
attr(gr_clean$age_clean, "description") <- "Age variable with non-sensical responses corrected."  
attr(gr_clean$age_corrected, "description") <- "Binary flag of whether a respondent had their age input corrected post-survey."
attr(gr_clean$pph_clean, "description") <- "Pay per hour variable with non-sensical responses corrected."  
attr(gr_clean$pph_corrected, "description") <- "Binary flag of whether a respondent had their pay per hour input corrected post-survey. Ceiling for pph was set a $40."

# raking_names <- names(select(gr_clean, rk_age:rk_age_educ))
# for(i in which(colnames(gr_clean) %in% raking_names)){
#   attr(gr_clean[[i]], "description") <- "Variables and interaction variables created for the purpose of creating raking weights. Variables with `dei` in their name are based on a Walmart publication outlining gender and age diversity in their workforce."
# }

# attr(gr_clean$rk_wgt_og, "description") <- "Raking weights based on estimated Facebook audience sizes in terms of gender, age, and education."
# attr(gr_clean$rk_wgt_trim, "description") <- "Facebook audience weights trimmed to [0.5, 2]."
# attr(gr_clean$rk_dei_og, "description") <- "Raking weights based on Walmart workforce statistics in terms of gender and age."
# attr(gr_clean$rk_wgt_trim, "description") <- "Walmart workforce weights trimmed to [0.5, 2]."

attr(gr_clean$tenure_fac, "description") <- "Factorized version of employ_period variable."
attr(gr_clean$tenure_fac_h, "description") <- "Helmert ordinal factorized version of employ_period variable."
attr(gr_clean$tenure_num, "description") <- "Numerical version of employ_period variable, bound to [0, 1]"

attr(gr_clean$num_jobs_fac, "description") <- "Factorized version of num_jobs variable."
attr(gr_clean$num_jobs_fac_h, "description") <- "Helmer ordinal factorized version of num_jobs variable."

attr(gr_clean$multiple_jobs, "description") <-  "Binary indicator based on num_jobs. TRUE values indicate that a respondent holds multiple jobs."
attr(gr_clean$fulltime, "description") <- "Binary indicator based on employ_status. TRUE values indicate that a respondent is a regular full-time employee."
attr(gr_clean$pay_type, "description") <- "Binary indicator based on employ_status. TRUE values indicate that a respondent is paid hourly."

attr(gr_clean$ehf_aware_pretr, "description") <- "Binary indicator of whether the respondent was aware of the existence of the ACNT before treatment. Based on emerg_assist_benefits variable."

attr(gr_clean$emergency_expense, "description") <- "Factorized version of expense variable."
attr(gr_clean$emergency_expense_num, "description") <- "Numerical version of expense variable, bound to [0, 1]"
attr(gr_clean$emergency_expense_bin, "description") <- "Binary version of expense variable, equal to 1 if respondent is more sure than not."

attr(gr_clean$new_job_num, "description") <- "Numerical version of new_job variable, bound to [0, 1]"
attr(gr_clean$new_job_bin, "description") <- "Binary version of new_job variable, equal to 1 if respondent is more likely than not to seek out a new job."

attr(gr_clean$wrk_loyal, "description") <- "Factorized version of loyal_workers variable."
attr(gr_clean$wrk_loyal_num, "description") <- "Numerical version of loyal_workers variable, bound to [0, 1]"
attr(gr_clean$wrk_loyal_bin, "description") <- "Binary version of loyal_workers variable, equal to 1 if respondent has some or a lot of loyalty to fellow workers."

attr(gr_clean$emp_loyal, "description") <- "Factorized version of loyal_comp variable."
attr(gr_clean$emp_loyal_num, "description") <- "Numerical version of loyal_comp variable, bound to [0, 1]"
attr(gr_clean$emp_loyal_bin, "description") <- "Binary version of loyal_comp variable, equal to 1 if respondent has some or a lot of loyalty to their company."

attr(gr_clean$union_vote, "description") <- "Factorized version of union vote questions. Respondents who responded they were unsure for union_elec are coded based on their response to union_unsure."
attr(gr_clean$union_vote_num, "description") <- "Numerical version of union_vote variable, bound to [0, 1]"
attr(gr_clean$union_vote_bin, "description") <- "Binary version of union_vote variable, equal to 1 if respondent is at least leaning towards voting in favor of unionization."

attr(gr_clean$emp_reco, "description") <- "Factorized version of recommend variable."
attr(gr_clean$emp_reco_num, "description") <- "Numerical version of recommend variable, bound to [0, 1]"
attr(gr_clean$emp_reco_bin, "description") <- "Binary version of recommend variable, equal to 1 if respondent might or certainly would recommend."

attr(gr_clean$app_time_unemp, "description") <- "Application time for unemployment benefits."
attr(gr_clean$app_time_unemp_num, "description") <- "Numerical version of app_time_unemp variable, expressed in minimum hours of stated timeframe."

attr(gr_clean$delay_unemp, "description") <- "Delay for unemployment benefits to process."
attr(gr_clean$delay_unemp_num, "description") <- "Numerical version of delay_unemp variable, expressed in minimum hours of stated timeframe."

hire_ben_names <- names(select(gr_clean, hire_benefits_pto_num:ehf_hire_bin))
for(i in which(colnames(gr_clean) %in% hire_ben_names)){
  attr(gr_clean[[i]], "description") <- "Binary indicator of responses to the question: Imagine another retailer was hiring, paying the same wage you are earning at Walmart. The new job has a commute that is 10 minutes longer but has better benefits. How seriously would you consider this offer, if the better benefits were: [listed benefit in variable name]."
}

attr(gr_clean$ideology_conlib, "description") <- "Responses to ideology question, among those who did not refuse to respond."
attr(gr_clean$ideology_conlib_num, "description") <- "Numerical version of ideology_conlib variable, bound to [0, 1] from most conservative (0) to most liberal (1)."

attr(gr_clean$voted, "description") <- "Binary variable: did the respondent vote in the 2024 presidential eleciton."

govt_num_names <- names(select(gr_clean, govt_responsib_elder_num:govt_responsib_hardship_num))
for(i in which(colnames(gr_clean) %in% govt_num_names)){
  attr(gr_clean[[i]], "description") <- "Numerical breakdown in [0, 1] of responses to question: People have different views on the responsibilities of government. How much responsibility do you think the US government should have for ensuring  a reasonable standard of living for each of the following groups?"
}

govt_bin_names <- names(select(gr_clean, govt_responsib_elder_bin:govt_responsib_hardship_bin))
for(i in which(colnames(gr_clean) %in% govt_bin_names)){
  attr(gr_clean[[i]], "description") <- "Binary version of responses to question: People have different views on the responsibilities of government. How much responsibility do you think the US government should have for ensuring  a reasonable standard of living for each of the following groups?"
}

attr(gr_clean$ehf_hire_relative, "description") <- "The difference between hire_benefits_emerg_num and the with-respondent average for all other hire_benefits variables (not including hire_benefits_emerg_num)."

attr(gr_clean$college, "description") <- "Did the respondent report education of at least completed College degree."
attr(gr_clean$male, "description") <- "Did the respondent identify as male."
attr(gr_clean$nonwhite, "description") <- "Did the respondent identify as non-white."
attr(gr_clean$home_ownership, "description") <- "Does the respondent own their home."
attr(gr_clean$healthcare, "description") <- "Does the respondent have any healthcare."

attr(gr_clean$practice_religion_num, "description") <- "Numerical version of practice_religion variable, bound to [0, 1]"
attr(gr_clean$practice_religion_bin, "description") <- "Binary version of practice_religion variable, equal to 1 if respondent practices religion at all."
attr(gr_clean$identify_religion_bin, "description") <- "Binary version of religion variable, equal to 1 if respondent identifies with any religion."

attr(gr_clean$income_num, "description") <- "Numerical version of income variable, representing the lower bound of the range of the response in thousands of USD."

# attr(gr_clean$attachment_index, "description") <- "The first principal component of the variables emp_loyal_num, wrk_loyal_num, emp_reco_num, new_job_num. Indicates attachment of respondent to their job. Explains 65% of the variance in the stated variables."

# General retail specific vars
attr(gr_clean$ehf_prior_know, "description") <- "Before taking this survey, had you ever heard of Employee Hardship Funds?"

ehf_offer_names <- names(select(gr_clean, ehf_offer_thd:ehf_offer_costco))
for(i in which(colnames(gr_clean) %in% ehf_offer_names)){
  attr(gr_clean[[i]], "description") <- "Does the respondent believe that the company offers an EHF?"
}

ehf_offer_names <- paste(ehf_offer_names, "_bin", sep = "")
for(i in which(colnames(gr_clean) %in% ehf_offer_names)){
  attr(gr_clean[[i]], "description") <- "Binary variable of whether the respondent is sure that the company in question offers an EHF."
}

attr(gr_clean$ehf_own_comp, "description") <- "Does ${e://Field/EmployerName} offer you an Employee Hardship Fund? Depending on answers, respondents are routed to questions on their company's EHF, or whether their company should create one."

attr(gr_clean$ehf_support_new, "description") <- "If ${e://Field/EmployerName} was considering starting an Employee Hardship Fund, how supportive would you be?"
attr(gr_clean$ehf_support_new_num, "description") <- "Numerical version of ehf_support_new."
attr(gr_clean$ehf_support_new_bin, "description") <- "Binary version of ehf_support_new."

attr(gr_clean$ehf_wrk_new, "description") <- "Factorized version of ehf_type_new."
attr(gr_clean$ehf_wrk_new_num, "description") <- "Numerical version of ehf_wrk_new. Larger values indicate support for an EHF that is more controlled by workers."
attr(gr_clean$ehf_wrk_new_bin, "description") <- "Binary version of ehf_wrk_new. Is equal to 1 if respondents think the EHF should be more controlled by workers, rather than management."

attr(gr_clean$ehf_donate_new, "description") <- "If ${e://Field/EmployerName} created an Employee Hardship Fund, how much would be willing to donate?  Making a donation does not guarantee support if you experience an emergency."

attr(gr_clean$ehf_support_exist, "description") <- "How supportive are you of your retail employer's Employee Hardship Fund?"
attr(gr_clean$ehf_support_exist_num, "description") <- "Numerical version of ehf_support_exist."
attr(gr_clean$ehf_support_exist_bin, "description") <- "Binary version of ehf_support_exist."

attr(gr_clean$ehf_wrk_exist, "description") <- "Factorized version of ehf_type_exist."
attr(gr_clean$ehf_wrk_exist_num, "description") <- "Numerical version of ehf_wrk_exist. Larger values indicate EHF is more controlled by workers."
attr(gr_clean$ehf_wrk_exist_bin, "description") <- "Binary version of ehf_wrk_exist. Is equal to 1 if EHF is more controlled by workers, rather than management."

attr(gr_clean$acs_weight, "description") <- "Weights calculated as the PUMS weighted proportion of ACS respondents over the proportion of respondents in the survey, using education, gender, race, and age bins."
attr(gr_clean$acs_weight_trim, "description") <- "Weights trimmed to [0.4, 2.5]."

attr(gr_clean$ehf_offer_ans, "description") <- "Number of companies for which respondent had an opinion as to whether they offer an EHF."
attr(gr_clean$ehf_offer_crct, "description") <- "Number of companies for which respondent correctly identified whether they offer an EHF."
attr(gr_clean$ehf_offer_percent, "description") <- "Percentage of correct responses over all complete (non-DK) responses to whether a company offers an EHF."


output_codebook <- codebook(
  df = gr_clean,
  title = "EHF Project: General Retail Survey",
  subtitle = "Ahlquist, John S.",
  description = "Cleaned survey data relating to the General Retail EHF study. Compiled by Theodoros Ntounias (RA)."
)

print(output_codebook, "general_retail_EHF_codebook.docx")
