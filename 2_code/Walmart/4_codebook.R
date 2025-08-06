library(here)
source(here::here('2_code', 'Walmart', '1_libraries_and_settings_ACNT.R'))
here::i_am("2_code/Walmart/4_codebook.R")

ACNT_clean <- read_csv(here("3_cleaned_data", "ACNT_clean.csv"))

# Create codebook
attr(ACNT_clean$past_work, "description") <- "Have you worked for ${e://Field/EmployerName} as an employee, contractor, or other associate at any time in the last 12 months?"
attr(ACNT_clean$current_work, "description") <- "Have you worked for ${e://Field/EmployerName} as an employee, contractor, or other associate at any time in the last 12 months?"
attr(ACNT_clean$main_job, "description") <- "Is your ${e://Field/EmployerName} job your main paying job?" 
attr(ACNT_clean$employ_period, "description") <- "How long have you worked with ${e://Field/EmployerName}?"
attr(ACNT_clean$manager, "description") <- "Are you a manager at ${e://Field/EmployerName}?"

ben_names <- names(select(ACNT_clean, vac_days_benefits:tuition_benefits))
for(i in which(colnames(ACNT_clean) %in% ben_names)){
  attr(ACNT_clean[[i]], "description") <- "Breakdown of responses to question: Employers sometimes create programs or offer benefits for their workers. Does ${e://Field/EmployerName} offer any of the following to you?"
}

attr(ACNT_clean$health_ins, "description") <- "Do you now have any type of health plan or medical insurance?"
attr(ACNT_clean$health_job, "description") <- "Do you get health coverage through your job with ${e://Field/EmployerName} or do you get it in some other way? Asked only if answer to health insurance question is positive."
attr(ACNT_clean$expense, "description") <- "How confident are you that you could come up with $400 if an unexpected need arose within the next month?"
attr(ACNT_clean$new_job, "description") <- "Taking everything into consideration, how likely is it you will make a genuine effort to find a new job within the next 3 months?"
attr(ACNT_clean$loyal_workers, "description") <- "How much loyalty would you say you feel toward other workers at ${e://Field/EmployerName}?"
attr(ACNT_clean$loyal_comp, "description") <- "How much loyalty would you say you feel toward ${e://Field/EmployerName} as a company?"
attr(ACNT_clean$union_elec, "description") <- "If an election were held today to decide whether employees like you should be represented by a union at ${e://Field/EmployerName}, would you vote for the union or against the union?"
attr(ACNT_clean$union_unsure, "description") <- "Are you leaning one way or another about voting for unionization? Asked only if Not Sure is selected in the previous question."
attr(ACNT_clean$union_coworkers_6, "description") <- "If an election were held today to decide whether employees like you should be represented by a union at ${e://Field/EmployerName}, what percent of your co-workers do you think would vote for the union? Please use the slider below to indicate your response."
attr(ACNT_clean$recommend, "description") <- "If a friend asked you about working at ${e://Field/EmployerName} doing a job like yours, how likely is it that you would recommend ${e://Field/EmployerName} as a place to work?"
attr(ACNT_clean$ehf_exist, "description") <- "Some companies have a program that accepts donations from employees and then uses those donations to provide emergency cash to help their workers who are facing hard times. Does ${e://Field/EmployerName} offer a program like this? If No is selected, all questions directly pertaining to the EHF (received funds, donated, etc) are skipped."
attr(ACNT_clean$ehf_other_recip, "description") <- "Do you know someone personally who has received money from ${e://Field/EHFNameAbbr}?"
attr(ACNT_clean$ehf_applied, "description") <- "Have you ever applied to the ${e://Field/EHFNameAbbr} for financial assistance? Questions relating to the application process are skipped if respondent selects No."
attr(ACNT_clean$ehf_reason, "description") <- "Why did you apply to the ${e://Field/EHFNameAbbr} for assistance? If you have applied more than once, think only of the most recent time. "
attr(ACNT_clean$ehf_permission, "description") <- "Did you need permission or support from a manager or other ${e://Field/EmployerName} employee in order to apply for help from the ${e://Field/EHFNameAbbr}?"
attr(ACNT_clean$ehf_time, "description") <- "About how much time did you spend completing the paperwork for your application to the ${e://Field/EHFNameAbbr}, including the time it took to assemble the required paperwork?"
attr(ACNT_clean$emergency_aid, "description") <- "During this financial emergency, what other sources of material support did you turn to? Please check all that apply."
attr(ACNT_clean$ehf_received, "description") <- "Have you ever received financial assistance from the ${e://Field/EHFNameAbbr}?"
attr(ACNT_clean$ehf_rec_amount, "description") <- "To the nearest $100, how much did you receive in financial assistance from the ${e://Field/EHFNameAbbr}? If you have received money more than once, think only of the most recent time."
attr(ACNT_clean$ehf_coverage, "description") <- "About how much of your emergency financial need did this assistance cover? "
attr(ACNT_clean$ehf_delay, "description") <- "Once you submitted your application, about how long did it take for you to receive your money?"
attr(ACNT_clean$ehf_followup, "description") <- "Did anyone from the ${e://Field/EHFNameAbbr} follow up with you later?"
attr(ACNT_clean$ehf_donation, "description") <- "Have you ever made a donation to the ${e://Field/EHFNameAbbr}? Following question skipped if No is selected."
attr(ACNT_clean$ehf_don_amount, "description") <- "To the nearest $100, how much did you donate to the ${e://Field/EHFNameAbbr}? If you have donated money more than once, think only of the most recent time."
attr(ACNT_clean$ehf_prior_know, "description") <- "Before taking this survey, had you ever heard of the ${e://Field/EHFNameAbbr}?"
attr(ACNT_clean$emergency, "description") <- "In the last 12 months, have any of the following events happened to you, causing unmanageable expenses or affecting your ability to work (see survey for specifics)?"
attr(ACNT_clean$app_time, "description") <- "How much time did you spend on your unemployment insurance claim, including gathering information, assembling materials, and completing the paperwork?"
attr(ACNT_clean$delay, "description") <- "Once you submitted your application, about how long did it take for you to receive your money?"


hire_ben_names <- names(select(ACNT_clean, hire_benefits_pto:hire_benefits_tuition))
for(i in which(colnames(ACNT_clean) %in% hire_ben_names)){
  attr(ACNT_clean[which(colnames(ACNT_clean) == "i")], "description") <- "Breakdown of responses to question: Imagine another retailer was hiring, paying the same wage you are earning at Walmart. The new job has a commute that is 10 minutes longer but has better benefits. How seriously would you consider this offer, if the better benefits were:"
}

attr(ACNT_clean$vote_lik_post_elec, "description") <- "Question assessing whether individuals voted in the 2024 US presidential election. Due to the survey overlapping with election day, this question was edited mid-survey. This led to some non-sensical values, which are interpreted in further variables."
attr(ACNT_clean$walpac, "description") <- "In the last 12 months, have you made a donation to Walmart's political action committee, also known as WALPAC?"
attr(ACNT_clean$attention, "description") <- "Attention check question. Not everyone pays close attention in surveys. For this question, please select “A little of the time”"
attr(ACNT_clean$`vote_pres_post_elec _3_TEXT`, "description") <- "Text response for those responding `Other` in the previous question." 

govt_resp_names <- names(select(ACNT_clean, govt_responsib_elder:govt_responsib_hardship))
for(i in which(colnames(ACNT_clean) %in% govt_resp_names)){
  attr(ACNT_clean[[i]], "description") <- "Breakdown of responses to question: People have different views on the responsibilities of government. How much responsibility do you think the US government should have for ensuring  a reasonable standard of living for each of the following groups?"
}

attr(ACNT_clean$gender_3_TEXT, "description") <- "Text answers to those who responded `Prefer to self-describe:` in the gender field."
attr(ACNT_clean$bias, "description") <- "Do you feel like this survey was biased in any way? Please tell us!"
attr(ACNT_clean$donate, "description") <- "Would you like to learn how to make a donation to the Walmart ACNT Together fund for Walmart employees? Respondents who answered affirmatively were redirected to a donation page."
attr(ACNT_clean$utm_medium, "description") <- "Medium through which respondents were exposed to Facebook advertisement of survey."
attr(ACNT_clean$utm_campaign, "description") <- "Which ad set was the respondent exposed to."
attr(ACNT_clean$IP_dupe, "description") <- "Flag for duplicate IP addresses created using the excluder package." 
attr(ACNT_clean$Loc_dupe, "description") <- "Flag for duplicate locations created using the excluder package.."

# Below are all variables that have undergone any data wrangling
attr(ACNT_clean$treatment_full, "description") <- "Treatment categories. Control (ctrl), control video unrelated to an EHF (vid0), charity treatment video (vidChar), solidarity treatment video (vidSolid)."
attr(ACNT_clean$treatment_placebo, "description") <- "Treatment categories. Control (control), control video unrelated to an EHF (placebo), charity treatment video and solidarity treatment video (treatment)."
attr(ACNT_clean$treatment_bin, "description") <- "Treatment (ctrl, vid0) or no treatment (vidChar, vidSolid) binary."
attr(ACNT_clean$treatment_vid, "description") <- "Whether the respondent was exposed to a video (vid0, vidChar, vidSolid) or not (ctrl)."
attr(ACNT_clean$treatment_framing, "description") <- "Whether the respondent was exposed to solidarity video (solidarity), charity video (charity), or cntrl/placebo (cntrl)."
attr(ACNT_clean$completion_subgroup, "description") <- "Whether the respondent was in the subgroup that completed the survey without providing an email (5), or the subgroup that completed and provided an email (6). Groups 1-4 are excluded from the survey sample, for details please refer to the Pre-Analysis Plan."
attr(ACNT_clean$age, "description") <- "Recoded from birthyear by subtracting birthyear from 2025."
attr(ACNT_clean$quality, "description") <- "High quality respondents versus low-quality respondents. Low-quality respondents are those who either incorrectly responded to the attention check while completing the survey, or failed to accurately provide specified numerical variables (age, pph) in a sensical manner."
attr(ACNT_clean$age_clean, "description") <- "Age variable with non-sensical responses corrected."  
attr(ACNT_clean$age_corrected, "description") <- "Binary flag of whether a respondent had their age input corrected post-survey."
attr(ACNT_clean$pph_clean, "description") <- "Pay per hour variable with non-sensical responses corrected."  
attr(ACNT_clean$pph_corrected, "description") <- "Binary flag of whether a respondent had their pay per hour input corrected post-survey. Ceiling for pph was set a $40."

raking_names <- names(select(ACNT_clean, rk_age:rk_age_educ))
for(i in which(colnames(ACNT_clean) %in% raking_names)){
  attr(ACNT_clean[[i]], "description") <- "Variables and interaction variables created for the purpose of creating raking weights. Variables with `dei` in their name are based on a Walmart publication outlining gender and age diversity in their workforce."
}

attr(ACNT_clean$rk_wgt_og, "description") <- "Raking weights based on estimated Facebook audience sizes in terms of gender, age, and education."
attr(ACNT_clean$rk_wgt_trim, "description") <- "Facebook audience weights trimmed to [0.5, 2]."
attr(ACNT_clean$rk_dei_og, "description") <- "Raking weights based on Walmart workforce statistics in terms of gender and age."
attr(ACNT_clean$rk_wgt_trim, "description") <- "Walmart workforce weights trimmed to [0.5, 2]."

attr(ACNT_clean$tenure_fac, "description") <- "Factorized version of employ_period variable."
attr(ACNT_clean$tenure_fac_h, "description") <- "Helmert ordinal factorized version of employ_period variable."
attr(ACNT_clean$tenure_num, "description") <- "Numerical version of employ_period variable, bound to [0, 1]"

attr(ACNT_clean$num_jobs_fac, "description") <- "Factorized version of num_jobs variable."
attr(ACNT_clean$num_jobs_fac_h, "description") <- "Helmer ordinal factorized version of num_jobs variable."

attr(ACNT_clean$multiple_jobs, "description") <-  "Binary indicator based on num_jobs. TRUE values indicate that a respondent holds multiple jobs."
attr(ACNT_clean$fulltime, "description") <- "Binary indicator based on employ_status. TRUE values indicate that a respondent is a regular full-time employee."
attr(ACNT_clean$pay_type, "description") <- "Binary indicator based on employ_status. TRUE values indicate that a respondent is paid hourly."

attr(ACNT_clean$ehf_aware_pretr, "description") <- "Binary indicator of whether the respondent was aware of the existence of the ACNT before treatment. Based on emerg_assist_benefits variable."

attr(ACNT_clean$emergency_expense, "description") <- "Factorized version of expense variable."
attr(ACNT_clean$emergency_expense_num, "description") <- "Numerical version of expense variable, bound to [0, 1]"
attr(ACNT_clean$emergency_expense_bin, "description") <- "Binary version of expense variable, equal to 1 if respondent is more sure than not."

attr(ACNT_clean$new_job_num, "description") <- "Numerical version of new_job variable, bound to [0, 1]"
attr(ACNT_clean$new_job_bin, "description") <- "Binary version of new_job variable, equal to 1 if respondent is more likely than not to seek out a new job."

attr(ACNT_clean$wrk_loyal, "description") <- "Factorized version of loyal_workers variable."
attr(ACNT_clean$wrk_loyal_num, "description") <- "Numerical version of loyal_workers variable, bound to [0, 1]"
attr(ACNT_clean$wrk_loyal_bin, "description") <- "Binary version of loyal_workers variable, equal to 1 if respondent has some or a lot of loyalty to fellow workers."

attr(ACNT_clean$emp_loyal, "description") <- "Factorized version of loyal_comp variable."
attr(ACNT_clean$emp_loyal_num, "description") <- "Numerical version of loyal_comp variable, bound to [0, 1]"
attr(ACNT_clean$emp_loyal_bin, "description") <- "Binary version of loyal_comp variable, equal to 1 if respondent has some or a lot of loyalty to their company."

attr(ACNT_clean$union_vote, "description") <- "Factorized version of union vote questions. Respondents who responded they were unsure for union_elec are coded based on their response to union_unsure."
attr(ACNT_clean$union_vote_num, "description") <- "Numerical version of union_vote variable, bound to [0, 1]"
attr(ACNT_clean$union_vote_bin, "description") <- "Binary version of union_vote variable, equal to 1 if respondent is at least leaning towards voting in favor of unionization."

attr(ACNT_clean$emp_reco, "description") <- "Factorized version of recommend variable."
attr(ACNT_clean$emp_reco_num, "description") <- "Numerical version of recommend variable, bound to [0, 1]"
attr(ACNT_clean$emp_reco_bin, "description") <- "Binary version of recommend variable, equal to 1 if respondent might or certainly would recommend."

attr(ACNT_clean$ehf_time_num, "description") <- "Numeric version of the ehf_time variable."

emerg_names <- names(select(ACNT_clean, emergency_bank:emergency_other))
for(i in which(colnames(ACNT_clean) %in% emerg_names)){
  attr(ACNT_clean[[i]], "description") <- "Binary indicator of whether the respondent experienced the listed emergency."
}

attr(ACNT_clean$emergency_sum, "description") <- "The number of concurrent emergencies a respondent experienced."

attr(ACNT_clean$ehf_coverage_num, "description") <- "Numerical version of ehf_coverage variable, bound to [0, 1]"
attr(ACNT_clean$ehf_coverage_bin, "description") <- "Binary version of ehf_coverage variable, equal to 1 if EHF funds covered at least half of respondent's emergency needs."

attr(ACNT_clean$ehf_delay_num, "description") <- "Numerical version of ehf_delay variable, bound to [0, 1]"

attr(ACNT_clean$app_time_unemp, "description") <- "Application time for unemployment benefits."
attr(ACNT_clean$app_time_unemp_num, "description") <- "Numerical version of app_time_unemp variable, expressed in minimum hours of stated timeframe."

attr(ACNT_clean$delay_unemp, "description") <- "Delay for unemployment benefits to process."
attr(ACNT_clean$delay_unemp_num, "description") <- "Numerical version of delay_unemp variable, expressed in minimum hours of stated timeframe."

hire_ben_names <- names(select(ACNT_clean, hire_benefits_pto_num:ehf_hire_bin))
for(i in which(colnames(ACNT_clean) %in% hire_ben_names)){
  attr(ACNT_clean[[i]], "description") <- "Binary indicator of responses to the question: Imagine another retailer was hiring, paying the same wage you are earning at Walmart. The new job has a commute that is 10 minutes longer but has better benefits. How seriously would you consider this offer, if the better benefits were: [listed benefit in variable name]."
}

attr(ACNT_clean$ehf_hire_relative, "description") <- "The difference between hire_benefits_emerg_num and the with-respondent average for all other hire_benefits variables (not including hire_benefits_emerg_num)."

attr(ACNT_clean$ideology_conlib, "description") <- "Responses to ideology question, among those who did not refuse to respond."
attr(ACNT_clean$ideology_conlib_num, "description") <- "Numerical version of ideology_conlib variable, bound to [0, 1] from most conservative (0) to most liberal (1)."

attr(ACNT_clean$vote_lik_fix_flag, "description") <- "Flag indicating whether the observed response to turnout was recoded to compensate for the 2025 Presidential election occuring during the survey."
attr(ACNT_clean$vote_lik_fix, "description") <- "`Fixed` version of turnour. If respondents prior to the election selected a candidate, they are classified as `I'm sure I voted`; if not, they are classified as `I did not vote`."
attr(ACNT_clean$voted, "description") <- "Binary version of vote_lik_fix variable, equal to 1 if respondent is sure they voted."

govt_num_names <- names(select(ACNT_clean, govt_responsib_elder_num:govt_responsib_hardship_num))
for(i in which(colnames(ACNT_clean) %in% govt_num_names)){
  attr(ACNT_clean[[i]], "description") <- "Numerical breakdown in [0, 1] of responses to question: People have different views on the responsibilities of government. How much responsibility do you think the US government should have for ensuring  a reasonable standard of living for each of the following groups?"
}

govt_bin_names <- names(select(ACNT_clean, govt_responsib_elder_bin:govt_responsib_hardship_bin))
for(i in which(colnames(ACNT_clean) %in% govt_bin_names)){
  attr(ACNT_clean[[i]], "description") <- "Binary version of responses to question: People have different views on the responsibilities of government. How much responsibility do you think the US government should have for ensuring  a reasonable standard of living for each of the following groups?"
}

attr(ACNT_clean$college, "description") <- "Did the respondent report education of at least completed College degree."
attr(ACNT_clean$male, "description") <- "Did the respondent identify as male."
attr(ACNT_clean$nonwhite, "description") <- "Did the respondent identify as non-white."
attr(ACNT_clean$home_ownership, "description") <- "Does the respondent own their home."
attr(ACNT_clean$healthcare, "description") <- "Does the respondent have any healthcare."

attr(ACNT_clean$practice_religion_num, "description") <- "Numerical version of practice_religion variable, bound to [0, 1]"
attr(ACNT_clean$practice_religion_bin, "description") <- "Binary version of practice_religion variable, equal to 1 if respondent practices religion at all."
attr(ACNT_clean$identify_religion_bin, "description") <- "Binary version of religion variable, equal to 1 if respondent identifies with any religion."

attr(ACNT_clean$income_num, "description") <- "Numerical version of income variable, representing the lower bound of the range of the response in thousands of USD."

attr(ACNT_clean$attachment_index, "description") <- "The first principal component of the variables emp_loyal_num, wrk_loyal_num, emp_reco_num, new_job_num. Indicates attachment of respondent to their job. Explains 65% of the variance in the stated variables."

fup_names <- names(select(ACNT_clean, gender_f:emp_reco_bin_f))
for(i in which(colnames(ACNT_clean) %in% fup_names)){
  attr(ACNT_clean[[i]], "description") <- "All variables with a `_f` suffix refer to the follow-up survey sent to respondents approximately a week after completion of the initial survey. Question codings are identical to those for the main survey. NA values indicate that the respondent did not complete the follow-up survey." 
}

attr(ACNT_clean$ehf_received_all, "description") <- "TRUE for everyone who responded YES to ehf_received, FALSE for everyone who did not answer NO to ehf_exist and did not answer YES to ehf_received, and NA for people who answered NO to ehf_exist"

attr(ACNT_clean$WG_TANF, "source") <- "Fox, A.M., Feng, W., Reynolds, M. (2022). The Effect of Administrative Burden on State Safety-Net Participation: Evidence from food assistance, cash assistance and Medicaid, Public Administration Review"
attr(ACNT_clean$WG_TANF, "description") <- "TANF Generosity Index, for 2016 which is the last year covered by the dataset. For more information, please see the original paper."


attr(ACNT_clean$replacement_2024_ui, "description") <- "Replacement rate (proportion of workers wages replaced by unemployment insurance benefits is referred to as the replace rate) from the US Department of Labor. Latest year covered (2011)."
attr(ACNT_clean$recipiency_2024_ui, "description") <- "Recipiency rate for Unemployment insurance, from the US Department of Labor. Latest year prior to survey covered (2024)."


output_codebook <- codebookr::codebook(
  df = ACNT_clean,
  title = "EHF Project: Walmart Survey",
  subtitle = "Ahlquist, John S.",
  description = "Cleaned survey data relating to the Walmart Associates in Critical Need Fund EHF study. Compiled by Theodoros Ntounias (RA)."
)

print(output_codebook, "Walmart_EHF_codebook.docx")
