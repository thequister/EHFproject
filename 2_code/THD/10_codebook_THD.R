library(here)
source(here::here('2_code', '1_libraries_and_settings_global.R'))
source(here::here('2_code', 'THD', '2_data_format.R'))
here::i_am("2_code/THD/10_codebook_THD.R")

THD_clean <- THD_comp_uw

# Create codebook
attr(THD_clean$Q2.2, "description") <- "Do you currently work for ${e://Field/EmployerName}?"
attr(THD_clean$Q2.3, "description") <- "Why did you stop working for ${e://Field/EmployerName}?"
attr(THD_clean$Q2.5, "description") <- "Worksite location."
attr(THD_clean$Q2.6, "description") <- "Residence location."
attr(THD_clean$Q2.7, "description") <- "Is your job at ${e://Field/EmployerName} your main paying job?"
attr(THD_clean$Q2.8, "description") <- "How many paying jobs do you have now, including your main job, night jobs, and part-time work?"
attr(THD_clean$Q2.9, "description") <- "How long have you worked for ${e://Field/EmployerName}?"
attr(THD_clean$Q2.10, "description") <- "In your job with ${e://Field/EmployerName}, which of the following best describes your employment arrangement?"
attr(THD_clean$Q2.11, "description") <- "Are you a manager at ${e://Field/EmployerName}?"
attr(THD_clean$Q2.12, "description") <- "Do you belong to a labor union at ${e://Field/EmployerName}?"
attr(THD_clean$Q2.13, "description") <- "Are you paid by the hour at ${e://Field/EmployerName}?"
attr(THD_clean$Q2.14, "description") <- "How much are you paid per hour at ${e://Field/EmployerName}, before any tips or overtime? Please enter dollars per hour."
attr(THD_clean$Q2.15, "description") <- "Please look at the following list of benefits that employers sometimes make available to their employees. Which of the benefits on this list are offered to workers like you at ${e://Field/EmployerName}? Please select all that apply."
attr(THD_clean$Q2.16, "description") <- "Do you now have any type of health plan or medical insurance?"
attr(THD_clean$Q2.17, "description") <- "Do you get health coverage through your job with ${e://Field/EmployerName} or do you get it in some other way?"

attr(THD_clean$Q3.4, "description") <- "How confident are you that you could come up with $400 if an unexpected need arose within the next month?"
attr(THD_clean$Q3.5, "description") <- "How much loyalty would you say you feel toward other workers at ${e://Field/EmployerName}?"
attr(THD_clean$Q3.6, "description") <- "How much loyalty would you say you feel toward ${e://Field/EmployerName} as a company?"
attr(THD_clean$Q3.7, "description") <- "If an election were held today to decide whether employees like you should be represented by a union at ${e://Field/EmployerName}, would you vote for the union or against the union?"
attr(THD_clean$Q3.8, "description") <- "If a friend asked you about working at ${e://Field/EmployerName} doing a job like yours, how likely is it that you would recommend ${e://Field/EmployerName} as a place to work?"
attr(THD_clean$Q3.10, "description") <- "Do you personally know someone who has received money from ${e://Field/EHFName}?"
attr(THD_clean$EHF_aware_simp, "description") <- "Answer to Q3.10, binary."
attr(THD_clean$Q3.12, "description") <- "Do you personally know someone who has received money from ${e://Field/EHFName}?"
attr(THD_clean$HF_know, "description") <- "Factor version of Q3.12"
attr(THD_clean$Q3.13, "description") <- "Have you ever applied to ${e://Field/EHFName} for financial assistance?"
attr(THD_clean$HF_applied, "description") <- "Factor version of Q3.13"
attr(THD_clean$Q3.14, "description") <- "Why did you apply to ${e://Field/EHFName} for assistance? If you have applied more than once, think only of the most recent time. "
attr(THD_clean$Q3.15, "description") <- "Was this death, illness, injury, or lost job related to COVID-19, also known as the coronavirus?"
attr(THD_clean$Q3.16, "description") <- "Did you need permission or support from a manager or other ${e://Field/EmployerName} employee in order to apply for help from ${e://Field/EHFName}?"
attr(THD_clean$HF_mgr, "description") <- "Factor version of Q3.16"
attr(THD_clean$Q3.17, "description") <- "How much time did you spend completing the paperwork for your application to ${e://Field/EHFName}?"
attr(THD_clean$Q3.18, "description") <- "During this emergency, what other sources of material support did you use?"
attr(THD_clean$Q3.19, "description") <- "Have you ever received financial assistance from ${e://Field/EHFName}?"
attr(THD_clean$HF_received, "description") <- "Factor version of Q3.19"
attr(THD_clean$Q3.20, "description") <- "To the nearest $100, how much did you receive in financial assistance from ${e://Field/EHFName}? If you have received money more than once, think only of the most recent time."
attr(THD_clean$Q3.21, "description") <- "About how much of your emergency financial need did this assistance cover?"
attr(THD_clean$Q3.22, "description") <- "Once you submitted your application, about how long did it take for you to receive your money?"
attr(THD_clean$Q3.23, "description") <- "Did anyone from ${e://Field/EHFName} follow up with you later?"
attr(THD_clean$Q3.24, "description") <- "Have you ever made a donation to ${e://Field/EHFName}?"
attr(THD_clean$HF_donate, "description") <- "Factor version of Q3.24"
attr(THD_clean$Q3.25, "description") <- "Thinking about other workers like you at ${e://Field/EmployerName}, about what percent do you think will donate to ${e://Field/EHFName} in 2022?"

attr(THD_clean$Q4.2, "description") <- "In the last 12 months, have any of the following events happened to you, causing unmanageable expenses or affecting your ability to work?
- Victim of a natural disaster (tornado, flood, earthquake, hurricane, etc.)
- Victim of a house fire
- Victim of a violent crime or domestic abuse
- Illness, injury, or death in your immediate family
- lost job in your immediate family"
attr(THD_clean$Q4.5, "description") <- "In the past 12 months, did you ever work at ${e://Field/EmployerName} even though you were feeling sick?"
attr(THD_clean$Q4.6, "description") <- "Do you know someone personally who has died due to complications related to COVID-19?"
attr(THD_clean$Q4.7, "description") <- "In the last 12 months have you or someone in your household received unemployment insurance payments?"
attr(THD_clean$Q4.8, "description") <- "How much time did you spend on your unemployment insurance claim, including gathering information, assembling materials, and completing the paperwork?"
attr(THD_clean$Q4.9, "description") <- "Once you submitted your application, about how long did it take for you to receive your money?"
attr(THD_clean$Q4.10, "description") <- "In the past 12 months, have you or someone in your household received help from any of the following programs?  

• Supplemental Nutrition Assistance Program (SNAP), sometimes called “food stamps.”
• Temporary Assistance for Needy Families (TANF), sometimes called “welfare”.
• Supplemental Security Income (SSI), sometimes called “disability”."

attr(THD_clean$Q5.2, "description") <- "Are you registered to vote in ${q://QID21/ChoiceGroup/SelectedChoices}?"
attr(THD_clean$Q5.3, "description") <- "When it comes to politics do you usually think of yourself as extremely liberal, liberal, slightly liberal, moderate or middle of the road, slightly conservative, extremely conservative, or haven't you thought much about this?"
attr(THD_clean$Q5.4, "description") <- "Generally speaking, do you think of yourself as a Republican, Democrat, Independent, or what?"
attr(THD_clean$Q5.5, "description") <- "In the 2020 Presidential election between Democrat Joe Biden and Republican Donald Trump, did things come up that kept you from voting or did you vote?"
attr(THD_clean$Q5.6, "description") <- "For whom did you vote for President in 2020?"
attr(THD_clean$Q5.9, "description") <- "Is ${e://Field/EHFName} the name of ${e://Field/EmployerName}’s program to provide emergency cash to employees in need?"
attr(THD_clean$Q5.10, "description") <- "Not everyone pays close attention in surveys. For this question, please select “A little of the time”"
attr(THD_clean$Q5.11, "description") <- "Is your overall opinion of labor unions very favorable, mostly favorable, mostly unfavorable, or very unfavorable?"
attr(THD_clean$Q5.12, "description") <- "How about business corporations? Is your overall opinion of business corporations very favorable, mostly favorable, mostly unfavorable, or very unfavorable?"
attr(THD_clean$Q5.15, "description") <- " Do you strongly favor, somewhat favor, somewhat oppose, or strongly oppose the Coronavirus Relief Act?"
attr(THD_clean$Q5.16, "description") <- " Do you strongly favor, somewhat favor, somewhat oppose, or strongly oppose the American Rescue Plan?"
attr(THD_clean$Q5.17, "description") <- "Companies in some industries such as airlines have received direct financial support from the federal government during the coronavirus pandemic. Do you strongly favor, somewhat favor, somewhat oppose, or strongly oppose extending this support- as part of the Coronavirus Relief Act?"
attr(THD_clean$Q5.18, "description") <- "Companies in some industries such as airlines have received direct financial support from the federal government during the coronavirus pandemic. Do you strongly favor, somewhat favor, somewhat oppose, or strongly oppose extending this support- as part of the American Rescue Plan?"

attr(THD_clean$Q6.2, "description") <- "Gender"
attr(THD_clean$Q6.3, "description") <- "How would you describe your race or ethnicity? Please mark all that apply."
attr(THD_clean$Q6.4, "description") <- "In what year were you born? "
attr(THD_clean$Q6.5, "description") <- "What is the highest grade of school you completed?"
attr(THD_clean$Q6.6, "description") <- "Aside from weddings and funerals, how often do you attend religious services?"
attr(THD_clean$Q6.7, "description") <- "What is your present religion, if any?"
attr(THD_clean$Q6.8, "description") <- "Are you living with a spouse or partner?"
attr(THD_clean$Q6.9, "description") <- "Is your spouse or partner employed?"
attr(THD_clean$Q6.10, "description") <- "How many children 5 years old or under live in your home?"
attr(THD_clean$Q6.11, "description") <- "How many children between the ages of 6 and 18 live in your home?"
attr(THD_clean$Q6.12, "description") <- "How would you describe the place where you live?"
attr(THD_clean$Q6.13, "description") <- "Do you own or rent your home?"
attr(THD_clean$Q6.14, "description") <- "What is your household's approximate annual income, including earnings from all jobs and any other sources of income? Would you say it is:."
attr(THD_clean$Q6.15, "description") <- "In a typical month, how difficult is it for you to cover your expenses and pay all your bills?  "
attr(THD_clean$bills, "description") <- "Answer to Q6.15, binary"
attr(THD_clean$Q6.16, "description") <- "During 2020 have you or someone in your household received disability insurance payments?"

attr(THD_clean$healthcare, "description") <- "Do you now have any type of health plan or medical insurance?"

attr(THD_clean$IP_dupe, "description") <- "Flag for duplicate IP addresses created using the excluder package." 
attr(THD_clean$Loc_dupe, "description") <- "Flag for duplicate locations created using the excluder package.."

# Below are all variables that have undergone any data wrangling
attr(THD_clean$HDTreatment, "description") <- "Treatment categories."
attr(THD_clean$COVIDTreatment, "description") <- "Treatment categories for Covid treatment."
attr(THD_clean$completion_subgroup, "description") <- "Whether the respondent was in the subgroup that completed the survey without providing an email (5), or the subgroup that completed and provided an email (6). Groups 1-4 are excluded from the survey sample, for details please refer to the Pre-Analysis Plan."
attr(THD_clean$age, "description") <- "Recoded from birthyear by subtracting birthyear from 2025."
attr(THD_clean$EHF_aware_list, "description") <- "Pre-treatment of EHF awareness, measured from Q2.15."


raking_names <- names(select(THD_clean, rk_age:rk_gender_age))
for(i in which(colnames(THD_clean) %in% raking_names)){
  attr(THD_clean[[i]], "description") <- "Variables and interaction variables created for the purpose of creating raking weights. Variables with `dei` in their name are based on a Walmart publication outlining gender and age diversity in their workforce."
}

attr(THD_clean$rk_wgt, "description") <- "Raking weights based on estimated Facebook audience sizes in terms of gender and age."

attr(THD_clean$tenure_fac, "description") <- "Factorized version of employ_period variable."
attr(THD_clean$tenure_fac_h, "description") <- "Helmert ordinal factorized version of employ_period variable."
attr(THD_clean$tenure_num, "description") <- "Numerical version of employ_period variable, bound to [0, 1]"

attr(THD_clean$fulltime, "description") <- "Binary indicator based on employ_status. TRUE values indicate that a respondent is a regular full-time employee."

attr(THD_clean$emergency_expense, "description") <- "Factorized version of expense variable."
attr(THD_clean$emergency_expense_num, "description") <- "Numerical version of expense variable, bound to [0, 1]"
attr(THD_clean$emergency_expense_bin, "description") <- "Binary version of expense variable, equal to 1 if respondent is more sure than not."

attr(THD_clean$wrk_loyal, "description") <- "Factorized version of loyal_workers variable."
attr(THD_clean$wrk_loyal_num, "description") <- "Numerical version of loyal_workers variable, bound to [0, 1]"

attr(THD_clean$emp_loyal, "description") <- "Factorized version of loyal_comp variable."
attr(THD_clean$emp_loyal_num, "description") <- "Numerical version of loyal_comp variable, bound to [0, 1]"

attr(THD_clean$union_vote, "description") <- "Factorized version of union vote questions. Respondents who responded they were unsure for union_elec are coded based on their response to union_unsure."

attr(THD_clean$emp_reco, "description") <- "Factorized version of recommend variable."
attr(THD_clean$emp_reco_num, "description") <- "Numerical version of recommend variable, bound to [0, 1]"

attr(THD_clean$college, "description") <- "Did the respondent report education of at least completed College degree."
attr(THD_clean$male, "description") <- "Did the respondent identify as male."
attr(THD_clean$nonwhite, "description") <- "Did the respondent identify as non-white."
attr(THD_clean$home_ownership, "description") <- "Does the respondent own their home."
attr(THD_clean$healthcare, "description") <- "Does the respondent have any healthcare."
attr(THD_clean$ed, "description") <- "Factorized version of education variable."
attr(THD_clean$ed_h, "description") <- "Helmert ordered version of education variable."

attr(THD_clean$gov_pension, "description") <- "Government responsibility towards pensioners."
attr(THD_clean$gov_pension_num, "description") <- "Numeric version of gov_pension."
attr(THD_clean$gov_ui, "description") <- "Government responsibility towards the unemployed."
attr(THD_clean$gov_ui_num, "description") <- "Numeric version of gov_ui."
attr(THD_clean$gov_childcare, "description") <- "Government responsibility to childcare."
attr(THD_clean$gov_childcare_num, "description") <- "Numeric version of gov_childcare."

attr(THD_clean$attachment_index, "description") <- "The first principal component of the variables emp_loyal_num, wrk_loyal_num, emp_reco_num, new_job_num. Indicates attachment of respondent to their job. Explains 65% of the variance in the stated variables."

output_codebook <- codebookr::codebook(
  df = THD_clean,
  title = "EHF Project: Home Depot Survey",
  subtitle = "Ahlquist, John S.",
  description = "Cleaned survey data relating to the Home Depot Homer Fund EHF study. Compiled by Theodoros Ntounias (RA)."
)

print(output_codebook, "HomeDepot_EHF_codebook.docx")
