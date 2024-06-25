rm(list=ls());gc()

library("here")
library("tidyverse")
qualtrics = read_csv("~/Dropbox/EHF/Amazon/qualtrics_data/EHF+Survey+-+Amazon_December+12%2C+2022_10.25.csv") %>%
  slice(-1:-2)

qualtrics_questions = read_csv("~/Dropbox/EHF/Amazon/qualtrics_data/EHF+Survey+-+Amazon_September+7%2C+2022_12.20.csv") %>%
  slice(1:2)
qualtrics_questions = tibble(
  id = colnames(qualtrics_questions),
  label = unlist(qualtrics_questions[1, ]),
  import_id = unlist(qualtrics_questions[2, ])
)

sum(!is.na(qualtrics$Q7.1))
emails = qualtrics %>%
  filter(!is.na(Q7.1)) %>%
  select(Q7.1) %>%
  unlist()
length(unique(emails))
emails[duplicated(emails)]
IPs = qualtrics %>%
  filter(!is.na(IPAddress)) %>%
  select(IPAddress) %>%
  unlist()
length(unique(IPs))
IPs[duplicated(IPs)]
qualtrics %>%
  filter(Q2.1=="Yes") %>%
  nrow()
qualtrics %>%
  filter(Q2.1=="No") %>%
  nrow()
qualtrics %>%
  filter(Finished=="True") %>%
  nrow()

# Thank you note
table(qualtrics$HDTreatment, !is.na(qualtrics$Q7.1))

# manatory question before treatment
table(qualtrics$HDTreatment, !is.na(qualtrics$Q2.16))

# question right after treatment
table(qualtrics$HDTreatment, !is.na(qualtrics$Q3.4))


mean(!is.na(qualtrics$Q7.1[190:nrow(qualtrics)]))
mean(!is.na(qualtrics$Q7.1[1:189]))


# duration for all subjects

qualtrics %>%
  group_by(HDTreatment) %>%
  summarize(mean_duration = median(as.numeric(`Duration (in seconds)`), na.rm=TRUE))

ggplot(qualtrics, aes(x=as.numeric(`Duration (in seconds)`))) +
  geom_histogram(bins=20, color="black", fill="grey") +
  facet_grid(~HDTreatment) +
  theme_minimal()

# duration for completed subjects

qualtrics %>%
  filter(!is.na(Q7.1)) %>%
  group_by(HDTreatment) %>%
  summarize(mean_duration = median(as.numeric(`Duration (in seconds)`), na.rm=TRUE))

ggplot(qualtrics %>% filter(!is.na(Q7.1)),
       aes(x=as.numeric(`Duration (in seconds)`))) +
  geom_histogram(bins=20, color="black", fill="grey") +
  facet_grid(~HDTreatment) +
  theme_minimal()


table(qualtrics$HDTreatment, qualtrics$Q3.5)
table(qualtrics$HDTreatment, qualtrics$Q3.6)
table(qualtrics$HDTreatment, qualtrics$Q3.7)


qualtrics %>%
  mutate(HDTreatment = case_when(HDTreatment==0 ~ "0: Control",
                                 HDTreatment==1 ~ "1: Text",
                                 HDTreatment==2 ~ "2: Video")) %>%
  group_by(HDTreatment) %>%
  summarize(n=n(),
            Q2.1=sum(!is.na((Q2.1))),
            Q2.1_p=Q2.1/n,
            Q2.16=sum(!is.na((Q2.16))),
            Q2.16_p=Q2.16/n,
            Q3.4=sum(!is.na((Q3.4))),
            Q3.4_p=Q3.4/n,
            Q4.2=sum(!is.na((Q4.2))),
            Q4.2_p=Q4.2/n,
            Q5.2=sum(!is.na((Q5.2))),
            Q5.2_p=Q5.2/n,
            Q6.2=sum(!is.na((Q6.2))),
            Q6.2_p=Q6.2/n,
            Q6.16=sum(!is.na((Q6.16))),
            Q6.16_p=Q6.16/n,
            Q7.1=sum(!is.na((Q7.1))),
            Q7.1_p=Q7.1/n)


table(qualtrics$Q2.1, useNA = "ifany")
table(qualtrics$Q2.2, useNA = "ifany")
table(qualtrics$Q5.5, useNA = "ifany")


qualtrics = qualtrics %>%
  mutate(completion_subgroup = case_when(
    (Q2.1 != "Yes" | is.na((Q2.1))) ~ "(1) Haven't worked for THD",
    is.na((Q2.16)) ~ "(2) Quit before receiving HD treatment",
    is.na((Q3.4)) ~ "(3) Quit before HD outcome",
    is.na((Q5.12)) ~ "(4) Quit before receiving COVID treatment",
    (((COVIDTreatment==0 & is.na((Q5.17))) | (COVIDTreatment==1 & is.na((Q5.18))))) ~ "(5) Quit before COVID outcome",
    is.na((Q7.1)) ~ "(6) Quit before email",
    !is.na((Q7.1)) ~ "(7) Finished"
  )) 
# %>%
#   select(Q2.1, Q2.16, Q3.4, Q5.12, COVIDTreatment, Q5.17, Q5.18, Q7.1, completion_subgroup)
table(qualtrics$completion_subgroup)

qualtrics$completion_group_num <- as.numeric(as.factor(qualtrics$completion_subgroup))
qualtrics$fininsec_bin <- qualtrics$Q3.4 %in% c("I am certain I could not come up with $400", 
  "I could probably not come up with $400")
qualtrics$work_loyalty_bin <- qualtrics$Q3.5 %in% c("A lot of loyalty", 
  "Some loyalty")
qualtrics$nps

tempdat<-qualtrics[qualtrics$completion_group_num >5,]




