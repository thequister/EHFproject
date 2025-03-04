#source("1_libraries_and_settings_ACNT.R")
#source("2_data_format_ACNT.R")



#summary tables

attach.index.out.tab<-wmt_unwgt %>% 
  group_by(treatment_bin) %>%
  summarise(attachment = survey_mean(attachment_index, vartype="ci"),
            n = unweighted(n()))

attach.index.out.tab.placebo<-wmt_unwgt %>% 
  group_by(treatment_placebo) %>%
  summarise(attachment = survey_mean(attachment_index, vartype="ci"),
            n = unweighted(n()))


attach.index.full.out<-wmt_unwgt %>% 
  group_by(treatment_full) %>%
  summarise(attachment = survey_mean(attachment_index, vartype="ci"),
            n = unweighted(n()))

attach.index.hq.out<-wmt.hq_unwgt %>% 
  group_by(treatment_bin) %>%
  summarise(attachment = survey_mean(attachment_index, vartype="ci"),
            n = unweighted(n()))

attach.index.hq.full.out<-wmt.hq_unwgt %>% 
  group_by(treatment_full) %>%
  summarise(attachment = survey_mean(attachment_index, vartype="ci"),
            n = unweighted(n()))


attach.index.out<-data.frame(
  with(wmt, 
       rbind(
         -MeanDiffCI(attachment_index ~ treatment_bin),  # - b/c order of difference.  care with upper, lower CI bounds
         -MeanDiffCI(attachment_index ~ treatment_placebo, subset = treatment_placebo %in% c("control", "placebo")),  # - b/c order of difference.  care with upper, lower CI bounds
         -MeanDiffCI(attachment_index ~ treatment_placebo, subset = treatment_placebo %in% c("control", "treatment")),  
         #-MeanDiffCI(attachment_index ~ treatment_full,  subset = treatment_full %in% c("ctrl", "vid0")),
         -MeanDiffCI(attachment_index ~ treatment_full, subset = treatment_full %in% c("ctrl", "vidChar")),
         -MeanDiffCI(attachment_index ~ treatment_full, subset = treatment_full %in% c("ctrl", "vidSolid"))
         )
     )
)
attach.index.out$trt <- factor(
  c("pooled", "placebo-control", "treated-control", "charity-control", "solidarity-control"),
  levels = c("pooled", "placebo-control", "treated-control", "charity-control", "solidarity-control")
)


attach.index.hq.out<-data.frame(
  with(wmt.hq, 
       rbind(
         -MeanDiffCI(attachment_index ~ treatment_bin),  # - b/c order of difference.  care with upper, lower CI bounds
         -MeanDiffCI(attachment_index ~ treatment_placebo, subset = treatment_placebo %in% c("control", "placebo")),  # - b/c order of difference.  care with upper, lower CI bounds
         -MeanDiffCI(attachment_index ~ treatment_placebo, subset = treatment_placebo %in% c("control", "treatment")),  
         #-MeanDiffCI(attachment_index ~ treatment_full,  subset = treatment_full %in% c("ctrl", "vid0")),
         -MeanDiffCI(attachment_index ~ treatment_full, subset = treatment_full %in% c("ctrl", "vidChar")),
         -MeanDiffCI(attachment_index ~ treatment_full, subset = treatment_full %in% c("ctrl", "vidSolid"))
       )
  )
)

attach.index.hq.out$trt <- attach.index.out$trt


p.ai_wmt_bin <- ggplot(attach.index.out, aes(x = trt, y = meandiff)) +
  geom_point() +  # Plot the mean values as points
  geom_errorbar(aes(ymin = lwr.ci, ymax = upr.ci), width = 0.2) +  # Add error bars for CI
  labs(title = "ATE for Walmart attachment index (full sample)",
       x = "Treatment",
       y = "Change in attachment Index") +
  geom_hline(yintercept = 0, linetype = "dotted")


p.ai_wmt_bin_hq <- ggplot(attach.index.hq.out, aes(x = trt, y = meandiff)) +
  geom_point() +  # Plot the mean values as points
  geom_errorbar(aes(ymin = lwr.ci, ymax = upr.ci), width = 0.2) +  # Add error bars for CI
  labs(title = "ATE for Walmart attachment index (high quality sample)",
       x = "Treatment",
       y = "Change in attachment Index") +
  geom_hline(yintercept = 0, linetype = "dotted")

ggsave(filename = here::here("4_output", "plots", "attachment_wmt.pdf"),
       plot = p.ai_wmt_bin)

ggsave(filename = here::here("4_output", "plots", "attachment_wmt_hq.pdf"),
       plot = p.ai_wmt_bin_hq)


#components


new.job.out<-wmt_unwgt %>% 
  group_by(treatment_bin, new_job_num) %>%
  summarise(new_job = survey_mean(proportion=TRUE, vartype="ci"),
            w_n = n(),
            n = unweighted(n()))


# loyalty to coworkers
wrk.loyal.out<-wmt_unwgt %>% 
  group_by(treatment_bin, wrk_loyal_num) %>%
  summarise(loyal = survey_mean(proportion=TRUE, vartype="ci"),
            w_n = n(),
            n = unweighted(n()))

#loyalty to employer
emp.loyal.out<-wmt_unwgt %>% 
  group_by(treatment_bin, emp_loyal_num) %>%
  summarise(loyal = survey_mean(proportion=TRUE, vartype="ci"),
            n = unweighted(n()))

#recomend employer
emp.reco.out <- wmt_unwgt %>% 
  group_by(treatment_bin, emp_reco_num) %>%
  summarise(reco = survey_mean(proportion=TRUE, vartype="ci"),
            n = unweighted(n()))

nps<-emp.reco.out %>%  #calculating reco + v. reco - props by treatment
  group_map(~sum(.x$reco[.x$emp_reco_num>.5], na.rm=T) - 
              sum(.x$reco[.x$emp_reco_num<.5], na.rm=T))

sds<-THD_comp %>% 
  summarise(wl.sd = survey_sd(as.numeric(wrk_loyal), vartype="ci", na.rm=T),
            el.sd = survey_sd(as.numeric(emp_loyal), vartype="ci", na.rm=T),
            er.sd = survey_sd(as.numeric(emp_reco), vartype="ci", na.rm=T)
            )

#plots
newjob.p <- ggplot(data = new.job.out,
                   mapping = aes(x = new_job_num*4, y = new_job,
                                 ymin = new_job_low,
                                 ymax = new_job_upp,
                                 fill = treatment_bin,
                                 color = treatment_bin,
                                 group = treatment_bin))

newjob.p <- newjob.p + geom_col(position = dodge, alpha = 0.2) +
  geom_errorbar(position = dodge, width = 0.2) +
  scale_x_continuous(name=NULL, 
                     breaks = 0:4,
                     labels = c("0" ="not at all likely", "1" ="",
                                "2"="", "3" = "", "4"= "Extremely likely")) +
  labs(title = "Likelihood of looking for a new job among Walmart workers, by EHF treatment",
       x = NULL, y = "% treatment group") +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L))



wk.loyal <- ggplot(data = wrk.loyal.out,
            mapping = aes(x = wrk_loyal_num*3, y = loyal,
                          ymin = loyal_low,
                          ymax = loyal_upp,
                          fill = treatment_bin,
                          color = treatment_bin,
                          group = treatment_bin))

wk.loyal <- wk.loyal + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 0:3,
                     labels = c("0" ="none at all", "1" ="",
                                "2"="", "3" = "a lot")) +
  labs(title = "Loyalty to coworkers by EHF treatment",
       x = NULL, y = "% treatment group") +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L))


#+
#  scale_fill_discrete(name="EHF treatment", 
#                      breaks=c("cntrl", "txt", "vid"),
#                      labels=c("Control", "Text", "Video")) +
#  scale_color_discrete(name = "EHF treatment", 
#                       labels=c("Control", "Text", "Video")) +
  #scale_color_brewer(type = "qual", palette = "Dark2", aesthetics = c("colour", "fill")) +
#  theme(legend.position = "top",
#        panel.grid = element_blank())


emp.loyal <- ggplot(data = emp.loyal.out,
            mapping = aes(x = emp_loyal_num*3, y = loyal,
                          ymin = loyal_low,
                          ymax = loyal_upp,
                          fill = treatment_bin,
                          color = treatment_bin,
                          group = treatment_bin))

emp.loyal <- emp.loyal + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 0:3,
                     labels = c("0" ="none at all", "1"="",
                                "2"="", "3" = "a lot")) +
  labs(title = "Loyalty to Walmart by EHF treatment",
       x = NULL, y = "% treatment group") +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L)) 


emp.reco <- ggplot(data = emp.reco.out,
            mapping = aes(x = emp_reco_num*4, y = reco,
                          ymin = reco_low,
                          ymax = reco_upp,
                          fill = treatment_bin,
                          color = treatment_bin,
                          group = treatment_bin))

emp.reco <- emp.reco + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 1:5,
                     labels = c("0" ="def. wouldn't", "1" ="",
                                "2"="unsure", "3" = "",
                                "4"="def. would")) +
  labs(title = "Would recommend Walmart by EHF treatment",
       x = NULL, y = "% treatment group") +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L))

  
gridExtra::grid.arrange(wk.loyal, emp.loyal, emp.reco, nrow = 4)
#pdf("attachments.pdf")
#gridExtra::grid.arrange(wk.loyal, emp.reco, nrow = 2)
#dev.off()

ggsave(filename = here::here("4_output", "plots", "new_job_trt_wmt.pdf"),
       plot = newjob.p)

ggsave(filename = here::here("4_output", "plots", "wk_loyal_wmt.pdf"),
  plot = wk.loyal)
ggsave(filename = here::here("4_output", "plots", "emp_loyal_wmt.pdf"),
  plot = emp.loyal)
ggsave(filename = here::here("4_output", "plots", "emp_reco_wmt.pdf"),
  plot = emp.reco)
