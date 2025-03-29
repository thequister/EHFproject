#source("1_libraries_and_settings.R")
#source("2_data_format.R")



#summary tables

attach.index.out<-data.frame(
  with(THD_comp_uw, 
       rbind(
         -MeanDiffCI(attachment_index ~ HDTreatment, subset = HDTreatment %in% c("cntrl", "txt")),
         -MeanDiffCI(attachment_index ~ HDTreatment, subset = HDTreatment %in% c("cntrl", "vid"))
       )
  )
)


attach.index.out$trt <- factor(
  c("text", "video"),
  levels = c("text", "video")
)

p.ai_thd <- ggplot(attach.index.out, aes(x = trt, y = meandiff)) +
  geom_point() +  # Plot the mean values as points
  geom_errorbar(aes(ymin = lwr.ci, ymax = upr.ci), width = 0.2) +  # Add error bars for CI
  labs(title = "ATE for Home Depot attachment index",
       x = "Treatment",
       y = "Change in Attachment Index") +
  geom_hline(yintercept = 0, linetype = "dotted")

ggsave(filename = here::here("4_output", "plots", "attachment_thd.pdf"),
       plot = p.ai_thd)

wrk.loyal.out<-THD_comp %>% 
  group_by(HDTreatment, wrk_loyal) %>%
  summarise(loyal = survey_mean(proportion=TRUE, vartype="ci"),
            w_n = n(),
            n = unweighted(n()))
#wrk.loyal.out
wrk.loyal.out$loyal_num<-as.numeric(wrk.loyal.out$wrk_loyal)

emp.loyal.out<-THD_comp %>% 
  group_by(HDTreatment, emp_loyal) %>%
  summarise(loyal = survey_mean(proportion=TRUE, vartype="ci"),
            n = unweighted(n()))
emp.loyal.out$loyal_num<-as.numeric(emp.loyal.out$emp_loyal)
#emp.loyal.out

emp.reco.out <- THD_comp %>% 
  group_by(HDTreatment, emp_reco) %>%
  summarise(reco = survey_mean(proportion=TRUE, vartype="ci"),
            n = unweighted(n()))
emp.reco.out$reco_num <- as.numeric(emp.reco.out$emp_reco)
#emp.loyal.out

nps<-emp.reco.out %>%  #calculating reco + v. reco - props by treatment
  group_map(~sum(.x$reco[.x$reco_num>3], na.rm=T) - 
              sum(.x$reco[.x$reco_num<3], na.rm=T))

sds<-THD_comp %>% 
  summarise(wl.sd = survey_sd(as.numeric(wrk_loyal), vartype="ci", na.rm=T),
            el.sd = survey_sd(as.numeric(emp_loyal), vartype="ci", na.rm=T),
            er.sd = survey_sd(as.numeric(emp_reco), vartype="ci", na.rm=T)
            )

#plots
wk.loyal <- ggplot(data = wrk.loyal.out,
            mapping = aes(x = loyal_num, y = loyal,
                          ymin = loyal_low,
                          ymax = loyal_upp,
                          fill = HDTreatment,
                          color = HDTreatment,
                          group = HDTreatment))

wk.loyal <- wk.loyal + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 1:4,
                     labels = c("1" ="none at all",
                                "2"="", "3" = "",
                                "4"="a lot")) +
  labs(title = "Loyalty to coworkers by EHF treatment",
       x = NULL, y = "% treatment group") +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L)) +
  scale_fill_discrete(name="EHF treatment", 
                      breaks=c("cntrl", "txt", "vid"),
                      labels=c("Control", "Text", "Video")) +
  scale_color_discrete(name = "EHF treatment", 
                       labels=c("Control", "Text", "Video")) +
  #scale_color_brewer(type = "qual", palette = "Dark2", aesthetics = c("colour", "fill")) +
  theme(legend.position = "top",
        panel.grid = element_blank())


emp.loyal <- ggplot(data = emp.loyal.out,
            mapping = aes(x = loyal_num, y = loyal,
                          ymin = loyal_low,
                          ymax = loyal_upp,
                          fill = HDTreatment,
                          color = HDTreatment,
                          group = HDTreatment))

emp.loyal <- emp.loyal + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 1:4,
                     labels = c("1" ="none at all",
                                "2"="", "3" = "",
                                "4"="a lot")) +
  labs(title = "Loyalty to employer by EHF treatment",
       x = NULL, y = "% treatment group") +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L)) +
  scale_fill_discrete(name="EHF treatment", 
                      breaks=c("cntrl", "txt", "vid"),
                      labels=c("Control", "Text", "Video")) +
  scale_color_discrete(name = "EHF treatment", 
                       labels=c("Control", "Text", "Video")) +
  #scale_color_brewer(type = "qual", palette = "Dark2", aesthetics = c("colour", "fill")) +
  theme(legend.position = "none",
        panel.grid = element_blank())


emp.reco <- ggplot(data = emp.reco.out,
            mapping = aes(x = reco_num, y = reco,
                          ymin = reco_low,
                          ymax = reco_upp,
                          fill = HDTreatment,
                          color = HDTreatment,
                          group = HDTreatment))

emp.reco <- emp.reco + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 1:5,
                     labels = c("1" ="def. wouldn't",
                                "2"="", "3" = "unsure",
                                "4"="",
                                "5" = "def. would")) +
  labs(title = "Would recommend employer by EHF treatment",
       x = NULL, y = "% treatment group") +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L)) +
  scale_fill_discrete(name="EHF treatment", 
                      breaks=c("cntrl", "txt", "vid"),
                      labels=c("Control", "Text", "Video")) +
  scale_color_discrete(name = "EHF treatment", 
                       labels=c("Control", "Text", "Video")) +
  #scale_color_brewer(type = "qual", palette = "Dark2", aesthetics = c("colour", "fill")) +
  theme(legend.position = "none",
        panel.grid = element_blank())


gridExtra::grid.arrange(wk.loyal, emp.loyal, emp.reco, nrow = 3)
#pdf("attachments.pdf")
#gridExtra::grid.arrange(wk.loyal, emp.reco, nrow = 2)
#dev.off()

ggsave(filename = here::here("4_output", "plots", "wk_loyal.pdf"),
  plot = wk.loyal)
ggsave(filename = here::here("4_output", "plots", "emp_loyal.pdf"),
  plot = emp.loyal)
ggsave(filename = here::here("4_output", "plots", "emp_reco.pdf"),
  plot = emp.reco)
