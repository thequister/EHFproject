#source("1_libraries_and_settings.R")
#source("2_data_format.R")


ee.out<-THD_comp %>% 
  group_by(HDTreatment, emergency_expense) %>%
  summarise(finsec = survey_mean(proportion=TRUE, vartype="ci"),
            w_n = n(),
            n = unweighted(n()))
ee.out$emergency_expense_num<-as.numeric(ee.out$emergency_expense)

ee.out.bin<-THD_comp %>% 
  group_by(HDTreatment, emergency_expense_bin) %>%
  summarise(finsec = survey_mean(proportion=TRUE, vartype="ci"),
            w_n = n(),
            n = unweighted(n()))
ee.out.bin$emergency_expense_num<-as.numeric(ee.out.bin$emergency_expense_bin)


p.ord <- ggplot(data = ee.out,
            mapping = aes(x = emergency_expense_num, y = finsec,
                          ymin = finsec_low,
                          ymax = finsec_upp,
                          fill = HDTreatment,
                          color = HDTreatment,
                          group = HDTreatment))

dodge <- position_dodge(width=0.9)

p.ord<- p.ord + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 1:4,
                     labels = c("1" ="definitely can't cover $400",
                                "2"="","3"="",
                                "4"="can definitely cover $400")) +
  labs(#title = "Financial insecurity by EHF treatment",
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

p.bin<- ggplot(data = ee.out.bin,
            mapping = aes(x = emergency_expense_num, y = finsec,
                          ymin = finsec_low,
                          ymax = finsec_upp,
                          fill = HDTreatment,
                          color = HDTreatment,
                          group = HDTreatment))

p.bin<- p.bin + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 0:1,
                     labels = c("0" ="can't cover $400",
                                "1"="can cover $400")) +
  labs(title = "Financial insecurity by EHF treatment",
       x = NULL, y = "% treatment group") +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L)) +
  scale_fill_discrete(name="EHF treatment", 
                      breaks=c("cntrl", "txt", "vid"),
                      labels=c("Control", "Text", "Video")) +
  scale_color_discrete(name = "EHF treatment", 
                       labels=c("Control", "Text", "Video")) +
  theme(legend.position = "none",
        panel.grid = element_blank())
  
  
#gridExtra::grid.arrange(p.ord, p.bin, nrow = 2)
ggsave(filename = here::here("4_output", "plots", "finsec_ord.pdf"),
  plot = p.ord)
ggsave(filename = here::here("4_output", "plots", "finsec_bin.pdf"),
  plot = p.bin)

#summary(svyglm(emergency_expense_bin~HDTreatment, design = THD_comp, family=binomial))
