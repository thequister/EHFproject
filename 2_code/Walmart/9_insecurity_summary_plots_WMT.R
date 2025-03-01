#source("1_libraries_and_settings.R")
#source("2_data_format.R")


ee.out<-wmt_unwgt %>% 
  group_by(treatment_full, emergency_expense_num) %>%
  summarise(finsec = survey_mean(proportion=TRUE, vartype="ci"),
            w_n = n(),
            n = unweighted(n()))
#ee.out$emergency_expense_num<-as.numeric(ee.out$emergency_expense)

ee.out.bin<-wmt_unwgt %>% 
  group_by(treatment_full, emergency_expense_bin) %>%
  summarise(finsec = survey_mean(proportion=TRUE, vartype="ci"),
            w_n = n(),
            n = unweighted(n()))
#ee.out.bin$emergency_expense_num<-as.numeric(ee.out.bin$emergency_expense_bin)

ee.out.bin<-wmt_unwgt %>% 
  group_by(treatment_bin, emergency_expense_bin) %>%
  summarise(finsec = survey_mean(proportion=TRUE, vartype="ci"),
            w_n = n(),
            n = unweighted(n()))

p.ord <- ggplot(data = ee.out,
            mapping = aes(x = emergency_expense_num*4, y = finsec,
                          ymin = finsec_low,
                          ymax = finsec_upp,
                          fill = treatment_full,
                          color = treatment_full,
                          group = treatment_full))

dodge <- position_dodge(width=0.9)

p.ord<- p.ord + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 1:4,
                     labels = c("1" ="definitely can't cover $400",
                                "2"="","3"="",
                                "4"="can definitely cover $400")) +
  labs(title = "Financial insecurity by EHF treatment",
       x = NULL, y = "% treatment group") +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L)) +
  scale_fill_discrete(name="EHF treatment", 
                      breaks=c("cntrl", "vid0", "vidChar", "vidSolid"),
                      labels=c("Control", "Placebo", "Charity", "Solidarity")) +
  scale_color_discrete(name = "EHF treatment", 
                       labels=c("Control", "Placebo", "Charity", "Solidarity")) +
  #scale_color_brewer(type = "qual", palette = "Dark2", aesthetics = c("colour", "fill")) +
  theme(legend.position = "top",
        panel.grid = element_blank())

p.bin<- ggplot(data = ee.out.bin,
            mapping = aes(x = as.numeric(emergency_expense_bin), y = finsec,
                          ymin = finsec_low,
                          ymax = finsec_upp,
                          fill = treatment_bin,
                          color = treatment_bin,
                          group = treatment_bin))

p.bin<- p.bin + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 0:1,
                     labels = c("0" ="can't cover $400",
                                "1"="can cover $400")) +
  labs(title = "Financial insecurity by EHF treatment, Walmart sample",
       x = NULL, y = "% treatment group") +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L)) +
  scale_fill_discrete(name="EHF treatment", 
                      #breaks=c(0, 1),
                      labels=c("Control", "Treated")) +
  scale_color_discrete(#name = "EHF treatment", 
                       labels=c("Control", "Treated")) +
  theme(legend.position = "top",
        panel.grid = element_blank())
  
  
#gridExtra::grid.arrange(p.ord, p.bin, nrow = 2)
#ggsave(filename = here::here("output", "plots", "finsec_ord.pdf"),
#  plot = p.ord)
#ggsave(filename = here::here("output", "plots", "finsec_bin.pdf"),
#  plot = p.bin)

ggsave(filename = here::here("4_output", "plots", "finsec_bin_wmt.pdf"),
         plot = p.bin)

#summary(svyglm(emergency_expense_bin~HDTreatment, design = THD_comp, family=binomial))
