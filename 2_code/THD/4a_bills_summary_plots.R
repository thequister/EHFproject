#source("1_libraries_and_settings.R")
#source("2_data_format.R")


bill.out<-THD_comp %>% 
  group_by(HDTreatment, bills) %>%
  summarise(billpay = survey_mean(proportion=TRUE, vartype="ci"),
            w_n = n(),
            n = unweighted(n()))

bill.out$bill_num<-as.numeric(bill.out$bills)
dodge <- position_dodge(width=0.9)

p.ord <- ggplot(data = bill.out,
            mapping = aes(x = bill_num, y = billpay,
                          ymin = billpay_low,
                          ymax = billpay_upp,
                          fill = HDTreatment,
                          color = HDTreatment,
                          group = HDTreatment))

p.ord <- p.ord + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 1:3,
                     labels = c("1" ="not difficult",
                                "2"="",
                                "3"="very difficult")) +
  labs(title = "Difficulty paying bills by EHF treatment (weighted)",
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

