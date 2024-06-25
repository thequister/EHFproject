#source("1_libraries_and_settings.R")
#source("2_data_format.R")


unionvote.out<-THD_comp %>% 
  drop_na(union_vote) %>%
  group_by(HDTreatment, union_vote) %>%
  summarise(union = survey_mean(proportion=TRUE, 
    vartype="ci", na.rm=TRUE),
            n = unweighted(n()))

ui.out<-THD_comp %>% 
  group_by(HDTreatment, gov_ui) %>%
  summarise(ui = survey_mean(proportion=TRUE, vartype="ci"),
            n = unweighted(n()))
ui.out$ui_num <- as.numeric(ui.out$gov_ui)

pension.out<-THD_comp %>% 
  group_by(HDTreatment, gov_pension) %>%
  summarise(pension = survey_mean(proportion=TRUE, vartype="ci"),
            n = unweighted(n()))

cc.out<-THD_comp %>% 
  group_by(HDTreatment, gov_childcare) %>%
  summarise(childcare = survey_mean(proportion=TRUE, vartype="ci"),
            n = unweighted(n()))



#plots
uv_plot <- ggplot(data = unionvote.out,
            mapping = aes(x = union_vote, y = union,
                          ymin = union_low,
                          ymax = union_upp,
                          fill = HDTreatment,
                          color = HDTreatment,
                          group = HDTreatment))

uv_plot <- uv_plot + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    #scale_x_continuous(name=NULL, 
    #                 breaks = 1:4,
    #                 labels = c("1" ="none at all",
    #                            "2"="", "3" = "",
    #                            "4"="a lot")) +
  labs(title = "Support for unionization by EHF treatment",
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

ui_plot <- ggplot(data = ui.out,
            mapping = aes(x = ui_num, y = ui,
                          ymin = ui_low,
                          ymax = ui_upp,
                          fill = HDTreatment,
                          color = HDTreatment,
                          group = HDTreatment))

ui_plot <- ui_plot + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 1:4,
                     labels = c("1" ="none",
                                "2"="", "3" = "",
                                "4"="a lot")) +
  labs(title = "Support for UI by EHF treatment",
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

gridExtra::grid.arrange(uv_plot, ui_plot, nrow = 2)
#pdf("ui_uv.pdf")
#gridExtra::grid.arrange(uv_plot, ui_plot, nrow = 2)
#dev.off()
