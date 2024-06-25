```{r fig-HTE-unexposed, echo=FALSE, fig.cap="Subjective financial security"}

ee.out.bin<-THD_comp_uw %>% 
  group_by(EHF_aware_list, HDTreatment) %>%
  summarise(finsec = mean(emergency_expense_bin, na.rm=T),
            wrkl = mean(as.numeric(wrk_loyal), na.rm=T),
            empl = mean(as.numeric(emp_loyal), na.rm=T),
            empr = mean(as.numeric(emp_reco), na.rm=T))
#,
            #uv = mean(union_vote, na.rm=T),
            #ui = mean(gov_ui, na.rm=T))
            
ee.out.bin$emergency_expense_num<-as.numeric(ee.out.bin$emergency_expense_bin)

unionvote.out.hte<-THD_comp %>% 
  group_by(HDTreatment, EHF_aware_list, union_vote) %>%
  summarise(union = survey_mean(proportion=TRUE, vartype="ci"),
            n = unweighted(n()))

uv_plot_hte <- ggplot(data = filter(unionvote.out.hte, EHF_aware_list == F),
            mapping = aes(x = union_vote, y = union,
                          ymin = union_low,
                          ymax = union_upp,
                          fill = HDTreatment,
                          color = HDTreatment,
                          group = HDTreatment))

uv_plot_hte <- uv_plot_hte + geom_col(position = dodge, alpha = 0.2) +
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

```
