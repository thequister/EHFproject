#source("1_libraries_and_settings.R")
#source("2_data_format.R")



unionvote_pretr_trbin <- wmt %>%
  group_by(treatment_bin, ehf_aware_pretr, union_elec) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(treatment_bin, ehf_aware_pretr) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup() |> 
  mutate(
    ci = map2(count, total, ~binom.confint(.x, .y, methods = "wilson")),
    ci_low = unlist(map(ci, ~.x$lower)),
    ci_high = unlist(map(ci, ~.x$upper))
  )  |> 
  select(-ci)
#print(unionvote_pretr_trbin)

unionvote_pretr_trbin_hq <- wmt.hq %>%
  group_by(treatment_bin, ehf_aware_pretr, union_elec) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(treatment_bin, ehf_aware_pretr) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup() |> 
  mutate(
    ci = map2(count, total, ~binom.confint(.x, .y, methods = "wilson")),
    ci_low = unlist(map(ci, ~.x$lower)),
    ci_high = unlist(map(ci, ~.x$upper))
  )  |> 
  select(-ci)
#print(unionvote_pretr_trbin_hq)



unionvote_pretr_trplacebo <- wmt %>%
  group_by(treatment_placebo, ehf_aware_pretr, union_elec) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(treatment_placebo, ehf_aware_pretr) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup() |> 
  mutate(
    ci = map2(count, total, ~binom.confint(.x, .y, methods = "wilson")),
    ci_low = unlist(map(ci, ~.x$lower)),
    ci_high = unlist(map(ci, ~.x$upper))
  )  |> 
  select(-ci)
#print(unionvote_pretr_trplacebo)


ui.out.wmt<-wmt.hq_unwgt %>% 
  group_by(treatment_bin, govt_responsib_unemp_num) %>%
  summarise(ui = survey_mean(proportion=TRUE, vartype="ci"),
            n = unweighted(n()))
ui.out.wmt$govt_responsib_unemp_num <- 4*as.numeric(ui.out.wmt$govt_responsib_unemp_num)

pension.out.wmt<-wmt.hq_unwgt %>% 
  group_by(treatment_bin, govt_responsib_elder_num) %>%
  summarise(pension = survey_mean(proportion=TRUE, vartype="ci"),
            n = unweighted(n()))
pension.out.wmt$govt_responsib_elder_num <- 4*as.numeric(pension.out.wmt$govt_responsib_elder_num)


emerg.out.wmt<-wmt.hq_unwgt %>% 
  group_by(treatment_bin, govt_responsib_hardship_num) %>%
  summarise(emerg = survey_mean(proportion=TRUE, vartype="ci"),
            n = unweighted(n()))
emerg.out.wmt$govt_responsib_hardship_num <- 4*as.numeric(emerg.out.wmt$govt_responsib_hardship_num)



#plots
uv_plot_wmt <- ggplot(data = unionvote_pretr_trbin,
                          mapping = aes(x = union_elec, y = proportion,
                                        ymin = ci_low,
                                        ymax = ci_high,
                                        fill = treatment_bin,
                                        color = treatment_bin,
                                        group = treatment_bin))

uv_plot_wmt <- uv_plot_wmt + geom_col(position = dodge, alpha = 0.2) +
  geom_errorbar(position = dodge, width = 0.2) +
  facet_wrap(~ ehf_aware_pretr) +
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



uv_plot_placebo <- ggplot(data = unionvote_pretr_trplacebo,
            mapping = aes(x = union_elec, y = proportion,
                          ymin = ci_low,
                          ymax = ci_high,
                          fill = treatment_placebo,
                          color = treatment_placebo,
                          group = treatment_placebo))

uv_plot_placebo <- uv_plot_placebo + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
  facet_wrap(~ ehf_aware_pretr) +
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


## UI plot

ui_plot_wmt <- ggplot(data = ui.out.wmt,
            mapping = aes(x = govt_responsib_unemp_num, y = ui,
                          ymin = ui_low,
                          ymax = ui_upp,
                          fill = treatment_bin,
                          color = treatment_bin,
                          group = treatment_bin))

ui_plot_wmt <- ui_plot_wmt + geom_col(position = dodge, alpha = 0.2) +
    geom_errorbar(position = dodge, width = 0.2) +
    scale_x_continuous(name=NULL, 
                     breaks = 1:4,
                     labels = c("1" ="none",
                                "2"="", "3" = "",
                                "4"="a lot")) +
  labs(#title = "Support for UI by EHF treatment",
       x = NULL, y = "% treatment group") +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L)) +
  scale_fill_discrete(name="EHF treatment", 
                      breaks=c(FALSE, TRUE),
                      labels=c("Untreated", "Treated")) +
  scale_color_discrete(name = "EHF treatment", 
                       labels=c("Untreated", "Treated")) +
  #scale_color_brewer(type = "qual", palette = "Dark2", aesthetics = c("colour", "fill")) +
  theme(legend.position = "top",
        panel.grid = element_blank())

ggsave(filename = 'ui_plot_wmt.png',
       plot = ui_plot_wmt,
       path = here('4_output', 'plots'))

#gridExtra::grid.arrange(uv_plot, ui_plot, nrow = 2)
#pdf("ui_uv.pdf")
#gridExtra::grid.arrange(uv_plot, ui_plot, nrow = 2)
#dev.off()

emerg_plot_wmt <- ggplot(data = emerg.out.wmt,
                      mapping = aes(x = govt_responsib_hardship_num, y = emerg,
                                    ymin = emerg_low,
                                    ymax = emerg_upp,
                                    fill = treatment_bin,
                                    color = treatment_bin,
                                    group = treatment_bin))

emerg_plot_wmt <- emerg_plot_wmt + geom_col(position = dodge, alpha = 0.2) +
  geom_errorbar(position = dodge, width = 0.2) +
  scale_x_continuous(name=NULL, 
                     breaks = 1:4,
                     labels = c("1" ="none",
                                "2"="", "3" = "",
                                "4"="a lot")) +
  labs(#title = "Support for UI by EHF treatment",
    x = NULL, y = "% treatment group") +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L)) +
  scale_fill_discrete(name="EHF treatment", 
                      breaks=c(FALSE, TRUE),
                      labels=c("Untreated", "Treated")) +
  scale_color_discrete(name = "EHF treatment", 
                       labels=c("Untreated", "Treated")) +
  #scale_color_brewer(type = "qual", palette = "Dark2", aesthetics = c("colour", "fill")) +
  theme(legend.position = "top",
        panel.grid = element_blank())

ggsave(filename = 'emerg_plot_wmt.png',
       plot = emerg_plot_wmt,
       path = here('4_output', 'plots'))

#gridExtra::grid.arrange(uv_plot, emerg_plot, nrow = 2)
#pdf("emerg_uv.pdf")
#gridExtra::grid.arrange(uv_plot, emerg_plot, nrow = 2)
#dev.off()

