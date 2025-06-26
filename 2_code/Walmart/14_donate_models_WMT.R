#source("1_libraries_and_settings.R")
#source("2_data_format.R")

d_wmt <- d_int_wmt <-glm(wmt.hq$donate == "YES I would like to learn how to donate" ~ treatment_bin, family=binomial, data=wmt.hq)
d_int_wmt <-glm(wmt.hq$donate == "YES I would like to learn how to donate" ~ treatment_bin*ehf_aware_pretr, family=binomial, data=wmt.hq)

d_c_wmt <-glm(wmt.hq$donate == "YES I would like to learn how to donate" ~ treatment_bin +
                age_clean + 
                male +
                main_job +
                tenure_num +
                nonwhite +
                fulltime +
                #    hourly+ perfect separation
                college, family=binomial, data=wmt.hq)
d_firth <-logistf::logistf(donate == "YES I would like to learn how to donate" ~ treatment_bin,
                           data = wmt.hq)
d_firth <-logistf::logistf(donate == "YES I would like to learn how to donate" ~ treatment_full,
                           data = wmt.hq)
d_mods<-list(d_wmt, d_c_wmt, d_int_wmt)
names(d_mods)<-c("base", "covariates", "pre-exposure")

gm <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = 0),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 0))
note1 <- "Standard errors in parentheses. Covariates include age, gender race, job tenure, full time status, college degree, and main job.  Hourly omitted due to separation"
donation_tab_wmt<-modelsummary(d_mods,
                               #shape = term + response ~ statistic,
                               #coef_map = coef_maps,
                               title = "Logistic regression on ACNT donation  \\label{tab:tab-donate-models-wmt}",
                               #vcov = "robust",
                               gof_map = gm,
                               #add_rows = rows,
                               notes = list(note1),
                               threeparttable=TRUE, 
                               stars = c('*' = .05, '**' = .01),
                               escape = FALSE
)

donation_summary <- wmt.hq  |>
  mutate(
    donate_bin = donate=="YES I would like to learn how to donate"
  ) |> 
  group_by(treatment_bin)  |> 
  summarise(
    prop = mean(donate_bin),
    n = n(),
    ci_lower = binom.test(sum(donate_bin), n)$conf.int[1],
    ci_upper = binom.test(sum(donate_bin), n)$conf.int[2]
  )

d_plot <-ggplot(donation_summary, aes(x = as.factor(treatment_bin), y = prop, color = as.factor(treatment_bin))) +
  geom_point(position = position_dodge(width = 0.25), size = 4) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(0.25)) +
  labs(x = "", y = "Proportion", title = "Proportion taking donation action") +
  scale_color_brewer(palette = "Dark2", name = "Treatment Group") +
  scale_x_discrete(labels = c("FALSE" = "Untreated", "TRUE" = "Treated"))+
  theme_minimal() + 
  theme(legend.position = "none") 

ggsave(d_plot, filename=here::here("4_output", "plots", "donate_wmt.pdf"))

donate_dp<-interpretCI::propCI(n1=donation_summary$n[1],
                               n2=donation_summary$n[2],
                               p1=donation_summary$prop[1],
                               p2=donation_summary$prop[2])

