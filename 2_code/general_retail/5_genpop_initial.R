library(here)
source(here::here('2_code', 'general_retail', '2_data_format_gr.R'))
source(here::here('2_code', 'general_retail', '3_weights_gr.R'))

gr <- gr_clean

#### EHF Aware Plot ----
gr_pl1 <- gr %>%
  select(nonwhite, male, age_clean, college, tenure_fac, hourly, fulltime, ehf_prior_know) %>%
  mutate(age_bin = cut(age_clean, 
                       breaks=c(-Inf, 24, 34, 44, 54, 64, Inf),
                       labels=c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")), 
         ehf_prior_know = (ehf_prior_know == "Yes"))

nw <- aggregate(ehf_prior_know ~ nonwhite, data = gr_pl1, mean)
m <- aggregate(ehf_prior_know ~ male, data = gr_pl1, mean)
age <- aggregate(ehf_prior_know ~ age_bin, data = gr_pl1, mean)
col <- aggregate(ehf_prior_know ~ college, data = gr_pl1, mean)
ten <- aggregate(ehf_prior_know ~ tenure_fac, data = gr_pl1, mean)
hrl <- aggregate(ehf_prior_know ~ hourly, data = gr_pl1, mean)
ft <- aggregate(ehf_prior_know ~ fulltime, data = gr_pl1, mean)

names(nw)[1] <- "group"
nw$group <- c("White", "BIPOC")

names(m)[1] <- "group"
m$group <- c("Female", "Male")

names(age)[1] <- "group"

names(col)[1] <- "group"
col$group <- c("No college", "College")

names(ten)[1] <- "group"

names(hrl)[1] <- "group"
hrl$group <- c("Not hourly", "Hourly")

names(ft)[1] <- "group"
ft$group <- c("Not full time", "Full time")

nw$category <- "Race"
m$category <- "Gender"
age$category <- "Age"
col$category <- "College"
ten$category <- "Tenure"
hrl$category <- "Hourly"
ft$category <- "Fulltime"

plotdt <- rbind(nw, m, age, col, ten, hrl, ft)
plotdt$ehf_prior_know <- 100*plotdt$ehf_prior_know

plotdt$group <- factor(plotdt$group, levels=rev(levels(factor(plotdt$group))))
plotdt$outcome <- "Prior EHF Knowledge %"

ggplot(plotdt, aes(x=group, y=ehf_prior_know, group=outcome, shape=outcome)) +
  geom_point(position=position_dodge(.2)) +
  facet_wrap(~category, ncol=1, scales="free_y") +
  scale_shape_manual(values=c(19,17,0), name="") +
  ylab("% Aware of EHFs Prior to Survey (0-50%)") +
  xlab("") +
  ylim(0, 50) +
  coord_flip() +
  theme(legend.position = "")
  
ggsave("4_output/plots/genpop_ehfaware_crosstab.pdf")

gr_pl1.5 <- gr %>%
  select(ehf_prior_know)

ggplot(data = gr_pl1.5) +
  geom_bar(aes(fill = ehf_prior_know, x = ehf_prior_know), position = "dodge", color = "black") +
  
  scale_fill_brewer("", palette = "RdYlBu") +
  xlab("Prior Knowlegde of EHFs") +
  ylab("Count") +
  theme(legend.position = "")

ggsave("4_output/plots/genpop_ehfaware_simple.pdf")

#### EHF Support + Control Plots -----

# EHF Support Plot
gr_pl2.1 <- gr %>%
  select(ehf_support_exist, ehf_support_new, acs_weight_trim) %>%
  pivot_longer(cols = c(ehf_support_exist:ehf_support_new), 
               names_to = "Q", 
               values_to = "ans") %>%
  group_by(Q, ans) %>%
  summarize(per=sum(acs_weight_trim)) %>%
  filter(!is.na(ans)) %>%
  mutate(per = ifelse(Q == "ehf_support_exist", per/197, per/811),
    SE = ifelse(Q == "ehf_support_exist", sqrt(per*(1 - per)/197), sqrt(per*(1 - per)/811))) %>%
  mutate(Q = case_match(Q, "ehf_support_exist" ~ "Employer has an EHF (N = 197)", 
                        "ehf_support_new" ~ "Employer does not have an EHF (N = 811)"))

ggplot(gr_pl2.1, aes(x = Q, y = per, fill = ans)) +
  geom_col(color = "black", position = "dodge") +
  geom_errorbar(
    aes(
      ymin = per - 1.96*SE, 
      ymax = per + 1.96*SE), 
    color = "black", position=position_dodge(.9), width = .2
  ) +
  scale_fill_brewer("", palette = "RdYlBu") +
  ggtitle("Worker support for EHF (weighted)") +
  ylab("Proportion") +
  xlab("")

ggsave("4_output/plots/genpop_ehfsupport.pdf")

# EHF Control Plot
gr_pl2.2 <- gr %>%
  select(ehf_wrk_exist, ehf_wrk_new, acs_weight_trim) %>%
  mutate(ehf_wrk_exist = factor(ehf_wrk_exist, levels = 
                                  c("Only management controls the fund",
                                    "Management control the fund with worker input",
                                    "Workers and management share control equally",
                                    "Workers control the fund with management input",
                                    "Only workers control the fund"),
                                ordered= TRUE), 
         ehf_wrk_new =  factor(ehf_wrk_new, levels = 
           c("Only management should control the fund",
             "Management should control the fund with worker input",
             "Workers and management should share control equally",
             "Workers should control the fund with management input",
             "Only workers should control the fund"),
         ordered= TRUE))

levels(gr_pl2.2$ehf_wrk_exist) <- c("Only management control", "More management control", 
                                  "Equal control", "More worker control", "Only worker control")
levels(gr_pl2.2$ehf_wrk_new) <- c("Only management control", "More management control", 
                                "Equal control", "More worker control", "Only worker control")

gr_pl2.2 <- gr_pl2.2 %>%
  mutate(across(ehf_wrk_exist:ehf_wrk_new, ~ factor(.x, ordered = F))) %>%
  pivot_longer(cols = c(ehf_wrk_exist:ehf_wrk_new), 
               names_to = "Q", 
               values_to = "ans") %>%
  group_by(Q, ans) %>%
  summarize(per=sum(acs_weight_trim)) %>%
  filter(!is.na(ans)) %>%
  mutate(per = ifelse(Q == "ehf_wrk_exist", per/197, per/811),
         SE = ifelse(Q == "ehf_wrk_exist", sqrt(per*(1 - per)/197), sqrt(per*(1 - per)/811))) %>%
  mutate(fac = case_match(Q, "ehf_wrk_exist" ~ "Actual control over existing EHF", 
                        "ehf_wrk_new" ~ "Prefered control over new EHF")) %>%
  mutate(Q = case_match(Q, "ehf_wrk_exist" ~ "Employer has an EHF (N = 197)", 
                        "ehf_wrk_new" ~ "Employer does not have an EHF (N = 811)")) %>%
  mutate(ans = factor(ans, levels=c("Only management control", "More management control", 
                                    "Equal control", "More worker control", "Only worker control")))

ggplot(filter(gr_pl2.2, fac == "Prefered control over new EHF"), aes(x = ans, y = per)) +
  geom_col(color = "black", position = "dodge", alpha = .5) +
  geom_errorbar(
    aes(
      ymin = per - 1.96*SE, 
      ymax = per + 1.96*SE), 
    color = "black", position=position_dodge(.9), width = .2
  ) +
  labs(title = "Preferences over worker control for hypothetical EHF", 
       subtitle = "(N = 811, weighted estimates)") +
  ylab("Proportion") +
  xlab("") +
  theme(axis.text.x = element_text(size = 13))

ggsave("4_output/plots/genpop_ehfcontrol.pdf")


#### EHF Correct Attribution Plots ------
gr_pl3 <- gr %>%
  select("ehf_offer_thd", "ehf_offer_wal", "ehf_offer_stb", "ehf_offer_disn", "ehf_offer_kohls", "ehf_offer_costco", 
         "ehf_offer_thd_bin", "ehf_offer_wal_bin", "ehf_offer_stb_bin", "ehf_offer_disn_bin", "ehf_offer_kohls_bin", "ehf_offer_costco_bin", "acs_weight_trim")
  

# Correct Identification plot
gr_pl3.1 <- gr_pl3 %>%
  mutate(ehf_offer_costco_bin = ehf_offer_costco == "Does not offer an EHF") %>%
  select(ehf_offer_thd_bin:ehf_offer_costco_bin, acs_weight_trim) %>%
  mutate(across(ehf_offer_thd_bin:ehf_offer_costco_bin, ~ .x * acs_weight_trim)) %>%
  select(-acs_weight_trim) %>%
  gather("Q", "ans") %>%
  group_by(Q) %>%
  summarize(n=sum(ans)) %>%
  mutate(per = n/1008) %>%
  mutate(SE = sqrt(per*(1 - per)/1008))

gr_pl3.1$Q <- c("Costco", 
                "Disney",
                "Kohls",
                "Starbucks",
                "The Home Depot",
                "Walmart")

ggplot(gr_pl3.1) +
  geom_col(aes(x = Q, y = per), color = "black", fill = "lightgrey") +
  geom_errorbar(
    aes(x = Q,
            ymin = per - 1.96*SE, 
            ymax = per + 1.96*SE), 
        color = "black", width = .2
    ) +
  ggtitle("Proportion of correct identification of whether company has an EHF (weighted)") +
  ylab("Proportion Correct")
  
ggsave("4_output/plots/genpop_ehfcorrect.pdf")

# Overall answers plot
gr_pl3.2 <- gr_pl3 %>%
  select(ehf_offer_thd:ehf_offer_costco, acs_weight_trim) %>%
  pivot_longer(cols = c(ehf_offer_thd:ehf_offer_costco), 
               names_to = "Q", 
               values_to = "ans") %>%
  group_by(Q, ans) %>%
  summarize(per=sum(acs_weight_trim)/1008) %>%
  mutate(SE = sqrt(per*(1 - per)/1008)) %>%
  mutate(Q = case_match(Q, "ehf_offer_costco" ~ "Costco", 
                        "ehf_offer_disn" ~ "Disney",
                        "ehf_offer_kohls" ~ "Kohls",
                        "ehf_offer_stb" ~ "Starbucks",
                        "ehf_offer_thd" ~ "The Home Depot",
                        "ehf_offer_wal" ~ "Walmart"))
  

ggplot(gr_pl3.2, aes(x = Q, y = per, fill = ans)) +
  geom_col(color = "black", position = "dodge") +
  geom_errorbar(
    aes(
        ymin = per - 1.96*SE, 
        ymax = per + 1.96*SE), 
    color = "black", position=position_dodge(.9), width = .2
  ) +
  scale_fill_brewer("", palette = "RdYlBu") +
  ggtitle("Does the company in question offer an EHF? (weighted)") +
  ylab("Proportion")

ggsave("4_output/plots/genpop_ehfoffer.pdf")

