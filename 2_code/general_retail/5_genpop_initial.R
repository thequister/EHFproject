library(here)
source(here::here('2_code', 'general_retail', '2_data_format_gr.R'))
here::i_am("2_code/general_retail/3_weights_gr.R")

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
  theme_bw() +
  theme(legend.position = "")
  
ggsave("4_output/plots/genpop_ehfaware.pdf")

#### EHF Likert Plot -----

# Code from https://scisley.github.io/articles/ggplot-likert-plot/

gr_pl2 <- gr %>%
  select(ehf_support_exist, ehf_wrk_new, ehf_support_new, ehf_wrk_exist) 

levels(gr_pl2$ehf_wrk_exist) <- c("Only management control", "More management control", 
                                  "Equal control", "More worker control", "Only worker control")
levels(gr_pl2$ehf_wrk_new) <- c("Only management control", "More management control", 
                                "Equal control", "More worker control", "Only worker control")

names(gr_pl2) <- c("Support for Existing EHF", "Control of Existing EHF", 
                   "Support for New EHF", "Control of New EHF")

gr_pl2 <- gr_pl2 %>%
  gather("Q", "ans") %>%
  filter(!is.na(ans)) %>%
  group_by(Q, ans) %>%
  summarize(n=n()) %>%
  mutate(per = n/sum(n), ans = factor(ans, levels=c("Only management control", "More management control", 
                                                    "Equal control", "More worker control", "Only worker control", 
                                                    "Not at all supportive", "Moderately supportive", "Extremely supportive"))) %>%
  arrange(Q, ans)

stage1 <- gr_pl2 %>%
  mutate(text = paste0(formatC(100 * per, format="f", digits=0), "%"),
         cs = cumsum(per),
         offset = sum(per[1:(floor(n()/2))]) + (n() %% 2)*0.5*(per[ceiling(n()/2)]),
         xmax = -offset + cs,
         xmin = xmax-per) %>%
  ungroup()

gap <- 0.2

stage2 <- stage1 %>%
  left_join(stage1 %>%
              group_by(Q) %>%
              summarize(max.xmax = max(xmax)) %>%
              mutate(r = c(1, 2, 4, 3)),
            by = "Q") %>%
  arrange(desc(r)) %>%
  mutate(ymin = r - (1-gap)/2,
         ymax = r + (1-gap)/2) %>%
  mutate(category = ifelse(grepl("Control", Q, fixed = T), "Worker Control", "EHF Support")) %>%
  arrange(Q)

percent <- c("-100%", "-75%", "-50%", "-25%", "0%", 
             "25%", "50%", "75%", "100%")


wrk_plot <- ggplot(filter(stage2, category == "Worker Control")) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill=ans)) +
  geom_text(aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=text), size = 3) +
  scale_x_continuous("", labels=percent, breaks=seq(-1, 1, len=9), limits=c(-1, 1)) +
  scale_y_continuous("", breaks = 1:2,
                     labels=rev(filter(stage2, category == "Worker Control") %>% distinct(Q) %>% .$Q)) +
  scale_fill_brewer("", palette = "RdYlBu") +
  annotate("text", x=1, y=2, label= "N = 811") + 
  annotate("text", x=1, y=1, label= "N = 197")

supp_plot <- ggplot(filter(stage2, category == "EHF Support")) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill=ans)) +
  geom_text(aes(x=(xmin+xmax)/2, y=(ymin+ymax)/2, label=text), size = 3) +
  scale_x_continuous("", labels=percent, breaks=seq(-1, 1, len=9), limits=c(-1, 1)) +
  scale_y_continuous("", breaks = 3:4,
                     labels=rev(filter(stage2, category == "EHF Support") %>% distinct(Q) %>% .$Q)) +
  scale_fill_brewer("", palette = "RdYlBu") + 
  annotate("text", x=1, y=4, label= "N = 811") + 
  annotate("text", x=1, y=3, label= "N = 197")

plot2 <- grid.arrange(wrk_plot, supp_plot)

ggsave("4_output/plots/genpop_ehfcontrol.pdf", plot = plot2)

