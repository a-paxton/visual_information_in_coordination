####### Visual Information in Coordination: Prepare for Robustness Analyses #######

# In this script, we'll knit together our various outputs from the other CRQA 
# parameters.

#############################################################################

##### 0. Preliminaries #####

# preliminaries
rm(list=ls())

# load libraries and set working directory
setwd('~/GitHub/visual_information_in_coordination/')
library(tidyverse)
library(signal)

##### 1. Plot tailored parameters' CRQA Metrics #####

# read in our data
crqa_tailored_df = read.csv(file = './data/analyses/crqa_df_tailored-vic.csv')

# plot RR
overall_RR_plot_tailored = ggplot(crqa_tailored_df,
                                  aes(x = conv_type,
                                      y = RR,
                                      color = conv_type)) +
  geom_violin() +
  facet_grid(vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference"),
             )) +
  scale_color_manual(values = c("blue", "red","gray"),
                     labels = c("Affiliative",
                                "Argumentative",
                                "Cooperative"),
                     name = "Conversation Type") + 
  scale_x_discrete(labels = c("Affiliative",
                              "Argumentative",
                              "Cooperative")) +
  theme(legend.position = "none") +
  geom_jitter(width=.2) +
  xlab("Conversation Type") +
  ylab("RR (Recurrence Rate)")+ 
  ggtitle("Inverse of noise of movement\nby condition and conversation type\n(Tailored Parameters)")
ggsave(filename = paste0('./figures/robustness/rr_results-alt_tailored-vic.png'),
       plot = overall_RR_plot_tailored,
       height = 6,
       width = 4,
       units = "in")

# plot determinism
overall_DET_plot_tailored = ggplot(crqa_tailored_df,
                                   aes(x = conv_type,
                                       y = DET,
                                       color = conv_type))  +
  geom_violin() +
  facet_grid(vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference"),
             )) +
  scale_color_manual(values = c("blue", "red","gray"),
                     labels = c("Affiliative",
                                "Argumentative",
                                "Cooperative"),
                     name = "Conversation Type") + 
  scale_x_discrete(labels = c("Affiliative",
                              "Argumentative",
                              "Cooperative")) +
  theme(legend.position = "none") +
  geom_jitter(width=.2) +
  xlab("Conversation Type") +
  ylab("DET (Determinism)") + 
  ggtitle("Structure of movement\nby condition and conversation type\n(Tailored Parameters)")
ggsave(filename = paste0('./figures/robustness/det_results-alt_tailored-vic.png'),
       plot = overall_DET_plot_tailored,
       height = 6,
       width = 4,
       units = "in")

# plot maxline
overall_maxL_plot_tailored = ggplot(crqa_tailored_df,
                                    aes(x = conv_type,
                                        y = maxL,
                                        color = conv_type)) +
  geom_violin() +
  facet_grid(vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference"),
             )) +
  scale_color_manual(values = c("blue", "red","gray"),
                     labels = c("Affiliative",
                                "Argumentative",
                                "Cooperative"),
                     name = "Conversation Type") + 
  scale_x_discrete(labels = c("Affiliative",
                              "Argumentative",
                              "Cooperative")) +
  theme(legend.position = "none") +
  geom_jitter(width=.2) +
  xlab("Conversation Type") +
  ylab("maxL (Maximum Line Length)") + 
  ggtitle("Attractor strength of movement\nby condition and conversation type\n(Tailored Parameters)")
ggsave(filename = paste0('./figures/robustness/maxl_results-alt_tailored-vic.png'),
       plot = overall_maxL_plot_tailored,
       height = 6,
       width = 4,
       units = "in")

##### 2. Plot global set 02 parameters' CRQA Metrics #####

# read in our data
crqa_opt_02_df = read.csv(file = './data/analyses/crqa_df_global_02-vic.csv')

# plot RR
overall_RR_plot_opt_02 = ggplot(crqa_opt_02_df,
                                aes(x = conv_type,
                                    y = RR,
                                    color = conv_type)) +
  geom_violin() +
  facet_grid(vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference"),
             )) +
  scale_color_manual(values = c("blue", "red","gray"),
                     labels = c("Affiliative",
                                "Argumentative",
                                "Cooperative"),
                     name = "Conversation Type") + 
  scale_x_discrete(labels = c("Affiliative",
                              "Argumentative",
                              "Cooperative")) +
  theme(legend.position = "none") +
  geom_jitter(width=.2) +
  xlab("Conversation Type") +
  ylab("RR (Recurrence Rate)")+ 
  ggtitle("Inverse of noise of movement\nby condition and conversation type\n(Global Set 02 Parameters)")
ggsave(filename = paste0('./figures/robustness/rr_results-alt_opt_02-vic.png'),
       plot = overall_RR_plot_opt_02,
       height = 6,
       width = 4,
       units = "in")

# plot determinism
overall_DET_plot_opt_02 = ggplot(crqa_opt_02_df,
                                 aes(x = conv_type,
                                     y = DET,
                                     color = conv_type))  +
  geom_violin() +
  facet_grid(vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference"),
             )) +
  scale_color_manual(values = c("blue", "red","gray"),
                     labels = c("Affiliative",
                                "Argumentative",
                                "Cooperative"),
                     name = "Conversation Type") + 
  scale_x_discrete(labels = c("Affiliative",
                              "Argumentative",
                              "Cooperative")) +
  theme(legend.position = "none") +
  geom_jitter(width=.2) +
  xlab("Conversation Type") +
  ylab("DET (Determinism)") + 
  ggtitle("Structure of movement\nby condition and conversation type\n(Global Set 02 Parameters)")
ggsave(filename = paste0('./figures/robustness/det_results-alt_opt_02-vic.png'),
       plot = overall_DET_plot_opt_02,
       height = 6,
       width = 4,
       units = "in")

# plot maxline
overall_maxL_plot_opt_02 = ggplot(crqa_opt_02_df,
                                  aes(x = conv_type,
                                      y = maxL,
                                      color = conv_type)) +
  geom_violin() +
  facet_grid(vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference"),
             )) +
  scale_color_manual(values = c("blue", "red","gray"),
                     labels = c("Affiliative",
                                "Argumentative",
                                "Cooperative"),
                     name = "Conversation Type") + 
  scale_x_discrete(labels = c("Affiliative",
                              "Argumentative",
                              "Cooperative")) +
  theme(legend.position = "none") +
  geom_jitter(width=.2) +
  xlab("Conversation Type") +
  ylab("maxL (Maximum Line Length)") + 
  ggtitle("Attractor strength of movement\nby condition and conversation type\n(Global Set 02 Parameters)")
ggsave(filename = paste0('./figures/robustness/maxl_results-alt_opt_02-vic.png'),
       plot = overall_maxL_plot_opt_02,
       height = 6,
       width = 4,
       units = "in")

##### 3. Plot tailored parameter DRPs #####

# read in the data
drp_tailored_df = read.csv(file = './data/analyses/drp_tailored_df-vic.csv')

# identify mean and SE
plot_drp_tailored_df = drp_tailored_df %>% ungroup() %>%
  group_by(lag, conv_type, condition) %>%
  summarize(RR = mean(profile),
            SE = FSA::se(profile)) %>%
  ungroup()

# plot everything
all_drp_tailored_plots = ggplot(drp_tailored_df,
                       aes(x = lag,
                           y = profile,
                           color = conv_type)) +
  geom_line(data = plot_drp_tailored_df,
            aes(x = lag,
                y = RR,
                color = conv_type),
            alpha = .3) +
  geom_ribbon(data = plot_drp_tailored_df,
              aes(x = lag,
                  y = RR,
                  ymin = RR-SE,
                  ymax = RR+SE,
                  fill = conv_type),
              linewidth = .01,
              alpha = .3)+
  geom_smooth(se=TRUE) +
  facet_grid(vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference"),
             )) +
  scale_fill_manual(values = alpha(c("blue", "red","gray"), .5),
                    labels = c("Aff.",
                               "Arg.",
                               "Coop."),
                    name = "Conversation\nType")+
  scale_color_manual(values = c("blue", "red","darkgray"),
                     labels = c("Aff.",
                                "Arg.",
                                "Coop."),
                     name = "Conversation\nType")+
  theme(legend.position = "bottom") +
  xlab("Lag (in 10Hz)") +
  ylab("RR (Recurrence Rate)")+ 
  ggtitle("Diagonal recurrence profile of movement\nby condition and conversation type\n(Tailored Parameters)")
ggsave(filename = paste0('./figures/robustness/drp_results-alt_tailored-vic.png'),
       plot = all_drp_tailored_plots,
       height = 6,
       width = 4,
       units = "in")

##### 4. Plot global set 02 parameter DRPs #####

# read in the data
drp_opt_02_df = read.csv(file = './data/analyses/drp_02_df-vic.csv')

# identify mean and SE
plot_drp_opt_02_df = drp_opt_02_df %>% ungroup() %>%
  group_by(lag, conv_type, condition) %>%
  summarize(RR = mean(profile),
            SE = FSA::se(profile)) %>%
  ungroup()

# plot everything
all_drp_opt_02_plots = ggplot(drp_opt_02_df,
                                aes(x = lag,
                                    y = profile,
                                    color = conv_type)) +
  geom_line(data = plot_drp_opt_02_df,
            aes(x = lag,
                y = RR,
                color = conv_type),
            alpha = .3) +
  geom_ribbon(data = plot_drp_opt_02_df,
              aes(x = lag,
                  y = RR,
                  ymin = RR-SE,
                  ymax = RR+SE,
                  fill = conv_type),
              linewidth = .01,
              alpha = .3)+
  geom_smooth(se=TRUE) +
  facet_grid(vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference"),
             )) +
  scale_fill_manual(values = alpha(c("blue", "red","gray"), .5),
                    labels = c("Aff.",
                               "Arg.",
                               "Coop."),
                    name = "Conversation\nType")+
  scale_color_manual(values = c("blue", "red","darkgray"),
                     labels = c("Aff.",
                                "Arg.",
                                "Coop."),
                     name = "Conversation\nType")+
  theme(legend.position = "bottom") +
  xlab("Lag (in 10Hz)") +
  ylab("RR (Recurrence Rate)")+ 
  ggtitle("Diagonal recurrence profile of movement\nby condition and conversation type\n(Global Set 02 Parameters)")
ggsave(filename = paste0('./figures/robustness/drp_results-alt_opt_02-vic.png'),
       plot = all_drp_opt_02_plots,
       height = 6,
       width = 4,
       units = "in")
