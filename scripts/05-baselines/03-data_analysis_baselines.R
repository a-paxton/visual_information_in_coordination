####### Visual Information in Coordination: Comparing Real and Surrogate #######

# In this script, we'll do our statistical analyses.

#############################################################################

##### 0. Preliminaries #####

# preliminaries
rm(list=ls())
setwd('~/GitHub/visual_information_in_coordination/')

# load libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(apaTables)
library(gtsummary)

# real data: load our interaction-level data and prepare for analyses
analysis_crqa_df = read.csv(file = './data/analyses/analysis_crqa_df-vic.csv') %>%
  dplyr::select(date, condition, conv_num, conv_type, RR, DET, maxL) %>%
  dplyr::rename("dyad" = date) %>%
  mutate(conv_type = factor(conv_type, 
                            labels = c("Affiliative", 
                                       "Argumentative", 
                                       "Cooperative"))) %>%
  mutate(condition = factor(condition, 
                            labels = c("FtF Laboratory", 
                                       "VC Remote", 
                                       "VC Laboratory"))) %>%
  mutate(condition = as.factor(condition),
         conv_type = as.factor(conv_type),
         dyad = as.factor(dyad),
         dataset = as.factor("Real")) 

# baseline data: load our interaction-level data and prepare for analyses
baseline_crqa_df = read.csv(file = './data/analyses/baseline_crqa_df-vic.csv') %>%
  dplyr::select(date, condition, conv_num, conv_type, RR, DET, maxL) %>%
  dplyr::rename("dyad" = date) %>%
  mutate(conv_type = factor(conv_type, 
                            labels = c("Affiliative", 
                                       "Argumentative", 
                                       "Cooperative"))) %>%
  mutate(condition = factor(condition, 
                            labels = c("FtF Laboratory", 
                                       "VC Remote", 
                                       "VC Laboratory"))) %>%
  mutate(condition = as.factor(condition),
         conv_type = as.factor(conv_type),
         dyad = as.factor(dyad),
         dataset = as.factor("Baseline"))

# bind them
comparison_crqa_df = rbind.data.frame(analysis_crqa_df,
                                      baseline_crqa_df)
contrasts(comparison_crqa_df$condition) = contr.treatment(3)
contrasts(comparison_crqa_df$conv_type) = contr.treatment(3)
contrasts(comparison_crqa_df$dataset) = contr.treatment(2)

##### 1. Compare real to baseline #####

# effects on recurrence rate
rr_comparison_baseline = lmer(RR ~ dataset +
                                (1 | dyad), 
                              data = comparison_crqa_df)
summary(rr_comparison_baseline)

# effects on determinism
det_comparison_baseline = lmer(DET ~ dataset +
                                 (1 | dyad), 
                               data = comparison_crqa_df)
summary(det_comparison_baseline)

# effects on maximum line
maxl_comparison_baseline = lmer(maxL ~ dataset +
                                  (1 | dyad), 
                                data = comparison_crqa_df)
summary(maxl_comparison_baseline)

##### 2. Plot the results #####

# plot: comparing baseline and observed determinism
plot_DET_dataset_comparison = ggplot(comparison_crqa_df,
                                     aes(y = DET,
                                         x = conv_type,
                                         color = conv_type)) +
  geom_violin() +
  facet_grid(cols = vars(dataset)) +
  scale_color_manual(values = c("blue", "red","darkgray"),
                     labels = c("Aff.",
                                "Arg.",
                                "Coop."),
                     name = "Conversation Type")+
  theme(legend.position = "none") +
  geom_jitter(width=.1, alpha = .3) + 
  xlab("Conversation Type") + 
  ylab("DET (Determinism)")+ 
  ggtitle("Baseline comparison:\nDeterminism by condition and conversation type")
ggsave(filename = paste0('./figures/baselines/plot_DET_dataset_comparison-vic.png'),
       plot = plot_DET_dataset_comparison,
       height = 6,
       width = 6,
       units = "in")

# plot: comparing baseline and observed RR
plot_RR_dataset_comparison = ggplot(comparison_crqa_df,
                                     aes(y = RR,
                                         x = conv_type,
                                         color = conv_type)) +
  geom_violin() +
  facet_grid(cols = vars(dataset)) +
  scale_color_manual(values = c("blue", "red","darkgray"),
                     labels = c("Aff.",
                                "Arg.",
                                "Coop."),
                     name = "Conversation Type")+
  theme(legend.position = "none") +
  geom_jitter(width=.1, alpha = .3) + 
  xlab("Conversation Type") + 
  ylab("RR (RRerminism)")+ 
  ggtitle("Baseline comparison:\nRecurrence rate by condition and conversation type")
ggsave(filename = paste0('./figures/baselines/plot_RR_dataset_comparison-vic.png'),
       plot = plot_RR_dataset_comparison,
       height = 6,
       width = 6,
       units = "in")

# plot: comparing baseline and observed maxL
plot_maxL_dataset_comparison = ggplot(comparison_crqa_df,
                                    aes(y = maxL,
                                        x = conv_type,
                                        color = conv_type)) +
  geom_violin() +
  facet_grid(cols = vars(dataset)) +
  scale_color_manual(values = c("blue", "red","darkgray"),
                     labels = c("Aff.",
                                "Arg.",
                                "Coop."),
                     name = "Conversation Type")+
  theme(legend.position = "none") +
  geom_jitter(width=.1, alpha = .3) + 
  xlab("Conversation Type") + 
  ylab("maxL (Maximum Line Length)")+ 
  ggtitle("Baseline comparison:\nMaximum line length by condition and conversation type")
ggsave(filename = paste0('./figures/baselines/plot_maxL_dataset_comparison-vic.png'),
       plot = plot_maxL_dataset_comparison,
       height = 6,
       width = 6,
       units = "in")
