####### Visual Information in Coordination: Prepare for Analyses #######

# In this script, we'll knit together our various outputs from the previous
# scripts (including raw data, CRQA, and DRP files) and plot each of them.
# We'll also compare among the three parameter sets to choose the one that
# we'll present in the text of the paper (although stability analyses
# will be conducted to ensure similar patterns of results across all three
# parameter sets).

#############################################################################

##### 0. Preliminaries #####

# preliminaries
rm(list=ls())

# load libraries and set working directory
setwd('~/GitHub/visual_information_in_coordination/')
library(tidyverse)
library(signal)

##### 1. CRQA Metrics #####

# bind crqa dataframe
baseline_crqa_files = list.files(path = './data/baselines/',
                                 pattern = "crqa*",
                                 full.names = TRUE)
baseline_crqa_df <- do.call(rbind,
                            lapply(baseline_crqa_files,read.csv))

# identify which folks we'll keep (to remove the untailored videos)
keep_videos = unique(baseline_crqa_df$conv)

# clean up the CRQA dataframe
baseline_crqa_df = baseline_crqa_df %>% ungroup() %>%
  mutate(conv = gsub("_video",
                     "",
                     conv,
                     ignore.case = TRUE)) %>%
  separate(conv,
           sep = "_",
           into = c("date",
                    "condition",
                    "conv_num",
                    "conv_type",
                    "trim"),
           fill = "right") %>%
  mutate(conv_type = stringr::str_to_lower(conv_type)) %>%
  mutate(params = "tailored")

# save
write.csv(x = baseline_crqa_df,
          file = './data/analyses/baseline_crqa_df-vic.csv',
          row.names = FALSE)

# show the ranges of RR
hist(baseline_crqa_df$RR)

##### 2. Plot CRQA metrics #####

# plot overall metrics
baseline_overall_RR_plot = ggplot(baseline_crqa_df,
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
  ggtitle("Phase-randomized surrogates:\nInverse of noise of movement\nby condition and conversation type")
ggsave(filename = paste0('./figures/baselines/baseline-rr_results-vic.png'),
       plot = baseline_overall_RR_plot,
       height = 6,
       width = 4,
       units = "in")

baseline_overall_DET_plot = ggplot(baseline_crqa_df,
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
  ggtitle("Phase-randomized surrogates:\nStructure of movement\nby condition and conversation type")
ggsave(filename = paste0('./figures/baselines/baseline-det_results-vic.png'),
       plot = baseline_overall_DET_plot,
       height = 6,
       width = 4,
       units = "in")

baseline_overall_maxL_plot = ggplot(baseline_crqa_df,
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
  ggtitle("Phase-randomized surrogates:\nAttractor strength of movement\nby condition and conversation type")
ggsave(filename = paste0('./figures/baselines/baseline-maxl_results-vic.png'),
       plot = baseline_overall_maxL_plot,
       height = 6,
       width = 4,
       units = "in")