####### Visual Information in Coordination: Data Analysis #######

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

# load our interaction-level data and prepare for analyses
analysis_crqa_df = read.csv(file = './data/analyses/analysis_crqa_df-vic.csv') %>%
  dplyr::select(date, condition, conv_num, conv_type, RR, DET, maxL) %>%
  dplyr::rename("dyad" = date) %>%
  mutate(condition = as.factor(condition),
         conv_type = as.factor(conv_type),
         dyad = as.factor(dyad))
contrasts(analysis_crqa_df$condition) = contr.treatment(3)
contrasts(analysis_crqa_df$conv_type) = contr.treatment(3)

# load our moment-to-moment data and prepare for analyses
analysis_drp_df = read.csv(file = './data/analyses/analysis_drp_df-vic.csv') %>%
  dplyr::select(date, condition, conv_num, conv_type, lag, profile) %>%
  dplyr::rename("dyad" = date,
                "RR" = profile) %>%
  mutate(condition = as.factor(condition),
         conv_type = as.factor(conv_type),
         dyad = as.factor(dyad))
contrasts(analysis_drp_df$condition) = contr.treatment(3)
contrasts(analysis_drp_df$conv_type) = contr.treatment(3)

# create first- and second-order orthogonal polynomials for lag
raw_lag = min(analysis_drp_df$lag):max(analysis_drp_df$lag)
lag_vals = data.frame(raw_lag)
lag_offset = (0-min(raw_lag)) + 1
t = stats::poly((raw_lag + lag_offset), 2)
lag_vals[, paste("ot", 1:2, sep="")] = t[lag_vals$raw_lag + lag_offset, 1:2]
analysis_drp_df = left_join(analysis_drp_df,lag_vals, by = c("lag" = "raw_lag"))

# read in our survey metrics
analysis_survey_df = read.csv('./data/surveys/COVID_ACTA_Survey_ConversationFeelings_Organized.csv') %>%
  mutate(dyad = str_remove_all(Participant,'[ABab]')) %>%
  pivot_longer(cols = Aff1:Coop3,
               names_to = "question",
               values_to = "rating") %>%
  mutate(condition = as.factor(Condition)) %>%
  mutate(conv_type = as.factor(str_to_lower(str_remove_all(question,'[:digit:]')))) %>%
  mutate(question = ifelse(grepl('1', question),
                           "felt_close",
                           ifelse(grepl('2',question),
                                  "understood_partner",
                                  "understood_me"))) %>%
  mutate(dyad = as.factor(dyad),
         Participant = as.factor(Participant))
analysis_survey_df = as.data.frame(analysis_survey_df)
contrasts(analysis_survey_df$condition) = contr.treatment(3)
contrasts(analysis_survey_df$conv_type) = contr.treatment(3)

##### 1. Interaction-level metrics (H1-H3; 3.4.1) #####

# effects on recurrence rate
rr_anova = aov(RR ~ conv_type * condition, data = analysis_crqa_df)
summary(rr_anova)

# effects on determinism
det_anova = aov(DET ~ conv_type * condition, data = analysis_crqa_df)
summary(det_anova)

# effects on maximum line
maxl_anova = aov(maxL ~ conv_type * condition, data = analysis_crqa_df)
summary(maxl_anova)

##### 2. Interaction-level metrics (H1-H3; 3.4.2) #####

drp_lmer = lmer(RR ~ ot1 * ot2 * conv_type * condition + 
                  (1 + conv_type | dyad), 
                data = analysis_drp_df)
summary(drp_lmer)

##### 3. Comparing outcomes (H4-H5; 3.4.3) #####

analysis_close_df = analysis_survey_df %>% ungroup() %>%
  dplyr::filter(question == "felt_close")
close_lmer = lmer(rating ~ conv_type * condition + 
                    (1 + conv_type | dyad) + (1 | Participant), 
                  data = analysis_close_df)
summary(close_lmer)

analysis_understood_partner_df = analysis_survey_df %>% ungroup() %>%
  dplyr::filter(question == "understood_partner")
undersood_partner_lmer = lmer(rating ~ conv_type * condition + 
                                (1 | dyad) + (1 | Participant), 
                              data = analysis_understood_partner_df)
summary(undersood_partner_lmer)

analysis_understood_me_df = analysis_survey_df %>% ungroup() %>%
  dplyr::filter(question == "understood_me")
understood_me_lmer = lmer(rating ~ conv_type * condition + 
                            (1 | dyad) + (1 | Participant), 
                          data = analysis_understood_me_df)
summary(understood_me_lmer)

##### 3. Exploratory: Dynamics impacting outcomes (E1; 3.5) #####

# grab dyad-wise means for each subjective experience
mean_closeness_df = analysis_close_df %>% ungroup() %>%
  group_by(dyad, condition, conv_type) %>%
  summarize(mean_closeness = mean(as.numeric(rating)))
mean_understood_partner_df = analysis_understood_partner_df %>% ungroup() %>%
  group_by(dyad, condition, conv_type) %>%
  summarize(mean_understood_partner = mean(as.numeric(rating)))
mean_understood_me_df = analysis_understood_me_df %>% ungroup() %>%
  group_by(dyad, condition, conv_type) %>%
  summarize(mean_understood_me = mean(as.numeric(rating)))

# bind with CRQA metrics
analysis_exploratory_crqa_df = left_join(analysis_crqa_df,mean_closeness_df,
                                         by = c("dyad", "condition", "conv_type")) %>%
  left_join(., mean_understood_partner_df,
            by = c("dyad", "condition", "conv_type")) %>%
  left_join(., mean_understood_me_df,
            by = c("dyad", "condition", "conv_type")) %>%
  mutate(condition = as.factor(condition),
         conv_type = as.factor(conv_type),
         dyad = as.factor(dyad))
contrasts(analysis_exploratory_crqa_df$condition) = contr.treatment(3)
contrasts(analysis_exploratory_crqa_df$conv_type) = contr.treatment(3)

# bind with DRPs analyses
analysis_exploratory_drp_df = left_join(analysis_drp_df,mean_closeness_df,
                                        by = c("dyad", "condition", "conv_type")) %>%
  left_join(., mean_understood_partner_df,
            by = c("dyad", "condition", "conv_type")) %>%
  left_join(., mean_understood_me_df,
            by = c("dyad", "condition", "conv_type")) %>%
  mutate(condition = as.factor(condition),
         conv_type = as.factor(conv_type),
         dyad = as.factor(dyad))
contrasts(analysis_exploratory_drp_df$condition) = contr.treatment(3)
contrasts(analysis_exploratory_drp_df$conv_type) = contr.treatment(3)

# interaction-level analyses
exploratory_close_crqa_lmer = lmer(mean_closeness ~ (RR + DET + maxL) * conv_type * condition + 
                                     (1 | dyad),
                                   data = analysis_exploratory_crqa_df)
summary(exploratory_close_crqa_lmer)
exploratory_understood_partner_crqa_lmer = lmer(mean_understood_partner ~ (RR + DET + maxL) * 
                                                  conv_type * condition + 
                                                  (1 | dyad),
                                                data = analysis_exploratory_crqa_df)
summary(exploratory_understood_partner_crqa_lmer)
exploratory_understood_me_crqa_lmer = lmer(mean_understood_me ~ (RR + DET + maxL) * 
                                             conv_type * condition + 
                                             (1 | dyad),
                                           data = analysis_exploratory_crqa_df)
summary(exploratory_understood_me_crqa_lmer)

# plot: closeness by determinsim
plot_e1_crqa_closeness_ylim = ggplot(analysis_exploratory_crqa_df,
                                     aes(y = DET,
                                         x = mean_closeness,
                                         color = conv_type)) +
  geom_violin() +
  facet_grid(cols = vars(conv_type),
             rows = vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference",
                 "aff" = "Affiliative",
                 "arg" = "Argumentative",
                 "coop" = "Cooperative")
             )) +
  scale_color_manual(values = c("blue", "red","darkgray"),
                     labels = c("Aff.",
                                "Arg.",
                                "Coop."),
                     name = "Conversation\nType")+
  theme(legend.position = "bottom") +
  geom_jitter(width=.1) + 
  coord_cartesian(xlim = c(2.5,6),
                  ylim=c(80,100))+
  xlab("Mean Closeness Rating") + 
  ylab("DET (Determinism)")+ 
  ggtitle("Mean ratings of closeness by determinism,\ncondition, and conversation type")
ggsave(filename = paste0('./figures/e1_crqa_closeness_ylim-vic.png'),
       plot = plot_e1_crqa_closeness_ylim,
       height = 6,
       width = 6,
       units = "in")

# plot: understood partner by determinsim
plot_e1_crqa_understood_partner_ylim = ggplot(analysis_exploratory_crqa_df,
                                              aes(y = DET,
                                                  x = mean_understood_partner,
                                                  color = conv_type)) +
  geom_violin() +
  facet_grid(cols = vars(conv_type),
             rows = vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference",
                 "aff" = "Affiliative",
                 "arg" = "Argumentative",
                 "coop" = "Cooperative")
             )) +
  scale_color_manual(values = c("blue", "red","darkgray"),
                     labels = c("Aff.",
                                "Arg.",
                                "Coop."),
                     name = "Conversation\nType")+
  theme(legend.position = "bottom") + 
  geom_jitter(width=.1) +
  xlab("Mean Perception of Understanding") + 
  ylab("DET (Determinism)") + 
  coord_cartesian(xlim = c(2.5,6),
                  ylim=c(80,100))+
  ggtitle("Mean perception of having understood their partner\n by determinism, condition, and conversation type")
ggsave(filename = paste0('./figures/e1_crqa_understood_partner_ylim-vic.png'),
       plot = plot_e1_crqa_understood_partner_ylim,
       height = 6,
       width = 6,
       units = "in")

# plot: understood me by determinsim
plot_e1_crqa_understood_me_ylim = ggplot(analysis_exploratory_crqa_df,
                                         aes(y = DET,
                                             x = mean_understood_me,
                                             color = conv_type)) +
  geom_violin() +
  facet_grid(cols = vars(conv_type),
             rows = vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference",
                 "aff" = "Affiliative",
                 "arg" = "Argumentative",
                 "coop" = "Cooperative")
             )) +
  scale_color_manual(values = c("blue", "red","darkgray"),
                     labels = c("Aff.",
                                "Arg.",
                                "Coop."),
                     name = "Conversation\nType")+
  theme(legend.position = "bottom") +
  geom_jitter(width=.1) +
  xlab("Mean Perception of Being Understood") + 
  ylab("DET (Determinism)") + 
  coord_cartesian(xlim = c(2.5,6),
                  ylim=c(80,100))+
  ggtitle("Mean perception of having been understood\n by determinism, condition, and conversation type")
ggsave(filename = paste0('./figures/e1_crqa_understood_me_ylim-vic.png'),
       plot = plot_e1_crqa_understood_me_ylim,
       height = 6,
       width = 6,
       units = "in")

# dynamics analyses
exploratory_close_drp_lmer = lmer(mean_closeness ~ (ot1 + ot2) * conv_type * condition + 
                                    (1 | dyad),
                                  data = analysis_exploratory_drp_df)
summary(exploratory_close_drp_lmer)
exploratory_understood_partner_drp_lmer = lmer(mean_understood_partner ~ (ot1 + ot2) * 
                                                 conv_type * condition + 
                                                 (1 | dyad),
                                               data = analysis_exploratory_drp_df)
summary(exploratory_understood_partner_drp_lmer)
exploratory_understood_me_drp_lmer = lmer(mean_understood_me ~ (ot1 + ot2) * 
                                            conv_type * condition + 
                                            (1 | dyad),
                                          data = analysis_exploratory_drp_df)
summary(exploratory_understood_me_drp_lmer)

# create plotting dataframes for DRPs
plot_exploratory_drp_df = analysis_exploratory_drp_df %>%
  drop_na() %>%
  mutate(plot_closeness = mean_closeness >= median(mean_closeness),
         plot_understood_partner = mean_understood_partner >= median(mean_understood_partner, na.rm=TRUE),
         plot_understood_me = mean_understood_me >= median(mean_understood_me))
plot_closeness_drp_df = plot_exploratory_drp_df %>% ungroup() %>%
  group_by(lag, conv_type, condition, plot_closeness) %>%
  summarize(mean_RR = mean(RR, na.rm=TRUE),
            SE = FSA::se(RR, na.rm=TRUE)) %>%
  ungroup()
plot_understood_partner_drp_df = plot_exploratory_drp_df %>% ungroup() %>%
  group_by(lag, conv_type, condition, plot_understood_partner) %>%
  summarize(mean_RR = mean(RR, na.rm=TRUE),
            SE = FSA::se(RR, na.rm=TRUE)) %>%
  ungroup()
plot_understood_me_drp_df = plot_exploratory_drp_df %>% ungroup() %>%
  group_by(lag, conv_type, condition, plot_understood_me) %>%
  summarize(mean_RR = mean(RR, na.rm=TRUE),
            SE = FSA::se(RR, na.rm=TRUE)) %>%
  ungroup()

# plot: closeness DRPs
plot_e1_drp_closeness = ggplot(plot_exploratory_drp_df,
                               aes(x = lag,
                                   y = RR,
                                   color = conv_type)) +
  geom_line(data = plot_closeness_drp_df,
            aes(x = lag,
                y = mean_RR,
                color = conv_type),
            alpha = .3) +
  geom_ribbon(data = plot_closeness_drp_df,
              aes(x = lag,
                  y = mean_RR,
                  ymin = mean_RR-SE,
                  ymax = mean_RR+SE,
                  fill = conv_type),
              linewidth = .01,
              alpha = .3)+
  geom_smooth(se=TRUE) +
  facet_grid(cols = vars(plot_closeness),
             rows = vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference",
                 "TRUE" = "High Closeness",
                 "FALSE" = "Low Closeness",
                 "aff" = "Affiliative",
                 "arg" = "Argumentative",
                 "coop" = "Cooperative")
             )) +
  scale_fill_manual(values = alpha(c("blue", "red","gray"), .5),
                    labels = c("Aff.",
                               "Arg.",
                               "Coop."),
                    name = "Conversation Type")+
  scale_color_manual(values = c("blue", "red","darkgray"),
                     labels = c("Aff.",
                                "Arg.",
                                "Coop."),
                     name = "Conversation Type")+
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,0.12)) +
  xlab("Lag (in 10Hz)") +
  ylab("RR (Recurrence Rate)")+ 
  ggtitle("Diagonal recurrence profile of movement\nby ratings of closeness, condition, and conversation type")
ggsave(filename = paste0('./figures/e1_drp_closeness-vic.png'),
       plot = plot_e1_drp_closeness,
       height = 6,
       width = 6,
       units = "in")

# plot: understood partner DRPs
plot_e1_drp_understood_partner = ggplot(plot_exploratory_drp_df,
                                        aes(x = lag,
                                            y = RR,
                                            color = conv_type)) +
  geom_line(data = plot_understood_partner_drp_df,
            aes(x = lag,
                y = mean_RR,
                color = conv_type),
            alpha = .3) +
  geom_ribbon(data = plot_understood_partner_drp_df,
              aes(x = lag,
                  y = mean_RR,
                  ymin = mean_RR-SE,
                  ymax = mean_RR+SE,
                  fill = conv_type),
              linewidth = .01,
              alpha = .3) +
  geom_smooth(se=TRUE) +
  facet_grid(cols = vars(plot_understood_partner),
             rows = vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference",
                 "TRUE" = "High Understanding",
                 "FALSE" = "Low Understanding",
                 "aff" = "Affiliative",
                 "arg" = "Argumentative",
                 "coop" = "Cooperative")
             )) +
  scale_fill_manual(values = alpha(c("blue", "red","gray"), .5),
                    labels = c("Aff.",
                               "Arg.",
                               "Coop."),
                    name = "Conversation Type")+
  scale_color_manual(values = c("blue", "red","darkgray"),
                     labels = c("Aff.",
                                "Arg.",
                                "Coop."),
                     name = "Conversation Type")+
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,0.12)) +
  xlab("Lag (in 10Hz)") +
  ylab("RR (Recurrence Rate)")+ 
  ggtitle("Diagonal recurrence profile of movement\nby ratings of understanding, condition, and conversation type")
ggsave(filename = paste0('./figures/e1_drp_understood_partner-vic.png'),
       plot = plot_e1_drp_understood_partner,
       height = 6,
       width = 6,
       units = "in")

# plot: felt understood DRPs
plot_e1_drp_understood_me = ggplot(plot_exploratory_drp_df,
                                   aes(x = lag,
                                       y = RR,
                                       color = conv_type)) +
  geom_line(data = plot_understood_me_drp_df,
            aes(x = lag,
                y = mean_RR,
                color = conv_type),
            alpha = .3) +
  geom_ribbon(data = plot_understood_me_drp_df,
              aes(x = lag,
                  y = mean_RR,
                  ymin = mean_RR-SE,
                  ymax = mean_RR+SE,
                  fill = conv_type),
              linewidth = .01,
              alpha = .3) +
  geom_smooth(se=TRUE) +
  facet_grid(cols = vars(plot_understood_me),
             rows = vars(condition),
             labeller = as_labeller(
               c("FF" = "Laboratory\nFace-to-Face", 
                 "ZR" = "Remote\nVideoconference",
                 "ZT" = "Laboratory\nVideoconference",
                 "TRUE" = "High Feeling of Being Understood",
                 "FALSE" = "Low Feeling of Being Understood",
                 "aff" = "Affiliative",
                 "arg" = "Argumentative",
                 "coop" = "Cooperative")
             )) +
  scale_fill_manual(values = alpha(c("blue", "red","gray"), .5),
                    labels = c("Aff.",
                               "Arg.",
                               "Coop."),
                    name = "Conversation Type")+
  scale_color_manual(values = c("blue", "red","darkgray"),
                     labels = c("Aff.",
                                "Arg.",
                                "Coop."),
                     name = "Conversation Type")+
  theme(legend.position = "bottom") +
  coord_cartesian(ylim = c(0,0.12)) +
  xlab("Lag (in 10Hz)") +
  ylab("RR (Recurrence Rate)")+ 
  ggtitle("Diagonal recurrence profile of movement\nby ratings of feeling understood, condition, and conversation type")
ggsave(filename = paste0('./figures/e1_drp_understood_me-vic.png'),
       plot = plot_e1_drp_understood_me,
       height = 6,
       width = 6,
       units = "in")