####### Visual Information in Coordination: Post Hoc Power Analyses #######

# In this script, we'll do post-hoc power analyses.

#############################################################################

##### 0. Preliminaries #####

# preliminaries
rm(list=ls())
setwd('~/GitHub/visual_information_in_coordination/')

# source things we'll need
source('./scripts/03-data_analysis/04-data_analysis-supplementary_code-vic.R')

##### 0a. Prepare interaction-level dataset #####

# load our interaction-level data and prepare for analyses
analysis_crqa_df = read.csv(file = './data/analyses/analysis_crqa_df-vic.csv') %>%
  dplyr::select(date, condition, conv_num, conv_type, RR, DET, maxL) %>%
  
  # extract conversation number variable
  rename(order_var_num = conv_num) %>%
  rowwise() %>%
  mutate(conv_num = order_key_df$conv_num[order_key_df$order_var == order_var_num &
                                            order_key_df$value == conv_type]) %>%
  ungroup() %>%
  
  # treat factors as factors
  dplyr::rename("dyad" = date) %>%
  mutate(conv_type = factor(conv_type, 
                            labels = c("Affiliative", # level 1
                                       "Argumentative",  # level 2
                                       "Cooperative"))) %>% # level 3
  mutate(condition = factor(condition, 
                            labels = c("FTF Laboratory", # level 1
                                       "VC Remote",  # level 2
                                       "VC Laboratory"))) %>% # level 3
  mutate(condition = as.factor(condition),
         conv_type = as.factor(conv_type),
         dyad = as.factor(dyad))
contrasts(analysis_crqa_df$condition) = contr.treatment(3)
contrasts(analysis_crqa_df$conv_type) = contr.treatment(3)

##### 0b. Prepare moment-to-moment dataset #####

# load our moment-to-moment data and prepare for analyses
analysis_drp_df = read.csv(file = './data/analyses/analysis_drp_df-vic.csv') %>%
  dplyr::select(date, condition, conv_num, conv_type, lag, profile) %>%
  
  # extract conversation number variable
  rename(order_var_num = conv_num) %>%
  rowwise() %>%
  mutate(conv_num = order_key_df$conv_num[order_key_df$order_var == order_var_num &
                                            order_key_df$value == conv_type]) %>%
  ungroup() %>%
  
  # treat factors as factors
  dplyr::rename("dyad" = date,
                "RR" = profile)  %>%
  mutate(conv_type = factor(conv_type, 
                            labels = c("Affiliative", 
                                       "Argumentative", 
                                       "Cooperative"))) %>%
  mutate(condition = factor(condition, 
                            labels = c("FTF Laboratory", 
                                       "VC Remote", 
                                       "VC Laboratory"))) %>%
  mutate(condition = as.factor(condition),
         conv_type = as.factor(conv_type),
         dyad = as.factor(dyad))
contrasts(analysis_drp_df$condition) = contr.treatment(3)
contrasts(analysis_drp_df$conv_type) = contr.treatment(3)

# reorganize DRPs so that we always have the leader (if there is one) on one side
leadership_test_df = analysis_drp_df %>% ungroup() %>%
  dplyr::filter(lag != 0) %>%
  mutate(side = ifelse(lag < 0,
                       0,
                       1)) %>%
  group_by(dyad, conv_type, condition, side) %>%
  summarize(mean_leadership = mean(RR)) %>%
  pivot_wider(names_from = "side",
              names_prefix = "side_",
              values_from = mean_leadership) %>%
  ungroup() %>%
  
  # figure out who's leading
  mutate(mean_leader_direction = side_0 - side_1) %>%
  dplyr::filter(mean_leader_direction != 0) %>%
  mutate(leader = ifelse(mean_leader_direction > 0,
                         0,
                         1)) %>%
  
  # only keep the folks who we'd need to swap
  dplyr::filter(leader == 1) %>%
  mutate(leader = -1) %>% 
  dplyr::select(dyad, conv_type, condition, leader)

# merge and convert
analysis_drp_df = left_join(analysis_drp_df, 
                            leadership_test_df,
                            by = c("dyad", "condition", "conv_type")) %>%
  mutate(lag = ifelse(!is.na(leader),
                      lag * leader,
                      lag)) %>%
  arrange(dyad, condition, conv_num, conv_type, lag)

# create first- and second-order orthogonal polynomials for lag
raw_lag = min(analysis_drp_df$lag):max(analysis_drp_df$lag)
lag_vals = data.frame(raw_lag)
lag_offset = (0-min(raw_lag)) + 1
t = stats::poly((raw_lag + lag_offset), 2)
lag_vals[, paste("ot", 1:2, sep="")] = t[lag_vals$raw_lag + lag_offset, 1:2]
analysis_drp_df = left_join(analysis_drp_df,lag_vals, by = c("lag" = "raw_lag"))

##### 0c. Prepare survey dataset #####

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

##### 1a. RR #####

# log-transform the data
analysis_crqa_df = analysis_crqa_df %>%
  mutate(log_RR = log(RR))

# create log RR model
model_log_rr = lmer(log_RR ~ conv_type * condition + (1 | dyad), 
                    data = analysis_crqa_df)

##### 1b. DET #####

# create DET model
model_det = lmer(DET ~ conv_type * condition + (1 | dyad), 
                 data = analysis_crqa_df)

##### 1c. maxL #####

# create maxL model
model_maxL = lmer(maxL ~ conv_type * condition + (1 | dyad), 
                  data = analysis_crqa_df)

##### 2. Moment-to-moment metrics (H1-H3; 3.4.2) #####

# build a model
model_drp = lmer(RR ~ ot1 * ot2 * conv_type * condition + 
                   (1 + conv_type | dyad), 
                 data = analysis_drp_df)

##### 3. Comparing outcomes (H4-H5; 3.4.3) #####

# dataframe for closeness
analysis_close_df = analysis_survey_df %>% ungroup() %>%
  dplyr::filter(question == "felt_close") %>%
  mutate(conv_type = factor(conv_type, 
                            labels = c("Affiliative", 
                                       "Argumentative", 
                                       "Cooperative"))) %>%
  mutate(condition = factor(condition, 
                            labels = c("FTF Laboratory", 
                                       "VC Remote", 
                                       "VC Laboratory")))
contrasts(analysis_close_df$condition) = contr.treatment(3)
contrasts(analysis_close_df$conv_type) = contr.treatment(3)

# dataframe for feelings of understanding partner
analysis_understood_partner_df = analysis_survey_df %>% ungroup() %>%
  dplyr::filter(question == "understood_partner") %>%
  mutate(conv_type = factor(conv_type, 
                            labels = c("Affiliative", 
                                       "Argumentative", 
                                       "Cooperative"))) %>%
  mutate(condition = factor(condition, 
                            labels = c("FTF Laboratory", 
                                       "VC Remote", 
                                       "VC Laboratory")))
contrasts(analysis_understood_partner_df$condition) = contr.treatment(3)
contrasts(analysis_understood_partner_df$conv_type) = contr.treatment(3)

# dataframe for feelings of being understood
analysis_understood_me_df = analysis_survey_df %>% ungroup() %>%
  dplyr::filter(question == "understood_me") %>%
  mutate(conv_type = factor(conv_type, 
                            labels = c("Affiliative", 
                                       "Argumentative", 
                                       "Cooperative"))) %>%
  mutate(condition = factor(condition, 
                            labels = c("FTF Laboratory", 
                                       "VC Remote", 
                                       "VC Laboratory")))
contrasts(analysis_understood_me_df$condition) = contr.treatment(3)
contrasts(analysis_understood_me_df$conv_type) = contr.treatment(3)

##### 3a. Feelings of closeness #####

# run analysis
model_closeness = lmer(rating ~ conv_type * condition + 
                         (1 + conv_type | dyad) + (1 | Participant), 
                       data = analysis_close_df)

##### 3b. Feelings of understanding partner #####

# run analysis
model_understood_partner = lmer(rating ~ conv_type * condition + 
                                  (1 | dyad) + (1 | Participant), 
                                data = analysis_understood_partner_df)

##### 3c. Feelings of being understood #####

# run analysis
model_understood_me = lmer(rating ~ conv_type * condition + 
                             (1 | dyad) + (1 | Participant), 
                           data = analysis_understood_me_df)

##### 4. Exploratory: Interaction with Conversation Order #####

# snag maximum RR within window
max_RR_drp_df = analysis_drp_df %>% ungroup() %>%
  group_by(dyad, condition, conv_type) %>%
  summarize(max_drp_rr = max(RR))

# bind with CRQA metrics
analysis_exploratory_df = left_join(analysis_crqa_df,analysis_close_df,
                                    by = c("dyad", "condition", "conv_type")) %>%
  rename(closeness = rating) %>%
  dplyr::select(-Condition, -Order, -question) %>%
  left_join(., analysis_understood_partner_df, 
            by = c("dyad", "condition", "conv_type", "Participant")) %>%
  rename(understood_partner = rating) %>%
  dplyr::select(-Condition, -Order, -question) %>%
  left_join(., analysis_understood_me_df,
            by = c("dyad", "condition", "conv_type", "Participant")) %>%
  rename(understood_me = rating) %>%
  dplyr::select(-Condition, -Order, -question) %>%
  left_join(., max_RR_drp_df,
            by = c("dyad", "condition", "conv_type")) %>%
  
  # rescale RQA metrics given different values
  mutate(across(c(log_RR, DET, maxL, max_drp_rr), ~ as.numeric(scale(as.numeric(.x))))) %>%
  
  # contrast-code
  mutate(conv_type = factor(conv_type, 
                            labels = c("Affiliative", 
                                       "Argumentative", 
                                       "Cooperative"))) %>%
  mutate(condition = factor(condition, 
                            labels = c("FTF Laboratory", 
                                       "VC Remote", 
                                       "VC Laboratory"))) %>%
  mutate(condition = as.factor(condition),
         conv_type = as.factor(conv_type),
         dyad = as.factor(dyad),
         participant = as.factor(Participant)) 
contrasts(analysis_exploratory_df$condition) = contr.treatment(3)
contrasts(analysis_exploratory_df$conv_type) = contr.treatment(3)

# add order along to RR and DET
model_exploratory_with_order = lmer(closeness ~ (log_RR + DET) * conv_type * conv_num * condition + 
                                      (1 | dyad) + (1 | participant),
                                    data = analysis_exploratory_df)
summary(model_exploratory_with_order)

##### 5. Observed power analyses for significant effects and trends #####

##### 5a. Log RR model #####

# argumentative conversations
model_log_rr_power_conv_type2 = powerSim(model_log_rr, 
                                         fixed("conv_type2", "t"), 
                                         nsim=1000,
                                         progress=FALSE)
model_log_rr_power_conv_type2

# cooperative conversations
model_log_rr_power_conv_type3 = powerSim(model_log_rr, 
                                         fixed("conv_type3", "t"), 
                                         nsim=1000,
                                         progress=FALSE)
model_log_rr_power_conv_type3

# for VC Laboratory
model_log_rr_power_condition3 = powerSim(model_log_rr, 
                                         fixed("condition3", "t"), 
                                         nsim=1000,
                                         progress=FALSE)
model_log_rr_power_condition3

##### 5b. DET model #####

# argumentative conversations
model_det_power_conv_type2 = powerSim(model_det, 
                                      fixed("conv_type2", "t"), 
                                      nsim=1000,
                                      progress=FALSE)
model_det_power_conv_type2

# cooperative conversations
model_det_power_conv_type3 = powerSim(model_det, 
                                      fixed("conv_type3", "t"), 
                                      nsim=1000,
                                      progress=FALSE)
model_det_power_conv_type3

# for VC Remote
model_det_power_condition2 = powerSim(model_det, 
                                      fixed("condition2", "t"), 
                                      nsim=1000,
                                      progress=FALSE)
model_det_power_condition2

# for VC Laboratory
model_det_power_condition3 = powerSim(model_det, 
                                      fixed("condition3", "t"), 
                                      nsim=1000,
                                      progress=FALSE)
model_det_power_condition3

##### 5c. maxL model #####

# argumentative conversations
model_maxL_power_conv_type2 = powerSim(model_maxL, 
                                       fixed("conv_type2", "t"), 
                                       nsim=1000,
                                       progress=FALSE)
model_maxL_power_conv_type2

##### 5d. DRP model #####

# leading/following
model_drp_power_ot1 = powerSim(model_drp, 
                               fixed("ot1", "t"), 
                               nsim=1000,
                               progress = TRUE)
model_drp_power_ot1

# synchrony
model_drp_power_ot2 = powerSim(model_drp, 
                               fixed("ot2", "t"), 
                               nsim=1000,
                               progress = TRUE)
model_drp_power_ot2

# cooperative conversations
model_drp_power_conv_type3 = powerSim(model_drp, 
                                      fixed("conv_type3", "t"), 
                                      nsim=1000,
                                      progress = TRUE)
model_drp_power_conv_type3

# synchrony by argument
model_drp_power_ot2_conv_type2 = powerSim(model_drp, 
                                          fixed("ot2:conv_type2", "t"), 
                                          nsim=1000,
                                          progress = TRUE)
model_drp_power_ot2_conv_type2

# synchrony by VC Remote
model_drp_power_ot2_condition2 = powerSim(model_drp, 
                                          fixed("ot2:condition2", "t"), 
                                          nsim=1000,
                                          progress = TRUE)
model_drp_power_ot2_condition2

# synchrony by VC Lab
model_drp_power_ot2_condition3 = powerSim(model_drp, 
                                          fixed("ot2:condition3", "t"), 
                                          nsim=1000,
                                          progress = TRUE)
model_drp_power_ot2_condition3

# lead/follow by cooperative by VC Remote
model_drp_power_ot1_conv_type_3_condition2 = powerSim(model_drp, 
                                                      fixed("ot1:conv_type_3:condition2", "t"), 
                                                      nsim=1000,
                                                      progress = TRUE)
model_drp_power_ot1_conv_type_3_condition2

# synchrony by cooperative by VC Remote
model_drp_power_ot2_conv_type_2_condition2 = powerSim(model_drp, 
                                                      fixed("ot2:conv_type_2:condition2", "t"), 
                                                      nsim=1000,
                                                      progress = TRUE)
model_drp_power_ot2_conv_type_2_condition2

# synchrony by argumentative by VC Lab
model_drp_power_ot2_conv_type_3_condition3 = powerSim(model_drp, 
                                                      fixed("ot2:conv_type_3:condition3", "t"), 
                                                      nsim=1000,
                                                      progress = TRUE)
model_drp_power_ot2_conv_type_3_condition3