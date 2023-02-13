####### Visual Information in Coordination: Data Analysis #######

# In this script, we'll do our statistical analyses.

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

# our DV distribution doesn't conform to ANOVA assumptions
hist(analysis_crqa_df$RR)

# create initial RR model
model_rr = lmer(RR ~ conv_type * condition + (1 | dyad), 
                data = analysis_crqa_df)

# showing that we're violating assumptions
plot(model_rr)
hist(resid(model_rr))

# trying for log-transforming the data
analysis_crqa_df = analysis_crqa_df %>%
  mutate(log_RR = log(RR))

# create log RR model
model_log_rr = lmer(log_RR ~ conv_type * condition + (1 | dyad), 
                    data = analysis_crqa_df)

# showing that we're good here
plot(model_log_rr)
hist(resid(model_log_rr))

# show our results and save them
summary(model_log_rr)
grob_table_subtable_log_rr = create_publication_tables(model_name = model_log_rr,
                                                       dv_label = c("Log RR"),
                                                       pred_label_list = experimental_pred_labels,
                                                       table_name = "table_subtable_log_rr",
                                                       output_dir = "./tables/")

##### 1b. DET #####

# our DV distribution doesn't conform to ANOVA assumptions
hist(analysis_crqa_df$DET, breaks = 20)

# create initial DET model
model_det = lmer(DET ~ conv_type * condition + (1 | dyad), 
                 data = analysis_crqa_df)

# showing that we're not violating assumptions
plot(model_det)
hist(resid(model_det), breaks = 100)

# show our results and save them
summary(model_det)
grob_table_subtable_det = create_publication_tables(model_name = model_det,
                                                    dv_label = c("DET"),
                                                    pred_label_list = experimental_pred_labels,
                                                    table_name = "table_subtable_det",
                                                    output_dir = "./tables/")

##### 1c. maxL #####

# our DV distribution doesn't conform to ANOVA assumptions
hist(analysis_crqa_df$maxL, breaks = 20)

# create initial maxL model
model_maxL = lmer(maxL ~ conv_type * condition + (1 | dyad), 
                  data = analysis_crqa_df)

# showing that we're not violating assumptions
plot(model_maxL)
hist(resid(model_maxL))

# show our results and save them
summary(model_maxL)
grob_table_subtable_maxL = create_publication_tables(model_name = model_maxL,
                                                     dv_label = c("maxLine"),
                                                     pred_label_list = experimental_pred_labels,
                                                     table_name = "table_subtable_maxL",
                                                     output_dir = "./tables/")

##### 1d. Build tables #####

# bind table grobs in cowplot
table_space = grid::nullGrob()
table_interaction_level_data = cowplot::plot_grid(grob_table_subtable_log_rr,
                                                  table_space,
                                                  grob_table_subtable_det,
                                                  table_space,
                                                  grob_table_subtable_maxL,
                                                  rel_widths = c(1,.01,1.05,.01, 1.07),
                                                  nrow=1)

# create and add title
table_interaction_level_title = ggdraw() + 
  draw_label("Models predicting CRQA metrics", 
             fontface = 'bold', x = 0, hjust = 0) 
table_interaction_level = cowplot::plot_grid(table_interaction_level_title,
                                             table_interaction_level_data,
                                             rel_heights = c(0.1, 1.5),
                                             ncol=1)
table_interaction_level

# save to file
ggsave(filename = paste0('./tables/table_interaction_level-vic.png'),
       plot = table_interaction_level,
       height = 3,
       width = 10,
       units = "in")

##### 2. Moment-to-moment metrics (H1-H3; 3.4.2) #####

# build a model
model_drp = lmer(RR ~ ot1 * ot2 * conv_type * condition + 
                   (1 + conv_type | dyad), 
                 data = analysis_drp_df)

# check that our results don't violate assumptions
plot(model_drp)
hist(resid(model_drp))

# show results and save them
summary(model_drp)
grob_table_drp = create_publication_tables(model_name = model_drp,
                                           dv_label = c("DRP RR"),
                                           pred_label_list = drp_pred_labels,
                                           table_name = "table_drp",
                                           output_dir = "./tables/")

##### 3. Comparing outcomes (H4-H5; 3.4.3) #####

##### 3a. Feelings of closeness #####

# predicting feelings of being close
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

# run analysis
model_closeness = lmer(rating ~ conv_type * condition + 
                         (1 + conv_type | dyad) + (1 | Participant), 
                       data = analysis_close_df)

# show results and save them
summary(model_closeness)
grob_table_closeness = create_publication_tables(model_name = model_closeness,
                                                 dv_label = c("Closeness"),
                                                 pred_label_list = experimental_pred_labels,
                                                 table_name = "table_closeness",
                                                 output_dir = "./tables/")

##### 3b. Feelings of understanding partner #####

# predicting feelings of understanding partner
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

# run analysis
model_understood_partner = lmer(rating ~ conv_type * condition + 
                                  (1 | dyad) + (1 | Participant), 
                                data = analysis_understood_partner_df)

# show results and save them
summary(model_understood_partner)
grob_table_understood = create_publication_tables(model_name = model_understood_partner,
                                                  dv_label = c("Understood Partner"),
                                                  pred_label_list = experimental_pred_labels,
                                                  table_name = "table_understood",
                                                  output_dir = "./tables/")

##### 3c. Feelings of being understood #####

# predicting feelings of being understood
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

# run analysis
model_understood_me = lmer(rating ~ conv_type * condition + 
                             (1 | dyad) + (1 | Participant), 
                           data = analysis_understood_me_df)

# show results and save them
summary(model_understood_me)
grob_table_understood_me = create_publication_tables(model_name = model_understood_me,
                                                     dv_label = c("Felt Understood"),
                                                     pred_label_list = experimental_pred_labels,
                                                     table_name = "table_understood_me",
                                                     output_dir = "./tables/")

##### 3d. Build compiled table #####

# bind table grobs in cowplot
table_space = grid::nullGrob()
table_outcome_data = cowplot::plot_grid(grob_table_closeness,
                                        table_space,
                                        grob_table_understood,
                                        table_space,
                                        grob_table_understood_me,
                                        rel_widths = c(1,.01,1,.01, 1),
                                        nrow=1)

# create and add title
table_outcome_title = ggdraw() + 
  draw_label("Models predicting outcomes", 
             fontface = 'bold', x = 0, hjust = 0) 
table_outcome = cowplot::plot_grid(table_outcome_title,
                                   table_outcome_data,
                                   rel_heights = c(0.1, 1.5),
                                   ncol=1)
table_outcome

# save to file
ggsave(filename = paste0('./tables/table_outcomes_combined-vic.png'),
       plot = table_outcome,
       height = 3,
       width = 10,
       units = "in")

##### 4. Exploratory: Dynamics impacting outcomes (E1; 3.5) #####

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

##### 4a. Predicting closeness (E1; 3.5) #####

# interaction-level analyses: building and comparing models
model_exploratory_close_m0 = lmer(closeness ~ conv_type * condition + 
                                    (1 | dyad) + (1 | participant),
                                  data = analysis_exploratory_df)
model_exploratory_close_m1 = lmer(closeness ~ (log_RR) * conv_type * condition + 
                                    (1 | dyad) + (1 | participant),
                                  data = analysis_exploratory_df)
model_exploratory_close_m2 = lmer(closeness ~ (DET) * conv_type * condition + 
                                    (1 | dyad) + (1 | participant),
                                  data = analysis_exploratory_df)
model_exploratory_close_m3 = lmer(closeness ~ (maxL) * conv_type * condition + 
                                    (1 | dyad) + (1 | participant),
                                  data = analysis_exploratory_df)
model_exploratory_close_m4 = lmer(closeness ~ (max_drp_rr) * conv_type * condition + 
                                    (1 | dyad) + (1 | participant),
                                  data = analysis_exploratory_df)
model_exploratory_close_m5 = lmer(closeness ~ (DET + maxL) * conv_type * condition + 
                                    (1 | dyad) + (1 | participant),
                                  data = analysis_exploratory_df)
model_exploratory_close_m6 = lmer(closeness ~ (DET + maxL + max_drp_rr) * conv_type * condition + 
                                    (1 | dyad) + (1 | participant),
                                  data = analysis_exploratory_df)
model_exploratory_close_m7 = lmer(closeness ~ (log_RR + DET + maxL) * conv_type * condition + 
                                    (1 | dyad) + (1 | participant),
                                  data = analysis_exploratory_df)
model_exploratory_close_m8 = lmer(closeness ~ (log_RR + DET + maxL + max_drp_rr) * conv_type * condition + 
                                    (1 | dyad) + (1 | participant),
                                  data = analysis_exploratory_df)

# let's see if any of the models perform better than our condition-only model
anova(model_exploratory_close_m0, model_exploratory_close_m1)
anova(model_exploratory_close_m0, model_exploratory_close_m1)
anova(model_exploratory_close_m0, model_exploratory_close_m2)
anova(model_exploratory_close_m0, model_exploratory_close_m3)
anova(model_exploratory_close_m0, model_exploratory_close_m4)
anova(model_exploratory_close_m0, model_exploratory_close_m5)
anova(model_exploratory_close_m0, model_exploratory_close_m6)
anova(model_exploratory_close_m0, model_exploratory_close_m7) # significantly worse
anova(model_exploratory_close_m0, model_exploratory_close_m8) # significantly worse

# nothing was significantly better than baseline

##### 4b. Predicting understood partner (E1; 3.5) #####

# interaction-level analyses: building and comparing models
model_exploratory_understood_partner_m0 = lmer(understood_partner ~ conv_type * condition + 
                                                 (1 | dyad) + (1 | participant),
                                               data = analysis_exploratory_df)
model_exploratory_understood_partner_m1 = lmer(understood_partner ~ (log_RR) * conv_type * condition + 
                                                 (1 | dyad) + (1 | participant),
                                               data = analysis_exploratory_df)
model_exploratory_understood_partner_m2 = lmer(understood_partner ~ (DET) * conv_type * condition + 
                                                 (1 | dyad) + (1 | participant),
                                               data = analysis_exploratory_df)
model_exploratory_understood_partner_m3 = lmer(understood_partner ~ (maxL) * conv_type * condition + 
                                                 (1 | dyad) + (1 | participant),
                                               data = analysis_exploratory_df)
model_exploratory_understood_partner_m4 = lmer(understood_partner ~ (max_drp_rr) * conv_type * condition + 
                                                 (1 | dyad) + (1 | participant),
                                               data = analysis_exploratory_df)
model_exploratory_understood_partner_m5 = lmer(understood_partner ~ (DET + maxL) * conv_type * condition + 
                                                 (1 | dyad) + (1 | participant),
                                               data = analysis_exploratory_df)
model_exploratory_understood_partner_m6 = lmer(understood_partner ~ (DET + maxL + max_drp_rr) * conv_type * condition + 
                                                 (1 | dyad) + (1 | participant),
                                               data = analysis_exploratory_df)
model_exploratory_understood_partner_m7 = lmer(understood_partner ~ (log_RR + DET + maxL) * conv_type * condition + 
                                                 (1 | dyad) + (1 | participant),
                                               data = analysis_exploratory_df)
model_exploratory_understood_partner_m8 = lmer(understood_partner ~ (log_RR + DET + maxL + max_drp_rr) * conv_type * condition + 
                                                 (1 | dyad) + (1 | participant),
                                               data = analysis_exploratory_df)

# let's see if any of the models perform better than our condition-only model
anova(model_exploratory_understood_partner_m0, model_exploratory_understood_partner_m1)
anova(model_exploratory_understood_partner_m0, model_exploratory_understood_partner_m1)
anova(model_exploratory_understood_partner_m0, model_exploratory_understood_partner_m2)
anova(model_exploratory_understood_partner_m0, model_exploratory_understood_partner_m3)
anova(model_exploratory_understood_partner_m0, model_exploratory_understood_partner_m4)
anova(model_exploratory_understood_partner_m0, model_exploratory_understood_partner_m5)
anova(model_exploratory_understood_partner_m0, model_exploratory_understood_partner_m6)
anova(model_exploratory_understood_partner_m0, model_exploratory_understood_partner_m7)
anova(model_exploratory_understood_partner_m0, model_exploratory_understood_partner_m8)

# non-significant trend toward model 7 (log_RR + DET + maxL) being worse

##### 4c. Predicting understood me (E1; 3.5) #####

# interaction-level analyses: building and comparing models
model_exploratory_understood_me_m0 = lmer(understood_me ~ conv_type * condition + 
                                            (1 | dyad) + (1 | participant),
                                          data = analysis_exploratory_df)
model_exploratory_understood_me_m1 = lmer(understood_me ~ (log_RR) * conv_type * condition + 
                                            (1 | dyad) + (1 | participant),
                                          data = analysis_exploratory_df)
model_exploratory_understood_me_m2 = lmer(understood_me ~ (DET) * conv_type * condition + 
                                            (1 | dyad) + (1 | participant),
                                          data = analysis_exploratory_df)
model_exploratory_understood_me_m3 = lmer(understood_me ~ (maxL) * conv_type * condition + 
                                            (1 | dyad) + (1 | participant),
                                          data = analysis_exploratory_df)
model_exploratory_understood_me_m4 = lmer(understood_me ~ (max_drp_rr) * conv_type * condition + 
                                            (1 | dyad) + (1 | participant),
                                          data = analysis_exploratory_df)
model_exploratory_understood_me_m5 = lmer(understood_me ~ (DET + maxL) * conv_type * condition + 
                                            (1 | dyad) + (1 | participant),
                                          data = analysis_exploratory_df)
model_exploratory_understood_me_m6 = lmer(understood_me ~ (DET + maxL + max_drp_rr) * conv_type * condition + 
                                            (1 | dyad) + (1 | participant),
                                          data = analysis_exploratory_df)
model_exploratory_understood_me_m7 = lmer(understood_me ~ (log_RR + DET + maxL) * conv_type * condition + 
                                            (1 | dyad) + (1 | participant),
                                          data = analysis_exploratory_df)
model_exploratory_understood_me_m8 = lmer(understood_me ~ (log_RR + DET + maxL + max_drp_rr) * conv_type * condition + 
                                            (1 | dyad) + (1 | participant),
                                          data = analysis_exploratory_df)

# let's see if any of the models perform better than our condition-only model
anova(model_exploratory_understood_me_m0, model_exploratory_understood_me_m1)
anova(model_exploratory_understood_me_m0, model_exploratory_understood_me_m1)
anova(model_exploratory_understood_me_m0, model_exploratory_understood_me_m2)
anova(model_exploratory_understood_me_m0, model_exploratory_understood_me_m3)
anova(model_exploratory_understood_me_m0, model_exploratory_understood_me_m4)
anova(model_exploratory_understood_me_m0, model_exploratory_understood_me_m5)
anova(model_exploratory_understood_me_m0, model_exploratory_understood_me_m6)
anova(model_exploratory_understood_me_m0, model_exploratory_understood_me_m7)
anova(model_exploratory_understood_me_m0, model_exploratory_understood_me_m8)

# nothing fits better than our baseline

##### 5. Exploratory: Interaction with Conversation Order #####

# add order along to RR and DET
model_exploratory_with_order = lmer(closeness ~ (log_RR + DET) * conv_type * conv_num * condition + 
                                      (1 | dyad) + (1 | participant),
                                    data = analysis_exploratory_df)
summary(model_exploratory_with_order)

# significantly better fit!
anova(model_exploratory_close_m0, model_exploratory_with_order)

# save to table
grob_table_exploratory_with_order = create_publication_tables(model_name = model_exploratory_with_order,
                                                              dv_label = c("Closeness"),
                                                              pred_label_list = exploratory_order_pred_table_labels,
                                                              table_name = "table_best_exploratory_with_order",
                                                              output_dir = "./tables/")


# plot DET
plot_e1_det_closeness_ylim = ggplot(analysis_exploratory_df,
                                    aes(y = DET,
                                        x = closeness,
                                        color = as.factor(conv_num))) +
  geom_violin() +
  facet_grid(cols = vars(conv_type),
             rows = vars(condition)) +
  scale_color_viridis(discrete = TRUE,
                      labels = c("First",
                                 "Second",
                                 "Third"),
                      name = "Conversation Order")+
  theme(legend.position = "bottom") +
  geom_jitter(width = .1,
              height = .1,
              alpha = .5) +
  coord_cartesian(xlim = c(0.8,6.2),
                  ylim=c(-2,1))+
  xlab("Rating of Closeness") + 
  ylab("DET (Determinism), Scaled and Centered")+ 
  ggtitle("Closeness by determinism, condition, and conversation type")
plot_e1_det_closeness_ylim
ggsave(filename = paste0('./figures/e1_closeness_det_ylim-vic.png'),
       plot = plot_e1_det_closeness_ylim,
       height = 6,
       width = 6,
       units = "in")

# plot RR
plot_e1_rr_closeness_ylim = ggplot(analysis_exploratory_df,
                                   aes(y = log_RR,
                                       x = closeness,
                                       color = as.factor(conv_num))) +
  geom_violin() +
  facet_grid(cols = vars(conv_type),
             rows = vars(condition)) +
  scale_color_viridis(discrete = TRUE,
                      labels = c("First",
                                 "Second",
                                 "Third"),
                      name = "Conversation Order")+
  theme(legend.position = "bottom") +
  geom_jitter(width = .1,
              height = .1,
              alpha = .5) +
  xlab("Rating of Closeness") + 
  ylab("Log RR (Recurrence Rate), Scaled and Centered")+ 
  ggtitle("Closeness by log RR, condition, and conversation type")
plot_e1_rr_closeness_ylim
ggsave(filename = paste0('./figures/e1_closeness_rr-vic.png'),
       plot = plot_e1_rr_closeness_ylim,
       height = 6,
       width = 6,
       units = "in")