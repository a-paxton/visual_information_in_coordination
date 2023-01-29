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

##### 1. Gross movement #####

##### 1a. Prepare gross movement unified dataframe #####

# if we already have the output file, just read it in (instead of recalculating)
unified_movement_file = './data/analyses/downsampled_movement_df.csv'
if (file.exists(unified_movement_file)){
  movement_df = read.csv(unified_movement_file)
} else {
  
  # create directories for our output, if we don't have them yet
  dir.create('./data/analyses',
             showWarnings = TRUE,
             recursive = TRUE)
  
  # plot and save
  movement_files = list.files(path = "./data/movement_dataframes-aggregated/",
                              full.names = TRUE)
  movement_df = data.frame()
  for (this_file in movement_files){
    
    # specify sampling rate and filters
    original_sampling_rate = 30 # in Hz
    downsampled_sampling_rate = 10 # in Hz
    anti_aliasing_butter = butter(4,.1)
    
    # read in the next set of movement
    this_dyad_df = read.csv(this_file,
                            header=TRUE) %>%
      tidyr::pivot_wider(names_from = participant,
                         values_from = movement,
                         names_prefix = "movement_") %>%
      mutate(movement_left = as.numeric(movement_left),
             movement_right = as.numeric(movement_right)) %>%
      
      # convert difference value to time
      mutate(t = as.numeric(difference_number)/original_sampling_rate) %>%
      
      # apply anti-aliasing filter
      mutate(movement_left = signal::filtfilt(anti_aliasing_butter, movement_left),
             movement_right = signal::filtfilt(anti_aliasing_butter, movement_right)) %>%
      
      # create new time variable to downsample
      mutate(t = floor(t * downsampled_sampling_rate) / downsampled_sampling_rate) %>%
      
      # just take the first slice (first observation in that window)
      group_by(t) %>%
      slice(1) %>%
      ungroup()
    
    # get conversation identifier
    conv = unique(this_dyad_df$dyad)
    
    # if they're part of the Zoom conditions, drop the first 1 second
    if (grepl("Z",conv)){
      this_dyad_df = this_dyad_df %>% ungroup() %>%
        dplyr::filter(t > 1)
    }
    
    # check to see if we need to trim... 
    # (not a very elegant solution, but here we are, deadlines being as they are)
    trim_start = 0
    trim_end = 0
    if (grepl("1036_ZR_coop", conv)){
      trim_start = 15
    } else if (grepl("1002_FF_1_Coop", conv)){
      trim_start = 54
    } else if (grepl("1031_ZT_1_Aff", conv)){
      trim_end = 54
    } else if (grepl("1054_FF_6_Aff", conv)){
      trim_end = 9
    } else if (grepl("1054_FF_6_Aff", conv)){
      trim_end = 9
    } else if (grepl("1035_ZT_5_Coop", conv)){
      trim_end = 16
    } else if(grepl("1038_ZT_2_Arg_Video", conv)){
      trim_start = 2*60 + 34 # problems with recording setting until 2:34
    }
    
    # trim as needed
    this_duration = max(this_dyad_df$t)
    this_dyad_df = this_dyad_df %>% ungroup() %>%
      dplyr::filter(t >= trim_start) %>%
      dplyr::filter(t <= (this_duration - trim_end))
    
    # concatenate
    movement_df = rbind.data.frame(movement_df,
                                   this_dyad_df)
  }
  
  # save to file
  write.csv(file = unified_movement_file,
            x = movement_df,
            row.names = FALSE)
}

##### 1b. Plot gross movement #####

# create directories for our figures, if we don't have them yet
dir.create('./figures',
           showWarnings = TRUE,
           recursive = TRUE)

# plot everything
movement_plot = ggplot(movement_df) +
  geom_line(aes(x = t,
                y = movement_left,
                group = dyad),
            color="blue",
            alpha=.1) +
  geom_line(aes(x = t,
                y = movement_right,
                group = dyad),
            color="red",
            alpha=.1) +
  geom_smooth(aes(x = t,
                  y = movement_left),
              color = "black") +
  geom_smooth(aes(x = t,
                  y = movement_right),
              color = "grey") +
  labs(y = "Movement",
       x = "Time (seconds)") +
  ggtitle("Movement for all conversations")
ggsave(filename = paste0('./figures/raw/all_movement.png'),
       plot = movement_plot,
       height = 6,
       width = 6,
       units = "in")

# plot everything --- with a y-axis cutoff for better visualization
movement_cutoff_plot = ggplot(movement_df) +
  geom_line(aes(x = t,
                y = movement_left,
                group = dyad),
            color="blue",
            alpha=.1) +
  geom_line(aes(x = t,
                y = movement_right,
                group = dyad),
            color="red",
            alpha=.1) +
  geom_smooth(aes(x = t,
                  y = movement_left),
              color = "black") +
  geom_smooth(aes(x = t,
                  y = movement_right),
              color = "grey") +
  coord_cartesian(ylim = c(0, 10)) +
  labs(y = "Movement",
       x = "Time (seconds)") +
  ggtitle("Movement for all conversations\n(y-axis restricted)")
ggsave(filename = paste0('./figures/raw/all_movement-ylim.png'),
       plot = movement_cutoff_plot,
       height = 6,
       width = 6,
       units = "in")
rm(movement_df)

##### 2. CRQA Metrics #####

##### 2a. Prepare CRQA unified dataframes #####

# bind crqa dataframe
crqa_tailored_files = list.files(path = './data/crqa/',
                                 pattern = "crqa*",
                                 full.names = TRUE)
crqa_tailored_df <- do.call(rbind,
                            lapply(crqa_tailored_files,read.csv))

# identify which folks we'll keep (to remove the untailored videos)
keep_videos = unique(crqa_tailored_df$conv)

# clean up the CRQA dataframe
crqa_tailored_df = crqa_tailored_df %>% ungroup() %>%
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
write.csv(x = crqa_tailored_df,
          file = './data/analyses/crqa_df_tailored-vic.csv',
          row.names = FALSE)

# for unified parameter set 01: bind crqa dataframe and clean it up a bit
crqa_opt_01_files = list.files(path = './data/crqa-opt_01/',
                               pattern = "crqa*",
                               full.names = TRUE)
crqa_01_df <- do.call(rbind,
                      lapply(crqa_opt_01_files,read.csv)) %>%
  dplyr::filter(conv %in% keep_videos) %>%
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
  mutate(params = "set_01")

# save
write.csv(x = crqa_01_df,
          file = './data/analyses/crqa_df_global_01-vic.csv',
          row.names = FALSE)

# for unified parameter set 02: bind crqa dataframe and clean it up a bit
crqa_opt_02_files = list.files(path = './data/crqa-opt_02/',
                               pattern = "crqa*",
                               full.names = TRUE)
crqa_02_df <- do.call(rbind,
                      lapply(crqa_opt_02_files,read.csv)) %>%
  dplyr::filter(conv %in% keep_videos) %>%
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
  mutate(params = "set_02")

# save
write.csv(x = crqa_02_df,
          file = './data/analyses/crqa_df_global_02-vic.csv',
          row.names = FALSE)

# show the ranges of RR
hist(crqa_tailored_df$RR)
hist(crqa_01_df$RR)
hist(crqa_02_df$RR)

# we'll choose set 01 of our global parameters as our main
# (since it gives us a cleaner distribution of RR)
analysis_crqa_df = crqa_01_df
write.csv(x = analysis_crqa_df,
          file = './data/analyses/analysis_crqa_df-vic.csv',
          row.names = FALSE)
rm(crqa_tailored_df, crqa_01_df, crqa_02_df)

##### 2b. Plot CRQA metrics #####

# plot overall metrics
overall_RR_plot = ggplot(analysis_crqa_df,
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
  ggtitle("Inverse of noise of movement\nby condition and conversation type")
ggsave(filename = paste0('./figures/rr_results-vic.png'),
       plot = overall_RR_plot,
       height = 6,
       width = 4,
       units = "in")

overall_DET_plot = ggplot(analysis_crqa_df,
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
  ggtitle("Structure of movement\nby condition and conversation type")
ggsave(filename = paste0('./figures/det_results-vic.png'),
       plot = overall_DET_plot,
       height = 6,
       width = 4,
       units = "in")

overall_maxL_plot = 
  ggplot(analysis_crqa_df,
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
  ggtitle("Attractor strength of movement\nby condition and conversation type")
ggsave(filename = paste0('./figures/maxl_results-vic.png'),
       plot = overall_maxL_plot,
       height = 6,
       width = 4,
       units = "in")

##### 3. DRPs #####

##### 3a. Prepare DRP unified dataframes #####

# bind our tailored drp dataframe and clean it up a bit
drp_tailored_files = list.files(path = './data/crqa/',
                                pattern = "drp*",
                                full.names = TRUE)
drp_tailored_df <- do.call(rbind,
                           lapply(drp_tailored_files,read.csv)) %>%
  dplyr::filter(conv %in% keep_videos) %>%
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
  
  # filter out what we don't need here
  dplyr::filter(abs(lag)<=30) %>%
  mutate(conv_type = stringr::str_to_lower(conv_type))

# save to file
write.csv(x = drp_tailored_df,
          file = './data/analyses/drp_tailored_df-vic.csv',
          row.names = FALSE)

# bind our opt_01 drp dataframe and clean it up a bit
drp_01_files = list.files(path = './data/crqa-opt_01/',
                          pattern = "drp*",
                          full.names = TRUE)
drp_01_df <- do.call(rbind,
                     lapply(drp_01_files,read.csv)) %>%
  dplyr::filter(conv %in% keep_videos) %>%
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
  
  # filter out what we don't need here
  dplyr::filter(abs(lag)<=30) %>%
  mutate(conv_type = stringr::str_to_lower(conv_type))

# save to file
write.csv(x = drp_01_df,
          file = './data/analyses/drp_01_df-vic.csv',
          row.names = FALSE)

# bind our opt_02 drp dataframe and clean it up a bit
drp_02_files = list.files(path = './data/crqa-opt_02/',
                          pattern = "drp*",
                          full.names = TRUE)
drp_02_df <- do.call(rbind,
                     lapply(drp_02_files,read.csv)) %>%
  dplyr::filter(conv %in% keep_videos) %>%
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
  
  # filter out what we don't need here
  dplyr::filter(abs(lag)<=30) %>%
  mutate(conv_type = stringr::str_to_lower(conv_type))

# save to file
write.csv(x = drp_02_df,
          file = './data/analyses/drp_02_df-vic.csv',
          row.names = FALSE)

# in keeping with our CRQA, we'll choose set 01 for DRPs
analysis_drp_df = drp_01_df
write.csv(x = analysis_drp_df,
          file = './data/analyses/analysis_drp_df-vic.csv',
          row.names = FALSE)
rm(drp_tailored_df, drp_01_df, drp_02_df)

##### 3b. Plot DRPs #####

plot_drp_df = analysis_drp_df %>% ungroup() %>%
  group_by(lag, conv_type, condition) %>%
  summarize(RR = mean(profile),
            SE = FSA::se(profile)) %>%
  ungroup()

all_drp_plots = ggplot(analysis_drp_df,
                       aes(x = lag,
                           y = profile,
                           color = conv_type)) +
  geom_line(data = plot_drp_df,
            aes(x = lag,
                y = RR,
                color = conv_type),
            alpha = .3) +
  geom_ribbon(data = plot_drp_df,
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
  ggtitle("Diagonal recurrence profile of movement\nby condition and conversation type")
ggsave(filename = paste0('./figures/drp_results-vic.png'),
       plot = all_drp_plots,
       height = 6,
       width = 4,
       units = "in")
