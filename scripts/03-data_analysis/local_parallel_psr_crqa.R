#### parallel_crqa-vic.R  ####
#
# This script creates a function to run CRQA with the
# `parallel` function.
#
##############################

# create a function to be applied over a `split` df
local_parallel_psr_crqa <- function(input_file_list,
                                    crqa_output_directory) {
  
  # cycle through each subset
  foreach(i = seq_along(input_file_list), .errorhandling='pass') %dopar% {
    
    # grab the next file
    this_file = input_file_list[[i]]
    
    # specify sampling rate and filters
    original_sampling_rate = 30 # in Hz
    downsampled_sampling_rate = 10 # in Hz
    anti_aliasing_butter = butter(4,.4)
    
    # set RQA parameters
    target_winsize = downsampled_sampling_rate * 3
    target_delay = 3
    target_embedding = 14
    target_radius = .2
    # Set 1: delay 3, embed 14, radius .2
    # Set 2: delay 21, embed 9, radius .35
    # Tailored set: optimal delay, optimal embedding, radius .3
    
    # grab the dyad we're analyzing and then reform wider
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
    }
    
    # trim as needed
    this_duration = max(this_dyad_df$t)
    this_dyad_df = this_dyad_df %>% ungroup() %>%
      dplyr::filter(t >= trim_start) %>%
      dplyr::filter(t <= (this_duration - trim_end))
    
    # calculate delay if we're not going with pre-sets
    if (is.na(target_delay)){
      
      # identify AMI for both participants in each conversation
      ami_lag_max = 100
      delay_left = first_local_minimum(tseriesChaos::mutual(this_dyad_df$movement_left,
                                                            lag.max = ami_lag_max,
                                                            plot = FALSE))
      delay_right = first_local_minimum(tseriesChaos::mutual(this_dyad_df$movement_right,
                                                             lag.max = ami_lag_max,
                                                             plot = FALSE))
      delay_selected = max(delay_left, delay_right, na.rm = TRUE)
    } else {
      delay_selected = target_delay
    }
    
    # calculate false nearest neighbors for left participant
    if (is.na(target_embedding)){
      fnn_dim_max = 15
      fnn_left = false.nearest(this_dyad_df$movement_left,
                               m = fnn_dim_max,
                               d = delay_selected,
                               t = 0,
                               rt = 10,
                               eps = sd(this_dyad_df$movement_left) / 10)
      fnn_left = fnn_left[1,][complete.cases(fnn_left[1,])]
      
      # calculate false nearest neighbors for right participant
      fnn_right = false.nearest(this_dyad_df$movement_right,
                                m = fnn_dim_max,
                                d = delay_selected,
                                t = 0,
                                rt = 10,
                                eps = sd(this_dyad_df$movement_right) / 10)
      fnn_right = fnn_right[1,][complete.cases(fnn_right[1,])]
      
      # identify the largest dimension after a large drop for each participant
      # ("largest drop" specified as 10% of first dimension), while accounting
      # for ones that have only small drops
      threshold_left = as.numeric(fnn_left[1]/10)
      threshold_right = as.numeric(fnn_right[1]/10)
      if (is.infinite(max(as.numeric(which(diff(fnn_left) < -threshold_left))))){
        embed_left = min(which(fnn_left == 0),
                         max(first_local_minimum(diff(fnn_left))))
      } else {
        embed_left = min(which(fnn_left == 0),
                         max(as.numeric(which(diff(fnn_left) < -threshold_left)))) + 1
      }
      if (is.infinite(max(as.numeric(which(diff(fnn_right) < -threshold_right))))){
        embed_right = min(which(fnn_right == 0),
                          max(first_local_minimum(diff(fnn_right))))
      } else {
        embed_right = min(which(fnn_right == 0),
                          max(as.numeric(which(diff(fnn_right) < -threshold_right)))) + 1
      }
      embed_selected = max(embed_left, embed_right)
    } else {
      embed_selected = target_embedding
    }
    
    # run CRQA
    this_crqa = crqa::crqa(ts1 = this_dyad_df$movement_left,
                           ts2 = this_dyad_df$movement_right,
                           delay = delay_selected,
                           embed = embed_selected,
                           rescale = 1,
                           radius = target_radius,
                           normalize = 2,
                           mindiagline = 2,
                           minvertline = 2,
                           tw = 0,
                           whiteline = FALSE,
                           recpt = FALSE,
                           side = 'both',
                           method = "crqa",
                           data = "continuous")
    
    # create dataframe for plot-wide results
    this_crqa_df = data.frame(conv,
                              delay = delay_selected,
                              embed = embed_selected,
                              RR = this_crqa$RR,
                              DET = this_crqa$DET,
                              NRLINE = this_crqa$NRLINE,
                              maxL = this_crqa$maxL,
                              L = this_crqa$L,
                              ENTR = this_crqa$ENTR,
                              rENTR = this_crqa$rENTR,
                              LAM = this_crqa$LAM,
                              TT = this_crqa$TT)
    
    # save to file
    write.csv(this_crqa_df,
              file = paste0(crqa_output_directory,
                            'crqa-',conv,'.csv'),
              row.names = FALSE)
    
    # free up memory
    rm(this_crqa)
    
    # calculate DRP
    this_drp = crqa::drpfromts(ts1 = this_dyad_df$movement_left,
                               ts2 = this_dyad_df$movement_right,
                               delay = delay_selected,
                               embed = embed_selected,
                               windowsize = 30,
                               rescale = 1,
                               radius = target_radius,
                               normalize = 2,
                               mindiagline = 2,
                               minvertline = 2,
                               tw = 0,
                               whiteline = FALSE,
                               recpt = FALSE,
                               side = 'both',
                               method = "crqa",
                               data = "continuous")
    
    # create dataframe for DRP results
    this_drp_df = as.data.frame(this_drp) %>%
      rownames_to_column('lag') %>%
      select(-maxrec, -maxlag) %>%
      mutate(conv = conv,
             delay = delay_selected,
             embed = embed_selected)
    
    # save to file
    write.csv(this_drp_df,
              file = paste0(crqa_output_directory,
                            'drp-',conv,'.csv'),
              row.names = FALSE)
    
    # free up memory
    rm(this_drp)
  }}