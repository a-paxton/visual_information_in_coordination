#### local_baseline_parallel_psr_crqa.R  ####
#
# This script creates a function to run CRQA with the
# `parallel` function.
#
##############################

# create a function to be applied over a `split` df
local_baseline_parallel_psr_crqa <- function(input_file_list,
                                             crqa_output_directory) {
  
  # cycle through each subset
  foreach(i = seq_along(input_file_list), .errorhandling='pass') %dopar% {
    
    # grab the next file
    this_file = input_file_list[[i]]
    
    # specify sampling rate and filters
    original_sampling_rate = 30 # in Hz
    downsampled_sampling_rate = 10 # in Hz
    anti_aliasing_butter = butter(4,.1)
    
    # set RQA parameters for baseline (i.e., using the main ones we are reporting)
    target_winsize = downsampled_sampling_rate * 3
    target_delay = 3
    target_embedding = 14
    target_radius = .2
    
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
    } else if(grepl("1038_ZT_2_Arg_Video", conv)){
      trim_start = 2*60 + 34 # problems with recording setting until 2:34
    }
    
    # trim as needed
    this_duration = max(this_dyad_df$t)
    this_dyad_df = this_dyad_df %>% ungroup() %>%
      dplyr::filter(t >= trim_start) %>%
      dplyr::filter(t <= (this_duration - trim_end))
    
    # specify parameters
    delay_selected = target_delay
    embed_selected = target_embedding
    
    # run through iterations of the phase randomization
    total_random_runs = c(1:10)
    for (random_run in total_random_runs){
      
      # turn to string for printing
      this_run_string = as.character(total_random_runs[[random_run]])
      
      # create phase-randomized baseline for each participant
      shuffle_left = data.frame(baseline = t(nonlinearTseries::FFTsurrogate(this_dyad_df$movement_left, 
                                                                            n.samples = 1)))
      shuffle_right = data.frame(baseline = t(nonlinearTseries::FFTsurrogate(this_dyad_df$movement_right, 
                                                                             n.samples = 1)))
      
      # run CRQA
      this_crqa = crqa::crqa(ts1 = shuffle_left$baseline,
                             ts2 = shuffle_right$baseline,
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
                                run = random_run,
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
                              'crqa-',conv,
                              '-baseline_',this_run_string,'.csv'),
                row.names = FALSE)
      
      # free up space
      rm(this_crqa)
    }
  }
}