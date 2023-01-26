#### parallel_psr-vic.R  ####
#
# This script creates a function to run phase space reconstruction with the
# `parallel` function.
#
##############################

# create a function to be applied over a `split` df
parallel_psr <- function(input_file_list,
                         processed_output_directory) {
  
  # cycle through each subset
  foreach(i = seq_along(input_file_list), .errorhandling='pass') %dopar% {
    
    # grab the next file
    this_file <- input_file_list[[i]]
    
    # grab the dyad we're analyzing and then reform wider
    this_dyad_df = read.csv(this_file,
                            header=TRUE) %>%
      tidyr::pivot_wider(names_from = participant,
                         values_from = movement,
                         names_prefix = "movement_")
    
    # identify AMI for both participants in each conversation
    ami_lag_max = 200
    ami_left = first_local_minimum(tseriesChaos::mutual(this_dyad_df$movement_left,
                                                        lag.max = ami_lag_max, 
                                                        plot = FALSE))
    ami_right = first_local_minimum(tseriesChaos::mutual(this_dyad_df$movement_right, 
                                                         lag.max = ami_lag_max, 
                                                         plot = FALSE))
    ami_selected = min(ami_left, ami_right, na.rm = TRUE)
    
    # calculate false nearest neighbors for left participant
    fnn_dim_max = 15
    fnn_left = false.nearest(this_dyad_df$movement_left,
                             m = fnn_dim_max,
                             d = ami_selected,
                             t = 0,
                             rt = 10,
                             eps = sd(this_dyad_df$movement_left) / 10)
    fnn_left = fnn_left[1,][complete.cases(fnn_left[1,])]
    
    # calculate false nearest neighbors for right participant
    fnn_right = false.nearest(this_dyad_df$movement_right,
                              m = fnn_dim_max,
                              d = ami_selected,
                              t = 0,
                              rt = 10,
                              eps = sd(this_dyad_df$movement_right) / 10)
    fnn_right = fnn_right[1,][complete.cases(fnn_right[1,])]
    
    # identify the largest dimension after a large drop for each participant
    # ("largest drop" specified as 10% of first dimension)
    threshold_left = as.numeric(fnn_left[1]/10)
    threshold_right = as.numeric(fnn_right[1]/10)
    embed_left = min(which(fnn_left == 0), max(as.numeric(which(diff(fnn_left) < -threshold_left)))) + 1
    embed_right = min(which(fnn_right == 0), max(as.numeric(which(diff(fnn_right) < -threshold_right)))) + 1
    embed_selected = max(embed_left, embed_right)
    
    # bind everything to data frame
    conv = unique(this_dyad_df$dyad)
    this_parameter_df = data.frame(conv,
                                   ami_left,
                                   ami_right,
                                   ami_selected,
                                   embed_left,
                                   embed_right,
                                   embed_selected)
    write.csv(this_parameter_df,
              file = paste0(processed_output_directory,
                            '/',
                            conv,
                            '.csv'),
              row.names = FALSE)
    
  }}

