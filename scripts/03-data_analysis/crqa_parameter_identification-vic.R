####### Visual Information in Coordination: Phase-Space Reconstruction #######

# In this script, we'll do some phase-space reconstruction to prepare for CRQA.
# Part of data analysis for Romero & Paxton (2021).

#############################################################################

# grab the array id value from the environment variable passed from sbatch
slurm_arrayid = Sys.getenv('SLURM_ARRAY_TASK_ID')

# coerce the value to an integer
n = as.numeric(slurm_arrayid)

# load libraries
library(crqa)
library(tidyverse)
library(tseriesChaos)

# function to identify first local minimum (modified from https://stackoverflow.com/a/6836583)
first_local_minimum <- function(x){
  flm = as.numeric((which(diff(sign(diff(x)))==-2)+1)[1])
  if (is.na(flm)) { flm = as.numeric(which(diff(x)==max(diff(x))))-1 }
  return(flm)
}

# get our list of files
movement_data_files = list.files(path = './data/movement_dataframes-aggregated',
                                 pattern = "*.csv",
                                 full.names = TRUE)

# grab the dyad we're analyzing and then reform wider
this_dyad_df = read.csv(movement_data_files[1],
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
          file = paste0('./data/parameters/parameters-',
                        conv,
                        '.csv'),
          row.names = FALSE)
