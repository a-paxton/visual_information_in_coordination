####### Visual Information in Coordination: Cross-Recurrence Quantification Analysis #######

# In this script, we'll do some phase-space reconstruction and then run CRQA.
# Part of data analysis for Romero & Paxton (2021). To make everything more
# efficient, we'll go ahead and use local parallelization (rather than doing
# them all in sequential order).

#############################################################################

# preliminaries
rm(list=ls())
setwd('~/GitHub/visual_information_in_coordination/')

# try to avoid memory allocation issues
Sys.setenv('R_MAX_VSIZE'=32000000000)

# load libraries
library(crqa)
library(tidyverse)
library(tseriesChaos)
library(doParallel)

# set seed
set.seed(42)

# function to identify first local minimum (modified from https://stackoverflow.com/a/6836583)
first_local_minimum <- function(x){
  flm = as.numeric((which(diff(sign(diff(x)))==-2)+1)[1])
  if (is.na(flm)) { flm = as.numeric(which(diff(x)==max(diff(x))))-1 }
  return(flm)
}

# get our list of files
movement_data_file_list = list.files(path = './data/movement_dataframes-aggregated',
                                     pattern = "*.csv",
                                     full.names = TRUE)

# list the total output directories we want
output_directory_list = c("./data/crqa/",
                          "./data/crqa-opt_01/",
                          "./data/crqa-opt_02/")
for (next_output_directory in output_directory_list){
  
  # tell us what we're doing
  print(paste0("Processing: ",next_output_directory))
  
  # create directories for our output, if we don't have them yet
  dir.create(next_output_directory,
             showWarnings = TRUE,
             recursive = TRUE)
  
  # identify number of cores available
  available_cores = detectCores() - 1
  
  # initialize a pseudo-cluster with available cores
  pseudo_cluster = parallel::makeCluster(available_cores,
                                         type="FORK",
                                         setup_strategy="sequential", 
                                         outfile = './crqa_log.txt', 
                                         verbose = TRUE)
  
  # set seed for everyone
  parallel::clusterSetRNGStream(pseudo_cluster, iseed = 42)
  
  # parallelize our  analyses
  doParallel::registerDoParallel(pseudo_cluster)
  source('./scripts/03-data_analysis/local_parallel_psr_crqa.R')
  local_parallel_psr_crqa(movement_data_file_list,
                          next_output_directory)
  
  # stop the pseudocluster
  stopCluster(pseudo_cluster)
}