####### Visual Information in Coordination: CRQA for Phase-Randomized Baselines #######

# In this script, we'll generate data for our phase-randomized baselines.

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

# create directories for our output, if we don't have them yet
baseline_output_directory = "./data/baselines/"
dir.create(baseline_output_directory,
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
source('./scripts/05-baselines/local_baseline_parallel_psr_crqa.R')
local_baseline_parallel_psr_crqa(movement_data_file_list,
                                 baseline_output_directory)

# stop the pseudocluster
stopCluster(pseudo_cluster)