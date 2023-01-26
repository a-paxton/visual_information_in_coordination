####### Visual Information in Coordination: Phase-Space Reconstruction #######

# In this script, we'll do some phase-space reconstruction to prepare for CRQA.
# Part of data analysis for Romero & Paxton (2021). To make everything more
# efficient, we'll go ahead and use local parallelization (rather than doing
# them all in sequential order).

#############################################################################

# preliminaries
rm(list=ls())

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
movement_data_files = list.files(path = './data/movement_dataframes-aggregated',
                                 pattern = "*.csv",
                                 full.names = TRUE)

# create directories for our output, if we don't have them yet
processed_output_directory = "./data/parameters"
dir.create(processed_output_directory,
           showWarnings = TRUE,
           recursive = TRUE)

# identify number of cores available
available_cores = detectCores() - 1

# initialize a pseudo-cluster with available cores
pseudo_cluster = parallel::makeCluster(available_cores,
                                       type="FORK",
                                       setup_strategy="sequential", 
                                       outfile = './psr_log.txt', 
                                       verbose = TRUE)

# set seed for everyone
parallel::clusterSetRNGStream(pseudo_cluster, iseed = 42)

# parallelize our  analyses
doParallel::registerDoParallel(pseudo_cluster)
source('./scripts/03-data_analysis/parallel_psr-vic.R')
parallel_psr(movement_data_files, 
             processed_output_directory)

# stop the pseudocluster
stopCluster(pseudo_cluster)