####### Visual Information in Coordination: Cross-Recurrence Quantification Analysis #######

# In this script, we'll do some phase-space reconstruction to prepare for CRQA.
# Part of data analysis for Romero & Paxton (2021). To make everything more
# efficient, we'll go ahead and use local parallelization (rather than doing
# them all in sequential order).

#############################################################################

# preliminaries
rm(list=ls())

# try to avoid memory allocation issues
Sys.setenv('R_MAX_VSIZE'=32000000000)

# load libraries
library(crqa)
library(tidyverse)
library(tseriesChaos)
library(doParallel)

# set seed
set.seed(42)

# get our list of files
movement_data_files = list.files(path = './data/movement_dataframes-aggregated',
                                 pattern = "*.csv",
                                 full.names = TRUE)

# create directories for our output, if we don't have them yet
crqa_output_directory = "./data/crqa"
dir.create(crqa_output_directory,
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
source('./scripts/03-data_analysis/parallel_crqa-vic.R')
parallel_crqa(movement_data_files, 
              crqa_output_directory)

# stop the pseudocluster
stopCluster(pseudo_cluster)