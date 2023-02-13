####### Visual Information in Coordination: Supplementary Code for Data Analysis #######

# In this script, we have some supporting code for our statistical analyses (to make
# our main analysis script a bit prettier).

#############################################################################

##### 00. Call libraries #####

# load libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(cowplot)
library(webshot)
library(viridis)
library(simr)

##### 01. Create or call new functions #####

# thanks to https://rdrr.io/github/kylebutts/kfbmisc/src/R/ggplot_helpers.R
png_to_grob <- function(source) {
  temp <- png::readPNG(source)
  cli::cli_alert_info("Image has width = {dim(temp)[2]}px and height = {dim(temp)[1]}px")
  temp <- grid::rasterGrob(temp, interpolate = TRUE)
  return(temp)
}

# create a new function to output nicer tables
create_publication_tables = function(model_name, 
                                     dv_label, 
                                     pred_label_list, 
                                     table_name, 
                                     output_dir){
  
  # create table and png file names
  html_table_file = paste0(output_dir,table_name,"-vic.html")
  png_table_file = paste0(output_dir,table_name,"-vic.png")
  
  # create a table for us
  temp_table = sjPlot::tab_model(model_name,
                                 dv.labels = dv_label,
                                 pred.labels = pred_label_list,
                                 show.re.var = FALSE,
                                 show.obs = FALSE,
                                 show.icc = FALSE,
                                 show.ngroups = FALSE,
                                 file = html_table_file)
  print(temp_table)
  webshot(html_table_file,
          png_table_file,
          selector = 'table') %>%
    resize("150%")
  return(png_to_grob(paste0(output_dir,table_name,"-vic.png")))
}

# convert conversation order to conversation number

order_key_df = read.csv('./scripts/03-data_analysis/order_variable_key-vic.csv') %>%
  pivot_longer(cols = c(conv_1,conv_2,conv_3),
               names_to = "conv_num") %>%
  mutate(conv_num = as.integer(gsub("conv_","", conv_num)))

##### 02. Create long lists we'll need later #####

# create labels for experimental design predictors
experimental_pred_labels = c("Intercept",
                             "Arg.",
                             "Coop.",
                             "VC Remote",
                             "VC Lab",
                             "Arg. x VC Remote",
                             "Coop. x VC Remote",
                             "Arg. x VC Lab",
                             "Coop. x VC Lab")

# create labels for lag interactions with experimental design predictors
drp_pred_labels = c("Intercept",
                    "Linear Lag",
                    "Quadratic Lag",
                    "Arg.",
                    "Coop.",
                    "VC Remote",
                    "VC Lab",
                    "L. Lag x Q. Lag",
                    "Linear Lag x Arg.",
                    "Linear Lag x Coop.",
                    "Quadratic Lag x Arg.",
                    "Quadratic Lag x Coop.",
                    "Linear Lag x VC Remote",
                    "Linear Lag x VC Lab",
                    "Quadratic Lag x VC Remote",
                    "Quadratic Lag x VC Lab",
                    "Arg. x VC Remote",
                    "Coop. x VC Remote",
                    "Arg. x VC Lab",
                    "Coop. x VC Lab",
                    "L. Lag x Q. Lag x Arg.",
                    "L. Lag x Q. Lag x Coop.",
                    "L. Lag x Q. Lag x VC Remote",
                    "L. Lag x Q. Lag x VC Lab",
                    "L. Lag x Arg. x VC Remote",
                    "L. Lag x Coop. x VC Remote",
                    "L. Lag x Arg. x VC Lab",
                    "L. Lag Coop. x VC Lab",
                    "Q. Lag x Arg. x VC Remote",
                    "Q. Lag x Coop. x VC Remote",
                    "Q. Lag x Arg. x VC Lab",
                    "Q. Lag x Coop. x VC Lab",
                    "L. Lag x Q. Lag x Arg. x VC Remote",
                    "L. Lag x Q. Lag x Coop. x VC Remote",
                    "L. Lag x Q. Lag x Arg. x VC Lab",
                    "L. Lag x Q. Lag x Coop. x VC Lab")

# create labels for drp-level exploratory tables
best_exploratory_pred_table_labels = c("(Intercept)",
                                       "Log RR",
                                       "DET",
                                       "maxL",
                                       "Max DRP RR",
                                       "Arg.",
                                       "Coop.",
                                       "VC Remote",
                                       "VC Lab",
                                       "Log RR x Arg.",
                                       "Log RR x Coop.",
                                       "DET x Arg.",
                                       "DET x Coop.",
                                       "maxL x Arg.",
                                       "maxL x Coop.",
                                       "Max DRP RR x Arg.",
                                       "Max DRP RR x Coop.",
                                       "Log RR x VC Remote",
                                       "Log RR x VC Lab",
                                       "DET x VC Remote",
                                       "DET x VC Lab",
                                       "maxL x VC Remote",
                                       "maxL x VC Lab",
                                       "Max DRP RR x VC Remote",
                                       "Max DRP RR x VC Lab",
                                       "Arg. x VC Remote",
                                       "Coop. x VC Remote",
                                       "Arg. x VC Lab",
                                       "Coop. x VC Lab",
                                       "Log RR x Arg. x VC Remote",
                                       "Log RR x Coop. x VC Remote",
                                       "Log RR x Arg. x VC Lab",
                                       "Log RR x Coop. x VC Lab",
                                       "DET x Arg. x VC Remote",
                                       "DET x Coop. x VC Remote",
                                       "DET x Arg. x VC Lab",
                                       "DET x Coop. x VC Lab",
                                       "maxL x Arg. x VC Remote",
                                       "maxL x Coop. x VC Remote",
                                       "maxL x Arg. x VC Lab",
                                       "maxL x Coop. x VC Lab",
                                       "Max DRP RR x Arg. x VC Remote",
                                       "Max DRP RR x Coop. x VC Remote",
                                       "Max DRP RR x Arg. x VC Lab",
                                       "Max DRP RR x Coop. x VC Lab")


# create labels for drp-level exploratory tables
best_exploratory_pred_table_labels = c("(Intercept)",
                                       "Log RR",
                                       "DET",
                                       "maxL",
                                       "Max DRP RR",
                                       "Arg.",
                                       "Coop.",
                                       "VC Remote",
                                       "VC Lab",
                                       "Log RR x Arg.",
                                       "Log RR x Coop.",
                                       "DET x Arg.",
                                       "DET x Coop.",
                                       "maxL x Arg.",
                                       "maxL x Coop.",
                                       "Max DRP RR x Arg.",
                                       "Max DRP RR x Coop.",
                                       "Log RR x VC Remote",
                                       "Log RR x VC Lab",
                                       "DET x VC Remote",
                                       "DET x VC Lab",
                                       "maxL x VC Remote",
                                       "maxL x VC Lab",
                                       "Max DRP RR x VC Remote",
                                       "Max DRP RR x VC Lab",
                                       "Arg. x VC Remote",
                                       "Coop. x VC Remote",
                                       "Arg. x VC Lab",
                                       "Coop. x VC Lab",
                                       "Log RR x Arg. x VC Remote",
                                       "Log RR x Coop. x VC Remote",
                                       "Log RR x Arg. x VC Lab",
                                       "Log RR x Coop. x VC Lab",
                                       "DET x Arg. x VC Remote",
                                       "DET x Coop. x VC Remote",
                                       "DET x Arg. x VC Lab",
                                       "DET x Coop. x VC Lab",
                                       "maxL x Arg. x VC Remote",
                                       "maxL x Coop. x VC Remote",
                                       "maxL x Arg. x VC Lab",
                                       "maxL x Coop. x VC Lab",
                                       "Max DRP RR x Arg. x VC Remote",
                                       "Max DRP RR x Coop. x VC Remote",
                                       "Max DRP RR x Arg. x VC Lab",
                                       "Max DRP RR x Coop. x VC Lab")

# create labels for drp-level exploratory tables
exploratory_order_pred_table_labels = c("(Intercept)",
                                        "Log RR",
                                        "DET",
                                        "Arg.",
                                        "Coop.",
                                        "Conv. Num.",
                                        "VC Remote",
                                        "VC Lab",
                                        "Log RR x Arg.",
                                        "Log RR x Coop.",
                                        "DET x Arg.",
                                        "DET x Coop.",
                                        "Log RR x Conv. Num.",
                                        "DET x Conv. Num.",
                                        "Arg. x Conv. Num.",
                                        "Coop. x Conv. Num.",
                                        "Log RR x VC Remote",
                                        "Log RR x VC Lab",
                                        "DET x VC Remote",
                                        "DET x VC Lab",
                                        "Arg. x VC Remote",
                                        "Coop. x VC Remote",
                                        "Arg. x VC Lab",
                                        "Coop. x VC Lab",
                                        "Conv. Num. x VC Remote",
                                        "Conv. Num. x VC Lab",
                                        "Log RR x Arg. x Conv. Num.",
                                        "Log RR x Coop. x Conv. Num.",
                                        "DET x Arg. x Conv. Num.",
                                        "DET x Coop. x Conv. Num.",
                                        "Log RR x Arg. x VC Remote",
                                        "Log RR x Coop. x VC Remote",
                                        "Log RR x Arg. x VC Lab",
                                        "Log RR x Coop. x VC Lab",
                                        "DET x Arg. x VC Remote",
                                        "DET x Coop. x VC Remote",
                                        "DET x Arg. x VC Lab",
                                        "DET x Coop. x VC Lab",
                                        "Log RR x Conv. Num. x VC Remote",
                                        "Log RR x Conv. Num. x VC Lab",
                                        "DET x Conv. Num. x VC Remote",
                                        "DET x Conv. Num. x VC Lab",
                                        "Arg. x Conv. Num. x VC Remote",
                                        "Coop. x Conv. Num. x VC Remote",
                                        "Arg. x Conv. Num. x VC Lab",
                                        "Coop. x Conv. Num. x VC Lab",
                                        "Log RR x Arg. x Conv. Num. x VC Remote",
                                        "Log RR x Coop. x Conv. Num. x VC Remote",
                                        "Log RR x Arg. x Conv. Num. x VC Lab",
                                        "Log RR x Coop. x Conv. Num. x VC Lab",
                                        "DET x Arg. x Conv. Num. x VC Remote",
                                        "DET x Coop. x Conv. Num. x VC Remote",
                                        "DET x Arg. x Conv. Num. x VC Lab",
                                        "DET x Coop. x Conv. Num. x VC Lab")
