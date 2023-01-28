#### parallel_crqa-vic.R  ####
#
# This script creates a function to run phase space reconstruction with the
# `parallel` function.
#
##############################

# create a function to be applied over a list of data file
parallel_crqa <- function(movement_data_files,
                          processed_output_directory) {
  
  # cycle through each subset
  foreach(i = seq_along(movement_data_files), .errorhandling='pass') %dopar% {
    
    # grab the next file
    this_file = movement_data_files[[i]]
    
    # grab the dyad we're analyzing and then reform wider
    this_dyad_df = read.csv(this_file,
                            header=TRUE) %>%
      tidyr::pivot_wider(names_from = participant,
                         values_from = movement,
                         names_prefix = "movement_") %>%
      mutate(movement_left = as.numeric(scale(movement_left)),
             movement_right = as.numeric(scale(movement_right)))
    
    # grab phase space reconstruction values
    conv = unique(this_dyad_df$dyad)
    psr_parameters = read.csv(paste0('./data/parameters/',
                                     conv,
                                     '.csv'))
    ami_selected = unique(psr_parameters$ami_selected)
    embed_selected = unique(psr_parameters$embed_selected)
    
    # truncate if necessary
    if ( length(this_dyad_df$movement_left) > 15000){
      max_length = 15000
    } else {
      max_length = length(this_dyad_df$movement_left)
    }
    
    # run CRQA
    this_crqa = crqa::crqa(ts1 = this_dyad_df$movement_left[1:max_length],
                           ts2 = this_dyad_df$movement_right[1:max_length],
                           delay = ami_selected,
                           embed = embed_selected,
                           rescale = 1,
                           radius = .5,
                           normalize = 0,
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
                              delay = ami_selected,
                              embed = embed_selected,
                              RR = this_crqa$RR,
                              DET = this_crqa$DET,
                              NRLINE = this_crqa$NRLINE,
                              maxL = this_crqa$maxL,
                              L = this_crqa$L,
                              ENTR = this_crqa$ENTR,
                              rENTR = this_crqa$rENTR,
                              LAM = this_crqa$LAM,
                              TT = this_crqa$TT,
                              max_length)
    
    # save to file
    write.csv(this_crqa_df,
              file = paste0(processed_output_directory,
                            '/crqa-',
                            conv,
                            '.csv'),
              row.names = FALSE)
    
    # free up memory
    rm(this_crqa)
    
    # calculate DRP
    this_drp = crqa::drpfromts(ts1 = this_dyad_df$movement_left[1:max_length],
                               ts2 = this_dyad_df$movement_right[1:max_length],
                               delay = ami_selected,
                               embed = embed_selected,
                               windowsize = 200,
                               rescale = 1,
                               radius = .5,
                               normalize = 0,
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
             delay = ami_selected,
             embed = embed_selected,
             max_length)
    
    # save to file
    write.csv(this_drp_df,
              file = paste0(processed_output_directory,
                            '/drp-',
                            conv,
                            '.csv'),
              row.names = FALSE)
    
    # free up memory
    rm(this_drp)
    
  }}

