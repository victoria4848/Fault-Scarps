####################################################################################
####### Scarp offest versus length  #######
####################################################################################
# Find offsets along scarp for all profiles by testing various parameters
spacing <- 100  # metres: distance along faults that manual profiles have been done

######### ######### CREST-BASE ALGORITHM PARAMETER CHOICES ######### ######### 
SlopeRange      <- c(0.01,0.1)
SlopeStep       <- 0.01
CurvatureRange  <- c(0.001,0.01)
CurvatureStep   <- 0.001
SteepInRowRange <- c(2,4)
SteepInRowStep  <- 2
##############################################################
redo <- 'no'  # 'no' or 'yes': If no, if the functions have already been run, they won't run again
###############################

slope_cutoff_choices <- seq(SlopeRange[1],SlopeRange[2],SlopeStep)
curve_cutoff_choices <- seq(CurvatureRange[1],CurvatureRange[2],CurvatureStep)
steep_in_a_row_choices <- seq(SteepInRowRange[1],SteepInRowRange[2],SteepInRowStep)

exclude <- 5  #Number of points to exclude either side of crest and base for forming planes to allow for error in picking
manual_profiles_done <- list.files(path='Manual_crba/',pattern=paste(spacing))
f2 <- function(x) strsplit(x, split='_')[[1]][1]
fault_name_list <- unlist(lapply(manual_profiles_done,f2))
source('Functions/libraries_functions.R') 

for (i in 1:length(fault_name_list)){ 
  fault_name <- fault_name_list[i]
  
  print(paste0('Starting ',fault_name))
  
  ##############################################################
  length_profs <- readRDS(paste0('Faults_R/',fault_name,'_lengths.RDS'))[5]
  length_profs <- sum(length_profs[[1]]$length)
  alg_prof_maxNAs <- floor(length_profs/spacing/2)  # Maximum number of NA values to accept in parameters
  
  ######### ######### HEIGHT FROM MANUAL CREST BASE ######### ######### 
  if (!file.exists(paste0('Manual_profiles_offsets/manual_offset_',fault_name,'_',spacing,'.RDS'))| redo!='no'){
    print('Doing height from manual crest base') 
    function_offset_from_crest_base_manual(fault_name,spacing,exclude)
  }
  ##############################################################
  
  ######### ######### CREST & BASE FROM ALGORITHM  ######### ######### 
  if (!file.exists(paste0('Algorithm_offsets/algorithm_crba_',fault_name,'.RDS'))| redo!='no'){
    print('Doing crest base from algorithm')
    function_crest_base_algorithm(fault_name,slope_cutoff_choices,
                                  curve_cutoff_choices,steep_in_a_row_choices)
  }
  ##############################################################
  
  ######### ######### HEIGHT FROM ALGORITHM CREST BASE ######### ######### 
  if (!file.exists(paste0('algorithm_offsets/algorithm_offset_df_',fault_name,'_',spacing,'.RDS'))| redo!='no'){
    print('Doing height from algorithm crest base')
    function_offset_from_crest_base_algorithm(fault_name,spacing,exclude)
  }
  ##############################################################
  
  ######### ######### FIND RMS DIFFERENCE MANUAL / ALGORITHM ######### ######### 
#  if (!file.exists(paste0('rms_difs/conds_difs_',fault_name,'_',spacing,'.RDS'))| redo!='no'){
    print('Doing rms differences') 
    enough <- rms_difs(fault_name,
                       slope_cutoff_choices,
                       curve_cutoff_choices,
                       steep_in_a_row_choices,
                       spacing,
                       alg_prof_maxNAs)
 # }
  ##############################################################
  
  if (enough == TRUE){
    ######### ######### PLOT RMS DIFS and OFFSET PROFILES ######### #########
    if (!file.exists(paste0('Plots/plots',fault_name,'_',spacing,',RDS'))| redo!='no'){
      print('Doing rms plots etc')
      plots_list <- plots(fault_name,spacing,alg_prof_maxNAs) #list of 4 plots if result
    }
    ##############################################################
    
    ######### ######### MULTIPLE COMPARISON PLOTS: MANUAL vs ALGORITHM ######### #########
    if (!file.exists(paste0('Plots/plot_multi_',fault_name,'_',spacing,'.RDS'))| redo!='no'){
      print('Doing multicomparison plot')
      plot_multi <- multi_plot_comp(fault_name,spacing)
    }
    ##############################################################
    
    ######### ######### FIND AVERAGE AND MAX OFFSET and RATIO ######### #########
    if (!file.exists(paste0('Offsets/offsets_',fault_name,'_',spacing,'.RDS'))| redo!='no'){
      print('Finding average and max offsets')
      find_av_max_offsets(fault_name,spacing) 
    }
    ##############################################################

  }
  print(paste0('Finished ',fault_name))
}

## Final comparison of lengths versus average and max offsets
source('Functions/final_plot.R')


######### ######### LOAD FILES ######### #########
#manual_profiles_offset <- readRDS(paste0('manual_profiles_offsets/manual_offset_',fault_name,'_',spacing,'.RDS'))
#algorithm_crba <- readRDS(paste0('algorithm_offsets/algorithm_crba_',fault_name,'.RDS') )
#algorithm_offset <- readRDS(paste0('algorithm_offsets/algorithm_offset_',fault_name,'_',spacing,'.RDS'))
#alg.comb.df <- readRDS(paste0('algorithm_offsets/algorithm_offset_df_',fault_name,'_',spacing,'.RDS'))
#conds_difs <- readRDS(paste0('rms_difs/conds_difs_',fault_name,'_',spacing,'.RDS'))
## PLOTS
#plots <- readRDS(paste0('plots/plots',fault_name,'_',spacing,',RDS'))
#plot_multi <- readRDS(paste0('plots/plot_multi',fault_name,'_',spacing,'.RDS'))
#for (n in 1:length(plot_multi)){
#  plot(plot_multi[[n]])
#}



# ######### ######### COMPARISON PLOTS: MANUAL vs ALGORITHM ######### ######### 
# prof_num <- 5
# plot_sing <- single_plot_comp(fault_name,spacing,prof_num)
# ##############################################################


# #get indices of NA in man_height and in alg_height 
# man_na_ind <- unique(manual_crba$prof_ind[is.na(manual_crba$pointNumber)])
# # for alg_height, will get length_choices number of a list of things
# alg_na_ind <- list()
# for (i in 1:length_choices){
#   alg_na_ind[[i]] <- unique(manual_crba$prof_ind[is.na(alg_height[,i])])
# }
# ## Finish deciding penalty points for not matching heights
# ## Make for each parameters, the mean difference, and the number of NA profiles, 
# # and the number of false positives. 
# ##############################################################

