function_offset_from_crest_base_algorithm <- function(fault_name,spacing,exclude){
  
  # Load Manual Profiles #
  manual_crba <- readRDS(paste0('Manual_crba/',fault_name,'_spacing_',spacing,'.RDS'))
  num_manual_crba <- length(unique(manual_crba$prof_num))
  # Load algorithm profiles #
  algorithm_crba <- readRDS(paste0('algorithm_offsets/algorithm_crba_',fault_name,'.RDS') )
  # Load scarp profiles and select those which have been done manually # 
  scarp_profiles <-  readRDS(paste0('Profiles/',fault_name,'_profile.RDS'))
  scarp_profiles_manual <- scarp_profiles[manual_crba$prof_ind]
  # # # # # # # # # # # # # 
  
  # Get profiles that were found manually #
  algorithm_crba_msubset <- list()
  algorithm_crba_msubset <- algorithm_crba[manual_crba$prof_ind]
  # # # # # # # # # # # # # 
  
  algorithm_offset <- list()
  #for (i in 1:length(scarp_profiles)){
  for (i in 1:num_manual_crba){
    algorithm_offset[[i]] <- offset_from_crest_base(
      crest_base = algorithm_crba_msubset[[i]],
      profiles = scarp_profiles_manual[[i]],
      exclude
    )
  }
  
  # Get algorithm_offset in format of one dataframe per parameters, instead of per profile #
  algorithm_offset.byparams <- list()
  for (i in 1:nrow(algorithm_offset[[1]])) {
    algorithm_offset.byparams[[i]] <- data.frame(matrix(NA,num_manual_crba,length(algorithm_offset[[1]])))
    names(algorithm_offset.byparams[[i]]) <- names(algorithm_offset[[1]])
  }
  for (j in 1:nrow(algorithm_offset[[1]])){
    algorithm_offset.byparams[[j]][1,] <- algorithm_offset[[1]][j,]
    for (i in 2:num_manual_crba){
      algorithm_offset.byparams[[j]][i,] <- algorithm_offset[[i]][j,]
    }
  }
  # combined dataframe of indx,prof_ind,offset_height
  alg.comb.df <- algorithm_offset.byparams[[1]][c('indx','prof_ind','offset_height')]
  for (i in 1:nrow(algorithm_offset[[1]])){
    alg.comb.df <- rbind(alg.comb.df,algorithm_offset.byparams[[i]][c('indx','prof_ind','offset_height')])
  }
  
  saveRDS(algorithm_offset,paste0('algorithm_offsets/algorithm_offset_',fault_name,'_',spacing,'.RDS'))
  saveRDS(alg.comb.df,paste0('algorithm_offsets/algorithm_offset_df_',fault_name,'_',spacing,'.RDS'))
  
}