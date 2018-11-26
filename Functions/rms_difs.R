rms_difs <- function(fault_name,
                     slope_cutoff_choices,
                     curve_cutoff_choices,
                     steep_in_a_row_choices,
                     spacing,
                     alg_prof_maxNAs){
  
  # Load Manual Profiles #
  manual_crba <- readRDS(paste0('Manual_crba/',fault_name,'_spacing_',spacing,'.RDS'))
  algorithm_crba <- readRDS(paste0('algorithm_offsets/algorithm_crba_',fault_name,'.RDS'))
  num_manual_crba <- length(unique(manual_crba$prof_num))
  manual_profiles_offset <- readRDS(paste0('manual_profiles_offsets/manual_offset_',fault_name,'_',spacing,'.RDS'))
  algorithm_offset <- readRDS(paste0('algorithm_offsets/algorithm_offset_',fault_name,'_',spacing,'.RDS'))
  scarp_profiles <-  readRDS(paste0('Profiles/',fault_name,'_profile.RDS'))
  
  length_choices <- length(slope_cutoff_choices)*length(steep_in_a_row_choices)*length(curve_cutoff_choices)
  
  
  abs_dif <-    matrix(NA,nrow=num_manual_crba, ncol=length_choices)
  alg_height <- matrix(NA,nrow=num_manual_crba, ncol=length_choices)
  for (i in 1:num_manual_crba){
    # Manual offset_height
    man_height <-  manual_profiles_offset$offset_height[i]
    # Algorithm offset_height
    for (j in 1:length_choices){
      alg_height[i,j] <- algorithm_offset[[i]]$offset_height[j]
      # RMS differences
      abs_dif[i,j] <- abs(man_height-alg_height[i,j])
    }
  }
  dif_per_choices <- colMeans(abs_dif, na.rm=TRUE)
  NAs_per_chocies <- colSums(is.na(abs_dif))
  conds <- expand.grid(slope_cutoff=slope_cutoff_choices,steep_in_a_row=steep_in_a_row_choices,curve_cutoff=curve_cutoff_choices)
  conds_difs <- cbind(conds, abs_dif=dif_per_choices,NAs=NAs_per_chocies)
  
  # Find height offset for best parameters all the way along the fault
  if (nrow(subset(conds_difs,NAs < alg_prof_maxNAs))!=0){  # Some crest and base values must be found by algorithm
    
    best_ind <- which(conds_difs$abs_dif==min(subset(conds_difs,NAs < alg_prof_maxNAs)$abs_dif ) ) 
    algorithm_crba_best <- list()
    
    for (i in 1:length(algorithm_crba)){  # Select best paramter crest and bases
      algorithm_crba_best[[i]] <- algorithm_crba[[i]][best_ind,]
    }
    best_offset  <- list()
    for (i in 1:length(algorithm_crba)){
      best_offset[[i]] <- offset_from_crest_base(
        crest_base = algorithm_crba_best[[i]],
        profiles = scarp_profiles[[i]],
        exclude
      )
    }
    # Combine back into one dataframe
    best_offset.df <- best_offset[[1]]
    for (i in 2:length(best_offset)){
      best_offset.df <- rbind(best_offset.df,best_offset[[i]])
    }
    
    saveRDS(best_offset.df,paste0('rms_difs/best_offset_',fault_name,'_',spacing,'.RDS'))
    saveRDS(conds_difs,paste0('rms_difs/conds_difs_',fault_name,'_',spacing,'.RDS'))
    return(TRUE)
  }
  
  if (nrow(subset(conds_difs,NAs < alg_prof_maxNAs))==0){
    saveRDS(NA,paste0('rms_difs/best_offset_',fault_name,'_',spacing,'.RDS'))
    saveRDS(conds_difs,paste0('rms_difs/conds_difs_',fault_name,'_',spacing,'.RDS'))
    print('Not enough crests and bases found by algorithm - try different parameters')
    return(FALSE)
  }
}