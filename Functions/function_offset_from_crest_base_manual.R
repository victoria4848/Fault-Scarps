function_offset_from_crest_base_manual <- function(fault_name, spacing,exclude){
  
  # Load Manual Profiles #
  manual_crba <- readRDS(paste0('Manual_crba/',fault_name,'_spacing_',spacing,'.RDS'))
  # If lhs of profile was clicked, it means no scarp found, so NA
  manual_crba$pointNumber[manual_crba$pointNumber < 5] = NA
  manual_crba$pointNumber2[manual_crba$pointNumber2 < 5] = NA
  num_manual_crba <- length(unique(manual_crba$prof_num))
  # # # # # # # # # # # # # 
  
  # Load scarp profiles and select those which have been done manually # 
  scarp_profiles <-  readRDS(paste0('Profiles/',fault_name,'_profile.RDS'))
  scarp_profiles_manual <- scarp_profiles[manual_crba$prof_ind]
  # # # # # # # # # # # # # 
  
  ######### ######### HEIGHT FROM MANUAL CREST BASE ######### ######### 
  # Find scarp heigh offset, width and slope from manually picked crest and base #
  manual_offset<-data.frame(offset_height=rep(NA,num_manual_crba),offset_height2=NA,width=NA,widthmin=NA,slope=NA,slopemax=NA)
  for (i in 1:num_manual_crba){
    prof_ind <- manual_crba$prof_ind[i]
    left_ind  <- manual_crba$pointNumber2[i]
    right_ind <- manual_crba$pointNumber[i]
    # If profiles were clicked right then left, rather than left right
    if (!is.na(left_ind)& !is.na(right_ind)){
      if (left_ind > right_ind){swap(left_ind, right_ind)}
      df <- scarp_profiles_manual[[i]]
      # Find max gradient and index of the max gradient 
      deriv1 <- c(diff(df$height) / diff(df$DistTotal),NA)
      max_gra <- max(abs(deriv1[left_ind:right_ind]))
      if (max_gra!=0){
        ms_ind <- which(abs(deriv1)==max_gra)
        ms_ind <- ms_ind[(ms_ind >= left_ind) & (ms_ind <= right_ind)] #index has to be within range
        # Get the index that is nearest the centre of the scarp, if there are multiple
        ms_ind <-ms_ind[which(abs(ms_ind - mean(right_ind, left_ind))==min(abs(ms_ind - mean(right_ind, left_ind))))]
      }
      
      manual_offset[i,] <- offset_from_crest_base(crest_base = data.frame(crest=left_ind,base=right_ind,
                                                                          max_slope.ind=ms_ind,max_gra=max_gra),
                                                  df,exclude)
    }
    
  }
  manual_profiles_offset <- cbind(manual_crba,manual_offset)
  
  saveRDS(manual_profiles_offset,paste0('Manual_profiles_offsets/manual_offset_',fault_name,'_',spacing,'.RDS') )
  
}