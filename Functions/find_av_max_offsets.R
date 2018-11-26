find_av_max_offsets <- function(fault_name, spacing){
  
  best_offset <- readRDS(paste0('rms_difs/best_offset_',fault_name,'_',spacing,'.RDS'))
  dist_along_fault <- readRDS(paste0('Profiles/',fault_name,'_distances.RDS'))
  best_offset <- join(best_offset, dist_along_fault, by = 'prof_ind')
  
  # ggplot() + 
  #   geom_line(data=best_offset, aes(x=dist_along_fault,y=offset_height),color='magenta',size=1) +
  #   theme(legend.position="none")
  # 
  # ## SMO0THING
  # sm_num <- 100
  # best_offset$offset_height.sm <- c(rep(NA,sm_num/2-1),
  #                                   rollapply(best_offset$offset_height, sm_num, mean, na.rm = TRUE),
  #                                   rep(NA,sm_num/2))
  # ggplot() + 
  #   geom_line(data=best_offset, aes(x=dist_along_fault,y=offset_height.sm),color='magenta',size=1) +
  #   theme(legend.position="none")
  # 
  # # Histogram 
  # ggplot()+
  #   geom_histogram(data=best_offset, aes(x=offset_height.sm), binwidth = 0.1)
  
  # Cutoff outliers so 90% of data remains. 
  n_values <- length(best_offset$offset_height) - sum(is.na(best_offset$offset_height))
  offset_height_sorted <- sort(best_offset$offset_height)
  offset_height_sorted_trim <- offset_height_sorted[round(n_values*0.05):round(n_values*0.95)]
  
  ## Find average and max from trimmed data - exclude 10% lowest and highest values 
  av_offset <- mean(offset_height_sorted_trim)
  max_offset <- max(offset_height_sorted_trim)
  
  offsets <- c(av_offset, max_offset)
  
  saveRDS(offsets,paste0('Offsets/offsets_',fault_name,'_',spacing,'.RDS'))
}