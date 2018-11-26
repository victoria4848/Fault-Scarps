single_plot_comp <- function(FaultNameChoice,spacing,prof_num){
  
  manual_profiles_offset <- readRDS(paste0('manual_profiles_offsets/manual_offset_',FaultNameChoice,'_',spacing,'.RDS') )
  algorithm_offset <- readRDS(algorithm_offset,paste0('algorithm_offsets/algorithm_offset_',FaultNameChoice,'_',spacing,'.RDS'))
  
  # Load scarp profiles and select those which have been done manually # 
  scarp_profiles <-  readRDS(paste0('Profiles/',FaultNameChoice,'_profile.RDS'))
  scarp_profiles_manual <- scarp_profiles[manual_crba$prof_ind]
  # # # # # # # # # # # # # 
  
  colors=viridis(n=length_choices)
  p <- ggplot() 
  prof_ind <- manual_profiles_offset$prof_ind[prof_num]
  df <- scarp_profiles_manual[[prof_num]]
  p <- p + geom_path(data=df, aes(x=DistTotal,y=SMA1))
  da <- algorithm_offset[[prof_num]]
  for (i in 1:length_choices){
    p <- p +  geom_point(data=df[da[i,]$crest,],aes(x=DistTotal, y=SMA1),color=colors[i]) 
    p <- p +  geom_point(data=df[da[i,]$base,], aes(x=DistTotal, y=SMA1),color=colors[i]) 
  }
  # Add on manually found points
  p <- p + geom_point(data=df[c(manual_profiles_offset$pointNumber[prof_num]+3,
                                manual_profiles_offset$pointNumber2[prof_num]+3),],
                      aes(x=DistTotal,y=SMA1),color='magenta',size=0.7)

  return(p)
  
}