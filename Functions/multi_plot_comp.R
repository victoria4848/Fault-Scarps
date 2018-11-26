multi_plot_comp <- function(FaultNameChoice,spacing){
  
  # Load Manual Profiles #
  manual_crba <- readRDS(paste0('Manual_crba/',FaultNameChoice,'_spacing_',spacing,'.RDS'))
  # If lhs of profile was clicked, it means no scarp found, so NA
  manual_crba$pointNumber[manual_crba$pointNumber < 5] = NA
  manual_crba$pointNumber2[manual_crba$pointNumber2 < 5] = NA
  num_manual_crba <- length(unique(manual_crba$prof_num))
  # # # # # # # # # # # # # 
  
  # Load scarp profiles and select those which have been done manually # 
  scarp_profiles <-  readRDS(paste0('Profiles/',FaultNameChoice,'_profile.RDS'))
  scarp_profiles_manual <- scarp_profiles[manual_crba$prof_ind]
  # # # # # # # # # # # # # 
  
  algorithm_offset <- readRDS(paste0('algorithm_offsets/algorithm_offset_',FaultNameChoice,'_',spacing,'.RDS'))
  
  
  # Divide into eights so i can plot at same time and inspect 
  ind_check <- list()
  for (j in (1:floor(num_manual_crba/8))){  #which indices in which plot
    ind_check[[j]] <- seq(1,8,1)+(j-1)*8
  }
  if (num_manual_crba%%8 !=0){
    ind_check[[j+1]] <- seq(1,num_manual_crba%%8,1)+j*8
  }
  
  colors <- viridis(n=length(ind_check))
  plot_multi <- list()
  prof_num <- 0
  for (n in 1:length(ind_check)){
    pl <- list()
    for (i in 1:length(ind_check[[n]])){
      prof_num <- prof_num+1
      prof_ind <- manual_crba$prof_ind[prof_num]
      da <- algorithm_offset[[prof_num]]
      df <- scarp_profiles_manual[[prof_num]]
      pl[[i]] <- ggplot() + geom_path(data=df, aes(x=DistTotal,y=height),na.rm=TRUE) +
        theme(axis.title=element_blank(), title = element_text(size = 6))+
        ggtitle(paste(prof_ind)) 
      # Add algorithm found points
      for (j in 1:length(ind_check)){
        pl[[i]] <- pl[[i]] +  geom_point(data=df[da[j,]$crest,],aes(x=DistTotal, y=height),color=colors[j], na.rm=TRUE)
        pl[[i]] <- pl[[i]] +  geom_point(data=df[da[j,]$base,], aes(x=DistTotal, y=height),color=colors[j], na.rm=TRUE)
      }
      # Add manually found points
      pl[[i]]  <-  pl[[i]] + geom_point(data=df[c(manual_crba$pointNumber[prof_num]+3,
                                                  manual_crba$pointNumber2[prof_num]+3),],
                                        aes(x=DistTotal,y=height),color='magenta',size=1, na.rm=TRUE)
    }
    nCol <- floor(sqrt(length(pl)))
    plot_multi[[n]] <- do.call("grid.arrange", c(pl, ncol=nCol))
  }
  
  
  saveRDS(plot_multi,paste0('Plots/plot_multi_',FaultNameChoice,'_',spacing,'.RDS'))
  
  
}