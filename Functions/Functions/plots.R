plots <- function(fault_name,spacing,alg_prof_maxNAs){
  
  conds_difs <- readRDS(paste0('rms_difs/conds_difs_',fault_name,'_',spacing,'.RDS'))
  best_ind <- which(conds_difs$abs_dif==min(subset(conds_difs,NAs < alg_prof_maxNAs)$abs_dif ) ) 
  
  alg.comb.df <- data.frame(readRDS(paste0('algorithm_offsets/algorithm_offset_df_',fault_name,'_',spacing,'.RDS')))
  manual_profiles_offset <- readRDS(paste0('manual_profiles_offsets/manual_offset_',fault_name,'_',spacing,'.RDS'))
  best_offset <- readRDS(paste0('rms_difs/best_offset_',fault_name,'_',spacing,'.RDS'))
  dist_along_fault <- readRDS(paste0('Profiles/',fault_name,'_distances.RDS'))
  
  alg.comb.df<- join(alg.comb.df, dist_along_fault, by = 'prof_ind')
  manual_profiles_offset <- join(manual_profiles_offset, dist_along_fault, by = 'prof_ind')
  best_offset <- join(best_offset, dist_along_fault, by = 'prof_ind')
  
  plot_dif <- ggplot() + 
    geom_tile(data=conds_difs,aes(x=slope_cutoff, y=curve_cutoff, fill = abs_dif)) +
    theme_bw()+
    ggtitle(fault_name)
  plot_na <- ggplot() + 
    geom_tile(data=conds_difs,aes(x=slope_cutoff, y=curve_cutoff, fill = NAs)) +
    theme_bw()+
    ggtitle(fault_name)
  
  g <- c() 
  if (sum(alg.comb.df$offset_height, na.rm=TRUE)!=0){
    g <- ggplot() + 
      geom_line(data=manual_profiles_offset, aes(x=dist_along_fault,y=offset_height),color='black',size=5)+
      geom_line(data=alg.comb.df,aes(x=dist_along_fault, y=offset_height,group = indx,color=indx)) +
      theme(legend.position="none")+
      ggtitle(fault_name)
  }
  
  
  f <- ggplot() + 
    geom_line(data=manual_profiles_offset, aes(x=dist_along_fault,y=offset_height),color='black',size=5)+
    geom_line(data=subset(alg.comb.df,indx%in%best_ind),aes(x=dist_along_fault, y=offset_height,group = indx,color=indx)) +
    theme(legend.position="none")+
    ggtitle(fault_name)
  
  h <- ggplot() + 
    geom_line(data=manual_profiles_offset, aes(x=dist_along_fault,y=offset_height),color='black',size=5)+
    geom_line(data=best_offset, aes(x=dist_along_fault,y=offset_height),color='magenta',size=1) +
    theme(legend.position="none")+
    ggtitle(fault_name)
  
  # Plot distance versus all height offsets for best index 
  
  plots <- list(plot_dif, plot_na,g,h)
  saveRDS(plots,paste0('plots/plots_',fault_name,'_',spacing,'.RDS'))
  
  return(plots)
  
}