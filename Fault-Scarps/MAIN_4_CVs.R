## Take offsets and plot in order of confidence intervals 
# Sometimes it's ok if the gradient of planes on either side of the fault are different
# Sometimes the gradient is quite similar to the fault.... e.g. in Malawi 

for (i in i_choices){ 
  fault_name      <- fault_name_list[i]

  print(paste0('MAIN_3_CVs ',fault_name))
  
  ### LOAD DATA NEEDED ###
  # Original profiles, segmented profiles, distances along fault of profiles, fault planes found
  profiles_list   <- readRDS ( paste0 ( 'Results/Profiles/',fault_name,'_prof_dist.RDS') )
  scarp_profiles  <- profiles_list[[2]]   # Original profiles 
  scarp_distances <- profiles_list[[3]]   # Distances along fault 
  seg_profiles    <- profiles_list[[4]]   # Segmented planes
  okp_all         <- profiles_list[[6]]   # Fault planes found 
  n_fp_combos     <- length(okp_all[[1]]) # Number of combinations to get fault planes 
  # Offsets and landscape planes found 
  height_pl_list  <- readRDS ( paste0 ( 'Results/Offset_Planes/', fault_name , '_offsets_planes.RDS') )
  height_offset   <- height_pl_list[[2]]  # Offsets
  found_planes    <- height_pl_list[[3]]  # Found landscape planes
  
  
  ## BEST FAULTS ?  
  # If multiple variations of fault planes have been found i.e. differeng grad combos, can select the best here
  no_pl_found <- c()   # Number of fault planes found
  conf_mean   <- c()   # Average confidence of planes found 
  for (i in 1:n_fp_combos){
    no_pl_found[i] <- nrow  ( height_offset [[i]])
    conf_mean[i]   <- round ( mean ( height_offset [[i]]$offset , na.rm = TRUE ) * 10 ) / 10
  }
  # Find grad combos in top half of num fault planes found and with confidence value < 0.3 below best 
  ok_faults        <- intersect ( which ( no_pl_found >= median ( no_pl_found , na.rm=TRUE )),
                                  which ( ( mean ( conf_mean , na.rm=TRUE ) - conf_mean ) < 0.3 ))
  # If the top half of num of planes wasn't near confidence top, try
  if ( length ( ok_faults ) == 0 ) {  
    ok_faults      <- intersect ( which ( no_pl_found >= median ( no_pl_found , na.rm=TRUE)),
                                  which ( conf_mean   >= median ( conf_mean   , na.rm=TRUE)))
  }
  best_fault       <- ok_faults[which(no_pl_found[ok_faults]==max(no_pl_found[ok_faults]))]
  
  # Now continue with only the best fault planes found 
  height_offset_b    <- height_offset [[best_fault]]
  okp_b              <- lapply ( okp_all , function(x) x[[best_fault]])
  found_planes_b     <- found_planes  [[best_fault]]
  
  
  ## Remove those with less than mean - 2*standard deviation 
  CVlist <- c('confid_dif_l','confid_dif_r','confid_pl_l','confid_pl_r','confid_s_pl')
  height_offset_b$confid_s_pl <- max( height_offset_b$confid_s_pl, na.rm = TRUE) -  height_offset_b$confid_s_pl
  
  
  keep_val_above <- c()
  for (j in 1:length(CVlist)){
    CV_nam         <- CVlist[j]
    height_offset_b[paste0(CV_nam,'_n')] <- NA
    CV_val         <- height_offset_b[CV_nam][,1]
    mean_CV        <- mean(CV_val,na.rm=TRUE) 
    sd_CV          <- sd  (CV_val,na.rm=TRUE) 
    keep_val_above <- mean_CV - 2 * sd_CV
    keep_val_max   <- mean_CV + 2 * sd_CV
    good_ind       <- which(CV_val >= keep_val_above)
    CV_val_keep    <- CV_val[good_ind]
    CV_val_keep[CV_val_keep>keep_val_max] <- keep_val_max
    normalization  <- keep_val_max - keep_val_above
    CV_val_keep    <- CV_val_keep  / normalization - keep_val_above / normalization 
    height_offset_b[paste0(CV_nam,'_n')][good_ind,] <- CV_val_keep
  }
  
  height_offset_b$confid <- height_offset_b$confid_dif_l_n + height_offset_b$confid_dif_r_n +
    height_offset_b$confid_pl_l_n + height_offset_b$confid_pl_r_n + height_offset_b$confid_s_pl_n
  keep_val_above <- mean(height_offset_b$confid,na.rm=TRUE)  - 2 * sd  (height_offset_b$confid,na.rm=TRUE) 
  keep_val_max   <- mean(height_offset_b$confid,na.rm=TRUE)  + 2 * sd  (height_offset_b$confid,na.rm=TRUE) 
  good_ind       <- which(height_offset_b$confid >= keep_val_above)
  height_offset_b$confid_n <- NA
  height_offset_b$confid_n[good_ind] <- height_offset_b$confid[good_ind]
  height_offset_b$confid_n[height_offset_b$confid_n>keep_val_max] <- keep_val_max
  normalization  <- keep_val_max - keep_val_above
  height_offset_b$confid_n <- height_offset_b$confid_n / normalization - keep_val_above / normalization 
  
  ## ADD offsets_CV column for those remaining within CV
  height_offset_b$offset_CV <- NA
  height_offset_b$offset_CV [ !is.na(height_offset_b$confid_n) ] <- height_offset_b$offset_original [ !is.na(height_offset_b$confid_n) ]
  
  # ## PLOTS multiplots  ordered CONFIDENCE INTERVALS to choose confidence intervals 
  # num_plots <- 14
  # height_offset_b$offset <- height_offset_b$offset_original
  # plot_multi_arrange ( height_offset_b , num_plots , scarp_profiles , seg_profiles , okp_b , found_planes_b , 'prof_ind'       )
  # plot_multi_arrange ( height_offset_b , num_plots , scarp_profiles , seg_profiles , okp_b , found_planes_b , 'confid_n'       )
  # plot_multi_arrange ( height_offset_b , num_plots , scarp_profiles , seg_profiles , okp_b , found_planes_b , 'confid_dif_l_n' )
  # plot_multi_arrange ( height_offset_b , num_plots , scarp_profiles , seg_profiles , okp_b , found_planes_b , 'confid_dif_r_n' )
  # plot_multi_arrange ( height_offset_b , num_plots , scarp_profiles , seg_profiles , okp_b , found_planes_b , 'confid_pl_l_n'  )
  # plot_multi_arrange ( height_offset_b , num_plots , scarp_profiles , seg_profiles , okp_b , found_planes_b , 'confid_pl_r_n'  )
  # plot_multi_arrange ( height_offset_b , num_plots , scarp_profiles , seg_profiles , okp_b , found_planes_b , 'confid_s_pl_n'  )
  # plot_multi_arrange ( height_offset_b , num_plots , scarp_profiles , seg_profiles , okp_b , found_planes_b , 'offset_original')
  # height_offset_b <- subset(height_offset_b, select = -offset)
  # 
  # ## PLOTS multiplots AFTER CV SUBSETTING
  # height_offset_b$offset <- height_offset_b$offset_CV
  # plot_multi_arrange ( height_offset_b , num_plots , scarp_profiles , seg_profiles , okp_b , found_planes_b , 'prof_ind' )
  # plot_multi_arrange ( height_offset_b , num_plots , scarp_profiles , seg_profiles , okp_b , found_planes_b , 'confid_n' )
  # plot_multi_arrange ( height_offset_b , num_plots , scarp_profiles , seg_profiles , okp_b , found_planes_b , 'offset_CV')
  # height_offset_b <- subset(height_offset_b, select = - offset)
  # 
  # 
  # ## PDF of PLOTS
  # pdf(file = paste0(main_results_dir,'/',conf_vals_dir,'/',fault_name,'_CVs.pdf'))
  # plot(multi_scarp_profiles_prof_ind)
  # plot(multi_height_offset_b_offset_original)
  # plot(multi_height_offset_b_confid_n)
  # plot(multi_height_offset_b_confid_dif_l_n)
  # plot(multi_height_offset_b_confid_dif_r_n)
  # plot(multi_height_offset_b_confid_pl_l_n)
  # plot(multi_height_offset_b_confid_pl_r_n)
  # plot(multi_height_offset_b_confid_s_pl_n)
  # dev.off()
  # 
  # ## PDF of PLOTS SUBSETTED
  # pdf(file = paste0(main_results_dir,'/',conf_vals_be_dir,'/',fault_name,'_CVsBest.pdf'))
  # plot(multi_scarp_profiles_prof_ind)
  # plot(multi_height_offset_b_offset_CV)
  # plot(multi_height_offset_b_confid_n)
  # dev.off()
  
  # Save best fault files
  list_details <- c('details',
                    'height_offset_orig',  'found_planes_orig',  'okp_orig',
                    'height_offset_bestF', 'found_planes_bestF', 'okp_bestF')
  saveRDS ( list(list_details,
                 height_offset   , found_planes   , okp_all  ,
                 height_offset_b , found_planes_b , okp_b),
            paste0 ( 'Results/Offset_Planes/' , fault_name , '_offsets_planes.RDS') )
  
  
  #  }
  
}

## PLOT
# Data after subsetted by confidence values 
height_pl_bF_s_list      <- readRDS ( paste0 ( 'Results/Offset_Planes/' , fault_name , '_offsets_planes.RDS') )
height_offset           <- height_pl_bF_s_list[[5]]
ho           <- subset(height_offset, !is.na(offset_CV) & offset_CV!=0)
ho$f2        <- ho$f
ho$offset_f2 <- ho$offset_CV 
ho$confid_f2 <- ho$confid_n

ggplot(ho)+
  geom_point(aes(x=dist_along_fault/1000,y=offset_original,color=confid_n),size=3)+
  scale_color_distiller(palette = "Spectral",direction = -1,limits=c(0,1.1)) +
  ggtitle('Zomba Fault') + 
  xlab('Distance along Fault, km') +
  ylab('Vertical Offset, m') +
  labs(color='Conf.') +
  theme_linedraw()         +
  theme(
    panel.grid   = element_blank(),
    title        = element_text(size = 7,vjust=-0.5),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text    = element_text(size = 7),
    legend.title = element_text(size = 7), 
    legend.text  = element_text(size = 7),
    legend.key.height = unit(0.6,"cm"),
    legend.key.width = unit(0.3,"cm")
  ) 



