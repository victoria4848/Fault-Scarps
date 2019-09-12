# MAIN_13_PLOT_PROFILES


num_plots       <- 14
perc_p_found <- c()
gg <- 0
for (i in i_choices){
  gg <- gg + 1
  fault_name <- fault_name_list[i]
  # Load Data
  # Original profiles, segmented profiles, distances along fault of profiles, fault planes found
  profiles_list   <- readRDS ( paste0 ( 'Results/Profiles/',fault_name,'_prof_dist.RDS') )
  scarp_profiles  <- profiles_list[[2]]   # Original profiles 
  scarp_distances <- profiles_list[[3]] # Distances along fault 
  seg_profiles    <- profiles_list[[4]]   # Segmented planes
  
  list_prof_stuff <- readRDS ( paste0 ( 'Results/Offset_Planes/' , fault_name , '_offsets_planes.RDS') )
  height_offset_b <- list_prof_stuff[[5]]
  okp_b           <- list_prof_stuff[[7]]
  found_planes_b  <- list_prof_stuff[[6]]
  
  perc_p_found[gg] <- sum(!is.na(height_offset_b$offset_CV)) / nrow(height_offset_b)*100

  ## Find indices of scarp profiles to plot - arrange by arrange_name, then pick equally spaced
  n                    <- length ( scarp_profiles)
  ind_plots            <- seq    ( 1 , n , floor ( n / ( num_plots - 1 ))) 
  ind_plots[num_plots] <- n  # To make sure we get the end one
  
  ### MALAWI TEST ###
  ind_plots <- which(!is.na(height_offset_b$offset_original))
  ind_plots <- ind_plots[seq(1,length(ind_plots),35)]
 # ind_plots <- 1877
  ind_plots <- which(height_offset_b$confid_n > 0.9)
  
  ## Select by ind_plots found 
  # Select scarp profiles
  scarp_profiles.ss  <- scarp_profiles  [ ind_plots]
  scarp_distances.ss <- scarp_distances$dist_along_fault[ ind_plots ]
  scarp_distances.ss <- scarp_distances.ss - min ( scarp_distances.ss) 
  # Select segmented profiles 
  seg_profiles.ss    <- lapply ( seg_profiles[ind_plots] , function(x) x[[2]] )
  # Select fault planes list
  okp.ss             <- lapply ( okp_b       [ind_plots] , function(x) c ( which (x) , which (x) +1 ))
  # Select planes found 
  planes_found_i     <- mapply ( function (x) unique (x$i) , found_planes_b)
  n                  <- 0 
  found_planes.ss    <- vector ( 'list' , length ( ind_plots ))
  for (i in ind_plots){
    n <- n+1 
    if (i %in%  planes_found_i){
      wh                   <- which ( planes_found_i == i )
      found_planes.ss[[n]] <- found_planes_b[[wh]]
    }
  }
  # Select the columns found 
  n                  <- 0 
  offsets.ss         <- data.frame ( offset   = rep ( NA , length ( ind_plots )),
                                     confids  = NA,
                                     confid_t = NA)
  for (i in ind_plots){
    n <- n+1 
    if (i %in% height_offset_b$prof_ind){
      wh                      <- which ( height_offset_b$prof_ind == i )
      offsets.ss$offset   [n] <- height_offset_b$offset_CV[wh]
      offsets.ss$confids  [n] <- height_offset_b$confid_n[wh]
      offsets.ss$confid_t [n] <- height_offset_b$prof_ind[wh]
    }
  }
  
  
  
  
  
  ############# TRY FACET PLOT #################
  plane_ind <- list()
  prof      <- list()
  seg_p     <- list()
  okp.f     <- list()
  planes    <- list()
  confids   <- list()
  confid_t  <- list()
  offs      <- list()
  hc        <- list()
  fault_elev<- list()
  a         <- data.frame(one=NA,two=NA,prof_ind=NA)
  b         <- data.frame( x.l=c(NA,NA) , height.l=c(NA,NA) , x.r=c(NA,NA) , height.r=c(NA,NA) , i=c(NA,NA) , np_found=c(NA,NA) )
  for (i in 1:length(ind_plots)){
    plane_ind[[i]]  <- ind_plots[i]
    profa           <- scarp_profiles.ss  [[i]]
    profa$prof_ind  <- ind_plots[i]
    prof[[i]]       <- profa
    sps             <- seg_profiles.ss    [[i]]
    seg_p[[i]]      <- sps
    seg_p[[i]]$prof_ind <- ind_plots[i]
    a$one      <- okp.ss[[i]][1]
    a$two      <- okp.ss[[i]][2]
    a$prof_ind <- ind_plots[i]
    okp.f[[i]] <- a
    if (is.null(found_planes.ss[[i]])) {b <- data.frame( x.l=c(NA,NA) , height.l=c(NA,NA) , x.r=c(NA,NA) , height.r=c(NA,NA) , i=c(NA,NA) , np_found=c(NA,NA) )}
    if (!is.null(found_planes.ss[[i]])){b <- found_planes.ss[[i]]}
    b$prof_ind <- ind_plots[i]
    planes[[i]]     <- b
    confids[[i]]    <- offsets.ss$confids  [i]
    confid_t[[i]]   <- offsets.ss$confid_t [i]
    offs[[i]]       <- offsets.ss$offset   [i]
    hc[[i]]         <- max ( profa$height ) - min ( profa$height ) # To make width/height the same
    fault_elev[[i]] <- sps[okp.ss[[i]],]
    if ( nrow ( sps [ okp.ss[[i]],] ) == 0 ){ fault_elev[[i]] <- data.frame( x = c(NA,NA) , .fitted = c(NA,NA) )}
    fault_elev[[i]]$prof_ind <- ind_plots[i]
  }
  plane_ind <- do.call(rbind, plane_ind)
  prof      <- do.call(rbind, prof)
  seg_p     <- do.call(rbind, seg_p)
  okp.f     <- do.call(rbind,okp.f)
  planes    <- do.call(rbind, planes)
  confids   <- do.call(rbind, confids)
  confid_t  <- do.call(rbind,confid_t)
  offs      <- do.call (rbind, offs)
  hc        <- do.call(rbind, hc)
  fault_elev <- do.call(rbind, fault_elev)
  
  okp.f$confids  <- confids 
  okp.f$confid_t <- confid_t
  okp.f$offs     <- offs
  okp.f$hc       <- hc
  
  prof_ind_label <- list()
  for (la in 1:nrow(plane_ind)){
    prof_ind_label[[la]] <- paste0('Prof = ', plane_ind[la],
                                   ', Conf = ', round(confids[la]*10)/10,
                                   ', D (km) = ', round(scarp_distances.ss[la]/100)/10,
                                   ', O (m) = ',round(offs[la]*10)/10)
  }
  prof_ind_label <- unlist(prof_ind_label)
  names(prof_ind_label) <- as.character(plane_ind)
  
  ####################################3
  plot_profiles <- ggplot()+
    geom_line ( data = prof , aes ( x = DistTotal , y = height )) +
    geom_line ( data = seg_p, aes ( x = x , y = .fitted ) , color = 'blue' ) +
    geom_line ( data = fault_elev , aes ( x = x , y = .fitted ) , color = 'green' ) +
    geom_line ( data = planes , aes ( x = x.l , y= height.l ),color='red') + 
    geom_line ( data = planes , aes ( x = x.r , y= height.r ),color='red') +
    facet_wrap(~ prof_ind, ncol = 2, scales = "free_y",labeller = labeller(prof_ind = prof_ind_label) ) +
    ylab('Elevation, m') + 
    xlab('Distance along Profile, m') +
    theme_linedraw() +
    theme(
      strip.text        = element_text(size=8 , margin = margin(.0025, 0, .0025, 0, "cm") ),
      panel.grid      = element_blank(),
      axis.title.x    = element_text(size = 8),
      axis.title.y    = element_text(size = 8),
      axis.text.x     = element_text(size = 7),
      axis.text.y     = element_text(size = 7)
    )+
    scale_x_continuous(limits = c(0,profile_half_width*2),expand=expand_scale(0,0))
  
  if (fault_name=='Parvie1'){
    f_nameM   <- paste0 ( 'Final_Figures_SI/' , fault_name , '_profiles.pdf' )
  }
  if (fault_name=='Rojnoret'){
    f_nameM   <- paste0 ( 'Final_Figures_SI/' , fault_name , '_profiles.pdf' )
  }
  if (fault_name!='Parvie1'){
    f_nameM   <- paste0 ( 'Figures/' , fault_name , '_profiles.pdf' )
  }
  ggsave(f_nameM,  plot_profiles, width = 190, units = 'mm',dpi=600)
  plot_crop ( f_nameM ) #to crop the white space around the figure
  
  
}





