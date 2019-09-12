#MAIN_4c_smoothedBins, Loess, after interpolation

plot_list <- list()

nnn <- 0 
for (i in i_choices){ 
  nnn <- nnn + 1
  fault_name        <- fault_name_list[i]
  cc_colm <- cc_col_valsm[[i]]
  bw_colm <- bw_col_valsm[[i]]
  cc_cold <- cc_col_valsd[[i]]
  bw_cold <- bw_col_valsd[[i]]
  print(paste0('MAIN_4c_LOESS ',fault_name))
  
  # SOIL distances for distance plot
  f_dist.df <-readRDS(paste0(main_results_dir,'/Soil_fault_dfs/',   fault_name,'_f_dist.df.RDS')) 
  f_dist.df$width <- f_dist.df$xmax - f_dist.df$xmin
  f_dist.df$Quaternary_english_simp[is.na(f_dist.df$Quaternary_english_simp)] <- 'Till'
  if (nrow(f_dist.df)>1){# If adjacent rectangles have same Quaternary, make one bigger rectangle
    n <- 1
    for (j in 2:nrow(f_dist.df)){
      n <- n + 1
      if (f_dist.df$Quaternary_english_simp[n-1] == f_dist.df$Quaternary_english_simp[n]){
        f_dist.df$xmax[n-1]=f_dist.df$xmax[n]
        f_dist.df <- f_dist.df[-n,]
        n  <- n  - 1
      }
    }
    if (nrow(f_dist.df)>1){
      n <- 1
      for (j in 2:nrow(f_dist.df)){
        n <- n + 1
        if (f_dist.df$xmax[n-1]-f_dist.df$xmin[n-1] < 10){   # Remove areas that are less than 10 m across
          f_dist.df$xmin[n] <- f_dist.df$xmin[n-1]
          f_dist.df <- f_dist.df[-(n-1),]
          n  <- n  - 1
        }
      }
    }
  }
  # Make factor to be in correct order
  f_dist.df$Quaternary_english_simp <- factor(f_dist.df$Quaternary_english_simp, 
                                              levels = c('Bedrock','Till','Peat','Sand','Water'))
  
 
  saveRDS(f_dist.df,paste0(main_results_dir,'/Soil_fault_dfs/',   fault_name,'_f_dist.df.RDS')) 
  
  # Data after subsetted by confidence values 
  height_pl_bF_s_list     <- readRDS ( paste0 ( main_results_dir , '/' , off_pla_dir , '/' , fault_name , '_offsets_planes.RDS') )
  height_offset           <- height_pl_bF_s_list[[8]]
  height_offset.offset_CV           <- subset(height_offset, !is.na(offset_CV) & offset_CV!=0)
  height_offset.offset_CV$f2        <- height_offset.offset_CV$f
  height_offset.offset_CV$offset_f2 <- height_offset.offset_CV$offset_CV 
  height_offset.offset_CV$confid_f2 <- height_offset.offset_CV$confid_n
  
  height_offset.offset_CV_strands           <- subset(height_offset, !is.na(offset_CV_strands) & offset_CV_strands!=0)
  height_offset.offset_CV_strands$f2        <- 1 #all on same branch 
  height_offset.offset_CV_strands$offset_f2 <- height_offset.offset_CV_strands$offset_CV_strands #all on same branch 
  height_offset.offset_CV_strands$confid_f2 <- height_offset.offset_CV_strands$confid_n_strands #all on same branch 
  height_offset.offset_CV_strands  <- arrange(height_offset.offset_CV_strands, dist_along_fault_found)
  l_fault <- ( max(height_offset$d_km) - min(height_offset$d_km) )* 1000
  
  # Interpolate - but not if gap is more than 1% of the fault
  
  ## PLOT STANDARD DEVIATION OVER 1km, rather than max and minimum.....Moving mean of 1km
  dist   <- height_offset.offset_CV_strands$dist_along_fault_found
  offset <- height_offset.offset_CV_strands$offset_f2
  confid <- height_offset.offset_CV_strands$confid_f2
  # Find gaps
  rm <- which(diff(dist) > l_fault/100) 
  new_dists   <- list()
  new_offsets <- list()
  new_confids <- list()
  new_dists[[1]]   <- seq(min(dist), dist[rm[1]],2)
  new_offsets[[1]] <- approx (x=dist, y=offset, xout=new_dists[[1]])[[2]]
  new_confids[[1]] <- approx (x=dist, y=confid, xout=new_dists[[1]])[[2]]
  new_dists[[1]]   <- c(new_dists[[1]]  , dist[rm[1]] + 2)
  new_offsets[[1]] <- c(new_offsets[[1]] , NA)
  new_confids[[1]] <- c(new_confids[[1]] , NA)
  for (i in 2:length(rm)){
    #  if (rm[i-1]-rm[i] > 1)
    new_dists[[i]]   <- c(seq(dist[rm[i-1]+1], dist[rm[i]],2))
    new_offsets[[i]] <- approx (x=dist, y=offset, xout=new_dists[[i]])[[2]]
    new_confids[[i]] <- approx (x=dist, y=confid, xout=new_dists[[i]])[[2]]
    new_dists[[i]]   <- c(new_dists[[i]]  , dist[rm[i]] + 2)
    new_offsets[[i]] <- c(new_offsets[[i]] , NA)
    new_confids[[i]] <- c(new_confids[[i]] , NA)
  }
  new_dists[[i+1]]   <- c(seq(dist[rm[i]+1], max(dist),2))
  new_offsets[[i+1]] <- approx (x=dist, y=offset, xout=new_dists[[i+1]])[[2]]
  new_confids[[i+1]] <- approx (x=dist, y=confid, xout=new_dists[[i+1]])[[2]]
  
  new_dists_v   <- unlist(new_dists)
  new_offsets_v <- unlist(new_offsets)
  new_confids_v <- unlist(new_confids)
  height_offset <- data.frame(new_offsets_v=new_offsets_v,new_dists_v=new_dists_v,new_confids_v=new_confids_v)
  
  # Look over 250 m
  span1      <- 250 / max ( height_offset$new_dists_v ) # Should be about 0.1 for short faults and lower for larger faults 
  span2      <- 2.5 * span1
  loessModA  <- loess(new_offsets_v ~ new_dists_v, data = height_offset , weights=new_confids_v, span=span1) # short smoothing span
  loessModB  <- loess(new_offsets_v ~ new_dists_v, data = height_offset , weights=new_confids_v, span=span2) # longer smoothing span
  loessMod10 <- loess(new_offsets_v ~ new_dists_v, data = height_offset , weights=new_confids_v, span=0.1) # 10% smoothing span
  loessMod25 <- loess(new_offsets_v ~ new_dists_v, data = height_offset , weights=new_confids_v, span=0.25) # 25% smoothing span
  loessMod50 <- loess(new_offsets_v ~ new_dists_v, data = height_offset , weights=new_confids_v, span=0.50) # 50% smoothing span
  
  lA  <- loess.sd(new_offsets_v[!is.na(new_offsets_v)] ~ new_dists_v[!is.na(new_offsets_v)],nsigma = 1,weights=new_confids_v[!is.na(new_offsets_v)],span=span1)
  l10 <- loess.sd(new_offsets_v[!is.na(new_offsets_v)] ~ new_dists_v[!is.na(new_offsets_v)],nsigma = 1,weights=new_confids_v[!is.na(new_offsets_v)],span=0.1)
  l25 <- loess.sd(new_offsets_v[!is.na(new_offsets_v)] ~ new_dists_v[!is.na(new_offsets_v)],nsigma = 1,weights=new_confids_v[!is.na(new_offsets_v)],span=0.25)
  l50 <- loess.sd(new_offsets_v[!is.na(new_offsets_v)] ~ new_dists_v[!is.na(new_offsets_v)],nsigma = 1,weights=new_confids_v[!is.na(new_offsets_v)],span=0.5)
  
  # get smoothed output
  height_offset$smA [!is.na(height_offset$new_offsets_v)] <- predict(loessModA ) 
  height_offset$smB [!is.na(height_offset$new_offsets_v)] <- predict(loessModB ) 
  height_offset$sm10[!is.na(height_offset$new_offsets_v)] <- predict(loessMod10) 
  height_offset$sm25[!is.na(height_offset$new_offsets_v)] <- predict(loessMod25) 
  height_offset$sm50[!is.na(height_offset$new_offsets_v)] <- predict(loessMod50) 
  
  
  height_offset$d_km <- height_offset$new_dists_v/1000
  ho_orig <- data.frame(d_km = dist/1000,offset=offset,confid=confid) 
  
  l10 <- lA # to get same as red line
  add2 <- which(is.na(new_offsets_v))
  add2 <- add2 - seq(1,length(add2), 1) # Because new_offsets_v has NA values added 
  ym <- list() 
  yl <- list() 
  yu <- list() 
  ym[[1]] <- c(l10$y    [1:add2[1]],NA)
  yu[[1]] <- c(l10$upper[1:add2[1]],NA)
  yl[[1]] <- c(l10$lower[1:add2[1]],NA)
  for (i in 2:length(add2)){
    ym[[i]] <- c(l10$y    [(add2[i-1]+1):add2[i]],NA)
    yu[[i]] <- c(l10$upper[(add2[i-1]+1):add2[i]],NA)
    yl[[i]] <- c(l10$lower[(add2[i-1]+1):add2[i]],NA)
  }
  ym[[i+1]] <- l10$y    [(add2[i]+1):length(l10$y)]
  yu[[i+1]] <- l10$upper[(add2[i]+1):length(l10$y)]
  yl[[i+1]] <- l10$lower[(add2[i]+1):length(l10$y)]
  ym_v  <- unlist(ym)
  yu_v  <- unlist(yu)
  yl_v  <- unlist(yl)
  sd_df <- data.frame(d_km = new_dists_v/1000, ym=ym_v,yu=yu_v,yl=yl_v)
  av_sd <- mean(yu_v-yl_v, na.rm = TRUE)
  x_pos <- max(f_dist.df$xmax)/1000 - 0.025*max(f_dist.df$xmax)/1000
  y_pos <- ceiling( max(sd_df$yu,na.rm=TRUE)) - 0.1*ceiling( max(sd_df$yu,na.rm=TRUE))
  y_min <- y_pos - av_sd
  y_max <- y_pos + av_sd
  sd.df <- data.frame(name=fault_name,d_km = x_pos, ymin=y_min, ymax=y_max)
  
  
  p_loess <-  ggplot() +   #Span of 0.1 and 0.5 
    geom_errorbar( data = sd.df,          aes ( x = d_km , ymin = ymin , ymax = ymax ) , width=0.075*sd.df$d_km/5 , size = 0.6 , color='red' ) + 
    geom_rect    ( data = f_dist.df ,     aes ( xmin = xmin/1000 , xmax = xmax/1000 , ymin = ymin , ymax = ymax , group = rect.id, fill = Quaternary_english_simp), alpha=0.25)+
    geom_point   ( data = ho_orig ,       aes ( x = d_km , y = offset,color=confid )   , size = 0.7 ) +
    geom_path    ( data = height_offset , aes ( x = d_km , y = smA )  ,color = 'red'   , size = 0.6 ) +
    geom_path    ( data = height_offset , aes ( x = d_km , y = sm50 ) ,color = 'black' , size = 0.6 ) +
    scale_fill_manual(values = cc_cold,name='Quat.') +
    scale_colour_viridis_c (name = 'Conf.', guide = 'legend') +
    xlab ( 'Distance along Fault, km' ) + 
    ylab ( 'Offset, m' ) +
    theme_linedraw() +
    theme(
      axis.text         =  element_text(size = 8),
      axis.title        =  element_text(size = 10),
      legend.position   = c(0.5,-0.3),legend.box = "horizontal",legend.direction= "horizontal",
      legend.title      = element_text(size = 8,face="bold"), 
      legend.text       = element_text(size = 8),
      legend.key.height = unit(0.25,"cm"),
      legend.key.width  = unit(0.5,"cm"),
      panel.grid        = element_blank()
    ) +
    guides(colour = guide_legend(override.aes = list(size=2))) + 
    guides(fill = guide_legend(override.aes=list(shape=21))) +
    coord_equal(ratio=max(f_dist.df$xmax)/max(sd_df$yu,na.rm=TRUE)*0.25/1000 , expand=FALSE , 
                xlim =c(0,max(f_dist.df$xmax)/1000),
                ylim = c(0,ceiling( max(sd_df$yu,na.rm=TRUE)) ) )
  if (file.exists ( paste0 ( main_data_dir , '/' , rivers_dir , '_R/' ,fault_name , '_rivers.RDS' ))) {
    scarp_rivers <- readRDS(paste0 ( main_data_dir , '/' , rivers_dir , '_R/' ,fault_name , '_rivers.RDS' ))
    p_loess <-   p_loess +
      geom_line ( data = scarp_rivers , aes ( x = d_km , y = height , group = f ) , color = 'blue' , size = 2 )
  }
    
 # plot(p_loess)
  # f_name <-  paste0('Final_Figures/',fault_name,'_dist.pdf')
  # ggsave(f_name,width = 190, units = 'mm',dpi=600)
  # plot_crop(f_name) #to crop the white space around the figure
  
  plot_list[[nnn]] <-  p_loess
  
}

saveRDS(plot_list, 'Results/p_loess_plots.RDS')
