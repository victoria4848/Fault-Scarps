#MAIN_4b_SMOOTH_OFFSETS_BINS

bw_list <- c(seq(2,200,2), seq(210,500,10), seq(600,1000,50),1500,2000)
bw_list <- c(seq(2,100,2), seq(110,500,10), seq(650,1000,50),seq(1100,5000,100),seq(6000,20000,1000)) # from Main_4b_SMOOTH_OFFSETS_BINS


for (i in i_choices){ 
  fault_name        <- fault_name_list[i]
  
  print(paste0('MAIN_4b_SMOOTH_OFFSETS_bins ',fault_name))
  
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
  l_fault <- max(height_offset$d_km) - min(height_offset$d_km)
  # AVERAGE OFFESTS ALONG INDIVIDUAL STRANDS for MAPVIEW
  
  offset_bins <- function(bw,df){
    
    h_mean <- data.frame(d_km= NA,lat=NA,lon=NA,offset=NA,CV=NA,np_av=NA,offset_w=NA,confid_w=NA, f=NA, bw=NA,
                         max_off=NA, min_off=NA)
    nu     <- 0

    for (j in unique(df$f2)){
      height_offset.offset_CV.f2 <- subset(df, f2 == j)
      
      d <- seq(min(height_offset.offset_CV.f2$dist_along_fault_found, na.rm = TRUE),
               max(height_offset.offset_CV.f2$dist_along_fault_found, na.rm = TRUE),bw)
      
      
      for (i in 1:(length(d)-1)){
        nu  <- nu  + 1
        ho.ss <- subset(height_offset.offset_CV.f2, dist_along_fault>=d[i] & dist_along_fault <d[i+1])
        offset_w   <- mean ( ho.ss$offset_f2 * ho.ss$confid_f2 / mean(ho.ss$confid_f2) ) # weight by confidence
        confid_w   <- mean ( ho.ss$confid_f2 * ho.ss$confid_f2 / mean(ho.ss$confid_f2) )  # weight by confidence
        h_mean[nu,] <- c(mean(ho.ss$d_km),
                         mean(ho.ss$lat),
                         mean(ho.ss$lon),
                         mean(ho.ss$offset_f2),
                         mean(ho.ss$confid_f2),
                         min(nrow(ho.ss),bw/2),  # Number of points that were averaged 
                         offset_w,
                         confid_w,
                         f=j,
                         bw = bw,
                         max_off <- max(ho.ss$offset_f2, na.rm = TRUE), 
                         min_off <- min(ho.ss$offset_f2, na.rm = TRUE))
      }
    }
    
    nr     <- sort(h_mean$np_av[h_mean$np_av!=0])[round(nrow(h_mean)/10)] # Remove 10% surrounded by fewest planes
    h_mean$offset_w[h_mean$np_av < nr] <- NA
    h_mean$confid_w[h_mean$np_av < nr] <- NA
    h_mean$min_off[h_mean$np_av < nr] <- NA
    h_mean$max_off[h_mean$np_av < nr] <- NA
    h_mean$np_av[h_mean$np_av < nr] <- NA
    return (h_mean)
  }
  
  
  
  p_map_offset <- function(dfm,dotsize,min_o,max_o,dfm2) {
    dfm <- subset(dfm, !is.na(offset_w))      # WHY OFFSET_W AND NOT OFFSET_F@?
    ggplot() +
      geom_point( data = dfm, 
                  aes ( x = lon/1000 , y = lat/1000 , color = offset_w ) , size = dotsize) +
      scale_color_distiller(palette = "Spectral",direction = -1,limits=c(min_o, max_o)) +
      labs(color = "Offset,\nm") +
      ggtitle(paste0(unique(dfm$bw))) + 
      theme_linedraw()         +
      theme(
        panel.grid   = element_blank(),
        title        = element_text(size = 7,vjust=-0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text    =  element_text(size = 7),
        legend.title = element_text(size = 7), 
        legend.text  = element_text(size = 7),
        legend.key.height = unit(0.6,"cm"),
        legend.key.width = unit(0.3,"cm")
      ) +
      coord_equal(xlim = c(min(dfm2$lon/1000,na.rm=TRUE), max(dfm2$lon/1000,na.rm=TRUE)), ylim = c(min(dfm2$lat/1000,na.rm=TRUE), max(dfm2$lat/1000,na.rm=TRUE)) )
  }
  
  p_dis_offset <- function(dfm,dotsize,dfm2){
    max_offset   <- max (dfm2$offset_w, na.rm=TRUE)
    max_distance <- max (dfm2$d_km,     na.rm=TRUE)
    
    ggplot()+
      geom_point( data = dfm , aes ( x = d_km , y = offset_w , color = confid_w ) , size = dotsize ) +
      # geom_line ( data=subset ( height_offset.ss , point.density > min_pdensity ) , aes ( x = d_km , y = smA , group = gr ) , color = 'red' ) +
      xlab ( 'Distance along the fault, km' ) + 
      ylab ( 'Offset, m' ) +
      theme_linedraw() +
      theme(
        axis.text         =  element_text(size = 7),
        axis.title        =  element_text(size = 8),
        # legend.position   = 'bottom',
        # legend.title      = element_text(size = 7), 
        # legend.text       = element_text(size = 7),
        # legend.key.height = unit(0.3,"cm"),
        # legend.key.width  = unit(0.6,"cm"),
        legend.position   = 'none',
        panel.grid.minor  = element_blank()
      ) +
      guides(colour = guide_legend(override.aes = list(size=2))) + 
      scale_colour_viridis_c (name = 'Conf. Values', guide = 'legend',limits=c(0,1)) +
      coord_cartesian(ylim = c(0,ceiling(max_offset)), xlim = c(0,ceiling(max_distance)), expand=FALSE )
    # coord_equal(ratio=max_distance/max_offset*0.5 ) 
    
    
    # if (file.exists ( paste0 ( main_data_dir , '/' , rivers_dir , '_R/' ,fault_name , '_rivers.RDS' ))) {
    #   scarp_rivers <- readRDS(paste0 ( main_data_dir , '/' , rivers_dir , '_R/' ,fault_name , '_rivers.RDS' ))
    #   p_length_offset <- p_length_offset + 
    #     geom_line ( data = scarp_rivers , aes ( x = d_km , y = height , group = f ) , color = 'blue' , size = 1 )
    # }
  }
  
  
  
  
  # bw_list <- c(10,20,50,100,500,1000,5000)
  hm_list <- vector('list',length(bw_list))
  pm_list <- c()
  hd_list <- vector('list',length(bw_list))
  pd_list <- c()
  dfm <- height_offset.offset_CV # or height_offset.offset_CV_strands
  dfd <- height_offset.offset_CV_strands 
  nu <- 0
  for (bw in bw_list){
    nu <- nu + 1
    hm_list[[nu]] <- offset_bins(bw,dfm)
    min_o <- min(hm_list[[1]]$offset_w, na.rm=TRUE)
    max_o <- max(hm_list[[1]]$offset_w, na.rm=TRUE)
    dotsize       <- 8 / ( nrow(hm_list[[nu]])  ^ 0.4 )
    if (nrow(hm_list[[nu]]) < 5 ){
      pm_list[[nu]] <- p_map_offset(hm_list[[nu]], dotsize,min_o,max_o,hm_list[[1]]) # last df to get same map extent
    }
    if (nrow(hm_list[[nu]]) >= 5 ){
      pm_list[[nu]] <- p_map_offset(hm_list[[nu]], dotsize,min_o,max_o,hm_list[[1]])
    }
    
    hd_list[[nu]] <- offset_bins(bw,dfd)
    dotsize       <- 10 / ( nrow(hd_list[[nu]])  ^ 0.4 )
    if (nrow(hd_list[[nu]]) > 5 ){
      pd_list[[nu]] <- p_dis_offset(hd_list[[nu]], dotsize,hd_list[[1]])
    }
  }
  
  
  ### Make overall plot of lines of average 
  max_offset <- max(hd_list[[1]]$offset, na.rm=TRUE)
  max_distan <- max(hd_list[[1]]$d_km,   na.rm=TRUE)
  color_list <- c('red','orange','yellow','green','blue','black','purple')
  pl <- ggplot()
  for (i in 1:length(pd_list)){
    col <- color_list[i]
    pl <- pl + geom_path( data = hd_list[[i]] , aes ( x = d_km , y = offset_w ) ,color = color_list[i]  , size = 1 )
    if (nrow(hd_list[[i]]) < 10 & nrow(hd_list[[i]]) > 2 ){
      pl <- pl + geom_point( data = hd_list[[i]] , aes ( x = d_km , y = offset_w ) ,color = color_list[i]  , size = 2 )
    }
  }
  pl <- pl + 
    xlab ( 'Distance along the fault, km' ) + 
    ylab ( 'Offset, m' ) +
    theme_linedraw() +
    theme(
      axis.text         =  element_text(size = 7),
      axis.title        =  element_text(size = 8),
      legend.position   = 'bottom',
      legend.title      = element_text(size = 7), 
      legend.text       = element_text(size = 7),
      legend.key.height = unit(0.3,"cm"),
      legend.key.width  = unit(0.6,"cm"),
      panel.grid.minor  = element_blank()
    ) +
    guides(colour = guide_legend(override.aes = list(size=2))) + 
    ylim(c(0,ceiling(max_offset))) + 
    xlim(c(0,ceiling(max_distan))) +
    coord_equal(ratio=max_distan/max_offset*0.5 , expand=FALSE ) 
  
  ## 10 m and 1 km with max and min 
  pl <- ggplot()
  for (i in 1:length(pd_list)){
    col <- color_list[i]
    pl <- pl + geom_path( data = hd_list[[i]] , aes ( x = d_km , y = offset_w ) ,color = color_list[i]  , size = 1 )
    if (nrow(hd_list[[i]]) < 10 & nrow(hd_list[[i]]) > 2 ){
      pl <- pl + geom_point( data = hd_list[[i]] , aes ( x = d_km , y = offset_w ) ,color = color_list[i]  , size = 2 )
    }
  }
  
  
  # Nice distance offset for 10m and also larger binning
  if (l_fault <= 5){fl_num <- 4}
  if (l_fault >5 & l_fault <= 10){fl_num <- 5}
  if (l_fault > 10){fl_num <- 6}
  ribbon <- hd_list[[fl_num]]
  naid <- which(is.na(ribbon$d_km))
  for (i in naid){
    ribbon$d_km[naid] <- ribbon$d_km[naid-1] + 0.005 # To make sure it's in bewteen 
  }
  pd2 <-  ggplot() +   #10m and 1000m 
    geom_ribbon(data = ribbon, aes ( x = d_km,  ymin = min_off, ymax = max_off),
                fill = 'grey', alpha = 0.75,na.rm = FALSE)  + 
    # geom_path(data = hd_list[[6]],   aes ( x = d_km,  y = min_off),color='green', alpha = 0.75)  + 
    # geom_path(data = hd_list[[6]],   aes ( x = d_km,  y = max_off),color='green', alpha = 0.75)  + 
    geom_path( data = hd_list[[1]] , aes ( x = d_km , y = offset_w ) ,color = 'red'  , size = 1 ) +
    geom_path( data = hd_list[[fl_num]] , aes ( x = d_km , y = offset_w ) ,color = 'black'  , size = 1 ) +
    xlab ( 'Distance along the fault, km' ) + 
    ylab ( 'Offset, m' ) +
    theme_linedraw() +
    theme(
      axis.text         =  element_text(size = 7),
      axis.title        =  element_text(size = 8),
      legend.position   = 'bottom',
      legend.title      = element_text(size = 7), 
      legend.text       = element_text(size = 7),
      legend.key.height = unit(0.3,"cm"),
      legend.key.width  = unit(0.6,"cm"),
      panel.grid.minor  = element_blank()
    ) +
    guides(colour = guide_legend(override.aes = list(size=2))) + 
    ylim(c(0,ceiling(max(ribbon$max_off)))) + 
    xlim(c(0,ceiling(max_distan))) +
    coord_equal(ratio=max_distan/max_offset*0.5 , expand=FALSE ) 
  
  
  
  pdf(file = paste0(figures_dir,'/',fault_name,'_mean_map_dist.pdf'))
  do.call(grid.arrange, c(pm_list,ncol=2))
  do.call(grid.arrange, c(pd_list,ncol=1))
  plot(pl)
  plot(pd2)
  dev.off()
  
  saveRDS(list(c('details','CV 10 20 50 100 500 1000 5000','CV_strands'),hm_list, hd_list), paste0(main_results_dir,'/Binned_Offsets_RDS/',fault_name,'_bins_CV_CVS.RDS'))
  
  # AVERAGE OFFSETS ALONG COMBINED PROFILES FOR DISTANCE PROFILES 
  
  
  
}