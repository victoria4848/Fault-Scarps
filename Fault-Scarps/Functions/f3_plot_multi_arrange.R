## Multiplots 

## Multi plots of many profiles ##
# SELECT PROFILES to plot 

plot_multi_arrange <- function ( df , num_plots , scarp_profiles , seg_profiles , okp, found_planes, arrange_name){
  
  ## Find indices of scarp profiles to plot - arrange by arrange_name, then pick equally spaced
  if (arrange_name =='prof_ind'){
    n                    <- length ( scarp_profiles)
    ind_plots            <- seq    ( 1 , n , floor ( n / ( num_plots - 1 ))) 
    ind_plots[num_plots] <- n  # To make sure we get the end one
    plot_name            <- paste0 ( 'multi_',deparse(substitute(scarp_profiles)),'_',arrange_name) 
  }
  if (arrange_name !='prof_ind'){
    df.nona              <- subset(df, !is.na(offset))
    offsets_ar           <- arrange ( df.nona , get ( arrange_name ))
    n                    <- nrow    ( offsets_ar )
    ind_plots1           <- seq     ( 1 , n , floor ( n / ( num_plots - 1 ))) 
    ind_plots            <- offsets_ar$prof_ind [ ind_plots1 ]
    ind_plots[num_plots] <- offsets_ar$prof_ind [ n ]          # To make sure we get the end one
    plot_name            <- paste0  ( 'multi_',deparse(substitute(df)),'_',arrange_name) 
  }
  
  ## Select by ind_plots found 
  # Select scarp profiles
  scarp_profiles.ss  <- scarp_profiles  [ ind_plots]
  scarp_distances.ss <- scarp_distances [ ind_plots , 'dist_along_fault' ]
  scarp_distances.ss <- scarp_distances.ss - min ( scarp_distances.ss) 
  # Select segmented profiles 
  seg_profiles.ss    <- lapply ( seg_profiles[ind_plots] , function(x) x[[2]] )
  # Select fault planes list
  okp.ss             <- lapply ( okp         [ind_plots] , function(x) c ( which (x) , which (x) +1 ))
  # Select planes found 
  planes_found_i     <- mapply ( function (x) unique (x$i) , found_planes)
  n                  <- 0 
  found_planes.ss    <- vector ( 'list' , length ( ind_plots ))
  for (i in ind_plots){
    n <- n+1 
    if (i %in%  planes_found_i){
      wh                   <- which ( planes_found_i == i )
      found_planes.ss[[n]] <- found_planes [[wh]]
    }
  }
  # Select the columns found 
  n                  <- 0 
  offsets.ss         <- data.frame ( offset   = rep ( NA , length ( ind_plots )),
                                     confids  = NA,
                                     confid_t = NA)
  for (i in ind_plots){
    n <- n+1 
    if (i %in% df$prof_ind){
      wh                      <- which ( df$prof_ind == i )
      offsets.ss$offset   [n] <- df$offset[wh]
      offsets.ss$confids  [n] <- df$confid_n[wh]
      offsets.ss$confid_t [n] <- df[,arrange_name][wh]
    }
  }
  
  # # Select planes found 
  # planes_found_i     <- mapply ( function(x) unique(x$i) , found_planes)
  # found_planes.ss    <- vector ( 'list',length(ind_plots))
  #  wh                 <- which  ( ind_plots      %in% planes_found_i )
  # # wh2                <- which  ( planes_found_i %in% ind_plots ) [order(ind_plots)]
  #  wh2                <- which  ( planes_found_i %in% ind_plots ) [order(ind_plots)]
  #  found_planes.ss[wh]<- found_planes [ wh2  ]
  #  # Select the columns found 
  #  offsets.ss               <- data.frame ( offset   = rep ( NA , length ( ind_plots )) ,
  #                                           confids  = NA , 
  #                                           confid_t = NA )
  #  offsets.ss$offset   [wh] <- df$offset[wh2]
  #  offsets.ss$confids  [wh] <- df$confid[wh2]
  #  offsets.ss$confid_t [wh] <- df[,arrange_name][wh2]
  
  
  
  ## MULTIPLOTS of PROFILES 12 with fitted planes ##
  
  p <- list()   # List of plots 
  for (i in 1:length(ind_plots)){
    plane_ind <- ind_plots[i]
    prof      <- scarp_profiles.ss  [[i]]
    seg_p     <- seg_profiles.ss    [[i]]
    okp.f     <- okp.ss             [[i]]
    planes    <- found_planes.ss    [[i]]
    confids   <- offsets.ss$confids  [i]
    confid_t  <- offsets.ss$confid_t [i]
    offs      <- offsets.ss$offset   [i]
    hc        <- max ( prof$height ) - min ( prof$height ) # To make width/height the same
    
    p[[i]] <-  ggplot()+
      # Original profile
      geom_line ( data = prof , aes ( x = DistTotal , y = height ))+
      # Fitted profile
      geom_line ( data = seg_p, aes ( x = x , y = .fitted ) , color = 'blue' )+
      # Fault
      geom_line ( data = seg_p [okp.f,] , aes ( x = x , y = .fitted ) , color = 'green' ) +
      ggtitle(
        paste0(plane_ind,
               ' D: ' , round ( scarp_distances.ss[i]/1000),'km',
               ' C: ' , round ( confids *10)/10,
               ' O: ' , round ( offs    *10)/10,
               ' ', arrange_name,': ', round(confid_t*100)/100)
      ) +
      theme_linedraw()+
      theme(
        panel.grid      = element_blank(),
        plot.title      = element_text(size = 7),
        axis.title.x    = element_blank(),
        axis.title.y    = element_blank(),
        axis.text.x     = element_text(size = 7),
        axis.text.y     = element_text(size = 7)
      ) #+ 
    #  coord_equal(ratio = 80/hc, expand=FALSE) 
    # Planes found 
    if (!is.null(planes)){
      p[[i]] <- p[[i]] + 
        geom_line(data=planes,aes(x=x.l,y=height.l),color='red') + 
        geom_line(data=planes,aes(x=x.r,y=height.r),color='red')
    } 
  }
  
  nCol <- 2
  print(plot_name)
  assign(plot_name , do.call("grid.arrange", c(p, ncol=nCol)),envir = parent.frame())
  
  
  
}

