## Make segmented profiles and store results 


f1_segmented_profiles <- function(profiles,
                                  profile_half_width,
                                  profile_point_spacing,
                                  n_pp_prof){
  

  # Very occasionally profiles have NA values. If so - make NA values average of surrounding 2
  if (sum(is.na(profiles$height))!=0){
    na_ind <- which(is.na(profiles$height))
    profiles$height[na_ind] <- mean(c(profiles$height[na_ind-1], profiles$height[na_ind+1]),na.rm =TRUE)
  }
  
  
  # Check profile height change is large enough to contain a fault 
  profile_height_change <- max(profiles$height,na.rm=TRUE) - min(profiles$height,na.rm=TRUE)
  if (profile_height_change < 0.02){ # Don't try and find segmented profile 
    tab=NA
    o.df=NA
    tab_ids=NA
    return(list(tab,o.df,tab_ids))
  } 
  
  if (profile_height_change>=0.02){  # ignore profiles with tiny height change i.e. across a lake
    
    
    ## Make segmented profiles
    y      <- profiles$height; x <- profiles$DistTotal
    out.lm <- lm ( y~x )
    o      <- list ()
    nom    <- 50;
    # Keep changing nom (number of itererations), until a fitted profile can be found
    while (class(o)[1]=="list" | class(o)[1]=='lm'){
      tryCatch(o <- segmented(out.lm,psi=NA,control=seg.control(stop.if.error=FALSE,it.max = nom,K=10,n.boot=0))
               ,error = function(e) e)
      nom <- nom-5
    }
    
    plane_ind   <- c ( 1 , (round(o$psi[,2]/2)*2 / profile_point_spacing)+1,n_pp_prof)
    breakpoints <- c ( 0 , o$psi[,2] , profile_half_width*2)
    
    # # Make table of useful parameters to save 
    o.df             <- fortify(o)
    o.df             <- subset(o.df,select=c(x,.fitted))
    o.df             <- o.df[plane_ind,]
    
    nplanes          <- length(plane_ind)-1
    plane_id         <- seq(1,nplanes,1)
    
    # Widths
    widths           <- round(diff(breakpoints)*1000)/1000
    distance_along   <- round(breakpoints)
    start_dist_along <- distance_along[1:nplanes]
    end_dist_along   <- distance_along[2:(nplanes+1)]
    
    # Heights
    heights          <- o.df$.fitted
    if (sum(is.na(heights))>=1) {
      heights[nplanes+1] <- o$fitted.values[length(o$fitted.values)]
    }
    if (sum(is.na(heights))==1) { 
      heights[nplanes]   <- o$fitted.values[distance_along[length(distance_along)-1]/2-2]
    }
    start_height     <- heights [1:nplanes]
    end_height       <- heights [2:(nplanes+1)]
    
    # Gradients and curvature 
    height_dif       <- round ( diff ( heights )  *1000)/1000
    gradients        <- round ( height_dif/widths *1000)/1000
    curvature_between_planes <- round( diff ( gradients ) *1000)/1000
    right_curvature <- c ( curvature_between_planes , NA )
    left_curvature  <- c ( NA , curvature_between_planes )
    
    # Make dataframe about planes
    tab <- data.frame(plane_id, gradients,right_curvature,left_curvature,height_dif,
                      start_dist_along,end_dist_along,
                      start_height, end_height,
                      start_ind= plane_ind[1:nplanes], end_ind= plane_ind[2:(nplanes+1)],
                      row.names = NULL)
    
    tab_ids <- vector("list",nrow(tab))
    for (b in 1:nrow(tab)){
      tab_ids[[b]] <- seq(tab[b,]$start_ind,tab[b,]$end_ind,1) 
    }
    
  } # If profile height change > 0.02m
  
  
  return(list(tab,o.df,tab_ids))
  
}