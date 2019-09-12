f1_create_profiles <- function(fault_name,profile_spacing,profile_point_spacing,profile_half_width){
  
  print(paste0('Creating profiles for ',fault_name))
  
  faults_dir_R   <- paste0('Data/Faults_R/')
  DEMs_dir_R     <- paste0('Data/DEMs_R/')
  profiles_dir_t <- paste0('Results/Profiles/')
  
  # Load fault and cropped DEM
  fault_list     <- readRDS(paste0(faults_dir_R, fault_name,'.RDS')) # created in fault_smoothing
  fault          <- fault_list[[2]] 
  fault.df       <- fault_list[[3]] # mini corner smoothing
  fault.df.sm    <- fault_list[[4]] # To assign distances to profiles 
  dem_crop       <- readRDS(paste0(DEMs_dir_R,  fault_name,'_dem_crop.RDS'))
  
  
  # Make points every 1m along smooth fault line to then later find distance to match profiles
  smooth_fault_line <- list               ( Lines ( Line ( coords=fault.df.sm ) , ID=1 ))
  fault.df.sm.sp    <- SpatialLines       ( smooth_fault_line , proj4string = crs(dem_crop))
  len               <- SpatialLinesLengths( fault.df.sm.sp)
  # fault.distances will be every 1m along the smooth line - find which point is closest 
  fault.distances   <- spsample(fault.df.sm.sp,round(len), type='regular')
  
  dem_crop.v        <- velox(dem_crop) # For faster raster manipulation


  
  prof_list_overall       <- list(list())
  n_points_total_segments <- c()
  sta_sto                 <- c()
  prof_dists              <- list()
  fval                    <- list()
  
  num_fault_segs <- length(unique(fault.df$id))
  for (f in 1:num_fault_segs){
    fault_sub <- subset(fault, id==f)
    
    # If remainder changes across a number, add 1 point to that segment
    dt.psp         <- as.psp      ( fault_sub )  # Create line segment pattern from fault
    di             <- lengths.psp ( dt.psp ) # Find length of fault segments
    an             <- angles.psp  ( dt.psp , directed = TRUE )  # Find angles of fault segments
    ex             <- di%%profile_spacing  # get remainder of distance and profile_spacing
    exc            <- cumsum      ( ex )  # find cummulative of remainder
    
    # If remainder changes across a number, add 1 point to that segment
    no_points_new  <- floor    ( di/profile_spacing) + c(1, diff(floor(exc/profile_spacing)))
    n_points_total <- sum      ( no_points_new) # find total number of points on fault
    
    pts            <- spsample (fault_sub,n_points_total,type='regular') # Create points along line
    n_points_total <- length   (pts)  # doesn't sample always the same number
    
    # Initialize pts_an and make sure it's the correct lenght
    pts_an        <- rep ( an , no_points_new)  # get angles for each points
    pts_an        <- c   ( pts_an , rep(pts_an[length(pts_an)],3))
    
    # Find points perpendicular to line at all the points 
    points <- data.frame(longn=rep(NA,n_points_total),latn=NA,longs=NA,lats=NA)
    for (i in 1:n_points_total) {
      points$longn[i] <- pts@coords[i,1] - sin(pts_an[i]) * profile_half_width 
      points$latn[i]  <- pts@coords[i,2] + cos(pts_an[i]) * profile_half_width 
      points$longs[i] <- pts@coords[i,1] + sin(pts_an[i]) * profile_half_width 
      points$lats[i]  <- pts@coords[i,2] - cos(pts_an[i]) * profile_half_width 
    }
    
    # Make fault go from low to high latitude 
    if (pts@coords[1,2] > pts@coords[n_points_total,2]){
      points <- arrange ( points, as.numeric ( rev ( rownames ( points ))) )
      seqinr::swap ( points$latn  , points$lats)
      seqinr::swap ( points$longn , points$longs)
      # And swith around north and south 
    }
    
    ### Make elevation profiles ###
    prof_list <- list()
    for (i in 1:n_points_total) {
      cds             <- cbind        ( c(points$longn[i],points$longs[i]),
                                        c(points$latn[i],points$lats[i]))
      transects       <- SpatialLines ( list(Lines(list(Line(cds)), ID =paste(i))),proj4string = crs(dem_crop))
      transects.pts   <- spsample     ( transects, n=(profile_half_width*2/profile_point_spacing)+1, type="regular")
      
      line1           <- data.frame   ( 'lon' = transects.pts@coords[,1],
                                        'lat'=transects.pts@coords[,2])
      line1$height    <- as.numeric   ( dem_crop.v$extract_points(sp=transects.pts))

      # If no velox... 
      # g <- as(dem_crop.v, 'SpatialGridDataFrame')
      # line1$height <-  as.numeric   ( over(transects.pts,g)$layer )
      
      line1$DistTotal <- seq          ( 0,profile_half_width*2,profile_point_spacing)
      prof_list[[i]]  <- line1
    }
    
    prof_list_overall[[f]]     <- prof_list
    n_points_total_segments[f] <- n_points_total
    
    # for each point in points, find closest point on fault.distances, index is in m along fault
    prof_dists[[f]]   <- data.frame ( distance=sort(unlist(nn2(fault.distances@coords,pts@coords,k=1)[1])))
    fval[[f]]         <- rep        ( f,nrow(prof_dists[[f]]))
  }
  
  prof_list_overall_unlist  <- unlist     ( prof_list_overall,recursive =FALSE)
  prof_dists_overall_unlist <- unlist     ( prof_dists)
  fval_unlist               <- unlist     ( fval)
  
  dist_along_fault          <- data.frame ( prof_ind         = 1:length(prof_dists_overall_unlist),
                                            dist_along_fault = prof_dists_overall_unlist,
                                            f                = fval_unlist)
  
  # Save
  # saveRDS(prof_list_overall_unlist,file = paste0(profiles_dir_t,fault_name,'_profile.RDS'))
  # saveRDS(dist_along_fault,        file = paste0(profiles_dir_t,fault_name,'_distances.RDS'))
  
  list_details <- c('details','prof_list_overall_unlist','dist_along_fault')
  saveRDS( list   ( list_details , prof_list_overall_unlist , dist_along_fault ),
           paste0 ( profiles_dir_t , fault_name , '_prof_dist.RDS' ) )
}



