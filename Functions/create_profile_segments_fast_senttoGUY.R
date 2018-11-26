create_profiles_segments_fast <- function(fault_name,mov_av,profile_spacing,profile_half_width){

  
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
  # Load things needed 
  fault <- readRDS(paste0('Faults_R/',fault_name,'.RDS'))
  l.c      <- readRDS(paste0('Lidar_R_crop/',fault_name,'_l.c.RDS'))
  
  num_fault_segs <- length(fault$id)
  le <- c()
  prof_list_overall <- list(list())
  n_points_total_segments <- c()
  for (f in 1:num_fault_segs){
    fault_sub <- subset(fault, id==f)
    le[f] <- SpatialLinesLengths(fault_sub)
    
    dt.psp <- as.psp(fault_sub)  # Create line segment pattern from fault
    di <- lengths.psp(dt.psp) # Find length of fault segments
    an <- angles.psp(dt.psp,directed=TRUE)  # Find angles of fault segments
    ex <- di%%profile_spacing  # get remainder of distance and profile_spacing
    exc <- cumsum(ex)  # find cummulative of remainder
    # If remainder changes across a number, add 1 point to that segment
    no_points_new <- floor(di/profile_spacing) + c(1, diff(floor(exc/profile_spacing)))
    n_points_total <- sum(no_points_new) # find total number of points on fault
    
    
    pts <- spsample(fault,n_points_total,type='regular') # Create points along line
    n_points_total <- length(pts)  # doesn't sample always the same number
    
    # Initialize pts_an and make sure it's the correct lenght
    pts_an <- rep(an,no_points_new)  # get angles for each points
    pts_an <- c(pts_an, rep(pts_an[length(pts_an)],3))
    # Find points perpendicular to line at all the points 
    points <- data.frame(longn=rep(NA,n_points_total),latn=NA,longs=NA,lats=NA)
    for (i in 1:n_points_total) {
      points$longn[i] <- pts@coords[i,1] - sin(pts_an[i]) * profile_half_width 
      points$latn[i] <-  pts@coords[i,2]  + cos(pts_an[i]) * profile_half_width 
      points$longs[i] <- pts@coords[i,1] + sin(pts_an[i]) * profile_half_width 
      points$lats[i] <-  pts@coords[i,2]  - cos(pts_an[i]) * profile_half_width 
    }
    # ggplot()+
    #   geom_point(data=data.frame(lon=pts@coords[,1], lat=pts@coords[,2]), aes(x=lon,y=lat)) +
    #   geom_point(data = points, aes(x=longn,y=latn),color='red',size=0.5) +
    #   geom_point(data = points, aes(x=longs,y=lats),color='green',size=0.5)+
    #   coord_equal(ratio=1)
    
    # Make fault go from low to high latitude 
    if (pts@coords[1,2] > pts@coords[n_points_total,2]){
      points <- arrange(points, rev(rownames(points)))
    }
    
    ### Make elevation profiles ###
    prof_list <- list()
    l.cv <- velox(l.c)
    for (i in 1:n_points_total) {
      cds <- cbind(c(points$longn[i],points$longs[i]),
                   c(points$latn[i],points$lats[i]))
      transects <- SpatialLines(list(Lines(list(Line(cds)), ID =paste(i))),proj4string = crs(l.c))
      transects.pts <- spsample(transects, n=profile_half_width*2, type="regular")
      
      line1 <- data.frame('lon' = transects.pts@coords[,1],
                          'lat'=transects.pts@coords[,2])
      line1$height <- l.cv$extract_points(sp=transects.pts)
      line1$DistTotal <- seq(1,profile_half_width*2,1)
      line1$SMA1 <- SMA(line1$height, n = mov_av)
      prof_list[[i]] <- line1
    }
    
    prof_list_overall[[f]] <- prof_list
    n_points_total_segments[f] <- n_points_total
    
  }
  prof_list_overall_unlist <- unlist(prof_list_overall,recursive =FALSE)

  
  dist_along_fault <- data.frame(prof_ind=1:sum(n_points_total_segments),dist_along_fault=1:sum(n_points_total_segments))
  
  
  # Save
  saveRDS(prof_list_overall_unlist,file = paste0('Profiles/',fault_name,'_profile.RDS'))
  saveRDS(dist_along_fault,file = paste0('Profiles/',fault_name,'_distances.RDS'))
}

