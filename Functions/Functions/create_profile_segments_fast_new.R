create_profiles_segments_fast <- function(fault_name,mov_av,profile_spacing,profile_half_width){
  # library(argosfilter)
  library(spatstat)
  library(smoothr)
  # library(seqinr)
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
  fault_name <- 'Isovaara'
  # Load things needed 
  fault.df <- readRDS(paste0('Faults_R/',fault_name,'.df.RDS'))
  fault <- readRDS(paste0('Faults_R/',fault_name,'.RDS'))
  l.c      <- readRDS(paste0('Lidar_R_crop/',fault_name,'_l.c.RDS'))
  
  # Find lenght of fault
  #plyse <-  as.PolySet(data.frame(X=fault.df$long, Y=fault.df$lat, PID=as.numeric(fault.df$id)+1,POS=1:nrow(fault.df)),projection='UTM')
  #le <- calcLength (plyse, rollup = 3, close = FALSE)
  
  
  ######## NEW ###########
  
  num_fault_segs <- length(fault$id)
  le <- c()
  prof_list_overall <- list(list())
  n_points_total_segments <- c()
  sta_sto <- c()
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
    
    # Find coordinates of start and end points of fault segments 
    crds <- coordinates(as(fault_sub, 'SpatialPoints'))
    sta_sto[[f]] <- crds[c(1, nrow(crds)), ]
  }
  
  prof_list_overall_unlist <- unlist(prof_list_overall,recursive =FALSE)
  # Find euclidian distances betwen starts of segments
  euc_distas <- c()
  euc_distae <- c()
  for (f in 1:num_fault_segs){
    euc_distas[f] <-   euc.dist(c(sta_sto[[f]][2,1],sta_sto[[f]][2,2]), 
                                c(sta_sto[[1]][2,1],sta_sto[[1]][2,2]))
    euc_distae[f] <-   euc.dist(c(sta_sto[[f]][1,1],sta_sto[[f]][1,2]), 
                                c(sta_sto[[1]][1,1],sta_sto[[1]][1,2]))
  }
  
  ## Finding distances 
  start_seg_dist <- 0; end_seg_dist <- le[1]; 
  if (num_fault_segs>1){   # find addition to add on if more than 1 fault segment
    for (f in 2:num_fault_segs){
      # If previous segment doesn't overlap with this segment
      if (euc_distas[f] > euc_distae[f-1]) { 
        
        
        
        
        # If previous segment doesn't overlap with this segment
        if (fault_segments$euc_distas[f] > fault_segments$euc_distae[f-1]) {
          # Find distance bewteen start and end, times by weighted curvature ratio of previous segments 
          dist_between <- euc.dist( c(as.numeric(fault_segments$longe[(f-1)]),as.numeric(fault_segments$late[(f-1)])),
                                    c(as.numeric(fault_segments$longs[f]),    as.numeric(fault_segments$lats[f])) ) *
            fault_segments$curv_ratio[f-1]
          start_seg_dist[f] <- end_seg_dist[f-1]+dist_between
          end_seg_dist[f] <- fault_segments$length[f]+start_seg_dist[f]
        }
        # If previous segment overlaps 
        if (fault_segments$euc_distas[f] <= fault_segments$euc_distae[f-1])  {
          linef <- CreateLinePoints(cbind(fault_segments$longs[f-1],fault_segments$lats[f-1]), 
                                    cbind(fault_segments$longe[f-1],fault_segments$late[f-1]))
          proj_start_p <- ProjectPoint(cbind(fault_segments$longs[f],fault_segments$lats[f]), linef)
          dist_proj_start_p <- euc.dist(cbind(fault_segments$longs[f-1],fault_segments$lats[f-1]),proj_start_p)
          dist_proj_start_p_c <- dist_proj_start_p *  fault_segments$curv_ratio[f-1]
          start_seg_dist[f] <- start_seg_dist[f-1] + dist_proj_start_p_c
          end_seg_dist[f] <- fault_segments$length[f]+start_seg_dist[f]
        }
      }   
    }
    
    # make index of profile number versus 
    dist_along_fault <- list()
    for(c in 1:num_fault_segs) {dist_along_fault[[c]] <- vector()}
    for (f in 1:num_fault_segs){
      for (i in 1:num_points[f]){
        dist_along_fault[[f]][i] <- (i-1)*profile_spacing + start_seg_dist[f]
      }
    }
    
    
    dist_along_fault <- data.frame(prof_ind=1:length(unlist(dist_along_fault)),dist_along_fault=unlist(dist_along_fault))
    
    
    # Save
    saveRDS(prof_list_overall_unlist,file = paste0('Profiles/',fault_name,'_profile.RDS'))
    saveRDS(dist_along_fault,file = paste0('Profiles/',fault_name,'_distances.RDS'))
  }
  
  