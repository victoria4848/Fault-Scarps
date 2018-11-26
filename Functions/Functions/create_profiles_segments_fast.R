create_profiles_segments_fast <- function(fault_name,mov_av,profile_spacing,profile_half_width){
  
  # Load things needed 
  fault.df <- readRDS(paste0('Faults_R/',fault_name,'_fault.df.RDS'))
  l.c      <- readRDS(paste0('Lidar_R_crop/',fault_name,'_l.c.RDS'))
  
  # library(PBSmapping)
  plyse <-  as.PolySet(data.frame(X=fault.df$long, Y=fault.df$lat, PID=as.numeric(fault.df$id)+1,POS=1:nrow(fault.df)),projection='UTM')
  le <- calcLength (plyse, rollup = 3, close = FALSE)
  
  euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
  
  # Find distance of points away to add to line1
  fault.df$id <- as.numeric(fault.df$id)+1
  num_fault_segs <- length(unique(fault.df$id))
  no_segments_sep <- c()
  fault_segments <- data.frame(seg=(1:num_fault_segs))
  ps <- c(); pe <- c()
  euc_distas <- c(); euc_distae <- c();euc_length <- c()
  p1 <- c(fault.df[1,c('long')],fault.df[1,c('lat')])
  for (i in unique(fault.df$id)){
    ps[i] <- min(which(fault.df$id==i))
    pe[i] <- max(which(fault.df$id==i))
    ps2 <- c(fault.df[ps[i],c('long')],fault.df[ps[i],c('lat')])
    pe2 <- c(fault.df[pe[i],c('long')],fault.df[pe[i],c('lat')])
    euc_distas[i] <- euc.dist(p1,ps2) 
    euc_distae[i] <- euc.dist(p1,pe2) 
    no_segments_sep[i] <- sum(fault.df$id==i)-1
    euc_length[i] <- euc_distae[i]-euc_distas[i]
  }
  fault_segments$ps <- ps
  fault_segments$pe <- pe
  fault_segments$euc_distas <- euc_distas
  fault_segments$euc_distae <- euc_distae
  fault_segments$no_segments_sep <- no_segments_sep
  fault_segments$length <- le$length
  fault_segments$euc_length <- euc_length
  fault_segments$curv_ratio <- le$length/euc_length
  fault_segments$weight <- le$length/sum(le$length)
  fault_segments <- arrange(fault_segments, euc_distas)
  fault_segments$longs <- fault.df$long[ps]
  fault_segments$lats <- fault.df$lat[ps]
  fault_segments$longe <- fault.df$long[pe]
  fault_segments$late <- fault.df$lat[pe]
  
  
  ## Points at which to find profiles, along fault line
  
  prof_list_overall <- list(list())
  length_seg <- c()
  num_points <- c()
  for (f in (1:num_fault_segs)){
    
    fault.df.sub <- subset(fault.df, id==f)
    
    # Find start points of segments 
    no_segments <- nrow(fault.df.sub)-1
    angle <- numeric(length=no_segments)
    angle2 <- numeric(length=no_segments)
    seg_length <- numeric(length=no_segments)
    tot_length <- numeric(length=no_segments)
    rem <- numeric(length=no_segments)
    no_points <- numeric(length=no_segments)
    start_point <- matrix(data=0,nrow=no_segments,ncol=2)
    add_lon_lat <- rep('plon_plat',length=no_segments)
    
    
    for (j in 1:no_segments){
      p1 <- c(fault.df.sub$long[j], fault.df.sub$lat[j])
      p2 <- c(fault.df.sub$long[j+1], fault.df.sub$lat[j+1])
      if (sum(p1==p2)==2){  # if there is only 1 point in the segment
        p1 <- c(fault.df.sub$long[j+1], fault.df.sub$lat[j+1])
        p2 <- c(fault.df.sub$long[j+2], fault.df.sub$lat[j+2])
      }
      if (((p2-p1)[1]>0) & ((p2-p1)[2]<0))  {add_lon_lat[j] <- 'plon_nlat'}
      if (((p2-p1)[1]<0) & ((p2-p1)[2]>0))  {add_lon_lat[j] <- 'nlon_plat'}
      if (((p2-p1)[1]<0) & ((p2-p1)[2]<0))  {add_lon_lat[j] <- 'nlon_nlat'}
      angle[j] <- atan((p2[1]-p1[1])/(p2[2]-p1[2]))
      angle2[j] <- pi/2-angle[j]
      
      
      seg_length[j] <- euc.dist(p1,p2)
      if (j == 1){tot_length[j]=0}
      if (j >  1){tot_length[j] <- seg_length[j-1]+tot_length[j-1]}
      rem[j] <- tot_length[j]%%profile_spacing
      if (j==1) {start_point[j,1] <- p1[1];start_point[j,2] <- p1[2] }
      if (j>1) {
        start_point[j,1] <- p1[1] + sin(angle[j])*(profile_spacing-rem[j])
        start_point[j,2] <- p1[2] + cos(angle[j])*(profile_spacing-rem[j])
        if (add_lon_lat[j] == 'plon_nlat'){
          start_point[j,1] <- p1[1] + sin(abs(angle[j]))*(profile_spacing-rem[j])
          start_point[j,2] <- p1[2] - cos(angle[j])*(profile_spacing-rem[j])
        }
        if (add_lon_lat[j] == 'nlon_plat'){
          start_point[j,1] <- p1[1] - sin(abs(angle[j]))*(profile_spacing-rem[j])
          start_point[j,2] <- p1[2] + cos(abs(angle[j]))*(profile_spacing-rem[j])
        }
        if (add_lon_lat[j] == 'nlon_nlat'){
          start_point[j,1] <- p1[1] - sin(angle[j])*(profile_spacing-rem[j])
          start_point[j,2] <- p1[2] - cos(angle[j])*(profile_spacing-rem[j])
        }
      }
      no_points[j] <- floor((euc.dist(p1,p2)-(profile_spacing-rem[j])) /profile_spacing)+1 
      if (j==1){ no_points[j] <- floor(euc.dist(p1,p2) /profile_spacing) +1}
    }
    
    npc <- cumsum(no_points)
    colu <- matrix(NA,nrow=sum(no_points),ncol=1)
    points <- data.frame(long=colu,lat=NA,longn=NA, latn=NA,longs=NA,lats=NA,segment=NA)
    
    
    
    # Find points spaced along all the segments 
    ind <- 1
    for (j in 1:no_segments){
      if (no_points[j]>0){
        for (i in 1:no_points[j]){
          if (i == 1){
            points$long[ind]<- start_point[j,1]
            points$lat[ind] <-start_point[j,2]
            points$segment[ind] <- j
            ind <- ind+1
          }
          if (i > 1) {
            if (add_lon_lat[j] == 'plon_plat'){
              points$long[ind] <- points$long[ind-1]+ sin(angle[j]) * profile_spacing
              points$lat[ind] <- points$lat[ind-1]+ cos(angle[j]) * profile_spacing
            }
            if (add_lon_lat[j] == 'plon_nlat'){
              points$long[ind] <- points$long[ind-1]+ sin(abs(angle[j])) * profile_spacing
              points$lat[ind] <- points$lat[ind-1]- cos(angle[j]) * profile_spacing
            }
            if (add_lon_lat[j] == 'nlon_plat'){
              points$long[ind] <- points$long[ind-1]- sin(abs(angle[j])) * profile_spacing
              points$lat[ind] <- points$lat[ind-1]+ cos(abs(angle[j])) * profile_spacing
            }
            if (add_lon_lat[j] == 'nlon_nlat'){
              points$long[ind] <- points$long[ind-1]- sin(angle[j]) * profile_spacing
              points$lat[ind] <- points$lat[ind-1]- cos(angle[j]) * profile_spacing
            }
            points$segment[ind] <- j
            ind <- ind+1
          } 
        }
      }
    }
    num_points[f] <- ind-1
    
    
    
    # Find points perpendicular to line at all the points 
    ind <- 1
    for (j in 1:no_segments){
      if (no_points[j]>0){
        for (i in 1:no_points[j]) {
          points$longn[ind] <- points$long[ind] - sin(angle2[j]) * profile_half_width 
          points$latn[ind] <-  points$lat[ind]  + cos(angle2[j]) * profile_half_width 
          points$longs[ind] <- points$long[ind] + sin(angle2[j]) * profile_half_width 
          points$lats[ind] <-  points$lat[ind]  - cos(angle2[j]) * profile_half_width 
          ind <- ind+1
        }
      }
    }
    # ############################## TESTING ##############################
    # segment 19 - some points are on wrong side of line....
    #  #ggplot(data=points[965:985,])+
    # ggplot(data=points)+
    # ggplot(data=points)+
    #   geom_path(data=fault.df.sub, aes(x=long,y=lat))+
    #   #geom_raster(data=h.df,aes(x=x,y=y,fill=layer))+
    #   geom_point(aes(x=long,y=lat),size=0.4)+
    #   geom_point(aes(x=longn,y=latn),size=0.4,color='red')+
    #   geom_point(aes(x=longs,y=lats),size=0.4,color='green')
    # ggplot()+
    #   geom_point(data=points[240:248,],aes(x=long,y=lat),size=1,color='red')+
    #   geom_point(data=points[249:255,],aes(x=long,y=lat),size=1,color='cyan')+
    #   # geom_point(data=points[1:10,],aes(x=long,y=lat),size=1,color='red')+
    #   geom_point(data=fault.df.sub[2,],aes(x=long,y=lat),size=1,color='magenta')+
    #   geom_point(data=data.frame(start_point)[2,], aes(x=X1,y=X2),size=0.1, color='black')
    # # ############################## TESTING ##############################
    
    #######################################################
    ### Make elevation profiles ###
    prof_list <- list()
    l.cv <- velox(l.c)
    for (i in 1:npc[length(npc)]) {
      cds <- cbind(c(max(points$longs[i],points$longn[i]),
                     min(points$longs[i],points$longn[i])),
                   c(min(points$latn[i],points$lats[i]),
                     max(points$latn[i],points$lats[i])))
      transects <- SpatialLines(list(Lines(list(Line(cds)), ID =paste(i))),proj4string = crs(l.c))
      
      transects.pts <- spsample(transects, n=profile_half_width*2, type="regular")
      
      line1 <- data.frame('lon' = rev(transects.pts@coords[,1]),
                          'lat'=rev(transects.pts@coords[,2]))
      line1$height <- rev(l.cv$extract_points(sp=transects.pts))
      line1$DistTotal <- seq(1,profile_half_width*2,1)
      prof_list[[i]] <- line1
    }
    
    prof_list_overall[[f]] <- prof_list
  }
  
  prof_list_overall_unlist <- unlist(prof_list_overall,recursive =FALSE)
  
  # fault_segments$length_seg <- length_seg
  #fault_smooth2 <- smooth.spline(cbind(x=fault.df$long,y=fault.df$lat))
  start_seg_dist <- 0; end_seg_dist <- le$length[1]; 
  if (num_fault_segs>1){   # find addition to add on if more than 1 fault segment
    for (f in 2:num_fault_segs){
      # Find distance of points by adding previous fault distances
      # If no ends of segments are closer that start of new segment..
      # if (sum((fault_segments$euc_distas[f] > fault_segments$euc_distae))==0) {
      #   add_on <- fault_segments$euc_distas[f]
      #   end_seg_dist[f] <- length_seg[f]+add_on[f]
      #   start_seg_dist[f] <- euc.dist(c(as.numeric(fault_segments[1,'longs']),as.numeric(fault_segments[1,'lats'])),
      #                                 c(as.numeric(fault_segments[f,c('longs')]),as.numeric(fault_segments[f,c('lats')])))
      # }
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

