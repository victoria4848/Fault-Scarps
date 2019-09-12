# Load shp file for faults, make segments ordered 1:n, make dataframe, load shp for smoothed fault, and smooth more

f1_fault_smoothing <- function(fault_name,proj_all){
  
  print(paste0('Creating fault trace for ',fault_name))
  
  faults_dir_t <- paste0('Data/Faults/')
  faults_dir_R <- paste0('Data/Faults_R/')
  
  # Director where fault shapefile is
  dir         <- paste0(faults_dir_t,fault_name)
  
  ## Add continuously drawn fault to find distance along fault for profiles
  # Load shapefile
  fault.cont_o <- readOGR     ( dir , paste0(fault_name,"smooth"))
  fault.cont   <- spTransform ( fault.cont_o , proj_all)
  
  # Smooth a bit 
  fault_sm2    <- smooth(fault.cont,method='ksmooth',smoothness=2)
  fault.df.sm  <- fortify(fault_sm2)
  fault.df.sm  <- fault.df.sm[,c(1,2,3,5)]
  # Make sure smooth fault goes south to north 
  if (fault.df.sm$lat[1] > fault.df.sm$lat[nrow(fault.df.sm)]){
    fault.df.sm$order <- rev(fault.df.sm$order)
  }
  fault.df.sm <- arrange(fault.df.sm, order(id, order)) # Smooth fault in the correct order
  fault.df.sm <- fault.df.sm[,c(1,2)]
  # Make back into a shapefile again that is smooth and in the correct order 
  fault.cont2 <- list ( Lines ( Line ( coords=fault.df.sm ) , ID=1 ))
  fault.cont2 <- SpatialLines(fault.cont2,proj_all)
  fault.cont2 <- SpatialLinesDataFrame(fault.cont2,data=data.frame(id=1))
  
  # TO MAKE DISTANCES ALONG THE FAULT so in correct distance order 
  len             <- SpatialLinesLengths(fault.cont2)
  # fault.distances will be every 1m along the smooth line - find which point is closest 
  fault.distances <- spsample(fault.cont2,round(len), type='regular')
  
  ## Smooth continuously drawn fault to find distance along fault for length of fault 
  fault_fm3    <- smooth(fault.cont,method='ksmooth',smoothness=4)
  fault.df.sm2 <- fortify(fault_fm3)
  fault.df.sm2 <- fault.df.sm2[,c(1,2)]
  
  
  #### SEGMENTED FAULT ####
  # Load shapefile
  fault_o    <- readOGR     (dir , fault_name)
  fault_o$id <- seq         (1 , nrow ( fault_o@data ) , 1 )   # Make sure ids of individual segments are 1,2,3 etc.
  fault      <- spTransform (fault_o , proj_all) # Put in projection of all other data (in Start_Script)
  fault@data <- data.frame(id=fault@data$id) # Remove unuseful columns
  
  # Smooth fault to fit a high order polynomial for profiles
  fault_sm    <- smooth      ( fault, method = "ksmooth", smoothness=2)
  
  # Make fault into a dataframe
  fault.df     <- fortify    ( fault_sm)[c(1,2,3,5)] # make dataframe and get rid of unneccessary columns
  fault.df$id  <-  as.numeric(fault.df$id)   # so ID starts at 1 (not 0)
  
  # Get rid of empty geometries
  seg_numbers <- unique(fault.df$id)
  for(i in 1:length(seg_numbers)){
    fault.df$id[fault.df$id==seg_numbers[i]] <- i
  }
  seg_numbers <- unique(fault.df$id)
  num_segs    <- length(seg_numbers)
  
  # Make distances along fault, then need to order by distances from the beginning of the fault. 
  ft_segs_dist <- data.frame(p1=rep(NA,num_segs),p2=NA)
  for (i in 1:num_segs){
    fault.df.ss     <- fault.df[which(fault.df$id==i),]
    pts             <- matrix(c(fault.df.ss$long[1], fault.df.ss$long[nrow(fault.df.ss)],fault.df.ss$lat[1],fault.df.ss$lat[nrow(fault.df.ss)]) , nrow=2) # Create points along line
    ft_segs_dist[i,]<- unlist ( nn2 ( fault.distances@coords,pts,k=1)[1] )
  }
  
  
  # Make sure segments go from nearest distance to farthest distance, and within segments also
  # Individual segments go from south to north 
  for (i in 1:num_segs){
    ind.ss   <- which(fault.df$id==i)
    fault_ss <- fault.df[ind.ss,]
    if (ft_segs_dist[i,1] > ft_segs_dist[i,2]){
      fault.df$order[ind.ss] <- rev(fault.df$order[ind.ss])
      x <- ft_segs_dist[i,1] ; y <- ft_segs_dist[i,2]
      ft_segs_dist[i,1] <- y ; ft_segs_dist[i,2] <- x
    }
  }
  fault.df <- arrange(fault.df, order(id, order)) # Segments internally in the correct order
  
  # Make segments go in order from closest to furthest 
  f_dist <- ft_segs_dist$p1
  if (is.unsorted(f_dist)){   # If the segments were not sorted 
    ord <- order(f_dist)
    for (i in 1:length(ord)){
      ind.ss   <- which(fault.df$id==ord[i])
      fault.df$id[ind.ss] <- i + 1000000    # to make sure they don't overlap 
    }
    fault.df$id <- fault.df$id - 1000000
  }
  fault.df <- arrange(fault.df, id)
  fault.df <- fault.df[,c(1,2,4)] # Get rid of order column
  
  # Make back into a shapefile again that is smooth and in the correct order 
  fault_list <- vector('list',num_segs)
  for (i in 1:num_segs){
    f_sub <- subset(fault.df, id==i)
    #    slinelist[[i]] <-  Line ( coords=f_sub[,c(1,2)] )
    fault_list[[i]] <- Lines ( Line ( coords=f_sub[,c(1,2)] ), ID=i )
  }
  # fault_list <- Lines(slinelist, ID=1:num_segs)
  fault <- SpatialLines(fault_list,proj_all)
  fault <- SpatialLinesDataFrame(fault,data=data.frame(id=1:num_segs))
  
  ## SAVE RDS FILES ##
  list_details <- c('details','fault sp','fault.df','fault.df.sm','fault.df.sm2','fault cont sp','fault cont original proj','fault original proj')
  saveRDS( list   ( list_details , fault , fault.df , fault.df.sm , fault.df.sm2 , fault.cont2 ,fault.cont_o,fault_o), 
           paste0 ( faults_dir_R , fault_name , '.RDS' ))
  
}
