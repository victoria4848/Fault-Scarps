fault_lengths <- function(fault_name) {
  fault.df     <- readRDS(paste0('Faults_R/',fault_name,'.df.RDS')) # mini corner smoothing
  fault.df.sm  <- readRDS(paste0('Faults_R/',fault_name,'.df.sm.RDS')) # more smoothing for offset versus distance
  fault.df.sm2 <- readRDS(paste0('Faults_R/',fault_name,'.df.sm2.RDS')) # even more smoothing for fault length
  
  # Find straight distance between ends of fault
  straight <-  euc.dist(c(fault.df$long[1], fault.df$lat[1]), 
                        c(fault.df$long[nrow(fault.df)], fault.df$lat[nrow(fault.df)]))
  
  # Make points polylines and find lengths - with slightly smoothed profile sm
  plyse   <-  as.PolySet(data.frame(X=fault.df.sm$long, Y=fault.df.sm$lat, PID=1,POS=1:nrow(fault.df.sm)),projection='UTM')
  offset_distance <- as.numeric(calcLength (plyse, rollup = 3, close = FALSE)[2])
  
  # Make points polylines and find lengths - with more smoothed profile sm2
  plyse  <-  as.PolySet(data.frame(X=fault.df.sm2$long, Y=fault.df.sm2$lat, PID=1,POS=1:nrow(fault.df.sm2)),projection='UTM')
  length_smooth <- as.numeric(calcLength (plyse, rollup = 3, close = FALSE)[2])
  
  
  ## segment lengths for wiggliness index
  plyse <-  as.PolySet(data.frame(X=fault.df$long, Y=fault.df$lat, PID=as.numeric(fault.df$id),POS=1:nrow(fault.df)),projection='UTM')
  segment_lengths <- calcLength (plyse, rollup = 3, close = FALSE)
  
  segment_straight <- c()
  for (i in 1:nrow(segment_lengths)){
    segment_straight[i] <- euc.dist(c(fault.df[fault.df$id==i,]$long[1], fault.df[fault.df$id==i,]$lat[1]), 
                                    c(fault.df[fault.df$id==i,]$long[nrow(fault.df[fault.df$id==i,])], fault.df[fault.df$id==i,]$lat[nrow(fault.df[fault.df$id==i,])]))
  }
  
  lengths_list <- list(straight,length_smooth,offset_distance,segment_straight,segment_lengths)
  
  saveRDS(lengths_list, paste0('Faults_R/',fault_name,'_lengths.RDS'))
  
}