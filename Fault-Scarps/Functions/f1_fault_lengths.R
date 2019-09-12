# Called from MAIN
# For each fault_name, find lengths:
# 1. Straight line distance between end of faults
# 2. Strike
# 3. Lengths from slightly smoothed fault profile for offset distance
# 4. Lengths from more smoothed profile for length of fault
# 5. Lenghts actual and lengths straight for fault segments - (segment_straight, segment_lengths) for wiggliness index


f1_fault_lengths <- function(fault_name) {
  
  faults_dir_R <- paste0('Data/Faults_R/')
  
  fault_list   <- readRDS(paste0(faults_dir_R, fault_name,'.RDS')) # created in fault_smoothing
  fault.df     <- fault_list[[3]] # mini corner smoothing
  fault.df.sm  <- fault_list[[4]] # more smoothing for offset versus distance
  fault.df.sm2 <- fault_list[[5]] # even more smoothing for fault length
  
  # Find straight distance between ends of fault
  straight <-  euc.dist(c(fault.df$long[1], fault.df$lat[1]), 
                        c(fault.df$long[nrow(fault.df)], fault.df$lat[nrow(fault.df)]))
  # Find strike
  strike <- atan((fault.df$lat[nrow(fault.df)]-fault.df$lat[1])/
                    (fault.df$long[nrow(fault.df)]-fault.df$long[1]))*180/pi 
  
  
  # Make points polylines and find lengths - with slightly smoothed profile sm
  plyse           <-  as.PolySet(data.frame(X=fault.df.sm$long, Y=fault.df.sm$lat, PID=1,POS=1:nrow(fault.df.sm)),projection='UTM')
  offset_distance <- as.numeric(calcLength (plyse, rollup = 3, close = FALSE)[2])
  
  # Make points polylines and find lengths - with more smoothed profile sm2
  plyse           <-  as.PolySet(data.frame(X=fault.df.sm2$long, Y=fault.df.sm2$lat, PID=1,POS=1:nrow(fault.df.sm2)),projection='UTM')
  length_smooth   <- as.numeric(calcLength (plyse, rollup = 3, close = FALSE)[2])
  
  
  ## segment lengths for wiggliness index
  plyse           <-  as.PolySet(data.frame(X=fault.df$long, Y=fault.df$lat, PID=as.numeric(fault.df$id),POS=1:nrow(fault.df)),projection='UTM')
  segment_lengths <- calcLength (plyse, rollup = 3, close = FALSE)
  
  segment_straight <- c()
  for (i in 1:nrow(segment_lengths)){
    segment_straight[i] <- euc.dist(c(fault.df[fault.df$id==i,]$long[1], fault.df[fault.df$id==i,]$lat[1]), 
                                    c(fault.df[fault.df$id==i,]$long[nrow(fault.df[fault.df$id==i,])], fault.df[fault.df$id==i,]$lat[nrow(fault.df[fault.df$id==i,])]))
  }
  
  list_details <- c('details','straight','length_smooth','offset_distance','segment_straight','segment_lengths','strike')
  lengths_list <- list(list_details,straight,length_smooth,offset_distance,segment_straight,segment_lengths,strike)
  saveRDS(lengths_list, paste0(faults_dir_R,fault_name,'_lengths.RDS'))
  
}

