## River distances
# Find distances of rivers etc. to plot along the fault offset profile
#( And maybe exclude these segments from calculating average offset)

f1_river_distances <- function(fault_name,proj_all){
  
  rivers_dir_t <- paste0('Data/Rivers/',fault_name)
  rivers_dir_R <- paste0('Data/Rivers_R/')
  faults_dir_R <- paste0('Data/Faults_R/')
  
  proj_all  <- crs( readRDS( paste0(faults_dir_R,fault_name,'.RDS'))[[2]] )
  
  # Find name of directory where fault shapefile is
  dir         <- list.dirs(path= rivers_dir_t)
  # Extract name of shapefile 
  # Load shapefile
  rivers      <- readOGR     ( dsn=rivers_dir_t , layer=paste0(fault_name, '_rivers'))
  rivers$id   <- seq         ( 1 , nrow (rivers@data) , 1 )  # Make sure ids of individual segments are 1,2,3 etc.
  rivers      <- spTransform ( rivers , proj_all) # Put in projection of all other data (in Start_Script)
  
  # Smoothed line along fault for scarp and river distances 
  fault.df.sm <- readRDS(paste0(faults_dir_R,fault_name,'.RDS'))[[4]] # To assign distances to profiles
  
  # Make points every 1m along smooth fault line to then later find distance to match profiles
  smooth_fault_line <- list(Lines(Line(coords=fault.df.sm),ID=1))
  fault.df.sm.sp    <- SpatialLines(smooth_fault_line ,proj4string=crs(proj_all))
  len               <- SpatialLinesLengths(fault.df.sm.sp)
  # fault.distances will be every 1m along the smooth line - find which point is closest 
  fault.distances   <- spsample(fault.df.sm.sp,round(len), type='regular')
  river_dists       <- data.frame(distance=rep(NA,max(rivers$id)*2),f=NA)
  # rivers.sp <- SpatialPointsDataFrame(coordinates(rivers),proj4string=crs(proj_all))
  for (f in 1:max(rivers$id)){
    river_ss        <- subset(rivers, id==f)
    distances       <- unlist(nn2(fault.distances@coords,matrix(unlist(coordinates(river_ss)),ncol =2),k=1)[1])
    river_dists[c(f*2-1,f*2),] <- data.frame(distance=c(min(distances), max(distances)),
                                             f=f)
  }
  
  # Find distances of river along the fault 
  
  # Scarp_rivers should be a list of distances along the fault - starts and ends 
  scarp_rivers      <- cbind(river_dists,height=0)
  scarp_rivers$d_km <- scarp_rivers$distance / 1000 # Get in km 
  
  saveRDS( scarp_rivers, file = paste0( rivers_dir_R , fault_name , '_rivers.RDS') )
  
}
