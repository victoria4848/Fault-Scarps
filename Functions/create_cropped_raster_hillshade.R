create_cropped_raster_hillshade <- function(fault_name,crop_opt){
  
  fault_border <- 400
  
  dirname_faults <-paste0('Faults/')
  area_fault_name <- dir(path = dirname_faults,pattern = fault_name,full.names = FALSE,recursive=FALSE)
  country <- strsplit(area_fault_name, "[_]")[[1]][1]
  area_number <- strsplit(area_fault_name, "[_]")[[1]][2]
  area_name <- paste0(country,'_',area_number)
  
  fault <- readRDS(paste0('Faults_R/',fault_name,'.RDS'))
  fault.df <- readRDS(paste0('Faults_R/',fault_name,'_fault.df.RDS'))
  
  # Crop raster to nearby fault and calculate hillshade 
  e <- extent(fault) + fault_border  #40m either side of fault 
  
  # Directory name where downloaded rasters are
  dirname <- paste0('Lidar/',area_name,'/')
  # Find names of all .tif files in the directory
  raster_tifs <- list.files(path = dirname,pattern = "\\.tif$")
  
  ## If you want to merge and crop raster
  if (crop_opt==TRUE){
    ## Load in raster, crop in, then merge them together later
    m<-list()
    ind <- 0
    for (i in 1:length(raster_tifs)){
      # for (i in 1:10){
      te <- raster(paste0(dirname, raster_tifs[i]))
      if (tryCatch(!is.null(crop(te,e)), error=function(e) return(FALSE))){
        ind <- ind+1
        m[[ind]] <- raster::crop(te,e)
      }
    }
    # intersect
    
    l.c <- m[[1]]
    if (length(m)>1){
      for (i in 2:length(m)){
        l.c <- merge(l.c,m[[i]])
      }
    }
    
  }
  if (crop_opt==FALSE){
    l.c <- raster(paste0(dirname, raster_tifs))
  }
  
  # # Create slope and aspect from cropped raster
  # s.c <- terrain(l.c, opt = 'slope')
  # a.c <- terrain(l.c, opt = 'aspect')
  # # Create hillshade
  # h.c <- hillShade(s.c, a.c, angle=45, direction=0)
  # h.df <- fortify(as.data.frame(h.c,xy=TRUE)) # fortify to plot with ggplot
  # l.df <- fortify(as.data.frame(l.c,xy=TRUE)) # fortify to plot with ggplot
  # 
  # saveRDS(l.df,paste0('Lidar_R_crop/',fault_name,'_l.df.RDS'))
   saveRDS(l.c,paste0('Lidar_R_crop/',fault_name,'_l.c.RDS'))
  # saveRDS(h.df,paste0('Lidar_R_crop/',fault_name,'_h.df.RDS'))
  
}