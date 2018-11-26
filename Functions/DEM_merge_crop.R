DEM_merge_crop <- function(fault_name,dem_folder_name,profile_half_width){
  
  
  # Load fault 
  fault <- readRDS(paste0('Faults_R/',fault_name,'.RDS'))
  
  # Find extent of fault plus added border 
  e <- extent(fault) + profile_half_width*2 
  
  # Directory name where downloaded rasters are
  dirname <- paste0('DEMs/',dem_folder_name,'/')
  # Find names of all .tif files in the directory
  raster_tifs <- list.files(path = dirname,pattern = "\\.tif$")
  
  # Find tifs that overlap with fault and crop them to fault extent 
  m<-list()
  ind <- 0
  for (i in 1:length(raster_tifs)){
    te <- raster(paste0(dirname, raster_tifs[i]))
    if (tryCatch(!is.null(crop(te,e)), error=function(e) return(FALSE))){
      ind <- ind+1
      m[[ind]] <- raster::crop(te,e)
    }
  }
  
  # Merge the tifs 
  dem_crop <- m[[1]]
  if (length(m)>1){
    for (i in 2:length(m)){
      dem_crop <- merge(dem_crop,m[[i]])
    }
  }
  
  print(paste0('Resolution of DEM is ',res(te)[1],', ',res(te)[2]))
  # # Create slope and aspect from cropped raster
  # s.c <- terrain(l.c, opt = 'slope')
  # a.c <- terrain(l.c, opt = 'aspect')
  # # Create hillshade
  # h.c <- hillShade(s.c, a.c, angle=45, direction=0)
  # h.df <- fortify(as.data.frame(h.c,xy=TRUE)) # fortify to plot with ggplot
  # l.df <- fortify(as.data.frame(l.c,xy=TRUE)) # fortify to plot with ggplot
  
  
  saveRDS(dem_crop,paste0('DEMs_R/',fault_name,'_dem_crop.RDS'))
  
  
}