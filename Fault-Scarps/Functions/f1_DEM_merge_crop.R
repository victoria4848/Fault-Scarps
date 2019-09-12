f1_DEM_merge_crop <- function(fault_name,dem_folder_name,profile_half_width,proj_all){
  
  print(paste0('Creating DEMS for ',fault_name))
  
  faults_dir_R  <- paste0('Data/Faults_R/')
  DEMs_dir      <- paste0('Data/DEMs/')
  DEMs_dir_R    <- paste0('Data/DEMs_R/')
  
  # Load fault 
  fault         <- readRDS(paste0(faults_dir_R,fault_name,'.RDS'))[[8]] # Original projection of segmented fault

  # Find extent of fault plus added border 
  e             <- extent(fault) + profile_half_width*2.5
  
  # Directory name where downloaded rasters are
  dirname       <- paste0(DEMs_dir,dem_folder_name,'/')
  # Find names of all .tif files in the directory
  raster_tifs   <- list.files(path = dirname,pattern = "\\.tif$")
  
  # If length of raster is one, no need to merge, can just rasterize 
  if (length(raster_tifs) == 1){
    dem_crop      <- raster(paste0(dirname,raster_tifs))  # no more croppping

  } 
  
  if ( length(raster_tifs) != 1 ){ 
    # Find tifs that overlap with fault and crop them to fault extent 
    m     <- list()
    ind   <- 1
    for (i in 1:length(raster_tifs)){
      te    <- raster        ( paste0 ( dirname , raster_tifs[i] ))
      if (tryCatch(!is.null(crop(te,e)), error=function(e) return(FALSE))){
        m    [[ind+0]] <-  te
        ind            <- ind+1
      }
    }
    
    # Merge the tifs 
    if (length(m) > 1) {
    dem_crop    <- do.call(merge, m)
    }
    if (length(m) == 1) {
      dem_crop    <- m[[1]]
    }
    rm(m)
  } # End of if lengths of tifs is more than 1
  

  # If the projection of the dem is different to the wanted projects, proj_all, then convert it
  if  (!compareCRS ( dem_crop , proj_all)) {
    dem_crop      <- projectRaster ( dem_crop , crs = proj_all )            
  }
  
  saveRDS( dem_crop    ,paste0 ( DEMs_dir_R , fault_name , '_dem_crop.RDS'    ))
  print(paste0('Resolution of DEM is ',res(dem_crop)[1],', ',res(dem_crop)[2]))
  
  rm(dem_crop) # Remove this from memory so it doesn't fill up
}



