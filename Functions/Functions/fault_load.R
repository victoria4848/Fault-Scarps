fault_load <- function(fault_name){
  
  # Find name of directory where fault shapefile is
  dirs <- list.dirs(path= 'Faults/')
  dir <- grep(x=dirs, pattern=fault_name, value=TRUE)
  # Extract name of shapefile 
  fl_nam <- strsplit(list.files(path = dir,pattern = "\\.shp$"),"[.]")[[1]][1]
  # Load shapefile
  fault <- readOGR(dir,fl_nam)
  fault$id <- seq(1,nrow(fault@data),1)  # Make sure ids of individual segments are 1,2,3 etc.
  
  return(fault)
  
}