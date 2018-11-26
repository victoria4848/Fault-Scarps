# Load fault and make RDS
fault_rds<- function(fault_name){
  
  
  # Find name of directory where fault shapefile is
  dirs <- list.dirs(path='Faults/')
  dir <- grep(x=dirs, pattern=fault_name, value=TRUE)
  # Extract name of shapefile 
  fl_nam <- strsplit(list.files(path = dir,pattern = "\\.shp$"),"[.]")[[1]][1]
  # Load shapefile
  fault <- readOGR(dir,fl_nam)
  # Smooth line so not super tight corners, so perpendicular isn't too jagged
  fault <- smooth(fault, method = "chaikin")
  fault.df <- fortify(fault)[c(1,2,5)] # fortify to plot with ggplot - get rid of unneccessary columns
  # Make the fault start from the lowest latitude end
  if (fault.df[1,2] > fault.df[nrow(fault.df),2]){
    fault.df <-  fault.df[rev(rownames(fault.df)),]
  } 
  # Save as RDS
  saveRDS(fault, file=paste0('Faults_R/',fault_name,'.RDS'))
  saveRDS(fault.df,paste0('Faults_R/',fault_name,'_fault.df.RDS'))
  
}