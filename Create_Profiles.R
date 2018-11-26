## 1.  Alter profile_spacing: this is the spacing along the fault in metres
## 2.  Alter profile_point_spacing: this is the spacing of points along the elevation profiles in metres. The minimum spacing
#      should be the resolution of your DEM. The algorithm uses a nearest neighbours technique to interpolate the elevations
#      along the profiles. 
## 3.  Alter profile_half_width: this is in metres, and will be the half-width of the elevation profiles
## 4.  When done, open app_manual_profiles.R and run this


## ***Parameters to alter*** ##
profile_spacing <- 2  # metres: Spacing of profiles along the fault
profile_point_spacing <- 2  # metres: Spacing of points along elevation profiles, perpendicular to scarp
profile_half_width <- 200  # Half-width of elevation profiles in metres

redo <- 'yes'  # 'no' or 'yes': If no, if the functions have already been run, they won't run again
###############################


library(rstudioapi)
# Set working directory to where this file is
setwd( dir = dirname(rstudioapi::getSourceEditorContext()$path)) 
source('Functions/libraries_functions.R') # Load libraries and source functions 
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

fault_name_area_downtoside <- read.xlsx('FaultNames.xlsx')
fault_name_list <- fault_name_area_downtoside$FaultNames
dem_folder_name_list   <- fault_name_area_downtoside$DEM_folder_name

#for (i in 1:length(fault_name_list)){
for (i in 13){
  fault_name <- fault_name_list[i]
  dem_folder_name <- dem_folder_name_list[i]
  
  ## FAULT TRACE ##
  if (!file.exists(paste0('Faults_R/',fault_name,'.RDS'))| redo!='no'){
    
    # Load fault
    fault <- fault_load(fault_name)
    
    # Smooth fault to fit a high order polynomial for profiles
    fault_sm <- smooth(fault, method = "ksmooth", smoothness=3)
    saveRDS(fault_sm, file=paste0('Faults_R/',fault_name,'.RDS'))
    # Make fault into a dataframe
    fault.df <- fortify(fault_sm)[c(1,2,5)] # make dataframe and get rid of unneccessary columns
    # Make the fault start from the lowest latitude end
    if (fault.df[1,2] > fault.df[nrow(fault.df),2]){fault.df <-fault.df[rev(rownames(fault.df)),]}
    fault.df$id <-  as.numeric(fault.df$id) + 1  # so ID starts at 1 (not 0)
    saveRDS(fault.df,paste0('Faults_R/',fault_name,'.df.RDS'))
    # Smooth to find distance along the fault for profiles
    sp <- smooth.spline(fault.df$long,fault.df$lat,spar=0.5)
    smoothedsp <- predict(sp)
    plot(fault.df$long, fault.df$lat)
    lines(smoothedsp$x,smoothedsp$y, col="red")
    fault.df.sm <- data.frame(long=smoothedsp$x, lat=smoothedsp$y)
    saveRDS(fault.df.sm,paste0('Faults_R/',fault_name,'.df.sm.RDS'))
    
    # Smooth fault more to measure total length
    sp2 <- smooth.spline(fault.df$long,fault.df$lat,spar=1)
    smoothedsp2 <- predict(sp2)
    plot(fault.df$long, fault.df$lat)
    lines(smoothedsp2$x,smoothedsp2$y, col="red")
    fault.df.sm2 <- data.frame(long=smoothedsp2$x, lat=smoothedsp2$y)
    saveRDS(fault.df.sm2,paste0('Faults_R/',fault_name,'.df.sm2.RDS'))
    # Find fault lengths: saved as fault_name_lengths in Faults_R
    fault_lengths(fault_name)
  }
  
  ## DEM ##
  # Merge and crop DEMs together which fault passes through
  if (!file.exists(paste0('DEMs_R/',fault_name,'_dem_crop.RDS')) | redo!='no'){
    DEM_merge_crop(fault_name,dem_folder_name,profile_half_width)
  }
  
  ## PROFILES ##
  # **Options**
  # : profile spacing in metres. 
  # : spacing of points along profile to get height in metres
  
  if (!file.exists(paste0('Profiles/',fault_name,'_profile.RDS'))| redo!='no'){
    create_profiles(fault_name,profile_spacing,profile_point_spacing,profile_half_width)
  }
  
  print(paste0('Profiles created for ',fault_name))
}


## MANUALLY FIND CREST AND BASE ##
# **Options**
# : Choose spacing of manually chosen profiles in app
print('Open app_manual_profiles.R and run this')



## ALOGORITHM CREST AND BASE ##
# **Options**
# : Smoothing bin width in DEM points 
# : Slope cutoff
# : Curve cutoff
# : Length of fault cutoff DEM points 

## OFFSETS FROM MANUAL AND ALGORITHM ##

## RMS DIFFERENCE BETWEEN MANUAL AND ALGORITHM ##
# Find RMS difference and NAs per profile

# Plots RMS difference versus number of NAs

# Choose the best RMS difference/NA compromise 

# Plot the best compromise offset along length








