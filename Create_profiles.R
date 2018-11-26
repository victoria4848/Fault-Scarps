####################################################################################
####### Scarp offest versus length #######
####################################################################################
library(rstudioapi)
setwd( dir = dirname(rstudioapi::getSourceEditorContext()$path)) # set working directory to where this file is
# libraries needed
library(smoothr)
library(rgdal)
library(raster)
library(PBSmapping)  # For finding length of polyline
library(KernSmooth)  # For kernel smoothing of fault 
library(spatstat) # For creating line segment pattern from fault
library(maptools) # For creating line segment pattern from fault
library(velox)    # For fast raster manipulation 
library(geosphere) # For distance matrix betwen points 
library(RANN)
library(ggplot2)
library(dplyr)
library(openxlsx)

## SETUP 
# Create folders to add folders to
dir.create('Faults')    # Add folder containing fault shp file to this
dir.create('DEMs')      # Add folder containing DEM(s) tif files - extension should be .tif
# Create folders to store R data made from input data 
dir.create('Faults_R')
dir.create('DEMs_R')
dir.create('Profiles')  # To store the created profiles

## Downloading from GitHub 
url <- "https://github.com/victoria4848/Fault-Scarps/archive/master/Functions.zip"
destfile = "Fault-Scarps.zip"
download.file(url, destfile)
unzip("Fault-Scarps.zip")
file.copy(from = 'Fault-Scarps-master/Functions/', to = './', recursive = TRUE, copy.mode = TRUE)
file.remove("Fault-Scarps.zip")
unlink("Fault-Scarps-master",recursive=TRUE)
##





# Functions to initialize
source('Functions/fault_load.R')
source('Functions/DEM_merge_crop.R')
source('Functions/create_profiles.R')
source('Functions/fault_lengths.R')
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

fault_name_area_downtoside <- read.xlsx('FaultNames.xlsx')
fault_name_list <- fault_name_area_downtoside$FaultNames
dem_folder_name_list   <- fault_name_area_downtoside$DEMname
redo <- 'no'  # 'no' or 'yes': If no, if the functions have already been run, they won't run again
profile_spacing <- 2  # metres: Spacing of profiles along the fault
profile_point_spacing <- 2  # metres: Spacing of points along elevaiton profiles, perpendicular to scarp


#fault_name_list <- 'Isovaara'  # Name of folder containing fault shp file must have this name in it
#dem_folder_name_list <- 'Fin_6' # Name of folder containing DEM(s) tif or img files to be used
for (i in 1:length(fault_name_list)){
  fault_name <- fault_name_list[i]
  dem_folder_name <- dem_folder_name_list[i]
  profile_half_width <- 200  # Half-width of elevation profiles in metres
  
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
#source('app_manual_profiles.R')
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








