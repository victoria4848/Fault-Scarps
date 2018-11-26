## 1.  Run this a few times to install all the packages correctly:
##     - run until you get a printout message 

## 2.  Add your folder containing fault shp file to Faults folder
## 3.  Add your folder containing DEM(s) tif files to the DEMs folder

install.packages('rstudioapi')
library(rstudioapi)
# Set working directory to where this file is
setwd( dir = dirname(rstudioapi::getSourceEditorContext()$path)) 

# Download scripts from GitHub 
url <- "https://github.com/victoria4848/Fault-Scarps/archive/master.zip"
destfile = "Fault-Scarps.zip"
download.file(url, destfile)
unzip("Fault-Scarps.zip")
file.remove("Fault-Scarps.zip")
file.rename(from='Fault-Scarps-master', to='Fault-Scarps')

# Create folders to add folders to
if (!dir.exists('Fault-Scarps-master/Faults')){
  dir.create('Fault-Scarps-master/Faults')    # Add folder containing fault shp file to this
  dir.create('Fault-Scarps-master/DEMs')      # Add folder containing DEM(s) tif files - extension should be .tif
  # Create folders to store R data made from input data 
  dir.create('Fault-Scarps-master/Faults_R')
  dir.create('Fault-Scarps-master/DEMs_R')
  dir.create('Fault-Scarps-master/Profiles')  # To store the created profiles
  # Create folders for storing outputs from finding crest and base 
  dir.create('Fault-Scarps-master/Manual_profiles_offsets')
  dir.create('Fault-Scarps-master/Offsets')
  dir.create('Fault-Scarps-master/Plots')
  dir.create('Fault-Scarps-master/rms_difs')
}

# Install packages needed
install.packages('smoothr')
install.packages('rgdal')
install.packages('raster')
install.packages('PBSmapping')  # For finding length of polyline
install.packages('KernSmooth')  # For kernel smoothing of fault 
install.packages('spatstat') # For creating line segment pattern from fault
install.packages('maptools') # For creating line segment pattern from fault
install.packages('velox')    # For fast raster manipulation 
install.packages('geosphere') # For distance matrix betwen points 
install.packages('RANN')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('openxlsx')
install.packages('seqinr')
install.packages('shiny')
install.packages('plotly')
install.packages('shinyjs')
install.packages('shinyalert')
install.packages('leaflet')
install.packages('leaflet.extras')
install.packages('retistruct')
install.packages('viridis')
install.packages('TTR')
install.packages('caTools')
install.packages('RColorBrewer')

print('All packages now installed')
print('Add your folder containing fault shp file to Faults folder')
print('Add your folder containing DEM(s) tif files to the DEMs folder')
