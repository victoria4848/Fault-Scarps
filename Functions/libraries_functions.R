
# Load libraries needed 
library(smoothr)
library(rgdal)
library(raster)
library(PBSmapping)  # For finding length of polyline
library(KernSmooth)  # For kernel smoothing of fault 
library(spatstat)    # For creating line segment pattern from fault
library(maptools)    # For creating line segment pattern from fault
library(velox)       # For fast raster manipulation 
library(geosphere)   # For distance matrix betwen points 
library(RANN)
library(ggplot2)
library(dplyr)
library(openxlsx)
library(seqinr)

library(shiny)
library(plotly)
library(shinyjs)
library(shinyalert)
library(leaflet)
library(leaflet.extras)
library(retistruct)
library(viridis)
library(TTR)
library(caTools)
library(RColorBrewer)

# Source functions needed
# For Create_profiles
source('Functions/fault_load.R')
source('Functions/DEM_merge_crop.R')
source('Functions/create_profiles.R')
source('Functions/fault_lengths.R')

# For Find_offsets
source('Functions/find_av_max_offsets.R')
source('Functions/offset_from_crest_base.R')
source('Functions/function_offset_from_crest_base_manual.R')
source('Functions/function_offset_from_crest_base_algorithm.R')
source('Functions/function_crest_base_algorithm.R')
source('Functions/plots.R')
source('Functions/multi_plot_comp.R')
source('Functions/rms_difs.R')
source('Functions/fault_scarp_app_efficient.R')