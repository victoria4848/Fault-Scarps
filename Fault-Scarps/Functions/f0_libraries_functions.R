
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
library(plyr)
library(gridExtra)
library(grid)
library(SpaDES)     # For splitraster function
library(zoo)        # For rollapply smoothing function
library(segmented)  # For segmenting fault 
library(tictoc)     # For finding time taken... 
library(reshape2)
library(tidyverse)
library(parallel)
library(ff)         # For file.move

library(retistruct)
library(viridis)
library(TTR)
library(caTools)
library(RColorBrewer)

library(sp)
library(rgeos)
library(cowplot)
library(rsvg)
library(png)
library(sf)
library(showtext)
library(rmapshaper) # for simplifying shapefile

library(msir)       # for loess.sd for standard deviations
library(ggspatial)  # For geom_polyplot
library(filesstrings)
library(fasterize)
library(sf)
library(pracma)


# Source functions needed
# For Create_profiles
source('Functions/f1_fault_smoothing.R')
source('Functions/f1_fault_lengths.R')
source('Functions/f1_river_distances.R')
source('Functions/f1_DEM_merge_crop.R')
source('Functions/f1_create_profiles.R')

# For algorithm
source('Functions/f1_segmented_profiles.R')
source('Functions/f1_unique_fault_planes.R')
source('Functions/f1_find_fault_plane.R')               
source('Functions/f1_remove_multi_fault_planes.R')      

# For overall plots 
source('Functions/plot_multi_arrange.R')


## Make mini functions
euc.dist      <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
rms           <- function(x) sqrt(mean(x$residuals^2))

get_pl        <- function(x){
    ldf1  <- profiles[tab$start_ind[x[1]]:tab$end_ind[x[2]],]
    X     <- matrix(c(rep(1,nrow(ldf1)), ldf1[,4]),ncol=2)
    lmf   <- lm.fit(X,y=ldf1[,3])
}

get_coef       <- function(x)     x$coefficients[2]
# get_width      <- function(x) diff(range(profiles$DistTotal[unlist(tab_ids[x[1]:x[2]])]))
get_width      <- function(x)     tab$end_dist_along[x[2]]    - tab$start_dist_along[x[1]]
get_cr_st_dist <- function(x,y)   tab$start_dist_along[y]     - tab$end_dist_along[x[2]] # y is crest
get_ba_st_dist <- function(x,y)   tab$start_dist_along[x[1]]  - tab$end_dist_along[y] #y is base

# For OPTI_MAX
fun_m_grad     <- function(x,y,z)      max(z*x,y)
fun_m_resid    <- function(x,y,z,a,b)  max(a*x*b*y,z)
fun_mpwl       <- function(x,y,z)      z*(1+(x-z)/(profile_half_width)* y)
fun_mpwr       <- function(x,y,z)      x*(1+(z-x) /(profile_half_width)* y)
fun_okgrad_dsl <- function(x,a,b)      (x <   a) & (x > -b)
fun_okgrad_dsr <- function(x,a,b)      (x >  -a) & (x <  b)
fun_mrl        <- function(x,y,z,a)    a * (1 + (x-y)/profile_half_width*z)
fun_mrn        <- function(x,y,z)      1 - (x-y)/profile_half_width*z
fun_prg        <- function(x,a)        if(is.na(x))  rep(NA,length(a)) else abs(a-x)
fun_mal        <- function(x,a)        a[which(a[x,2]==max(a[x,2])),]
fun_mil        <- function(x)          as.numeric(x[which(x[,1]==min(x[,1])),])
fun_mar        <- function(x,a)        a[which(a[,1]*as.logical(x)==min(a[which(as.logical(x)),1])),]
fun_mir        <- function(x)          as.numeric(x[which(x[,2]==max(x[,2])),])

fun_rmatch <- function(x,a) which(a[,1]==x[1] & a[,2]==x[2])

fun_proj    <- function(x,a) x$coefficients[1]+x$coefficients[2]*a
fun_grad    <- function(x) round(x$coefficients[2]*100000)/100000
fun_inter   <- function(x) round(x$coefficients[1]*100000)/100000
fun_lgrgdif <- function(x) if (x < 0.005) 0 else if (x < 0.01) 2 else if (x < 0.03) 4 else 6
fun_width   <- function(x) if (x < 25)    0 else if (x < 50)   2 else if (x < 100)  4 else 6
fun_lgrgsum <- function(x) if (x < 0.0025) 0 else if (x < 0.005) 2 else if (x < 0.015) 4 else 6
fun_centre  <- function(x) if (x < 25)    0 else if (x < 50)   2 else if (x < 125)  4 else 6

fun_ci <- function(x,y,z,a) {  # For finding confidence intervals
    if (sum(which(x>0)>0)) grad_which_ind <- which(x>0) else return(0)
    l_grad_prev_dif    <- x[grad_which_ind]
    ci_grad_prev_dif_l <- sapply(l_grad_prev_dif,fun_lgrgsum)
    ci_left_init       <- ci_grad_prev_dif_l + y[grad_which_ind] + z[grad_which_ind]
    le_cr_select       <- grad_which_ind[which(ci_left_init==min(ci_left_init))[1]]
}


