# MAIN_0_START_SCRIPT 
# ============================================================================================================= #
# Written by Victoria Stevens, in 2019 (victoria.stevens@uct.ac.za, victoria4848@gmail.com)
# Released under the MIT License
# Article published in GRL as 'AAn Algorithm to find Vertical Offsets in a High-Resolution DEM: 
# Fault Scarps in Fennoscandia and Implications for Seismic Hazard Analysis in Stable Continental Regions'
# Please cite the article above if you end up using this algorithm.

# This  script:
# 1) Organizes data and sets parameters. 
# 2) Calls 'MAIN SCRIPTS' which find offsets every n metres (where n = profile_spacing set above) and assign confidence values based on various criteria.
# 3) If uncommented, and more parameters chosen, calls 'EXTRA SCRIPTS', which are further analyses e.g. summing offsets for overlapping fault strands, smoothing offsets, binning offsets etc.
# ============================================================================================================= #



# 0. ======================= SETUP ============================================================================ #
# a) Set current working directory to be this folder: click 'Files' tab on rhs and then 'More', then 'Set as Working Directory'

# b) Run line below to install packages needed. May have to run a few times to make sure all pacakges are installed: then comment out
source('Functions/f0_install_packages.R')    # *Comment out after running once* - only need to install packages once
# c) Line below loads packages (after they are installed), and creates some short functions needed 
source('Functions/f0_libraries_functions.R') # Don't comment out
# ============================================================================================================= #


# 1. ===================== FAULT DETAILS ====================================================================== #
# a) Fill in 'fault_details.xlsx' which is in Data folder - keep the file name the same 
# FaultNames, DowntoSide, DEM_folder_name	are essential 
# DowntoSide is looking from south to north 
# ============================================================================================================= #


# 2. ======================== DEMS ============================================================================ #
# a). Put DEM tif(s) in 'Data/DEMs' folder
# You can crop the DEM to within the profile_half width of the fault (crop_opt <- 'yes') - this makes the file saved smaller
# If the DEM is already cropped, or you aren't concerned with saved file size, change yes' to 'no'
crop_opt <- 'yes'  # 'yes' to save a cropped version of the DEM. 'no' to save DEM as it is. 
# ============================================================================================================= #
 

# 3. ==================== FAULT SHAPEFILES ==================================================================== #
# a) Put fault shp files to 'Data/Faults/' folder - the shp folder name should have fault name in it
# The fault(s) should each have two shp files 
#   - one that is a fairly detailed trace along the fault in the DEM and can have multiple segments 
#   - one that is a continuous line and best approximates where you think the deeper fault may be
#     This should have 'smooth' in the title 
#     This is to get a continuous distance along the fault. The segment offsets will be projected onto here. 
#     If you have only one segment, the two shp files can be the same, though one should have 'smooth' in the title
# ============================================================================================================= #


# 4. ================== PROJECTION OF DATA ==================================================================== #
# a) ***CHECK*** - projection used should be in utm with units of meters - important to have units of meters 

# Projection for all data will be taken from a fault named below. If all your data is already 
# in the same projection, this won't be used. If it isn't, everything will be converted to this projection. 

# b) Choose name of fault to check all projections
fault_name_for_proj   <- 'Burtrask'  # Name of shp file - all subsequent spatial data used will be converted to the projection of this fault
# ============================================================================================================= #


# 5. (optional) ========= RIVER SHAPEFILE ===================================================================== #
# a) You can add a shapefile to the 'Data/Rivers/' folder that marks rivers/scarp alteration etc. along the fault trace.
# The offsets found in these areas will be removed. Another option would use fault segments that stop and start either side of the river.
# This 'rivers' method can be used if you draw the fault as a continuous trace, but want to not include areas where the scarp has been altered. 
# ============================================================================================================= #


# 6. ================= PROFILE PARAMETERS ===================================================================== #
# a) Chose spacing of profiles along the fault, spacing of points along elevation profiles, and profile half width - used in 'MAIN_1_FAULT_DEM_PROF.R'
profile_spacing       <- 2    # metres: Spacing of profiles created along the fault scarp
profile_point_spacing <- 2    # metres: Spacing of points along elevation profiles, perpendicular to the fault scarp
profile_half_width    <- 200  # metres: Half-width of elevation profiles created
# ============================================================================================================= #


# ============================================================================================================= #
# Shouldn't need to alter anything below, unless you only want to analyze a subset of faults in the fault_details table, 
# in which case you should change i_choices
# ============================================================================================================= #

# Load fault details - dem folder name, fault name, downtoside (down thrown side when looking from south to north) 
fault_details        <- read.xlsx ( 'Data/fault_details.xlsx' )
dem_folder_name_list <- fault_details$DEM_folder_name
fault_name_list      <- fault_details$FaultNames
downtoside_list      <- fault_details$DowntoSide

i_choices <- 1:length(fault_name_list)  # Pick which faults in your fault_details list that you actually want to analyze


# Get projection to convert all data to. 
fault_for_proj <- readOGR ( paste0 ( 'Data/Faults/',fault_name_for_proj) , fault_name_for_proj) # Load fault chosen to get projection from (set above)
proj_all       <- crs     ( fault_for_proj ) # Everything will be converted into this projection (should be utm)



# ======================== MAIN SCRIPTS ============================================================== #
# See 'MORE DETAILS' section below
source ( 'MAIN_1_FAULT_DEM_PROF.R' ) # Load fault, smooth, find lengths of fault, check DEM, create profiles, make segmented profiles, find fault planes 
source ( 'MAIN_2_FAULT_PLANE.R' )    # Find fault planes 
source ( 'MAIN_3_OFFSETS.R' )        # Find offsets and landscape planes from segmented profiles and found fault planes
source ( 'MAIN_4_CVs.R' )            # Consider confidence values and remove offsets from profiles that get a confidence value less than 1sd below the mean confidence value
# ===================================================================================================================== #


# ===================================================================================================================== #
# ========================= EXTRA ===================================================================================== #
# ===================================================================================================================== #

# ============== SET SMOOTHING PARAMETERS ============================================================================= #
# Smooth profiles - also make gaps in smooth line where there are e.g. rivers, no pofiles, low density of profiles
# line_gap_m            : If there is more than a 50m gap in points found, make a gap in smooth profiles 
# min_pdensity          : If there are less than min_pdensity points per 1000m, then remove points 
# smoothing_CV_weight   : Smoothing is weighted by confidence values (CVs), increase to increase affect of CVs, lower to reduce affect of CVs
# overlap_dist_within_m : Distance along smoothed fault that offsets have to be within to combine measurements on different strands

line_gap_m            <- 50  # metres
min_pdensity          <- 5   # points per 1000m
smoothing_CV_weight   <- 1   # 
overlap_dist_within_m <- 4   # metres

source ( 'MAIN_4_SMOOTH_OFFSETS.R' )   # Add up offsets on different strands of fault, and then smooth offsets
source ( 'MAIN_4b_SMOOTH_OFFSETS_BINS.R' )
source ( 'MAIN_4c_LOESS_INTERP.R' )

source ('MAIN_9_PLOT_QUAT_MAP.R') # check colors first
source ('MAIN_8_FACET_PLOTS.R')  # Makes df with quaternary
source ('MAIN_7_SLIP_DISTRIBUTIONS.R')
source ('MAIN_13_PLOT_PROFILES.R')




# ======================= MORE DETAILS ================================================================================ #

# MAIN_1_FAULT_DEM_PROF.R
# Inputs:
#   fault.shp,
#   faultsmooth.shp
# Functions: 
#   f1_fault_smoothing    : Smooth fault - round corners
#   f1_river_distances    : If river.shp exists, find distances along fault where rivers are
#   f1_fault_lengths      : Find lengths of fault (straight distances, smoothed distance etc.)
#   f1_DEM_merge_crop     : load, crop (if crop_opt = TRUE), and merge (if necessary) DEM
#   f1_create_profiles    : create elevation / distance across faults profiles 
#   f1_function_segmented_profiles_new  : create segmented profiles 
#   f1_function_get_unique_fault_planes : find potential fault plane s
# Outputs:

# MAIN_2_OFFSETS.R
# Inputs: 
#   Segmented planes,      seg_prof
#   Distances along fault, scarp_distances,
#   Fault planes found,    okp_all
# Functions:
# Outputs:
#   list of height offsets with confidence values for each profile, height_offset_list
#   list of landscape planes found with their distance along fault, found_planes.df_list

# MAIN_3_CVs.R
# Inputs: 
# Functions:
#   f3_plot_multi_arrange : plot profiles with found planes ordered by confidence values 
# Outputs:
#   okp_orig
#   list of height offsets, for the best faults
#   list of landscape planes found, for the best faults
#   list of fault planes found, for the best faults



# MAIN_4_SMOOTH_OFFSETS.R
# Inputs: 
# Outputs:




