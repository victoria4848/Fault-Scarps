# MAIN_1_FAULT_DEM_PROF

# SCRIPT OUTLINE
# 1. Load fault shapefile, smooth it, and save it as an RDS file in 'Data/Faults_R/'
# 2. If a river shapefile exists in 'Data/Rivers/', this is also loaded and saved in Data/Faults/Rivers_R 
# 3. Find lengths of the fault: total length, smoothed length, length of segments, length of straight line segments 
# 4. Load DEMs - merge if needed, crop if crop_opt <- 'yes' (set in MAIN_0_START_SCRIPT), save as RDS in 'Data/DEMS_R/'
# 5. Create elevation profiles - parameters set in 'MAIN_0_START_SCRIPT.R', save profiles in 'Results/Profiles/'
# 6. Create segmented profiles


# ============================================================================================================= #

for (i in i_choices) {
  
  fault_name         <- fault_name_list[i]
  dem_folder_name    <- dem_folder_name_list[i]
  downtoside         <- downtoside_list[i]

  # 1. =========================== FAULT TRACE ================================================================== #
  f1_fault_smoothing ( fault_name , proj_all ) # Load fault and smooth. Proj_all from Start_Script
  
  
  # 2. ============================ RIVERS ====================================================================== #
  if (file.exists(paste0('Data/Rivers/' , fault_name , '_rivers.shp'))){
    f1_river_distances ( fault_name , proj_all )    # Load marked rivers and distance along profiles. 
  }
  
  
  # 3. =========================== FAULT LENGTHS ================================================================ #
  f1_fault_lengths ( fault_name )        # Find lengths of fault. Lengths are sum of detailed fault segments, straight line segments, length of smooth fault 
  
  
  # 4. ============================= DEMS ======================================================================= #
  print ( paste0 ( 'Creating DEMS for ' , fault_name ))
  f1_DEM_merge_crop ( fault_name , dem_folder_name , profile_half_width , proj_all )
  
  
  # 5. ======================= ELEVATION PROFILES =============================================================== #
  # ELEVATION PROFILES - extract elevation profiles perpendicular to fault trace every profile_point_spacing metres
  # **Options** set in MAIN_0_START_SCRIPT
  # : profile_spacing (metres) is spacing of points along the fault scarp where perpendicular elevation profiles will be taken 
  # : profile_point_spacing (metres) is spacing of points along elevation profile where height will be take 
  # : profile_half_width (metres)
  print ( paste0 ( 'Creating profiles for ' , fault_name ))
  f1_create_profiles ( fault_name , profile_spacing , profile_point_spacing , profile_half_width)   
  
  
  # 6. ======================= SEGMENTED PROFILES =============================================================== #
  print ( paste0 ( 'Creating segmented profiles for ' , fault_name ))
  profiles_list   <- readRDS ( paste0 ( 'Results/Profiles/' , fault_name , '_prof_dist.RDS' ) ) # these were made just above 
  scarp_profiles  <- profiles_list[[2]]
  n_pp_prof       <- nrow     ( scarp_profiles[[1]])
  seg_prof        <- vector   ( 'list' , length ( scarp_profiles ) )  # Initialize list for segmented profiles 
  seg_prof        <- mclapply ( scarp_profiles,
                                f1_segmented_profiles,
                                profile_half_width,
                                profile_point_spacing,
                                n_pp_prof)
  
  # Add segmented profile to profiles_list and save in 'Results/Profiles/'
  profiles_list[[1]][c(4)] <- c('segmented') 
  profiles_list[[4]]       <- seg_prof
  saveRDS ( profiles_list , paste0('Results/Profiles/',fault_name,'_prof_dist.RDS'))

  
} 




