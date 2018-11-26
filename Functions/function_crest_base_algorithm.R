function_crest_base_algorithm <- function(fault_name,
                                          slope_cutoff_choices,
                                          curve_cutoff_choices,
                                          steep_in_a_row_choices){
  
  fault_info <- read.xlsx('FaultNames.xlsx',sheet = 1,colNames=TRUE)
  downtoside <- as.character(fault_info$DowntoSide[fault_info$FaultNames==fault_name])
  length_choices <- length(slope_cutoff_choices)*length(steep_in_a_row_choices)*length(curve_cutoff_choices)
  
  # Load scarp profiles and select those which have been done manually # 
  scarp_profiles <-  readRDS(paste0('Profiles/',fault_name,'_profile.RDS'))
  # # # # # # # # # # # # # 
  profile_hw <- round(max(scarp_profiles[[1]]$DistTotal)/2)
  
  # Initialize paramters # 
  algorithm_crba <- list()
  crests <- matrix(NA,ncol=length_choices,nrow=length(scarp_profiles))
  bases <-  matrix(NA,ncol=length_choices,nrow=length(scarp_profiles))
  # # # # # # # # # # # # # 
  for (i in 1:length(scarp_profiles)){
    algorithm_crba[[i]] <- fault_scarp_app_efficient(
      dataframe=scarp_profiles[[i]],
      index_prof = i, 
      crests,
      bases,
      slope_cutoff_choices=slope_cutoff_choices,
      curve_cutoff_choices=curve_cutoff_choices,
      steep_in_a_row_choices=steep_in_a_row_choices,
      length_choices=length_choices,
      downtoside=downtoside,
      middist    =profile_hw,
      withindist =round(profile_hw/2))  # profile_ind aka distance along the fault
  }

  saveRDS(algorithm_crba,paste0('Algorithm_offsets/algorithm_crba_',fault_name,'.RDS') )
}