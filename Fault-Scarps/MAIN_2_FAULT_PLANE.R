# MAIN_2_FAULT_PLANE

# SCRIPT OUTLINE
# 1. Set parameters for finding potential fault plane
# 2. Find potential fault planes
# 3. Save results in 'Results/Profiles/'

# 1. ======================= SET PARAMETERS ================================================================== #

min_grad_multi_choices   <- 2  # seq ( 2,10,1)  # Muptiplication of average gradient. If lower, can be less steep 
min_min_gradient_choices <- 0.01                # Very minimum gradient in case profile is very flat (i.e. sometimes a lake, stops faults being create here)
max_fault_width          <- 50    

min_min_gradient2        <- 0.005
grad_combos              <- expand.grid ( min_grad_multi_choices , min_min_gradient_choices)


for (i in i_choices) {  # For all faults selected 
  fault_name      <- fault_name_list[i]
  downtoside      <- downtoside_list[i]
  
  print(paste0('MAIN_2_FAULT_PLANES ',fault_name))
  
  # 2. ======================= FIND FAULT PLANE ================================================================= #
  profiles_list <- readRDS ( paste0 ( 'Results/Profiles/' , fault_name , '_prof_dist.RDS' ) ) # Load profiles (made in MAIN_1_FAULT_DEM_PROF)
  seg_prof      <- profiles_list[[4]]                                                         # Get segmented profiles 
  
  okp         <- vector('list',length(seg_prof))
  for (pr in 1:length(seg_prof)){
    tab        <- data.frame(seg_prof[[pr]][1])
    
    okp[[pr]]  <- f2_unique_fault_planes(tab,                     # Two other functions are used within here 
                                         grad_combos,
                                         profile_half_width,
                                         downtoside,
                                         min_min_gradient2,
                                         max_fault_width)
  }
  
  # Put found fault planes into different format
  okp_all <- vector ( 'list' , length(seg_prof) )
  for (j in 1:length(seg_prof)){
    tab  <- data.frame ( seg_prof[[j]][1])
    for (i in 1:nrow(grad_combos)){
      okp_all[[j]][[i]] <- rep ( FALSE,nrow(tab)-1)
      if (length(okp[[j]])>0){
        for (g in 1:length(okp[[j]])){
          if (i %in% okp[[j]][[g]][[2]] ){
            okp_all[[j]][[i]] <- okp[[j]][[g]][[1]] 
          }
        }
      }
    }
  }
  
  
  # 3. ======================= SAVE FAULT PLANES ================================================================= #
  profiles_list[[1]][c(5,6)] <- c( 'okp' , 'okp_all')  # What is the ??????? difference between okp and okp_all??
  profiles_list[[5]]           <- okp 
  profiles_list[[6]]           <- okp_all
  saveRDS ( profiles_list , paste0('Results/Profiles/',fault_name,'_prof_dist.RDS'))
  
}


