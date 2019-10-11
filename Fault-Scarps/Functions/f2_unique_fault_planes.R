# Function get unique fault_planes
f2_unique_fault_planes <- function(tab, grad_combos,profile_half_width,downtoside,min_min_gradient2,max_fault_width){
  
  # Must be at least height of minimum fault plane
  high <- abs(tab$height_dif) > 0.02  # More than minimum height
  # Must be near the middle half 
  mid  <- ((tab$start_dist_along > (profile_half_width/2)) & (tab$end_dist_along < profile_half_width+(profile_half_width/2)))
  

  # Find gradient of profile and alternative gradient for later
  profile_height_change     <- max(tab$start_height,tab$end_height) - min(tab$start_height,tab$end_height) 
  profile_entire_gradient   <- profile_height_change/(profile_half_width*2)
  profile_gradients_alt     <- (tab$end_height[nrow(tab)] - tab$start_height[1] ) / (profile_half_width*2)
  tab2                      <- tab
  tab2$gradients            <- tab$gradients - profile_gradients_alt
  
  min_fault_plan_grad       <- numeric(length(min_grad_multi_choices)*length(min_min_gradient_choices))
  # Find minimum gradient for all different combinations
  for (mg in 1:nrow(grad_combos)){
    min_grad_multi          <- grad_combos[mg,1]
    min_min_gradient        <- grad_combos[mg,2]
    min_fault_plan_grad[mg] <- max(profile_entire_gradient*min_grad_multi,min_min_gradient) # Fault plane
  } # End of for mgmg
  
  new_fp_grad_choices       <- unique(min_fault_plan_grad)
  okp                       <- vector('list',length(new_fp_grad_choices))
  for (i in 1:length(new_fp_grad_choices)){ # For unique gradient choices
    min_fp_grad             <- new_fp_grad_choices[i]
    # If all gradients are similar apart from some in a row, then lower needed gradient
    grad_sim_to_av <- (profile_gradients_alt-tab$gradients) < min_min_gradient2 & (profile_gradients_alt-tab$gradients) > -min_min_gradient2
    dif_grads <- diff(grad_sim_to_av)
    dif_grads <- diff(dif_grads[dif_grads!=0])
    if ((sum(grad_sim_to_av)>2) & (2 %in% dif_grads) & sum(abs(dif_grads)) < 4){ # If gradients are the same as others 
      min_fp_grad <- max(min_fp_grad/1.5,min_min_gradient)
    }
    # Must be high, in middle, above minimum gradient
    okp[[i]][[1]]           <- f2_find_fault_plane(tab,high,mid,min_fp_grad,downtoside)
    okp[[i]][[2]]           <- i
  } # End of for unique gradient choices 
  
  
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  ## ## If no fault has been found.... rotate fault to flat and find steepest now
  for (i in 1:length(okp)){
    if (sum(okp[[i]][[1]])==0){
      min_fp_grad           <- new_fp_grad_choices[i]
      min_fp_grad2          <- min_fp_grad*(profile_height_change/10)
      okp[[i]][[1]]         <- f2_find_fault_plane(tab2,high,mid,min_fp_grad2,downtoside)
      okp[[i]][[2]]         <- which(min_fault_plan_grad==min_fp_grad)
    }
  }
  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
  
  # Now get unique fault plane choices
  fps                       <- lapply(okp, function(x) x[[1]])
  fault_plane_choices       <- unique(fps)
  num_dif_fault_planes      <- length(fault_plane_choices)
  okp_ss                    <- vector('list',num_dif_fault_planes)
  for (i in 1:num_dif_fault_planes){
    fault_planes            <- fault_plane_choices[[i]]
    okp_ss[[i]][[1]]        <- fault_planes
    output                  <- !as.logical(sapply(fps, function(x) sum(1-as.logical(x)==fault_planes)))
    okp_ss[[i]][[2]]        <- c(unlist(sapply(okp[output], function(x) x[[2]])))
    fault_plane_found       <- ifelse(sum(fault_planes)==0, FALSE, TRUE)
    if (fault_plane_found){
      # If there are multiple fault planes, get rid of them, keeping the highest one #previously kept the one nearest the centre
      num_fault_plane_sections <- sum(abs(diff(okp_ss[[i]][[1]])))/2
      if (num_fault_plane_sections>1){
        okp_ss[[i]][[1]]    <- f2_remove_multi_fault_planes(tab,okp_ss[[i]][[1]],profile_half_width,num_fault_plane_sections)
      } # If there are multiple fault planes 
      # If there are still multiple fault planes 
      num_fault_plane_sections <- sum(abs(diff(okp_ss[[i]][[1]])))/2
      if (num_fault_plane_sections>1){
        okp_ss[[i]][[1]]    <- f2_remove_multi_fault_planes(tab,okp_ss[[i]][[1]],profile_half_width,num_fault_plane_sections)
      } # If there are multiple fault planes 
    }
  } # For num_dif_fault_planes
  

  for (i in 1:length(okp_ss)){
    # If there is more than one plane in the fault, make sure it isn't too wide - keep only steepest section
    if (sum(okp_ss[[i]][[1]])>1){
      fault_widths <- abs(tab$start_dist_along[which(okp_ss[[i]][[1]])] - tab$end_dist_along[which(okp_ss[[i]][[1]])])
      ## ## ## ## If gradients of two planes are similar, they can be larger ## ## ## ## 
      # If gradient is lower, and gradients are similar, plane can be wider #
      if (sum(okp_ss[[i]][[1]])==2){
        grs       <- tab$gradients[okp_ss[[i]][[1]]]
        close_grs <- max(grs)/min(grs) <= 1.5
        if (sum(grs-profile_gradients_alt<1)>0 & close_grs){max_fault_width<-max_fault_width*1.5}
      }
      ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
      while (sum(fault_widths)>max_fault_width & sum(okp_ss[[i]][[1]])>1){
        fault_height_change <- abs(tab$start_height[which(okp_ss[[i]][[1]])] - tab$end_height[which(okp_ss[[i]][[1]])])
        fault_heigth_pc     <- fault_height_change/max(fault_height_change)
        fault_too_small     <- fault_heigth_pc < 0.25
        fault_too_small     <- c(fault_too_small[1],fault_too_small[length(fault_too_small)]) # only remove from ends 
        if (sum(fault_too_small)>0){  # If more than 25% less height change
          remove_pl <- c(min(which(okp_ss[[i]][[1]])),max(which(okp_ss[[i]][[1]])))[fault_too_small]
          okp_ss[[i]][[1]][remove_pl] <- FALSE
        }
        if (sum(fault_too_small)==0){
          fault_gradients <- fault_height_change/fault_widths
          least_steep     <- which(fault_gradients==min(fault_gradients))
          # Remove this least steep segement from fault plane 
          remove_pl <- which(okp_ss[[i]][[1]])[least_steep]
          okp_ss[[i]][[1]][remove_pl] <- FALSE
          if (sum(diff(okp_ss[[1]][[1]])==-1)==2){ # If this has created a hole in the fault, remove furthest from centre
            d1 <- abs(profile_half_width - (tab$start_dist_along[okp_ss[[1]][[1]]] + tab$end_dist_along[okp_ss[[1]][[1]]])/2)
            furthest_from_centre <- which(d1==max(d1))
            okp_ss[[i]][[1]][which(okp_ss[[i]][[1]])[furthest_from_centre]] <- FALSE
          }
        }
        fault_widths <- abs(tab$start_dist_along[which(okp_ss[[i]][[1]])] - tab$end_dist_along[which(okp_ss[[i]][[1]])])
      }
    }
    
    
    # If there is one plane make sure it is less than max_fault_width*2 max_fault_width = 35m
    if (sum(okp_ss[[i]][[1]])==1){ # If one plane, can be twice as wide 
      fault_width <- abs(tab$start_dist_along[which(okp_ss[[i]][[1]])] - tab$end_dist_along[which(okp_ss[[i]][[1]])])
      if (fault_width > max_fault_width*2){  # If one plane, can be twice as wide 
        okp_ss[[i]][[1]] <- rep(FALSE,length(okp[[1]][[1]])) 
      }
    }
  }
  
  ## Remove fault segment if it is entirely outside of the nearest 50m either side of the centre
  for (i in 1:length(okp_ss)){
    if (sum(okp_ss[[i]][[1]])>0){
      ng_l <- abs(profile_half_width - tab$start_dist_along[which(okp_ss[[i]][[1]])[1]]) > 50
      ng_r <- abs(profile_half_width - tab$end_dist_along  [which(okp_ss[[i]][[1]])[sum(okp_ss[[i]][[1]])]]) > 50
      if (ng_r & ng_l){  # If more than 25% less height change
        okp_ss[[i]][[1]]<- rep(FALSE,length(okp[[1]][[1]]))
      }
    }
  }
  
  
  
  
  # Refind the unique fault planes 
  fps2                     <- lapply(okp_ss, function(x) x[[1]])
  fault_plane_choices2     <- unique(fps2)
  num_dif_fault_planes2    <- length(fault_plane_choices2)
  okp_ss2 <- vector('list',num_dif_fault_planes2)
  for (i in 1:num_dif_fault_planes2){
    fault_planes2          <- fault_plane_choices2[[i]]
    okp_ss2[[i]][[1]]      <- fault_planes2
    output2                <- !as.logical(sapply(fps2, function(x) sum(1-as.logical(x)==fault_planes2)))
    okp_ss2[[i]][[2]]      <- c(unlist(sapply(okp_ss[output2], function(x) x[[2]])))
  }
  
  # Get rid of list elements if no plane has been found 
  fp_found                <- !sapply(okp_ss2, function(x) sum(x[[1]])==0)
  # Get rid of planes if planes height change is less than 0.2m (resolution of data)
  for (i in which(fp_found)){
    fault_height <-  tab$start_height[okp_ss2[[i]][[1]]][1] - 
      tab$end_height[which(okp_ss2[[i]][[1]])][sum(okp_ss2[[i]][[1]])]
    if (abs(fault_height) < 0.2){  # 0.2m is the resolution of the data
      okp_ss2[[i]][[1]] <- rep(FALSE,length(okp[[1]][[1]])) 
    }
  }
  
  fp_found                <- !sapply(okp_ss2, function(x) sum(x[[1]])==0)
  
  okp_ss2                 <- okp_ss2[fp_found]
  
  return (okp_ss2) 
  
}