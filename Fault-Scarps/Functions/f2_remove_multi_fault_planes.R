## Remove multi segment fault planes
f2_remove_multi_fault_planes <- function(tab,okp,profile_half_width,num_fault_plane_sections){
  ############################################################################################

  ## Remove fault segments if there are multiple unattached ones  ##
  crest_mul              <- which(diff(okp)==1)+1
  base_mul               <- which(diff(okp)==-1)
  fault_plane_inds_multi <- vector('list',num_fault_plane_sections)
  dist_from_centre       <- numeric(num_fault_plane_sections)
  fault_height           <- numeric(num_fault_plane_sections)
  
  # If one plane is more than 1.5 times higher, take this #
  
  
  # Find distances from centre for each potential fault section
  for (i in 1:num_fault_plane_sections){
    fault_plane_inds_multi[[i]] <- seq(crest_mul[i], base_mul[i],1)
    dist_from_centre[i]         <- abs(profile_half_width-
                                         mean(c(tab[crest_mul[i],'start_dist_along'],tab[base_mul[i],'end_dist_along'])))
    fault_height[i]             <- abs(tab$start_height[crest_mul[i]] - tab$end_height[base_mul[i]])
  } 
  
  # First check is one plane is more central
  ok_dist    <- dist_from_centre < 50
  if (sum(ok_dist)>0){
    crest_mul    <- crest_mul   [which(ok_dist)]
    base_mul     <- base_mul    [which(ok_dist)]
    fault_height <- fault_height[which(ok_dist)]
      if  (length(crest_mul)==1){
        okp <- rep(FALSE, length(okp))
        okp[crest_mul:base_mul] <- TRUE
      }
  }
  
  # Then exclude planes which are a lot smaller 
  min_fault_height <- max(fault_height)*0.66
  
  ok_fault_h <- fault_height > min_fault_height
  crest_mul  <- crest_mul[which(ok_fault_h)]
  base_mul   <- base_mul [which(ok_fault_h)]
  if  (length(crest_mul)==1){
    okp <- rep(FALSE, length(okp))
    okp[crest_mul:base_mul] <- TRUE
  }
  
  while (length(crest_mul)>1){
    if (downtoside=='left'){
      #    if (tab[crest_mul,'start_height'][i] > tab[base_mul,'end_height'][i+1]){
      donotkeep        <- which(dist_from_centre[c(1,2)]==max(dist_from_centre[c(1,2)]))
      suppressWarnings(okp[crest_mul[donotkeep]:base_mul[donotkeep]] <- FALSE)
      #  ok[crest_mul[donotkeep]:(base_mul[donotkeep]+1)] <- FALSE
      crest_mul        <-crest_mul[-donotkeep]
      base_mul         <-base_mul[-donotkeep]
      dist_from_centre <-dist_from_centre[-donotkeep]
      #   }
    }
    
    if (downtoside=='right'){
      #  if (tab[crest_mul,'start_height'][i] < tab[base_mul,'end_height'][i+1]){
      donotkeep        <-which(dist_from_centre[c(1,2)]==max(dist_from_centre[c(1,2)]))
      okp[crest_mul[donotkeep]:base_mul[donotkeep]] <- FALSE
      #  ok[crest_mul[donotkeep]:(base_mul[donotkeep]+1)] <- FALSE
      crest_mul        <-crest_mul[-donotkeep]
      base_mul         <-base_mul[-donotkeep]
      dist_from_centre <-dist_from_centre[-donotkeep]
      #   }
    }
  } # End of while crest_mul > 1
  
  return(okp)
  
}