f2_find_fault_plane <- function(tab,high,mid,min_fp_grad,downtoside){
  
  ############################################################################################
  ## Find which planes are fault plane ##
  
  # Must be a certain steepness 
  if (downtoside == 'left') {steep <- tab$gradients > min_fp_grad}
  if (downtoside == 'right'){steep <- tab$gradients < -min_fp_grad}
  
  # Must be all of these things 
  okp <- as.logical(mid*steep*high)

  return(okp)
}


  

