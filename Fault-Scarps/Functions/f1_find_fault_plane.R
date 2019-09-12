f1_find_fault_plane <- function(tab,high,mid,min_fp_grad,downtoside){
  
  ############################################################################################
  ## Find which planes are fault plane ##
  
  # Must be a certain steepness 
  if (downtoside == 'left') {steep <- tab$gradients > min_fp_grad}
  if (downtoside == 'right'){steep <- tab$gradients < -min_fp_grad}
  
  # Must be all of these things 
  okp <- as.logical(mid*steep*high)

  return(okp)
}


  
 
    
    
    
    # ok <- c(okp,0)+c(0,okp)
    # ok[ok>1] <- 1
    # ok <- as.logical(ok)



# ind.sig.left <- 1
# ind.con.left <- 1
# ind.dis.left <- 1
# ind.sig.right <- 1
# ind.con.right <- 1
# ind.dis.right <- 1
# 
# 
# #min_curvature <- 0.02 # between two planes this is set lower down as 0.05
# dista <- profile_half_width/20
# 
# # For finding planes for height
# min_plane_width<- profile_half_width/5   #50
# #max_resid <- profile_height_change/max_resid_multi*nplanes/20
# max_resid_multi <- 200
# max_resid_min <- 0.08  #0.1 #0.07#0.01  # For fitting planes
# cf <- 1.5  #How much more residuals concave planes can have (cos they're concave... not straight)
# #max_grad <- av_grad*max_grad_multi  #0.01  # For fitted planes
# max_grad_multi <- 2  #2
# min_max_grad <- 0.01

