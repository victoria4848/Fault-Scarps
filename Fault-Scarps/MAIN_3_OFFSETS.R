# MAIN_3_OFFSETS

# SCRIPT OUTLINE
# 1. Set parameters for finding ok landscape planes 
# 2. Find potential fault planes
# 3. Save results in 'Results/Profiles/'


for (i in i_choices){
  fault_name      <- fault_name_list[i]

    print(paste0('MAIN_2_OFFSETS ',fault_name))
    
    profiles_list   <- readRDS ( paste0 ( 'Results/Profiles/',fault_name,'_prof_dist.RDS') )
    scarp_profiles  <- profiles_list[[2]] # Segmented planes
    seg_prof        <- profiles_list[[4]] # Segmented planes
    scarp_distances <- profiles_list[[3]] # Distances along fault 
    okp_all         <- profiles_list[[6]] # Fault planes found 
    
    n_fp_combos     <- length(okp_all[[1]])  # Will be one for one combo of fault planes 
    
    # 1. ======================= SET PARAMETERS ================================================================== #
    # le              <- max(1, max(scarp_distances$dist_along_fault)/75000)
    # fact_grad       <- 0.5  # 1.5 If higher, planes have to be more similiar gradient, so fewer planes 
    # fact_dis        <- 1*le # If higher, fault ends can be more different from previous average 5 could be 2 for actual metres, so 45 degrees ok. 
    # fact_height     <- 0.25 # If higher, fault ends can be more different. Max profile height dif, times this
    # max_resid       <- 3    # Max residuals from planes to points on line 
    
    le              <- max(1, max(scarp_distances$dist_along_fault)/75000)
    fact_grad       <- 0.01  # 1.5 If higher, planes have to be more similiar gradient, so fewer planes 
    fact_dis        <- 5*le # If higher, fault ends can be more different from previous average 5 could be 2 for actual metres, so 45 degrees ok. 
    fact_height     <- 5 # If higher, fault ends can be more different. Max profile height dif, times this
    max_resid       <- 15    # Max residuals from planes to points on line 
    

    # 1. ======================= FIND LANDSCAPE PLANES =========================================================== #
    # Initialize variables 
    height_offset_list  <- vector('list',n_fp_combos)
    found_planes.df_list<- vector('list',n_fp_combos)
    cen_ind             <- data.frame(centre_l=NA,centre_r=NA,height_l=NA,height_r=NA,i=NA)
    ok_offsets          <- TRUE
    
    for (fn in 1:n_fp_combos){  # To test different variations of fault planes found (HERE 1)
      height_offset_all <- data.frame( prof_ind = 1:length(seg_prof), offset_original= NA ,
                                       confid_dif_l = NA            , confid_dif_r   = NA , 
                                       confid_pl_l  = NA            , confid_pl_r    = NA , confid_s_pl = NA ,
                                       np_found     = NA ) 
      np_found           <- 0 # Number of planes that have been found so far
      g_list             <- list()
      found_planes.df    <- list()
      cen_ind            <- data.frame(centre_l=NA,centre_r=NA,height_l=NA,height_r=NA,i=NA,height_mid_r=NA,height_mid_l=NA)
      ok_centres         <- TRUE # Start off with any location ok, till 5 planes are found 
      ok_heights         <- TRUE
      
      for (i in 1:length(seg_prof)){   # For number of profiles 

        if (i %in% which(!duplicated(scarp_distances$f))){ 
          np_found2      <- 0          # Reset for new fault branch 
          ok_offsets     <- TRUE
        } 
        
        
        if (sum(okp_all[[i]][[fn]]) > 0){   # If a fault plane has been found in this profile
          
          tab           <- seg_prof[[i]][[1]]
          fault_planes  <- tab$plane_id[okp_all[[i]][[fn]]]
          nplanes       <- nrow(tab)
          
          g             <- c(which(okp_all[[i]][[fn]]), max(which(okp_all[[i]][[fn]]))+1) # Fault plane
          p_r           <- c(max(fault_planes)+1, nrow(tab))
          p_l           <- c(1,min(fault_planes)-1)
          
          end_ps_r     <- seg_prof[[i]][[2]][c(p_r[1],p_r[2]+1),]
          end_ps_l     <- seg_prof[[i]][[2]][c(p_l[1],p_l[2]+1),]
          centre_l     <- end_ps_l[2,1]
          centre_r     <- end_ps_r[1,1]
          mid_point    <- mean(c(centre_l,centre_r))
          height_or_l  <- end_ps_l[2,2]
          height_or_r  <- end_ps_r[1,2]
          width_fault  <- centre_r-centre_l
          
          l_new           <-  which(abs(centre_l-20-tab$end_dist_along)  ==min(abs(centre_l-20-tab$end_dist_along)))[1]
          r_new           <-  which(abs(centre_r+20-tab$start_dist_along)==min(abs(centre_r+20-tab$start_dist_along)))[1]
          
          # On crest side, take plane around 10 metres away if there is one, to compensate for sedimentation
          # Can change to make plane start exactly next to fault plane 
          end_ps_r        <- seg_prof[[i]][[2]][c(r_new,p_r[2]+1),]

          end_ps_l        <- seg_prof[[i]][[2]][c(p_l[1],l_new+1),]
          
          grad_r          <- (end_ps_r [2,2] - end_ps_r [1,2]) / (end_ps_r [2,1] - end_ps_r [1,1])
          grad_l          <- (end_ps_l [2,2] - end_ps_l [1,2]) / (end_ps_l [2,1] - end_ps_l [1,1])
          grad_dif        <- abs(grad_l-grad_r)
          max_height_dif  <- max(seg_prof[[i]][[2]][,2])-min(seg_prof[[i]][[2]][,2])
          grad_dif_cutoff <- max_height_dif/400/fact_grad
          
          if (grad_dif    <= grad_dif_cutoff){
            
            height_l      <- end_ps_l[2,2]
            height_r      <- end_ps_r[1,2]
            centre_l      <- end_ps_l[2,1]
            centre_r      <- end_ps_r[1,1]
            
            int_l              <- end_ps_l$.fitted[1]
            int_r              <- end_ps_r$.fitted[1]-end_ps_r$x[1]*grad_r
            
            # For different points, check hight diff from plane at same x values
            tab_l              <- tab[seq(p_l[1],l_new,1),]
            tab_r              <- tab[seq(r_new,p_r[2],1),]
            
            max_wrong_l        <- max(abs(tab_l$end_height-(int_l+grad_l*tab_l$end_dist_along)))
            max_wrong_r        <- max(abs(tab_r$end_height-(int_r+grad_r*tab_r$end_dist_along)))
            pl_ch_r            <- 0
            pl_ch_l            <- 0
            width_plane_l      <-  (end_ps_l$x[2]-end_ps_l$x[1])
            width_plane_r      <-  (end_ps_r$x[2]-end_ps_r$x[1])
            max_width_pl_l     <-  width_plane_l
            max_width_pl_r     <-  width_plane_r
            
            s                  <- 0
            while (max_wrong_r > max_resid & s == 0){ # Make plane shorter, but not too short
              pl_ch_r          <- pl_ch_r + 1
              end_ps_r         <- seg_prof[[i]][[2]][c(r_new,p_r[2]+1-pl_ch_r),] 
              width_plane_r    <- (end_ps_r$x[2]-end_ps_r$x[1])
              
              if (width_plane_r < (max_width_pl_r*2/3)){ # Plane now too narrow
                s               <- 1
                end_ps_r        <- seg_prof[[i]][[2]][c(r_new,p_r[2]+1-pl_ch_r+1),] 
                pl_ch_r         <- pl_ch_r - 1
              }
              
              grad_r           <- (end_ps_r [2,2] - end_ps_r [1,2]) / (end_ps_r [2,1] - end_ps_r [1,1])
              int_r            <- end_ps_r$.fitted[1]-end_ps_r$x[1]*grad_r
              tab_r            <- tab[seq(r_new,p_r[2]-pl_ch_r,1),]
              max_wrong_r      <- max(abs(tab_r$end_height-(int_r+grad_r*tab_r$end_dist_along)))
            }
            
            s                  <- 0
            while (max_wrong_l > max_resid & s == 0){ # Make plane shorter
              pl_ch_l          <- pl_ch_l + 1
              end_ps_l         <- seg_prof[[i]][[2]][c(p_l[1]+pl_ch_l,l_new+1),]
              width_plane_l    <- (end_ps_l$x[2]-end_ps_l$x[1])
              
              if (width_plane_l < (max_width_pl_l*2/3)){ # Plane now too narrow
                s               <- 1
                end_ps_l        <- seg_prof[[i]][[2]][c(p_l[1]+pl_ch_l-1,l_new+1),]
                pl_ch_l         <- pl_ch_l -1
              }
              
              grad_l           <- (end_ps_l [2,2] - end_ps_l [1,2]) / (end_ps_l [2,1] - end_ps_l [1,1])
              int_l            <- end_ps_l$.fitted[1]-end_ps_l$x[1]*grad_l
              tab_l            <- tab[seq(p_l[1]+pl_ch_l,l_new,1),]
              max_wrong_l      <- max(abs(tab_l$end_height-(int_l+grad_l*tab_l$end_dist_along)))
            }
            
            av_grad_planes     <- mean(c(grad_r,grad_l))
            grad_fault         <- (height_or_r - height_or_l) / (centre_r-centre_l)
            confid_dif_l       <- 1-grad_l/grad_fault
            confid_dif_r       <- 1-grad_r/grad_fault
            confid_s_pl        <- abs(grad_r-grad_l)
            
            # Recalculate mid heights 
            centre_l           <- end_ps_l[2,1]
            centre_r           <- end_ps_r[1,1]
            height_l           <- end_ps_l[2,2]
            height_r           <- end_ps_r[1,2]
            
            height_mid_r       <- height_r+grad_r*(mid_point-centre_r)
            height_mid_l       <- height_l+grad_l*(mid_point-centre_l)
            
            if (np_found2  >= 5){  # Five previous found planes - distance and height similar
              gg <- cen_ind [ seq ( np_found-4 , np_found , 1 ) , ]
              av_centre   <- colMeans ( gg )
              av_o        <- mean(abs(gg$height_r - gg$height_l),na.rm=TRUE)
              # Average distance from current profiles * fact_dis i.e. how much offset can change from one profile to the next 
            #  dis         <- max( ( i-av_centre[5])*fact_dis , av_o*0.25 ) # can move at least 0.25 of average over past
              dis         <- max( ( i-av_centre[5])*fact_dis  ) # can move at least 0.25 of average over past
              ok_offsets  <- ( abs ( height_mid_r-height_mid_l) - abs ( av_centre[6] - av_centre[7])) < dis 
            }
       
            ##### Making sure offset found is sensible #####
            # Offset also has to be in the correct direction for the dip of the fault...
            ok_direction     <- sign(height_mid_l-height_mid_r)==sign(height_or_l - height_or_r)
            
            # Offset has to be at least 25% of the height difference of the fault, and fault at least 25% of the offset... 
            ok_h_wrt_fault_h <- (abs(height_mid_l-height_mid_r) > 0.25*abs(height_or_l - height_or_r)) && 
              (abs(height_or_l - height_or_r) > 0.25*abs(height_mid_l-height_mid_r))
            
            # Mid point has to be nearer top height than bottom height 
            ok_mid_height <- abs(height_mid_l - height_or_l) < abs(height_mid_l - height_or_r) &&
              abs(height_mid_r - height_or_r) < abs(height_mid_r - height_or_l) 
 

            if (ok_offsets & ok_direction & ok_h_wrt_fault_h & ok_mid_height){ 

              
              if ( abs ( height_mid_r - height_mid_l ) > 0.2 ){ # minimum offset considered (resolution of data)
                np_found           <- np_found  + 1 
                np_found2          <- np_found2 + 1
                cen_ind[np_found,] <- c(centre_l,centre_r,height_l,height_r,i,height_mid_r,height_mid_l)
                g_list[[np_found]] <- g        # Indexes of fault plane
                
                
                found_planes.df[[np_found]] <- data.frame(x.l=end_ps_l$x,height.l=end_ps_l$.fitted,
                                                          x.r=end_ps_r$x,height.r=end_ps_r$.fitted,i=i,np_found=np_found)
                
                confid_pl_l                  <- 1 - ( max_wrong_l / abs(sqrt(max_height_dif)) ) # make height dif count, but not too much
                confid_pl_r                  <- 1 - ( max_wrong_r / abs(sqrt(max_height_dif)) )
                height_offset_all[i,]        <- c ( i            , abs ( height_mid_r-height_mid_l) ,
                                                    confid_dif_l , confid_dif_r ,
                                                    confid_pl_l  , confid_pl_r  , confid_s_pl,
                                                    np_found     )
              }   # If height change > 0.2
            }   # If offsets, direction, heights etc. are reasonable 
            
          }   # If gradient difference between planes and faults is ok
        }   # If fault has been found for this profile
        
      }    # For number of profiles in fault 
      height_offset_all$dist_along_fault <- scarp_distances$dist_along_fault
      height_offset_all$f                <- scarp_distances$f
      
      ######### Add on lat / lon to height_offset
      height_offset_all$lat <- sapply ( scarp_profiles    , function(x) x[101,2]) # Get middle of profiles i.e. where fault it
      height_offset_all$lon <- sapply ( scarp_profiles    , function(x) x[101,1])
      
      height_offset_list[[fn]]   <- height_offset_all
      found_planes.df_list[[fn]] <- found_planes.df
    }

    
    list_details <- c ( 'details' , 'height_offset_list' , 'found_planes.df_list' )
    saveRDS ( list ( list_details , height_offset_list , found_planes.df_list ) ,
              paste0 ( 'Results/Offset_Planes/' , fault_name , '_offsets_planes.RDS' ))

}


