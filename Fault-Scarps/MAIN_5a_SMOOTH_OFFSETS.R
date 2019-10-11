# MAIN_5a_SMOOTH_OFFSETS

# SCRIPT OUTLINE
# 1. Load data
# 2. Add together overlapping strands
# 3. Keep places only where offsets have been found on both overlapping strands 
# 4. Smooth profiles
# 5. Remove smoothed offsets where there is no fault, is a river, or a very low density of offsets found
# 6. Save results in 'Results/Profiles/' and 'Results/Offset_Planes/'


for (i in i_choices){ 
  fault_name        <- fault_name_list[i]
  
  print(paste0('MAIN_5a_SMOOTH_OFFSET ',fault_name))
  
  # 1. ======================= LOAD DATA++++++++ ================================================================= #
  # Data after subsetted by confidence values 
  height_pl_bF_s_list <- readRDS ( paste0 ( 'Results/Offset_Planes/' , fault_name , '_offsets_planes.RDS') )
  height_offset       <- height_pl_bF_s_list[[5]]
  

  
  # 2. ======================= ADD OVERLAPPING STRANDS ============================================================= #
  # These are the same, then where they overlap they become summed or zero. First strand keeps the sum, second gets zero.
  height_offset$offset_CV_strands  <- height_offset$offset_CV
  height_offset$confid_n_strands   <- height_offset$confid_n
  
  ## ADD OVERLAPPING STRANDS 
  # For different branches - if ends overlap, add them together
  if (fault_name=='Pasmajarvi'){ # Remove annoying mini offset branch
    height_offset$offset_CV_strands [which ( height_offset$f == 2 )] <- NA
    height_offset$offset_CV [which ( height_offset$f == 2 )] <- NA
    height_offset$f = 1
  }
  
  un_f  <- unique ( height_offset$f )
  num_f <- length ( un_f)
  ## Make sure f values are in the right order from lower to higher distance
  mini  <- c()
  ind   <- 0
  for ( i in un_f ) {
    ind <- ind + 1
    mini[ind]             <- min ( height_offset$dist_along_fault[ height_offset$f == i] )
  }
  or_f                    <- un_f [ order ( mini ) ]
  height_offset_all       <- height_offset [height_offset$f == or_f [1] , ]
  height_offset_all$f     <- 1
  if (num_f > 1){
    for ( i in 2:num_f ){
      height_offset_all_a   <- height_offset [height_offset$f == or_f [i] , ]
      height_offset_all_a$f <- i
      height_offset_all     <- rbind ( height_offset_all , height_offset_all_a )
    }
  }
  height_offset           <- height_offset_all
  
  
  # 3. ======================= KEEP WHERE OFFSETS HAVE BEEN FOUND ===================================================== #
  # Subset where offsets have been found and kept
  overlap <- c()
  if ( num_f > 1 ){
    for (j in 1:( num_f - 1 )) {
      overlap[j] <- max ( subset ( height_offset_all , f == j , dist_along_fault )) > min ( subset ( height_offset_all , f == ( j + 1 ) , dist_along_fault))
    }
    if ( sum ( overlap ) > 0 ){ # If there are overlapping segments 
      overlap_segments <- which ( overlap == TRUE )
      for (j in overlap_segments){  # Find indices where they overlap
        p1         <- subset ( height_offset_all , f == j )
        p1_maxd    <- max    ( p1$dist_along_fault)
        p2         <- subset ( height_offset_all , f == ( j + 1 ))
        p2_mind    <- min    ( p2$dist_along_fault )
        p2_maxd    <- max    ( p2$dist_along_fault )
        p2_overlap <- subset ( p2 , p2$dist_along_fault <= p1_maxd )  # Where second strand overlaps 
        p1_overlap <- subset ( p1 , p1$dist_along_fault >= p2_mind & p1$dist_along_fault <= p2_maxd ) # Where first strand overlaps 
        
        # Must both be overlapping
        if ( nrow ( p1_overlap ) > 0 & nrow ( p2_overlap ) > 0 ){
          
          ## Discard measurements if not within 4m of each other, and add p2 overlap to the nearest distance of p1
          all_combos2 <-  expand.grid ( p1_overlap$dist_along_fault , p2_overlap$dist_along_fault )
          all_combos1 <-  expand.grid ( p2_overlap$dist_along_fault , p1_overlap$dist_along_fault )
          differ1     <-  abs         ( all_combos1$Var2 - all_combos1$Var1 )
          differ2     <-  abs         ( all_combos2$Var2 - all_combos2$Var1 )
          keep_good1  <- c()
          keep_good2  <- c()
          for (i in 1:nrow(p1_overlap)){
            keep_good1[i] <- sum ( differ1 [ seq( ((i-1) * nrow ( p2_overlap ) + 1 ) , ( i * nrow ( p2_overlap )) , 1 )] <= overlap_dist_within_m)
          }
          for (i in 1:nrow(p2_overlap)){
            keep_good2[i] <- sum ( differ2 [ seq( ((i-1) * nrow ( p1_overlap ) + 1 ) , ( i * nrow ( p1_overlap )) , 1 )] <= overlap_dist_within_m)
          }
          
          p1_overlap_keep <- p1_overlap [ as.logical ( keep_good1 ) , ]  # These are profiles that will be summed/averaged 
          p2_overlap_keep <- p2_overlap [ as.logical ( keep_good2 ) , ]  # These are profiles that will be summed/averaged 
          
          if ( nrow ( p1_overlap_keep ) > 0 ) {
            offset_CV_strands <- c()
            confid_n_strands <- c()
            for (i in 1:nrow ( p1_overlap_keep )) {
              dista              <- abs   ( p1_overlap_keep$dist_along_fault[i] - p2_overlap_keep$dist_along_fault )
              w                  <- which ( dista == min ( dista )) [1] # which is the closest second strand to first strand
              # SUM OFFSETS and average Confidnce Values
              ## Take one if there are still values there, don't delete them all
              offset_CV_strands[i] <-  p1_overlap_keep$offset_CV[i] + p2_overlap_keep$offset_CV[w]
              confid_n_strands[i]  <- (p1_overlap_keep$confid_n[i] + p2_overlap_keep$confid_n[w]) / 2
              if (is.na(offset_CV_strands[i])){
                offset_CV_strands[i] <-  p1_overlap_keep$offset_CV[i] 
                confid_n_strands[i]  <- p1_overlap_keep$confid_n[i] 
              }
              if (is.na(offset_CV_strands[i])){   # if it is still na.... 
                offset_CV_strands[i] <-  p2_overlap_keep$offset_CV[w]
                confid_n_strands[i]  <-  p2_overlap_keep$confid_n[w]
              }
            }
            
            for ( r in 1:nrow ( p1_overlap_keep )) {  # For first strand, get summed / mean results 
              ind                                  <- p1_overlap_keep$prof_ind  [r]
              height_offset$offset_CV_strands[ind] <- offset_CV_strands[r]
              height_offset$confid_n_strands[ind] <- confid_n_strands [r]
            }
            for ( r in 1:nrow ( p2_overlap_keep )) {
              ind                                  <- p2_overlap_keep$prof_ind [r]
              height_offset$offset_CV_strands[ind] <- 0 # Remove sum from second strand, do don't count twice 
              height_offset$confid_n_strands[ind] <- 0
            }
            
          } # If nrow to keep is more than zero
        } # If both overlapping
      } # For number of overlapping segments 
    }
  }
  
  ##########################################
  
  
  
  
  
  # 4. ======================= SMOOTH PROFILES ==================================================================== #
  # SMOOTH PROFILES  (weighted by confidence values)
  # Smoothing is weighted by confidence values, so if you make them more different, the convidence values have more affect
  height_offset$confid_nS_strands <- height_offset$confid_n_strands * smoothing_CV_weight 
  height_offset$confid_nS         <- height_offset$confid_n         * smoothing_CV_weight 
  
  span1      <- 250 / max ( height_offset$dist_along_fault ) # Should be about 0.1 for short faults and lower for larger faults 
  span2      <- 2.5 * span1
  loessModA  <- loess(offset_CV_strands ~ dist_along_fault, data = height_offset , weights=confid_nS_strands, span=span1) # short smoothing span
  loessModB  <- loess(offset_CV_strands ~ dist_along_fault, data = height_offset , weights=confid_nS_strands, span=span2) # longer smoothing span
  loessMod10 <- loess(offset_CV_strands ~ dist_along_fault, data = height_offset , weights=confid_nS_strands, span=0.1) # 10% smoothing span
  loessMod25 <- loess(offset_CV_strands ~ dist_along_fault, data = height_offset , weights=confid_nS_strands, span=0.25) # 25% smoothing span
  loessMod50 <- loess(offset_CV_strands ~ dist_along_fault, data = height_offset , weights=confid_nS_strands, span=0.50) # 50% smoothing span
  # get smoothed output
  height_offset$smA [!is.na(height_offset$offset_CV_strands)] <- predict(loessModA ) 
  height_offset$smB [!is.na(height_offset$offset_CV_strands)] <- predict(loessModB ) 
  height_offset$sm10[!is.na(height_offset$offset_CV_strands)] <- predict(loessMod10) 
  height_offset$sm25[!is.na(height_offset$offset_CV_strands)] <- predict(loessMod25) 
  height_offset$sm50[!is.na(height_offset$offset_CV_strands)] <- predict(loessMod50) 
  
  
  # 5. ======================= REMOVE NO FAULT, RIVER, LOW DENSITY ================================================== #
  # REMOVE where NO FAULT, RIVER, LOW DENSITY
  height_offset$gr <- 0 # Need to change groups so that smoothing lines have gaps in them
  noprof_dist      <- 0 # Find distances where no profiles have been found e.g. rivers, no scarp, low density 
  
 # River
  river_dist <- 0
  if (file.exists(paste0(main_data_dir , '/' , rivers_dir , '_R/' , fault_name , '_rivers.RDS'))){
   scarp_rivers <- readRDS(paste0(main_data_dir , '/' , rivers_dir , '_R/' , fault_name , '_rivers.RDS'))
     for ( i in 1:max ( scarp_rivers$f )){
      s_riv.ss          <- subset ( scarp_rivers,f==i)
      gr_ind4           <- which  ( height_offset$dist_along_fault >= max(s_riv.ss$distance))
      height_offset$gr[gr_ind4] <- height_offset$gr[gr_ind4]+121212
      gr_ind1           <- which  ( height_offset$dist_along_fault   >= min ( s_riv.ss$distance) &
                                      height_offset$dist_along_fault <= max ( s_riv.ss$distance))
      if ( length (gr_ind1) > 0 ){
        height_offset$offset_CV[gr_ind1]   <- NA
        height_offset$offset_CV_strands[gr_ind1]   <- NA
      }
      river_dist[i]     <- max ( s_riv.ss$distance ) - min ( s_riv.ss$distance ) # to find distance of river
    }
  }
  
  found <- !is.na(height_offset$offset_CV_strands)
  height_offset$dist_along_fault_found        <- NA
  height_offset$dist_along_fault_found[found] <- height_offset$dist_along_fault[found]
  
  # Point density (is less than 5 per 1000 meters over 500m and 250m average) 
  for (i in 1:nrow ( height_offset )){
    height_offset$point.density[i]  <- nrow ( subset ( height_offset , ( dist_along_fault_found < height_offset$dist_along_fault_found[i] + 500 ) & ( dist_along_fault_found > height_offset$dist_along_fault_found[i ] - 500)))
  }
  filt           <- height_offset$point.density <= min_pdensity & height_offset$point.density > 0 # If zero, profile not found 
  change_line_gr <- which (filt ) [ which ( diff ( which ( filt )) > 1 )]
  change_line_gr <- c ( which ( filt ) [1] , change_line_gr , which ( filt ) [ length ( which ( filt ))])
  
  if ( length ( change_line_gr ) > 0 & sum ( is.na ( change_line_gr )) == 0 ){
    height_offset$gr [1:change_line_gr[1]] <- height_offset$gr [ 1:change_line_gr[1] ] + change_line_gr [1]
    if ( length ( change_line_gr ) > 1){
      for (i in 1: ( length ( change_line_gr ) -1 )){
        height_offset$gr [ change_line_gr[i] : change_line_gr [i+1] ]  <- height_offset$gr [change_line_gr[i] : change_line_gr[i+1] ] + change_line_gr[i] + 5
      }
    }
  }
  rm_pd <- which ( filt ) # filt is where density of points is less than the density required 
  if ( length ( rm_pd ) > 0 ){ height_offset$offset_CV_strands[rm_pd] <- 0 } # Remove points with too low a point density
  
  ## GAPS IN SMOOTH LINE : Make gaps in smooth lines if there are gaps in points of more than 50 metres 
  gaps2                   <- which ( diff ( sort ( height_offset$dist_along_fault_found )) > line_gap_m ) # If gaps are more than 50 metres, break smooth line
  distances_start         <- sort  ( height_offset$dist_along_fault_found)[gaps2]
  distances_end           <- sort  ( height_offset$dist_along_fault_found)[gaps2+1]
  ifelse(length(gaps2) != 0 ,
         scarp_noprofile2 <- data.frame(distance = c ( distances_start , distances_end) , f = rep ( seq ( 1 , length(distances_start) , 1 ) , 2), height = 0 ), 
         scarp_noprofile2 <- data.frame(distance = c ( 0,0 ) ,                            f = c ( 0,0 ) ,                                         height = c ( 0,0 )))
  # scarp_noprofile
  if ( scarp_noprofile2$f [1] > 0 ){
    for (i in 1: max ( scarp_noprofile2$f )){
      if ( i == 1 ){
        gr_ind1 <- which ( height_offset$dist_along_fault <= min ( scarp_noprofile2$distance [ scarp_noprofile2$f==i ]))
        gr_ind2 <- which ( height_offset$dist_along_fault >= max ( scarp_noprofile2$distance [ scarp_noprofile2$f==i ]))
        height_offset$gr [gr_ind1] <- height_offset$gr [ gr_ind1 ] + i
        height_offset$gr [gr_ind2] <- height_offset$gr [ gr_ind2 ] + i + 200   # Add on random number to make sure not the same as before 
      }
      if ( i > 1 ){
        gr_ind3 <- which ( height_offset$dist_along_fault >= max ( scarp_noprofile2$distance [ scarp_noprofile2$f==i ] ))
        height_offset$gr[gr_ind3] <-  height_offset$gr [ gr_ind3 ] + i + 200      # Add on random number to make sure not the same as before 
      }
      noprof_dist[i] <- max ( scarp_noprofile2$distance [ scarp_noprofile2$f==i ] ) - min ( scarp_noprofile2$distance [ scarp_noprofile2$f == i ])
    }
  }
  ##########################################
  
  # 6. ======================= SAVE RESULTS ======================================================================== #
  
  height_offset$d_km          <- height_offset$dist_along_fault / 1000  # Create distance in km 
  height_pl_bF_s_list[[8]]    <- height_offset    # Save now with smoothed profiles 
  height_pl_bF_s_list[[1]][8] <- 'Height_offset_with_strands'
  
  # Save Smoothed Profiles 
  saveRDS ( height_pl_bF_s_list ,  paste0 ( 'Results/Offset_Planes/' , fault_name , '_offsets_planes.RDS'  ))
  saveRDS ( scarp_noprofile2     , paste0 ( 'Results/Profiles/'      , fault_name , '_scarp_noprofile.RDS' ))
  
  
}