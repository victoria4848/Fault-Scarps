## Function to find stuff for fault scarp
fault_scarp_app_efficient <- function(dataframe, 
                                      index_prof,
                                      crests,
                                      bases,
                                      slope_cutoff_choices,
                                      curve_cutoff_choices,
                                      steep_in_a_row_choices,
                                      length_choices,
                                      downtoside,
                                      middist,
                                      withindist,
                                      spread1=5,
                                      rat_dif=3,
                                      deriv_lag=2,
                                      min_scarp_height=0.25,
                                      prof_ind){
  
  # Initialize lists 
  st_cur.c2 <- list();  st_cur.c_ <- list();  st_cur.c__ <- list()
  st_cur.b2 <- list();  st_cur.b_ <- list();  st_cur.b__ <- list()
  crestbase <- list(); crestbase_ <- list(); crestbase__ <- list()
  for(a in 1:length(slope_cutoff_choices)) {
    for(b in 1:length(steep_in_a_row_choices)) {
      for(c in 1:length(curve_cutoff_choices)) {
        st_cur.c2[[c]] <- vector()
        st_cur.b2[[c]] <- vector()
        crestbase[[c]] <- vector()
      }
      st_cur.c_[[b]] <- st_cur.c2
      st_cur.b_[[b]] <-st_cur.b2
      crestbase_[[b]] <- crestbase
    }
    st_cur.c__[[a]] <- st_cur.c_
    st_cur.b__[[a]] <- st_cur.b_
    crestbase__[[a]] <- crestbase_
  }
  
  # Find slope and gradient of slope
  deriv1 <- c(diff(dataframe$height) / diff(dataframe$DistTotal),NA)
  deriv2 <- c(rep(NA,ceiling(deriv_lag/2)), diff(deriv1,deriv_lag) / diff(dataframe$DistTotal,deriv_lag),rep(NA,floor(deriv_lag/2)))# this is just for steep slopes
  
  
  ## Find max curvature 
  max_curve.c <- list(); max_curve.b <- list(); 
  # Find points with a high curvature / second derivative
  for (c in 1:length(curve_cutoff_choices)){
    if (downtoside=='left'){
      max_curve.c[[c]] <- which(deriv2 > curve_cutoff_choices[c])+1
      max_curve.b[[c]] <- which(deriv2 < -curve_cutoff_choices[c])+1
    }
    if (downtoside=='right'){
      max_curve.c[[c]] <- which(deriv2 < -curve_cutoff_choices[c])+1
      max_curve.b[[c]] <- which(deriv2 > curve_cutoff_choices[c])+1
    }
  }
  
  
  # Find where slopes are steep - within distance either side of center
  indmid <- which(dataframe$DistTotal > (middist-withindist) & dataframe$DistTotal < (middist+withindist))
  # Find where steep and longer than steep in a row
  for (a in 1:length(slope_cutoff_choices)){
    if (downtoside=='left') {steep1 <- which(deriv1>slope_cutoff_choices[a])}
    if (downtoside=='right'){steep1 <- which(deriv1< -slope_cutoff_choices[a])}
    steep <- intersect(indmid,steep1)
    
    for (b in 1:length(steep_in_a_row_choices)){
      #slope needs to be at least a certain length
      steep.c <- 0
      steep.b <- 0
      if (length(steep) >= steep_in_a_row_choices[b]) {
        steep_inarow <- c(0,which(diff(steep)>1),length(steep))
        lengths_st <- diff(steep_inarow)
        keep_segments <- which(lengths_st > steep_in_a_row_choices[b])
        if (sum(keep_segments) > 0){
          steep_keep <- list()
          height_change <- c()
          cum_sum <- cumsum(lengths_st)
          for (k in 1:length(keep_segments)){
            steep_keep[[k]] <- steep[(cum_sum[keep_segments][k]-lengths_st[keep_segments][k]+1):
                                       cum_sum[keep_segments][k]]
            height_change[k] <-  abs(dataframe$height[steep_keep[[k]][1]]-dataframe$height[steep_keep[[k]][length(steep_keep[[k]])]])
          }
          steep_keep <- steep_keep[height_change>min_scarp_height] #remove segments that have height change less than min scarp height
          steep_keep <- unlist(steep_keep)
          steep.c<- steep_keep-2 #2 meters infront, i is curve, j is steep in a row 
          steep.b<- steep_keep+2 #2 meters behind
        }
      }
      
      # Find where both steep and curved
      for (c in 1:length(curve_cutoff_choices)){
        st_cur.c__[[a]][[b]][[c]] <- intersect(steep.c,max_curve.c[[c]])
        st_cur.b__[[a]][[b]][[c]] <- intersect(steep.b,max_curve.b[[c]])
      }
      
    }
  }
  
  
  ################ LOOK AT PREVIOUS CREST AND BASE ############### 
  r <- 0
  for (a in 1:length(slope_cutoff_choices)){
    for (b in 1:length(steep_in_a_row_choices)){
      for (c in 1:length(curve_cutoff_choices)){
        r <- r+1
        st_cur.c <- 0
        st_cur.b <- 0
        # If a previous crest and base has been found
        if (  (sum(!is.na(crests[0:(index_prof-1),r]))  > 0)  &  (sum(!is.na(bases[0:(index_prof-1),r])) > 0) ){
          
          last_crest.ind <- max(which(!is.na(crests[0:(index_prof-1),r])))
          dif.c <- index_prof-last_crest.ind
          
          crest_choices <- crests[last_crest.ind,r] + seq((-spread1-round(dif.c/rat_dif)),(spread1+round(dif.c/rat_dif)),1)
          st_cur.c <- intersect(st_cur.c__[[a]][[b]][[c]],crest_choices)
          
          dif.b <- index_prof-last_crest.ind
          base_choices <- crests[last_crest.ind,r] + seq((-spread1-round(dif.b/rat_dif)),(spread1+round(dif.b/rat_dif)),1)
          st_cur.b <- intersect(st_cur.b__[[a]][[b]][[c]],base_choices)
          
          #if st_cur.b hasn't been found, try within the crest and base
          if (sum(st_cur.c)==0){
            crest_choices <- seq(min(crests[last_crest.ind,r], bases[last_crest.ind,r]),max(crests[last_crest.ind,r],bases[last_crest.ind,r]), 1)
            st_cur.c <- intersect(st_cur.c__[[a]][[b]][[c]],crest_choices)
          }
          if (sum(st_cur.b)==0){
            base_choices  <- seq(min(crests[last_crest.ind,r], bases[last_crest.ind,r]),max(crests[last_crest.ind,r],bases[last_crest.ind,r]), 1)
            st_cur.b <- intersect(st_cur.b__[[a]][[b]][[c]],base_choices)
          }
          
        }  # Inside here was done if crest/base have been found before
        
        # If no crest of base found before, or can't be found near them, they can be anywhere
        if (sum(st_cur.c)==0) {
          st_cur.c <- st_cur.c__[[a]][[b]][[c]]
        }
        if (sum(st_cur.b)==0){
          st_cur.b <- st_cur.b__[[a]][[b]][[c]]
        }
        
        suppressWarnings(crestbase__[[a]][[b]][[c]] <- c(min(st_cur.c),max(st_cur.b)))
      }
    }
  }
  
  
  ## Output should be crest and base
  conds <- expand.grid(slope_cutoff=slope_cutoff_choices,steep_in_a_row=steep_in_a_row_choices,curve_cutoff=curve_cutoff_choices)
  crest <- unlist(crestbase__)[seq(1,length_choices*2,2)]
  base <- unlist(crestbase__)[seq(2,length_choices*2,2)]
  crest[crest%in%c(-Inf,Inf)] <- NA ; crest[base%in%c(-Inf,Inf)] <- NA ; 
  base[base%in%c(-Inf,Inf)] <- NA ; base[crest%in%c(-Inf,Inf)] <- NA
  # find max slope of fault scarp
  max_gra <- rep(NA, length=length_choices)
  max_slope.ind <- rep(NA, length=length_choices)
  for (i in 1:length(crest)){
    if (!is.na(crest[i]) & !is.na(base[i])){
      max_gra[i] <- max(abs(deriv1[crest[i]:base[i]]))
      max_slope.ind[i] <- which(abs(deriv1)==max_gra[i])
    }
  }
  
  
  crestbase.df <- cbind(conds,crest,base, max_slope.ind,max_gra,prof_ind=rep(index_prof,nrow(conds)))
  crestbase.df$indx=row.names(crestbase.df)
  
  return(crestbase.df)
  
}
