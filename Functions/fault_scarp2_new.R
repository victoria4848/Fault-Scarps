## Function to find stuff for fault scarp
fault_scarp2_new <- function(i,dataframe, res,downtoside,
                             plane_width_ind,
                             middist,withindist,
                             slope_cutoff=0.05,
                             #curve_cutoff=0.01,
                             curve_cutoff=0.006,
                             spread=5,
                             rat_dif=3,
                             deriv_lag=2,
                             steep_in_a_row=2,
                             param_name,
                             min_scarp_height=0.25){
  # rat_dif is how many indices crest/base index can change
  # repno is the number of steep slopes in a row i.e. similar to min width of fault plane
  # av_steep is the average steepness within steep_in_a_row to make scarp
  # spread is how far apart the next crest/base can be
  #smoothing=5;deriv_lag=4;steep_in_a_row=6;repno=8;av_steep=1.5;
  #slope_cutoff=0.02;curve_cutoff=-0.03;plane_width_ind=50
  
  # save parameters
  if (i==1){
    params <- c(spread,deriv_lag,steep_in_a_row,
                slope_cutoff,curve_cutoff,plane_width_ind,middist,withindist,rat_dif)
    saveRDS(params,paste0('Scandi_para/',param_name,'.RDS'))
  }
  # original
  fault.scarp <- data.frame(dist_along_fault=NA,
                            crest=NA,base=NA,
                            height=NA, height2=NA, 
                            width=NA, widthmin=NA,
                            slope=NA,slopemax=NA)
  
  
  # Find slope and gradient of slope
  # deriv1 <- c(rep(NA,ceiling(deriv_lag/2)), diff(dataframe$height,deriv_lag) / diff(dataframe$DistTotal,deriv_lag),rep(NA,floor(deriv_lag/2)))
  #  deriv2 <- diff(deriv1) / diff(dataframe$DistTotal) # this is just for steep slopes
  deriv1 <- c(diff(dataframe$SMA1) / diff(dataframe$DistTotal),NA)
  deriv2 <- c(rep(NA,ceiling(deriv_lag/2)), diff(deriv1,deriv_lag) / diff(dataframe$DistTotal,deriv_lag),rep(NA,floor(deriv_lag/2)))# this is just for steep slopes
  #dataframe$deriv2.av <- c(rep(NA,ceiling(deriv_lag/2)),second_d,rep(NA,floor(deriv_lag/2)))
  dataframe$deriv2.av <- c(deriv2)
  dataframe$deriv1 <- deriv1 
  
  # Find where slopes are steep - within distance either side of center
  indmid <- which(dataframe$DistTotal > (middist-withindist) & dataframe$DistTotal < (middist+withindist))
  if (downtoside=='left') {steep <- which(dataframe$deriv1>slope_cutoff)}
  if (downtoside=='right'){steep <- which(dataframe$deriv1< -slope_cutoff)}
  steep <- intersect(indmid,steep)
  #slope needs to be at least a certain length
  if (length(steep) > steep_in_a_row) {
    steep_inarow <- c(0,which(diff(steep)>1),length(steep))
    lengths_st <- diff(steep_inarow)
    keep_segments <- which(lengths_st > steep_in_a_row)
    if (sum(keep_segments) > 0){
      steep_keep <- list()
      height_change <- c()
      cum_sum <- cumsum(lengths_st)
      for (k in 1:length(keep_segments)){
        steep_keep[[k]] <- steep[(cum_sum[keep_segments][k]-lengths_st[keep_segments][k]+1):
                                   cum_sum[keep_segments][k]]
        height_change[k] <-  abs(dataframe$SMA1[steep_keep[[k]][1]]-dataframe$SMA1[steep_keep[[k]][length(steep_keep[[k]])]])
      }
      steep_keep <- steep_keep[height_change>min_scarp_height]
      steep_keep <- unlist(steep_keep)
      # Remove segments that are less than 50cm height difference apart
      dataframe$height[steep_keep]
      steep.c <- steep_keep-2 #2 meters infront
      steep.b <- steep_keep+2 #2 meters behind
      
      
      # Find points with a high curvature / second derivative
      if (downtoside=='left'){
        max_curve.c <- which(dataframe$deriv2.av > curve_cutoff)+1
        max_curve.b <- which(dataframe$deriv2.av < -curve_cutoff)+1
      }
      
      if (downtoside=='right'){
        max_curve.c <- which(dataframe$deriv2.av < -curve_cutoff)+1
        max_curve.b <- which(dataframe$deriv2.av > curve_cutoff)+1
      }
      # Find where steep and curved 
      st_cur.c <- intersect(steep.c,max_curve.c)
      st_cur.b <- intersect(steep.b,max_curve.b)
      
      ## Constrain to be near crest and base if found previously 
      if (sum(!is.na(res[0:(i-1),]$crest))>0) {
        last_crest.ind <- max(which(!is.na(res[0:(i-1),]$crest)))
        dif.c <- i-last_crest.ind
        crest_choices <- res[last_crest.ind,]$crest + seq((-spread-round(dif.c/rat_dif)),(spread+round(dif.c/rat_dif)),1)
        st_cur.c <- intersect(st_cur.c,crest_choices)
        
        last_base.ind <- max(which(!is.na(res[0:(i-1),]$base)))
        dif.b <- i-last_base.ind
        base_choices <- res[last_base.ind,]$base + seq((-spread-round(dif.b/rat_dif)),(spread+round(dif.b/rat_dif)),1)
        st_cur.b <- intersect(st_cur.b,base_choices)
      }
      # Test if there are two breaks in slope - i.e. two subscarps, so find in the middle
      if (sum(st_cur.b)==0 | sum(st_cur.c)==0){
        crest_choices <- seq(min(res[last_crest.ind,]$crest, res[last_base.ind,]$base),max(res[last_crest.ind,]$crest, res[last_base.ind,]$base), 1) 
        st_cur.c <- intersect(st_cur.c,crest_choices)
        base_choices <- seq(min(res[last_crest.ind,]$crest, res[last_base.ind,]$base),max(res[last_crest.ind,]$crest, res[last_base.ind,]$base), 1)
        st_cur.b <- intersect(st_cur.b,base_choices)
      }
      # If no crest or base has been found yet, try anywhere 
      if (sum(st_cur.b)==0 | sum(st_cur.c)==0){
        st_cur.c <- intersect(steep.c,max_curve.c)
        st_cur.b <- intersect(steep.b,max_curve.b)
      }
      
      # Find crest and base
      crest <- min(st_cur.c)
      base  <- max(st_cur.b) 
      
      # Don't do the rest if base is on the wrong side of crest, and must be 2 long
      if (base - crest > 2) {
        
        # Find height, slope, widths
        if (crest!='Inf' & crest!='-Inf' & base!='Inf' & base!='-Inf' ){
          ####### FIND SLOPES ON EITHER SIDE #######
          # plane1 <- lsfit(dataframe$DistTotal[(crest-plane_width_ind):(crest)],dataframe$height[(crest-plane_width_ind):(crest)])
          plane1 <- lsfit(dataframe$DistTotal[0:crest],dataframe$height[0:crest])
          int1 <- plane1$coefficients[1]
          gra1 <- plane1$coefficients[2]
          plane1.df <- data.frame(dist=c(0,dataframe$DistTotal[crest]),
                                  height = c(int1,int1+gra1*dataframe$DistTotal[crest]))
          
          plane2 <- lsfit(dataframe$DistTotal[base:nrow(dataframe)],dataframe$height[base:nrow(dataframe)])
          int2 <- plane2$coefficients[1]
          gra2 <- plane2$coefficients[2]
          plane2.df <- data.frame(dist=c(dataframe$DistTotal[base],dataframe$DistTotal[nrow(dataframe)]),
                                  height = c(int2+gra2*(dataframe$DistTotal[base]),
                                             int2+gra2*(dataframe$DistTotal[nrow(dataframe)])))
          
          
          # find max slope of fault scarp
          max_slope.ind <- which(abs(dataframe$deriv1)==max(abs(dataframe$deriv1[crest:base])))
          # find height value of plane1
          plane1.height <- int1+gra1*dataframe$DistTotal[max_slope.ind]
          # find height value of plane2
          plane2.height <- int2+gra2*dataframe$DistTotal[max_slope.ind]
          # FAULT SCARP HEIGHT
          fs.offset_height <- as.numeric(abs(plane1.height-plane2.height))
          # FAULT SCARP WIDTH
          fs.width <- abs(dataframe$DistTotal[crest]-dataframe$DistTotal[base])
          # FAULT SCARP SLOPE
          fs.slope <- atan(fs.offset_height/fs.width)*180/pi
          fs.slope.max <- atan(max(abs(dataframe$deriv1[crest:base])))*180/pi
          gra.max <- max(abs(dataframe$deriv1[crest:base]))
          # slope line
          c.int <- dataframe$height[max_slope.ind] + gra.max * dataframe$DistTotal[max_slope.ind]
          
          # Find values from projecting max slope to intersect either side slopes
          top1 <- line.line.intersection(c(plane1.df$dist[1],plane1.df$height[1]),
                                         c(plane1.df$dist[2],plane1.df$height[2]),
                                         c(0,c.int),
                                         c(dataframe$DistTotal[nrow(dataframe)],c.int-gra.max*dataframe$DistTotal[nrow(dataframe)]))
          top2 <- line.line.intersection(c(plane2.df$dist[1],plane2.df$height[1]),
                                         c(plane2.df$dist[2],plane2.df$height[2]),
                                         c(0,c.int),
                                         c(dataframe$DistTotal[nrow(dataframe)],c.int-gra.max*dataframe$DistTotal[nrow(dataframe)]))
          # MIN WIDTH and HEIGHT 2
          fs.width.min <- abs(top1-top2)[1]
          fs.offset_height.2  <- abs(top1-top2)[2]
          
          if (downtoside=='left'){crest1=base; base=crest;crest=crest1}
          
          fault.scarp <- data.frame(dist_along_fault=i,crest=crest,base=base,
                                    offset_height=fs.offset_height, offset_height2=fs.offset_height.2, 
                                    width=fs.width, widthmin=fs.width.min,
                                    slope=fs.slope,slopemax=fs.slope.max)
          
        }
      }
    }
  }
  
  return(fault.scarp)
  
}