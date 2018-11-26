offset_from_crest_base <- function(crest_base,profiles, exclude){
  
  
  #exclude is exclude this many points either side of crest and base to find the planes, incase crest and base were found wrongly
  test_plwi <- seq(round(nrow(profiles)/16),round(nrow(profiles)/2),round(nrow(profiles)/16))
  
  cr_ba_ss <- subset(crest_base,select=c(crest,base,max_slope.ind, max_gra))
  cr_ba_in <- unique(cr_ba_ss)
  results<-data.frame(offset_height=rep(NA,nrow(crest_base)),offset_height2=NA,width=NA,widthmin=NA,slope=NA,slopemax=NA)
  crest_base2 <- cbind(crest_base,results)
  
  # Do if profile is not full of NA values 
  if(sum(is.na(profiles$height))<20){
    
    # Find crest and base offsets, but exclude NA values of crest/base
    cr_ba_in <- na.omit(cr_ba_in)
    if (nrow(cr_ba_in)>0){
      
      for (i in 1:nrow(cr_ba_in)){
        cr_ind <- cr_ba_in$crest[i]
        ba_ind <- cr_ba_in$base[i]
        ms_ind <- cr_ba_in$max_slope.ind[i]
        max_gra <- cr_ba_in$max_gra[i]
        
        ind <- 0
        plane1.residuals <- c()
        for (plwi in test_plwi) {
          if ((cr_ind-exclude) > plwi){
            ind <- ind+1
            plane1 <- lsfit(profiles$DistTotal[(cr_ind-plwi):(cr_ind-exclude)],profiles$height[(cr_ind-plwi):(cr_ind-exclude)])
            plane1.residuals[ind] <- sqrt(mean(plane1$residuals^2))
          }
        }
        plwi_f <- test_plwi[which(plane1.residuals==min(plane1.residuals,na.rm=TRUE))]  #best plane 
        plane1 <- lsfit(profiles$DistTotal[(cr_ind-plwi_f):(cr_ind-exclude)],profiles$height[(cr_ind-plwi_f):(cr_ind-exclude)])
        int1 <- plane1$coefficients[1]
        gra1 <- plane1$coefficients[2]
        plane1.df <- data.frame(dist=c(profiles$DistTotal[(cr_ind-plwi_f)],profiles$DistTotal[(cr_ind-exclude)]),
                                height = c(int1+gra1*profiles$DistTotal[(cr_ind-plwi_f)],int1+gra1*profiles$DistTotal[(cr_ind-exclude)]))
        
        ind <- 0
        plane2.residuals <- c()
        for (plwi in test_plwi) {
          if ((nrow(profiles)-(ba_ind+exclude)) > plwi){
            ind <- ind+1
            plane2 <- lsfit(profiles$DistTotal[(ba_ind+exclude):(ba_ind+plwi)],profiles$height[(ba_ind+exclude):(ba_ind+plwi)])
            plane2.residuals[ind] <- sqrt(mean(plane2$residuals^2))
          }
        }
        plwi_f <- test_plwi[which(plane2.residuals==min(plane2.residuals,na.rm=TRUE))]  #best plane 
        plane2 <- lsfit(profiles$DistTotal[(ba_ind+exclude):(ba_ind+plwi_f)],profiles$height[(ba_ind+exclude):(ba_ind+plwi_f)])
        int2 <- plane2$coefficients[1]
        gra2 <- plane2$coefficients[2]
        plane2.df <- data.frame(dist=c(profiles$DistTotal[(ba_ind+exclude)],profiles$DistTotal[(ba_ind+plwi_f)]),
                                height = c(int2+gra2*(profiles$DistTotal[(ba_ind+exclude)]),
                                           int2+gra2*(profiles$DistTotal[(ba_ind+plwi_f)])))
        
        # Find heights of planes at middle of scarp aka maximum slope
        # find height value of plane1  
        plane1.height <- int1+gra1*profiles$DistTotal[ms_ind]
        # find height value of plane2
        plane2.height <- int2+gra2*profiles$DistTotal[ms_ind]
        
        # FAULT SCARP HEIGHT
        fs.offset_height <- as.numeric(abs(plane1.height-plane2.height))
        # FAULT SCARP WIDTH
        fs.width <- abs(profiles$DistTotal[cr_ind]-profiles$DistTotal[ba_ind])
        # FAULT SCARP SLOPE
        fs.slope <- atan(fs.offset_height/fs.width)*180/pi
        
        
        # slope line
        fs.slope.max <- atan(max_gra)*180/pi
        c.int <- profiles$height[ms_ind] + max_gra * profiles$DistTotal[ms_ind]
        
        # Find values from projecting max slope to intersect either side slopes
        top1 <- line.line.intersection(c(plane1.df$dist[1],plane1.df$height[1]),
                                       c(plane1.df$dist[2],plane1.df$height[2]),
                                       c(0,c.int),
                                       c(profiles$DistTotal[nrow(profiles)],c.int-max_gra*profiles$DistTotal[nrow(profiles)]))
        top2 <- line.line.intersection(c(plane2.df$dist[1],plane2.df$height[1]),
                                       c(plane2.df$dist[2],plane2.df$height[2]),
                                       c(0,c.int),
                                       c(profiles$DistTotal[nrow(profiles)],c.int-max_gra*profiles$DistTotal[nrow(profiles)]))
        # MIN WIDTH and HEIGHT 2
        fs.width.min <- abs(top1-top2)[1]
        fs.offset_height.2  <- abs(top1-top2)[2]
        
        
        results[i,] <-  c(fs.offset_height, fs.offset_height.2, 
                          fs.width, fs.width.min,
                          fs.slope,fs.slope.max)
      }
      
      for (j in 1:nrow(crest_base)){
        if (isTRUE(all.equal(cr_ba_ss[j,],cr_ba_in[i,],check.attributes = FALSE))){
          crest_base2[j,] <- cbind(crest_base[j,],  results[i,])
        }
      }
      
    }
  }
  
  
  return(crest_base2)
}

