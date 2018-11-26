dist4plot_UTM <- function(linename, movav=1,movav2=1,movav3=1){
  
  DistTotal <- 0
  for(i in 2:length(linename$lon)) {
    DistTotal[i] = spDistsN1(as.matrix(linename[i,c("lon", "lat")]),
                             c(linename$lon[1], linename$lat[1])) 
  }
  linename$DistTotal <- DistTotal
  
  too_close <- which(diff(linename$DistTotal)<1)
  if ((sum(too_close) > 0)){
    linename <- linename[-too_close, ]  # remove points that are too close in distance
  }
  
  if (movav > 1){
    movingN <- movav # define the n for the moving average calculations
    linename$SMA1 <- SMA(linename$height, 
                         n = movingN)
    # linename <- linename[movingN:length(linename$SMA), ] # remove first n-1 points
  }
  
  if (movav2 > 1){
    movingN <- movav2 # define the n for the moving average calculations
    linename$SMA2 <- SMA(linename$height, 
                         n = movingN)
    # linename <- linename[movingN:length(linename$SMA2), ] # remove first n-1 points
  }
  
  if (movav3 > 1){
    movingN <- movav3 # define the n for the moving average calculations
    linename$SMA3 <- SMA(linename$height, 
                         n = movingN)
    # linename <- linename[movingN:length(linename$SMA3), ] # remove first n-1 points
  }
  
  
  
  
  # DistTotalProj <- 0
  # Proj.point <- c(0,0)
  # line.sta <- c(linename$lon[1], linename$lat[1])
  # line.end <- c(linename$lon[nrow(linename)],linename$lat[nrow(linename)])
  # Line <- CreateLinePoints(line.sta, line.end)
  # for(i in 2:length(linename$lon)) {
  #  # point <- as.matrix(linename[i,c("lon", "lat")])
  #   Proj.point <- ProjectPoint(as.matrix(linename[i,c("lon", "lat")]), Line)
  #   DistTotalProj[i] <- spDistsN1(as.matrix(cbind(Proj.point[1],Proj.point[2])),
  #                                 c(linename$lon[1], linename$lat[1]))
  #   # CoordinatePlane(min(linename$lon), max(linename$lon), min(linename$lat), max(linename$lat))
  #   # Draw(Line, "black")
  #   # Draw(as.numeric(point), "black")
  #   # Draw(Proj.point, "red")
  #
  #   # line <- fortify(data.frame(rbind(line.sta,line.end)))
  #   # p <- fortify(data.frame(point))
  #   # pp <- fortify(data.frame(X=Proj.point[1],Y=Proj.point[2]))
  #   # ggplot()+
  #   #   geom_line(data=line, aes(x=X1,y=X2)) +
  #   #   geom_point(data=p,aes(x=lon,y=lat))+
  #   #   geom_point(data=pp,aes(x=X,y=Y),color='red')+
  #   #   coord_fixed(ratio = 1)
  # }
  # linename$DistTotalProj <- DistTotalProj
  
  
  return(linename)
}