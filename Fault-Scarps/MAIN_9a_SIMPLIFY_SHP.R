## MAIN_9a_SIMPLIFY_SHP

# Extent of groups
groups_faults_ind <- c(1:13)
group_extent.df <- data.frame(xmin=rep(NA,length(groups_faults_ind)),ymin=NA,xmax=NA,ymax=NA)
for (group_num in groups_faults_ind){
  fault_name_subset <- fault_name_list [ which(group_no_list == group_num) ]
  tempdf <- data.frame(xmin=rep(NA,length(fault_name_subset)),ymin=NA,xmax=NA,ymax=NA)
  for (k in 1:length(fault_name_subset)){
    fault_name <- fault_name_subset[k]
    fault_ex       <- extent_faults[[which(fault_name_list==fault_name)]]
    tempdf[k,]     <- c(fault_ex@xmin, fault_ex@ymin, fault_ex@xmax, fault_ex@ymax)
  }
  group_extent.df[group_num,] <- c(min(tempdf$xmin), min(tempdf$ymin), max(tempdf$xmax), max(tempdf$ymax))
}


groups_faults_ind <- c(1:13)
for (group_num in groups_faults_ind){
  ini <- readRDS(paste('Data/Soil_R/group_',group_num,'_soil.RDS'))
  ini_df   <- ini[[1]]
  ini_spdf <- ini[[2]]
  group_extent <- group_extent.df[group_num,]
  area_group   <- (group_extent$xmax-group_extent$xmin)*(group_extent$ymax-group_extent$ymin)
  keep_area    <- area_group/(200^2)  # Keep areas more than 0.5% of the total area
  if (area_group < 400000000)  { simp_sp <- ini_spdf }
  if (area_group >= 400000000  & area_group < 1000000000){simp_sp <- gSimplify ( ini_spdf , tol=5 , topologyPreserve = TRUE) }
  if (area_group >= 1000000000 & area_group < 2500000000){simp_sp <- gSimplify ( ini_spdf , tol=10 , topologyPreserve = TRUE) }
  if (area_group >= 2500000000 & area_group < 5000000000){simp_sp <- gSimplify ( ini_spdf , tol=20 , topologyPreserve = TRUE) }
  if (area_group >= 5000000000){ simp_sp <- gSimplify ( ini_spdf , tol=30 , topologyPreserve = TRUE) }
  #areas     <- gArea     ( ini_spdf , byid=TRUE) 
  simp_spdf <- SpatialPolygonsDataFrame(simp_sp, ini_spdf@data)
  #simp_spdf2<- simp_spdf[areas > keep_area,]
  simp_df   <- join    (  ini_df , simp_spdf@data , by = "id")
  simp_df   <- simp_df[, !duplicated(colnames(simp_df))]
  saveRDS(list(ini_df,ini_spdf,simp_df), paste('Data/Soil_R/group_',group_num,'_soil.RDS'))
}



# # Finland
# groups_faults_ind <- c(4, 7, 8, 9, 10, 11)
# for (group_num in groups_faults_ind){
#   soil_RDS            <- readOGR(dsn = paste0('Data/Soil/group_',group_num), layer = paste0('group_',group_num))
#   soil_RDS$Quaternary <- soil_RDS$POHJAMAA_1
#   soil_RDS            <- fun_swedish_english (soil_RDS)
#   soil_RDS            <- subset   (soil_RDS, select = c(OBJECTID,Quaternary_english,Quaternary_english_simp))
#   soil_RDS            <- spTransform(soil_RDS,crs(proj_all))
#   soil_RDS.simplify   <- gSimplify(soil_RDS,tol=1,topologyPreserve = TRUE)
#   soil_RDS.simplify2   <- gSimplify(soil_RDS,tol=10,topologyPreserve = TRUE)
#   soil_RDS.df1        <- data.frame(soil_RDS@data)
#   merge.spdf          <- SpatialPolygonsDataFrame(soil_RDS.simplify, soil_RDS.df1)
#   merge.spdf2          <- SpatialPolygonsDataFrame(soil_RDS.simplify2, soil_RDS.df1)
#   
#   merge.spdf@data$id  <- rownames (merge.spdf@data)
#   soil_RDS.df         <- fortify  (merge.spdf, region = "id")
#   soil_RDS.df         <- merge    (soil_RDS.df, merge.spdf@data, by = "id")
#   merge.spdf2@data$id  <- rownames (merge.spdf2@data)
#   soil_RDS.df2         <- fortify  (merge.spdf2, region = "id")
#   soil_RDS.df2         <- merge    (soil_RDS.df2, merge.spdf2@data, by = "id")
#   saveRDS(list(soil_RDS.df,merge.spdf,soil_RDS.df2), paste('Data/Soil_R/group_',group_num,'_soil.RDS'))
# }
