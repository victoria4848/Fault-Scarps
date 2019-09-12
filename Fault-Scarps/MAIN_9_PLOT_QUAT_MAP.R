# Plot map of offsets and Quaternary
plot_list <- list()
h_w_ratio_list <- list()

# Plot Lon, Lat, Offset for Individual Faults and for Fault Groups
n <- 0
for (i in i_choices_final){ 
  n <- n + 1
  fault_name        <- fault_name_list[i]
  fault_group       <- group_no_list  [i]
  fault_extent      <- extent_faults [[i]] 
  cc_colm <- cc_col_valsm[[i]]
  bw_colm <- bw_col_valsm[[i]]
  cc_cold <- cc_col_valsd[[i]]
  bw_cold <- bw_col_valsd[[i]]
  print(paste0('MAIN_9_PLOT_QUAT_MAP ',fault_name))
  
  # SOIL df for map plot
  soil      <- readRDS(paste0('Data/Soil_R/group_ ',fault_group, ' _soil.RDS'))
  # Depending on extent, read more or less detailed versions
  # soil.df   <- soil[[1]] # 1 is much more detailed, 3 is simplified with tol=10, 3 isn't smaller though....
  # soil.df$Quaternary_english_simp <- factor(soil.df$Quaternary_english_simp, 
  #                                          levels = c('Bedrock','Till','Peat','Sand','Water'))
  
  # # SOIL distances for distance plot
  # f_dist.df <-readRDS(paste0(main_results_dir,'/Soil_fault_dfs/',   fault_name,'_f_dist.df.RDS'))  
  # f_dist.df$Quaternary_english_simp <- factor(f_dist.df$Quaternary_english_simp, 
  #                                             levels = c('Bedrock','Till','Peat','Sand','Water'))
  
  # Offsets for map in individual strands
  # Data after subsetted by confidence values 
  height_pl_bF_s_list     <- readRDS ( paste0 ( main_results_dir , '/' , off_pla_dir , '/' , fault_name , '_offsets_planes.RDS') )
  height_offset           <- height_pl_bF_s_list[[8]]
  height_offset.offset_CV <- subset(height_offset, !is.na(offset_CV) & offset_CV!=0)
  
  dfm      <- height_offset.offset_CV 
  min_o    <- min(dfm$offset_CV)
  max_o    <- max(dfm$offset_CV)
  # dotsize  <- (nrow(dfm))^0.25 / 5
  dotsize  <- 0.75 #(nrow(dfm))^0.15 / 4 # Pasmajarvi nrow 1311,   dotsize = 0.733
  alphaval <- 1
  
  # Rasterize shapefile 
  xlen        <- fault_extent@xmax - fault_extent@xmin
  ylen        <- fault_extent@ymax - fault_extent@ymin
  extra_add   <- max(xlen,ylen)*0.2
  poly        <- soil[[2]] 
  crop_extent <- extent(fault_extent@xmin,fault_extent@xmax,fault_extent@ymin,fault_extent@ymax)+extra_add
  poly_crop   <- crop(poly,crop_extent)
  
  poly_crop$shade <- poly_crop$Quaternary_english_simp
  poly_crop$shade[poly_crop$shade=='Bedrock']= as.numeric(1)
  poly_crop$shade[poly_crop$shade=='Till']=as.numeric(2)
  poly_crop$shade[poly_crop$shade=='Peat']=as.numeric(3)
  poly_crop$shade[poly_crop$shade=='Sand']=as.numeric(4)
  poly_crop$shade[poly_crop$shade=='Water']=as.numeric(5)
  poly_crop$shade <- as.numeric(poly_crop$shade)
  r <- raster(ncol=540, nrow=round(540*ylen/xlen))
  extent(r) <- extent(poly_crop)
  
  rp <- fasterize(sf::st_as_sf(poly_crop),
                  r, 'shade',fun='first')
  
  #  rp <- rasterize(poly_crop, r, 'shade',fun='first')
  rp.df <- as.data.frame(rp,xy=TRUE)
  rp.df$layer[rp.df$layer==1]= 'Bedrock'
  rp.df$layer[rp.df$layer==2]= 'Till'
  rp.df$layer[rp.df$layer==3]= 'Peat'
  rp.df$layer[rp.df$layer==4]= 'Sand'
  rp.df$layer[rp.df$layer==5]= 'Water'
  rp.df$layer[is.na(rp.df$laye)]= 'Till'
  
  rp.df$layer <- factor(rp.df$layer, levels = c('Bedrock','Till','Peat','Sand','Water'))
  
  # Correct colors
  soiltypem <- unique(rp.df$layer)
  bw_colm <- bwh_vals[which(soil_types %in% soiltypem)]
  
  ############### MAP PLOT ##################
  ex_x_min <- min(dfm$lon) ;  ex_x_max <- max(dfm$lon)
  ex_y_min <- min(dfm$lat) ;  ex_y_max <- max(dfm$lat)
  ex_x_extra <- ( ex_x_max - ex_x_min ) * 0.025
  ex_y_extra <- ( ex_y_max - ex_y_min ) * 0.025
  ratio <- ex_x_extra/ex_y_extra
  if(ex_x_extra < ex_y_extra) {ex_x_extra<-ex_y_extra/ratio} # Because it's limited in y direction, not in x
  if(ex_y_extra < ex_x_extra) {ex_y_extra<-ex_x_extra*ratio}
  ex_y <- c(ex_y_min - ex_y_extra, ex_y_max + ex_y_extra)
  ex_x <- c(ex_x_min - ex_x_extra, ex_x_max + ex_x_extra)
  
  pm2 <- ggplot() +
    geom_raster(data=rp.df,aes(x=x/1000,y=y/1000,fill=layer))+
    #  geom_polypath (data = soil.df,aes(x=long/1000,y=lat/1000,group=group,fill=Quaternary_english_simp))+
    geom_point( data = dfm, 
                aes ( x = lon/1000 , y = lat/1000 , color = offset_CV ) , size = dotsize) +
    scale_color_distiller(palette = "Spectral",direction = -1,limits=c(0, max_o)) +
    scale_fill_manual(values = bw_colm,name='Quat.') +
    labs(color = "Offset,\nm") +
    guides(colour = guide_legend(override.aes = list(size=2))) + 
    theme_linedraw()         +
    theme(
      panel.grid   = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text    = element_text(size = 7),
      legend.title = element_text(size = 8), 
      legend.text  = element_text(size = 7),
      legend.key.height = unit(0.6,"cm"),
      legend.key.width = unit(0.3,"cm")
    ) +
    coord_equal(xlim = ex_x/1000, ylim = ex_y/1000 ,expand = FALSE)
  # coord_equal(xlim = c(fault_extent@xmin/1000,fault_extent@xmax/1000), ylim = c(fault_extent@ymin/1000,fault_extent@ymax/1000),
  #             expand = FALSE)
  
  par(mar=c(0,0,0,0))
  h_w_ratio <- par()$pin[2]/par()$pin[1]
  # f_nameM <-  paste0('Final_Figures/',fault_name,'_map.pdf')
  # ggsave(f_nameM,width = 190,  units = 'mm',dpi=600)
  # plot_crop(f_nameM) #to crop the white space around the figure
  
  plot_list[[n]]      <- pm2
  h_w_ratio_list[[n]] <- h_w_ratio
  
}

saveRDS(plot_list, 'Results/p_map_plots.RDS')
saveRDS(h_w_ratio_list,'Results/h_w_ratio_list.RDS')



# Now plot distance and maps together for each fault
plot_dist_list <- readRDS ( 'Results/p_loess_plots.RDS')
plot_maps_list <- readRDS ( 'Results/p_map_plots.RDS')
h_w_ratio_list <- readRDS ( 'Results/h_w_ratio_list.RDS')

#for (i in length(plot_dist_list)){  # Rojnoret is 18
for (i in 1:length(i_choices_final)){
  i2 <- i_choices_final[i]
  fault_name <- fault_name_list[i2]
  plot_maps <- plot_maps_list[[i]] 
  plot_dist <- plot_dist_list[[i2]] # These have i_choices (not i_choices_final)  
  h_w_ratio <- h_w_ratio_list[[i]] # h_w_ratio = 0.8258706
  
  f_nameM   <- paste0 ( 'Final_Figures_SI/Map_Dist/' , fault_name , '_map_dist.pdf' )
  lay <- t(rbind(c(rep(1, times = 1/h_w_ratio*10), rep(2,5))))
  if(fault_name != 'Laisvall') { ggsave(f_nameM,  arrangeGrob(grobs = list(plot_maps , plot_dist),layout_matrix=lay), width = 190,  height = 235, units = 'mm',dpi=600) }
  if(fault_name == 'Laisvall') { ggsave(f_nameM,  arrangeGrob(grobs = list(plot_maps , plot_dist)), width = 190, height =165, units = 'mm',dpi=600)}
  if(fault_name == 'Rojnoret') { 
    f_nameM2   <- paste0 ( 'Final_Figures/' , fault_name , '_map_dist.pdf' ) # Save in final_figures as well as SI because used twice
    ggsave(f_nameM2,  arrangeGrob(grobs = list(plot_maps , plot_dist),layout_matrix=lay), width = 190,  height = 235, units = 'mm',dpi=600)
    plot_crop ( f_nameM2 ) 
     }
  plot_crop ( f_nameM ) #to crop the white space around the figure
  
  
}


# f_nameM <-  paste0('Figures/',fault_name,'map1.png')
# ggsave(f_nameM,width = 190, units = 'mm',dpi=900)

# f_nameM <-  paste0('Figures/',fault_name,'map2.png')
# png(filename=paste0('Figures/',fault_name,'map2_cairo.png'), 
#     type="cairo",
#     units = 'mm',
#     width=190, 
#     height=190*h_w_ratio, 
#     res=96*10)
# plot(pm2)
# dev.off()
# 
# ggsave(f_nameM, width = 190, height = 190*h_w_ratio, units = 'mm',dpi = 96*10)

# plot_crop(f_nameM) #to crop the white space around the figure
#  file.rename(f_nameM, paste0('Figures/',fault_name,'map2.pdf'))
#}

# ## Rename
# f_list <- list.files(path = 'Figures/',pattern="_dist.pdf")
# for (f in 1:length(f_list)){
#   f_list_f <- f_list[f]
#   f_newname <- gsub('dist','_dist',f_list_f)
#   file.rename(from = paste0('Figures/',f_list_f), to =paste0('Figures/',f_newname) )
# }
# 
# for (i in i_choices_final){
#   if (i != 41){ 
#     fault_name        <- fault_name_list[i]
#     f_list_f <- paste0(fault_name,'_map.pdf')
#     file.move(paste0('Figures/',f_list_f), paste0('Final_Figures/'),overwrite = TRUE)
#   }
#   # unlink(paste0('Final_Figures/',f_list_f), recursive = TRUE)
# }

# dir_list <- list.dirs('Data/Faults/',recursive=FALSE)
# dir_listS <- dir_list[str_detect(dir_list, 'NS')]
# dir_listF <- dir_list[str_detect(dir_list, 'NF')]
# ps <- 'NonameSwe'
# pf <- 'Noname'
# 
# # Sweden
# for (j in 1:length(dir_listS)){
#   di <- dir_listS[j]
#   f_lists <- list.files(path = di,pattern=ps)
#   for (f in 1:length(f_lists)){
#     f_list_f <- f_lists[f]
#     f_newname <- gsub(ps,'NS',f_list_f)
#     file.rename(from = paste0(di,'/',f_list_f), to =paste0(di,'/',f_newname) )
#   }
# }
# # Finland
# for (j in 1:length(dir_listF)){
#   di <- dir_listF[j]
#   f_lists <- list.files(path = di,pattern=pf)
#   for (f in 1:length(f_lists)){
#     f_list_f <- f_lists[f]
#     f_newname <- gsub(pf,'NF',f_list_f)
#     file.rename(from = paste0(di,'/',f_list_f), to =paste0(di,'/',f_newname) )
#   }
# }
# 
# di <- './'
# di <- 'Data/Geology/'
# f_listf <- list.files(path = di,pattern=pf,recursive=TRUE)
# f_lists <- list.files(path = di,pattern=ps,recursive=TRUE)
# Pattern = paste(f_lists, collapse="|")
# f_listf <- f_listf[!str_detect(f_listf,Pattern)]
# for (f in 1:length(f_lists)){
#   f_list_f <- f_lists[f]
#   f_newname <- gsub(ps,'NS',f_list_f)
#   file.rename(from = paste0(di,f_list_f), to =paste0(di,f_newname) )
# }
# for (f in 1:length(f_listf)){
#   f_list_f <- f_listf[f]
#   f_newname <- gsub(pf,'NF',f_list_f)
#   file.rename(from = paste0(di,f_list_f), to =paste0(di,f_newname) )
# }
