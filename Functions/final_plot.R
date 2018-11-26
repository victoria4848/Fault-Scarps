# Load all offsets and fault_length per fault and make a dataframe 

lengths_files <- list.files(path = 'Faults_R/',pattern = 'lengths',full.names = FALSE,recursive=FALSE)
offsets_files <- list.files(path = 'Offsets/', pattern = 'offsets',full.names = FALSE,recursive=FALSE)
plots_files   <- list.files(path = 'plots/',   pattern = 'plots',  full.names = FALSE,recursive=FALSE)


len_off.df <- data.frame(straight=rep(NA,length(offsets_files)),
                         length_smooth=NA, offset_distance=NA,
                         av_offset=NA, max_offset=NA, faultname=NA)

for (i in 1:length(offsets_files)){
  len_off.df[i, 1:3] <- unlist(readRDS(paste0('Faults_R/',lengths_files[i]))[1:3])
  len_off.df[i, 4:5] <- readRDS(paste0('Offsets/',offsets_files[i]))
  len_off.df[i, 6] <- strsplit(offsets_files[i],'_')[[1]][2]
}

len_off.df$ratio_av  <- len_off.df$av_offset  / len_off.df$length_smooth
len_off.df$ratio_max <- len_off.df$max_offset / len_off.df$length_smooth

ggplot()+
  geom_point(data=len_off.df, aes(length_smooth, av_offset), color='black')+
  geom_point(data=len_off.df, aes(length_smooth, max_offset), color='red')+
  geom_abline(intercept = 0, slope = 1e-04)+
  xlab('Fault Length (m)')+
  ylab('Offset (m)') 

ggplot()+
  geom_point(data=len_off.df,aes(x=faultname,y=ratio_av),color='black')+
  geom_point(data=len_off.df,aes(x=faultname,y=ratio_max),color='red')+
  geom_hline(yintercept=1e-04)+
  xlab('Fault Name')+
  ylab('Offset/Length ratio')+
  ylim(0,0.004)

plot_profile <- list()
## Plot offset-distance profiles 
for (i in 1:length(plots_files)){
  plot_profile[[i]] <-readRDS(paste0('plots/',plots_files[i]))[[4]]
}





