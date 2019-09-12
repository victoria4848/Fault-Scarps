# MAIN_8b_STDDEV

stand_devi_list <- readRDS('Results/std_dev_list.RDS')
stan_devi.df2   <- stand_devi_list[[1]]
stan_devi.df3   <- stand_devi_list[[2]]

df$av_slip <- df$av_offset/ sind(60)
stan_devi.df2$av_slip <- stan_devi.df2$av_offset/ sind(60)
stan_devi.df3$av_slip <- stan_devi.df3$av_offset/ sind(60)

df <- stan_devi.df2

#df <- subset(df, name %in% c('Bollnas','Sevetti','Pasmaj\u{00E4}rvi-Ruokovaara+','Pasmaj\u{00E4}rvi-Ruokovaara','Lansj\u{00E4}rv+','Lansj\u{00E4}rv'))
#df <- subset(df, name %in% c('Bollnas','Pasmaj\u{00E4}rvi-Ruokovaara'))
df$logsd       <- log(df$sd)
df$lognum_meas <- log(df$num_meas)

#f <- lm(logsd ~ 0+lognum_meas + av_offset, data = df)
f <- lm(logsd ~ 0+lognum_meas + av_slip  , data = df)
summary(f)

c <- coefficients(f)
logmodsd <- c[1]*df$lognum_meas + c[2]*df$av_slip
logmodsd <- -0.515*df$lognum_meas + 0.19*df$av_slip
df$logmodsd <- logmodsd
df$fv       <- (fitted.values(f))

hjusta <- -0.1
hjustb <- -0.12
hjust1 <- -0.14
vjust1 <- -7


p_mod <- ggplot()+
   geom_line ( data = df, aes ( x = log(num_meas),y=fv,group=name),size=0.1)+
 # geom_line ( data = df, aes ( x = log(num_meas),y=logmodsd,group=name),size=0.1)+
  geom_line ( data = df, aes ( x = log(num_meas),y=log(sd) ,group=name,color=av_slip))+
  theme_linedraw()+
  ggtitle('(b)') +
  scale_color_distiller(palette='Spectral',limits=c(0,NA))+
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(size=0.25,color='grey'), 
    axis.text         = element_text(size = 7),
    axis.title.y      = element_text(size=8),
    axis.title.x      = element_text(size=8),
    plot.title=element_text( hjust=hjustb, vjust=vjust1,size=10),
    legend.position = 'none',
    legend.key.height = unit(0.7,"cm"),
    legend.key.width = unit(0.35,"cm"),
    legend.title      = element_text(size = 8), 
    legend.text       = element_text(size = 7))+
  xlab('log(Number of Measurements)') +
  ylab('log(Av. Standard Deviation)')+
  labs(color='Length,\nkm')+
  coord_cartesian(expand=FALSE,xlim=c(0,max(log(df$num_meas))),ylim=c(-3,2.5))
plot(p)




psd <- ggplot()+
  # geom_line ( data = stan_devi.df2, aes ( x = num_meas,y=sd,group=name,color=length_f/1000))+
  geom_line ( data = stan_devi.df2, aes ( x = num_meas,y=sd,group=name,color=av_slip))+
  theme_linedraw()+
  ggtitle('(a)') +
  scale_color_distiller(palette='Spectral',limits=c(0,NA))+
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(size=0.25,color='grey'), 
    axis.text         = element_text(size = 7),
    axis.title.y      = element_text(size=8),
    axis.title.x      = element_text(size=8),
    plot.title=element_text( hjust=hjusta, vjust=vjust1,size=10),
    legend.position = 'none',
    legend.key.height = unit(0.6,"cm"),
    legend.key.width = unit(0.3,"cm"),
    legend.title      = element_text(size = 8), 
    legend.text       = element_text(size = 7))+
  xlab('Number of Measurements') +
  ylab('Av. Standard Deviation')+
  labs(color='Length,\nkm')+
  coord_cartesian(expand=FALSE,xlim=c(0,max(stan_devi.df3$num_meas)),ylim=c(0,6))

# f_nameM <-  'Figures/hist_graph_psd.pdf'
# ggsave(f_nameM,width = 190,  units = 'mm',dpi=600)
# plot_crop(f_nameM) #to crop the white space around the figure


p10 <- ggplot()+
  # geom_line ( data = stan_devi.df3, aes ( x = num_meas,y=prob_within_10,group=name,color=length_f/1000))+
  geom_line ( data = stan_devi.df3, aes ( x = num_meas,y=prob_within_10,group=name,color=av_slip))+
  theme_linedraw()+
  ggtitle('(c)') +
  scale_color_distiller(palette='Spectral',limits=c(0,NA))+
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(size=0.25,color='grey'), 
    axis.text         = element_text(size = 7),
    axis.title.y      = element_text(size=8),
    axis.title.x      = element_text(size=8),
    plot.title=element_text( hjust=hjust1, vjust=vjust1,size=10),
    legend.key.height = unit(0.6,"cm"),
    legend.key.width = unit(0.3,"cm"),
    legend.position = 'none',
    legend.title      = element_text(size = 8), 
    legend.text       = element_text(size = 7))+
  xlab('Number of Measurements') +
  ylab('Prob. within 10% of Av. Offset')+
  labs(color='Length,\nkm')+
  coord_cartesian(expand=FALSE,xlim=c(0,max(stan_devi.df3$num_meas)),ylim=c(0,100))

# f_nameM <-  'Figures/hist_graph_p10.pdf'
# ggsave(f_nameM,width = 190,  units = 'mm',dpi=600)
# plot_crop(f_nameM) #to crop the white space around the figure


p25 <- ggplot()+
  # geom_line ( data = stan_devi.df3, aes ( x = num_meas,y=prob_within_25,group=name,color=length_f/1000))+
  geom_line ( data = stan_devi.df3, aes ( x = num_meas,y=prob_within_25,group=name,color=av_slip))+
  theme_linedraw()+
  ggtitle('(d)') +
  scale_color_distiller(palette='Spectral')+
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(size=0.25,color='grey'), 
    axis.text         = element_text(size = 7),
    axis.title.y      = element_text(size=8),
    axis.title.x      = element_text(size=8),
    plot.title=element_text( hjust=hjust1, vjust=vjust1,size=10),
    legend.position   = c(0.85,0.45),
    legend.key.height = unit(0.5,"cm"),
    legend.key.width  = unit(0.25,"cm"),
    legend.title      = element_text(size = 8), 
    legend.text       = element_text(size = 7))+
  xlab('Number of Measurements') +
  ylab('Prob. within 25% of Av. Offset')+
  #  labs(color='Length,\nkm')+
  labs(color='Average\nslip, m')+
  coord_cartesian(expand=FALSE,xlim=c(0,max(stan_devi.df3$num_meas)),ylim=c(0,100))

# f_nameM <-  'Figures/hist_graph_p25.pdf'
# ggsave(f_nameM,width = 190,  units = 'mm',dpi=600)
# plot_crop(f_nameM) #to crop the white space around the figure

### Save all three in one plot ###
f_nameM <- 'Final_Figures/hist_combi_p_avoffset.pdf'
ggsave(filename=f_nameM,plot=grid.arrange(psd,p_mod,p10,p25,nrow=2),width = 190, height = 190*0.85,  units = 'mm',dpi=600)
plot_crop(f_nameM)
