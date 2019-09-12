# MAIN_10_RATIOS

# Length, average and max95% ratios
# load lengths

# Get average and various max offsets (90%, 95%, 100%)
offset.df <- data.frame(name=NA,offset_mean=NA,offset_max=NA,offset_95=NA,offset_90=NA,
                        r_max_av=NA,r_95_av=NA,r_90_av=NA,stan_dev=NA,off_p_sd=NA,off_m_sd=NA)
cn <- 0
for(c in i_choices_final){
  cn <- cn + 1
  fault_name <- fault_name_list[c] 
  hl <- readRDS(paste0 ( main_results_dir , '/' , off_pla_dir , '/' , fault_name , '_offsets_planes.RDS'))[[8]]
  ho <-  hl[!is.na(hl$offset_CV_strands),]
  ho <-  ho[(ho$offset_CV_strands>0),]
  offsets <- ho$offset_CV_strands
  n_offsets   <- length(offsets)
  offset_max  <- max(offsets)
  offset_mean <- mean(offsets)
  offset_95   <- sort(offsets)[round(n_offsets*0.95)]
  offset_90   <- sort(offsets)[round(n_offsets*0.90)]
  r_max_av <- offset_max / offset_mean
  r_95_av  <- offset_95 / offset_mean
  r_90_av  <- offset_90 / offset_mean
  stan_dev <- sd(offsets)
  off_p_sd <- offset_mean+stan_dev
  off_m_sd <- offset_mean-stan_dev
  offset.df[cn,] <- c(fault_name, offset_mean,offset_max,offset_95,offset_90,
                      r_max_av,r_95_av,r_90_av,stan_dev,off_p_sd,off_m_sd)
}
offset.dfm <- melt(offset.df,id.vars='name')
offset.dfm$value <- as.numeric(offset.dfm$value)
offset.dfm.ss <- subset(offset.dfm, variable %in% c('r_max_av','r_95_av','r_90_av'))

#offset.dfm.ss <- change_names(offset.dfm.ss)
f_nam_order <- subset(offset.dfm.ss,variable == 'r_max_av')
f_nam_order <- order(f_nam_order$value)
uni_nam     <- unique(offset.dfm.ss$name)
offset.dfm.ss$name <- factor(offset.dfm.ss$name,levels=uni_nam[f_nam_order] )
a <- subset(offset.dfm.ss,variable == 'r_max_av')
a <- mean(a$value)
b <- subset(offset.dfm.ss,variable == 'r_95_av',select=value)
b <- mean(b$value)
c <- subset(offset.dfm.ss,variable == 'r_90_av',select=value)
c <- mean(c$value)
av_lines.df <- data.frame(av_max100vsav=a,av_max95vsav=b,av_max90vsav=c)

#offset.dfm.ss <- subset(offset.dfm.ss, name %in% fault_name_list[i_choices_final])
######################################################################
########################### MAX_AV_OFFSETS ###########################
######################################################################
offset.dfm.ss$name <- factor(offset.dfm.ss$name,levels=uni_nam[f_nam_order] )
colors_points <- c('black','blue','cyan')
p_offsets <- ggplot()+
  geom_point (data=offset.dfm.ss,aes(x=name,y=value,color=variable))+
  scale_color_manual(values = c('black','blue','cyan'),name='Ratio Type',
                     labels = c('Max100 vs. Av','Max95 vs. Av','Max90 vs. Av'))+
  geom_hline(data=av_lines.df,aes(yintercept=av_max100vsav),color=colors_points[1])+
  geom_hline(data=av_lines.df,aes(yintercept=av_max95vsav),color=colors_points[2])+
  geom_hline(data=av_lines.df,aes(yintercept=av_max90vsav),color=colors_points[3])+
  theme_linedraw()+
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(size=0.25,color='grey'), 
    axis.text         = element_text(size = 7),
    axis.title.y      = element_text(size=8),
    axis.title.x      = element_blank(),
    axis.text.x       = element_text(angle = 90, hjust = 1,vjust=0.3),
    legend.position   = c(0.15,0.8),
    legend.title      = element_text(size = 8), 
    legend.text       = element_text(size = 7))+
  ylab('Ratio')

f_nameM <-  paste0('Final_Figures_SI/max_m95_m90_offsets.pdf')
ggsave(f_nameM,width = 190*0.75,  units = 'mm',dpi=600)
plot_crop(f_nameM) #to crop the white space around the figure

######################################################################
######################### OFFSET_LENGHT_RATIOS #######################
######################################################################

# Add data from other compilations to plot as grey points 
slip2length    <- read.xlsx('Data/slip2length.xlsx',sheet = 1,colNames = TRUE)
slip2length <- subset(slip2length, Reference =='WAC94')
length_km      <- as.numeric(slip2length[,8])
eq_name        <- as.character(slip2length[,2])
slip_a         <- as.numeric(slip2length[,11])
slip_m         <- as.numeric(slip2length[,10])
intra          <- slip2length[,18]
tennant            <- which(grepl('Tennant',eq_name)) # Cos this eq happened as 3 eqs very close to each otehr i think
slip_a[tennant]    <- mean(slip_a[tennant])
slip_m[tennant]    <- mean(slip_m[tennant])
length_km[tennant] <- mean(length_km[tennant])
slip2length.ss <- data.frame(length_km=length_km, eq_name = eq_name,slip_a=slip_a, slip_m=slip_m, intra=intra)
slip2length.ss <- slip2length.ss[-c(tennant[2:length(tennant)]),]
slip2length.ss <- subset(slip2length.ss, !is.na(slip_a) & !is.na(length_km))



len.df <- data.frame(name =NA,f_lengths=NA)
ff <- 0
for (i in i_choices_final){
  ff <- ff+1
  fault_name <- fault_name_list[i]
  f_lengths <- readRDS(paste0('Data/Faults_R/',fault_name,'_lengths.RDS'))[[3]]
  len.df[ff,] <- c(fault_name,f_lengths)
}
len_off.df           <- merge(len.df,offset.dfm)
len_off.df$f_lengths <- as.numeric(len_off.df$f_lengths)
len_off.df.ss        <- subset(len_off.df, variable == 'offset_mean')
len_off.df.max       <- subset(len_off.df, variable == 'offset_max')
len_off.df.m90       <- subset(len_off.df, variable == 'offset_90')
len_off.df.ss$olr    <-len_off.df.ss$value / len_off.df.ss$f_lengths
len_sd.df <- merge(len.df,offset.df)
## Need standard deviations of lengths (for now up 5km)
len_sd.df$off_m_sd    <- as.numeric(len_sd.df$off_m_sd)
len_sd.df$off_p_sd    <- as.numeric(len_sd.df$off_p_sd)
len_sd.df$f_lengths   <- as.numeric(len_sd.df$f_lengths)
len_sd.df$offset_mean <- as.numeric(len_sd.df$offset_mean)
len_sd.df$len_p_sd    <- len_sd.df$f_lengths+5000 #atm just 5km longer....

dips <- 90 # Assume dips at surface are vertical...
len_off.df.ss$slip      <- len_off.df.ss$value          / sind(dips) # we assume 50 degrees is the dip
len_off.df.max$slip_max <- len_off.df.max$value         / sind(dips) # we assume 50 degrees is the dip
len_off.df.m90$slip_m90 <- len_off.df.m90$value         / sind(dips) # we assume 50 degrees is the dip
len_sd.df$slip_mean     <- len_sd.df$offset_mean        / sind(dips) # we assume 50 degrees is the dip

# Dotted lines between combined faults #
dot_lines <- data.frame ( name_1 = NA , name_2 = NA , x = NA , y = NA )
nn        <- as.character(uni_nam[str_which(uni_nam, '_')])
strsplit2 <- function(x) strsplit(x,split='_',fixed=TRUE)[[1]]
strsplitc <- function(x) length(strsplit(x,split='_',fixed=TRUE)[[1]])
nn1   <- unlist ( lapply ( nn , strsplit2))
nn1.u <- unique ( nn1)
nnc   <- unlist ( lapply ( nn , strsplitc))
nn_o  <- nn     [ order ( nnc ) ]
in_another <- c()
for (n in 1:length(nn_o)){
  in_another[n] <- max(which(str_detect(nn_o,nn_o[n])))
}
in_another2 <- which(in_another - seq(1,length(in_another),1) >0 )
wh_in <- in_another[in_another2]

n_j <- 1
for (n in 1:length(nn_o)){
  nn_n <- as.character(nn_o[n])
  nn1  <- strsplit(nn_n,split='_',fixed=TRUE)[[1]]
  off  <- len_off.df.ss$slip      [ len_off.df.ss$name %in% nn1]
  len  <- len_off.df.ss$f_lengths [ len_off.df.ss$name %in% nn1]
  offt <- len_off.df.ss$slip      [ len_off.df.ss$name %in% nn_n]
  lent <- len_off.df.ss$f_lengths [ len_off.df.ss$name %in% nn_n]
  if (length(nn1)==2){
    dot_lines[n_j,]   <- c(name_1=nn1[1],name_2=nn_n,x=off[1],y=len[1])
    dot_lines[n_j+1,] <- c(name_1=nn1[2],name_2=nn_n,x=off[2],y=len[2])
    dot_lines[n_j+2,] <- c(name_1=nn1[1],name_2=nn_n,x=offt,y=lent)
    dot_lines[n_j+3,] <- c(name_1=nn1[2],name_2=nn_n,x=offt,y=lent)
    n_j <- n_j+4
  }
  if (length(nn1)==3){
    dot_lines[n_j,]   <- c(name_1=nn1[1],name_2=nn_n,x=off[1],y=len[1])
    dot_lines[n_j+1,] <- c(name_1=nn1[2],name_2=nn_n,x=off[2],y=len[2])
    dot_lines[n_j+2,] <- c(name_1=nn1[3],name_2=nn_n,x=off[3],y=len[3])
    dot_lines[n_j+3,] <- c(name_1=nn1[1],name_2=nn_n,x=offt,y=lent)
    dot_lines[n_j+4,] <- c(name_1=nn1[2],name_2=nn_n,x=offt,y=lent)
    dot_lines[n_j+5,] <- c(name_1=nn1[3],name_2=nn_n,x=offt,y=lent)
    n_j <- n_j+6
  }
  if(n %in% wh_in) {
    fi1 <- nn_o[in_another2][1]
    fi2 <- nn_o[in_another2][2]
    off1 <- len_off.df.ss$slip[len_off.df.ss$name == fi1]
    off2 <- len_off.df.ss$slip[len_off.df.ss$name == fi2]
    len1 <- len_off.df.ss$f_lengths[len_off.df.ss$name == fi1]
    len2 <- len_off.df.ss$f_lengths[len_off.df.ss$name == fi2]
    dot_lines[n_j,]   <- c(name_1=fi1,name_2=nn_n,x=off1,y=len1)
    dot_lines[n_j+1,] <- c(name_1=fi2,name_2=nn_n,x=off2,y=len2)
    dot_lines[n_j+2,] <- c(name_1=fi1,name_2=nn_n,x=offt,y=lent)
    dot_lines[n_j+3,] <- c(name_1=fi2,name_2=nn_n,x=offt,y=lent)
    n_j <- n_j+4
  }
}
dot_lines$slip      <- as.numeric(dot_lines$x)
dot_lines$f_lengths <- as.numeric(dot_lines$y)
lan_y  <- len_off.df.ss$f_lengths[len_off.df.ss$name=='Lansjarv'] 
lan_x  <- len_off.df.ss$slip     [len_off.df.ss$name=='Lansjarv'] 
lanp_y <- len_off.df.ss$f_lengths[len_off.df.ss$name=='Lansjarv_NS2h_NS2g_NS2f_NS2e'] 
lanp_x <- len_off.df.ss$slip     [len_off.df.ss$name=='Lansjarv_NS2h_NS2g_NS2f_NS2e'] 
dot_lines <- rbind(dot_lines, c('Lansjarv','Lansjarv_NS2h_NS2g_NS2f_NS2e',lan_x,lan_y,lan_x,lan_y ))
dot_lines <- rbind(dot_lines, c('Lansjarv','Lansjarv_NS2h_NS2g_NS2f_NS2e',lanp_x,lanp_y,lanp_x,lanp_y ))
dot_lines$f_lengths <- as.numeric(dot_lines$f_lengths)
dot_lines$slip      <- as.numeric(dot_lines$slip)

## Add on average standard deviations to offsets (from distances....) ##
## Add on error bar for lengths....

# Lines from other papers for offset/length ratios
name_list <- c('LE14_U','LE14_M','LE14_L',
               'Scholz_U','Scholz_L',
               'WC94',
               'HB02_U' ,'HB02_L' )
x_list <- c(122,122,122,
            85,85,
            110,
            135,135)
y_list <- c(7.3,4.7,2.95,
            7.95,0.55,
            2.05,
            4.25,1.7)
angle_list <- c(28,20,13,
                45,8,
                11,
                17,8)
OL_lines.df <- data.frame(name=name_list,x=x_list,y=y_list,angle=angle_list)
# Actual Line equations
L <- seq(0,450000,10)
OL_l.df <- data.frame(L=L)
# Leonard 2014 
Ls <- seq(0,2500,10) ; Ll <- seq(2510,450000,10)
bs <- 1.000 ; ams <- -4.137 ; aus <- -4.30 ; als <- -4.00
bl <- 0.833 ; aml <- -3.572 ; aul <- -3.78 ; all <- -3.38
dl.fun <- function(x,y,z) 10^(x + y * log10(z)) 
OL_l.df$D_L14m <- c(dl.fun(ams,bs,Ls),dl.fun(aml,bl,Ll))
OL_l.df$D_L14u <- c(dl.fun(aus,bs,Ls),dl.fun(aul,bl,Ll))
OL_l.df$D_L14l <- c(dl.fun(als,bs,Ls),dl.fun(all,bl,Ll))
# WC94 b = 0.88(0.11) for all, and a = -1.43(0.18) 
bm <- 0.88 ; bu <- 0.99 ; bl <-0.77 ; ams <- -1.43 ; aus <- -1.61 ; als <- -1.25
Ls <- seq(3800,450000,10) ;
OL_l.df$D_WC94m <- c(rep(NA,380),dl.fun(ams,bm,Ls/1000))
OL_l.df$D_WC94u <- c(rep(NA,380),dl.fun(aus,bu,Ls/1000))
OL_l.df$D_WC94l <- c(rep(NA,380),dl.fun(als,bl,Ls/1000))
# Scholz 10^-5 to 10^-4 
OL_l.df$SCHOLZu <- L * 10^-5
OL_l.df$SCHOLZl <- L * 10^-4
# Hanks & Bakun 2002 
OL_l.df$HANKSl <- L * 1.2*10^-5
OL_l.df$HANKSu <- L * 3.4*10^-5
OL_l.df$L_km <- OL_l.df$L/1000
# Other earthquakes that would have high offset/length values e.g. 
#ChiCHi 100km and up to 10m offset
len_sd.df$f_lengths <- as.numeric(len_sd.df$f_lengths)

## Add on errors from slip-dis offsets - to get max length
params_df                    <- readRDS(paste0('Results/params_slip_dis.RDS'))
params_df$L_slip_dis         <- as.numeric ( as.character( params_df$L_slip_dis))
#params_df$L_original         <- as.numeric ( as.character( params_df$L_original))
#params_df$av_offset_original <- as.numeric ( as.character( params_df$av_offset_original))
params_df$av_offset_slip_dis <- as.numeric ( as.character( params_df$av_offset_slip_dis))
len_sd.df  <- merge(len_sd.df , params_df, by = 'name')

# Make fault names more sensible
len_off.df.ss$name[len_off.df.ss$name=='Pasmajarvi_Ruokovaara_NF2_NF3_NF4']='PR+'
len_off.df.ss$name[len_off.df.ss$name=='Sjaunja_NS1c_NS1d']='Sjaunja'
len_off.df.ss$name[len_off.df.ss$name=='Lansjarv_NS2h_NS2g_NS2f_NS2e']='Lansjarv+'
len_off.df.ss$name[len_off.df.ss$name=='Isovaara_Riikonkumpu']='IR'

len_off.df.ss  <- change_names(len_off.df.ss)
len_off.df.max  <- change_names(len_off.df.max)
len_off.df.m90  <- change_names(len_off.df.m90)
len_sd.df$slip_slip_dis <- len_sd.df$av_offset_slip_dis / sind(dips) # we assume 90 degrees is the dip

data.df        <- subset(len_sd.df,select = c(name,slip_mean,f_lengths))
names(data.df) <- c('name','slip','length')
data.df$length <- data.df$length/1000
ssme.df        <- subset(len_sd.df,select = c(name,slip_slip_dis,L_slip_dis))
names(ssme.df) <- c('name','slip','length')
line_EB        <- rbind(data.df, ssme.df)

# Red least squares lines 
moavob   <- lm ( slip ~ f_lengths +0 , data = len_off.df.ss)   #8.8e05 
moavth   <- lm ( av_offset_slip_dis ~ L_slip_dis +0 , data = len_sd.df)  #  6-5
moavob.c <- moavob$coefficients*1000 ; moavth.c <- moavth$coefficients
# Burtrask ratio
br <- len_off.df.ss$olr[len_off.df.ss$name=='Burträsk']*1000 # 2e-4
# Suasselka ratio 
sr <- len_off.df.ss$olr[len_off.df.ss$name=='Suasselkä']*1000 # 3e-5
df_shade1 <- data.frame(x=c(0,450),ymin=c(0,450*moavth.c),ymax=c(0,450*moavob.c))
df_shade2 <- data.frame(x=c(0,450),ymin=c(0,450*sr),ymax=c(0,450*br))

p_len_off <- ggplot()+
  # geom_abline(intercept = 0, slope = moavob.c , color = 'red') +
  # geom_abline(intercept = 0, slope = moavth.c , color = 'red') +
  geom_ribbon(data=df_shade1, aes (x=x, ymin=ymin , ymax=ymax ), fill="red", alpha=0.2) +
  geom_ribbon(data=df_shade2, aes (x=x, ymin=ymin , ymax=ymax ), fill="red", alpha=0.1) +
  # geom_abline(intercept = 0, slope = br ,       color = 'red', linetype='dotted')+
  # geom_abline(intercept = 0, slope = sr ,       color = 'red', linetype='dotted')+
  # Grey dashed lines from points to joined points 
  geom_line(data=dot_lines, aes(x=f_lengths/1000,y=slip,group=name_1),linetype='dashed',color='grey',size=0.5)+
  # Magenta dashed straight lines
  geom_line(data=OL_l.df, aes(x=L_km,y=D_L14m) ,color='magenta',linetype='dotted')+
  geom_line(data=OL_l.df, aes(x=L_km,y=D_L14u) ,color='magenta',linetype='dotted')+
  geom_line(data=OL_l.df, aes(x=L_km,y=D_L14l) ,color='magenta',linetype='dotted')+
  geom_line(data=OL_l.df, aes(x=L_km,y=D_WC94m),color='magenta',linetype='solid')+
  geom_line(data=OL_l.df, aes(x=L_km,y=SCHOLZu),color='magenta',linetype='dashed')+
  geom_line(data=OL_l.df, aes(x=L_km,y=SCHOLZl),color='magenta',linetype='dashed')+
  geom_line(data=OL_l.df, aes(x=L_km,y=HANKSu) ,color='magenta',linetype='dotdash')+
  geom_line(data=OL_l.df, aes(x=L_km,y=HANKSl) ,color='magenta',linetype='dotdash')+
  # Magenta text for lines 
  geom_text(data=OL_lines.df,aes(x=x,y=y,label=name,angle=angle),
            size = 2.5, vjust = 0, hjust = 0,color='magenta')+
  # Red points and error bars 
  # geom_errorbar ( data = len_sd.df    , aes( x    = f_lengths/1000, ymin = off_m_sd,  ymax=off_p_sd      ),color='red')+
  # geom_errorbar ( data = len_sd.df    , aes( x    = f_lengths/1000, ymin = av_offset_slip_dis,  ymax=offset_mean     ),color='red',size=0.5)+
  #  geom_errorbarh( data = len_sd.df    , aes( xmin = f_lengths/1000, xmax = L_slip_dis,y=offset_mean ),color='red',size=0.5)+
  
  # Previous compilation (originally from Guy)
  geom_point(data=slip2length.ss, aes(length_km, slip_a), color='grey')+
  
  geom_line ( data = line_EB , aes( x=length , y=slip , group = name ) , color='red' , size=0.5 )+
  geom_point    ( data = len_off.df.ss, aes( x    = f_lengths/1000, y    = slip                       ),color='red')+
  # Text to label some faults 
  geom_text(data=subset(len_off.df.ss,f_lengths>20000|slip>7.5),aes(x=f_lengths/1000,y=slip,label=name),
            size = 3, vjust = 0, hjust = 0,nudge_y=0.1,nudge_x = 0.2)+
  geom_text(data=subset(len_off.df.ss,name %in% c('Bollnas')),aes(x=f_lengths/1000,y=slip,label=name),
            size = 3, vjust = 0, hjust = 0,nudge_y=-0.1,nudge_x = 2)+
  geom_text(data=subset(len_off.df.ss,name %in% c('Laisvall')),aes(x=f_lengths/1000,y=slip,label=name),
            size = 3, vjust = 0, hjust = 0,nudge_y=0.1,nudge_x = -10)+
  theme_linedraw()+
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(size=0.25,color='grey'), 
    axis.text         = element_text(size = 7),
    axis.title.y      = element_text(size=8),
    axis.title.x      = element_text(size=8),
    #  axis.text.x       = element_text(angle = 0, hjust = 1,vjust=0.3),
    legend.position   = 'none',
    legend.title      = element_text(size = 8), 
    legend.text       = element_text(size = 7))+
  ylab('Average Slip, m')+
  xlab('Fault Length, km') +
  coord_cartesian(expand=FALSE,xlim=c(0,450),ylim=c(0,12))

f_nameM <-  paste0('Final_Figures/max_av_offsets_long.pdf')
ggsave(f_nameM,width = 190,  units = 'mm',dpi=600)
plot_crop(f_nameM) #to crop the white space around the figure

saveRDS(len_sd.df,'Results/len_offset_data.RDS')

## Linear model bewteen max offset and length
moavob   <- lm ( slip ~ f_lengths +0 , data = len_off.df.ss)   #8.8e05 
moavth   <- lm ( av_offset_slip_dis ~ L_slip_dis +0 , data = len_sd.df)  #  6-5
moavob.c <- moavob$coefficients ; moavth.c <- moavth$coefficients
mod_av <- coefficients(moavth)[1] + coefficients(moavth)[2] * len_off.df.ss$f_lengths
moma   <- lm ( slip_max ~ f_lengths , data = len_off.df.max)
mod_ma <- coefficients(moma)[1] + coefficients(moma)[2] * len_off.df.ss$f_lengths
mod.df <- data.frame(f_lengths = len_off.df.ss$f_lengths, mod_av=mod_av, mod_ma=mod_ma)

R1.df   <- data.frame(x=c(87), y = c(9), text = 'R\u{00B2}=0.09')
R2.df   <- data.frame(x=c(87), y = c(37),text = 'R\u{00B2}=0.57')
############ MAX OFFSET ################
p_len_off <- ggplot()+
  # # Grey dashed lines from points to joined points 
  # geom_line(data=dot_lines, aes(x=f_lengths/1000,y=slip,group=name_1),linetype='dashed',color='grey',size=0.5)+
  # # Magenta dashed straight lines
  # geom_line(data=OL_l.df, aes(x=L_km,y=D_L14m) ,color='magenta',linetype='dotted')+
  # geom_line(data=OL_l.df, aes(x=L_km,y=D_L14u) ,color='magenta',linetype='dotted')+
  # geom_line(data=OL_l.df, aes(x=L_km,y=D_L14l) ,color='magenta',linetype='dotted')+
  # geom_line(data=OL_l.df, aes(x=L_km,y=D_WC94m),color='magenta',linetype='solid')+
  # geom_line(data=OL_l.df, aes(x=L_km,y=SCHOLZu),color='magenta',linetype='dashed')+
  # geom_line(data=OL_l.df, aes(x=L_km,y=SCHOLZl),color='magenta',linetype='dashed')+
  # geom_line(data=OL_l.df, aes(x=L_km,y=HANKSu) ,color='magenta',linetype='dotdash')+
  # geom_line(data=OL_l.df, aes(x=L_km,y=HANKSl) ,color='magenta',linetype='dotdash')+
# # Magenta text for lines 
  geom_text ( data = R1.df , aes ( x = x , y = y , label = text ) , size = 3 , color = 'red'  )+
  geom_text ( data = R2.df , aes ( x = x , y = y , label = text ) , size = 3 , color = 'black')+
# # Red points and error bars 
# # geom_errorbar ( data = len_sd.df    , aes( x    = f_lengths/1000, ymin = off_m_sd,  ymax=off_p_sd      ),color='red')+
# # geom_errorbar ( data = len_sd.df    , aes( x    = f_lengths/1000, ymin = av_offset_slip_dis,  ymax=offset_mean     ),color='red',size=0.5)+
# #  geom_errorbarh( data = len_sd.df    , aes( xmin = f_lengths/1000, xmax = L_slip_dis,y=offset_mean ),color='red',size=0.5)+
# geom_line ( data = line_EB , aes( x=length , y=slip , group = name ) , color='red' , size=0.5 )+
 geom_line   ( data = mod.df ,        aes( x = f_lengths/1000 , y = mod_av  ) , color='red'   , size=0.5 )+
  geom_line  ( data = mod.df ,        aes( x = f_lengths/1000 , y = mod_ma  ) , color='black' , size=0.5 )+
  geom_point ( data = len_off.df.ss,  aes( x = f_lengths/1000 , y = slip    ) , color='red'  )+
  geom_point ( data = len_off.df.max, aes( x = f_lengths/1000 , y = slip_max) , color='black')+
  #  geom_point    ( data = len_off.df.m90, aes( x    = f_lengths/1000, y    = slip_m90   ),color='blue')+
  # Text to label some faults 
  #  geom_text(data=subset(len_off.df.max,f_lengths>50000|slip_max>20),aes(x=f_lengths/1000,y=slip_max,label=name),
  #            size = 3, vjust = 0, hjust = 0,nudge_y=0.1,nudge_x = 0.2)+
  #  geom_text(data=subset(len_off.df.m90,f_lengths>50000|slip_max>20),aes(x=f_lengths/1000,y=slip_m90,label=name),
  #            size = 3, vjust = 0, hjust = 0,nudge_y=0.1,nudge_x = 0.2)+
  theme_linedraw()+
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(size=0.25,color='grey'), 
    axis.text         = element_text(size = 7),
    axis.title.y      = element_text(size=8),
    axis.title.x      = element_text(size=8),
    #  axis.text.x       = element_text(angle = 0, hjust = 1,vjust=0.3),
    legend.position   = 'none',
    legend.title      = element_text(size = 8), 
    legend.text       = element_text(size = 7))+
  ylab('Slip, m')+
  xlab('Fault Length, km') +
  coord_cartesian(expand=FALSE,xlim=c(0,160),ylim=c(0,60))

f_nameM <-  paste0('Final_Figures_SI/max_max_offsets.pdf')
ggsave(f_nameM,width = 190,  units = 'mm',dpi=600)
plot_crop(f_nameM) #to crop the white space around the figure

# Find percentage missing needed to make ratio (olr) 10^-4
target <- 1e-4
len_off.df.ss$length_1e4   <- len_off.df.ss$slip/target
len_off.df.ss$length_1e4pc <- (len_off.df.ss$length_1e4-len_off.df.ss$f_lengths) / len_off.df.ss$length_1e4 * 100

lll <- subset(len_off.df.ss, f_lengths <20000  &  slip > 3)

# Write ratios to excel ssme.df - this is theoretical and data.df -  this is the measrued 
guy_table <- cbind(ssme.df, data.df)
names(guy_table) <-c ('Fault_name','Theoretical_slip_m','Theoretical_length_km','Name','Measured_slip_m','Measured_length_km')
guy_table <- guy_table[,c(1,5,6,2,3)]
guy_table$Measured_ratio <- guy_table$Measured_slip/guy_table$Measured_length/1000
guy_table$Theoretical_ratio <- guy_table$Theoretical_slip/guy_table$Theoretical_length/1000

write.table(guy_table, file = 'Data/s2l.csv',row.names=FALSE,col.names=TRUE,sep = ',',quote = FALSE)



