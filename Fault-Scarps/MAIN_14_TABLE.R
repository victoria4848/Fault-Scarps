# MAIN_14_TABLE
# Make table for supplementary information for parameters

len_sd.df <- readRDS('Results/len_offset_data.RDS') # Make in MAIN_10_RATIOS

library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")


# TABLE 1: Overall offsets / lengths / ratios table for faults 
len_sd.df <- change_names(len_sd.df)

dips <- 90 # dip at surface
ta <- subset(len_sd.df, select = c(name,f_lengths,L_slip_dis,slip_mean,slip_slip_dis ))
ta$f_lengths <- ta$f_lengths/1000
ta$ratio_u   <- len_sd.df$slip_mean    /len_sd.df$f_length  * 1/10^-5
ta$ratio_l   <- len_sd.df$slip_slip_dis/(len_sd.df$L_slip_dis*1000) * 1/10^-5
max100 <- round(as.numeric(len_sd.df$offset_max) / sind(dips))
max90  <- round(as.numeric(len_sd.df$offset_90)  / sind(dips))
ta$max_ob_slip <- paste0(max100,',',max90)
ta$max_slip_slip_dis <- as.numeric(as.character(len_sd.df$max_offset_slip_dis))  / sind(dips)


ta$name <- str_replace_all(ta$name, "\u{00E4}", '\\\\"{a}')  
ta$name <- str_replace_all(ta$name, "\u{00F6}", '\\\\"{o}') 


names(ta)    <- c('Name','L\\textsubscript{ob}','L\\textsubscript{th}', 'O\\textsubscript{ob}','O\\textsubscript{th}',
                  'R\\textsubscript{ob}, 10\\textsuperscript{-5}','R\\textsubscript{th}, 10\\textsuperscript{-5}','S\\textsubscript{max,ob}','S\\textsubscript{max,th}')
digs <- c(1,   0, 0,  0,   1,   1,  0, 0,  0,  0 )
disp <- c('d','s','f','f','f','f','f','f','s','f')
xtab <- xtable(ta,digits = digs, display = disp)
print(xtab,include.rownames=FALSE,sanitize.text.function = identity)




# TABLE 2: MAGNITUDES 
#Fault Name  & Length (km) & Average Offset (m) & Dip ($\degree$) & Seismogenic Depth (km) & Width$^{b}$ \\

t2 <- readRDS('Results/mag.df.RDS')
t2$f_lengths <- t2$f_lengths/1000 ; t2$f_lengthsL <- t2$f_lengthsL/1000 ; t2$f_lengthsU <- t2$f_lengthsU/1000
t2$name <- as.character(t2$name)
t2 <- t2[order(t2$name),]
t2$widthL <- t2$widthL/1000 ; t2$widthM <- t2$widthM/1000 ; t2$widthU <- t2$widthU/1000
t2 <- subset(t2, select = c(name, Mag_ave,Mag_min,Mag_max,
                           f_lengths,f_lengthsU,
                           slip_mean,slip_lower,slip_upper,
                           widthM,widthL,widthU))

t2$name <- str_replace_all(t2$name, "\u{00E4}", '\\\\"{a}')  
t2$name <- str_replace_all(t2$name, "\u{00F6}", '\\\\"{o}') 
names(t2) <- c('Name','M\\textsubscript{b}','M\\textsubscript{l}','M\\textsubscript{u}',
               'L\\textsubscript{b,l}','L\\textsubscript{u}',
               'S\\textsubscript{b}','S\\textsubscript{l}','S\\textsubscript{u}',
               'W\\textsubscript{b}','W\\textsubscript{l}','W\\textsubscript{u}')
digs <- c(1,   0, 1,  1,   1,   0,  0,  1,  1,1,0,0,0 )
disp <- c('d','s','f','f','f','f','f','f','f','f','f','f','f')
xtab <- xtable(t2,digits = digs, display = disp)
print(xtab,include.rownames=FALSE,sanitize.text.function = identity)




# library(stringr)
# df$name <- str_replace_all(df$name, "é", "\\\\'{e}")  
# df$name <- str_replace_all(df$name, "á", "\\\\'{a}")
