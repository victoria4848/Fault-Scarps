## Plot Map with Finnish Fault Traces
# Country borders
finland  <- getData("GADM",country="Finland",level=1)
sweden  <- getData("GADM",country="Sweden",level=1)
norway  <- getData("GADM",country="Norway",level=1)


# Load fault traces
fault_traces_list <- grep(list.files(path='Faults_R/'), pattern=c('df|lengths'), inv=T, value=T)


# Plot
ggplot()+
  geom_polygon(data=finland, aes(x=long,y=lat), fill='white',color='black')+
  geom_polygon(data=china,fill="grey60",color="grey80")+
  coord_map(xlim=c(-1,1)+bbox(finland)["x",],ylim=c(-1,1)+bbox(finland)["y",])+
  scale_fill_discrete(guide="none")+
  theme_bw()+theme(panel.grid=element_blank())


