#Live Soil Germination
#Robin Bradley Experiment Fall 2022

#Figures
#Taylor Portman
#19OCT22

#Read in libraries
library(ggplot2)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/LiveGermDat_Clean.csv")

#Figure save file path
figures= 'LiveSoilGermination/figures'

# Plot Figures ------------------------------------------------------------

fig1<-dat%>%
  ggplot(aes(SoilType, TotalSeeds))+
  geom_point(aes(shape=CanopyStatus))+
  geom_boxplot(aes(fill=GrassType), alpha=0.7)+
  facet_wrap(~GrassType)+
  scale_shape_manual(values=c(19,1))+
  scale_fill_manual(values=c("royalblue","darkolivegreen", "darkgoldenrod1"))+
  theme_classic()
fig1

#Save to figures folder
fileName = paste(figures, 'LiveSoilGermbySoilType.png',sep = '/')
ggsave(fileName, fig1, dpi = 800,width= 18, height=12, units=c('cm'))





