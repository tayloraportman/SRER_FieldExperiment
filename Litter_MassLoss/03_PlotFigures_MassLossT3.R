#Field Litter Bag Mass Loss
# T3

#Plot Figures
#LitterType * PatchType interactions
#Taylor Portman
#Mod of Betsy email 3OCT22
#18OCT22

#Read in libraries
library(ggplot2)
library(dplyr)
library(ggpubr)

#Set working directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/MassLoss_T3_Clean.csv")

#Figure save file path
figures= 'Litter_MassLoss/figures'
# Plot Figures ------------------------------------------------------------

fig1<-dat%>%
  ggplot(aes(LitterType, MassLoss))+
  geom_point(aes(shape=CanopyStatus))+
  geom_boxplot(aes(fill=LitterType), alpha=0.7)+
  facet_wrap(~PatchType)+
  scale_shape_manual(values=c(19,1))+
  scale_fill_manual(values=c("dodgerblue3", "darkgoldenrod1"))+
  theme_bw()
  
fig1
#Save to figures folder
fileName = paste(figures, 'MassLossT3.png',sep = '/')
ggsave(fileName, fig1, dpi = 800,width= 18, height=12, units=c('cm'))



