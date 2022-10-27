#Live Soil Germination
#Robin Bradley Experiment Fall 2022

#Figures
#Taylor Portman
#19OCT22

#Read in libraries
library(ggplot2)
library(dplyr)
library(ggpubr)
#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/LiveGermDat_Clean.csv")

#Figure save file path
figures= 'LiveSoilGermination/figures'

# Plot Figures ------------------------------------------------------------
dat<-dat%>%
  mutate(seed= ((TotalSeeds/10)*100))
Pval<-data.frame(GrassType=c("BOCU (Native)", "ERIN (Native)", "ERLE (Invasive)"),
                 Pval= c("P<0.001", "P=0.022", "P=0.531"),
                 sig= c("***", "*", ""))
fig1<-dat%>%
  ggplot(aes(SoilType, y=seed))+
  geom_jitter(aes(shape=CanopyStatus, group= SoilType),width = 0.1)+
  geom_boxplot(aes(fill=GrassType), alpha=0.7)+
  facet_wrap(~GrassType)+
  scale_shape_manual(values=c(19,1))+
  scale_fill_manual(values=c("royalblue","darkolivegreen", "darkgoldenrod1"))+
  labs(y="% Germination")+
  theme_classic()+
  geom_text(data = Pval, aes(x = 1.5, y = 110, label = Pval), fontface="bold")+
  geom_text(data=Pval, aes(x=1.5, y= 105, label=sig), fontface="bold")
fig1


#Save to figures folder
fileName = paste(figures, 'LiveSoilGermbySoilType.png',sep = '/')
ggsave(fileName, fig1, dpi = 800,width= 18, height=12, units=c('cm'))


live<- dat%>%
  filter(SoilType=="Live")

fig2<-live%>%
  ggplot(aes(PatchType, seed))+
  geom_jitter(aes(shape=CanopyStatus, group= PatchType), width=0.1)+
  geom_boxplot(aes(fill=CanopyStatus) ,alpha=0.7)+
  facet_wrap(~GrassType)+
  scale_shape_manual(values=c(19,1))+
  scale_fill_manual(values=c("darkolivegreen","black" ))+
  labs(y="% Germination")+
  theme_classic()
fig2

#Save to figures folder
fileName = paste(figures, 'LiveSoilGermbySoilType_andCanopyStatus.png',sep = '/')
ggsave(fileName, fig2, dpi = 800,width= 18, height=12, units=c('cm'))


