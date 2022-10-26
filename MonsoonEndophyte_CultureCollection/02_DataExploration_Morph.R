#Monsoon Endophyte Culture Collection

#Data Exploration
#Taylor Portman
#20OCT22

#Read in libraries
library(dplyr)
library(ggplot2)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/MonsoonMorphotypes_Clean.csv")

#Figure save file path
figures= 'MonsoonEndophyte_CultureCollection/figures'

# Data Exploration --------------------------------------------------------
morphotypes<-dat%>%
  filter(Type!="Other")%>%
  na.omit()

count.morph<- morphotypes%>%
  count(Morphotype, PlantOrigin, Plot,Type)

isolation.freq.all<- morphotypes%>%
  filter(Type!="Other")%>%
  count(PlantOrigin, Plot, MesquiteNum, CulturePlate, Type)%>%
  

isolation.freq<- morphotypes%>%
  count(PlantOrigin, Plot, Type)

fig1<-ggplot(isolation.freq.all, aes(PlantOrigin, n, color=Type))+
  geom_point(position= position_dodge(width=0.75))+
  geom_boxplot()+
  facet_wrap(~Plot)+
  theme_bw()+
  labs(title="Average isolation frequency of 10 root segments per plot", 
       x= "Plant Species", y= "Count", 
       color= "Taxa")
fig1
#Save to figures folder
fileName = paste(figures, 'IsolationFrequency_byType.png',sep = '/')
ggsave(fileName, fig1, dpi = 800,width= 18, height=12, units=c('cm'))

fig2<-ggplot(morphotypes,aes(PlantOrigin, fill=Type))+
  geom_bar(position="fill")+
  facet_wrap(~Plot)+
  theme_classic()+
  labs(y="% Abundance", x= "Plant Host")
fig2
fileName = paste(figures, 'Abundance_byType.png',sep = '/')
ggsave(fileName, fig2, dpi = 800,width= 18, height=12, units=c('cm'))

#More bacteria in open plots than canopy plots for DICA




