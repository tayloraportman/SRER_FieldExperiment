#Monsoon Endophyte Culture Collection

#Stacked Bar Plots
#Taylor Portman
#20OCT22

#Read in libraries
library(dplyr)
library(ggplot2)
library(RColorBrewer)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/MonsoonMorphotypes_Clean.csv")
fungi<-read.csv("data_clean/FungalMorph_Clean.csv")
bac<-read.csv("data_clean/BacteriaMorph_Clean.csv")

#Figure save file path
figures= 'MonsoonEndophyte_CultureCollection/figures'
# Fungi: Stacked Bar ------------------------------------------------------

cols<-27
my.colors1<- colorRampPalette(brewer.pal(12, "Paired"))(cols)

fig1<-ggplot(fungi,aes(PlantOrigin, fill=Morphotype))+
  geom_bar(position="fill")+
  facet_wrap(~Plot)+
  scale_fill_manual(values= my.colors1)+
  theme_classic()+
  labs(title= "Fungi")
#Save to figures folder
fileName = paste(figures, 'StackedBar_MonsoonFungi.png',sep = '/')
ggsave(fileName, fig1, dpi = 800,width= 18, height=12, units=c('cm'))


cols<-16
my.colors<- colorRampPalette(brewer.pal(12, "Paired"))(cols)

ggplot(fungi ,aes(PlantOrigin, fill=morph))+
  geom_bar(position="fill")+
  facet_wrap(~Plot)+
  scale_fill_manual(values= my.colors)+
  theme_classic()
#Save to figures folder
fileName = paste(figures, 'StackedBar_MonsoonFungi_combUniques.png',sep = '/')
ggsave(fileName, fig2, dpi = 800,width= 18, height=12, units=c('cm'))

# Bacteria: Stacked Bar ---------------------------------------------------
cols<-45
my.colors2<- colorRampPalette(brewer.pal(12, "Paired"))(cols)

fig1<-ggplot(bac,aes(PlantOrigin, fill=Morphotype))+
  geom_bar(position=)+
  facet_wrap(~Plot)+
  scale_fill_manual(values= my.colors2)+
  theme_classic()+
  labs(title="Bacteria")
#Save to figures folder
fileName = paste(figures, 'StackedBar_MonsoonBacteria.png',sep = '/')
ggsave(fileName, fig1, dpi = 800,width= 18, height=12, units=c('cm'))

cols<-22
my.colors3<- colorRampPalette(brewer.pal(12, "Paired"))(cols)

fig2<-ggplot(bac,aes(PlantOrigin, fill=morph))+
  geom_bar(position="fill")+
  facet_wrap(~Plot)+
  scale_fill_manual(values= my.colors3)+
  theme_classic()+
  labs(title="Bacteria")
#Save to figures folder
fileName = paste(figures, 'StackedBar_MonsoonBacteria_combUniques.png',sep = '/')
ggsave(fileName, fig2, dpi = 800,width= 18, height=12, units=c('cm'))


