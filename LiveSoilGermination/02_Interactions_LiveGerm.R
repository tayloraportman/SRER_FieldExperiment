#Live Soil Germination
#Robin Bradley Experiment Fall 2022

#Statistics
#Taylor Portman
#19OCT22

#Read in libraries
library(dplyr)
library(lme4)
library(sjPlot )

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/LiveGermDat_Clean.csv")

# Interaction Effects -----------------------------------------------------
##SoilType * PatchType
ixn1<- germ.dat %>% 
  group_by(PatchType, SoilType, .add=TRUE) %>%
  summarise(
    means=mean(TotalSeeds))

ggplot()+
  geom_point(data=germ.dat, aes(SoilType, TotalSeeds, color=GrassType, shape=CanopyStatus))+
  geom_line(data=ixn1, aes(SoilType, means, group=PatchType, color=PatchType))+
  scale_shape_manual(values=c(19,1))+
  scale_fill_manual(values=c("darkolivegreen", "darkgoldenrod1"))+
  theme_bw()
##SoilType * GrassType
ixn2<- germ.dat %>%
  group_by(SoilType, GrassType, .add=TRUE) %>%
  summarise(
    means=mean(TotalSeeds))

ggplot()+
  geom_point(data=germ.dat, aes(SoilType, TotalSeeds, color=GrassType, shape=CanopyStatus))+
  geom_line(data=ixn2, aes(SoilType, means, group=GrassType, color=GrassType))+
  scale_shape_manual(values=c(19,1))+
  theme_bw()

##Soil Type * PatchType 
ixn1<- germ.dat %>% 
  group_by(SoilType, PatchType, .add=TRUE) %>%
  summarise(
    means=mean(TotalSeeds))

ggplot()+
  geom_point(data=germ.dat, aes(PatchType, TotalSeeds, color=GrassType, shape=CanopyStatus))+
  geom_line(data=ixn1, aes(PatchType, means, group=SoilType))+
  scale_shape_manual(values=c(19,1))+
  scale_fill_manual(values=c("darkolivegreen", "darkgoldenrod1"))+
  theme_bw()

##PatchType * CanopyStatus
ixn2<- germ.dat %>%
  group_by(CanopyStatus, PatchType, .add=TRUE) %>%
  summarise(
    means=mean(TotalSeeds))

ggplot()+
  geom_point(data=germ.dat, aes(CanopyStatus, TotalSeeds, color=GrassType, shape=CanopyStatus))+
  geom_line(data=ixn2, aes(CanopyStatus, means, group=PatchType, color=PatchType))+
  scale_shape_manual(values=c(19,1))+
  theme_bw()
