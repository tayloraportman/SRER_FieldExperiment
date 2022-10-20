#Live Sol Germination
#Robin Bradley Experiment Fall 2022

#Data Cleaning
#Taylor Portman
#19OCT22

#Read in libraries
library(dplyr)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_raw/LiveSoilGermDat.csv")

#Save data_clean file path
data_clean = 'data_clean'

# Data cleaning -----------------------------------------------------------
germ.dat<-dat%>%
  na.omit()

germ.dat<- germ.dat%>%
  filter(Grass!="DICA")%>%
  mutate(MesquiteNum=
           paste("M", MesquiteNum, sep=""))%>%
  select(ID, Grass, SoilType, Plot, MesquiteNum, TotalSeeds)%>%
  mutate(CanopyStatus= 
           case_when(Plot=="IO"| Plot=="NO"~"Open",
                     Plot=="IC"| Plot== "NC"~"Mesquite"))%>%
  mutate(PatchType=
           case_when(Plot=="IO"| Plot=="IC"~"Invasive",
                     Plot=="NO" | Plot== "NC" ~"Native"))%>%
  mutate(GrassType=
           case_when(Grass=="ERLE"~"ERLE (Invasive)",
                     Grass=="ERIN"~"ERIN (Native)",
                     Grass== "BOCU"~"BOCU (Native)"))%>%
  na.omit()

means <- germ.dat %>% 
  group_by(GrassType, PatchType, CanopyStatus, SoilType, .add=TRUE) %>%
  summarise(
    means=mean(TotalSeeds))

# Export to data_clean ----------------------------------------------------
fileName = paste(data_clean, 'LiveGermDat_Clean.csv',sep = '/')
write.csv(germ.dat, fileName )
