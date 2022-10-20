#Field Litter Bag Mass Loss

#Data Cleaning
#Taylor Portman
#17OCT22

#Read in libraries
library(dplyr)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
T3MassLoss<-read.csv("data_raw/T3LitterBags.csv")

#Save data_clean file path
data_clean = '/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment/data_clean'


# Data cleaning -----------------------------------------------------------

T3MassLoss$ID<-T3MassLoss$LitterbagCode
T3MassLoss$ScaledMassLoss<- T3MassLoss$MassLoss/T3MassLoss$InitialWeight.g

T3dat<- T3MassLoss%>%
  mutate(MesquiteNum=
           paste("M", Mesquite., sep=""))%>%
  select(ID, Grass, Plot, MesquiteNum, InitialWeight.g, MassLoss, ScaledMassLoss)%>%
  mutate(CanopyStatus= 
           case_when(Plot=="IO"| Plot=="NO"~"Open",
                     Plot=="IC"| Plot== "NC"~"Mesquite"))%>%
  mutate(PatchType=
           case_when(Plot=="IO"| Plot=="IC"~"Invasive",
                     Plot=="NO" | Plot== "NC" ~"Native"))%>%
  mutate(LitterType=
           case_when(Grass=="D"~"DICA (Native)",
                     Grass=="E"~"ERLE (Invasive)"))%>%
  na.omit()


# Normalization -----------------------------------------------------------

hist(T3dat$ScaledMassLoss)

T3dat.norm<-T3dat%>%
  mutate(logit=
           log(ScaledMassLoss/(1-ScaledMassLoss)))

hist(T3dat.norm$logit)

# Export to data_clean ----------------------------------------------------
fileName = paste(data_clean, 'MassLoss_T3_Clean.csv',sep = '/')
write.csv(T3dat.norm, fileName )


  