#Live Soil Germination
#Robin Bradley Experiment Fall 2022

#Statistics
#Taylor Portman
#19OCT22

#Read in libraries
library(dplyr)
library(lme4)
library(sjPlot )
library(car)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
germ.dat<-read.csv("data_clean/LiveGermDat_Clean.csv")

#Figure save file path
figures= 'LiveSoilGermination/figures'

# Model building ----------------------------------------------------------
#normalize data
germ.norm<- germ.dat%>%
  mutate(logit= logit((TotalSeeds/10), percent))

hist(germ.dat$TotalSeeds)
hist(germ.norm$logit)

# BOCU --------------------------------------------------------------------
germBOCU<-germ.norm%>%
  filter(Grass=="BOCU")
modelBOCU<-lm(logit~ SoilType+ PatchType + CanopyStatus, 
           data=germBOCU)

anova(modelBOCU)
tab_model(modelBOCU)
fileName = paste(figures, 'TabModel_BOCU.doc',sep = '/')
tab_model(modelBOCU, file= fileName)

# ERIN --------------------------------------------------------------------
germERIN<-germ.norm%>%
  filter(Grass=="ERIN")
modelERIN<-lm(logit~ SoilType+ PatchType + CanopyStatus, 
              data=germERIN)

anova(modelERIN)
tab_model(modelERIN)
fileName = paste(figures, 'TabModel_ERIN.doc',sep = '/')
tab_model(modelERIN, file= fileName)
# ERLE --------------------------------------------------------------------
germERLE<-germ.norm%>%
  filter(Grass=="ERLE")
modelERLE<-lm(logit~ SoilType+ PatchType + CanopyStatus, 
              data=germERLE)

anova(modelERLE)
tab_model(modelERLE)
fileName = paste(figures, 'TabModel_ERLE.doc',sep = '/')
tab_model(modelERLE, file= fileName)
#########
