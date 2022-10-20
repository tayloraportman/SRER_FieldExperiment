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
germ.dat<-read.csv("data_clean/LiveGermDat_Clean.csv")

# Model building ----------------------------------------------------------

model1<-lm(TotalSeeds~SoilType+ GrassType + PatchType + 
             CanopyStatus, data=germ.dat)
summary(model1)
anova(model1)
tab_model(model1)

model2<-lmer(TotalSeeds~SoilType+ GrassType + PatchType + 
               CanopyStatus+ (1|MesquiteNum), data=germ.dat)
summary(model2)
anova(model2)
tab_model(model2)

AICc(model1, model2)
#Choose model 1 as best fit
#Higher R2 and lower AICc
