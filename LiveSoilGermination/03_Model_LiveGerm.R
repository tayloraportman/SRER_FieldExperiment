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

#Figure save file path
figures= 'LiveSoilGermination/figures'

# Model building ----------------------------------------------------------
#normalize data
germ.norm<- germ.dat%>%
  mutate(seed= log(1+TotalSeeds))

hist(germ.dat$TotalSeeds)
hist(germ.norm$seed)

model1<-lm(seed~SoilType+ GrassType + PatchType + 
             CanopyStatus, data=germ.norm)
summary(model1)
anova(model1)
fileName = paste(figures, 'TabModel_SeedGerm.doc',sep = '/')
tab_model(model1, file= fileName)


model2<-lmer(seed~SoilType+ GrassType + PatchType + 
               CanopyStatus+ (1|MesquiteNum), data=germ.norm)
summary(model2)
anova(model2)
tab_model(model2)

AIC(model1, model2)
#Choose model 1 as best fit
#Higher R2 and lower AICc
