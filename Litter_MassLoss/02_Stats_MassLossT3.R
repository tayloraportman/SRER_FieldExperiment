#Field Litter Bag Mass Loss
# T3

#Statistics
#Taylor Portman
#Mod from Betsy email Oct3
#18OCT22

#Read in libraries
library(ggplot2)
library(lme4)
library(lmerTest)
library(MuMIn)
library(dplyr)
library(sjPlot )

#Set working directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/MassLoss_T3_Clean.csv")

# Visualizing interactions ------------------------------------------------
#In Progress

#remove 3_E_NO_3 b/c 0 logit= -lnf
dat.1<-dat%>%
  filter(ID!= "3_E_NO_3")

#calculate means for each variable combo
means<-dat.1%>%
  group_by(LitterType, PatchType)%>%
  summarize_at(vars(logit), mean)

#Look at interaction effects between variables
ggplot()+
  geom_point(data=dat.1, aes(LitterType, logit, shape=CanopyStatus))+
  geom_line(data=means,(aes(LitterType, logit, group=PatchType, color=PatchType)))+
  scale_shape_manual(values=c(19,1))+
  scale_color_manual(values=c( "darkgoldenrod1","darkolivegreen"))+
  theme_bw()

  
#Litter type and patch type are meaningful, will need to use interaction in model

# Modeling ----------------------------------------------------------------

#Multiple regression model
#General linear model--- no random effects
model<-lm(logit~LitterType+ PatchType+ CanopyStatus+ LitterType*PatchType, 
           data=dat.1)
summary(model)
anova(model)
r.squaredGLMM(model)
tab_model(model)
#Linear mixed effects model-- random effect: MesquiteNum
model.rand<-lmer(logit~LitterType+ PatchType+ CanopyStatus+ LitterType*PatchType+ (1|MesquiteNum), 
           data=dat.1)
summary(model.rand)
anova(model.rand)
r.squaredGLMM(model.rand) 
#LitterType*PatchType: P=0.3714
tab_model(model.rand)

#Check which has the best fit: Comparing AICc
AICc(model, model.rand)
