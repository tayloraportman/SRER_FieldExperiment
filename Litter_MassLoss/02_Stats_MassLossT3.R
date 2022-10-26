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
library(effects)

#Set working directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/MassLoss_T3_Clean.csv")

#Figure save file path
figures= 'Litter_MassLoss//figures'
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
plot<-ggplot()+
  geom_point(data=dat.1, aes(LitterType, logit, shape=CanopyStatus))+
  geom_line(data=means,(aes(LitterType, logit, group=PatchType, color=PatchType)))+
  scale_shape_manual(values=c(19,1))+
  scale_color_manual(values=c( "goldenrod1","dodgerblue"))+
  theme_bw()
plot
#Save to figures folder
fileName = paste(figures, 'Interaction_LitterType*PatchType.png',sep = '/')
ggsave(fileName, plot, dpi = 800,width= 18, height=12, units=c('cm'))
  
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

# Add Chemistry Data ----NEED TO NORMALIZE- 25OCT22--------------------------------------------------
chem<-read.csv("data_clean/MonsoonSoilChem_Clean.csv")

dat.chem<-dat%>%
  left_join(chem, by="Plot")%>%
  filter(ID!= "3_E_NO_3")

#Linear mixed effects model--add Chem data
model.chem1<-lm(logit~LitterType+ PatchType.x+ CanopyStatus.x+ LitterType*PatchType.x+ 
                   Ca.ppm.+ Fe.ppm.+K.ppm.+Mg.ppm.+Mn.ppm.+Na.ppm., 
                 data=dat.chem)
summary(model.chem1)
anova(model.chem1)
r.squaredGLMM(model.chem1) 

tab_model(model.chem1)

#Add Mesquite Num as random effect
model.chem<-lmer(logit~LitterType+ PatchType.x+ CanopyStatus.x+ LitterType*PatchType.x+ 
                   Ca.ppm.+ Fe.ppm.+K.ppm.+Mg.ppm.+Mn.ppm.+Na.ppm.+(1|MesquiteNum.x), 
                 data=dat.chem)
summary(model.chem)
anova(model.chem)
r.squaredGLMM(model.chem) 

tab_model(model.chem)

#Check which has the best fit: Comparing AICc
AICc(model, model.rand, model.chem1,model.chem)
