#Monsoon Soil Endophytes

#Data Exploration
#Taylor Portman
#21OCT22

#Read in libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(tibble)
library(sjPlot)
library(lme4)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/MonsoonSoilChem_Clean.csv")
metadata<-read.csv("data_clean/MonsoonSoilMetadata_Clean.csv")
chem.long<-read.csv("data_clean/MonsoonSoilChem_Long_Clean.csv")
norm.matrix.live<-read.csv("data_clean/MonsoonSoilChem_NormalizedMatrix_LIVE.csv", row.names = 1)
norm.matrix<-read.csv("data_clean/MonsoonSoilChem_NormalizedMatrix.csv", row.names = 1)
#Figure save file path
figures= 'MonsoonSoilChemistry/figures'

# Modeling live vs sterile ------------------------------------------------
norm.matrix<-rownames_to_column(norm.matrix,"SampleID")
metadata<-metadata%>%
  select(!X)
chem<- norm.matrix%>%
  pivot_longer(cols=c('pH':'EC'), 
               names_to = 'AnalysisType', 
               values_to = 'values')%>%
  left_join(metadata, by="SampleID")

mad1<-lmer(values~CanopyStatus+ SoilType+ PatchType+ (1|MesquiteNum), data=chem)

anova(mad1)
anova(mad1)
fileName = paste(figures, 'TabModel_ChemistryAnalysisType.doc',sep = '/')
tab_model(mad1, file=fileName)
tab_model(mad1)

# Modeling LIVE----------------------------------------------------------------
norm.matrix.live<-rownames_to_column(norm.matrix.live,"SampleID")
metadata<-metadata%>%
  filter(SoilType=="Live")
chem<- norm.matrix.live%>%
  pivot_longer(cols=c('pH':'EC'), 
               names_to = 'AnalysisType', 
               values_to = 'values')%>%
  left_join(metadata, by="SampleID")
  
mad1<-lm(values~AnalysisType+ CanopyStatus+ PatchType, data=chem)

anova(mad1)
fileName = paste(figures, 'TabModel_ChemistryAnalysisLIVE.doc',sep = '/')
tab_model(mad1, file=fileName)
tab_model(mad1)



# Sig Analysis  -----------------------------------------------------------
sig.analysis<-chem.long%>%
  filter(SoilType=="Live")%>%
  filter(AnalysisType== "K.ppm."| AnalysisType== "Zn.ppm."| AnalysisType=="Fe.ppm."|
         AnalysisType=="Ni.ppm"| AnalysisType=="PO4.P.ppm.")
 
compare_means(values ~ CanopyStatus, data = sig.analysis, 
               group.by = "AnalysisType")
 
 colors= c("Invasive"= "goldenrod2", "Native"= "dodgerblue")
 shapes= c("Mesquite"= 19, "Open"=1)
 p <- ggboxplot(sig.analysis, x = "CanopyStatus", y = "values",
                color = "PatchType", palette = colors,
                add = "jitter", shape= "CanopyStatus",
                facet.by = "AnalysisType", scales= "free",
                short.panel.labs = FALSE)
 # Use only p.format as label. Remove method name.
 p<-p + stat_compare_means(label = "p.signif" ,label.x.npc= 0.5, label.y.npc = .9)+ 
   theme_classic()+
   stat_compare_means(label="p.format", label.x.npc= .70, label.y.npc=0.99)+
   scale_shape_manual(values=shapes)
 p
 fileName = paste(figures, 'Boxplot_SigChemAnalysis_CanopyStatus.png',sep = '/')
 ggsave(fileName, p, dpi = 800,width= 18, height=12, units=c('cm'))
 
 

# -------------------------------------------------------------------------

 p <- ggboxplot(sig.analysis, x = "Plot", y = "values",
                color = "CanopyStatus", palette = colors,
                add = "jitter",
                facet.by = "AnalysisType", scales= "free",
                short.panel.labs = FALSE)
 # Use only p.format as label. Remove method name.
 p + stat_compare_means(label = "p.signif" ,label.x.npc= 0.5, label.y.npc = .9)+ theme_classic()
 

Fe.ppm.<-chem.long%>%
  filter(AnalysisType== "K.ppm.")
#Fe.ppm. much higher in Mesqute, especially Native
ggplot(Fe.ppm.)+
  geom_boxplot(aes(PatchType, values, fill=CanopyStatus))

#K.ppm.-- Canopy status, Mesquite higher (especially Native)
ggplot(K.ppm.)+
  geom_boxplot(aes(PatchType, values, fill=CanopyStatus))
#Cu, Open higher especially native
ggplot(Cu.ppm.)+
  geom_boxplot(aes(PatchType, values, fill=CanopyStatus))
#CEC.meq.100--Canopy Status lower
ggplot(CEC.meq.100)+
  geom_boxplot(aes(PatchType, values, fill=CanopyStatus))

#Ca.ppm.-- Canopy Status lower
ggplot(Ca.ppm.)+
  geom_boxplot(aes(PatchType, values, fill=CanopyStatus))

# B.ppm. no patern
ggplot(B.ppm.)+
  geom_boxplot(aes(PatchType, values, fill=CanopyStatus))
#pH-- PatchType Native slightly lower
ggplot(pH)+
  geom_boxplot(aes(PatchType, values, color=PatchType))



