#Monsoon Soil Endophytes

#AnalysisType Comparisons
#Taylor Portman
#21OCT22

#Read in libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggpubr)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/MonsoonSoilChem_Clean.csv")
metadata<-read.csv("data_clean/MonsoonSoilMetadata_Clean.csv")
chem.long<-read.csv("data_clean/MonsoonSoilChem_Long_Clean.csv")

#Figure save file path
figures= 'MonsoonSoilChemistry/figures'


# Significant AnalysisType Comparisons ------------------------------------
sig.analysis1<-chem.long%>%
  filter(AnalysisType=="Ca.ppm."| AnalysisType=="Fe.ppm." | AnalysisType== "K.ppm."|
           AnalysisType=="ElectricalConductivity.dS.m."| AnalysisType=="Ni.ppm."| AnalysisType=="NO3.N.ppm."|
           AnalysisType=="PO4.P.ppm."| AnalysisType=="SO4.S.ppm."| AnalysisType=="Zn.ppm.")

sig.analysis2<-chem.long%>%
  filter(AnalysisType=="Mn.ppm."|AnalysisType=="NO3.N.ppm.")

# Sig Analysis 1: CanopyStatus  -----------------------------------------------------------

compare_means(values ~ CanopyStatus, data = sig.analysis1, 
              group.by = "AnalysisType")

colors= c("Invasive"= "goldenrod2", "Native"= "dodgerblue")
shapes= c("Mesquite"= 19, "Open"=1)
p <- ggboxplot(sig.analysis1, x = "CanopyStatus", y = "values",
               color = "PatchType", palette = colors,
               add = "jitter", shape= "CanopyStatus",
               facet.by = "AnalysisType", scales= "free",
               short.panel.labs = FALSE)+
  labs(y="ppm")
# Use only p.format as label. Remove method name.
p<-p + stat_compare_means(label = "p.signif" ,label.x.npc= 0.5, label.y.npc = .9)+ 
  theme_classic()+
  stat_compare_means(label="p.format", label.x.npc= .70, label.y.npc=0.8)+
  scale_shape_manual(values=shapes)
p
fileName = paste(figures, 'Boxplot_SigChemAnalysis_CanopyStatus.png',sep = '/')
ggsave(fileName, p, dpi = 800,width= 18, height=12, units=c('cm'))

# Sig Analysis 2: SoilType  -----------------------------------------------------------

compare_means(values ~ SoilType, data = sig.analysis2, 
              group.by = "AnalysisType")

colors= c("Live"= "coral", "Sterile"= "black")
shapes= c("Mesquite"= 19, "Open"=1)
p <- ggboxplot(sig.analysis2, x = "SoilType", y = "values",
               color = "SoilType", palette = colors,
               add = "jitter", shape= "CanopyStatus",
               facet.by = "AnalysisType", scales= "free",
               short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
p<-p + stat_compare_means(label = "p.signif" ,label.x.npc= 0.5, label.y.npc = .9)+ 
  theme_classic()+
  stat_compare_means(label="p.format", label.x.npc= .70, label.y.npc=0.99)+
  scale_shape_manual(values=shapes)
p
fileName = paste(figures, 'Boxplot_SigChemAnalysis_SoilType.png',sep = '/')
ggsave(fileName, p, dpi = 800,width= 18, height=12, units=c('cm'))
