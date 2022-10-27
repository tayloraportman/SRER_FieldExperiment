#Monsoon Soil Chemistry

#Model: Morphotype + Soil Chem
#Taylor Portman
#24OCT22

#Read in libraries
library(dplyr)
library(ggplot2)
library(tidyr)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
chem<-read.csv("data_clean/MonsoonSoilChem_Clean.csv")
metadata<-read.csv("data_clean/MonsoonSoilMetadata_Clean.csv")
chem.long<-read.csv("data_clean/MonsoonSoilChem_Long_Clean.csv")
morph<-read.csv("data_clean/MonsoonMorphotypes_Clean.csv")
fungi<-read.csv("data_clean/FungalMorph_Clean.csv")
bac<-read.csv("data_clean/BacteriaMorph_Clean.csv")

#Figure save file path
figures= 'MonsoonSoilChemistry/figures'


# Add ChemLive data to Morphotype data ------------------------------------

chem.live<-chem%>%
  filter(SoilType== "Live")%>%
  mutate(Patch_CanStatus= paste0(MesquiteNum, "_",PatchType, "_", CanopyStatus))

morph<- morph%>%
  mutate(MesquiteNum= paste0("M",morph$MesquiteNum))%>%
  mutate(PatchType= 
           case_when(PlantOrigin=="DICA"~"Native",
                     PlantOrigin=="ERLE"~"Invasive"))%>%
  mutate(CanopyStatus=
           case_when(Plot=="CAN"~"Mesquite",
                     Plot=="OPN"~"Open"))%>%
  mutate(Patch_CanStatus= paste0(MesquiteNum, "_",PatchType, "_", CanopyStatus))


# Modeling ----------------------------------------------------------------
morph.chem<- morph%>%
  filter(Type=="Fungi")%>%
  select(IsolateID, Morphotype, Patch_CanStatus)%>%
  left_join(chem.live, by="Patch_CanStatus")%>%
  count(Morphotype, Patch_CanStatus, PatchType, CanopyStatus, pH, Mg.ppm., 
        Cu.ppm.)



mod<- lm(n~Morphotype + PatchType+ CanopyStatus+  pH, data= morph.chem)
anova(mod)
tab_model(mod)

#pH impacts morphotype abundance
#P=.04935

boxplot(pH~PatchType+CanopyStatus, data=morph.chem)


colors= c("Invasive"= "goldenrod2", "Native"= "dodgerblue")
shapes= c("Mesquite"= 19, "Open"=1)
morph.chem.2%>%
  ggplot(aes(x= Patch_Can, y= Fe.ppm.))+
  geom_boxplot(aes(fill=PatchType), alpha=0.7)+
  geom_jitter(aes(shape=CanopyStatus), width=0.1)+
  scale_fill_manual(values=colors)+
  scale_shape_manual(values=shapes)+
  theme_classic()+
  stat_compare_means(label = "p.signif" ,label.x.npc= 0.5, label.y.npc = .9)+ 
  stat_compare_means(label="p.format", label.x.npc= .90, label.y.npc=1)

morph.chem.2<- chem%>%
mutate(Patch_Can= paste0(PatchType, "_", CanopyStatus))

compare_means(K.ppm.~Patch_Can, data=morph.chem.2)  



