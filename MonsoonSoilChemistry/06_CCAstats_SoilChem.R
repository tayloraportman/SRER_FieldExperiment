#RDA

#Data Exploration
#Taylor Portman
#21OCT22

#Read in libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(vegan)
library(tibble)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
norm.matrix<-read.csv("data_clean/MonsoonSoilChem_NormalizedMatrix.csv",row.names= 1)

metadata<-read.csv("data_clean/MonsoonSoilMetadata_Clean.csv", row.names=2)

#Figure save file path
figures= 'MonsoonSoilChemistry/figures'

# CC ---------------------------------------------------------------------
metadata<-metadata%>%
  select(MesquiteNum, SoilType, PatchType, CanopyStatus)%>%
  mutate(Patch_Can= paste0(PatchType,"_",CanopyStatus))
norm.matrix<-norm.matrix+10

MesquiteNum = metadata[,1]
SoilType = metadata[,2]
PatchType = metadata[,3]
CanopyStatus = metadata[,4]
Patch_Can= metadata[,5]

MesquiteNum = as.factor(MesquiteNum)
SoilType = as.factor(SoilType)
CanopyStatus = as.factor(CanopyStatus)
PatchType = as.factor(PatchType)
Patch_Can = as.factor(Patch_Can)

#For RDA, use RDA instead CCA
CCA<- cca (formula= norm.matrix ~ SoilType+CanopyStatus+PatchType)

CCA2<- cca(formula= norm.matrix~ MesquiteNum*CanopyStatus*PatchType*SoilType)

CCA_stat1<- anova.cca(CCA, by= "terms")

CCA_stat2<- anova.cca(CCA2, by="terms")

eigenvals(CCA)

plot_CCA<- plot(CCA, scaling=2)


# Ploting -----------------------------------------------------------------


elements<-as.data.frame(plot_CCA$species)
sites<-as.data.frame(plot_CCA$sites)
centroids<-as.data.frame(plot_CCA$centroids)

#add metadata 
sites<-sites%>%
  rownames_to_column(var="SampleID")%>%
  left_join(metadata, by="SampleID")
centroids<-centroids%>%
  rownames_to_column(var="variable")
elements<-elements%>%
  rownames_to_column(var="elements")

shapes<-c("Mesquite"=19, "Open"=1)
colors<-c("Invasive"="goldenrod2", "Native"="dodgerblue")

plot(plot_CCA, type="n", las=1)
text(chem_pca, display = "sites", 
     labels=row.names(chem_s))
ordihull(plot_CCA, CanopyStatus, display="sites", label=T,
         lwd=2, col=c("blue","black"))

# Plotting ----------------------------------------------------------------
#PlotCentroids

plot1<-ggplot()+
  geom_point(data=sites, aes(CCA1, CCA2, color=PatchType, shape=CanopyStatus), size=3)+
  scale_color_manual(values=colors)+
  scale_shape_manual(values=shapes)+
  geom_point(data=centroids, aes(CCA1, CCA2, color= "black"), size=1)+
  geom_segment(data = centroids, aes(x = 0, y = 0, xend = (CCA1*3),
                                    yend = (CCA2*3)), arrow = arrow(length = unit(1/2, "picas")),
               color = "black")+
  geom_text_repel(data=centroids, 
                  x = (centroids$CCA1*3), 
                  y = (centroids$CCA2*3),
                  label=centroids$variable, max.overlaps = 12)+
  theme_classic()
plot1
#Save to figures folder
fileName = paste(figures, 'CCA_ExplanatoryCentroidswithSigArrows.png',sep = '/')
ggsave(fileName, plot1, dpi = 800,width= 18, height=12, units=c('cm'))


#element data
colors<-c("Live"="coral", "Sterile"="black")
plot2<- ggplot()+
  geom_point(data=sites, aes(CCA1, CCA2, color=SoilType, shape=CanopyStatus), size=3)+
  scale_color_manual(values=colors)+
  scale_shape_manual(values=shapes)+
  geom_segment(data = elements, aes(x = 0, y = 0, xend = (CCA1*20),
                                           yend = (CCA2*20)), arrow = arrow(length = unit(1/2, "picas")),
               color = "black")+
  theme_bw() +
  geom_text_repel(data=elements, 
                  x = (elements$CCA1*20), 
                  y = (elements$CCA2*20),
                  label=elements$elements, max.overlaps = 12, size=3)+

  theme_classic()
plot2
                      
#Save to figures folder
fileName = paste(figures, 'CCA_ElementswithSArrows.png',sep = '/')
ggsave(fileName, plot2, dpi = 800,width= 18, height=12, units=c('cm'))                   
