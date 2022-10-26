#RDA/ CCA analysis 

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
dat<-read.csv("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment/data_raw/RDA_SoilChem.csv", 
              header=TRUE)

metadata<-read.csv("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment/data_raw/RDA_Metadata.csv")

#Figure save file path
figures= 'MonsoonSoilChemistry/figures'

# CC ---------------------------------------------------------------------
metadata<-metadata%>%
  mutate(Patch_Can= paste0(PatchType,"_",CanopyStatus))%>%
  filter(SoilType=="Live")

dat<-dat%>%
  left_join(metadata, by= 'SampleID')%>%
  filter(SoilType== "Live")%>%
  select(SampleID, pH:B.ppm.)

names <- dat[,-1]
rownames(dat) <- dat[,1]

dat<-dat%>%
  select (-SampleID)

col_old<- colnames(dat)
col_new<-gsub(pattern=".ppm.", replacement= "", x=col_old)
colnames(dat)<-col_new


scale_data<- as.data.frame(log(dat))
hist(dat$Fe)
hist(scale_data$Fe)

MesquiteNum = metadata[,3]
SoilType = metadata[,4]
CanopyStatus = metadata[,5]
PatchType = metadata[,6]
Patch_Can= metadata[,7]

MesquiteNum = as.factor(MesquiteNum)
SoilType = as.factor(SoilType)
CanopyStatus = as.factor(CanopyStatus)
PatchType = as.factor(PatchType)
Patch_Can = as.factor(Patch_Can)

#For RDA, use RDA instead CCA
CCA<- cca (formula= scale_data ~ CanopyStatus+PatchType)

CCA2<- cca(formula= scale_data~ CanopyStatus*PatchType)

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
  theme_bw()
plot1

#Save to figures folder
fileName = paste(figures, 'CCA_LiveOnly_ExplanatoryCentroidswithSigArrows.png',sep = '/')
ggsave(fileName, plot1, dpi = 800,width= 18, height=12, units=c('cm'))


#element data
plot2<- ggplot()+
  geom_point(data=sites, aes(CCA1, CCA2, color=PatchType, shape=CanopyStatus), size=3)+
  scale_color_manual(values=colors)+
  scale_shape_manual(values=shapes)+
  geom_segment(data = elements, aes(x = 0, y = 0, xend = (CCA1*10),
                                           yend = (CCA2*10)), arrow = arrow(length = unit(1/2, "picas")),
               color = "black")+
  theme_bw() +
  geom_text_repel(data=elements, 
                  x = (elements$CCA1*10), 
                  y = (elements$CCA2*10),
                  label=elements$elements, max.overlaps = 12, size=3)+

  theme_bw()
plot2
                      
                      
