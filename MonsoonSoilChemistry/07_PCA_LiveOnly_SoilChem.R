#Monsoon Soil Chemistry

# PCA/ NMDS
#Taylor Portman
#22OCT22

#Read in libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)

#Set working directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/MonsoonSoilChem_Clean.csv")
metadata<-read.csv("data_clean/MonsoonSoilMetadata_Clean.csv")


#Figure save file path
figures= 'MonsoonSoilChemistry/figures'

# PCA: CanopyStatus ---------------------------------------------------------------------
#PCA
dat<-dat%>%
  filter(SoilType=="Live")
matrix<-dat%>%
  select(SampleID, pH, Ca.ppm.:B.ppm.)

matrix$EC<- dat$ElectricalConductivity.dS.m.

col_old<- colnames(matrix)
col_new<-gsub(pattern=".ppm.", replacement= "", x=col_old)
colnames(matrix)<-col_new
rownames(matrix) <- matrix[,1]
matrix<-matrix[,-1]

matrix.norm<-scale(matrix)
hist(matrix.norm)


results<-prcomp(matrix.norm)

biplot(results, scale = 0)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(results$rotation), results$rotation)
PCAloadings.sig<-PCAloadings
#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:12), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#Extract PC1 and PC2 for PCA plot
metadata$SampleID<- as.factor(metadata$SampleID)
pca_coordinates <- as_tibble(results$x)
pca_coordinates$SampleID <- rownames(results$x)
# Merge with metadata
pca_coordinates <- left_join(pca_coordinates, metadata, by ='SampleID')
# Prepare axis labels for PCA
pc1 <- paste0('PC1 (', round(var_explained[1]*100, digits = 2), '%)')
pc2 <- paste0('PC2 (', round(var_explained[2]*100, digits = 2), '%)')


shapes<-c("Mesquite"=19, "Open"=1)
colors<-c("Invasive"="goldenrod2", "Native"="dodgerblue")

# Plot Individuals PCA
pca_plot <- ggplot(pca_coordinates,
                   aes(x = PC1,
                       y = PC2)) +
  geom_segment(data = PCAloadings.sig, aes(x = 0, y = 0, xend = (PC1*10),
                                       yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")),
               color = "grey")+
  theme_bw() +
  geom_text_repel(data=PCAloadings.sig, 
                  x = (PCAloadings.sig$PC1*10), 
                  y = (PCAloadings.sig$PC2*10),
                  label=PCAloadings.sig$Variables, max.overlaps = 12)+
  #geom_text_repel(
   # label=pca_coordinates$MesquiteNum, max.overlaps = 12)+
  geom_point(aes(color = PatchType,
                 shape = CanopyStatus),
             size = 3) +
  scale_shape_manual(values=c(19,1))+
  scale_color_manual(values=colors)+
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))+
  labs(title = 'PCA plot: Soil Chemistry',
       x = pc1,
       y = pc2)
pca_plot

#Save to figures folder
fileName = paste(figures, 'PCA_LiveOnlywithSigArrows.png',sep = '/')
ggsave(fileName, pca_plot, dpi = 800,width= 18, height=12, units=c('cm'))

