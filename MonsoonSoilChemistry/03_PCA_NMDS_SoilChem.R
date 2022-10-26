#Monsoon Soil Chemistry

# PCA/ NMDS
#Taylor Portman
#22OCT22

#Read in libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(tibble)
library(vegan)

#Set working directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
norm.matrix<-read.csv("data_clean/MonsoonSoilChem_NormalizedMatrix.csv", row.names = 1)
metadata<-read.csv("data_clean/MonsoonSoilMetadata_Clean.csv")


#Figure save file path
figures= 'MonsoonSoilChemistry/figures'

# PCA: CanopyStatus ---------------------------------------------------------------------
#PCA

matrix.norm<-scale(norm.matrix)
hist(matrix.norm)


results<-prcomp(matrix.norm)

biplot(results, scale = 0)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(results$rotation), results$rotation)
PCAloadings.sig<-PCAloadings
#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:15), var_explained) + 
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
  theme_classic() +
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
fileName = paste(figures, 'PCA_withSigArrows.png',sep = '/')
ggsave(fileName, pca_plot, dpi = 800,width= 18, height=12, units=c('cm'))

# PCA: SoilType ---------------------------------------------------------------------
#PCA
# Extract loadings of the variables
PCAloadings.sig<-PCAloadings

shapes<-c("Mesquite"=19, "Open"=1)
colors<-c("Live"="coral", "Sterile"="black")

# Plot Individuals PCA
pca_plot2 <- ggplot(pca_coordinates,
                   aes(x = PC1,
                       y = PC2)) +
  geom_segment(data = PCAloadings.sig, aes(x = 0, y = 0, xend = (PC1*10),
                                           yend = (PC2*10)), arrow = arrow(length = unit(1/2, "picas")),
               color = "grey")+
  theme_classic() +
  geom_text_repel(data=PCAloadings.sig, 
                  x = (PCAloadings.sig$PC1*10), 
                  y = (PCAloadings.sig$PC2*10),
                  label=PCAloadings.sig$Variables, max.overlaps = 12)+
  geom_point(aes(color = SoilType,
                 shape = CanopyStatus),
             size = 3) +
  scale_shape_manual(values=c(19,1))+
  scale_color_manual(values=colors)+
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))+
  labs(title = 'PCA plot: Soil Chemistry',
       x = pc1,
       y = pc2)
pca_plot2

#Save to figures folder
fileName = paste(figures, 'PCA_SoilType.png',sep = '/')
ggsave(fileName, pca_plot2, dpi = 800,width= 18, height=12, units=c('cm'))


# NMDS --------------------------------------------------------------------
nmds.matrix <- matrix.norm
dm.method <- 'bray'
# distance matrix by Bray 
dm <- vegdist(nmds.matrix, method=dm.method)
nmds <- metaMDS(dm,
                k = 2,
                maxit = 999,
                trymax = 500,
                wascores = TRUE)
stressplot(nmds)
# Extract nmds scores for plotting and add metadata
nmds.scores <- as.data.frame(scores(nmds, display = 'sites'))
nmds.scores <- rownames_to_column(nmds.scores, var = 'SampleID')
nmds.scores <- left_join(nmds.scores, metadata, by = 'SampleID')

colors<-c("Invasive"="goldenrod2", "Native"="dodgerblue", "Open"="black", "Mesquite"="darkolivegreen4")

#Plot NMDS
nmds_plot <- ggplot(nmds.scores,
                    aes(x = NMDS1,
                        y = NMDS2,
                        color = PatchType,
                        shape = CanopyStatus)) +
  geom_jitter(size = 3, width = 0.01) +
  stat_ellipse(aes(group=CanopyStatus, color=CanopyStatus))+
  scale_shape_manual(values=shapes)+
  scale_color_manual(values=colors) +
  theme_bw() +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))+
  labs(title = 'NMDS plot')
nmds_plot
#Save to figures folder
fileName = paste(figures, 'NMDS_PatchType_CanopyStatusElipse.png',sep = '/')
ggsave(fileName, nmds_plot, dpi = 800,width= 18, height=12, units=c('cm'))


#NMDS focus on Live vs Sterile Soils
nmds_plot2 <- ggplot(nmds.scores,
                     aes(x = NMDS1,
                         y = NMDS2,
                         color = SoilType,
                         shape = Plot)) +
  geom_jitter(size = 3, width = 0.01) +
  scale_color_manual(values=c("coral", "black")) +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))+
  labs(title = 'NMDS plot')
nmds_plot2
#Save to figures folder
fileName = paste(figures, 'NMDS_SoilType.png',sep = '/')
ggsave(fileName, nmds_plot2, dpi = 800,width= 18, height=12, units=c('cm'))
