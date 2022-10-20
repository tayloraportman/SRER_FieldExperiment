#Monsoon Endophyte Culture Collection

#Diversity, Richness, Evenness
#Taylor Portman
#20OCT22

#Read in libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(vegan)
library(cowplot)
#------------------
library(VennDiagram)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/MonsoonMorphotypes_Clean.csv")
fungi<-read.csv("data_clean/FungalMorph_Clean.csv")
bac<-read.csv("data_clean/BacteriaMorph_Clean.csv")

#Figure save file path
figures= 'MonsoonEndophyte_CultureCollection/figures'


# Fungi: Diversity --------------------------------------------------------
pop = fungi%>%
  count(Morphotype, PlantOrigin, Plot)%>%
  mutate(PlantPlot= paste0(PlantOrigin , "_" , Plot))%>%
  select(-PlantOrigin, -Plot)%>%
  pivot_wider(names_from="Morphotype", values_from = "n", values_fill =0)%>%
  select(-PlantPlot)

env = fungi%>%
  count(Morphotype, PlantOrigin, Plot)%>%
  mutate(PlantPlot= paste0(PlantOrigin , "_" , Plot))%>%
  pivot_wider(names_from="Morphotype", values_from = "n", values_fill =0)%>%
  select(PlantOrigin, Plot, PlantPlot)


H<- diversity(pop)
richness<-specnumber(pop)
evenness<-H/log(richness)

alpha<-cbind(shannon=H, richness= richness, pielou=evenness, env)

#Plot Data
plot.shan <- ggplot(alpha, aes(x = PlantPlot, y = shannon, colour = PlantPlot)) +
  geom_point(size = 3) +
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  ylab("Shannon's H'") + 
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

plot.rich <-ggplot(alpha, aes(x = PlantPlot, y = richness, colour = PlantPlot)) +
  geom_point(size = 3) +
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  ylab("Species Richness") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

plot.even <- ggplot(alpha, aes(x = PlantPlot, y = evenness, colour = PlantPlot)) +
  geom_point(size = 3) +
  scale_colour_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  ylab("Pielou's Evenness") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

legend <- get_legend(plot.even)

plot_grid(plot.shan + theme(legend.position = "none"), plot.rich + theme(legend.position = "none"), plot.even + theme(legend.position = "none"),ncol = 3)


## ADD CHEM DATA??!!

# Venn Diagram ------------------------------------------------------------

fungi.venn<-fungi%>%
  select(PlantOrigin, Morphotype)
fileName = paste(figures, 'Venn_Fungi.png',sep = '/')

venn.diagram(
  x=list(
    fungi.venn%>% filter(PlantOrigin=="DICA")%>% select(Morphotype)%>% unlist(),
    fungi.venn%>% filter(PlantOrigin=="ERLE")%>% select(Morphotype)%>% unlist()),
  category.names=c("DICA", "ERLE"),
  filename= fileName,
  output=TRUE
)

