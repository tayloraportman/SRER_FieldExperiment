#Monsoon Endophyte Culture Collection

#Stats
#Taylor Portman
#20OCT22

#Read in libraries
library(dplyr)
library(ggplot2)
library(lme4)
library(sjPlot )

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_clean/MonsoonMorphotypes_Clean.csv")
fungi<-read.csv("data_clean/FungalMorph_Clean.csv")
bac<-read.csv("data_clean/BacteriaMorph_Clean.csv")

#Figure save file path
figures= 'MonsoonEndophyte_CultureCollection/figures'

# Modeling ----------------------------------------------------------------

#Compare Type count by Plant Origin and Plot
count.origin.plot<- dat%>%
  filter(Type!= "Other")%>%
  count(Type, PlantOrigin, Plot)

ixn<- count.origin.plot %>%
  group_by(Type, PlantOrigin,Plot, .add=TRUE) %>%
  summarise(
    means=mean(n))
ggplot(count.origin.plot)+
  geom_point(aes(PlantOrigin, n, color=Type, shape=Plot))+
  geom_line(data=ixn, aes(PlantOrigin, means, group=Type, color=Type))+
  scale_shape_manual(values=c(19,1))+
  theme_bw()

origin.plot.mod<- lm(n~Type + PlantOrigin+ Plot, data= count.origin.plot)
anova(origin.plot.mod)
tab_model(origin.plot.mod)

#Compare Morphotype count by Plant Origin and Plot
count<- fungi%>%
  count(Morphotype, PlantOrigin, Plot)

ixn<- count %>%
  group_by(Morphotype, PlantOrigin,Plot, .add=TRUE) %>%
  summarise(
    means=mean(n))

ggplot(count)+
  geom_point(aes(Plot, n, color=Morphotype, shape=Plot))+
  geom_line(data=ixn, aes(Plot, means, group=Morphotype, color=Morphotype))+
  scale_shape_manual(values=c(19,1))+
  theme_bw()

mod<- lm(n~Morphotype + PlantOrigin+ Plot, data= count)
anova(mod)
tab_model(mod)

#Compare Morphotype count by Plant Origin and Plot
count<- bac%>%
  count(Morphotype, PlantOrigin, Plot)

ixn<- count %>%
  group_by(Morphotype, PlantOrigin,Plot, .add=TRUE) %>%
  summarise(
    means=mean(n))

ggplot(count)+
  geom_point(aes(Plot, n, color=Morphotype, shape=Plot))+
  geom_line(data=ixn, aes(Plot, means, group=Morphotype, color=Morphotype))+
  scale_shape_manual(values=c(19,1))+
  theme_bw()

mod<- lm(n~Morphotype + PlantOrigin+ Plot, data= count)
anova(mod)
tab_model(mod)
