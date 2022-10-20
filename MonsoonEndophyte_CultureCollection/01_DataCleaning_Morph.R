#Monsoon Endophyte Culture Collection

#Data Cleaning
#Taylor Portman
#20OCT22

#Read in libraries
library(dplyr)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_raw/MonsoonCultureMorphotypes.csv")

#Save data_clean file path
data_clean = 'data_clean'

# Data Cleaning -----------------------------------------------------------
dat<-dat%>%
  na.omit()
# Export to data_clean ----------------------------------------------------
fileName = paste(data_clean, 'MonsoonMorphotypes_Clean.csv',sep = '/')
write.csv(dat, fileName )

bac<-filter(morphotypes, Type== "Bacteria")
fungi<-filter(morphotypes, Type== "Fungi")

fungi.count<- fungi%>%
  count(Morphotype, PlantOrigin, Plot)%>%
  mutate(morph= case_when(n<=1 ~ "Uniques", n>1 ~ Morphotype))%>%
  select(Morphotype, morph)

fungi.all<- left_join(fungi, fungi.count, by= "Morphotype")
# Export to data_clean ----------------------------------------------------
fileName = paste(data_clean, 'FungalMorph_Clean.csv',sep = '/')
write.csv(fungi.all, fileName )

bac.count<- bac%>%
  count(Morphotype, PlantOrigin, Plot)%>%
  mutate(morph= case_when(n<=1 ~ "Uniques", n>1 ~ Morphotype))%>%
  select(Morphotype, morph)

bac.all<- left_join(bac, bac.count, by= "Morphotype")
# Export to data_clean ----------------------------------------------------
fileName = paste(data_clean, 'BacteriaMorph_Clean.csv',sep = '/')
write.csv(bac.all, fileName )
