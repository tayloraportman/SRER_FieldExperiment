#Monsoon Soil Chemistry

#Data Cleaning
#Taylor Portman
#21OCT22

#Read in libraries
library(dplyr)
library(tidyr)

#Set workind directory
setwd("/Volumes/GoogleDrive/My Drive/projects/MastersThesis/R/SRER_FieldExperiment")

#Read in data
dat<-read.csv("data_raw/MonsoonSoilChem.csv")

#Save data_clean file path
data_clean = 'data_clean'
# Data cleaning -----------------------------------------------------------

chem<- dat%>%
  mutate(MesquiteNum=
           paste("M", MesquiteNum, sep=""))%>%
  mutate(CanopyStatus= 
           case_when(Plot=="IO"| Plot=="NO"~"Open",
                     Plot=="IC"| Plot== "NC"~"Mesquite"))%>%
  mutate(PatchType=
           case_when(Plot=="IO"| Plot=="IC"~"Invasive",
                     Plot=="NO" | Plot== "NC" ~"Native"))
col_old<- colnames(chem)
col_new<-gsub(pattern=".ppm.", replacement= "", x=col_old)
colnames(chem)<-col_new
# Export to data_clean ----------------------------------------------------
fileName = paste(data_clean, 'MonsoonSoilChem_Clean.csv',sep = '/')
write.csv(chem, fileName )

#Metadata
metadata<- chem%>%
  select(SampleID, MesquiteNum, Plot, SoilType, CanopyStatus, PatchType)
# Export to data_clean ----------------------------------------------------
fileName = paste(data_clean, 'MonsoonSoilMetadata_Clean.csv',sep = '/')
write.csv(metadata, fileName )



#Normalize data
matrix<-chem%>%
  select(SampleID, pH, Ca:B)
rownames(matrix) <- matrix[,1]
matrix<-matrix[,-1]
matrix$EC<- chem$ElectricalConductivity.dS.m.
matrix.norm<-scale(matrix)
hist(matrix.norm)

# Export to data_clean ----------------------------------------------------
fileName = paste(data_clean, 'MonsoonSoilChem_NormalizedMatrix.csv',sep = '/')
write.csv(matrix.norm, fileName )

#Normalize data-- Live Only
chem<-chem%>%
  filter(SoilType=="Live")
matrix<-chem%>%
  select(SampleID, pH, Ca:B)
rownames(matrix) <- matrix[,1]
matrix<-matrix[,-1]
matrix$EC<- chem$ElectricalConductivity.dS.m.
matrix.norm<-scale(matrix)
hist(matrix.norm)

# Export to data_clean ----------------------------------------------------
fileName = paste(data_clean, 'MonsoonSoilChem_NormalizedMatrix_LIVE.csv',sep = '/')
write.csv(matrix.norm, fileName )
  
#Data in Long form
chem.long<-dat%>%
  select(SampleID, pH:CEC.meq.100g.)%>%
  pivot_longer(cols=c('pH':'CEC.meq.100g.'), 
               names_to = 'AnalysisType', 
               values_to = 'values')%>%
  left_join(metadata, by='SampleID')
# Export to data_clean ----------------------------------------------------
fileName = paste(data_clean, 'MonsoonSoilChem_Long_Clean.csv',sep = '/')
write.csv(chem.long, fileName )
