# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Packages
library(plyr)
library(dplyr)

#######################################################

# Define directories
load("/Users/rujiabi/Desktop/Climate_Stock/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")
outputdir <- "~/Desktop/Climate_Stock/Bi_Code/B_F_OFL_dist/Data_preparation/output"

data <- read.csv(paste(outputdir, "0c_full_data_with_fishing_mortality.csv", sep="/"), as.is=T)

#####################################################################################################################

## Read data
bioparams <- bioparams 
bioparams_ERmsybest <- bioparams[bioparams$bioid=="ERmsybest-ratio",]  # 213 stocks, 246 assessments
bioparams_ERmsy <- bioparams[bioparams$bioid=="ERmsy-ratio",]  # 167 stocks, 199 assessments
bioparams_ERmgt <- bioparams[bioparams$bioid=="ERmgtbest-ratio",]  # 39 stocks, 55 assessments
bioparams_Fmsy <- bioparams[bioparams$bioid=="Fmsy-1/yr",]  # 254 stocks, 514 assessments
bioparams_Fmgt <- bioparams[bioparams$bioid=="Fmgt-1/yr",]  # 233 stocks, 493 assessments
bioparams_Flim <- bioparams[bioparams$bioid=="Flim-1/yr",]  # 130 stocks, 298 assessments
bioparams_TBmsybest <- bioparams[bioparams$bioid=="TBmsybest-MT",]  # 205 stocks, 227 assessments
bioparams_SSBmsy <- bioparams[bioparams$bioid=="SSBmsy-MT",]  # 222 stocks, 278 assessments
bioparams_SSBmgt <- bioparams[bioparams$bioid=="SSBmgt-MT",]  # 181 stocks, 461 assessments

# Add ERmsybest to assessment
data_with_ERmsybest <- data %>% 
  left_join(select(bioparams_ERmsybest, biovalue, bionotes, assessid), by=c("assessid")) %>% 
  rename(ERmsybest=biovalue, ERmsybest_notes=bionotes) 

# Add ERmsy to assessment
data_with_ERmsy <- data_with_ERmsybest %>% 
  left_join(select(bioparams_ERmsy, biovalue, bionotes, assessid), by=c("assessid")) %>% 
  rename(ERmsy=biovalue, ERmsy_notes=bionotes) 

ddcheck <- data_with_ERmsy[!is.na(data_with_ERmsy$ERmsy) & is.na(data_with_ERmsy$ERmsybest),]  # 0 row, so use ERmsybest only

# Add ERmgt to assessment
data_with_ERmgt <- data_with_ERmsybest %>% 
  left_join(select(bioparams_ERmgt, biovalue, bionotes, assessid), by=c("assessid")) %>% 
  rename(ERmgt=biovalue, ERmgt_notes=bionotes) 

# Add Fmsy to assessment
data_with_Fmsy <- data_with_ERmgt %>% 
  left_join(select(bioparams_Fmsy, biovalue, bionotes, assessid), by=c("assessid")) %>% 
  rename(Fmsy=biovalue, Fmsy_notes=bionotes) 

# Add Fmgt to assessment
data_with_Fmgt <- data_with_Fmsy %>% 
  left_join(select(bioparams_Fmgt, biovalue, bionotes, assessid), by=c("assessid")) %>% 
  rename(Fmgt=biovalue, Fmgt_notes=bionotes) 

# Add Flim to assessment
data_with_Flim <- data_with_Fmgt %>% 
  left_join(select(bioparams_Flim, biovalue, bionotes, assessid), by=c("assessid")) %>% 
  rename(Flim=biovalue, Flim_notes=bionotes) 

# Add TBmsybest to assessment
data_with_TBmsybest <- data_with_Flim %>% 
  left_join(select(bioparams_TBmsybest, biovalue, bionotes, assessid), by=c("assessid")) %>% 
  rename(TBmsybest=biovalue, TBmsybest_notes=bionotes) 

# Add SSBmsy to assessment
data_with_SSBmsy <- data_with_TBmsybest %>% 
  left_join(select(bioparams_SSBmsy, biovalue, bionotes, assessid), by=c("assessid")) %>% 
  rename(SSBmsy=biovalue, SSBmsy_notes=bionotes) 

# Add SSBmgt to assessment
data_with_SSBmgt <- data_with_SSBmsy %>% 
  left_join(select(bioparams_SSBmgt, biovalue, bionotes, assessid), by=c("assessid")) %>% 
  rename(SSBmgt=biovalue, SSBmgt_notes=bionotes) 

#########################################################################################################

## Data check
ddcheck <- data_with_SSBmgt[!is.na(data_with_SSBmgt$ERmsybest) | !is.na(data_with_SSBmgt$Fmsy),]
length(unique(ddcheck$stockid))  # 240 stocks
length(unique(ddcheck$assessid))  # 548 stocks

ddcheck <- data_with_SSBmgt[!is.na(data_with_SSBmgt$ERmsybest) & !is.na(data_with_SSBmgt$Fmsy),]
length(unique(ddcheck$stockid))  # 34 stocks
length(unique(ddcheck$assessid))  # 34 stocks

ddcheck <- data_with_SSBmgt[!is.na(data_with_SSBmgt$ERmsybest),]
length(unique(ddcheck$stockid))  # 113 stocks
length(unique(ddcheck$assessid))  # 146 stocks

ddcheck <- data_with_SSBmgt[!is.na(data_with_SSBmgt$Fmsy),]
length(unique(ddcheck$stockid))  # 181 stocks
length(unique(ddcheck$assessid))  # 436 stocks

ddcheck <- data_with_SSBmgt[!is.na(data_with_SSBmgt$ERmsybest) & is.na(data_with_SSBmgt$Fmsy),]
length(unique(ddcheck$stockid))  # 84 stocks
length(unique(ddcheck$assessid))  # 112 stocks

#########################################################################################################

# Export full data
write.csv(data_with_SSBmgt, paste(outputdir, "0d_full_data_with_msy.csv", sep="/"), row.names=F)

