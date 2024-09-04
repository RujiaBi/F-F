# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Packages
library(plyr)
library(dplyr)

##############################################################################################################

# Define directories
load("/Users/rujiabi/Desktop/Climate_Stock/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")
outputdir <- "~/Desktop/Climate_Stock/Bi_Code/B_F_OFL_dist/Data_preparation/output"

dd_2ass <- read.csv(paste(outputdir, "0g_1_data_2_assess_msy_ofl.csv", sep="/"), as.is=T)
dd_3ass <- read.csv(paste(outputdir, "0g_2_data_3_assess_msy_ofl.csv", sep="/"), as.is=T)
dd_4ass <- read.csv(paste(outputdir, "0g_3_data_4_assess_msy_ofl.csv", sep="/"), as.is=T)
dd_5ass <- read.csv(paste(outputdir, "0g_4_data_5_assess_msy_ofl.csv", sep="/"), as.is=T)
dd_6ass <- read.csv(paste(outputdir, "0g_5_data_6_assess_msy_ofl.csv", sep="/"), as.is=T)
dd_7ass <- read.csv(paste(outputdir, "0g_6_data_7_assess_msy_ofl.csv", sep="/"), as.is=T)
dd_8ass <- read.csv(paste(outputdir, "0g_7_data_8_assess_msy_ofl.csv", sep="/"), as.is=T)

# Find records with problems
dd_6ass[dd_6ass$assessid=="WGCSE-CODVIa-1981-2012-CHING",]$F_ty <- NA
dd_2ass[dd_2ass$assessid=="MARAM-CRLOBSTERSA7-1910-2012-Johnston",]$F_ty <- NA
dd_2ass[dd_2ass$assessid=="NZMFishLOBSTERWG-RROCKLOBSTERCRA3-1945-2007-JENSEN",]$F_ty <- NA
dd_7ass[dd_7ass$assessid=="WGNSDS-SOLEIS-1968-2011-NEUBAUER",]$F_ty <- NA
dd_2ass[dd_2ass$assessid=="ISC-PACBTUNA-1952-2012-PONS",]$Fmsy <- NA
dd_2ass[dd_2ass$stockid=="SARDSA",]$B_by_Bmsy <- NA
dd_2ass[dd_2ass$stockid=="WAREHOUESE",]$Bmsy <- NA

#####################################################################################################################

## Data with 2 assessments ##
dd_2ass$stock_comparison <- paste(dd_2ass$stockid, dd_2ass$ty_order, sep="_")
data_2ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_2ass$stock_comparison)), ncol=23))
colnames(data_2ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "period", "ty", 
                         "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old")
data_2ass$stock_comparison <- unique(dd_2ass$stock_comparison)
data_2ass$stock_comparison_code <- as.numeric(as.factor(data_2ass$stock_comparison))
for (i in 1:length(unique(dd_2ass$stock_comparison))){
  data_2ass[i,1] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$stockid)
  data_2ass[i,3] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$RegionCode)
  data_2ass[i,4] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$ty_order)
  data_2ass[i,7] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$period)
  data_2ass[i,8] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$ty)
  data_2ass[i,9] <- paste(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$method_pdf,
                          dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$method_pdf,
                          sep="->")
  data_2ass[i,10] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$B_ty
  data_2ass[i,11] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$B_ty
  data_2ass[i,12] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$F_ty
  data_2ass[i,13] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$F_ty
  data_2ass[i,14] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$Bmsy
  data_2ass[i,15] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$Bmsy
  data_2ass[i,16] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$Fmsy
  data_2ass[i,17] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$Fmsy
  data_2ass[i,18] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$B_by_Bmsy
  data_2ass[i,19] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_2ass[i,20] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$F_by_Fmsy
  data_2ass[i,21] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_2ass[i,22] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$OFL_ty
  data_2ass[i,23] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$OFL_ty 
}

data_2ass$stockid_code <- as.numeric(as.factor(data_2ass$stockid))

data_2ass$ty_code <- NA
for (i in 1:length(unique(data_2ass$stockid))){
  ty_vector <- sort(unique(data_2ass[data_2ass$stockid==unique(data_2ass$stockid)[i],]$ty))
  for(j in 1:length(ty_vector)){
    data_2ass[data_2ass$stockid==unique(data_2ass$stockid)[i] & data_2ass$ty==ty_vector[j],]$ty_code <- match(ty_vector, sort(unique(ty_vector)))[j]
  }
}

data_2ass$num_assess <- 2


## Data with 3 assessments ##
dd_3ass$stock_comparison <- paste(dd_3ass$stockid, dd_3ass$ty_order, sep="_")
data_3ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_3ass$stock_comparison)), ncol=23))
colnames(data_3ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "period", "ty", 
                         "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old")
data_3ass$stock_comparison <- unique(dd_3ass$stock_comparison)
data_3ass$stock_comparison_code <- as.numeric(as.factor(data_3ass$stock_comparison))
for (i in 1:length(unique(dd_3ass$stock_comparison))){
  data_3ass[i,1] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$stockid)
  data_3ass[i,3] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$RegionCode)
  data_3ass[i,4] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$ty_order)
  data_3ass[i,7] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$period)
  data_3ass[i,8] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$ty)
  data_3ass[i,9] <- paste(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$method_pdf,
                          dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$method_pdf,
                          sep="->")
  data_3ass[i,10] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$B_ty
  data_3ass[i,11] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$B_ty
  data_3ass[i,12] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$F_ty
  data_3ass[i,13] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$F_ty
  data_3ass[i,14] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$Bmsy
  data_3ass[i,15] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$Bmsy
  data_3ass[i,16] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$Fmsy
  data_3ass[i,17] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$Fmsy
  data_3ass[i,18] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$B_by_Bmsy
  data_3ass[i,19] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_3ass[i,20] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$F_by_Fmsy
  data_3ass[i,21] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_3ass[i,22] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$OFL_ty
  data_3ass[i,23] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$OFL_ty 
}

data_3ass$stockid_code <- as.numeric(as.factor(data_3ass$stockid))

data_3ass$ty_code <- NA
for (i in 1:length(unique(data_3ass$stockid))){
  ty_vector <- sort(unique(data_3ass[data_3ass$stockid==unique(data_3ass$stockid)[i],]$ty))
  for(j in 1:length(ty_vector)){
    data_3ass[data_3ass$stockid==unique(data_3ass$stockid)[i] & data_3ass$ty==ty_vector[j],]$ty_code <- match(ty_vector, sort(unique(ty_vector)))[j]
  }
}

data_3ass$num_assess <- 3


## Data with 4 assessments ##
dd_4ass$stock_comparison <- paste(dd_4ass$stockid, dd_4ass$ty_order, sep="_")
data_4ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_4ass$stock_comparison)), ncol=23))
colnames(data_4ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "period", "ty", 
                         "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old")
data_4ass$stock_comparison <- unique(dd_4ass$stock_comparison)
data_4ass$stock_comparison_code <- as.numeric(as.factor(data_4ass$stock_comparison))
for (i in 1:length(unique(dd_4ass$stock_comparison))){
  data_4ass[i,1] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$stockid)
  data_4ass[i,3] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$RegionCode)
  data_4ass[i,4] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$ty_order)
  data_4ass[i,7] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$period)
  data_4ass[i,8] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$ty)
  data_4ass[i,9] <- paste(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$method_pdf,
                          dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$method_pdf,
                          sep="->")
  data_4ass[i,10] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$B_ty
  data_4ass[i,11] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$B_ty
  data_4ass[i,12] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$F_ty
  data_4ass[i,13] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$F_ty
  data_4ass[i,14] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$Bmsy
  data_4ass[i,15] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$Bmsy
  data_4ass[i,16] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$Fmsy
  data_4ass[i,17] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$Fmsy
  data_4ass[i,18] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$B_by_Bmsy
  data_4ass[i,19] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_4ass[i,20] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$F_by_Fmsy
  data_4ass[i,21] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_4ass[i,22] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$OFL_ty
  data_4ass[i,23] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$OFL_ty 
}

data_4ass$stockid_code <- as.numeric(as.factor(data_4ass$stockid))

data_4ass$ty_code <- NA
for (i in 1:length(unique(data_4ass$stockid))){
  ty_vector <- sort(unique(data_4ass[data_4ass$stockid==unique(data_4ass$stockid)[i],]$ty))
  for(j in 1:length(ty_vector)){
    data_4ass[data_4ass$stockid==unique(data_4ass$stockid)[i] & data_4ass$ty==ty_vector[j],]$ty_code <- match(ty_vector, sort(unique(ty_vector)))[j]
  }
}

data_4ass$num_assess <- 4


## Data with 5 assessments ##
dd_5ass$stock_comparison <- paste(dd_5ass$stockid, dd_5ass$ty_order, sep="_")
data_5ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_5ass$stock_comparison)), ncol=23))
colnames(data_5ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "period", "ty", 
                         "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old")
data_5ass$stock_comparison <- unique(dd_5ass$stock_comparison)
data_5ass$stock_comparison_code <- as.numeric(as.factor(data_5ass$stock_comparison))
for (i in 1:length(unique(dd_5ass$stock_comparison))){
  data_5ass[i,1] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$stockid)
  data_5ass[i,3] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$RegionCode)
  data_5ass[i,4] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$ty_order)
  data_5ass[i,7] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$period)
  data_5ass[i,8] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$ty)
  data_5ass[i,9] <- paste(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$method_pdf,
                          dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$method_pdf,
                          sep="->")
  data_5ass[i,10] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$B_ty
  data_5ass[i,11] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$B_ty
  data_5ass[i,12] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$F_ty
  data_5ass[i,13] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$F_ty
  data_5ass[i,14] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$Bmsy
  data_5ass[i,15] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$Bmsy
  data_5ass[i,16] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$Fmsy
  data_5ass[i,17] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$Fmsy
  data_5ass[i,18] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$B_by_Bmsy
  data_5ass[i,19] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_5ass[i,20] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$F_by_Fmsy
  data_5ass[i,21] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_5ass[i,22] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$OFL_ty
  data_5ass[i,23] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$OFL_ty 
}

data_5ass$stockid_code <- as.numeric(as.factor(data_5ass$stockid))

data_5ass$ty_code <- NA
for (i in 1:length(unique(data_5ass$stockid))){
  ty_vector <- sort(unique(data_5ass[data_5ass$stockid==unique(data_5ass$stockid)[i],]$ty))
  for(j in 1:length(ty_vector)){
    data_5ass[data_5ass$stockid==unique(data_5ass$stockid)[i] & data_5ass$ty==ty_vector[j],]$ty_code <- match(ty_vector, sort(unique(ty_vector)))[j]
  }
}

data_5ass$num_assess <- 5


## Data with 6 assessments ##
dd_6ass$stock_comparison <- paste(dd_6ass$stockid, dd_6ass$ty_order, sep="_")
data_6ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_6ass$stock_comparison)), ncol=23))
colnames(data_6ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "period", "ty", 
                         "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old")
data_6ass$stock_comparison <- unique(dd_6ass$stock_comparison)
data_6ass$stock_comparison_code <- as.numeric(as.factor(data_6ass$stock_comparison))
for (i in 1:length(unique(dd_6ass$stock_comparison))){
  data_6ass[i,1] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$stockid)
  data_6ass[i,3] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$RegionCode)
  data_6ass[i,4] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$ty_order)
  data_6ass[i,7] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$period)
  data_6ass[i,8] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$ty)
  data_6ass[i,9] <- paste(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$method_pdf,
                          dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$method_pdf,
                          sep="->")
  data_6ass[i,10] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$B_ty
  data_6ass[i,11] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$B_ty
  data_6ass[i,12] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$F_ty
  data_6ass[i,13] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$F_ty
  data_6ass[i,14] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$Bmsy
  data_6ass[i,15] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$Bmsy
  data_6ass[i,16] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$Fmsy
  data_6ass[i,17] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$Fmsy
  data_6ass[i,18] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$B_by_Bmsy
  data_6ass[i,19] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_6ass[i,20] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$F_by_Fmsy
  data_6ass[i,21] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_6ass[i,22] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$OFL_ty
  data_6ass[i,23] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$OFL_ty 
}

data_6ass$stockid_code <- as.numeric(as.factor(data_6ass$stockid))

data_6ass$ty_code <- NA
for (i in 1:length(unique(data_6ass$stockid))){
  ty_vector <- sort(unique(data_6ass[data_6ass$stockid==unique(data_6ass$stockid)[i],]$ty))
  for(j in 1:length(ty_vector)){
    data_6ass[data_6ass$stockid==unique(data_6ass$stockid)[i] & data_6ass$ty==ty_vector[j],]$ty_code <- match(ty_vector, sort(unique(ty_vector)))[j]
  }
}

data_6ass$num_assess <- 6


## Data with 7 assessments ##
dd_7ass$stock_comparison <- paste(dd_7ass$stockid, dd_7ass$ty_order, sep="_")
data_7ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_7ass$stock_comparison)), ncol=23))
colnames(data_7ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "period", "ty", 
                         "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old")
data_7ass$stock_comparison <- unique(dd_7ass$stock_comparison)
data_7ass$stock_comparison_code <- as.numeric(as.factor(data_7ass$stock_comparison))
for (i in 1:length(unique(dd_7ass$stock_comparison))){
  data_7ass[i,1] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$stockid)
  data_7ass[i,3] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$RegionCode)
  data_7ass[i,4] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$ty_order)
  data_7ass[i,7] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$period)
  data_7ass[i,8] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$ty)
  data_7ass[i,9] <- paste(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$method_pdf,
                          dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$method_pdf,
                          sep="->")
  data_7ass[i,10] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$B_ty
  data_7ass[i,11] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$B_ty
  data_7ass[i,12] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$F_ty
  data_7ass[i,13] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$F_ty
  data_7ass[i,14] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$Bmsy
  data_7ass[i,15] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$Bmsy
  data_7ass[i,16] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$Fmsy
  data_7ass[i,17] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$Fmsy
  data_7ass[i,18] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$B_by_Bmsy
  data_7ass[i,19] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_7ass[i,20] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$F_by_Fmsy
  data_7ass[i,21] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_7ass[i,22] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$OFL_ty
  data_7ass[i,23] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$OFL_ty 
}

data_7ass$stockid_code <- as.numeric(as.factor(data_7ass$stockid))

data_7ass$ty_code <- NA
for (i in 1:length(unique(data_7ass$stockid))){
  ty_vector <- sort(unique(data_7ass[data_7ass$stockid==unique(data_7ass$stockid)[i],]$ty))
  for(j in 1:length(ty_vector)){
    data_7ass[data_7ass$stockid==unique(data_7ass$stockid)[i] & data_7ass$ty==ty_vector[j],]$ty_code <- match(ty_vector, sort(unique(ty_vector)))[j]
  }
}

data_7ass$num_assess <- 7


## Data with 8 assessments ##
dd_8ass$stock_comparison <- paste(dd_8ass$stockid, dd_8ass$ty_order, sep="_")
data_8ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_8ass$stock_comparison)), ncol=23))
colnames(data_8ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "period", "ty", 
                         "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old")
data_8ass$stock_comparison <- unique(dd_8ass$stock_comparison)
data_8ass$stock_comparison_code <- as.numeric(as.factor(data_8ass$stock_comparison))
for (i in 1:length(unique(dd_8ass$stock_comparison))){
  data_8ass[i,1] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$stockid)
  data_8ass[i,3] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$RegionCode)
  data_8ass[i,4] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$ty_order)
  data_8ass[i,7] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$period)
  data_8ass[i,8] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$ty)
  data_8ass[i,9] <- paste(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$method_pdf,
                          dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$method_pdf,
                          sep="->")
  data_8ass[i,10] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$B_ty
  data_8ass[i,11] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$B_ty
  data_8ass[i,12] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$F_ty
  data_8ass[i,13] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$F_ty
  data_8ass[i,14] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$Bmsy
  data_8ass[i,15] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$Bmsy
  data_8ass[i,16] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$Fmsy
  data_8ass[i,17] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$Fmsy
  data_8ass[i,18] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$B_by_Bmsy
  data_8ass[i,19] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_8ass[i,20] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$F_by_Fmsy
  data_8ass[i,21] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_8ass[i,22] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$OFL_ty
  data_8ass[i,23] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$OFL_ty 
}

data_8ass$stockid_code <- as.numeric(as.factor(data_8ass$stockid))

data_8ass$ty_code <- NA
for (i in 1:length(unique(data_8ass$stockid))){
  ty_vector <- sort(unique(data_8ass[data_8ass$stockid==unique(data_8ass$stockid)[i],]$ty))
  for(j in 1:length(ty_vector)){
    data_8ass[data_8ass$stockid==unique(data_8ass$stockid)[i] & data_8ass$ty==ty_vector[j],]$ty_code <- match(ty_vector, sort(unique(ty_vector)))[j]
  }
}

data_8ass$num_assess <- 8


####################################################################################################################

## Put data together ##
data_full <- rbind(data_2ass, data_3ass, data_4ass, data_5ass, data_6ass, data_7ass, data_8ass)

####################################################################################################################

## Method unchange (1) or change (2) ##

table(data_full$MethodChange)
data_full$MethodChangeCode <- NA

data_full[data_full$MethodChange == "Adapt-type model (in ADMB)->Adapt-type model (in ADMB)" |
            data_full$MethodChange == "Age analytical assessment (SAM)->Age analytical assessment (SAM)" |
            data_full$MethodChange == "Age structured stock assessment ->Age structured stock assessment " |
            data_full$MethodChange == "Age- and length-based analytical assessment (Stock Synthesis 3)->Age- and length-based analytical assessment (Stock Synthesis 3)" |
            data_full$MethodChange == "Age-based analytic assessment (TSA)->Age-based analytic assessment (TSA)" |
            data_full$MethodChange == "Age-based analytical (FLICA)->Age-based analytical (FLICA)" |
            data_full$MethodChange == "Age-based analytical (Linked Separable Adapt VPA)->Age-based analytical (Linked Separable Adapt VPA)" |
            data_full$MethodChange == "Age-based analytical (SAM)->Age-based analytical (SAM)" |
            data_full$MethodChange == "Age-based analytical (seasonal XSA, SXSA)->Age-based analytical (seasonal XSA, SXSA)" |
            data_full$MethodChange == "Age-based analytical (seasonal XSA, SXSA)->Age-based analytical assessment (quarterly SAM model) " |
            data_full$MethodChange == "Age-based analytical (TASACS)->Age-based analytical (TASACS)" |
            data_full$MethodChange == "Age-based analytical assessment (ASAP)->Age-based analytical assessment (ASAP)" |
            data_full$MethodChange == "Age-based analytical assessment (FLICA)->Age-based analytical assessment (FLICA)" |
            data_full$MethodChange == "Age-based analytical assessment (SAM)->Age-based analytical assessment (SAM)" |
            data_full$MethodChange == "Age-based analytical assessment (SS3) ->Age-based analytical assessment (SS3) " |
            data_full$MethodChange == "Age-based analytical assessment (Stochastic Multispecies Model)->Age-based analytical assessment (Stochastic Multispecies Model)" |
            data_full$MethodChange == "Age-based analytical assessment (TSA) ->Age-based analytical assessment (TSA) " |
            data_full$MethodChange == "Age-based analytical assessment (XSA) ->Age-based analytical assessment (XSA) " |
            data_full$MethodChange == "Age-based analytical assessment (XSA)->Age-based analytical assessment (XSA)" |
            data_full$MethodChange == "Age-based analytical assessment XSA->Age-based analytical assessment XSA" |
            data_full$MethodChange == "Age-based analytical model (SAM)->Age-based analytical model (SAM)" |
            data_full$MethodChange == "Age-based analytical stochastic assessment (SAM) ->Age-based analytical stochastic assessment (SAM) " |
            data_full$MethodChange == "Age-based assessment (XSA)->Age-based assessment (XSA)" |
            data_full$MethodChange == "Age-length and sex-structured model->Age-length and sex-structured model" |
            data_full$MethodChange == "Analytical age-based assessment (TSA)->Analytical age-based assessment (TSA)" |
            data_full$MethodChange == "Analytical age-based assessment (XSA)->Analytical age-based assessment (XSA)" |
            data_full$MethodChange == "Analytical assessment (Gadget model)->Analytical assessment (Gadget model)" |
            data_full$MethodChange == "Analytical length-based assessment (Gadget model) ->Analytical length-based assessment (Gadget model) " |
            data_full$MethodChange == "ASAP (Age-Structured Assessment Programme; NOAA toolbox)->ASAP (Age-Structured Assessment Programme; NOAA toolbox)" |
            data_full$MethodChange == "Biomass dynamics model (Age-structured surplus production model)->Biomass dynamics model (Age-structured surplus production model)" |
            data_full$MethodChange == "Biomass dynamics model (Delay difference model)->Biomass dynamics model (Delay difference model)" |
            data_full$MethodChange == "Biomass dynamics model (Delay difference model)->Biomass dynamics model (The KLAMZ assessment model is based on the Deriso-Schnute delay-difference equation)" |
            data_full$MethodChange == "Biomass dynamics model (Surplus production model)->Biomass dynamics model (Surplus production model)" |
            data_full$MethodChange == "Custom length-based Bayesian Model->Custom length-based Bayesian Model" |
            data_full$MethodChange == "Length-based model (SS3) ->Length-based model (SS3) " |
            data_full$MethodChange == "Length\xd0age analytical assessment (GADGET)->Length\xd0age analytical assessment (GADGET)" |
            data_full$MethodChange == "Multi-stock length-based model->Multi-stock length-based model" |
            data_full$MethodChange == "Population dynamics (Extended Depletion-Based Stock Reduction Analysis)->Population dynamics (Extended Depletion-Based Stock Reduction Analysis)" |
            data_full$MethodChange == "SAM ->SAM " |
            data_full$MethodChange == "SAM with catch-at-age data and age-disaggregated indices ->SAM with catch-at-age data and age-disaggregated indices " |
            data_full$MethodChange == "Seasonal age-based analytical (SMS-effort)->Seasonal age-based analytical (SMS-effort)" |
            data_full$MethodChange == "Size-structured population dynamics model->Size-structured population dynamics model" |
            data_full$MethodChange == "State-space age-structured production model->State-space age-structured production model" |
            data_full$MethodChange == "State\xd0space age-structured assessment model (SAM) ->State\xd0space age-structured assessment model (SAM) " |
            data_full$MethodChange == "Statistical assessment model (XSAM)->Statistical assessment model (XSAM)" |
            data_full$MethodChange == "Statistical catch at age ->Statistical catch at age " |
            data_full$MethodChange == "Statistical catch at age model->Statistical catch at age model" |
            data_full$MethodChange == "Statistical catch at length model->Statistical catch at length model" |
            data_full$MethodChange == "Statistical catch-at-age (SAM) ->Statistical catch-at-age (SAM) " |
            data_full$MethodChange == "Statistical catch-at-age (SCAA)->Statistical catch-at-age (SCAA)" |
            data_full$MethodChange == "Statistical catch-at-age model with flexible selectivity functions ->Statistical catch-at-age model with flexible selectivity functions " |
            data_full$MethodChange == "Stock Synthesis v3.0 model->Stock Synthesis v3.0 model" |
            data_full$MethodChange == "Survey index->Survey index" |
            data_full$MethodChange == "Two-stage Bayesian biomass dynamic model (CBBM) assessment->Two-stage Bayesian biomass dynamic model (CBBM) assessment" |
            data_full$MethodChange == "Unknown->Unknown" |
            data_full$MethodChange == "VPA (XSA)->VPA (XSA)" |
            data_full$MethodChange == "VPA->VPA" |
            data_full$MethodChange == "XSA using age-disaggregated indices->XSA using landings-at- age data and age-disaggregated indices." |
            data_full$MethodChange == "XSA with catch-at-age data and age-disaggregated indices ->XSA with catch-at-age data and age-disaggregated indices " |
            data_full$MethodChange == "XSA using landings-at- age data and age-disaggregated indices.->XSA using landings-at- age data and age-disaggregated indices." |
            data_full$MethodChange == "SA with catch-at-age data and age-disaggregated indices ->XSA with catch-at-age data and age-disaggregated indices ",]$MethodChangeCode <- 1

data_full[data_full$MethodChange == "Age-based analytical (FLICA)->Age-based analytical assessment (SAM)" |
            data_full$MethodChange == "Age-based analytical (Linked Separable Adapt VPA)->Length- and age-based analytical assessment (Stock Synthesis 3)" |
            data_full$MethodChange == "Age-based analytical (TASACS)->Statistical assessment model (XSAM)" |
            data_full$MethodChange == "Age-based analytical assessment (FLICA)->Age-based analytical assessment (ASAP)" |
            data_full$MethodChange == "Age-based analytical assessment (SAM)->Analytical assessment (ASAP) " |
            data_full$MethodChange == "Age-based analytical assessment (XSA)->Age structured stock assessment " |
            data_full$MethodChange == "Age-based analytical assessment (XSA)->Age-based analytical assessment (SAM)" |
            data_full$MethodChange == "Age-based analytical assessment (XSA)->Statistical catch-at-age (SAM) " |
            data_full$MethodChange == "Age-based analytical assessment (XSA)->Statistical catch-at-age model with flexible selectivity functions " |
            data_full$MethodChange == "Age-based analytical assessment XSA->Age-based analytical assessment (SAM)" |
            data_full$MethodChange == "Biomass dynaic model (Surplus production model)->Stock Synthesis v3.0 model" |
            data_full$MethodChange == "Biomass dynamics model (Age-structured surplus production model)->Statistical catch at age model" |
            data_full$MethodChange == "Biomass dynamics model (Delay difference model)->Stock Synthesis v3.0 model" |
            data_full$MethodChange == "Biomass dynamics model (Surplus production model)->Random effects model" |
            data_full$MethodChange == "Biomass dynamics model (Surplus production model)->Statistical catch at age model" |
            data_full$MethodChange == "Spreadsheet assessment model used for Capelin->Model based on acoustic survey and prediction six months ahead to calculate spawning biomass" |
            data_full$MethodChange == "Statistical catch at age model->Age-length-structured (Gadget model)" |
            data_full$MethodChange == "Statistical catch at age model->Statistical catch at age model with changes in growth and age error matrix" |
            data_full$MethodChange == "Statistical catch at age model->Stock Synthesis v3.0 model" |
            data_full$MethodChange == "Statistical catch at length model->Statistical catch at age model" |
            data_full$MethodChange == "Stock Synthesis v2.0 model->Stock Synthesis v3.0 model" |
            data_full$MethodChange == "Survey index->Statistical catch at age model" |
            data_full$MethodChange == "VPA->Age analytical assessment (SAM)" |
            data_full$MethodChange == "VPA->Age-based analytical assessment (SAM)" |
            data_full$MethodChange == "VPA->Age-length-structured (Gadget model)" |
            data_full$MethodChange == "VPA->Analytical assessment (ASAP) " |
            data_full$MethodChange == "VPA->SAM " |
            data_full$MethodChange == "VPA->Statistical catch at age model" |
            data_full$MethodChange == "XSA using age-disaggregated indices->SAM with catch-at-age data and age-disaggregated indices " |
            data_full$MethodChange == "XSA using landings-at- age data and age-disaggregated indices.->SAM with catch-at-age data and age-disaggregated indices " |
            data_full$MethodChange == "XSA with catch-at-age data and age-disaggregated indices ->SAM with catch-at-age data and age-disaggregated indices ",]$MethodChangeCode <- 2

data_full[is.na(data_full$MethodChangeCode),]$MethodChangeCode  # 0
table(data_full$MethodChangeCode)

##################################################################################################

write.csv(data_full, paste(outputdir, "0h_data_full_model.csv", sep="/"), row.names=F)


