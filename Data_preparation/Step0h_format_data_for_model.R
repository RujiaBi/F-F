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

# Define period (1 = CV_short, 2 = CV_long)
dd_2ass$period <- ifelse(dd_2ass$if_diff_5yrs=="N", 1, 2)  
dd_3ass$period <- ifelse(dd_3ass$if_diff_5yrs=="N", 1, 2)
dd_4ass$period <- ifelse(dd_4ass$if_diff_5yrs=="N", 1, 2)
dd_5ass$period <- ifelse(dd_5ass$if_diff_5yrs=="N", 1, 2)
dd_6ass$period <- ifelse(dd_6ass$if_diff_5yrs=="N", 1, 2)
dd_7ass$period <- ifelse(dd_7ass$if_diff_5yrs=="N", 1, 2)
dd_8ass$period <- ifelse(dd_8ass$if_diff_5yrs=="N", 1, 2)

# Find records with problems (ER > 1)
dd_6ass[dd_6ass$assessid=="WGCSE-CODVIa-1981-2012-CHING",]$F_ty <- 1-exp(-dd_6ass[dd_6ass$assessid=="WGCSE-CODVIa-1981-2012-CHING",]$instF_ty)
dd_2ass[dd_2ass$assessid=="MARAM-CRLOBSTERSA7-1910-2012-Johnston",]$F_ty <- NA
dd_2ass[dd_2ass$assessid=="NZMFishLOBSTERWG-RROCKLOBSTERCRA3-1945-2007-JENSEN",]$F_ty <- NA
dd_7ass[dd_7ass$assessid=="WGNSDS-SOLEIS-1968-2011-NEUBAUER",]$F_ty <- 1-exp(-dd_7ass[dd_7ass$assessid=="WGNSDS-SOLEIS-1968-2011-NEUBAUER",]$instF_ty)
dd_7ass[dd_7ass$assessid=="WGNSDS-SOLEIS-1968-2011-NEUBAUER",]$F_ty_minus1 <- 1-exp(-dd_7ass[dd_7ass$assessid=="WGNSDS-SOLEIS-1968-2011-NEUBAUER",]$instF_ty_minus1)
dd_2ass[dd_2ass$assessid=="ISC-PACBTUNA-1952-2012-PONS",]$Fmsy <- NA
dd_2ass[dd_2ass$stockid=="SARDSA",]$B_by_Bmsy <- NA
dd_2ass[dd_2ass$stockid=="WAREHOUESE",]$Bmsy <- NA

#####################################################################################################################

## Data with 2 assessments ##
dd_2ass$stock_comparison <- paste(dd_2ass$stockid, dd_2ass$ty_order, sep="_")
data_2ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_2ass$stock_comparison)), ncol=31))
colnames(data_2ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "interval", "period", 
                         "ty", "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old", "mu_B", "mu_F",
                         "mu_Bmsy", "mu_Fmsy", "mu_B_by_Bmsy", "mu_F_by_Fmsy", "mu_OFL")
data_2ass$stock_comparison <- unique(dd_2ass$stock_comparison)
data_2ass$stock_comparison_code <- as.numeric(as.factor(data_2ass$stock_comparison))
for (i in 1:length(unique(dd_2ass$stock_comparison))){
  data_2ass[i,1] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$stockid)
  data_2ass[i,3] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$region)
  data_2ass[i,4] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$ty_order)
  data_2ass[i,7] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$interval)
  data_2ass[i,8] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$period)
  data_2ass[i,9] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$ty)
  data_2ass[i,10] <- NA
  data_2ass[i,11] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$B_ty
  data_2ass[i,12] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$B_ty
  data_2ass[i,13] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$F_ty
  data_2ass[i,14] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$F_ty
  data_2ass[i,15] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$Bmsy
  data_2ass[i,16] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$Bmsy
  data_2ass[i,17] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$Fmsy
  data_2ass[i,18] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$Fmsy
  data_2ass[i,19] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$B_by_Bmsy
  data_2ass[i,20] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_2ass[i,21] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$F_by_Fmsy
  data_2ass[i,22] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_2ass[i,23] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$OFL_ty
  data_2ass[i,24] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$OFL_ty 
  data_2ass[i,25] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$B_ty
  data_2ass[i,26] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$F_ty
  data_2ass[i,27] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$Bmsy
  data_2ass[i,28] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$Fmsy
  data_2ass[i,29] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$B_by_Bmsy
  data_2ass[i,30] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$F_by_Fmsy
  data_2ass[i,31] <- dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,5],]$assess_order),]$OFL_ty
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
data_3ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_3ass$stock_comparison)), ncol=31))
colnames(data_3ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "interval", "period", 
                         "ty", "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old", "mu_B", "mu_F",
                         "mu_Bmsy", "mu_Fmsy", "mu_B_by_Bmsy", "mu_F_by_Fmsy", "mu_OFL")
data_3ass$stock_comparison <- unique(dd_3ass$stock_comparison)
data_3ass$stock_comparison_code <- as.numeric(as.factor(data_3ass$stock_comparison))
for (i in 1:length(unique(dd_3ass$stock_comparison))){
  data_3ass[i,1] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$stockid)
  data_3ass[i,3] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$region)
  data_3ass[i,4] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$ty_order)
  data_3ass[i,7] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$interval)
  data_3ass[i,8] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$period)
  data_3ass[i,9] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$ty)
  data_3ass[i,10] <- NA
  data_3ass[i,11] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$B_ty
  data_3ass[i,12] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$B_ty
  data_3ass[i,13] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$F_ty
  data_3ass[i,14] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$F_ty
  data_3ass[i,15] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$Bmsy
  data_3ass[i,16] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$Bmsy
  data_3ass[i,17] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$Fmsy
  data_3ass[i,18] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$Fmsy
  data_3ass[i,19] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$B_by_Bmsy
  data_3ass[i,20] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_3ass[i,21] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$F_by_Fmsy
  data_3ass[i,22] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_3ass[i,23] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$OFL_ty
  data_3ass[i,24] <- dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,5],]$assess_order),]$OFL_ty 
  data_3ass[i,25] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$B_ty)
  data_3ass[i,26] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$F_ty)
  data_3ass[i,27] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$Bmsy)
  data_3ass[i,28] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$Fmsy)
  data_3ass[i,29] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$B_by_Bmsy)
  data_3ass[i,30] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$F_by_Fmsy)
  data_3ass[i,31] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$OFL_ty)
  data_3ass[i,25] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$B_ty)
  data_3ass[i,26] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$F_ty)
  data_3ass[i,27] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$Bmsy)
  data_3ass[i,28] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$Fmsy)
  data_3ass[i,29] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$B_by_Bmsy)
  data_3ass[i,30] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$F_by_Fmsy)
  data_3ass[i,31] <- unique(dd_3ass[dd_3ass$stockid==data_3ass[i,1] & dd_3ass$ty==data_3ass[i,9] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stockid==data_3ass[i,1],]$assess_order),]$OFL_ty)
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
data_4ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_4ass$stock_comparison)), ncol=31))
colnames(data_4ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "interval", "period", 
                         "ty", "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old", "mu_B", "mu_F",
                         "mu_Bmsy", "mu_Fmsy", "mu_B_by_Bmsy", "mu_F_by_Fmsy", "mu_OFL")
data_4ass$stock_comparison <- unique(dd_4ass$stock_comparison)
data_4ass$stock_comparison_code <- as.numeric(as.factor(data_4ass$stock_comparison))
for (i in 1:length(unique(dd_4ass$stock_comparison))){
  data_4ass[i,1] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$stockid)
  data_4ass[i,3] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$region)
  data_4ass[i,4] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$ty_order)
  data_4ass[i,7] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$interval)
  data_4ass[i,8] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$period)
  data_4ass[i,9] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$ty)
  data_4ass[i,10] <- NA
  data_4ass[i,11] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$B_ty
  data_4ass[i,12] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$B_ty
  data_4ass[i,13] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$F_ty
  data_4ass[i,14] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$F_ty
  data_4ass[i,15] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$Bmsy
  data_4ass[i,16] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$Bmsy
  data_4ass[i,17] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$Fmsy
  data_4ass[i,18] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$Fmsy
  data_4ass[i,19] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$B_by_Bmsy
  data_4ass[i,20] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_4ass[i,21] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$F_by_Fmsy
  data_4ass[i,22] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_4ass[i,23] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$OFL_ty
  data_4ass[i,24] <- dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,5],]$assess_order),]$OFL_ty 
  data_4ass[i,25] <- unique(dd_4ass[dd_4ass$stockid==data_4ass[i,1] & dd_4ass$ty==data_4ass[i,9] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stockid==data_4ass[i,1],]$assess_order),]$B_ty)
  data_4ass[i,26] <- unique(dd_4ass[dd_4ass$stockid==data_4ass[i,1] & dd_4ass$ty==data_4ass[i,9] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stockid==data_4ass[i,1],]$assess_order),]$F_ty)
  data_4ass[i,27] <- unique(dd_4ass[dd_4ass$stockid==data_4ass[i,1] & dd_4ass$ty==data_4ass[i,9] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stockid==data_4ass[i,1],]$assess_order),]$Bmsy)
  data_4ass[i,28] <- unique(dd_4ass[dd_4ass$stockid==data_4ass[i,1] & dd_4ass$ty==data_4ass[i,9] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stockid==data_4ass[i,1],]$assess_order),]$Fmsy)
  data_4ass[i,29] <- unique(dd_4ass[dd_4ass$stockid==data_4ass[i,1] & dd_4ass$ty==data_4ass[i,9] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stockid==data_4ass[i,1],]$assess_order),]$B_by_Bmsy)
  data_4ass[i,30] <- unique(dd_4ass[dd_4ass$stockid==data_4ass[i,1] & dd_4ass$ty==data_4ass[i,9] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stockid==data_4ass[i,1],]$assess_order),]$F_by_Fmsy)
  data_4ass[i,31] <- unique(dd_4ass[dd_4ass$stockid==data_4ass[i,1] & dd_4ass$ty==data_4ass[i,9] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stockid==data_4ass[i,1],]$assess_order),]$OFL_ty)
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
data_5ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_5ass$stock_comparison)), ncol=31))
colnames(data_5ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "interval", "period", 
                         "ty", "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old", "mu_B", "mu_F",
                         "mu_Bmsy", "mu_Fmsy", "mu_B_by_Bmsy", "mu_F_by_Fmsy", "mu_OFL")
data_5ass$stock_comparison <- unique(dd_5ass$stock_comparison)
data_5ass$stock_comparison_code <- as.numeric(as.factor(data_5ass$stock_comparison))
for (i in 1:length(unique(dd_5ass$stock_comparison))){
  data_5ass[i,1] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$stockid)
  data_5ass[i,3] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$region)
  data_5ass[i,4] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$ty_order)
  data_5ass[i,7] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$interval)
  data_5ass[i,8] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$period)
  data_5ass[i,9] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$ty)
  data_5ass[i,10] <- NA
  data_5ass[i,11] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$B_ty
  data_5ass[i,12] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$B_ty
  data_5ass[i,13] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$F_ty
  data_5ass[i,14] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$F_ty
  data_5ass[i,15] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$Bmsy
  data_5ass[i,16] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$Bmsy
  data_5ass[i,17] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$Fmsy
  data_5ass[i,18] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$Fmsy
  data_5ass[i,19] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$B_by_Bmsy
  data_5ass[i,20] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_5ass[i,21] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$F_by_Fmsy
  data_5ass[i,22] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_5ass[i,23] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$OFL_ty
  data_5ass[i,24] <- dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,5],]$assess_order),]$OFL_ty 
  data_5ass[i,25] <- unique(dd_5ass[dd_5ass$stockid==data_5ass[i,1] & dd_5ass$ty==data_5ass[i,9] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stockid==data_5ass[i,1],]$assess_order),]$B_ty)
  data_5ass[i,26] <- unique(dd_5ass[dd_5ass$stockid==data_5ass[i,1] & dd_5ass$ty==data_5ass[i,9] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stockid==data_5ass[i,1],]$assess_order),]$F_ty)
  data_5ass[i,27] <- unique(dd_5ass[dd_5ass$stockid==data_5ass[i,1] & dd_5ass$ty==data_5ass[i,9] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stockid==data_5ass[i,1],]$assess_order),]$Bmsy)
  data_5ass[i,28] <- unique(dd_5ass[dd_5ass$stockid==data_5ass[i,1] & dd_5ass$ty==data_5ass[i,9] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stockid==data_5ass[i,1],]$assess_order),]$Fmsy)
  data_5ass[i,29] <- unique(dd_5ass[dd_5ass$stockid==data_5ass[i,1] & dd_5ass$ty==data_5ass[i,9] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stockid==data_5ass[i,1],]$assess_order),]$B_by_Bmsy)
  data_5ass[i,30] <- unique(dd_5ass[dd_5ass$stockid==data_5ass[i,1] & dd_5ass$ty==data_5ass[i,9] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stockid==data_5ass[i,1],]$assess_order),]$F_by_Fmsy)
  data_5ass[i,31] <- unique(dd_5ass[dd_5ass$stockid==data_5ass[i,1] & dd_5ass$ty==data_5ass[i,9] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stockid==data_5ass[i,1],]$assess_order),]$OFL_ty)
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
data_6ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_6ass$stock_comparison)), ncol=31))
colnames(data_6ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "interval", "period", 
                         "ty", "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old", "mu_B", "mu_F",
                         "mu_Bmsy", "mu_Fmsy", "mu_B_by_Bmsy", "mu_F_by_Fmsy", "mu_OFL")
data_6ass$stock_comparison <- unique(dd_6ass$stock_comparison)
data_6ass$stock_comparison_code <- as.numeric(as.factor(data_6ass$stock_comparison))
for (i in 1:length(unique(dd_6ass$stock_comparison))){
  data_6ass[i,1] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$stockid)
  data_6ass[i,3] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$region)
  data_6ass[i,4] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$ty_order)
  data_6ass[i,7] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$interval)
  data_6ass[i,8] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$period)
  data_6ass[i,9] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$ty)
  data_6ass[i,10] <- NA
  data_6ass[i,11] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$B_ty
  data_6ass[i,12] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$B_ty
  data_6ass[i,13] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$F_ty
  data_6ass[i,14] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$F_ty
  data_6ass[i,15] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$Bmsy
  data_6ass[i,16] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$Bmsy
  data_6ass[i,17] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$Fmsy
  data_6ass[i,18] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$Fmsy
  data_6ass[i,19] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$B_by_Bmsy
  data_6ass[i,20] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_6ass[i,21] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$F_by_Fmsy
  data_6ass[i,22] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_6ass[i,23] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$OFL_ty
  data_6ass[i,24] <- dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,5],]$assess_order),]$OFL_ty 
  data_6ass[i,25] <- unique(dd_6ass[dd_6ass$stockid==data_6ass[i,1] & dd_6ass$ty==data_6ass[i,9] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stockid==data_6ass[i,1],]$assess_order),]$B_ty)
  data_6ass[i,26] <- unique(dd_6ass[dd_6ass$stockid==data_6ass[i,1] & dd_6ass$ty==data_6ass[i,9] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stockid==data_6ass[i,1],]$assess_order),]$F_ty)
  data_6ass[i,27] <- unique(dd_6ass[dd_6ass$stockid==data_6ass[i,1] & dd_6ass$ty==data_6ass[i,9] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stockid==data_6ass[i,1],]$assess_order),]$Bmsy)
  data_6ass[i,28] <- unique(dd_6ass[dd_6ass$stockid==data_6ass[i,1] & dd_6ass$ty==data_6ass[i,9] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stockid==data_6ass[i,1],]$assess_order),]$Fmsy)
  data_6ass[i,29] <- unique(dd_6ass[dd_6ass$stockid==data_6ass[i,1] & dd_6ass$ty==data_6ass[i,9] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stockid==data_6ass[i,1],]$assess_order),]$B_by_Bmsy)
  data_6ass[i,30] <- unique(dd_6ass[dd_6ass$stockid==data_6ass[i,1] & dd_6ass$ty==data_6ass[i,9] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stockid==data_6ass[i,1],]$assess_order),]$F_by_Fmsy)
  data_6ass[i,31] <- unique(dd_6ass[dd_6ass$stockid==data_6ass[i,1] & dd_6ass$ty==data_6ass[i,9] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stockid==data_6ass[i,1],]$assess_order),]$OFL_ty)
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
data_7ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_7ass$stock_comparison)), ncol=31))
colnames(data_7ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "interval", "period", 
                         "ty", "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old", "mu_B", "mu_F",
                         "mu_Bmsy", "mu_Fmsy", "mu_B_by_Bmsy", "mu_F_by_Fmsy", "mu_OFL")
data_7ass$stock_comparison <- unique(dd_7ass$stock_comparison)
data_7ass$stock_comparison_code <- as.numeric(as.factor(data_7ass$stock_comparison))
for (i in 1:length(unique(dd_7ass$stock_comparison))){
  data_7ass[i,1] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$stockid)
  data_7ass[i,3] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$region)
  data_7ass[i,4] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$ty_order)
  data_7ass[i,7] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$interval)
  data_7ass[i,8] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$period)
  data_7ass[i,9] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$ty)
  data_7ass[i,10] <- NA
  data_7ass[i,11] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$B_ty
  data_7ass[i,12] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$B_ty
  data_7ass[i,13] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$F_ty
  data_7ass[i,14] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$F_ty
  data_7ass[i,15] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$Bmsy
  data_7ass[i,16] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$Bmsy
  data_7ass[i,17] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$Fmsy
  data_7ass[i,18] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$Fmsy
  data_7ass[i,19] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$B_by_Bmsy
  data_7ass[i,20] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_7ass[i,21] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$F_by_Fmsy
  data_7ass[i,22] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_7ass[i,23] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$OFL_ty
  data_7ass[i,24] <- dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,5],]$assess_order),]$OFL_ty 
  data_7ass[i,25] <- unique(dd_7ass[dd_7ass$stockid==data_7ass[i,1] & dd_7ass$ty==data_7ass[i,9] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stockid==data_7ass[i,1],]$assess_order),]$B_ty)
  data_7ass[i,26] <- unique(dd_7ass[dd_7ass$stockid==data_7ass[i,1] & dd_7ass$ty==data_7ass[i,9] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stockid==data_7ass[i,1],]$assess_order),]$F_ty)
  data_7ass[i,27] <- unique(dd_7ass[dd_7ass$stockid==data_7ass[i,1] & dd_7ass$ty==data_7ass[i,9] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stockid==data_7ass[i,1],]$assess_order),]$Bmsy)
  data_7ass[i,28] <- unique(dd_7ass[dd_7ass$stockid==data_7ass[i,1] & dd_7ass$ty==data_7ass[i,9] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stockid==data_7ass[i,1],]$assess_order),]$Fmsy)
  data_7ass[i,29] <- unique(dd_7ass[dd_7ass$stockid==data_7ass[i,1] & dd_7ass$ty==data_7ass[i,9] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stockid==data_7ass[i,1],]$assess_order),]$B_by_Bmsy)
  data_7ass[i,30] <- unique(dd_7ass[dd_7ass$stockid==data_7ass[i,1] & dd_7ass$ty==data_7ass[i,9] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stockid==data_7ass[i,1],]$assess_order),]$F_by_Fmsy)
  data_7ass[i,31] <- unique(dd_7ass[dd_7ass$stockid==data_7ass[i,1] & dd_7ass$ty==data_7ass[i,9] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stockid==data_7ass[i,1],]$assess_order),]$OFL_ty)
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
data_8ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_8ass$stock_comparison)), ncol=31))
colnames(data_8ass) <- c("stockid", "stockid_code", "region", "comparison", "stock_comparison", "stock_comparison_code", "interval", "period", 
                         "ty", "MethodChange", "B_recent", "B_old", "F_recent", "F_old", "Bmsy_recent", "Bmsy_old", "Fmsy_recent", "Fmsy_old",
                         "B_by_Bmsy_recent", "B_by_Bmsy_old", "F_by_Fmsy_recent", "F_by_Fmsy_old", "OFL_ty_recent", "OFL_ty_old", "mu_B", "mu_F",
                         "mu_Bmsy", "mu_Fmsy", "mu_B_by_Bmsy", "mu_F_by_Fmsy", "mu_OFL")
data_8ass$stock_comparison <- unique(dd_8ass$stock_comparison)
data_8ass$stock_comparison_code <- as.numeric(as.factor(data_8ass$stock_comparison))
for (i in 1:length(unique(dd_8ass$stock_comparison))){
  data_8ass[i,1] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$stockid)
  data_8ass[i,3] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$region)
  data_8ass[i,4] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$ty_order)
  data_8ass[i,7] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$interval)
  data_8ass[i,8] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$period)
  data_8ass[i,9] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$ty)
  data_8ass[i,10] <- NA
  data_8ass[i,11] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$B_ty
  data_8ass[i,12] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$B_ty
  data_8ass[i,13] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$F_ty
  data_8ass[i,14] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$F_ty
  data_8ass[i,15] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$Bmsy
  data_8ass[i,16] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$Bmsy
  data_8ass[i,17] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$Fmsy
  data_8ass[i,18] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$Fmsy
  data_8ass[i,19] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$B_by_Bmsy
  data_8ass[i,20] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$B_by_Bmsy 
  data_8ass[i,21] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$F_by_Fmsy
  data_8ass[i,22] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$F_by_Fmsy 
  data_8ass[i,23] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$OFL_ty
  data_8ass[i,24] <- dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,5],]$assess_order),]$OFL_ty 
  data_8ass[i,25] <- unique(dd_8ass[dd_8ass$stockid==data_8ass[i,1] & dd_8ass$ty==data_8ass[i,9] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stockid==data_8ass[i,1],]$assess_order),]$B_ty)
  data_8ass[i,26] <- unique(dd_8ass[dd_8ass$stockid==data_8ass[i,1] & dd_8ass$ty==data_8ass[i,9] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stockid==data_8ass[i,1],]$assess_order),]$F_ty)
  data_8ass[i,27] <- unique(dd_8ass[dd_8ass$stockid==data_8ass[i,1] & dd_8ass$ty==data_8ass[i,9] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stockid==data_8ass[i,1],]$assess_order),]$Bmsy)
  data_8ass[i,28] <- unique(dd_8ass[dd_8ass$stockid==data_8ass[i,1] & dd_8ass$ty==data_8ass[i,9] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stockid==data_8ass[i,1],]$assess_order),]$Fmsy)
  data_8ass[i,29] <- unique(dd_8ass[dd_8ass$stockid==data_8ass[i,1] & dd_8ass$ty==data_8ass[i,9] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stockid==data_8ass[i,1],]$assess_order),]$B_by_Bmsy)
  data_8ass[i,30] <- unique(dd_8ass[dd_8ass$stockid==data_8ass[i,1] & dd_8ass$ty==data_8ass[i,9] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stockid==data_8ass[i,1],]$assess_order),]$F_by_Fmsy)
  data_8ass[i,31] <- unique(dd_8ass[dd_8ass$stockid==data_8ass[i,1] & dd_8ass$ty==data_8ass[i,9] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stockid==data_8ass[i,1],]$assess_order),]$OFL_ty)
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
write.csv(data_full, paste(outputdir, "0h_data_full_model.csv", sep="/"), row.names=F)

####################################################################################################################

## Match with councils for US stocks ##
key <- read.csv(paste(outputdir, "council_key.csv", sep="/"), as.is=T)
data_full <- read.csv(paste(outputdir, "0h_data_full_model.csv", sep="/"), as.is=T)

data_council <- data_full %>% 
  left_join(key, by=c("stockid"))

data_council[is.na(data_council$council),]$council <- data_council[is.na(data_council$council),]$region
data_council$council <- factor(data_council$council, 
                               levels = c("ASMFC", "GMFMC", "HMS", "MAFMC", "NEFMC", "NPFMC", "PFMC", "SAFMC", "US non-federal",
                                          "Europe (EU)", "Europe (non-EU)", "Atlantic Ocean", "Australia", "Canada East Coast", "Canada West Coast", 
                                          "Indian Ocean", "Japan", "Mediterranean-Black Sea", "New Zealand", "Pacific Ocean", "South Africa",
                                          "South America"))

data_council$CouncilCode <- as.numeric(data_council$council)

data_council[data_council$stockid_code==56|data_council$stockid_code==76|data_council$stockid_code==179,]$B_recent <- 
  data_council[data_council$stockid_code==56|data_council$stockid_code==76|data_council$stockid_code==179,]$B_recent/10^9
data_council[data_council$stockid_code==56|data_council$stockid_code==76|data_council$stockid_code==179,]$B_old <- 
  data_council[data_council$stockid_code==56|data_council$stockid_code==76|data_council$stockid_code==179,]$B_old/10^9
data_council[data_council$stockid_code==56|data_council$stockid_code==76|data_council$stockid_code==179,]$Bmsy_recent <- 
  data_council[data_council$stockid_code==56|data_council$stockid_code==76|data_council$stockid_code==179,]$Bmsy_recent/10^9
data_council[data_council$stockid_code==56|data_council$stockid_code==76|data_council$stockid_code==179,]$Bmsy_old <- 
  data_council[data_council$stockid_code==56|data_council$stockid_code==76|data_council$stockid_code==179,]$Bmsy_old/10^9
data_council[data_council$stockid_code==56|data_council$stockid_code==76|data_council$stockid_code==179,]$OFL_ty_recent <- 
  data_council[data_council$stockid_code==56|data_council$stockid_code==76|data_council$stockid_code==179,]$OFL_ty_recent/10^9
data_council[data_council$stockid_code==56|data_council$stockid_code==76|data_council$stockid_code==179,]$OFL_ty_old <- 
  data_council[data_council$stockid_code==56|data_council$stockid_code==76|data_council$stockid_code==179,]$OFL_ty_old/10^9

data_council[data_council$stockid_code==32,]$B_recent <- data_council[data_council$stockid_code==32,]$B_recent/10^6
data_council[data_council$stockid_code==32,]$B_old <- data_council[data_council$stockid_code==32,]$B_old/10^6
data_council[data_council$stockid_code==32,]$Bmsy_recent <- data_council[data_council$stockid_code==32,]$Bmsy_recent/10^6
data_council[data_council$stockid_code==32,]$Bmsy_old <- data_council[data_council$stockid_code==32,]$Bmsy_old/10^6

data_council[data_council$stockid_code==190,]$Bmsy_old <- NA
data_council[data_council$stockid_code==190,]$B_by_Bmsy_old <- NA
data_council[data_council$stockid_code==190,]$Bmsy_recent <- 277871
data_council[data_council$stockid_code==190,]$B_by_Bmsy_recent <- data_council[data_council$stockid_code==190,]$B_recent/data_council[data_council$stockid_code==190,]$Bmsy_recent
data_council[data_council$stockid == "BRNSHRIMPGM" | data_council$stockid == "WSHRIMPGM",]$Fmsy_recent <- NA
data_council[data_council$stockid == "BRNSHRIMPGM" | data_council$stockid == "WSHRIMPGM",]$F_by_Fmsy_recent <- NA

plot(data_council$B_recent, data_council$B_old)
plot(data_council$F_recent, data_council$F_old)
plot(data_council$Bmsy_recent, data_council$Bmsy_old)
plot(data_council$Fmsy_recent, data_council$Fmsy_old)
plot(data_council$B_by_Bmsy_recent, data_council$B_by_Bmsy_old)
plot(data_council$F_by_Fmsy_recent, data_council$F_by_Fmsy_old)
plot(data_council$OFL_ty_recent, data_council$OFL_ty_old)

##################################################################################################

write.csv(data_council, paste(outputdir, "0h_data_council_model.csv", sep="/"), row.names=F)


