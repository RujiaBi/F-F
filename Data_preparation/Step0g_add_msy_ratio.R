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

data_2ass <- read.csv(paste(outputdir, "0f_1_data_2_assess_update_ty.csv", sep="/"), as.is=T)
data_3ass <- read.csv(paste(outputdir, "0f_2_data_3_assess_update_ty.csv", sep="/"), as.is=T)
data_4ass <- read.csv(paste(outputdir, "0f_3_data_4_assess_update_ty.csv", sep="/"), as.is=T)
data_5ass <- read.csv(paste(outputdir, "0f_4_data_5_assess_update_ty.csv", sep="/"), as.is=T)
data_6ass <- read.csv(paste(outputdir, "0f_5_data_6_assess_update_ty.csv", sep="/"), as.is=T)
data_7ass <- read.csv(paste(outputdir, "0f_6_data_7_assess_update_ty.csv", sep="/"), as.is=T)
data_8ass <- read.csv(paste(outputdir, "0f_7_data_8_assess_update_ty.csv", sep="/"), as.is=T)

colnames(data_2ass)[74:75] <- colnames(data_3ass)[74:75] <- colnames(data_4ass)[74:75] <- colnames(data_5ass)[74:75] <-
  colnames(data_6ass)[74:75] <- colnames(data_7ass)[74:75] <- colnames(data_8ass)[74:75] <- c("instFmsy", "instFmsy_notes")

##################################################################################################

## TB/TBmsy, SSB/SSBmsy, InstF/InstFmsy, ER/ERmsy
data_2ass$TB_by_TBmsy <- data_2ass$TB_ty/data_2ass$TBmsybest
data_2ass$SSB_by_SSBmsy <- data_2ass$SSB_ty/data_2ass$SSBmsy
data_2ass$InstF_by_InstFmsy <- data_2ass$instF_ty/data_2ass$instFmsy
data_2ass$ER_by_ERmsy <- data_2ass$ER_ty/data_2ass$ERmsybest

data_3ass$TB_by_TBmsy <- data_3ass$TB_ty/data_3ass$TBmsybest
data_3ass$SSB_by_SSBmsy <- data_3ass$SSB_ty/data_3ass$SSBmsy
data_3ass$InstF_by_InstFmsy <- data_3ass$instF_ty/data_3ass$instFmsy
data_3ass$ER_by_ERmsy <- data_3ass$ER_ty/data_3ass$ERmsybest

data_4ass$TB_by_TBmsy <- data_4ass$TB_ty/data_4ass$TBmsybest
data_4ass$SSB_by_SSBmsy <- data_4ass$SSB_ty/data_4ass$SSBmsy
data_4ass$InstF_by_InstFmsy <- data_4ass$instF_ty/data_4ass$instFmsy
data_4ass$ER_by_ERmsy <- data_4ass$ER_ty/data_4ass$ERmsybest

data_5ass$TB_by_TBmsy <- data_5ass$TB_ty/data_5ass$TBmsybest
data_5ass$SSB_by_SSBmsy <- data_5ass$SSB_ty/data_5ass$SSBmsy
data_5ass$InstF_by_InstFmsy <- data_5ass$instF_ty/data_5ass$instFmsy
data_5ass$ER_by_ERmsy <- data_5ass$ER_ty/data_5ass$ERmsybest

data_6ass$TB_by_TBmsy <- data_6ass$TB_ty/data_6ass$TBmsybest
data_6ass$SSB_by_SSBmsy <- data_6ass$SSB_ty/data_6ass$SSBmsy
data_6ass$InstF_by_InstFmsy <- data_6ass$instF_ty/data_6ass$instFmsy
data_6ass$ER_by_ERmsy <- data_6ass$ER_ty/data_6ass$ERmsybest

data_7ass$TB_by_TBmsy <- data_7ass$TB_ty/data_7ass$TBmsybest
data_7ass$SSB_by_SSBmsy <- data_7ass$SSB_ty/data_7ass$SSBmsy
data_7ass$InstF_by_InstFmsy <- data_7ass$instF_ty/data_7ass$instFmsy
data_7ass$ER_by_ERmsy <- data_7ass$ER_ty/data_7ass$ERmsybest

data_8ass$TB_by_TBmsy <- data_8ass$TB_ty/data_8ass$TBmsybest
data_8ass$SSB_by_SSBmsy <- data_8ass$SSB_ty/data_8ass$SSBmsy
data_8ass$InstF_by_InstFmsy <- data_8ass$instF_ty/data_8ass$instFmsy
data_8ass$ER_by_ERmsy <- data_8ass$ER_ty/data_8ass$ERmsybest

##################################################################################################

## OFL_InstF, OFL_ER
# data_2ass
data_2ass$OFL_instF <- data_2ass$B_ty*(1-exp(-data_2ass$instFmsy-data_2ass$M))*data_2ass$instFmsy/(data_2ass$instFmsy+data_2ass$M)
data_2ass$OFL_instF_2 <- data_2ass$B_ty*(1-exp(-data_2ass$instFmsy))
data_2ass$OFL_ER <- data_2ass$B_ty*data_2ass$ERmsybest

# data_3ass
data_3ass$OFL_instF <- data_3ass$B_ty*(1-exp(-data_3ass$instFmsy-data_3ass$M))*data_3ass$instFmsy/(data_3ass$instFmsy+data_3ass$M)
data_3ass$OFL_instF_2 <- data_3ass$B_ty*(1-exp(-data_3ass$instFmsy))
data_3ass$OFL_ER <- data_3ass$B_ty*data_3ass$ERmsybest

# data_4ass
data_4ass$OFL_instF <- data_4ass$B_ty*(1-exp(-data_4ass$instFmsy-data_4ass$M))*data_4ass$instFmsy/(data_4ass$instFmsy+data_4ass$M)
data_4ass$OFL_instF_2 <- data_4ass$B_ty*(1-exp(-data_4ass$instFmsy))
data_4ass$OFL_ER <- data_4ass$B_ty*data_4ass$ERmsybest

# data_5ass
data_5ass$OFL_instF <- data_5ass$B_ty*(1-exp(-data_5ass$instFmsy-data_5ass$M))*data_5ass$instFmsy/(data_5ass$instFmsy+data_5ass$M)
data_5ass$OFL_instF_2 <- data_5ass$B_ty*(1-exp(-data_5ass$instFmsy))
data_5ass$OFL_ER <- data_5ass$B_ty*data_5ass$ERmsybest

# data_6ass
data_6ass$OFL_instF <- data_6ass$B_ty*(1-exp(-data_6ass$instFmsy-data_6ass$M))*data_6ass$instFmsy/(data_6ass$instFmsy+data_6ass$M)
data_6ass$OFL_instF_2 <- data_6ass$B_ty*(1-exp(-data_6ass$instFmsy))
data_6ass$OFL_ER <- data_6ass$B_ty*data_6ass$ERmsybest

# data_7ass
data_7ass$OFL_instF <- data_7ass$B_ty*(1-exp(-data_7ass$instFmsy-data_7ass$M))*data_7ass$instFmsy/(data_7ass$instFmsy+data_7ass$M)
data_7ass$OFL_instF_2 <- data_7ass$B_ty*(1-exp(-data_7ass$instFmsy))
data_7ass$OFL_ER <- data_7ass$B_ty*data_7ass$ERmsybest

# data_8ass
data_8ass$OFL_instF <- data_8ass$B_ty*(1-exp(-data_8ass$instFmsy-data_8ass$M))*data_8ass$instFmsy/(data_8ass$instFmsy+data_8ass$M)
data_8ass$OFL_instF_2 <- data_8ass$B_ty*(1-exp(-data_8ass$instFmsy))
data_8ass$OFL_ER <- data_8ass$B_ty*data_8ass$ERmsybest

##################################################################################################

## Bmsy (choose TBmsy or SSBmsy)
# data_2ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_2ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_SSB", "number_of_assess_TB", "B_id")
ddb$stockid <- unique(data_2ass$stockid)
for (i in 1:length(unique(data_2ass$stockid))){
  ddb[i,2] <- length(unique(data_2ass[data_2ass$stockid==ddb[i,1] & !is.na(data_2ass$SSBmsy),]$assessid))
  ddb[i,3] <- length(unique(data_2ass[data_2ass$stockid==ddb[i,1] & !is.na(data_2ass$TBmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_ssb <- ddb[ddb$B_id==1,]$stockid
stock_tb <- ddb[ddb$B_id==2,]$stockid

data_2ass$Bmsy_type <- data_2ass$Bmsy <- NA
data_2ass[data_2ass$stockid %in% stock_ssb,]$Bmsy <- data_2ass[data_2ass$stockid %in% stock_ssb,]$SSBmsy
data_2ass[data_2ass$stockid %in% stock_ssb,]$Bmsy_type <- "SSBmsy"
data_2ass[data_2ass$stockid %in% stock_tb,]$Bmsy <- data_2ass[data_2ass$stockid %in% stock_tb,]$TBmsybest
data_2ass[data_2ass$stockid %in% stock_tb,]$Bmsy_type <- "TBmsybest"


# data_3ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_3ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_SSB", "number_of_assess_TB", "B_id")
ddb$stockid <- unique(data_3ass$stockid)
for (i in 1:length(unique(data_3ass$stockid))){
  ddb[i,2] <- length(unique(data_3ass[data_3ass$stockid==ddb[i,1] & !is.na(data_3ass$SSBmsy),]$assessid))
  ddb[i,3] <- length(unique(data_3ass[data_3ass$stockid==ddb[i,1] & !is.na(data_3ass$TBmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_ssb <- ddb[ddb$B_id==1,]$stockid
stock_tb <- ddb[ddb$B_id==2,]$stockid

data_3ass$Bmsy_type <- data_3ass$Bmsy <- NA
data_3ass[data_3ass$stockid %in% stock_ssb,]$Bmsy <- data_3ass[data_3ass$stockid %in% stock_ssb,]$SSBmsy
data_3ass[data_3ass$stockid %in% stock_ssb,]$Bmsy_type <- "SSBmsy"
data_3ass[data_3ass$stockid %in% stock_tb,]$Bmsy <- data_3ass[data_3ass$stockid %in% stock_tb,]$TBmsybest
data_3ass[data_3ass$stockid %in% stock_tb,]$Bmsy_type <- "TBmsybest"


# data_4ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_4ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_SSB", "number_of_assess_TB", "B_id")
ddb$stockid <- unique(data_4ass$stockid)
for (i in 1:length(unique(data_4ass$stockid))){
  ddb[i,2] <- length(unique(data_4ass[data_4ass$stockid==ddb[i,1] & !is.na(data_4ass$SSBmsy),]$assessid))
  ddb[i,3] <- length(unique(data_4ass[data_4ass$stockid==ddb[i,1] & !is.na(data_4ass$TBmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_ssb <- ddb[ddb$B_id==1,]$stockid
stock_tb <- ddb[ddb$B_id==2,]$stockid

data_4ass$Bmsy_type <- data_4ass$Bmsy <- NA
data_4ass[data_4ass$stockid %in% stock_ssb,]$Bmsy <- data_4ass[data_4ass$stockid %in% stock_ssb,]$SSBmsy
data_4ass[data_4ass$stockid %in% stock_ssb,]$Bmsy_type <- "SSBmsy"
data_4ass[data_4ass$stockid %in% stock_tb,]$Bmsy <- data_4ass[data_4ass$stockid %in% stock_tb,]$TBmsybest
data_4ass[data_4ass$stockid %in% stock_tb,]$Bmsy_type <- "TBmsybest"


# data_5ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_5ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_SSB", "number_of_assess_TB", "B_id")
ddb$stockid <- unique(data_5ass$stockid)
for (i in 1:length(unique(data_5ass$stockid))){
  ddb[i,2] <- length(unique(data_5ass[data_5ass$stockid==ddb[i,1] & !is.na(data_5ass$SSBmsy),]$assessid))
  ddb[i,3] <- length(unique(data_5ass[data_5ass$stockid==ddb[i,1] & !is.na(data_5ass$TBmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_ssb <- ddb[ddb$B_id==1,]$stockid
stock_tb <- ddb[ddb$B_id==2,]$stockid

data_5ass$Bmsy_type <- data_5ass$Bmsy <- NA
data_5ass[data_5ass$stockid %in% stock_ssb,]$Bmsy <- data_5ass[data_5ass$stockid %in% stock_ssb,]$SSBmsy
data_5ass[data_5ass$stockid %in% stock_ssb,]$Bmsy_type <- "SSBmsy"
data_5ass[data_5ass$stockid %in% stock_tb,]$Bmsy <- data_5ass[data_5ass$stockid %in% stock_tb,]$TBmsybest
data_5ass[data_5ass$stockid %in% stock_tb,]$Bmsy_type <- "TBmsybest"


# data_6ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_6ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_SSB", "number_of_assess_TB", "B_id")
ddb$stockid <- unique(data_6ass$stockid)
for (i in 1:length(unique(data_6ass$stockid))){
  ddb[i,2] <- length(unique(data_6ass[data_6ass$stockid==ddb[i,1] & !is.na(data_6ass$SSBmsy),]$assessid))
  ddb[i,3] <- length(unique(data_6ass[data_6ass$stockid==ddb[i,1] & !is.na(data_6ass$TBmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_ssb <- ddb[ddb$B_id==1,]$stockid
stock_tb <- ddb[ddb$B_id==2,]$stockid

data_6ass$Bmsy_type <- data_6ass$Bmsy <- NA
data_6ass[data_6ass$stockid %in% stock_ssb,]$Bmsy <- data_6ass[data_6ass$stockid %in% stock_ssb,]$SSBmsy
data_6ass[data_6ass$stockid %in% stock_ssb,]$Bmsy_type <- "SSBmsy"
data_6ass[data_6ass$stockid %in% stock_tb,]$Bmsy <- data_6ass[data_6ass$stockid %in% stock_tb,]$TBmsybest
data_6ass[data_6ass$stockid %in% stock_tb,]$Bmsy_type <- "TBmsybest"


# data_7ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_7ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_SSB", "number_of_assess_TB", "B_id")
ddb$stockid <- unique(data_7ass$stockid)
for (i in 1:length(unique(data_7ass$stockid))){
  ddb[i,2] <- length(unique(data_7ass[data_7ass$stockid==ddb[i,1] & !is.na(data_7ass$SSBmsy),]$assessid))
  ddb[i,3] <- length(unique(data_7ass[data_7ass$stockid==ddb[i,1] & !is.na(data_7ass$TBmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_ssb <- ddb[ddb$B_id==1,]$stockid
stock_tb <- ddb[ddb$B_id==2,]$stockid

data_7ass$Bmsy_type <- data_7ass$Bmsy <- NA
data_7ass[data_7ass$stockid %in% stock_ssb,]$Bmsy <- data_7ass[data_7ass$stockid %in% stock_ssb,]$SSBmsy
data_7ass[data_7ass$stockid %in% stock_ssb,]$Bmsy_type <- "SSBmsy"
data_7ass[data_7ass$stockid %in% stock_tb,]$Bmsy <- data_7ass[data_7ass$stockid %in% stock_tb,]$TBmsybest
data_7ass[data_7ass$stockid %in% stock_tb,]$Bmsy_type <- "TBmsybest"


# data_8ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_8ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_SSB", "number_of_assess_TB", "B_id")
ddb$stockid <- unique(data_8ass$stockid)
for (i in 1:length(unique(data_8ass$stockid))){
  ddb[i,2] <- length(unique(data_8ass[data_8ass$stockid==ddb[i,1] & !is.na(data_8ass$SSBmsy),]$assessid))
  ddb[i,3] <- length(unique(data_8ass[data_8ass$stockid==ddb[i,1] & !is.na(data_8ass$TBmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_ssb <- ddb[ddb$B_id==1,]$stockid
stock_tb <- ddb[ddb$B_id==2,]$stockid

data_8ass$Bmsy_type <- data_8ass$Bmsy <- NA
data_8ass[data_8ass$stockid %in% stock_ssb,]$Bmsy <- data_8ass[data_8ass$stockid %in% stock_ssb,]$SSBmsy
data_8ass[data_8ass$stockid %in% stock_ssb,]$Bmsy_type <- "SSBmsy"
data_8ass[data_8ass$stockid %in% stock_tb,]$Bmsy <- data_8ass[data_8ass$stockid %in% stock_tb,]$TBmsybest
data_8ass[data_8ass$stockid %in% stock_tb,]$Bmsy_type <- "TBmsybest"

##################################################################################################

## Fmsy (choose instFmsy or ERmsybest)
# data_2ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_2ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_InstF", "number_of_assess_ER", "B_id")
ddb$stockid <- unique(data_2ass$stockid)
for (i in 1:length(unique(data_2ass$stockid))){
  ddb[i,2] <- length(unique(data_2ass[data_2ass$stockid==ddb[i,1] & !is.na(data_2ass$instFmsy),]$assessid))
  ddb[i,3] <- length(unique(data_2ass[data_2ass$stockid==ddb[i,1] & !is.na(data_2ass$ERmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_instf <- ddb[ddb$B_id==1,]$stockid
stock_er <- ddb[ddb$B_id==2,]$stockid

data_2ass$Fmsy_type <- data_2ass$Fmsy <- NA
data_2ass[data_2ass$stockid %in% stock_instf,]$Fmsy <- data_2ass[data_2ass$stockid %in% stock_instf,]$instFmsy
data_2ass[data_2ass$stockid %in% stock_instf,]$Fmsy_type <- "instFmsy"
data_2ass[data_2ass$stockid %in% stock_er,]$Fmsy <- data_2ass[data_2ass$stockid %in% stock_er,]$ERmsybest
data_2ass[data_2ass$stockid %in% stock_er,]$Fmsy_type <- "ERmsybest"

# data_3ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_3ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_InstF", "number_of_assess_ER", "B_id")
ddb$stockid <- unique(data_3ass$stockid)
for (i in 1:length(unique(data_3ass$stockid))){
  ddb[i,2] <- length(unique(data_3ass[data_3ass$stockid==ddb[i,1] & !is.na(data_3ass$instFmsy),]$assessid))
  ddb[i,3] <- length(unique(data_3ass[data_3ass$stockid==ddb[i,1] & !is.na(data_3ass$ERmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_instf <- ddb[ddb$B_id==1,]$stockid
stock_er <- ddb[ddb$B_id==2,]$stockid

data_3ass$Fmsy_type <- data_3ass$Fmsy <- NA
data_3ass[data_3ass$stockid %in% stock_instf,]$Fmsy <- data_3ass[data_3ass$stockid %in% stock_instf,]$instFmsy
data_3ass[data_3ass$stockid %in% stock_instf,]$Fmsy_type <- "instFmsy"
data_3ass[data_3ass$stockid %in% stock_er,]$Fmsy <- data_3ass[data_3ass$stockid %in% stock_er,]$ERmsybest
data_3ass[data_3ass$stockid %in% stock_er,]$Fmsy_type <- "ERmsybest"

# data_4ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_4ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_InstF", "number_of_assess_ER", "B_id")
ddb$stockid <- unique(data_4ass$stockid)
for (i in 1:length(unique(data_4ass$stockid))){
  ddb[i,2] <- length(unique(data_4ass[data_4ass$stockid==ddb[i,1] & !is.na(data_4ass$instFmsy),]$assessid))
  ddb[i,3] <- length(unique(data_4ass[data_4ass$stockid==ddb[i,1] & !is.na(data_4ass$ERmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_instf <- ddb[ddb$B_id==1,]$stockid
stock_er <- ddb[ddb$B_id==2,]$stockid

data_4ass$Fmsy_type <- data_4ass$Fmsy <- NA
data_4ass[data_4ass$stockid %in% stock_instf,]$Fmsy <- data_4ass[data_4ass$stockid %in% stock_instf,]$instFmsy
data_4ass[data_4ass$stockid %in% stock_instf,]$Fmsy_type <- "instFmsy"
data_4ass[data_4ass$stockid %in% stock_er,]$Fmsy <- data_4ass[data_4ass$stockid %in% stock_er,]$ERmsybest
data_4ass[data_4ass$stockid %in% stock_er,]$Fmsy_type <- "ERmsybest"

# data_5ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_5ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_InstF", "number_of_assess_ER", "B_id")
ddb$stockid <- unique(data_5ass$stockid)
for (i in 1:length(unique(data_5ass$stockid))){
  ddb[i,2] <- length(unique(data_5ass[data_5ass$stockid==ddb[i,1] & !is.na(data_5ass$instFmsy),]$assessid))
  ddb[i,3] <- length(unique(data_5ass[data_5ass$stockid==ddb[i,1] & !is.na(data_5ass$ERmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_instf <- ddb[ddb$B_id==1,]$stockid
stock_er <- ddb[ddb$B_id==2,]$stockid

data_5ass$Fmsy_type <- data_5ass$Fmsy <- NA
data_5ass[data_5ass$stockid %in% stock_instf,]$Fmsy <- data_5ass[data_5ass$stockid %in% stock_instf,]$instFmsy
data_5ass[data_5ass$stockid %in% stock_instf,]$Fmsy_type <- "instFmsy"
data_5ass[data_5ass$stockid %in% stock_er,]$Fmsy <- data_5ass[data_5ass$stockid %in% stock_er,]$ERmsybest
data_5ass[data_5ass$stockid %in% stock_er,]$Fmsy_type <- "ERmsybest"


# data_6ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_6ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_InstF", "number_of_assess_ER", "B_id")
ddb$stockid <- unique(data_6ass$stockid)
for (i in 1:length(unique(data_6ass$stockid))){
  ddb[i,2] <- length(unique(data_6ass[data_6ass$stockid==ddb[i,1] & !is.na(data_6ass$instFmsy),]$assessid))
  ddb[i,3] <- length(unique(data_6ass[data_6ass$stockid==ddb[i,1] & !is.na(data_6ass$ERmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_instf <- ddb[ddb$B_id==1,]$stockid
stock_er <- ddb[ddb$B_id==2,]$stockid

data_6ass$Fmsy_type <- data_6ass$Fmsy <- NA
data_6ass[data_6ass$stockid %in% stock_instf,]$Fmsy <- data_6ass[data_6ass$stockid %in% stock_instf,]$instFmsy
data_6ass[data_6ass$stockid %in% stock_instf,]$Fmsy_type <- "instFmsy"
data_6ass[data_6ass$stockid %in% stock_er,]$Fmsy <- data_6ass[data_6ass$stockid %in% stock_er,]$ERmsybest
data_6ass[data_6ass$stockid %in% stock_er,]$Fmsy_type <- "ERmsybest"


# data_7ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_7ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_InstF", "number_of_assess_ER", "B_id")
ddb$stockid <- unique(data_7ass$stockid)
for (i in 1:length(unique(data_7ass$stockid))){
  ddb[i,2] <- length(unique(data_7ass[data_7ass$stockid==ddb[i,1] & !is.na(data_7ass$instFmsy),]$assessid))
  ddb[i,3] <- length(unique(data_7ass[data_7ass$stockid==ddb[i,1] & !is.na(data_7ass$ERmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_instf <- ddb[ddb$B_id==1,]$stockid
stock_er <- ddb[ddb$B_id==2,]$stockid

data_7ass$Fmsy_type <- data_7ass$Fmsy <- NA
data_7ass[data_7ass$stockid %in% stock_instf,]$Fmsy <- data_7ass[data_7ass$stockid %in% stock_instf,]$instFmsy
data_7ass[data_7ass$stockid %in% stock_instf,]$Fmsy_type <- "instFmsy"
data_7ass[data_7ass$stockid %in% stock_er,]$Fmsy <- data_7ass[data_7ass$stockid %in% stock_er,]$ERmsybest
data_7ass[data_7ass$stockid %in% stock_er,]$Fmsy_type <- "ERmsybest"

# data_8ass
ddb <- as.data.frame(matrix(NA, nrow=length(unique(data_8ass$stockid)), ncol=4))
colnames(ddb) <- c("stockid", "number_of_assess_InstF", "number_of_assess_ER", "B_id")
ddb$stockid <- unique(data_8ass$stockid)
for (i in 1:length(unique(data_8ass$stockid))){
  ddb[i,2] <- length(unique(data_8ass[data_8ass$stockid==ddb[i,1] & !is.na(data_8ass$instFmsy),]$assessid))
  ddb[i,3] <- length(unique(data_8ass[data_8ass$stockid==ddb[i,1] & !is.na(data_8ass$ERmsybest),]$assessid))
  vec <- c(ddb[i,2], ddb[i,3])
  ddb[i,4] <- which.max(vec)
}

stock_instf <- ddb[ddb$B_id==1,]$stockid
stock_er <- ddb[ddb$B_id==2,]$stockid

data_8ass$Fmsy_type <- data_8ass$Fmsy <- NA
data_8ass[data_8ass$stockid %in% stock_instf,]$Fmsy <- data_8ass[data_8ass$stockid %in% stock_instf,]$instFmsy
data_8ass[data_8ass$stockid %in% stock_instf,]$Fmsy_type <- "instFmsy"
data_8ass[data_8ass$stockid %in% stock_er,]$Fmsy <- data_8ass[data_8ass$stockid %in% stock_er,]$ERmsybest
data_8ass[data_8ass$stockid %in% stock_er,]$Fmsy_type <- "ERmsybest"

##################################################################################################

## B/Bmsy (SSB/SSBmsy; if unavailable, TB/TBmsy)
# data_2ass
data_2ass$B_by_Bmsy <- data_2ass$SSB_by_SSBmsy
data_2ass$B_by_Bmsy_type <- "SSB_by_SSBmsy"
data_2ass[is.na(data_2ass$B_by_Bmsy),]$B_by_Bmsy_type <- "TB_by_TBmsy"
data_2ass[is.na(data_2ass$B_by_Bmsy),]$B_by_Bmsy <- data_2ass[is.na(data_2ass$B_by_Bmsy),]$TB_by_TBmsy
data_2ass[is.na(data_2ass$B_by_Bmsy),]$B_by_Bmsy_type <- NA

# data_3ass
data_3ass$B_by_Bmsy <- data_3ass$SSB_by_SSBmsy
data_3ass$B_by_Bmsy_type <- "SSB_by_SSBmsy"
data_3ass[is.na(data_3ass$B_by_Bmsy),]$B_by_Bmsy_type <- "TB_by_TBmsy"
data_3ass[is.na(data_3ass$B_by_Bmsy),]$B_by_Bmsy <- data_3ass[is.na(data_3ass$B_by_Bmsy),]$TB_by_TBmsy
data_3ass[is.na(data_3ass$B_by_Bmsy),]$B_by_Bmsy_type <- NA

# data_4ass
data_4ass$B_by_Bmsy <- data_4ass$SSB_by_SSBmsy
data_4ass$B_by_Bmsy_type <- "SSB_by_SSBmsy"
data_4ass[is.na(data_4ass$B_by_Bmsy),]$B_by_Bmsy_type <- "TB_by_TBmsy"
data_4ass[is.na(data_4ass$B_by_Bmsy),]$B_by_Bmsy <- data_4ass[is.na(data_4ass$B_by_Bmsy),]$TB_by_TBmsy
data_4ass[is.na(data_4ass$B_by_Bmsy),]$B_by_Bmsy_type <- NA

# data_5ass
data_5ass$B_by_Bmsy <- data_5ass$SSB_by_SSBmsy
data_5ass$B_by_Bmsy_type <- "SSB_by_SSBmsy"
data_5ass[is.na(data_5ass$B_by_Bmsy),]$B_by_Bmsy_type <- "TB_by_TBmsy"
data_5ass[is.na(data_5ass$B_by_Bmsy),]$B_by_Bmsy <- data_5ass[is.na(data_5ass$B_by_Bmsy),]$TB_by_TBmsy
data_5ass[is.na(data_5ass$B_by_Bmsy),]$B_by_Bmsy_type <- NA

# data_6ass
data_6ass$B_by_Bmsy <- data_6ass$SSB_by_SSBmsy
data_6ass$B_by_Bmsy_type <- "SSB_by_SSBmsy"
data_6ass[is.na(data_6ass$B_by_Bmsy),]$B_by_Bmsy_type <- "TB_by_TBmsy"
data_6ass[is.na(data_6ass$B_by_Bmsy),]$B_by_Bmsy <- data_6ass[is.na(data_6ass$B_by_Bmsy),]$TB_by_TBmsy
data_6ass[is.na(data_6ass$B_by_Bmsy),]$B_by_Bmsy_type <- NA

# data_7ass
data_7ass$B_by_Bmsy <- data_7ass$SSB_by_SSBmsy
data_7ass$B_by_Bmsy_type <- "SSB_by_SSBmsy"
data_7ass[is.na(data_7ass$B_by_Bmsy),]$B_by_Bmsy_type <- "TB_by_TBmsy"
data_7ass[is.na(data_7ass$B_by_Bmsy),]$B_by_Bmsy <- data_7ass[is.na(data_7ass$B_by_Bmsy),]$TB_by_TBmsy
data_7ass[is.na(data_7ass$B_by_Bmsy),]$B_by_Bmsy_type <- NA

# data_8ass
data_8ass$B_by_Bmsy <- data_8ass$SSB_by_SSBmsy
data_8ass$B_by_Bmsy_type <- "SSB_by_SSBmsy"
data_8ass[is.na(data_8ass$B_by_Bmsy),]$B_by_Bmsy_type <- "TB_by_TBmsy"
data_8ass[is.na(data_8ass$B_by_Bmsy),]$B_by_Bmsy <- data_8ass[is.na(data_8ass$B_by_Bmsy),]$TB_by_TBmsy
data_8ass[is.na(data_8ass$B_by_Bmsy),]$B_by_Bmsy_type <- NA

##################################################################################################

## F/Fmsy (choose InstF/InstFmsy or ER/ERmsy)
# data_2ass
data_2ass$F_by_Fmsy <- data_2ass$InstF_by_InstFmsy
data_2ass$F_by_Fmsy_type <- "InstF_by_InstFmsy"
data_2ass[is.na(data_2ass$F_by_Fmsy),]$F_by_Fmsy_type <- "ER_by_ERmsy"
data_2ass[is.na(data_2ass$F_by_Fmsy),]$F_by_Fmsy <- data_2ass[is.na(data_2ass$F_by_Fmsy),]$ER_by_ERmsy
data_2ass[is.na(data_2ass$F_by_Fmsy),]$F_by_Fmsy_type <- NA

# data_3ass
data_3ass$F_by_Fmsy <- data_3ass$InstF_by_InstFmsy
data_3ass$F_by_Fmsy_type <- "InstF_by_InstFmsy"
data_3ass[is.na(data_3ass$F_by_Fmsy),]$F_by_Fmsy_type <- "ER_by_ERmsy"
data_3ass[is.na(data_3ass$F_by_Fmsy),]$F_by_Fmsy <- data_3ass[is.na(data_3ass$F_by_Fmsy),]$ER_by_ERmsy
data_3ass[is.na(data_3ass$F_by_Fmsy),]$F_by_Fmsy_type <- NA

# data_4ass
data_4ass$F_by_Fmsy <- data_4ass$InstF_by_InstFmsy
data_4ass$F_by_Fmsy_type <- "InstF_by_InstFmsy"
data_4ass[is.na(data_4ass$F_by_Fmsy),]$F_by_Fmsy_type <- "ER_by_ERmsy"
data_4ass[is.na(data_4ass$F_by_Fmsy),]$F_by_Fmsy <- data_4ass[is.na(data_4ass$F_by_Fmsy),]$ER_by_ERmsy
data_4ass[is.na(data_4ass$F_by_Fmsy),]$F_by_Fmsy_type <- NA

# data_5ass
data_5ass$F_by_Fmsy <- data_5ass$InstF_by_InstFmsy
data_5ass$F_by_Fmsy_type <- "InstF_by_InstFmsy"
data_5ass[is.na(data_5ass$F_by_Fmsy),]$F_by_Fmsy_type <- "ER_by_ERmsy"
data_5ass[is.na(data_5ass$F_by_Fmsy),]$F_by_Fmsy <- data_5ass[is.na(data_5ass$F_by_Fmsy),]$ER_by_ERmsy
data_5ass[is.na(data_5ass$F_by_Fmsy),]$F_by_Fmsy_type <- NA

# data_6ass
data_6ass$F_by_Fmsy <- data_6ass$InstF_by_InstFmsy
data_6ass$F_by_Fmsy_type <- "InstF_by_InstFmsy"
data_6ass[is.na(data_6ass$F_by_Fmsy),]$F_by_Fmsy_type <- "ER_by_ERmsy"
data_6ass[is.na(data_6ass$F_by_Fmsy),]$F_by_Fmsy <- data_6ass[is.na(data_6ass$F_by_Fmsy),]$ER_by_ERmsy
data_6ass[is.na(data_6ass$F_by_Fmsy),]$F_by_Fmsy_type <- NA

# data_7ass
data_7ass$F_by_Fmsy <- data_7ass$InstF_by_InstFmsy
data_7ass$F_by_Fmsy_type <- "InstF_by_InstFmsy"
data_7ass[is.na(data_7ass$F_by_Fmsy),]$F_by_Fmsy_type <- "ER_by_ERmsy"
data_7ass[is.na(data_7ass$F_by_Fmsy),]$F_by_Fmsy <- data_7ass[is.na(data_7ass$F_by_Fmsy),]$ER_by_ERmsy
data_7ass[is.na(data_7ass$F_by_Fmsy),]$F_by_Fmsy_type <- NA

# data_8ass
data_8ass$F_by_Fmsy <- data_8ass$InstF_by_InstFmsy
data_8ass$F_by_Fmsy_type <- "InstF_by_InstFmsy"
data_8ass[is.na(data_8ass$F_by_Fmsy),]$F_by_Fmsy_type <- "ER_by_ERmsy"
data_8ass[is.na(data_8ass$F_by_Fmsy),]$F_by_Fmsy <- data_8ass[is.na(data_8ass$F_by_Fmsy),]$ER_by_ERmsy
data_8ass[is.na(data_8ass$F_by_Fmsy),]$F_by_Fmsy_type <- NA

##################################################################################################

## OFL_ty (choose OFL_instF, OFL_instF_2 or OFL_ER)
# data_2ass
data_2ass$OFL_ty <- data_2ass$OFL_instF
data_2ass$OFL_ty_type <- "OFL_instF"
data_2ass[is.na(data_2ass$OFL_ty),]$OFL_ty_type <- "OFL_ER"
data_2ass[is.na(data_2ass$OFL_ty),]$OFL_ty <- data_2ass[is.na(data_2ass$OFL_ty),]$OFL_ER
data_2ass[is.na(data_2ass$OFL_ty),]$OFL_ty_type <- "OFL_instF_2"
data_2ass[is.na(data_2ass$OFL_ty),]$OFL_ty <- data_2ass[is.na(data_2ass$OFL_ty),]$OFL_instF_2
data_2ass[is.na(data_2ass$OFL_ty),]$OFL_ty_type <- NA

# data_3ass
data_3ass$OFL_ty <- data_3ass$OFL_instF
data_3ass$OFL_ty_type <- "OFL_instF"
data_3ass[is.na(data_3ass$OFL_ty),]$OFL_ty_type <- "OFL_ER"
data_3ass[is.na(data_3ass$OFL_ty),]$OFL_ty <- data_3ass[is.na(data_3ass$OFL_ty),]$OFL_ER
data_3ass[is.na(data_3ass$OFL_ty),]$OFL_ty_type <- "OFL_instF_2"
data_3ass[is.na(data_3ass$OFL_ty),]$OFL_ty <- data_3ass[is.na(data_3ass$OFL_ty),]$OFL_instF_2
data_3ass[is.na(data_3ass$OFL_ty),]$OFL_ty_type <- NA

# data_4ass
data_4ass$OFL_ty <- data_4ass$OFL_instF
data_4ass$OFL_ty_type <- "OFL_instF"
data_4ass[is.na(data_4ass$OFL_ty),]$OFL_ty_type <- "OFL_ER"
data_4ass[is.na(data_4ass$OFL_ty),]$OFL_ty <- data_4ass[is.na(data_4ass$OFL_ty),]$OFL_ER
data_4ass[is.na(data_4ass$OFL_ty),]$OFL_ty_type <- "OFL_instF_2"
data_4ass[is.na(data_4ass$OFL_ty),]$OFL_ty <- data_4ass[is.na(data_4ass$OFL_ty),]$OFL_instF_2
data_4ass[is.na(data_4ass$OFL_ty),]$OFL_ty_type <- NA

# data_5ass
data_5ass$OFL_ty <- data_5ass$OFL_instF
data_5ass$OFL_ty_type <- "OFL_instF"
data_5ass[is.na(data_5ass$OFL_ty),]$OFL_ty_type <- "OFL_ER"
data_5ass[is.na(data_5ass$OFL_ty),]$OFL_ty <- data_5ass[is.na(data_5ass$OFL_ty),]$OFL_ER
data_5ass[is.na(data_5ass$OFL_ty),]$OFL_ty_type <- "OFL_instF_2"
data_5ass[is.na(data_5ass$OFL_ty),]$OFL_ty <- data_5ass[is.na(data_5ass$OFL_ty),]$OFL_instF_2
data_5ass[is.na(data_5ass$OFL_ty),]$OFL_ty_type <- NA

# data_6ass
data_6ass$OFL_ty <- data_6ass$OFL_instF
data_6ass$OFL_ty_type <- "OFL_instF"
data_6ass[is.na(data_6ass$OFL_ty),]$OFL_ty_type <- "OFL_ER"
data_6ass[is.na(data_6ass$OFL_ty),]$OFL_ty <- data_6ass[is.na(data_6ass$OFL_ty),]$OFL_ER
data_6ass[is.na(data_6ass$OFL_ty),]$OFL_ty_type <- "OFL_instF_2"
data_6ass[is.na(data_6ass$OFL_ty),]$OFL_ty <- data_6ass[is.na(data_6ass$OFL_ty),]$OFL_instF_2
data_6ass[is.na(data_6ass$OFL_ty),]$OFL_ty_type <- NA

# data_7ass
data_7ass$OFL_ty <- data_7ass$OFL_instF
data_7ass$OFL_ty_type <- "OFL_instF"
data_7ass[is.na(data_7ass$OFL_ty),]$OFL_ty_type <- "OFL_ER"
data_7ass[is.na(data_7ass$OFL_ty),]$OFL_ty <- data_7ass[is.na(data_7ass$OFL_ty),]$OFL_ER
data_7ass[is.na(data_7ass$OFL_ty),]$OFL_ty_type <- "OFL_instF_2"
data_7ass[is.na(data_7ass$OFL_ty),]$OFL_ty <- data_7ass[is.na(data_7ass$OFL_ty),]$OFL_instF_2
data_7ass[is.na(data_7ass$OFL_ty),]$OFL_ty_type <- NA

# data_8ass
data_8ass$OFL_ty <- data_8ass$OFL_instF
data_8ass$OFL_ty_type <- "OFL_instF"
data_8ass[is.na(data_8ass$OFL_ty),]$OFL_ty_type <- "OFL_ER"
data_8ass[is.na(data_8ass$OFL_ty),]$OFL_ty <- data_8ass[is.na(data_8ass$OFL_ty),]$OFL_ER
data_8ass[is.na(data_8ass$OFL_ty),]$OFL_ty_type <- "OFL_instF_2"
data_8ass[is.na(data_8ass$OFL_ty),]$OFL_ty <- data_8ass[is.na(data_8ass$OFL_ty),]$OFL_instF_2
data_8ass[is.na(data_8ass$OFL_ty),]$OFL_ty_type <- NA

##################################################################################################

write.csv(data_2ass, paste(outputdir, "0g_1_data_2_assess_msy_ofl.csv", sep="/"), row.names=F)
write.csv(data_3ass, paste(outputdir, "0g_2_data_3_assess_msy_ofl.csv", sep="/"), row.names=F)
write.csv(data_4ass, paste(outputdir, "0g_3_data_4_assess_msy_ofl.csv", sep="/"), row.names=F)
write.csv(data_5ass, paste(outputdir, "0g_4_data_5_assess_msy_ofl.csv", sep="/"), row.names=F)
write.csv(data_6ass, paste(outputdir, "0g_5_data_6_assess_msy_ofl.csv", sep="/"), row.names=F)
write.csv(data_7ass, paste(outputdir, "0g_6_data_7_assess_msy_ofl.csv", sep="/"), row.names=F)
write.csv(data_8ass, paste(outputdir, "0g_7_data_8_assess_msy_ofl.csv", sep="/"), row.names=F)

