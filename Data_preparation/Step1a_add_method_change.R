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

#####################################################################################################################

## Data with 2 assessments ##
dd_2ass$stock_comparison <- paste(dd_2ass$stockid, dd_2ass$ty_order, sep="_")
data_2ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_2ass$stock_comparison)), ncol=7))
colnames(data_2ass) <- c("stockid", "species", "ty_old", "ty_recent", "comparison", "stock_comparison", "MethodChange")
data_2ass$stock_comparison <- unique(dd_2ass$stock_comparison)
for (i in 1:length(unique(dd_2ass$stock_comparison))){
  data_2ass[i,1] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,6],]$stockid)
  data_2ass[i,2] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,6],]$species)
  data_2ass[i,3] <- min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,6],]$report_year)
  data_2ass[i,4] <- max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,6],]$report_year)
  data_2ass[i,5] <- unique(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,6],]$ty_order)
  data_2ass[i,7] <- paste(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,6] & dd_2ass$assess_order==max(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,6],]$assess_order),]$method,
                          dd_2ass[dd_2ass$stock_comparison==data_2ass[i,6] & dd_2ass$assess_order==min(dd_2ass[dd_2ass$stock_comparison==data_2ass[i,6],]$assess_order),]$method,
                          sep="->")
}


## Data with 3 assessments ##
dd_3ass$stock_comparison <- paste(dd_3ass$stockid, dd_3ass$ty_order, sep="_")
data_3ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_3ass$stock_comparison)), ncol=7))
colnames(data_3ass) <- c("stockid", "species", "ty_old", "ty_recent", "comparison", "stock_comparison", "MethodChange")
data_3ass$stock_comparison <- unique(dd_3ass$stock_comparison)
for (i in 1:length(unique(dd_3ass$stock_comparison))){
  data_3ass[i,1] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,6],]$stockid)
  data_3ass[i,2] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,6],]$species)
  data_3ass[i,3] <- min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,6],]$report_year)
  data_3ass[i,4] <- max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,6],]$report_year)
  data_3ass[i,5] <- unique(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,6],]$ty_order)
  data_3ass[i,7] <- paste(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,6] & dd_3ass$assess_order==max(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,6],]$assess_order),]$method,
                          dd_3ass[dd_3ass$stock_comparison==data_3ass[i,6] & dd_3ass$assess_order==min(dd_3ass[dd_3ass$stock_comparison==data_3ass[i,6],]$assess_order),]$method,
                          sep="->")
}


## Data with 4 assessments ##
dd_4ass$stock_comparison <- paste(dd_4ass$stockid, dd_4ass$ty_order, sep="_")
data_4ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_4ass$stock_comparison)), ncol=7))
colnames(data_4ass) <- c("stockid", "species", "ty_old", "ty_recent", "comparison", "stock_comparison", "MethodChange")
data_4ass$stock_comparison <- unique(dd_4ass$stock_comparison)
for (i in 1:length(unique(dd_4ass$stock_comparison))){
  data_4ass[i,1] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,6],]$stockid)
  data_4ass[i,2] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,6],]$species)
  data_4ass[i,3] <- min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,6],]$report_year)
  data_4ass[i,4] <- max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,6],]$report_year)
  data_4ass[i,5] <- unique(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,6],]$ty_order)
  data_4ass[i,7] <- paste(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,6] & dd_4ass$assess_order==max(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,6],]$assess_order),]$method,
                          dd_4ass[dd_4ass$stock_comparison==data_4ass[i,6] & dd_4ass$assess_order==min(dd_4ass[dd_4ass$stock_comparison==data_4ass[i,6],]$assess_order),]$method,
                          sep="->")
}


## Data with 5 assessments ##
dd_5ass$stock_comparison <- paste(dd_5ass$stockid, dd_5ass$ty_order, sep="_")
data_5ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_5ass$stock_comparison)), ncol=7))
colnames(data_5ass) <- c("stockid", "species", "ty_old", "ty_recent", "comparison", "stock_comparison", "MethodChange")
data_5ass$stock_comparison <- unique(dd_5ass$stock_comparison)
for (i in 1:length(unique(dd_5ass$stock_comparison))){
  data_5ass[i,1] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,6],]$stockid)
  data_5ass[i,2] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,6],]$species)
  data_5ass[i,3] <- min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,6],]$report_year)
  data_5ass[i,4] <- max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,6],]$report_year)
  data_5ass[i,5] <- unique(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,6],]$ty_order)
  data_5ass[i,7] <- paste(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,6] & dd_5ass$assess_order==max(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,6],]$assess_order),]$method,
                          dd_5ass[dd_5ass$stock_comparison==data_5ass[i,6] & dd_5ass$assess_order==min(dd_5ass[dd_5ass$stock_comparison==data_5ass[i,6],]$assess_order),]$method,
                          sep="->")
}


## Data with 6 assessments ##
dd_6ass$stock_comparison <- paste(dd_6ass$stockid, dd_6ass$ty_order, sep="_")
data_6ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_6ass$stock_comparison)), ncol=7))
colnames(data_6ass) <- c("stockid", "species", "ty_old", "ty_recent", "comparison", "stock_comparison", "MethodChange")
data_6ass$stock_comparison <- unique(dd_6ass$stock_comparison)
for (i in 1:length(unique(dd_6ass$stock_comparison))){
  data_6ass[i,1] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,6],]$stockid)
  data_6ass[i,2] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,6],]$species)
  data_6ass[i,3] <- min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,6],]$report_year)
  data_6ass[i,4] <- max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,6],]$report_year)
  data_6ass[i,5] <- unique(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,6],]$ty_order)
  data_6ass[i,7] <- paste(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,6] & dd_6ass$assess_order==max(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,6],]$assess_order),]$method,
                          dd_6ass[dd_6ass$stock_comparison==data_6ass[i,6] & dd_6ass$assess_order==min(dd_6ass[dd_6ass$stock_comparison==data_6ass[i,6],]$assess_order),]$method,
                          sep="->")
}


## Data with 7 assessments ##
dd_7ass$stock_comparison <- paste(dd_7ass$stockid, dd_7ass$ty_order, sep="_")
data_7ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_7ass$stock_comparison)), ncol=7))
colnames(data_7ass) <- c("stockid", "species", "ty_old", "ty_recent", "comparison", "stock_comparison", "MethodChange")
data_7ass$stock_comparison <- unique(dd_7ass$stock_comparison)
for (i in 1:length(unique(dd_7ass$stock_comparison))){
  data_7ass[i,1] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,6],]$stockid)
  data_7ass[i,2] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,6],]$species)
  data_7ass[i,3] <- min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,6],]$report_year)
  data_7ass[i,4] <- max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,6],]$report_year)
  data_7ass[i,5] <- unique(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,6],]$ty_order)
  data_7ass[i,7] <- paste(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,6] & dd_7ass$assess_order==max(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,6],]$assess_order),]$method,
                          dd_7ass[dd_7ass$stock_comparison==data_7ass[i,6] & dd_7ass$assess_order==min(dd_7ass[dd_7ass$stock_comparison==data_7ass[i,6],]$assess_order),]$method,
                          sep="->")
}


## Data with 8 assessments ##
dd_8ass$stock_comparison <- paste(dd_8ass$stockid, dd_8ass$ty_order, sep="_")
data_8ass <- as.data.frame(matrix(NA, nrow=length(unique(dd_8ass$stock_comparison)), ncol=7))
colnames(data_8ass) <- c("stockid", "species", "ty_old", "ty_recent", "comparison", "stock_comparison", "MethodChange")
data_8ass$stock_comparison <- unique(dd_8ass$stock_comparison)
for (i in 1:length(unique(dd_8ass$stock_comparison))){
  data_8ass[i,1] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,6],]$stockid)
  data_8ass[i,2] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,6],]$species)
  data_8ass[i,3] <- min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,6],]$report_year)
  data_8ass[i,4] <- max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,6],]$report_year)
  data_8ass[i,5] <- unique(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,6],]$ty_order)
  data_8ass[i,7] <- paste(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,6] & dd_8ass$assess_order==max(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,6],]$assess_order),]$method,
                          dd_8ass[dd_8ass$stock_comparison==data_8ass[i,6] & dd_8ass$assess_order==min(dd_8ass[dd_8ass$stock_comparison==data_8ass[i,6],]$assess_order),]$method,
                          sep="->")
}


####################################################################################################################

## Put data together ##
data_full <- rbind(data_2ass, data_3ass, data_4ass, data_5ass, data_6ass, data_7ass, data_8ass)

####################################################################################################################

## pick information on species, ty_old, ty_recent
info <- data_full[,c(6,2,3,4)]
write.csv(info, paste(outputdir, "1a_species_ty_info.csv", sep="/"), row.names=F)

####################################################################################################################

## Method unchange (1) or change (2) ##

key_method_change <- as.data.frame(table(data_full$MethodChange))
#write.csv(key_method_change, paste(outputdir, "1a_key_method_change.csv", sep="/"), row.names=F)

#key_method_change <- read.csv("1a_key_method_change.csv")
dd1a <- merge(data_full, key_method_change, by="MethodChange", all.x=TRUE)
                data_full[is.na(data_full$MethodChangeCode),]$MethodChangeCode  # 0
table(dd1a$Method)
dd1a[dd1a$Method=="Length-based",]
dd1a[dd1a$Method=="Index-based",]
dd1a[dd1a$Method=="Length-structured",]
dd1a[dd1a$Method=="Size-structured",]
table(dd1a$MethodChangeType)

dd1a[dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown",]
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="FMEG8c9a",]$MethodChange <- 
  "Age-based assessment (XSA)->Age-based assessment (XSA)"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="FMEG8c9a",]$Method <- "Age-structured"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="FMEG8c9a",]$MethodChangeType <- "Unchanged"

dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="SNOWCRABBS",]$MethodChange <- 
  "Size-based biomass dynamics model->Size-based biomass dynamics model"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="SNOWCRABBS",]$Method <- "Length-based"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="SNOWCRABBS",]$MethodChangeType <- "Unchanged"

dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="TANNERCRABBSAI",]$MethodChange <- 
  "Size-based biomass dynamics model->Size-based biomass dynamics model"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="TANNERCRABBSAI",]$Method <- "Length-based"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="TANNERCRABBSAI",]$MethodChangeType <- "Unchanged"

dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="SBWHITACIR",]$MethodChange <- 
  "Integrated Analysis->Integrated Analysis"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="SBWHITACIR",]$MethodChangeType <- "Unchanged"

dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="COD4TVn",]$MethodChange <- 
  "Statistical catch at age model->Statistical catch at age model"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="COD4TVn",]$MethodChangeType <- "Unchanged"

dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="COD3Ps",]$MethodChange <- 
  "Survey index->Survey index"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="COD3Ps",]$MethodChangeType <- "Unchanged"

dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="MACKNWATLSA3-4",]$MethodChange <- 
  "Age-structured model->Age-structured model"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="MACKNWATLSA3-4",]$Method <- "Age-structured"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="MACKNWATLSA3-4",]$MethodChangeType <- "Unchanged"

dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="WHITVIIa",]$MethodChange <- 
  "Age-structured model->Age-structured model"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="WHITVIIa",]$Method <- "Age-structured"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="WHITVIIa",]$MethodChangeType <- "Unchanged"

dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="SAURNWPAC",]$MethodChange <- 
  "VPA->VPA"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="SAURNWPAC",]$Method <- "Age-structured"
dd1a[(dd1a$Method=="Unknown" | dd1a$MethodChangeType=="Unknown") & dd1a$stockid=="SAURNWPAC",]$MethodChangeType <- "Unchanged"
  
##################################################################################################

write.csv(dd1a, paste(outputdir, "1a_data_method_change.csv", sep="/"), row.names=F)
