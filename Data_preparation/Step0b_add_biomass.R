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

data <- read.csv(paste(outputdir, "0a_stocks_with_2+_assesses.csv", sep="/"), as.is=T)

#####################################################################################################################

## Read data 
bioparams_vals <- bioparams_values_views  # 639 stocks with 639 rows
bioparams_units <- bioparams_units_views
bioparams_assess <- bioparams_assessments_views

ts <- timeseries  # 1048437 rows; 1373 stocks, 2215 assessments
ts <- ts[ts$assessid %in% data$assessid,]   # 448 stocks, 1281 assessments
metric <- tsmetrics
table(metric$tscategory)
values <- ts %>% 
  left_join(metric, by=c("tsid"="tsunique"))
values_TB <- values[values$tscategory=="TOTAL BIOMASS" | values$tscategory=="SPAWNING STOCK BIOMASS or CPUE",] 

# For assessment "DFO-NFLD-COD3Ps-1959-2011-CHING": TB-1 is offshore, TB-2 is all strata <300fms
values_TB[values_TB$assessid=="DFO-NFLD-COD3Ps-1959-2011-CHING" & values_TB$tsid=="TB-1-MT",]$tsid <- "TB-MT"

# Pick assessments only with TB ratios
values_TB_abs <- values_TB[values_TB$tsid=="TBbest-MT" | values_TB$tsid=="TB-MT" | values_TB$tsid=="TN-E00" | values_TB$tsid == "SSB-MT" | 
                             values_TB$tsid == "SSB-E00eggs" | values_TB$tsid=="TB-relative" | values_TB$tsid == "SSB-relative",]
values_TB_ratio <- values_TB[values_TB$tsid=="BdivBmsypref-dimensionless" | values_TB$tsid=="SSBdivSSBmsy-calc-dimensionless" | values_TB$tsid=="SSBdivSSBmsy-dimensionless" |
                               values_TB$tsid=="TBdivTBmsy-calc-dimensionless" | values_TB$tsid=="TBdivTBmsy-dimensionless",]
assessid_TB_abs <- unique(values_TB_abs$assessid)
assessid_TB_ratio <- unique(values_TB_ratio$assessid)
assessids_TB <- assessid_TB_ratio[!assessid_TB_ratio %in% assessid_TB_abs]  # 43 assessments only with B/Bmsy ratios
assessids_TB_with_TBmsy <- na.omit(bioparams_assess$TBmsybest)
assessids_TB_with_SSBmsy <- na.omit(bioparams_assess$SSBmsybest)
assessids_TB_add_TBmsy <- assessids_TB[assessids_TB %in% assessids_TB_with_TBmsy]  # find 3 assessments with TBmsybest 
assessids_TB_add_SSBmsy <- assessids_TB[assessids_TB %in% assessids_TB_with_SSBmsy]  # find 0 assessment with SSBmsybest 

vv_TB <- values_TB[values_TB$assessid %in% assessids_TB_add_TBmsy & (values_TB$tsid=="TBdivTBmsy-calc-dimensionless" | values_TB$tsid=="TBdivTBmsy-dimensionless"),]
values_TB_add <- vv_TB
values_TB_add$tsid <- "TBbest-MT"; values_TB_add$tsunitsshort <- "MT"
for (i in 1:length(unique(values_TB_add$assessid))){
  TBmsy <- ifelse(length(bioparams_vals[bioparams_vals$stockid==vv_TB[vv_TB$assessid==assessids_TB_add_TBmsy[i],]$stockid[1],]$TBmsybest)!=0, 
                  bioparams_vals[bioparams_vals$stockid==vv_TB[vv_TB$assessid==assessids_TB_add_TBmsy[i],]$stockid[1],]$TBmsybest, NA)
  values_TB_add[values_TB_add$assessid==assessids_TB_add_TBmsy[i],]$tsvalue <- vv_TB[vv_TB$assessid==assessids_TB_add_TBmsy[i],]$tsvalue*TBmsy
}

# Combine values_add with original values
values_TB <- rbind(values_TB, values_TB_add)
values_TB <- values_TB[!is.na(values_TB$tsvalue),]
values_TB$key <- paste(values_TB$assessid, values_TB$tsyear, sep="_")
table(values_TB$tsid)

########################################################################################################################################

# Add Total Biomass / Spawning Stock Biomass (TB/SSB)
data$key_ty <- paste(data$assessid, data$ty, sep="_")
data$key_ty_minus1 <- paste(data$assessid, data$ty_minus1, sep="_")
key_id <- c(data$key_ty, data$key_ty_minus1)

values_TB_filtered <- values_TB[values_TB$key %in% key_id & !is.na(values_TB$tsvalue),]
ids_TB <- unique(values_TB_filtered$tsid)

values_TB_ty <- values_TB_filtered[values_TB_filtered$tsid=="TBbest-MT" | values_TB_filtered$tsid=="TB-MT" | values_TB_filtered$tsid=="TN-E00" |
                                     values_TB_filtered$tsid=="SSB-MT" | values_TB_filtered$tsid=="SSB-E00eggs",]

b1 <- values_TB_ty[values_TB_ty$tsid=="TBbest-MT",]  # 272 stocks, 676 assesses
b2 <- values_TB_ty[values_TB_ty$tsid=="TB-MT",]  # 269 stocks, 674 assesses
b3 <- values_TB_ty[values_TB_ty$tsid=="TN-E00",]  # 28 stocks, 33 assesses
b4 <- values_TB_ty[values_TB_ty$tsid=="SSB-MT",]  # 281 stocks, 798 assesses
b5 <- values_TB_ty[values_TB_ty$tsid=="SSB-E00eggs",]  # 12 stocks, 18 assesses

ddb <- as.data.frame(matrix(NA, nrow=length(unique(values_TB$stockid)), ncol=7))
colnames(ddb) <- c("stockid", "number_of_assess_TBbest", "number_of_assess_TB", "number_of_assess_TN", "number_of_assess_SSB", "number_of_assess_SSB_E00eggs", "B_id")
ddb$stockid <- unique(values_TB$stockid)
for (i in 1:length(unique(values_TB$stockid))){
  ddb[i,2] <- length(unique(b1[b1$stockid==ddb[i,1],]$assessid))
  ddb[i,3] <- length(unique(b2[b2$stockid==ddb[i,1],]$assessid))
  ddb[i,4] <- length(unique(b3[b3$stockid==ddb[i,1],]$assessid))
  ddb[i,5] <- length(unique(b4[b4$stockid==ddb[i,1],]$assessid))
  ddb[i,6] <- length(unique(b5[b5$stockid==ddb[i,1],]$assessid))
  vec <- c(ddb[i,2], ddb[i,3], ddb[i,4], ddb[i,5], ddb[i,6])
  ddb[i,7] <- which.max(vec)
}

stock_TBbest <- ddb[ddb$B_id==1,]$stockid
stock_TB <- ddb[ddb$B_id==2,]$stockid
stock_TN <- ddb[ddb$B_id==3,]$stockid
stock_SSB <- ddb[ddb$B_id==4,]$stockid
stock_SSB_E00eggs <- ddb[ddb$B_id==5,]$stockid

b1 <- b1[b1$stockid %in% stock_TBbest,]   # 197 stocks, 505 assesses
b2 <- b2[b2$stockid %in% stock_TB,]   # 1 stock, 2 assesses
b3 <- b3[b3$stockid %in% stock_TN,]   # 6 stocks, 10 assesses
b4 <- b4[b4$stockid %in% stock_SSB,]   # 125 stocks, 377 assesses
b5 <- b5[b5$stockid %in% stock_SSB_E00eggs,]   # 4 stocks, 8 assesses

values_b_together <- rbind(b1, b2, b3, b4, b5)

# Add TB at ty
data_with_b_ty <- data %>% 
  left_join(select(values_b_together, tsvalue, tsunitsshort, tsid, key), by=c("key_ty"="key")) %>% 
  rename(B_ty=tsvalue, B_unit=tsunitsshort, B_type=tsid) 

# Add TB at ty-1
data_with_b_ty_minus1 <- data_with_b_ty %>% 
  left_join(select(values_b_together, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(B_ty_minus1=tsvalue) 

data_with_bs <- data_with_b_ty_minus1[,c(1:46,49,48,47)]
dim(data_with_bs)  # 1281
summary(data_with_bs$B_ty)  # 379 NAs in B_ty

########################################################################################################################################

## Add TBbest, TB, TN, SSB, SSB_E00eggs to data
b1 <- values_TB_ty[values_TB_ty$tsid=="TBbest-MT",]  # 272 stocks, 676 assesses
b2 <- values_TB_ty[values_TB_ty$tsid=="TB-MT",]  # 269 stocks, 674 assesses
b3 <- values_TB_ty[values_TB_ty$tsid=="TN-E00",]  # 28 stocks, 33 assesses
b4 <- values_TB_ty[values_TB_ty$tsid=="SSB-MT",]  # 281 stocks, 798 assesses
b5 <- values_TB_ty[values_TB_ty$tsid=="SSB-E00eggs",]  # 12 stocks, 18 assesses

# add TBbest at ty and ty-1
data_with_bs_TBbest_ty <- data_with_bs %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TBbest_ty=tsvalue) 

data_with_bs_TBbest_ty_minus1 <- data_with_bs_TBbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TBbest_ty_minus1=tsvalue) 

# add TB at ty and ty-1
data_with_bs_TB_ty <- data_with_bs_TBbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TB_ty=tsvalue) 

data_with_bs_TB_ty_minus1 <- data_with_bs_TB_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TB_ty_minus1=tsvalue) 

# add TN at ty and ty-1
data_with_bs_TN_ty <- data_with_bs_TB_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TN_ty=tsvalue) 

data_with_bs_TN_ty_minus1 <- data_with_bs_TN_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TN_ty_minus1=tsvalue) 

# add SSB at ty and ty-1
data_with_bs_SSB_ty <- data_with_bs_TN_ty_minus1 %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_ty=tsvalue) 

data_with_bs_SSB_ty_minus1 <- data_with_bs_SSB_ty %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_ty_minus1=tsvalue) 

# add SSB_E00eggs at ty and ty-1
data_with_bs_SSB_E00eggs_ty <- data_with_bs_SSB_ty_minus1 %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_E00eggs_ty=tsvalue) 

data_with_bs_SSB_E00eggs_ty_minus1 <- data_with_bs_SSB_E00eggs_ty %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_E00eggs_ty_minus1=tsvalue) 

########################################################################################################################################

# Export full data
data_with_bs <- data_with_bs_SSB_E00eggs_ty_minus1
write.csv(data_with_bs, paste(outputdir, "0b_full_data_with_biomass.csv", sep="/"), row.names=F)

########################################################################################################################################

# Delete assessments without B records
data_with_b <- data_with_bs[!is.na(data_with_bs$B_ty),]

# Check number of assessments for each stock
data_num_assess <- as.data.frame(aggregate(assessid ~ stockid, data = data_with_b, FUN = length))
table(data_num_assess$assessid)  
#  1   2   3   4   5   6   7   8 
# 50 203  18   4   8  18  28   4
stockid_keep_2plus_assess <- unique(data_num_assess[data_num_assess$assessid > 1,]$stockid)
data_2plus_assess_with_b <- data_with_b[data_with_b$stockid %in% stockid_keep_2plus_assess,]  # 283 stocks
length(unique(data_2plus_assess_with_b[data_2plus_assess_with_b$country=="USA",]$stockid))  # 75
length(unique(data_2plus_assess_with_b[data_2plus_assess_with_b$region=="Europe (EU)",]$stockid))  # 61
length(unique(data_2plus_assess_with_b[data_2plus_assess_with_b$country=="Canada",]$stockid))  # 21
length(unique(data_2plus_assess_with_b[data_2plus_assess_with_b$country=="Japan",]$stockid))  # 14

# Check if assessments for each stock have the same B_type
ccheck <- data.frame(matrix(NA, nrow=length(unique(data_2plus_assess_with_b$stockid)), ncol=2))
colnames(ccheck) <- c("stockid", "B_type")
ccheck$stockid <- unique(data_2plus_assess_with_b$stockid)
for (i in 1:length(unique(data_2plus_assess_with_b$stockid))){
  ccheck[i,2] <- length(unique(data_2plus_assess_with_b[data_2plus_assess_with_b$stockid==ccheck[i,1],]$B_type))
}
table(ccheck$B_type)  # all=1, no problem

