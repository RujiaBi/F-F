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

data_2ass <- read.csv(paste(outputdir, "0e_1_data_2_assess_with_records.csv", sep="/"), as.is=T)
data_2ass_keep <- data_2ass[data_2ass$check_ty_correct=="Y",]; data_2ass_add <- data_2ass[data_2ass$check_ty_correct=="N",c(1:45,48:49,62:63,70:89)]

data_3ass <- read.csv(paste(outputdir, "0e_2_data_3_assess_with_records.csv", sep="/"), as.is=T)
data_3ass_keep <- data_3ass[!is.na(data_3ass$B_ty),]; data_3ass_add <- data_3ass[is.na(data_3ass$B_ty),c(1:45,48:49,62:63,70:89)]

data_4ass <- read.csv(paste(outputdir, "0e_3_data_4_assess_with_records.csv", sep="/"), as.is=T)
data_4ass_keep <- data_4ass[!is.na(data_4ass$B_ty),]; data_4ass_add <- data_4ass[is.na(data_4ass$B_ty),c(1:45,48:49,62:63,70:89)]

data_5ass <- read.csv(paste(outputdir, "0e_4_data_5_assess_with_records.csv", sep="/"), as.is=T)
data_5ass_keep <- data_5ass[!is.na(data_5ass$B_ty),]; data_5ass_add <- data_5ass[is.na(data_5ass$B_ty),c(1:45,48:49,62:63,70:89)]

data_6ass <- read.csv(paste(outputdir, "0e_5_data_6_assess_with_records.csv", sep="/"), as.is=T)
data_6ass_keep <- data_6ass[!is.na(data_6ass$B_ty),]; data_6ass_add <- data_6ass[is.na(data_6ass$B_ty),c(1:45,48:49,62:63,70:89)]

data_7ass <- read.csv(paste(outputdir, "0e_6_data_7_assess_with_records.csv", sep="/"), as.is=T)
data_7ass_keep <- data_7ass[!is.na(data_7ass$B_ty),]; data_7ass_add <- data_7ass[is.na(data_7ass$B_ty),c(1:45,48:49,62:63,70:89)]

data_8ass <- read.csv(paste(outputdir, "0e_7_data_8_assess_with_records.csv", sep="/"), as.is=T)
data_8ass_keep <- data_8ass[!is.na(data_8ass$B_ty),]; data_8ass_add <- data_8ass[is.na(data_8ass$B_ty),c(1:45,48:49,62:63,70:89)]

############################################################################################################################################

## Read data 
bioparams_vals <- bioparams_values_views  # 639 stocks with 639 rows
bioparams_units <- bioparams_units_views
bioparams_assess <- bioparams_assessments_views

ts <- timeseries  # 1048437 rows; 1373 stocks, 2215 assessments
assessid <- c(data_2ass_add$assessid, data_3ass_add$assessid, data_4ass_add$assessid, data_5ass_add$assessid, 
              data_6ass_add$assessid, data_7ass_add$assessid, data_8ass_add$assessid)
ts <- ts[ts$assessid %in% assessid,]   # 76 stocks, 359 assessments
metric <- tsmetrics
table(metric$tscategory)
values_TB <- ts %>% 
  left_join(metric, by=c("tsid"="tsunique"))
values_TB <- values_TB[values_TB$tscategory=="TOTAL BIOMASS" | values_TB$tscategory=="SPAWNING STOCK BIOMASS or CPUE",] 

# Pick assessments only with ratios
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

# Add Biomass
key_id <- c(data_2ass_add$key_ty, data_2ass_add$key_ty_minus1, data_3ass_add$key_ty, data_3ass_add$key_ty_minus1,
            data_4ass_add$key_ty, data_4ass_add$key_ty_minus1, data_5ass_add$key_ty, data_5ass_add$key_ty_minus1,
            data_6ass_add$key_ty, data_6ass_add$key_ty_minus1, data_7ass_add$key_ty, data_7ass_add$key_ty_minus1,
            data_8ass_add$key_ty, data_8ass_add$key_ty_minus1)

values_TB_filtered <- values_TB[values_TB$key %in% key_id & !is.na(values_TB$tsvalue),]
ids_TB <- unique(values_TB_filtered$tsid)

values_TB_ty <- values_TB_filtered[values_TB_filtered$tsid=="TBbest-MT" | values_TB_filtered$tsid=="TB-MT" | values_TB_filtered$tsid=="TN-E00" |
                                     values_TB_filtered$tsid=="SSB-MT" | values_TB_filtered$tsid=="SSB-E00eggs",]

b1 <- values_TB_ty[values_TB_ty$tsid=="TBbest-MT",]  # 72 stocks, 293 assesses
b2 <- values_TB_ty[values_TB_ty$tsid=="TB-MT",]  # 72 stocks, 293 assesses
b3 <- values_TB_ty[values_TB_ty$tsid=="TN-E00",]  # 2 stocks, 3 assesses
b4 <- values_TB_ty[values_TB_ty$tsid=="SSB-MT",]  # 75 stocks, 353 assesses
b5 <- values_TB_ty[values_TB_ty$tsid=="SSB-E00eggs",]  # 0 stock

## data_2ass ##
# add TBbest at ty and ty-1
data_2ass_add_with_TBbest_ty <- data_2ass_add %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TBbest_ty=tsvalue) 

data_2ass_add_with_TBbest_ty_minus1 <- data_2ass_add_with_TBbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TBbest_ty_minus1=tsvalue) 

# add TB at ty and ty-1
data_2ass_add_with_TB_ty <- data_2ass_add_with_TBbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TB_ty=tsvalue) 

data_2ass_add_with_TB_ty_minus1 <- data_2ass_add_with_TB_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TB_ty_minus1=tsvalue) 

# add TN at ty and ty-1
data_2ass_add_with_TN_ty <- data_2ass_add_with_TB_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TN_ty=tsvalue) 

data_2ass_add_with_TN_ty_minus1 <- data_2ass_add_with_TN_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TN_ty_minus1=tsvalue) 

# add SSB at ty and ty-1
data_2ass_add_with_SSB_ty <- data_2ass_add_with_TN_ty_minus1 %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_ty=tsvalue) 

data_2ass_add_with_SSB_ty_minus1 <- data_2ass_add_with_SSB_ty %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_ty_minus1=tsvalue) 

# add SSB_E00eggs at ty and ty-1
data_2ass_add_with_SSB_E00eggs_ty <- data_2ass_add_with_SSB_ty_minus1 %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_E00eggs_ty=tsvalue) 

data_2ass_add_with_SSB_E00eggs_ty_minus1 <- data_2ass_add_with_SSB_E00eggs_ty %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_E00eggs_ty_minus1=tsvalue) 


## data_3ass ##
# add TBbest at ty and ty-1
data_3ass_add_with_TBbest_ty <- data_3ass_add %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TBbest_ty=tsvalue) 

data_3ass_add_with_TBbest_ty_minus1 <- data_3ass_add_with_TBbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TBbest_ty_minus1=tsvalue) 

# add TB at ty and ty-1
data_3ass_add_with_TB_ty <- data_3ass_add_with_TBbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TB_ty=tsvalue) 

data_3ass_add_with_TB_ty_minus1 <- data_3ass_add_with_TB_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TB_ty_minus1=tsvalue) 

# add TN at ty and ty-1
data_3ass_add_with_TN_ty <- data_3ass_add_with_TB_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TN_ty=tsvalue) 

data_3ass_add_with_TN_ty_minus1 <- data_3ass_add_with_TN_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TN_ty_minus1=tsvalue) 

# add SSB at ty and ty-1
data_3ass_add_with_SSB_ty <- data_3ass_add_with_TN_ty_minus1 %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_ty=tsvalue) 

data_3ass_add_with_SSB_ty_minus1 <- data_3ass_add_with_SSB_ty %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_ty_minus1=tsvalue) 

# add SSB_E00eggs at ty and ty-1
data_3ass_add_with_SSB_E00eggs_ty <- data_3ass_add_with_SSB_ty_minus1 %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_E00eggs_ty=tsvalue) 

data_3ass_add_with_SSB_E00eggs_ty_minus1 <- data_3ass_add_with_SSB_E00eggs_ty %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_E00eggs_ty_minus1=tsvalue)

## data_4ass ##
# add TBbest at ty and ty-1
data_4ass_add_with_TBbest_ty <- data_4ass_add %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TBbest_ty=tsvalue) 

data_4ass_add_with_TBbest_ty_minus1 <- data_4ass_add_with_TBbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TBbest_ty_minus1=tsvalue) 

# add TB at ty and ty-1
data_4ass_add_with_TB_ty <- data_4ass_add_with_TBbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TB_ty=tsvalue) 

data_4ass_add_with_TB_ty_minus1 <- data_4ass_add_with_TB_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TB_ty_minus1=tsvalue) 

# add TN at ty and ty-1
data_4ass_add_with_TN_ty <- data_4ass_add_with_TB_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TN_ty=tsvalue) 

data_4ass_add_with_TN_ty_minus1 <- data_4ass_add_with_TN_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TN_ty_minus1=tsvalue) 

# add SSB at ty and ty-1
data_4ass_add_with_SSB_ty <- data_4ass_add_with_TN_ty_minus1 %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_ty=tsvalue) 

data_4ass_add_with_SSB_ty_minus1 <- data_4ass_add_with_SSB_ty %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_ty_minus1=tsvalue) 

# add SSB_E00eggs at ty and ty-1
data_4ass_add_with_SSB_E00eggs_ty <- data_4ass_add_with_SSB_ty_minus1 %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_E00eggs_ty=tsvalue) 

data_4ass_add_with_SSB_E00eggs_ty_minus1 <- data_4ass_add_with_SSB_E00eggs_ty %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_E00eggs_ty_minus1=tsvalue)


## data_5ass ##
# add TBbest at ty and ty-1
data_5ass_add_with_TBbest_ty <- data_5ass_add %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TBbest_ty=tsvalue) 

data_5ass_add_with_TBbest_ty_minus1 <- data_5ass_add_with_TBbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TBbest_ty_minus1=tsvalue) 

# add TB at ty and ty-1
data_5ass_add_with_TB_ty <- data_5ass_add_with_TBbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TB_ty=tsvalue) 

data_5ass_add_with_TB_ty_minus1 <- data_5ass_add_with_TB_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TB_ty_minus1=tsvalue) 

# add TN at ty and ty-1
data_5ass_add_with_TN_ty <- data_5ass_add_with_TB_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TN_ty=tsvalue) 

data_5ass_add_with_TN_ty_minus1 <- data_5ass_add_with_TN_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TN_ty_minus1=tsvalue) 

# add SSB at ty and ty-1
data_5ass_add_with_SSB_ty <- data_5ass_add_with_TN_ty_minus1 %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_ty=tsvalue) 

data_5ass_add_with_SSB_ty_minus1 <- data_5ass_add_with_SSB_ty %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_ty_minus1=tsvalue) 

# add SSB_E00eggs at ty and ty-1
data_5ass_add_with_SSB_E00eggs_ty <- data_5ass_add_with_SSB_ty_minus1 %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_E00eggs_ty=tsvalue) 

data_5ass_add_with_SSB_E00eggs_ty_minus1 <- data_5ass_add_with_SSB_E00eggs_ty %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_E00eggs_ty_minus1=tsvalue)


## data_6ass ##
# add TBbest at ty and ty-1
data_6ass_add_with_TBbest_ty <- data_6ass_add %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TBbest_ty=tsvalue) 

data_6ass_add_with_TBbest_ty_minus1 <- data_6ass_add_with_TBbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TBbest_ty_minus1=tsvalue) 

# add TB at ty and ty-1
data_6ass_add_with_TB_ty <- data_6ass_add_with_TBbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TB_ty=tsvalue) 

data_6ass_add_with_TB_ty_minus1 <- data_6ass_add_with_TB_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TB_ty_minus1=tsvalue) 

# add TN at ty and ty-1
data_6ass_add_with_TN_ty <- data_6ass_add_with_TB_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TN_ty=tsvalue) 

data_6ass_add_with_TN_ty_minus1 <- data_6ass_add_with_TN_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TN_ty_minus1=tsvalue) 

# add SSB at ty and ty-1
data_6ass_add_with_SSB_ty <- data_6ass_add_with_TN_ty_minus1 %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_ty=tsvalue) 

data_6ass_add_with_SSB_ty_minus1 <- data_6ass_add_with_SSB_ty %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_ty_minus1=tsvalue) 

# add SSB_E00eggs at ty and ty-1
data_6ass_add_with_SSB_E00eggs_ty <- data_6ass_add_with_SSB_ty_minus1 %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_E00eggs_ty=tsvalue) 

data_6ass_add_with_SSB_E00eggs_ty_minus1 <- data_6ass_add_with_SSB_E00eggs_ty %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_E00eggs_ty_minus1=tsvalue)


## data_7ass ##
# add TBbest at ty and ty-1
data_7ass_add_with_TBbest_ty <- data_7ass_add %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TBbest_ty=tsvalue) 

data_7ass_add_with_TBbest_ty_minus1 <- data_7ass_add_with_TBbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TBbest_ty_minus1=tsvalue) 

# add TB at ty and ty-1
data_7ass_add_with_TB_ty <- data_7ass_add_with_TBbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TB_ty=tsvalue) 

data_7ass_add_with_TB_ty_minus1 <- data_7ass_add_with_TB_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TB_ty_minus1=tsvalue) 

# add TN at ty and ty-1
data_7ass_add_with_TN_ty <- data_7ass_add_with_TB_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TN_ty=tsvalue) 

data_7ass_add_with_TN_ty_minus1 <- data_7ass_add_with_TN_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TN_ty_minus1=tsvalue) 

# add SSB at ty and ty-1
data_7ass_add_with_SSB_ty <- data_7ass_add_with_TN_ty_minus1 %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_ty=tsvalue) 

data_7ass_add_with_SSB_ty_minus1 <- data_7ass_add_with_SSB_ty %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_ty_minus1=tsvalue) 

# add SSB_E00eggs at ty and ty-1
data_7ass_add_with_SSB_E00eggs_ty <- data_7ass_add_with_SSB_ty_minus1 %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_E00eggs_ty=tsvalue) 

data_7ass_add_with_SSB_E00eggs_ty_minus1 <- data_7ass_add_with_SSB_E00eggs_ty %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_E00eggs_ty_minus1=tsvalue)


## data_8ass ##
# add TBbest at ty and ty-1
data_8ass_add_with_TBbest_ty <- data_8ass_add %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TBbest_ty=tsvalue) 

data_8ass_add_with_TBbest_ty_minus1 <- data_8ass_add_with_TBbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TBbest_ty_minus1=tsvalue) 

# add TB at ty and ty-1
data_8ass_add_with_TB_ty <- data_8ass_add_with_TBbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TB_ty=tsvalue) 

data_8ass_add_with_TB_ty_minus1 <- data_8ass_add_with_TB_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TB_ty_minus1=tsvalue) 

# add TN at ty and ty-1
data_8ass_add_with_TN_ty <- data_8ass_add_with_TB_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(TN_ty=tsvalue) 

data_8ass_add_with_TN_ty_minus1 <- data_8ass_add_with_TN_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(TN_ty_minus1=tsvalue) 

# add SSB at ty and ty-1
data_8ass_add_with_SSB_ty <- data_8ass_add_with_TN_ty_minus1 %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_ty=tsvalue) 

data_8ass_add_with_SSB_ty_minus1 <- data_8ass_add_with_SSB_ty %>% 
  left_join(select(b4, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_ty_minus1=tsvalue) 

# add SSB_E00eggs at ty and ty-1
data_8ass_add_with_SSB_E00eggs_ty <- data_8ass_add_with_SSB_ty_minus1 %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(SSB_E00eggs_ty=tsvalue) 

data_8ass_add_with_SSB_E00eggs_ty_minus1 <- data_8ass_add_with_SSB_E00eggs_ty %>% 
  left_join(select(b5, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(SSB_E00eggs_ty_minus1=tsvalue)

data_2ass_add_with_SSB_E00eggs_ty_minus1$B_ty_minus1 <- data_2ass_add_with_SSB_E00eggs_ty_minus1$B_ty <- NA
table(data_2ass_add_with_SSB_E00eggs_ty_minus1$B_type)
data_2ass_add_with_SSB_E00eggs_ty_minus1[data_2ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty <- data_2ass_add_with_SSB_E00eggs_ty_minus1[data_2ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty
data_2ass_add_with_SSB_E00eggs_ty_minus1[data_2ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty_minus1 <- data_2ass_add_with_SSB_E00eggs_ty_minus1[data_2ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty_minus1
data_2ass_add_with_SSB_E00eggs_ty_minus1[data_2ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty <- data_2ass_add_with_SSB_E00eggs_ty_minus1[data_2ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty
data_2ass_add_with_SSB_E00eggs_ty_minus1[data_2ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty_minus1 <- data_2ass_add_with_SSB_E00eggs_ty_minus1[data_2ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty_minus1

data_3ass_add_with_SSB_E00eggs_ty_minus1$B_ty_minus1 <- data_3ass_add_with_SSB_E00eggs_ty_minus1$B_ty <- NA
table(data_3ass_add_with_SSB_E00eggs_ty_minus1$B_type)
data_3ass_add_with_SSB_E00eggs_ty_minus1[data_3ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty <- data_3ass_add_with_SSB_E00eggs_ty_minus1[data_3ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty
data_3ass_add_with_SSB_E00eggs_ty_minus1[data_3ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty_minus1 <- data_3ass_add_with_SSB_E00eggs_ty_minus1[data_3ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty_minus1
data_3ass_add_with_SSB_E00eggs_ty_minus1[data_3ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty <- data_3ass_add_with_SSB_E00eggs_ty_minus1[data_3ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty
data_3ass_add_with_SSB_E00eggs_ty_minus1[data_3ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty_minus1 <- data_3ass_add_with_SSB_E00eggs_ty_minus1[data_3ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty_minus1

data_4ass_add_with_SSB_E00eggs_ty_minus1$B_ty_minus1 <- data_4ass_add_with_SSB_E00eggs_ty_minus1$B_ty <- NA
table(data_4ass_add_with_SSB_E00eggs_ty_minus1$B_type)
data_4ass_add_with_SSB_E00eggs_ty_minus1[data_4ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty <- data_4ass_add_with_SSB_E00eggs_ty_minus1[data_4ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty
data_4ass_add_with_SSB_E00eggs_ty_minus1[data_4ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty_minus1 <- data_4ass_add_with_SSB_E00eggs_ty_minus1[data_4ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty_minus1
data_4ass_add_with_SSB_E00eggs_ty_minus1[data_4ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty <- data_4ass_add_with_SSB_E00eggs_ty_minus1[data_4ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty
data_4ass_add_with_SSB_E00eggs_ty_minus1[data_4ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty_minus1 <- data_4ass_add_with_SSB_E00eggs_ty_minus1[data_4ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty_minus1

data_5ass_add_with_SSB_E00eggs_ty_minus1$B_ty_minus1 <- data_5ass_add_with_SSB_E00eggs_ty_minus1$B_ty <- NA
table(data_5ass_add_with_SSB_E00eggs_ty_minus1$B_type)
data_5ass_add_with_SSB_E00eggs_ty_minus1[data_5ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty <- data_5ass_add_with_SSB_E00eggs_ty_minus1[data_5ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty
data_5ass_add_with_SSB_E00eggs_ty_minus1[data_5ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty_minus1 <- data_5ass_add_with_SSB_E00eggs_ty_minus1[data_5ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty_minus1
data_5ass_add_with_SSB_E00eggs_ty_minus1[data_5ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty <- data_5ass_add_with_SSB_E00eggs_ty_minus1[data_5ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty
data_5ass_add_with_SSB_E00eggs_ty_minus1[data_5ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty_minus1 <- data_5ass_add_with_SSB_E00eggs_ty_minus1[data_5ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty_minus1

data_6ass_add_with_SSB_E00eggs_ty_minus1$B_ty_minus1 <- data_6ass_add_with_SSB_E00eggs_ty_minus1$B_ty <- NA
table(data_6ass_add_with_SSB_E00eggs_ty_minus1$B_type)
data_6ass_add_with_SSB_E00eggs_ty_minus1[data_6ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty <- data_6ass_add_with_SSB_E00eggs_ty_minus1[data_6ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty
data_6ass_add_with_SSB_E00eggs_ty_minus1[data_6ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty_minus1 <- data_6ass_add_with_SSB_E00eggs_ty_minus1[data_6ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty_minus1
data_6ass_add_with_SSB_E00eggs_ty_minus1[data_6ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty <- data_6ass_add_with_SSB_E00eggs_ty_minus1[data_6ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty
data_6ass_add_with_SSB_E00eggs_ty_minus1[data_6ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty_minus1 <- data_6ass_add_with_SSB_E00eggs_ty_minus1[data_6ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty_minus1

data_7ass_add_with_SSB_E00eggs_ty_minus1$B_ty_minus1 <- data_7ass_add_with_SSB_E00eggs_ty_minus1$B_ty <- NA
table(data_7ass_add_with_SSB_E00eggs_ty_minus1$B_type)
data_7ass_add_with_SSB_E00eggs_ty_minus1[data_7ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty <- data_7ass_add_with_SSB_E00eggs_ty_minus1[data_7ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty
data_7ass_add_with_SSB_E00eggs_ty_minus1[data_7ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty_minus1 <- data_7ass_add_with_SSB_E00eggs_ty_minus1[data_7ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty_minus1
data_7ass_add_with_SSB_E00eggs_ty_minus1[data_7ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty <- data_7ass_add_with_SSB_E00eggs_ty_minus1[data_7ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty
data_7ass_add_with_SSB_E00eggs_ty_minus1[data_7ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty_minus1 <- data_7ass_add_with_SSB_E00eggs_ty_minus1[data_7ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty_minus1

data_8ass_add_with_SSB_E00eggs_ty_minus1$B_ty_minus1 <- data_8ass_add_with_SSB_E00eggs_ty_minus1$B_ty <- NA
table(data_8ass_add_with_SSB_E00eggs_ty_minus1$B_type)
data_8ass_add_with_SSB_E00eggs_ty_minus1[data_8ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty <- data_8ass_add_with_SSB_E00eggs_ty_minus1[data_8ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty
data_8ass_add_with_SSB_E00eggs_ty_minus1[data_8ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$B_ty_minus1 <- data_8ass_add_with_SSB_E00eggs_ty_minus1[data_8ass_add_with_SSB_E00eggs_ty_minus1$B_type == "SSB-MT",]$SSB_ty_minus1
data_8ass_add_with_SSB_E00eggs_ty_minus1[data_8ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty <- data_8ass_add_with_SSB_E00eggs_ty_minus1[data_8ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty
data_8ass_add_with_SSB_E00eggs_ty_minus1[data_8ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$B_ty_minus1 <- data_8ass_add_with_SSB_E00eggs_ty_minus1[data_8ass_add_with_SSB_E00eggs_ty_minus1$B_type == "TBbest-MT",]$TBbest_ty_minus1


data_2ass_add_with_bs <- data_2ass_add_with_SSB_E00eggs_ty_minus1[,c(1:45,80:81,46:47,70:79,48:49,50:69)]
data_3ass_add_with_bs <- data_3ass_add_with_SSB_E00eggs_ty_minus1[,c(1:45,80:81,46:47,70:79,48:49,50:69)]
data_4ass_add_with_bs <- data_4ass_add_with_SSB_E00eggs_ty_minus1[,c(1:45,80:81,46:47,70:79,48:49,50:69)]
data_5ass_add_with_bs <- data_5ass_add_with_SSB_E00eggs_ty_minus1[,c(1:45,80:81,46:47,70:79,48:49,50:69)]
data_6ass_add_with_bs <- data_6ass_add_with_SSB_E00eggs_ty_minus1[,c(1:45,80:81,46:47,70:79,48:49,50:69)]
data_7ass_add_with_bs <- data_7ass_add_with_SSB_E00eggs_ty_minus1[,c(1:45,80:81,46:47,70:79,48:49,50:69)]
data_8ass_add_with_bs <- data_8ass_add_with_SSB_E00eggs_ty_minus1[,c(1:45,80:81,46:47,70:79,48:49,50:69)]

########################################################################################################################################

## Read data
bioparams_vals <- bioparams_values_views  # 639 stocks with 639 rows
bioparams_units <- bioparams_units_views
bioparams_assess <- bioparams_assessments_views

ts <- timeseries  # 1048437 rows; 1373 stocks, 2215 assessments
assessid <- c(data_2ass_add$assessid, data_3ass_add$assessid, data_4ass_add$assessid, data_5ass_add$assessid, 
              data_6ass_add$assessid, data_7ass_add$assessid, data_8ass_add$assessid)
ts <- ts[ts$assessid %in% assessid,]   # 76 stocks, 359 assessments
metric <- tsmetrics
table(metric$tscategory)
values <- ts %>% 
  left_join(metric, by=c("tsid"="tsunique"))
values <- values[values$tscategory=="FISHING MORTALITY",] 

# Pick assessments only with ratios
values_abs <- values[values$tsid=="ERbest-ratio" | values$tsid=="F-1/yr" | values$tsid=="ER-ratio" | values$tsid=="ER-calc-ratio" | values$tsid=="F-relative",]
values_ratio <- values[values$tsid=="FdivFmsy-dimensionless",]
assessid_abs <- unique(values_abs$assessid)
assessid_ratio <- unique(values_ratio$assessid)
assessids <- assessid_ratio[!assessid_ratio %in% assessid_abs]  # 0
assessids_with_Fmsy <- na.omit(bioparams_assess$Fmsy)
assessids_add <- assessids[assessids %in% assessids_with_Fmsy]  # 0

vv <- values[values$assessid %in% assessids_add & values$tsid=="FdivFmsy-dimensionless",]
values_add <- vv
values_add$tsid <- "F-1/yr"; values_add$tsunitsshort <- "1/yr"
for (i in 1:length(unique(values_add$assessid))){
  Fmsy <- ifelse(length(bioparams_vals[bioparams_vals$stockid==vv[vv$assessid==assessids_add[i],]$stockid[1],]$Fmsy)!=0, 
                 bioparams_vals[bioparams_vals$stockid==vv[vv$assessid==assessids_add[i],]$stockid[1],]$Fmsy, NA)
  values_add[values_add$assessid==assessids_add[i],]$tsvalue <- vv[vv$assessid==assessids_add[i],]$tsvalue*Fmsy
}

# Combine values_add with original values
values <- rbind(values, values_add)
values <- values[!is.na(values$tsvalue),]
values$key <- paste(values$assessid, values$tsyear, sep="_")
table(values$tsid)

########################################################################################################################################

# Add Fishing mortality
values_filtered <- values[values$key %in% key_id & !is.na(values$tsvalue),]
ids <- unique(values_filtered$tsid)

values_F <- values_filtered[values_filtered$tsid=="F-1/yr" | values_filtered$tsid=="ERbest-ratio" | values_filtered$tsid=="ER-ratio",]

b1 <- values_F[values_F$tsid=="ERbest-ratio",]  # 72 stocks, 297 assesses
b2 <- values_F[values_F$tsid=="ER-ratio",]  # 6 stocks, 10 assesses
b3 <- values_F[values_F$tsid=="F-1/yr",]  # 74 stocks, 348 assesses


## data_2ass ##
# add ERbest at ty and ty-1
data_2ass_add_with_ERbest_ty <- data_2ass_add_with_bs %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ERbest_ty=tsvalue) 

data_2ass_add_with_ERbest_ty_minus1 <- data_2ass_add_with_ERbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ERbest_ty_minus1=tsvalue) 

# add ER at ty and ty-1
data_2ass_add_with_ER_ty <- data_2ass_add_with_ERbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ER_ty=tsvalue) 

data_2ass_add_with_ER_ty_minus1 <- data_2ass_add_with_ER_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ER_ty_minus1=tsvalue) 

# add instF at ty and ty-1
data_2ass_add_with_instF_ty <- data_2ass_add_with_ER_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(instF_ty=tsvalue) 

data_2ass_add_with_instF_ty_minus1 <- data_2ass_add_with_instF_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(instF_ty_minus1=tsvalue) 


## data_3ass ##
# add ERbest at ty and ty-1
data_3ass_add_with_ERbest_ty <- data_3ass_add_with_bs %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ERbest_ty=tsvalue) 

data_3ass_add_with_ERbest_ty_minus1 <- data_3ass_add_with_ERbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ERbest_ty_minus1=tsvalue) 

# add ER at ty and ty-1
data_3ass_add_with_ER_ty <- data_3ass_add_with_ERbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ER_ty=tsvalue) 

data_3ass_add_with_ER_ty_minus1 <- data_3ass_add_with_ER_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ER_ty_minus1=tsvalue) 

# add instF at ty and ty-1
data_3ass_add_with_instF_ty <- data_3ass_add_with_ER_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(instF_ty=tsvalue) 

data_3ass_add_with_instF_ty_minus1 <- data_3ass_add_with_instF_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(instF_ty_minus1=tsvalue) 


## data_4ass ##
# add ERbest at ty and ty-1
data_4ass_add_with_ERbest_ty <- data_4ass_add_with_bs %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ERbest_ty=tsvalue) 

data_4ass_add_with_ERbest_ty_minus1 <- data_4ass_add_with_ERbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ERbest_ty_minus1=tsvalue) 

# add ER at ty and ty-1
data_4ass_add_with_ER_ty <- data_4ass_add_with_ERbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ER_ty=tsvalue) 

data_4ass_add_with_ER_ty_minus1 <- data_4ass_add_with_ER_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ER_ty_minus1=tsvalue) 

# add instF at ty and ty-1
data_4ass_add_with_instF_ty <- data_4ass_add_with_ER_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(instF_ty=tsvalue) 

data_4ass_add_with_instF_ty_minus1 <- data_4ass_add_with_instF_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(instF_ty_minus1=tsvalue) 

## data_5ass ##
# add ERbest at ty and ty-1
data_5ass_add_with_ERbest_ty <- data_5ass_add_with_bs %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ERbest_ty=tsvalue) 

data_5ass_add_with_ERbest_ty_minus1 <- data_5ass_add_with_ERbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ERbest_ty_minus1=tsvalue) 

# add ER at ty and ty-1
data_5ass_add_with_ER_ty <- data_5ass_add_with_ERbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ER_ty=tsvalue) 

data_5ass_add_with_ER_ty_minus1 <- data_5ass_add_with_ER_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ER_ty_minus1=tsvalue) 

# add instF at ty and ty-1
data_5ass_add_with_instF_ty <- data_5ass_add_with_ER_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(instF_ty=tsvalue) 

data_5ass_add_with_instF_ty_minus1 <- data_5ass_add_with_instF_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(instF_ty_minus1=tsvalue) 

## data_6ass ##
# add ERbest at ty and ty-1
data_6ass_add_with_ERbest_ty <- data_6ass_add_with_bs %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ERbest_ty=tsvalue) 

data_6ass_add_with_ERbest_ty_minus1 <- data_6ass_add_with_ERbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ERbest_ty_minus1=tsvalue) 

# add ER at ty and ty-1
data_6ass_add_with_ER_ty <- data_6ass_add_with_ERbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ER_ty=tsvalue) 

data_6ass_add_with_ER_ty_minus1 <- data_6ass_add_with_ER_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ER_ty_minus1=tsvalue) 

# add instF at ty and ty-1
data_6ass_add_with_instF_ty <- data_6ass_add_with_ER_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(instF_ty=tsvalue) 

data_6ass_add_with_instF_ty_minus1 <- data_6ass_add_with_instF_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(instF_ty_minus1=tsvalue) 


## data_7ass ##
# add ERbest at ty and ty-1
data_7ass_add_with_ERbest_ty <- data_7ass_add_with_bs %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ERbest_ty=tsvalue) 

data_7ass_add_with_ERbest_ty_minus1 <- data_7ass_add_with_ERbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ERbest_ty_minus1=tsvalue) 

# add ER at ty and ty-1
data_7ass_add_with_ER_ty <- data_7ass_add_with_ERbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ER_ty=tsvalue) 

data_7ass_add_with_ER_ty_minus1 <- data_7ass_add_with_ER_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ER_ty_minus1=tsvalue) 

# add instF at ty and ty-1
data_7ass_add_with_instF_ty <- data_7ass_add_with_ER_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(instF_ty=tsvalue) 

data_7ass_add_with_instF_ty_minus1 <- data_7ass_add_with_instF_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(instF_ty_minus1=tsvalue) 


## data_8ass ##
# add ERbest at ty and ty-1
data_8ass_add_with_ERbest_ty <- data_8ass_add_with_bs %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ERbest_ty=tsvalue) 

data_8ass_add_with_ERbest_ty_minus1 <- data_8ass_add_with_ERbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ERbest_ty_minus1=tsvalue) 

# add ER at ty and ty-1
data_8ass_add_with_ER_ty <- data_8ass_add_with_ERbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ER_ty=tsvalue) 

data_8ass_add_with_ER_ty_minus1 <- data_8ass_add_with_ER_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ER_ty_minus1=tsvalue) 

# add instF at ty and ty-1
data_8ass_add_with_instF_ty <- data_8ass_add_with_ER_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(instF_ty=tsvalue) 

data_8ass_add_with_instF_ty_minus1 <- data_8ass_add_with_instF_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(instF_ty_minus1=tsvalue) 


data_2ass_add_with_instF_ty_minus1$F_ty_minus1 <- data_2ass_add_with_instF_ty_minus1$F_ty <- NA
table(data_2ass_add_with_instF_ty_minus1$F_type)
data_2ass_add_with_instF_ty_minus1[data_2ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty <- data_2ass_add_with_instF_ty_minus1[data_2ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty
data_2ass_add_with_instF_ty_minus1[data_2ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty_minus1 <- data_2ass_add_with_instF_ty_minus1[data_2ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty_minus1
data_2ass_add_with_instF_ty_minus1[data_2ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty <- data_2ass_add_with_instF_ty_minus1[data_2ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty
data_2ass_add_with_instF_ty_minus1[data_2ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty_minus1 <- data_2ass_add_with_instF_ty_minus1[data_2ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty_minus1

data_3ass_add_with_instF_ty_minus1$F_ty_minus1 <- data_3ass_add_with_instF_ty_minus1$F_ty <- NA
table(data_3ass_add_with_instF_ty_minus1$F_type)
data_3ass_add_with_instF_ty_minus1[data_3ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty <- data_3ass_add_with_instF_ty_minus1[data_3ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty
data_3ass_add_with_instF_ty_minus1[data_3ass_add_with_instF_ty_minus1$F_type == "ERbest-ratIo",]$F_ty_minus1 <- data_3ass_add_with_instF_ty_minus1[data_3ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty_minus1
data_3ass_add_with_instF_ty_minus1[data_3ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty <- data_3ass_add_with_instF_ty_minus1[data_3ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty
data_3ass_add_with_instF_ty_minus1[data_3ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty_minus1 <- data_3ass_add_with_instF_ty_minus1[data_3ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty_minus1

data_4ass_add_with_instF_ty_minus1$F_ty_minus1 <- data_4ass_add_with_instF_ty_minus1$F_ty <- NA
table(data_4ass_add_with_instF_ty_minus1$F_type)
data_4ass_add_with_instF_ty_minus1[data_4ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty <- data_4ass_add_with_instF_ty_minus1[data_4ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty
data_4ass_add_with_instF_ty_minus1[data_4ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty_minus1 <- data_4ass_add_with_instF_ty_minus1[data_4ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty_minus1
data_4ass_add_with_instF_ty_minus1[data_4ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty <- data_4ass_add_with_instF_ty_minus1[data_4ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty
data_4ass_add_with_instF_ty_minus1[data_4ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty_minus1 <- data_4ass_add_with_instF_ty_minus1[data_4ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty_minus1

data_5ass_add_with_instF_ty_minus1$F_ty_minus1 <- data_5ass_add_with_instF_ty_minus1$F_ty <- NA
table(data_5ass_add_with_instF_ty_minus1$F_type)
data_5ass_add_with_instF_ty_minus1[data_5ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty <- data_5ass_add_with_instF_ty_minus1[data_5ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty
data_5ass_add_with_instF_ty_minus1[data_5ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty_minus1 <- data_5ass_add_with_instF_ty_minus1[data_5ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty_minus1
data_5ass_add_with_instF_ty_minus1[data_5ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty <- data_5ass_add_with_instF_ty_minus1[data_5ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty
data_5ass_add_with_instF_ty_minus1[data_5ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty_minus1 <- data_5ass_add_with_instF_ty_minus1[data_5ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty_minus1

data_6ass_add_with_instF_ty_minus1$F_ty_minus1 <- data_6ass_add_with_instF_ty_minus1$F_ty <- NA
table(data_6ass_add_with_instF_ty_minus1$F_type)
data_6ass_add_with_instF_ty_minus1[data_6ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty <- data_6ass_add_with_instF_ty_minus1[data_6ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty
data_6ass_add_with_instF_ty_minus1[data_6ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty_minus1 <- data_6ass_add_with_instF_ty_minus1[data_6ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty_minus1
data_6ass_add_with_instF_ty_minus1[data_6ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty <- data_6ass_add_with_instF_ty_minus1[data_6ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty
data_6ass_add_with_instF_ty_minus1[data_6ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty_minus1 <- data_6ass_add_with_instF_ty_minus1[data_6ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty_minus1

data_7ass_add_with_instF_ty_minus1$F_ty_minus1 <- data_7ass_add_with_instF_ty_minus1$F_ty <- NA
table(data_7ass_add_with_instF_ty_minus1$F_type)
data_7ass_add_with_instF_ty_minus1[data_7ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty <- data_7ass_add_with_instF_ty_minus1[data_7ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty
data_7ass_add_with_instF_ty_minus1[data_7ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty_minus1 <- data_7ass_add_with_instF_ty_minus1[data_7ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty_minus1
data_7ass_add_with_instF_ty_minus1[data_7ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty <- data_7ass_add_with_instF_ty_minus1[data_7ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty
data_7ass_add_with_instF_ty_minus1[data_7ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty_minus1 <- data_7ass_add_with_instF_ty_minus1[data_7ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty_minus1

data_8ass_add_with_instF_ty_minus1$F_ty_minus1 <- data_8ass_add_with_instF_ty_minus1$F_ty <- NA
table(data_8ass_add_with_instF_ty_minus1$F_type)
data_8ass_add_with_instF_ty_minus1[data_8ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty <- data_8ass_add_with_instF_ty_minus1[data_8ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty
data_8ass_add_with_instF_ty_minus1[data_8ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$F_ty_minus1 <- data_8ass_add_with_instF_ty_minus1[data_8ass_add_with_instF_ty_minus1$F_type == "ERbest-ratio",]$ERbest_ty_minus1
data_8ass_add_with_instF_ty_minus1[data_8ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty <- data_8ass_add_with_instF_ty_minus1[data_8ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty
data_8ass_add_with_instF_ty_minus1[data_8ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$F_ty_minus1 <- data_8ass_add_with_instF_ty_minus1[data_8ass_add_with_instF_ty_minus1$F_type == "F-1/yr",]$instF_ty_minus1


data_2ass_add_with_fr <- data_2ass_add_with_instF_ty_minus1[,c(1:59,88:89,60:61,82:87,62:81)]
data_3ass_add_with_fr <- data_3ass_add_with_instF_ty_minus1[,c(1:59,88:89,60:61,82:87,62:81)]
data_4ass_add_with_fr <- data_4ass_add_with_instF_ty_minus1[,c(1:59,88:89,60:61,82:87,62:81)]
data_5ass_add_with_fr <- data_5ass_add_with_instF_ty_minus1[,c(1:59,88:89,60:61,82:87,62:81)]
data_6ass_add_with_fr <- data_6ass_add_with_instF_ty_minus1[,c(1:59,88:89,60:61,82:87,62:81)]
data_7ass_add_with_fr <- data_7ass_add_with_instF_ty_minus1[,c(1:59,88:89,60:61,82:87,62:81)]
data_8ass_add_with_fr <- data_8ass_add_with_instF_ty_minus1[,c(1:59,88:89,60:61,82:87,62:81)]

##################################################################################################

data_2 <- rbind(data_2ass_keep, data_2ass_add_with_fr)
data_3 <- rbind(data_3ass_keep, data_3ass_add_with_fr)
data_4 <- rbind(data_4ass_keep, data_4ass_add_with_fr)
data_5 <- rbind(data_5ass_keep, data_5ass_add_with_fr)
data_6 <- rbind(data_6ass_keep, data_6ass_add_with_fr)
data_7 <- rbind(data_7ass_keep, data_7ass_add_with_fr)
data_8 <- rbind(data_8ass_keep, data_8ass_add_with_fr)

## Redefine assess_order ##
# data_2
data_2$ty_order <- 1
data_2 <- data_2[,c(1:86,90,87,88)]
table(data_2$assess_order)
ids <- unique(data_2[data_2$assess_order > 2,]$stockid)
data_2_keep <- data_2[!data_2$stockid %in% ids,]
data_2_add <- data_2[data_2$stockid %in% ids,]
data_2_add$assess_order <- 0  # the most recent = 1, earlier = 2,3......

data_order <- data_2_add[1,]
for (i in 1:length(ids)){
  data_stock <- data_2_add[data_2_add$stockid==ids[i],]
  data_stock$assess_order <- rank(-data_stock$terminal_year)
  data_order <- rbind(data_order, data_stock)
}
data_order <- data_order[-1,]
rownames(data_order) <- NULL

data_2_new <- rbind(data_2_keep, data_order)
data_2_new[data_2_new$assess_order==1.5,]$assess_order <- c(2,1)


# data_3
dd_order_3 <- unique(data_3[,c(3,2,40,41)])

data_order <- dd_order_3[1,]
for (i in 1:length(unique(dd_order_3$stockid))){
  data_stock <- dd_order_3[dd_order_3$stockid==unique(dd_order_3$stockid)[i],]
  data_stock$assess_order <- rank(-data_stock$terminal_year)
  data_order <- rbind(data_order, data_stock)
}
data_order <- data_order[-1,]
rownames(data_order) <- NULL

data_order[data_order$assess_order==2.5,]   # ATBTUNAWATL
data_order[data_order$stockid=="ATBTUNAWATL",]$assess_order <- c(2,3,1)

data_3_new <- data_3[,-c(41)] %>%
  left_join(select(data_order, assessid, assess_order), by=c("assessid"))
  
data_3_new <- data_3_new[,c(1:40, 89, 41:88)]


# data_4
dd_order_4 <- unique(data_4[,c(3,2,40,41)])

data_order <- dd_order_4[1,]
for (i in 1:length(unique(dd_order_4$stockid))){
  data_stock <- dd_order_4[dd_order_4$stockid==unique(dd_order_4$stockid)[i],]
  data_stock$assess_order <- rank(-data_stock$terminal_year)
  data_order <- rbind(data_order, data_stock)
}
data_order <- data_order[-1,]
rownames(data_order) <- NULL

data_order[data_order$assess_order==2.5,]   # ANCHOBAYB
data_order[data_order$stockid=="ANCHOBAYB",]$assess_order <- c(3,4,2,1)

data_4_new <- data_4[,-c(41)] %>%
  left_join(select(data_order, assessid, assess_order), by=c("assessid"))

data_4_new <- data_4_new[,c(1:40, 89, 41:88)]


# data_5
dd_order_5 <- unique(data_5[,c(3,2,40,41)])

data_order <- dd_order_5[1,]
for (i in 1:length(unique(dd_order_5$stockid))){
  data_stock <- dd_order_5[dd_order_5$stockid==unique(dd_order_5$stockid)[i],]
  data_stock$assess_order <- rank(-data_stock$terminal_year)
  data_order <- rbind(data_order, data_stock)
}
data_order <- data_order[-1,]
rownames(data_order) <- NULL

data_5_new <- data_5[,-c(41)] %>%
  left_join(select(data_order, assessid, assess_order), by=c("assessid"))

data_5_new <- data_5_new[,c(1:40, 89, 41:88)]


# data_6
dd_order_6 <- unique(data_6[,c(3,2,40,41)])

data_order <- dd_order_6[1,]
for (i in 1:length(unique(dd_order_6$stockid))){
  data_stock <- dd_order_6[dd_order_6$stockid==unique(dd_order_6$stockid)[i],]
  data_stock$assess_order <- rank(-data_stock$terminal_year)
  data_order <- rbind(data_order, data_stock)
}
data_order <- data_order[-1,]
rownames(data_order) <- NULL

data_order[data_order$assess_order == 4.5,]$assess_order <- c(5, 4)

data_6_new <- data_6[,-c(41)] %>%
  left_join(select(data_order, assessid, assess_order), by=c("assessid"))

data_6_new <- data_6_new[,c(1:40, 89, 41:88)]


# data_7
dd_order_7 <- unique(data_7[,c(3,2,40,41)])

data_order <- dd_order_7[1,]
for (i in 1:length(unique(dd_order_7$stockid))){
  data_stock <- dd_order_7[dd_order_7$stockid==unique(dd_order_7$stockid)[i],]
  data_stock$assess_order <- rank(-data_stock$terminal_year)
  data_order <- rbind(data_order, data_stock)
}
data_order <- data_order[-1,]
rownames(data_order) <- NULL

data_7_new <- data_7[,-c(41)] %>%
  left_join(select(data_order, assessid, assess_order), by=c("assessid"))

data_7_new <- data_7_new[,c(1:40, 89, 41:88)]


# data_8
dd_order_8 <- unique(data_8[,c(3,2,40,41)])

data_order <- dd_order_8[1,]
for (i in 1:length(unique(dd_order_8$stockid))){
  data_stock <- dd_order_8[dd_order_8$stockid==unique(dd_order_8$stockid)[i],]
  data_stock$assess_order <- rank(-data_stock$terminal_year)
  data_order <- rbind(data_order, data_stock)
}
data_order <- data_order[-1,]
rownames(data_order) <- NULL

data_order[data_order$assess_order == 3.5,]$assess_order <- c(4, 3, 4, 3, 4, 3)

data_8_new <- data_8[,-c(41)] %>%
  left_join(select(data_order, assessid, assess_order), by=c("assessid"))

data_8_new <- data_8_new[,c(1:40, 89, 41:88)]

##################################################################################################

write.csv(data_2_new, paste(outputdir, "0f_1_data_2_assess_update_ty.csv", sep="/"), row.names=F)
write.csv(data_3_new, paste(outputdir, "0f_2_data_3_assess_update_ty.csv", sep="/"), row.names=F)
write.csv(data_4_new, paste(outputdir, "0f_3_data_4_assess_update_ty.csv", sep="/"), row.names=F)
write.csv(data_5_new, paste(outputdir, "0f_4_data_5_assess_update_ty.csv", sep="/"), row.names=F)
write.csv(data_6_new, paste(outputdir, "0f_5_data_6_assess_update_ty.csv", sep="/"), row.names=F)
write.csv(data_7_new, paste(outputdir, "0f_6_data_7_assess_update_ty.csv", sep="/"), row.names=F)
write.csv(data_8_new, paste(outputdir, "0f_7_data_8_assess_update_ty.csv", sep="/"), row.names=F)

