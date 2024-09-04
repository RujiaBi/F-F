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

data <- read.csv(paste(outputdir, "0b_full_data_with_biomass.csv", sep="/"), as.is=T)

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
values <- values[values$tscategory=="FISHING MORTALITY",] 

# Pick assessments only with ratios
values_abs <- values[values$tsid=="ERbest-ratio" | values$tsid=="F-1/yr" | values$tsid=="ER-ratio" | values$tsid=="ER-calc-ratio" | values$tsid=="F-relative",]
values_ratio <- values[values$tsid=="FdivFmsy-dimensionless",]
assessid_abs <- unique(values_abs$assessid)
assessid_ratio <- unique(values_ratio$assessid)
assessids <- assessid_ratio[!assessid_ratio %in% assessid_abs]  # 45 assessments only with FdivFmsy-dimensionless
assessids_with_Fmsy <- na.omit(bioparams_assess$Fmsy)
assessids_add <- assessids[assessids %in% assessids_with_Fmsy]  # find 1 assessment with Fmsy among the 45 assessments only with BdivBmsypref-dimensionless

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
key_id <- c(data$key_ty, data$key_ty_minus1)

values_filtered <- values[values$key %in% key_id & !is.na(values$tsvalue),]
ids <- unique(values_filtered$tsid)

values_F <- values_filtered[values_filtered$tsid=="F-1/yr" | values_filtered$tsid=="ERbest-ratio" | values_filtered$tsid=="ER-ratio",]

b1 <- values_F[values_F$tsid=="ERbest-ratio",]  # 316 stocks, 764 assesses
b2 <- values_F[values_F$tsid=="ER-ratio",]  # 235 stocks, 300 assesses
b3 <- values_F[values_F$tsid=="F-1/yr",]  # 285 stocks, 738 assesses

ddb <- as.data.frame(matrix(NA, nrow=length(unique(values_F$stockid)), ncol=5))
colnames(ddb) <- c("stockid", "number_of_assess_ERbest", "number_of_assess_ER", "number_of_assess_F", "F_id")
ddb$stockid <- unique(values_F$stockid)
for (i in 1:length(unique(values_F$stockid))){
  ddb[i,2] <- length(unique(b1[b1$stockid==ddb[i,1],]$assessid))
  ddb[i,3] <- length(unique(b2[b2$stockid==ddb[i,1],]$assessid))
  ddb[i,4] <- length(unique(b3[b3$stockid==ddb[i,1],]$assessid))
  vec <- c(ddb[i,2], ddb[i,3], ddb[i,4])
  ddb[i,5] <- which.max(vec)
}

stock_b1 <- ddb[ddb$F_id==1,]$stockid
stock_b2 <- ddb[ddb$F_id==2,]$stockid
stock_b3 <- ddb[ddb$F_id==3,]$stockid

b1 <- b1[b1$stockid %in% stock_b1,]   # 249 stocks, 612 assesses
b2 <- b2[b2$stockid %in% stock_b2,]   # 0
b3 <- b3[b3$stockid %in% stock_b3,]   # 103 stocks, 316 assesses

values_f_together <- rbind(b1, b2, b3)

  
# Add F at ty
data_with_f_ty <- data %>% 
  left_join(select(values_f_together, tsvalue, tsunitsshort, tsid, key), by=c("key_ty"="key")) %>% 
  rename(F_ty=tsvalue, F_unit=tsunitsshort, F_type=tsid) 

# Add F at ty-1
data_with_f_ty_minus1 <- data_with_f_ty %>% 
  left_join(select(values_f_together, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(F_ty_minus1=tsvalue) 

data_with_fr <- data_with_f_ty_minus1[,c(1:60,63,62,61)]
dim(data_with_fr)  # 1281
summary(data_with_fr$F_ty)  # 354 NAs in F_ty

########################################################################################################################################

## Add ERbest, ER, instF to data
b1 <- values_F[values_F$tsid=="ERbest-ratio",]  # 316 stocks, 764 assesses
b2 <- values_F[values_F$tsid=="ER-ratio",]  # 235 stocks, 300 assesses
b3 <- values_F[values_F$tsid=="F-1/yr",]  # 285 stocks, 738 assesses

# add ERbest at ty and ty-1
data_with_fr_ERbest_ty <- data_with_fr %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ERbest_ty=tsvalue) 

data_with_fr_ERbest_ty_minus1 <- data_with_fr_ERbest_ty %>% 
  left_join(select(b1, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ERbest_ty_minus1=tsvalue) 

# add ER at ty and ty-1
data_with_fr_ER_ty <- data_with_fr_ERbest_ty_minus1 %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(ER_ty=tsvalue) 

data_with_fr_ER_ty_minus1 <- data_with_fr_ER_ty %>% 
  left_join(select(b2, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(ER_ty_minus1=tsvalue) 

# add instF at ty and ty-1
data_with_fr_instF_ty <- data_with_fr_ER_ty_minus1 %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty"="key")) %>% 
  rename(instF_ty=tsvalue) 

data_with_fr_instF_ty_minus1 <- data_with_fr_instF_ty %>% 
  left_join(select(b3, tsvalue, key), by=c("key_ty_minus1"="key")) %>% 
  rename(instF_ty_minus1=tsvalue) 

########################################################################################################################################

# Export full data
data_with_fr <- data_with_fr_instF_ty_minus1
write.csv(data_with_fr, paste(outputdir, "0c_full_data_with_fishing_mortality.csv", sep="/"), row.names=F)

########################################################################################################################################

# Delete assessments without TBbest records
data_with_f <- data_with_fr[!is.na(data_with_fr$F_ty),]

# Check number of assessments for each stock
data_num_assess <- as.data.frame(aggregate(assessid ~ stockid, data = data_with_f, FUN = length))
table(data_num_assess$assessid)  
#  1   2   3   4   5   6   7   8 
# 67 204  19   3   9  16  30   4 
stockid_keep_2plus_assess <- unique(data_num_assess[data_num_assess$assessid > 1,]$stockid)
data_2plus_assess_with_f <- data_with_f[data_with_f$stockid %in% stockid_keep_2plus_assess,]  # 285 stocks
length(unique(data_2plus_assess_with_f[data_2plus_assess_with_f$country=="USA",]$stockid))  # 76
length(unique(data_2plus_assess_with_f[data_2plus_assess_with_f$region=="Europe (EU)",]$stockid))  # 72
length(unique(data_2plus_assess_with_f[data_2plus_assess_with_f$country=="Canada",]$stockid))  # 18
length(unique(data_2plus_assess_with_f[data_2plus_assess_with_f$country=="Japan",]$stockid))  # 14

# Check if assessments for each stock have the same F_type
ccheck <- data.frame(matrix(NA, nrow=length(unique(data_2plus_assess_with_f$stockid)), ncol=2))
colnames(ccheck) <- c("stockid", "F_type")
ccheck$stockid <- unique(data_2plus_assess_with_f$stockid)
for (i in 1:length(unique(data_2plus_assess_with_f$stockid))){
  ccheck[i,2] <- length(unique(data_2plus_assess_with_f[data_2plus_assess_with_f$stockid==ccheck[i,1],]$F_type))
}
table(ccheck$F_type)  # all=1, no problem


