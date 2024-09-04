# read interval
dd_interval <- read.csv("0h_data_full_model.csv")
dd_interval$key <- paste(dd_interval$stockid, dd_interval$comparison, sep="_")
length(unique(dd_interval$key))  # 1286

# read data for model
dat <- read.csv("new_data_council_model.csv")
dat$key <- paste(dat$stockid, dat$comparison, sep="_")
length(unique(dat$key))  # 1315

# add interval
new_dat <- merge(dat, dd_interval[,c(34,7)], by="key", all.x=TRUE)

# check interval=NA
summary(new_dat$interval)  # 29 NAs
new_dat[new_dat$key=="atmackerel_1",]$interval <- 12
new_dat[new_dat$key=="atmackerel_2",]$interval <- 15
new_dat[new_dat$key=="atmackerel_3",]$interval <- 3
new_dat[new_dat$key=="BlackSeaBass_1",]$interval <- 2
new_dat[new_dat$key=="BlackSeaBass_2",]$interval <- 6
new_dat[new_dat$key=="BlackSeaBass_3",]$interval <- 4
new_dat[new_dat$key=="Gag_1",]$interval <- 8
new_dat[new_dat$key=="Gag_2",]$interval <- 15
new_dat[new_dat$key=="Gag_3",]$interval <- 7
new_dat[new_dat$key=="GreaterAmberjack_1",]$interval <- 11
new_dat[new_dat$key=="KingMackerel_1",]$interval <- 6
new_dat[new_dat$key=="KingMackerel_2",]$interval <- 11
new_dat[new_dat$key=="KingMackerel_3",]$interval <- 5
new_dat[new_dat$key=="MuttonSnapper_1",]$interval <- 7
new_dat[new_dat$key=="RedGrouper_1",]$interval <- 7
new_dat[new_dat$key=="RedPorgy_1",]$interval <- 7
new_dat[new_dat$key=="RedPorgy_2",]$interval <- 13
new_dat[new_dat$key=="RedPorgy_3",]$interval <- 6
new_dat[new_dat$key=="RedSnapper_1",]$interval <- 5
new_dat[new_dat$key=="SABluelineTilefish_1",]$interval <- 4
new_dat[new_dat$key=="SnowyGrouper_1",]$interval <- 6
new_dat[new_dat$key=="SpanishMackerel_1",]$interval <- 4
new_dat[new_dat$key=="Tilefish_1",]$interval <- 4
new_dat[new_dat$key=="Tilefish_2",]$interval <- 8
new_dat[new_dat$key=="Tilefish_3",]$interval <- 4
new_dat[new_dat$key=="Vermilionsnapper_1",]$interval <- 4
new_dat[new_dat$key=="Vermilionsnapper_2",]$interval <- 11
new_dat[new_dat$key=="Vermilionsnapper_3",]$interval <- 7
new_dat[new_dat$key=="YellowtailSnapper_1",]$interval <- 7

summary(new_dat)
table(new_dat$interval, new_dat$period)
new_dat[new_dat$interval==6 & new_dat$period==1,]$period <- 2  # RedPorgy_3


# add mu values
new_dat$mu_OFL <- new_dat$mu_F_by_Fmsy <- new_dat$mu_B_by_Bmsy <- new_dat$mu_Fmsy <- new_dat$mu_Bmsy <- new_dat$mu_F <- new_dat$mu_B <- NA
for (i in 1:dim(new_dat)[1]){
  new_dat[i,]$mu_B <- ifelse(length(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$B_recent),]$comparison),]$B_recent)==1,
                             new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$B_recent),]$comparison),]$B_recent, NA)
  new_dat[i,]$mu_F <- ifelse(length(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$F_recent),]$comparison),]$F_recent)==1,
                             new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$F_recent),]$comparison),]$F_recent, NA)
  new_dat[i,]$mu_Bmsy <- ifelse(length(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$Bmsy_recent),]$comparison),]$Bmsy_recent)==1,
                                new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$Bmsy_recent),]$comparison),]$Bmsy_recent, NA)
  new_dat[i,]$mu_Fmsy <- ifelse(length(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$Fmsy_recent),]$comparison),]$Fmsy_recent)==1,
                                new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$Fmsy_recent),]$comparison),]$Fmsy_recent, NA)
  new_dat[i,]$mu_B_by_Bmsy <- ifelse(length(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$B_by_Bmsy_recent),]$comparison),]$B_by_Bmsy_recent)==1,
                                     new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$B_by_Bmsy_recent),]$comparison),]$B_by_Bmsy_recent, NA)
  new_dat[i,]$mu_F_by_Fmsy <- ifelse(length(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$F_by_Fmsy_recent),]$comparison),]$F_by_Fmsy_recent)==1,
                                     new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$F_by_Fmsy_recent),]$comparison),]$F_by_Fmsy_recent, NA)
  new_dat[i,]$mu_OFL <- ifelse(length(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$OFL_ty_recent),]$comparison),]$OFL_ty_recent)==1,
                               new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & new_dat$comparison==max(new_dat[new_dat$stockid==new_dat[i,2] & new_dat$ty==new_dat[i,9] & !is.na(new_dat$OFL_ty_recent),]$comparison),]$OFL_ty_recent, NA)
}

# Check NA values
dim(new_dat[is.na(new_dat$mu_B),])  # 0
dim(new_dat[is.na(new_dat$mu_F),]) # 2 rows
dim(new_dat[is.na(new_dat$mu_Bmsy),])  # 78 rows
dim(new_dat[is.na(new_dat$mu_Fmsy),])  # 86 rows
dim(new_dat[is.na(new_dat$mu_B_by_Bmsy),])  # 79 rows
dim(new_dat[is.na(new_dat$mu_F_by_Fmsy),])  # 167 rows
dim(new_dat[is.na(new_dat$mu_OFL),])  # 80 rows

dd_B <- new_dat[!is.na(new_dat$B_recent) & !is.na(new_dat$B_old),]; summary(dd_B$mu_B)  # good
dd_F <- new_dat[!is.na(new_dat$F_recent) & !is.na(new_dat$F_old),]; summary(dd_F$mu_F)  # good
dd_Bmsy <- new_dat[!is.na(new_dat$Bmsy_recent) & !is.na(new_dat$Bmsy_old),]; summary(dd_Bmsy$mu_Bmsy)  # good
dd_Fmsy <- new_dat[!is.na(new_dat$Fmsy_recent) & !is.na(new_dat$Fmsy_old),]; summary(dd_Fmsy$mu_Fmsy)  # good
dd_B_by_Bmsy <- new_dat[!is.na(new_dat$B_by_Bmsy_recent) & !is.na(new_dat$B_by_Bmsy_old),]; summary(dd_B_by_Bmsy$mu_B_by_Bmsy)  # good
dd_F_by_Fmsy <- new_dat[!is.na(new_dat$F_by_Fmsy_recent) & !is.na(new_dat$F_by_Fmsy_old),]; summary(dd_F_by_Fmsy$mu_F_by_Fmsy)  # good
dd_OFL <- new_dat[!is.na(new_dat$OFL_ty_recent) & !is.na(new_dat$OFL_ty_old),]; summary(dd_OFL$mu_OFL)  # good

summary(new_dat)

# save data for models
write.csv(new_dat, "new_data_council_model_revision.csv", row.names=F)

############################################################################

dat <- read.csv("new_data_council_model_revision.csv", as.is=T)

dd <- as.data.frame(matrix(NA, nrow=20*7, ncol=3))
colnames(dd) <- c("region", "para", "average_interval")
 
dd$region <- rep(c("GMFMC", "MAFMC", "NEFMC", "NPFMC", "PFMC", "SAFMC", "US non-federal",
                 "Europe (EU)", "Europe (non-EU)",  "CA East Coast", "CA West Coast", 
                 "Australia", "New Zealand", "Japan", "South Africa", "South America",
                 "AOHS", "IOHS", "POHS", "MBS"), time=7)

dd$para <- rep(c("B_ty", "Bmsy", "B_Bmsy", "F_ty", "Fmsy", "F_Fmsy", "OFL_ty"), each=20)

dat_B <- dat[!is.na(dat$B_recent) & !is.na(dat$B_old),]
dat_Bmsy <- dat[!is.na(dat$Bmsy_recent) & !is.na(dat$Bmsy_old),]
dat_B_Bmsy <- dat[!is.na(dat$B_by_Bmsy_recent) & !is.na(dat$B_by_Bmsy_old),]
dat_F <- dat[!is.na(dat$F_recent) & !is.na(dat$F_old),]
dat_Fmsy <- dat[!is.na(dat$Fmsy_recent) & !is.na(dat$Fmsy_old),]
dat_F_Fmsy <- dat[!is.na(dat$F_by_Fmsy_recent) & !is.na(dat$F_by_Fmsy_old),]
dat_OFL <- dat[!is.na(dat$OFL_ty_recent) & !is.na(dat$OFL_ty_old),]

# calculate average_interval
dd$average_interval <- c(ifelse(length(dat_B[dat_B$CouncilCode==1,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==1,]$interval, list(dat_B[dat_B$CouncilCode==1,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==2,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==2,]$interval, list(dat_B[dat_B$CouncilCode==2,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==3,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==3,]$interval, list(dat_B[dat_B$CouncilCode==3,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==4,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==4,]$interval, list(dat_B[dat_B$CouncilCode==4,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==5,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==5,]$interval, list(dat_B[dat_B$CouncilCode==5,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==6,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==6,]$interval, list(dat_B[dat_B$CouncilCode==6,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==7,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==7,]$interval, list(dat_B[dat_B$CouncilCode==7,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==8,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==8,]$interval, list(dat_B[dat_B$CouncilCode==8,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==9,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==9,]$interval, list(dat_B[dat_B$CouncilCode==9,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==10,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==10,]$interval, list(dat_B[dat_B$CouncilCode==10,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==11,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==11,]$interval, list(dat_B[dat_B$CouncilCode==11,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==12,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==12,]$interval, list(dat_B[dat_B$CouncilCode==12,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==13,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==13,]$interval, list(dat_B[dat_B$CouncilCode==13,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==14,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==14,]$interval, list(dat_B[dat_B$CouncilCode==14,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==15,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==15,]$interval, list(dat_B[dat_B$CouncilCode==15,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==16,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==16,]$interval, list(dat_B[dat_B$CouncilCode==16,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==17,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==17,]$interval, list(dat_B[dat_B$CouncilCode==17,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==18,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==18,]$interval, list(dat_B[dat_B$CouncilCode==18,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==19,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==19,]$interval, list(dat_B[dat_B$CouncilCode==19,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B[dat_B$CouncilCode==20,]$interval)>0, mean(aggregate(dat_B[dat_B$CouncilCode==20,]$interval, list(dat_B[dat_B$CouncilCode==20,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==1,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==1,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==1,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==2,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==2,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==2,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==3,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==3,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==3,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==4,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==4,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==4,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==5,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==5,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==5,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==6,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==6,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==6,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==7,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==7,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==7,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==8,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==8,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==8,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==9,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==9,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==9,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==10,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==10,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==10,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==11,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==11,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==11,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==12,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==12,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==12,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==13,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==13,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==13,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==14,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==14,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==14,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==15,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==15,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==15,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==16,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==16,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==16,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==17,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==17,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==17,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==18,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==18,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==18,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==19,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==19,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==19,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Bmsy[dat_Bmsy$CouncilCode==20,]$interval)>0, mean(aggregate(dat_Bmsy[dat_Bmsy$CouncilCode==20,]$interval, list(dat_Bmsy[dat_Bmsy$CouncilCode==20,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==1,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==1,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==1,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==2,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==2,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==2,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==3,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==3,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==3,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==4,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==4,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==4,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==5,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==5,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==5,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==6,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==6,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==6,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==7,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==7,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==7,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==8,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==8,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==8,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==9,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==9,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==9,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==10,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==10,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==10,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==11,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==11,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==11,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==12,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==12,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==12,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==13,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==13,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==13,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==14,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==14,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==14,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==15,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==15,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==15,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==16,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==16,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==16,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==17,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==17,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==17,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==18,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==18,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==18,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==19,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==19,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==19,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==20,]$interval)>0, mean(aggregate(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==20,]$interval, list(dat_B_Bmsy[dat_B_Bmsy$CouncilCode==20,]$stockid), FUN=mean)$x), NA),
                         
                         ifelse(length(dat_F[dat_F$CouncilCode==1,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==1,]$interval, list(dat_F[dat_F$CouncilCode==1,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==2,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==2,]$interval, list(dat_F[dat_F$CouncilCode==2,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==3,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==3,]$interval, list(dat_F[dat_F$CouncilCode==3,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==4,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==4,]$interval, list(dat_F[dat_F$CouncilCode==4,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==5,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==5,]$interval, list(dat_F[dat_F$CouncilCode==5,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==6,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==6,]$interval, list(dat_F[dat_F$CouncilCode==6,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==7,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==7,]$interval, list(dat_F[dat_F$CouncilCode==7,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==8,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==8,]$interval, list(dat_F[dat_F$CouncilCode==8,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==9,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==9,]$interval, list(dat_F[dat_F$CouncilCode==9,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==10,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==10,]$interval, list(dat_F[dat_F$CouncilCode==10,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==11,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==11,]$interval, list(dat_F[dat_F$CouncilCode==11,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==12,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==12,]$interval, list(dat_F[dat_F$CouncilCode==12,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==13,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==13,]$interval, list(dat_F[dat_F$CouncilCode==13,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==14,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==14,]$interval, list(dat_F[dat_F$CouncilCode==14,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==15,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==15,]$interval, list(dat_F[dat_F$CouncilCode==15,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==16,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==16,]$interval, list(dat_F[dat_F$CouncilCode==16,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==17,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==17,]$interval, list(dat_F[dat_F$CouncilCode==17,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==18,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==18,]$interval, list(dat_F[dat_F$CouncilCode==18,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==19,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==19,]$interval, list(dat_F[dat_F$CouncilCode==19,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F[dat_F$CouncilCode==20,]$interval)>0, mean(aggregate(dat_F[dat_F$CouncilCode==20,]$interval, list(dat_F[dat_F$CouncilCode==20,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==1,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==1,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==1,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==2,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==2,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==2,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==3,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==3,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==3,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==4,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==4,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==4,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==5,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==5,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==5,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==6,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==6,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==6,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==7,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==7,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==7,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==8,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==8,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==8,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==9,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==9,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==9,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==10,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==10,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==10,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==11,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==11,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==11,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==12,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==12,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==12,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==13,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==13,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==13,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==14,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==14,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==14,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==15,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==15,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==15,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==16,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==16,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==16,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==17,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==17,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==17,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==18,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==18,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==18,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==19,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==19,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==19,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_Fmsy[dat_Fmsy$CouncilCode==20,]$interval)>0, mean(aggregate(dat_Fmsy[dat_Fmsy$CouncilCode==20,]$interval, list(dat_Fmsy[dat_Fmsy$CouncilCode==20,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==1,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==1,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==1,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==2,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==2,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==2,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==3,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==3,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==3,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==4,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==4,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==4,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==5,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==5,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==5,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==6,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==6,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==6,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==7,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==7,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==7,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==8,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==8,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==8,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==9,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==9,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==9,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==10,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==10,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==10,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==11,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==11,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==11,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==12,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==12,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==12,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==13,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==13,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==13,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==14,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==14,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==14,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==15,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==15,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==15,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==16,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==16,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==16,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==17,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==17,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==17,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==18,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==18,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==18,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==19,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==19,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==19,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==20,]$interval)>0, mean(aggregate(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==20,]$interval, list(dat_F_Fmsy[dat_F_Fmsy$CouncilCode==20,]$stockid), FUN=mean)$x), NA),
                         
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==1,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==1,]$interval, list(dat_OFL[dat_OFL$CouncilCode==1,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==2,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==2,]$interval, list(dat_OFL[dat_OFL$CouncilCode==2,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==3,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==3,]$interval, list(dat_OFL[dat_OFL$CouncilCode==3,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==4,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==4,]$interval, list(dat_OFL[dat_OFL$CouncilCode==4,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==5,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==5,]$interval, list(dat_OFL[dat_OFL$CouncilCode==5,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==6,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==6,]$interval, list(dat_OFL[dat_OFL$CouncilCode==6,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==7,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==7,]$interval, list(dat_OFL[dat_OFL$CouncilCode==7,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==8,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==8,]$interval, list(dat_OFL[dat_OFL$CouncilCode==8,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==9,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==9,]$interval, list(dat_OFL[dat_OFL$CouncilCode==9,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==10,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==10,]$interval, list(dat_OFL[dat_OFL$CouncilCode==10,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==11,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==11,]$interval, list(dat_OFL[dat_OFL$CouncilCode==11,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==12,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==12,]$interval, list(dat_OFL[dat_OFL$CouncilCode==12,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==13,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==13,]$interval, list(dat_OFL[dat_OFL$CouncilCode==13,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==14,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==14,]$interval, list(dat_OFL[dat_OFL$CouncilCode==14,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==15,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==15,]$interval, list(dat_OFL[dat_OFL$CouncilCode==15,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==16,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==16,]$interval, list(dat_OFL[dat_OFL$CouncilCode==16,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==17,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==17,]$interval, list(dat_OFL[dat_OFL$CouncilCode==17,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==18,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==18,]$interval, list(dat_OFL[dat_OFL$CouncilCode==18,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==19,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==19,]$interval, list(dat_OFL[dat_OFL$CouncilCode==19,]$stockid), FUN=mean)$x), NA),
                         ifelse(length(dat_OFL[dat_OFL$CouncilCode==20,]$interval)>0, mean(aggregate(dat_OFL[dat_OFL$CouncilCode==20,]$interval, list(dat_OFL[dat_OFL$CouncilCode==20,]$stockid), FUN=mean)$x), NA))        
                         

# read results
load("dd_B.RData")
res_B <- aggregate(dd_B$value, list(dd_B$Region, dd_B$Period), FUN=median)
colnames(res_B) <- c("region", "period", "cv")
res_B_long <- res_B[res_B$period=="Long", c(1,3)]; colnames(res_B_long)[2] <- "cv_long"
res_B_short <- res_B[res_B$period=="Short", c(1,3)]; colnames(res_B_short)[2] <- "cv_short"
res_B <- merge(res_B_long, res_B_short, by="region")
res_B$para <- "B_ty"

load("dd_Bmsy.RData")
res_Bmsy <- aggregate(dd_Bmsy$value, list(dd_Bmsy$Region, dd_Bmsy$Period), FUN=median)
colnames(res_Bmsy) <- c("region", "period", "cv")
res_Bmsy_long <- res_Bmsy[res_Bmsy$period=="Long", c(1,3)]; colnames(res_Bmsy_long)[2] <- "cv_long"
res_Bmsy_short <- res_Bmsy[res_Bmsy$period=="Short", c(1,3)]; colnames(res_Bmsy_short)[2] <- "cv_short"
res_Bmsy <- merge(res_Bmsy_long, res_Bmsy_short, by="region")
res_Bmsy$para <- "Bmsy"

load("dd_B_Bmsy.RData")
res_B_Bmsy <- aggregate(dd_B_Bmsy$value, list(dd_B_Bmsy$Region, dd_B_Bmsy$Period), FUN=median)
colnames(res_B_Bmsy) <- c("region", "period", "cv")
res_B_Bmsy_long <- res_B_Bmsy[res_B_Bmsy$period=="Long", c(1,3)]; colnames(res_B_Bmsy_long)[2] <- "cv_long"
res_B_Bmsy_short <- res_B_Bmsy[res_B_Bmsy$period=="Short", c(1,3)]; colnames(res_B_Bmsy_short)[2] <- "cv_short"
res_B_Bmsy <- merge(res_B_Bmsy_long, res_B_Bmsy_short, by="region")
res_B_Bmsy$para <- "B_Bmsy"

load("dd_F.RData")
res_F <- aggregate(dd_F$value, list(dd_F$Region, dd_F$Period), FUN=median)
colnames(res_F) <- c("region", "period", "cv")
res_F_long <- res_F[res_F$period=="Long", c(1,3)]; colnames(res_F_long)[2] <- "cv_long"
res_F_short <- res_F[res_F$period=="Short", c(1,3)]; colnames(res_F_short)[2] <- "cv_short"
res_F <- merge(res_F_long, res_F_short, by="region")
res_F$para <- "F_ty"

load("dd_Fmsy.RData")
res_Fmsy <- aggregate(dd_Fmsy$value, list(dd_Fmsy$Region, dd_Fmsy$Period), FUN=median)
colnames(res_Fmsy) <- c("region", "period", "cv")
res_Fmsy_long <- res_Fmsy[res_Fmsy$period=="Long", c(1,3)]; colnames(res_Fmsy_long)[2] <- "cv_long"
res_Fmsy_short <- res_Fmsy[res_Fmsy$period=="Short", c(1,3)]; colnames(res_Fmsy_short)[2] <- "cv_short"
res_Fmsy <- merge(res_Fmsy_long, res_Fmsy_short, by="region")
res_Fmsy$para <- "Fmsy"

load("dd_F_Fmsy.RData")
res_F_Fmsy <- aggregate(dd_F_Fmsy$value, list(dd_F_Fmsy$Region, dd_F_Fmsy$Period), FUN=median)
colnames(res_F_Fmsy) <- c("region", "period", "cv")
res_F_Fmsy_long <- res_F_Fmsy[res_F_Fmsy$period=="Long", c(1,3)]; colnames(res_F_Fmsy_long)[2] <- "cv_long"
res_F_Fmsy_short <- res_F_Fmsy[res_F_Fmsy$period=="Short", c(1,3)]; colnames(res_F_Fmsy_short)[2] <- "cv_short"
res_F_Fmsy <- merge(res_F_Fmsy_long, res_F_Fmsy_short, by="region")
res_F_Fmsy$para <- "F_Fmsy"

load("dd_OFL.RData")
res_OFL <- aggregate(dd_OFL$value, list(dd_OFL$Region, dd_OFL$Period), FUN=median)
colnames(res_OFL) <- c("region", "period", "cv")
res_OFL_long <- res_OFL[res_OFL$period=="Long", c(1,3)]; colnames(res_OFL_long)[2] <- "cv_long"
res_OFL_short <- res_OFL[res_OFL$period=="Short", c(1,3)]; colnames(res_OFL_short)[2] <- "cv_short"
res_OFL <- merge(res_OFL_long, res_OFL_short, by="region")
res_OFL$para <- "OFL_ty"

res <- rbind(res_B, res_Bmsy, res_B_Bmsy, res_F, res_Fmsy, res_F_Fmsy, res_OFL)
res$key <- paste(res$region, res$para, sep="_")
dd$key <- paste(dd$region, dd$para, sep="_")

dd_cv <- merge(dd, res[,c(5,2,3)], by="key")
summary(dd_cv)

dd_cv[is.na(dd_cv$average_interval),]$cv_long <- NA
dd_cv[is.na(dd_cv$average_interval),]$cv_short <- NA

dd_cv$para <- factor(dd_cv$para, levels=c("B_ty", "Bmsy", "B_Bmsy", "F_ty", "Fmsy", "F_Fmsy", "OFL_ty"))
dd_cv$group <- factor(dd_cv$para, labels=c(expression(bolditalic(B[ty])), expression(bolditalic(B[MSY])), expression(bolditalic(B[ty]/B[MSY])),
                                           expression(bolditalic(F[ty])), expression(bolditalic(F[MSY])), expression(bolditalic(F[ty]/F[MSY])),
                                           expression(bolditalic(OFL[ty]))))

dd_cv$region <- factor(dd_cv$region, levels = c("GMFMC", "MAFMC", "NEFMC", "NPFMC", "PFMC", "SAFMC", "US non-federal",
                                                "Europe (EU)", "Europe (non-EU)",  "CA East Coast", "CA West Coast", 
                                                "Australia", "New Zealand", "Japan", "South Africa", "South America",
                                                "AOHS", "IOHS", "POHS", "MBS"))


library(ggplot2)
library(ggpmisc)

cbPalette1 <- c(1:20)
cbPalette2 <- c("#CC79A7", "#0072B2")

dd_cv_long <- dd_cv[,c(2,4,5,3,7)]; colnames(dd_cv_long)[3] <- "cv"; dd_cv_long$type <- "CV_long"
dd_cv_short <- dd_cv[,c(2,4,6,3,7)]; colnames(dd_cv_short)[3] <- "cv"; dd_cv_short$type <- "CV_short"
dd <- rbind(dd_cv_long, dd_cv_short)
dd$type <- factor(dd$type, levels=c("CV_long", "CV_short"))

dd$region <- factor(dd$region, levels = c("GMFMC", "MAFMC", "NEFMC", "NPFMC", "PFMC", "SAFMC", "US non-federal",
                                          "Europe (EU)", "Europe (non-EU)",  "CA East Coast", "CA West Coast", 
                                          "Australia", "New Zealand", "Japan", "South Africa", "South America",
                                          "AOHS", "IOHS", "POHS", "MBS"))

p1 <- ggplot(data=dd, aes(x=average_interval, y=cv, colour=type)) +
  geom_point(aes(shape=region), size = 3, stroke = 1.5) + 
  stat_poly_line(size=1.1) +
  #stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), after_stat(p.value.label), sep = "*\", \"*")), size=5, vjust=1.58, hjust=0.05) +
  stat_poly_eq(aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "*\", \"*")), size=5, vjust=1.7, hjust=0.05) +
  scale_colour_manual(name = "Type", values=cbPalette2, labels = expression(CV[long], CV[short])) +
  scale_shape_manual(name = "Region", values=cbPalette1, labels = c("GMFMC", "MAFMC", "NEFMC", "NPFMC", "PFMC", "SAFMC", "US non-federal",
                                                                      "Europe (EU)", "Europe (non-EU)",  "CA East Coast", "CA West Coast", 
                                                                      "Australia", "New Zealand", "Japan", "South Africa", "South America",
                                                                      "AOHS", "IOHS", "POHS", "MBS")) +
  facet_wrap(~group, labeller = label_parsed, scales = "free", strip.position = "top", nrow = 3, dir = "v") + 
  xlab("Average assessment frequency (year)") +
  ylab("CV (%)") +
  theme_bw() + 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.y   = element_text(size=20),
        axis.text.x   = element_text(size=20),
        axis.title.y  = element_text(size=22),
        axis.title.x  = element_text(size=22),
        legend.title = element_text(size=22),
        legend.text = element_text(size=20),
        legend.position = c(0.84, 0.35),
        legend.box="vertical", 
        legend.margin=margin(40,0,0,0),
        legend.spacing.y = unit(0.8, "lines"),
        strip.text.x = element_text(size=22, face = "bold"),
        strip.text.y = element_text(size=22, face = "bold"),
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines")) 


##########################################################################

# CV_long linear regression
# B_ty
dd_Bty <- dd[dd$para=="B_ty" & dd$group=="CV_long",]
data = list(cv = as.vector(dd_Bty$cv),
            freq = as.vector(dd_Bty$average_interval),
            nrow = dim(dd_Bty)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta      2.911    -4.737     2.853      10.70
# intercept 33.778   -4.930     34.384     71.53
# R^2 = 0.03

# Bmsy
dd_Bmsy <- dd[dd$para=="Bmsy" & dd$group=="CV_long" & !is.na(dd$cv),]
data = list(cv = as.vector(dd_Bmsy$cv),
            freq = as.vector(dd_Bmsy$average_interval),
            nrow = dim(dd_Bmsy)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta       4.678    -0.3428     4.711     9.443
# intercept  15.274   -8.3947     14.978    40.247
# R^2 = 0.26

# B_Bmsy
dd_B_Bmsy <- dd[dd$para=="B_Bmsy" & dd$group=="CV_long" & !is.na(dd$cv),]
data = list(cv = as.vector(dd_B_Bmsy$cv),
            freq = as.vector(dd_B_Bmsy$average_interval),
            nrow = dim(dd_B_Bmsy)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta       1.754    -0.7288    1.723     4.319
# intercept  21.193   8.3639     21.256    33.947
# R^2 = 0.17

# F_ty
dd_Fty <- dd[dd$para=="F_ty" & dd$group=="CV_long",]
data = list(cv = as.vector(dd_Fty$cv),
            freq = as.vector(dd_Fty$average_interval),
            nrow = dim(dd_Fty)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta      2.316    -3.004     2.352     7.405
# intercept 38.792   13.529     38.726    65.242
# R^2 = 0.05

# Fmsy
dd_Fmsy <- dd[dd$para=="Fmsy" & dd$group=="CV_long" & !is.na(dd$cv),]
data = list(cv = as.vector(dd_Fmsy$cv),
            freq = as.vector(dd_Fmsy$average_interval),
            nrow = dim(dd_Fmsy)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta       3.295    -0.3662     3.274    7.015
# intercept  11.833   -4.4743     11.817   27.978
# R^2 = 0.23

# F_Fmsy
dd_F_Fmsy <- dd[dd$para=="F_Fmsy" & dd$group=="CV_long" & !is.na(dd$cv),]
data = list(cv = as.vector(dd_F_Fmsy$cv),
            freq = as.vector(dd_F_Fmsy$average_interval),
            nrow = dim(dd_F_Fmsy)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta       6.989    0.526       6.982    13.59
# intercept  19.004   -9.447      19.083   46.86
# R^2 = 0.34

# OFL_ty
dd_OFLty <- dd[dd$para=="OFL_ty" & dd$group=="CV_long" & !is.na(dd$cv),]
data = list(cv = as.vector(dd_OFLty$cv),
            freq = as.vector(dd_OFLty$average_interval),
            nrow = dim(dd_OFLty)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta       2.881    -4.157      2.975    9.403
# intercept  44.406   14.034      44.338   77.064
# R^2 = 0.06



# CV_short linear regression
# B_ty
dd_Bty <- dd[dd$para=="B_ty" & dd$group=="CV_short",]
data = list(cv = as.vector(dd_Bty$cv),
            freq = as.vector(dd_Bty$average_interval),
            nrow = dim(dd_Bty)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta      -1.986   -6.287     -2.009     2.303
# intercept 44.311   23.265     44.332     65.747
# R^2 = 0.05

# Bmsy
dd_Bmsy <- dd[dd$para=="Bmsy" & dd$group=="CV_short" & !is.na(dd$cv),]
data = list(cv = as.vector(dd_Bmsy$cv),
            freq = as.vector(dd_Bmsy$average_interval),
            nrow = dim(dd_Bmsy)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta       3.35    0.05297     3.334     6.593
# intercept  14.49   -1.65339    14.505    30.854
# R^2 = 0.29

# B_Bmsy
dd_B_Bmsy <- dd[dd$para=="B_Bmsy" & dd$group=="CV_short" & !is.na(dd$cv),]
data = list(cv = as.vector(dd_B_Bmsy$cv),
            freq = as.vector(dd_B_Bmsy$average_interval),
            nrow = dim(dd_B_Bmsy)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta       1.719    -0.642    1.728      4.091
# intercept  17.922   5.875     17.835    29.604
# R^2 = 0.17

# F_ty
dd_Fty <- dd[dd$para=="F_ty" & dd$group=="CV_short",]
data = list(cv = as.vector(dd_Fty$cv),
            freq = as.vector(dd_Fty$average_interval),
            nrow = dim(dd_Fty)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta      3.557   -1.678       3.482     9.111
# intercept 28.594   1.525       28.924    54.635
# R^2 = 0.09

# Fmsy
dd_Fmsy <- dd[dd$para=="Fmsy" & dd$group=="CV_short" & !is.na(dd$cv),]
data = list(cv = as.vector(dd_Fmsy$cv),
            freq = as.vector(dd_Fmsy$average_interval),
            nrow = dim(dd_Fmsy)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta       3.903   -0.7844     3.868     8.497
# intercept  9.407   -10.8820    9.643     29.565
# R^2 = 0.21

# F_Fmsy
dd_F_Fmsy <- dd[dd$para=="F_Fmsy" & dd$group=="CV_short" & !is.na(dd$cv),]
data = list(cv = as.vector(dd_F_Fmsy$cv),
            freq = as.vector(dd_F_Fmsy$average_interval),
            nrow = dim(dd_F_Fmsy)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta       7.113    0.6275      7.144    13.67
# intercept  16.976   -10.4720    17.055   44.42
# R^2 = 0.34

# OFL_ty
dd_OFLty <- dd[dd$para=="OFL_ty" & dd$group=="CV_short" & !is.na(dd$cv),]
data = list(cv = as.vector(dd_OFLty$cv),
            freq = as.vector(dd_OFLty$average_interval),
            nrow = dim(dd_OFLty)[1])
params = c("beta","intercept")
fit <- run.jags(model="Linear_model.txt", monitor=params, data=data, burnin=2000, sample=1000, adapt=2000, thin=5, n.chains=5, method = 'rjparallel')
mcmc <- as.mcmc(fit)
summary(mcmc)
#             Mean     2.5%       50%      97.5% 
# beta       2.992    -3.613      2.896    9.835
# intercept  35.170   3.643      35.505   65.763
# R^2 = 0.06

##########################################################################


### CHANGE!!!!!!!!!!!
tag_facet <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE)
}

my_tag_long <- c(expression("y=33.78[-4.93,71.53]+2.91[-4.73,10.70]x, R^2=0.03"), 
                 expression("y=15.27[-8.39,40.25]+4.68[-0.34,9.44]x, R^2=0.26"),
                 expression("y=21.19[8.36,33.95]+1.75[-0.73,4.32]x, R^2=0.17"),
                 expression("y=38.79[13.53,65.24]+2.32[-3.00,7.41]x, R^2=0.05"),
                 expression("y=11.83[-4.47,27.98]+3.30[-0.37,7.02]x, R^2=0.23"),
                 expression("y=19.00[-9.45,46.86]+6.99[0.53,13.59]x, R^2=0.34"),
                 expression("y=44.41[14.03,77.06]+2.88[-4.16,9.40]x, R^2=0.06"))

my_tag_short <- c(expression("y=33.78[-4.93,71.53]+2.91[-4.73,10.70]x, R^2=0.03"), 
                 expression("y=15.27[-8.39,40.25]+4.68[-0.34,9.44]x, R^2=0.26"),
                 expression("y=21.19[8.36,33.95]+1.75[-0.73,4.32]x, R^2=0.17"),
                 expression("y=38.79[13.53,65.24]+2.32[-3.00,7.41]x, R^2=0.05"),
                 expression("y=11.83[-4.47,27.98]+3.30[-0.37,7.02]x, R^2=0.23"),
                 expression("y=19.00[-9.45,46.86]+6.99[0.53,13.59]x, R^2=0.34"),
                 expression("y=44.41[14.03,77.06]+2.88[-4.16,9.40]x, R^2=0.06"))


p2 <- tag_facet(p1, 
                x = -Inf, y = -Inf, 
                vjust = -20, hjust = -0.05,
                open = "", close = "",
                fontface = 4,
                size = 4,
                colour = c("#CC79A7"),
                tag_pool = my_tag_long)

p3 <- tag_facet(p2, 
                x = -Inf, y = -Inf, 
                vjust = -4, hjust = -0.1,
                open = "", close = "",
                fontface = 4,
                size = 4,
                colour = c("#0072B2"),
                tag_pool = my_tag_short)


png("Frequency.png", height = 16, width = 16, units='in', res=600)
p1
dev.off()


