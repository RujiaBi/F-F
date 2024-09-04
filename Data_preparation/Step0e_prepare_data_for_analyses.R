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

data <- read.csv(paste(outputdir, "0d_full_data_with_msy.csv", sep="/"), as.is=T)
dim(data)  # 1281 rows

#####################################################################################################################

# Delete assessments without B nor F records
data_with_records <- data[!is.na(data$B_ty) & !is.na(data$F_ty),]
dim(data_with_records)  # 857 rows

# Check data
data_num_assess <- as.data.frame(aggregate(assessid ~ stockid, data = data_with_records, FUN = length))
colnames(data_num_assess) <- c("stockid", "number_of_assess")
table(data_num_assess$number_of_assess) 
# 1   2    3   4   5   6   7   8 
# 56 188  13   4   8  17  28   4 

stockid_2plus <- data_num_assess[data_num_assess$number_of_assess>1,]$stockid
stockid_2assess <- data_num_assess[data_num_assess$number_of_assess==2,]$stockid
stockid_3assess <- data_num_assess[data_num_assess$number_of_assess==3,]$stockid
stockid_4assess <- data_num_assess[data_num_assess$number_of_assess==4,]$stockid
stockid_5assess <- data_num_assess[data_num_assess$number_of_assess==5,]$stockid
stockid_6assess <- data_num_assess[data_num_assess$number_of_assess==6,]$stockid
stockid_7assess <- data_num_assess[data_num_assess$number_of_assess==7,]$stockid
stockid_8assess <- data_num_assess[data_num_assess$number_of_assess==8,]$stockid

data_2plus <- data_with_records[data_with_records$stockid %in% stockid_2plus,]
length(unique(data_2plus$stockid))  # 262 stocks
length(unique(data_2plus$assessid))  # 801 assessments

length(unique(data_2plus[data_2plus$country=="USA",]$stockid))  # 71
length(unique(data_2plus[data_2plus$region=="Europe (EU)",]$stockid))  # 60
length(unique(data_2plus[data_2plus$country=="Canada",]$stockid))  # 18
length(unique(data_2plus[data_2plus$country=="Japan",]$stockid))  # 14

########################################################################################################################################

dd_source <- read.csv(paste("~/Desktop/Climate_Stock/Bi_Code/B_F_OFL_dist/Data_preparation/output/Sheets", "0e_assess_for_PDF_done_old.csv", sep="/"), as.is=T)
dd_new <- merge(data_2plus, dd_source[,c(1,5,6,7,8,9,17,19,21,23,27)], by="assessid", all.x=TRUE)

dd_new$assesssource <- ifelse(!is.na(dd_new$assesssource.y), dd_new$assesssource.y, dd_new$assesssource.x)
dd_new$method <- ifelse(!is.na(dd_new$method.y), dd_new$method.y, dd_new$method.x)
dd_new$methodlong <- ifelse(!is.na(dd_new$methodlong.y), dd_new$methodlong.y, dd_new$methodlong.x)
dd_new$M <- ifelse(!is.na(dd_new$M.y), dd_new$M.y, dd_new$M.x)
dd_new$M_notes <- ifelse(!is.na(dd_new$M_notes.y), dd_new$M_notes.y, dd_new$M_notes.x)
dd_new$ERmsybest <- ifelse(!is.na(dd_new$ERmsybest.y), dd_new$ERmsybest.y, dd_new$ERmsybest.x)
dd_new$Fmsy <- ifelse(!is.na(dd_new$Fmsy.y), dd_new$Fmsy.y, dd_new$Fmsy.x)
dd_new$TBmsybest <- ifelse(!is.na(dd_new$TBmsybest.y), dd_new$TBmsybest.y, dd_new$TBmsybest.x)
dd_new$SSBmsy <- ifelse(!is.na(dd_new$SSBmsy.y), dd_new$SSBmsy.y, dd_new$SSBmsy.x)

dd_new <- dd_new[,c(12,1:7,96,97,9,98,11,13:23,25:28,99,30,100,32:69,101,71:73,102,75:79,103,81,104,83:85,95)]

dd_new <- dd_new[order(dd_new$region),]

# Export full data
write.csv(dd_new, paste(outputdir, "0e_0_data_2plus_assess_with_records.csv", sep="/"), row.names=F)

########################################################################################################################################

## Add information from PDFs ##
## Stocks: B using SSB
dd_ssb <- dd_new[dd_new$B_type == "SSB-MT" | dd_new$B_type == "SSB-E00eggs",]
dd_no_ssb <- dd_new[dd_new$B_type == "TBbest-MT" | dd_new$B_type == "TB-MT" | dd_new$B_type == "TN-E00",]

# US East Coast stocks
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "ACADREDGOMGB",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Acadian%20redfish%20-%20Gulf%20of%20Maine%20/%20Georges%20Bank&stockid=10455"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "ACADREDGOMGB" & dd_ssb$report_year==2014,]$M <- 0.05

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "AMPL5YZ",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=American%20plaice%20-%20Gulf%20of%20Maine%20/%20Georges%20Bank&stockid=10019"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "AMPL5YZ" & dd_ssb$report_year==2014,]$M <- 0.2

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "CODGOM",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Atlantic%20cod%20-%20Gulf%20of%20Maine&stockid=10508"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "CODGOM",]$M <- 0.2
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "CODGOM" & dd_ssb$report_year==2014,]$Fmsy <- 0.187

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "HAD5Y",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Haddock%20-%20Gulf%20of%20Maine&stockid=10519"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "HAD5Y",]$M <- 0.2

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "HADGB",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Haddock%20-%20Georges%20Bank&stockid=10520"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "HADGB",]$M <- 0.2

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "HERRNWATLC",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Atlantic%20herring%20-%20Northwestern%20Atlantic%20Coast&stockid=10572"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "HERRNWATLC",]$M <- 0.2

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "POLL5YZ",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Pollock%20-%20Gulf%20of%20Maine%20/%20Georges%20Bank&stockid=10515"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "POLL5YZ" & dd_ssb$report_year==2009,]$Fmsy <- 0.25
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "POLL5YZ" & dd_ssb$report_year==2009,]$SSBmsy <- 91000
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "POLL5YZ",]$M <- 0.2

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "SCUPNWATLC",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Scup%20-%20Atlantic%20Coast&stockid=10286"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "SCUPNWATLC" & dd_ssb$report_year==2007,]$Fmsy <- 0.177
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "SCUPNWATLC" & dd_ssb$report_year==2007,]$SSBmsy <- 92044
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "SCUPNWATLC",]$M <- 0.2

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "SDOGATLC",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Spiny%20dogfish%20-%20Atlantic%20Coast&stockid=10701"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "SDOGATLC" & dd_ssb$report_year==2006,]$method <- "Length based survey, swept area"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "SDOGATLC" & dd_ssb$report_year==2015,]$method <- "Area-swept survey biomass"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "SDOGATLC" & dd_ssb$report_year==2006,]$Fmsy <- 0.39
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "SDOGATLC",]$M <- 0.092

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "SFLOUNMATLC",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Summer%20flounder%20-%20Mid-Atlantic%20Coast&stockid=10191"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "SFLOUNMATLC" & dd_ssb$report_year==2012,]$Fmsy <- 0.309
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "SFLOUNMATLC",]$M <- 0.25

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "WHAKEGBGOM",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=White%20hake%20-%20Gulf%20of%20Maine%20/%20Georges%20Bank&stockid=10518"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "WHAKEGBGOM" & dd_ssb$report_year==2011,]$Fmsy <- 0.2
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "WHAKEGBGOM" & dd_ssb$report_year==2011,]$SSBmsy <- 32400
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "WHAKEGBGOM",]$M <- 0.2

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "WINFLOUN5Z",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Winter%20flounder%20-%20Georges%20Bank&stockid=10029"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "WINFLOUN5Z",]$M <- 0.3

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "WITFLOUN5Y",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Witch%20flounder%20-%20Northwestern%20Atlantic%20Coast&stockid=10014"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "WITFLOUN5Y",]$M <- 0.15

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "YELLCCODGOM",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Witch%20flounder%20-%20Northwestern%20Atlantic%20Coast&stockid=10014"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "YELLCCODGOM" & dd_ssb$report_year==2008,]$Fmsy <- 0.24
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "YELLCCODGOM",]$M <- 0.2

dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "YELLSNEMATL",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Yellowtail%20flounder%20-%20Southern%20New%20England%20/%20Mid-Atlantic&stockid=10034"
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "YELLSNEMATL" & dd_ssb$report_year==2011,]$Fmsy <- 0.316
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "YELLSNEMATL" & dd_ssb$report_year==2011,]$SSBmsy <- 2995
dd_ssb[dd_ssb$region=="US East Coast" & dd_ssb$stockid == "YELLSNEMATL" & dd_ssb$report_year==2014,]$M <- 0.3

# US Southeast and Gulf stocks
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "BRNSHRIMPGM",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Brown%20shrimp%20-%20Gulf%20of%20Mexico&stockid=10079"
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "BRNSHRIMPGM",]$M <- 3.24

dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "GTRIGGM",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Gray%20triggerfish%20-%20Gulf%20of%20Mexico&stockid=10062"
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "GTRIGGM" & dd_ssb$report_year==2013,]$B_ty <- 12500000000
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "GTRIGGM" & dd_ssb$report_year==2013,]$B_ty_minus1 <- 12400000000
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "GTRIGGM" & dd_ssb$report_year==2013,]$SSBmsy <- 11100000000
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "GTRIGGM" & dd_ssb$report_year==2010,]$SSBmsy <- 1780000000000
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "GTRIGGM" & dd_ssb$report_year==2010,]$Fmsy <- 0.34
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "GTRIGGM",]$M <- 0.27

dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "MENATGM" & dd_ssb$report_year==2011,]$assesssource <- "http://sedarweb.org/docs/sar/S32A_GoM_Menhaden_SAR_Final_9.26.2013.pdf"
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "MENATGM" & dd_ssb$report_year==2017,]$assesssource <- "http://sedarweb.org/docs/sar/S63_GulfMenSAR_12.17.2018_FINAL.pdf"
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "MENATGM" & dd_ssb$report_year==2011,]$M <- 1.10

dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "PINKSHRIMPGM",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Pink%20shrimp%20-%20Gulf%20of%20Mexico&stockid=10081"
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "PINKSHRIMPGM",]$M <- 0.3

dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "VSNAPGM",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Vermilion%20snapper%20-%20Gulf%20of%20Mexico&stockid=10256"
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "VSNAPGM" & dd_ssb$report_year==2004,]$SSBmsy <- 68800000000000
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "VSNAPGM" & dd_ssb$report_year==2014,]$SSBmsy <- 197000000000000
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "VSNAPGM",]$M <- 0.25

dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "WSHRIMPGM",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=White%20shrimp%20-%20Gulf%20of%20Mexico&stockid=10083"
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "WSHRIMPGM" & dd_ssb$report_year==2011,]$SSBmsy <- 68800000000000
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "WSHRIMPGM" & dd_ssb$report_year==2014,]$SSBmsy <- 197000000000000
dd_ssb[dd_ssb$region=="US Southeast and Gulf" & dd_ssb$stockid == "WSHRIMPGM",]$M <- 0.25

# US Alaska stocks
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="PCODGA",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Pacific%20cod%20-%20Gulf%20of%20Alaska&stockid=10506"
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="PCODGA" & dd_ssb$report_year==2014,]$Fmsy <- 0.626
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="PCODGA" & dd_ssb$report_year==2014,]$SSBmsy <- 110700
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="PCODGA" & dd_ssb$report_year==2014,]$SSBmsy_notes <- "B35%"
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="PCODGA" & dd_ssb$report_year==2015,]$assesssource <- "https://apps-afsc.fisheries.noaa.gov/REFM/Docs/2015/GOApcod.pdf"
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="PCODGA" & dd_ssb$report_year==2015,]$Fmsy <- 0.495
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="PCODGA" & dd_ssb$report_year==2015,]$SSBmsy <- 113800
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="PCODGA" & dd_ssb$report_year==2015,]$SSBmsy_notes <- "B35%"

dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="SNOWCRABBS" & dd_ssb$report_year==2013,]$assesssource <- "https://www.fisheries.noaa.gov/resource/data/2013-alaska-fisheries-stock-assessment-and-fishery-evaluation-report-king-and-tanner"
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="SNOWCRABBS" & dd_ssb$report_year==2013,]$M <- 0.246
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="SNOWCRABBS" & dd_ssb$report_year==2013,]$Fmsy <- 1.58
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="SNOWCRABBS" & dd_ssb$report_year==2013,]$SSBmsy <- 154170
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="SNOWCRABBS" & dd_ssb$report_year==2013,]$SSBmsy_notes <- "B35%"
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="SNOWCRABBS" & dd_ssb$report_year==2015,]$assesssource <- "https://www.fisheries.noaa.gov/resource/data/2015-alaska-stock-assessment-and-fishery-evaluation-report-king-and-tanner-crab"
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="SNOWCRABBS" & dd_ssb$report_year==2015,]$M <- 0.246
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="SNOWCRABBS" & dd_ssb$report_year==2015,]$Fmsy <- 1.32
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="SNOWCRABBS" & dd_ssb$report_year==2015,]$SSBmsy <- 157800
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="SNOWCRABBS" & dd_ssb$report_year==2015,]$SSBmsy_notes <- "B35%"

dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="TANNERCRABBSAI" & dd_ssb$report_year==2013,]$assesssource <- "https://www.fisheries.noaa.gov/resource/data/2013-alaska-fisheries-stock-assessment-and-fishery-evaluation-report-king-and-tanner"
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="TANNERCRABBSAI" & dd_ssb$report_year==2013,]$M <- 0.295
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="TANNERCRABBSAI" & dd_ssb$report_year==2013,]$Fmsy <- 0.73
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="TANNERCRABBSAI" & dd_ssb$report_year==2013,]$SSBmsy <- 33540
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="TANNERCRABBSAI" & dd_ssb$report_year==2013,]$SSBmsy_notes <- "B35%"
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="TANNERCRABBSAI" & dd_ssb$report_year==2015,]$assesssource <- "https://www.fisheries.noaa.gov/resource/data/2015-alaska-stock-assessment-and-fishery-evaluation-report-king-and-tanner-crab"
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="TANNERCRABBSAI" & dd_ssb$report_year==2015,]$M <- 0.295
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="TANNERCRABBSAI" & dd_ssb$report_year==2015,]$Fmsy <- 0.58
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="TANNERCRABBSAI" & dd_ssb$report_year==2015,]$SSBmsy <- 26800
dd_ssb[dd_ssb$region=="US Alaska" & dd_ssb$stockid=="TANNERCRABBSAI" & dd_ssb$report_year==2015,]$SSBmsy_notes <- "B35%"

# US West Coast stocks
dd_ssb[dd_ssb$region=="US West Coast" & dd_ssb$stockid=="PHAKEPCOAST",]$assesssource <- "https://www.st.nmfs.noaa.gov/stocksmart?stockname=Pacific%20hake%20-%20Pacific%20Coast&stockid=10523"

# Atlantic Ocean
dd_ssb[dd_ssb$region=="Atlantic Ocean" & dd_ssb$stockid=="ATBTUNAEATL",]
dd_ssb[dd_ssb$region=="Atlantic Ocean" & dd_ssb$stockid=="ATBTUNAWATL",]

# Europe (EU) stocks
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HERRSIRS" & dd_ssb$report_year == 2012,]$assesssource <- "http://www.ices.dk/sites/pub/Publication Reports/Advice/2012/2012/her-irls.pdf"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HERRSIRS" & dd_ssb$report_year == 2014,]$method <- "Age-based analytical assessment (SAM)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HERRSIRS" & dd_ssb$report_year == 2014,]$Fmsy <- 0.37
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HERRSIRS" & dd_ssb$report_year == 2015,]$method <- "Age-based analytical assessment (ASAP)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HERRSIRS" & dd_ssb$report_year == 2015,]$SSBmsy <- 54000

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA1" & dd_ssb$report_year == 2016,]$method <- "Seasonal age-based analytical (SMS-effort)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA1" & dd_ssb$report_year == 2017,]$method <- "Seasonal age-based analytical (SMS-effort)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA1" & dd_ssb$report_year == 2017,]$SSBmsy <- 145000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA1" & dd_ssb$report_year == 2017,]$Fmsy <- 0.49
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA1" & dd_ssb$report_year == 2017,]$Fmsy_notes <- "Fcap"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA1" & dd_ssb$report_year == 2018,]$method <- "Seasonal age-based analytical (SMS-effort)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA1" & dd_ssb$report_year == 2018,]$SSBmsy <- 145000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA1" & dd_ssb$report_year == 2018,]$Fmsy <- 0.49
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA1" & dd_ssb$report_year == 2018,]$Fmsy_notes <- "Fcap"

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA2" & dd_ssb$report_year == 2014,]$SSBmsy <- 215000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA2" & dd_ssb$report_year == 2016,]$method <- "Seasonal age-based analytical (SMS-effort)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA2" & dd_ssb$report_year == 2016,]$SSBmsy <- 100000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA2" & dd_ssb$report_year == 2017,]$method <- "Seasonal age-based analytical (SMS-effort)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA2" & dd_ssb$report_year == 2017,]$SSBmsy <- 84000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA2" & dd_ssb$report_year == 2017,]$Fmsy <- 0.44
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA2" & dd_ssb$report_year == 2017,]$Fmsy_notes <- "Fcap"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA2" & dd_ssb$report_year == 2018,]$method <- "Seasonal age-based analytical (SMS-effort)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA2" & dd_ssb$report_year == 2018,]$SSBmsy <- 84000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA2" & dd_ssb$report_year == 2018,]$Fmsy <- 0.44
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA2" & dd_ssb$report_year == 2018,]$Fmsy_notes <- "Fcap"

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA3" & dd_ssb$report_year == 2016,]$method <- "Seasonal age-based analytical (SMS-effort)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA3" & dd_ssb$report_year == 2016,]$SSBmsy <- 195000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA3" & dd_ssb$report_year == 2017,]$method <- "Seasonal age-based analytical (SMS-effort)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA3" & dd_ssb$report_year == 2017,]$SSBmsy <- 129000 
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA3" & dd_ssb$report_year == 2017,]$Fmsy <- 0.29
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA3" & dd_ssb$report_year == 2017,]$Fmsy_notes <- "Fcap"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA3" & dd_ssb$report_year == 2018,]$method <- "Seasonal age-based analytical (SMS-effort)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA3" & dd_ssb$report_year == 2018,]$SSBmsy <- 129000 
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA3" & dd_ssb$report_year == 2018,]$Fmsy <- 0.29
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA3" & dd_ssb$report_year == 2018,]$Fmsy_notes <- "Fcap"

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA4" & dd_ssb$report_year == 2017,]$method <- "Seasonal age-based analytical (SMS-effort)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA4" & dd_ssb$report_year == 2017,]$SSBmsy <- 102000 
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA4" & dd_ssb$report_year == 2017,]$Fmsy <- 0.15
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA4" & dd_ssb$report_year == 2017,]$Fmsy_notes <- "Fcap"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA4" & dd_ssb$report_year == 2018,]$method <- "Seasonal age-based analytical (SMS-effort)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA4" & dd_ssb$report_year == 2018,]$SSBmsy <- 102000 
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA4" & dd_ssb$report_year == 2018,]$Fmsy <- 0.15
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SEELNSSA4" & dd_ssb$report_year == 2018,]$Fmsy_notes <- "Fcap"

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SPRATNS" & dd_ssb$report_year == 2017,]$method <- "Age-based analytical assessment (Stochastic Multispecies Model)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SPRATNS" & dd_ssb$report_year == 2017,]$SSBmsy <- 142000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SPRATNS" & dd_ssb$report_year == 2017,]$Fmsy <- 0.70
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SPRATNS" & dd_ssb$report_year == 2017,]$Fmsy_notes <- "Fcap"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SPRATNS" & dd_ssb$report_year == 2018,]$method <- "Age-based analytical assessment (Stochastic Multispecies Model)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SPRATNS" & dd_ssb$report_year == 2018,]$SSBmsy <- 142000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SPRATNS" & dd_ssb$report_year == 2018,]$Fmsy <- 0.70
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SPRATNS" & dd_ssb$report_year == 2018,]$Fmsy_notes <- "Fcap"

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="CODBA2224" & dd_ssb$report_year == 2012,]$method <- "Age analytical assessment (SAM)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="CODBA2224" & dd_ssb$report_year == 2012,]$SSBmsy <- 23000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="CODBA2224" & dd_ssb$report_year == 2012,]$M <- 0.2

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HERR30-31" & dd_ssb$report_year == 2017,]$method <- "Age-based analytical assessment (SAM)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HERR30-31" & dd_ssb$report_year == 2017,]$SSBmsy <- 283180
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HERR30-31" & dd_ssb$report_year == 2018,]$method <- "Age-based analytical assessment (SAM)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HERR30-31" & dd_ssb$report_year == 2018,]$SSBmsy <- 283180

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEIIIa-2224" & dd_ssb$report_year == 2011,]$SSBmsy <- 2000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEIIIa-2224" & dd_ssb$report_year == 2013,]$method <- "Age-based analytical stochastic assessment (SAM)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEIIIa-2224" & dd_ssb$report_year == 2013,]$SSBmsy <- 2000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEIIIa-2224" & dd_ssb$report_year == 2014,]$method <- "Age-based analytical stochastic assessment (SAM)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEIIIa-2224" & dd_ssb$report_year == 2014,]$SSBmsy <- 2000

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="FMEG8c9a" & dd_ssb$report_year == 2014,]$SSBmsy <- 4600

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HAKESOTH" & dd_ssb$report_year == 2011,]$method <- "Length–age analytical assessment (GADGET)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HAKESOTH" & dd_ssb$report_year == 2013,]$method <- "Length–age analytical assessment (GADGET)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HAKESOTH" & dd_ssb$report_year == 2014,]$method <- "Length–age analytical assessment (GADGET)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HAKESOTH" & dd_ssb$report_year == 2015,]$method <- "Length–age analytical assessment (GADGET)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HAKESOTH" & dd_ssb$report_year == 2016,]$method <- "Length–age analytical assessment (GADGET)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HAKESOTH" & dd_ssb$report_year == 2017,]$method <- "Length–age analytical assessment (GADGET)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HAKESOTH" & dd_ssb$report_year == 2017,]$SSBmsy <- 11100
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HAKESOTH" & dd_ssb$report_year == 2018,]$method <- "Length–age analytical assessment (GADGET)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HAKESOTH" & dd_ssb$report_year == 2018,]$SSBmsy <- 11100

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="MEG8c9a" & dd_ssb$report_year == 2014,]$method <- "Age-based assessment (XSA)"
 
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="EBASSIVbc-VII" & dd_ssb$report_year == 2016,]$method <- "Age- and length-based analytical assessment (Stock Synthesis 3)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="EBASSIVbc-VII" & dd_ssb$report_year == 2016,]$SSBmsy <- 12673
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="EBASSIVbc-VII" & dd_ssb$report_year == 2016,]$Fmsy <- NA
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="EBASSIVbc-VII" & dd_ssb$report_year == 2018,]$method <- "Age- and length-based analytical assessment (Stock Synthesis 3)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="EBASSIVbc-VII" & dd_ssb$report_year == 2018,]$SSBmsy <- 13465

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HADIS" & dd_ssb$report_year == 2016,]$method <- "Age-structured model (ASAP)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HADIS" & dd_ssb$report_year == 2016,]$SSBmsy <- 3093
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HADIS" & dd_ssb$report_year == 2017,]$method <- "Age-structured model (ASAP)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HADIS" & dd_ssb$report_year == 2017,]$SSBmsy <- 2944
      
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIIe" & dd_ssb$report_year == 2017,]$method <- "VPA"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIIe" & dd_ssb$report_year == 2017,]$SSBmsy <- 2900

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2017,]$method <- "Age-based analytic assessment (TSA)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2017,]$SSBmsy <- 44600
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2012,]$SSBmsy <- 22000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2012,]$SSBmsy_notes <- "Bpa"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2012,]$Fmsy <- 0.6
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2012,]$Fmsy_notes <- "Fpa"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2013,]$SSBmsy <- 22000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2013,]$SSBmsy_notes <- "Bpa"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2013,]$Fmsy <- 0.6
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2013,]$Fmsy_notes <- "Fpa"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2014,]$SSBmsy <- 22000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2014,]$SSBmsy_notes <- "Bpa"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2014,]$Fmsy <- 0.6
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2014,]$Fmsy_notes <- "Fpa"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2015,]$SSBmsy <- 39900
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2015,]$SSBmsy_notes <- "Bpa"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2015,]$Fmsy <- NA
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIa" & dd_ssb$report_year == 2015,]$Fmsy_notes <- NA

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIIek" & dd_ssb$report_year == 2017,]$method <- "Age-based analytical assessment (XSA)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="WHITVIIek" & dd_ssb$report_year == 2017,]$SSBmsy <- 35000

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="BLINGVb-VI-VII" & dd_ssb$report_year == 2014,]$method <- "Multi-Year Catch Curves (MYCC)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="BLINGVb-VI-VII" & dd_ssb$report_year == 2014,]$methodlong <- "A model fitted to age composition and total catch in order to estimate annual total mortality (Z)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="BLINGVb-VI-VII" & dd_ssb$report_year == 2016,]$method <- "Multi-Year Catch Curves (MYCC)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="BLINGVb-VI-VII" & dd_ssb$report_year == 2016,]$methodlong <- "A model fitted to age composition and total catch in order to estimate annual total mortality (Z)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="BLINGVb-VI-VII" & dd_ssb$report_year == 2016,]$SSBmsy <- 75000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="BLINGVb-VI-VII" & dd_ssb$report_year == 2018,]$method <- "Multi-Year Catch Curves (MYCC)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="BLINGVb-VI-VII" & dd_ssb$report_year == 2018,]$methodlong <- "A model fitted to age composition and total catch in order to estimate annual total mortality (Z)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="BLINGVb-VI-VII" & dd_ssb$report_year == 2018,]$SSBmsy <- 75000

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="ANCHOBAYB" & dd_ssb$report_year == 2015,]$method <- "Two-stage Bayesian biomass dynamic model (CBBM)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="ANCHOBAYB" & dd_ssb$report_year == 2016,]$method <- "Two-stage Bayesian biomass dynamic model (CBBM)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="ANCHOBAYB" & dd_ssb$report_year == 2017,]$method <- "Two-stage Bayesian biomass dynamic model (CBBM)"

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HMACKIXa" & dd_ssb$report_year == 2013,]$method <- "Analytical assessment (AMISH model)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HMACKIXa" & dd_ssb$report_year == 2014,]$method <- "Analytical assessment (AMISH model)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HMACKIXa" & dd_ssb$report_year == 2015,]$method <- "Analytical assessment (AMISH model)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HMACKIXa" & dd_ssb$report_year == 2016,]$method <- "Analytical assessment (AMISH model)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HMACKIXa" & dd_ssb$report_year == 2016,]$SSBmsy <- 181000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HMACKIXa" & dd_ssb$report_year == 2017,]$method <- "Analytical assessment (AMISH model)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="HMACKIXa" & dd_ssb$report_year == 2017,]$SSBmsy <- 181000

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAIC7d" & dd_ssb$report_year == 2015,]$method <- "Age-based analytical assessment"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAIC7d" & dd_ssb$report_year == 2015,]$SSBmsy <- 25800
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAIC7d" & dd_ssb$report_year == 2016,]$method <- "Age-based analytical assessment"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAIC7d" & dd_ssb$report_year == 2016,]$SSBmsy <- 25826
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAIC7d" & dd_ssb$report_year == 2017,]$method <- "Age-based analytical assessment"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAIC7d" & dd_ssb$report_year == 2017,]$SSBmsy <- 25826
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAIC7d" & dd_ssb$report_year == 2018,]$method <- "Age-based analytical assessment"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAIC7d" & dd_ssb$report_year == 2018,]$SSBmsy <- 25826

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAICNS" & dd_ssb$report_year == 2014,]$method <- "Age-based analytical assessment (XSA)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAICNS" & dd_ssb$report_year == 2014,]$SSBmsy <- 230000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAICNS" & dd_ssb$report_year == 2014,]$Fmsy <- 0.25
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAICNS" & dd_ssb$report_year == 2015,]$method <- "Age-based analytical assessment (XSA)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAICNS" & dd_ssb$report_year == 2015,]$SSBmsy <- 230000 
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAICNS" & dd_ssb$report_year == 2015,]$Fmsy <- 0.19
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAICNS" & dd_ssb$report_year == 2016,]$method <- "Age-based analytical assessment (XSA)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAICNS" & dd_ssb$report_year == 2016,]$SSBmsy <- 230000 
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAICNS" & dd_ssb$report_year == 2016,]$Fmsy <- 0.19
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAICNS" & dd_ssb$report_year == 2017,]$method <- "Age-structured stock assessment (based on Aarts and Poos 2009)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="PLAICNS" & dd_ssb$report_year == 2018,]$method <- "Age-structured stock assessment (based on Aarts and Poos 2009)"

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLENS" & dd_ssb$report_year == 2014,]$method <- "Age-based analytical assessment (XSA)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLENS" & dd_ssb$report_year == 2014,]$SSBmsy <- 35000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLENS" & dd_ssb$report_year == 2014,]$Fmsy <- 0.22
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLENS" & dd_ssb$report_year == 2016,]$method <- "Statistical catch-at-age model with flexible selectivity functions"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLENS" & dd_ssb$report_year == 2016,]$SSBmsy <- 37000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLENS" & dd_ssb$report_year == 2016,]$Fmsy <- 0.2

dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIId" & dd_ssb$report_year == 2011,]$method <- "Age-based analytical assessment (XSA)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIId" & dd_ssb$report_year == 2013,]$method <- "Age-based analytical assessment (XSA)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIId" & dd_ssb$report_year == 2014,]$method <- "Age-based analytical assessment (XSA)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIId" & dd_ssb$report_year == 2015,]$method <- "Age-based analytical assessment (XSA)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIId" & dd_ssb$report_year == 2015,]$SSBmsy <- 8000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIId" & dd_ssb$report_year == 2015,]$Fmsy <- 0.3
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIId" & dd_ssb$report_year == 2016,]$method <- "Age-based analytical assessment (XSA)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIId" & dd_ssb$report_year == 2016,]$SSBmsy <- 8000
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIId" & dd_ssb$report_year == 2016,]$Fmsy <- 0.3
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIId" & dd_ssb$report_year == 2017,]$method <- "Age-based analytical assessment (XSA)"
dd_ssb[dd_ssb$region=="Europe (EU)" & dd_ssb$stockid=="SOLEVIId" & dd_ssb$report_year == 2018,]$method <- "Age-based analytical assessment (XSA)"

# Europe (non-EU) stocks
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODNEARNCW",]$M <- 0.2
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODNEARNCW" & dd_ssb$assessid=="AFWG-CODNEARNCW-1982-2016-ICESIMP2016",]$years <- 1984-2015
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODNEARNCW" & dd_ssb$assessid=="AFWG-CODNEARNCW-1982-2016-ICESIMP2016",]$start_year <- 1984
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODNEARNCW" & dd_ssb$assessid=="AFWG-CODNEARNCW-1982-2016-ICESIMP2016",]$method <- "VPA"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODNEARNCW" & dd_ssb$assessid=="AFWG-CODNEARNCW-1982-2016-ICESIMP2016",]$methodlong <- "Based on survey SSB index and estimates of F and relative recruitment from an exploratory VPA assessment"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODNEARNCW" & dd_ssb$assessid=="AFWG-CODNEARNCW-1982-2016-ICESIMP2016",]$SSBmsy <- 60000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODNEARNCW" & dd_ssb$assessid=="AFWG-CODNEARNCW-1982-2016-ICESIMP2016",]$Fmsy <- 0.1
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODNEARNCW" & dd_ssb$assessid=="AFWG-CODNEARNCW-1984-2016-ICESIMP2018",]$method <- "VPA"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODNEARNCW" & dd_ssb$assessid=="AFWG-CODNEARNCW-1984-2016-ICESIMP2018",]$methodlong <- "Based on survey SSB index and estimates of F and relative recruitment from an exploratory VPA assessment"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODNEARNCW" & dd_ssb$assessid=="AFWG-CODNEARNCW-1984-2016-ICESIMP2018",]$SSBmsy <- 60000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODNEARNCW" & dd_ssb$assessid=="AFWG-CODNEARNCW-1984-2016-ICESIMP2018",]$Fmsy <- 0.1

dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2012,]$SSBmsy <- 220000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2012,]$Fmsy <- 0.35
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2014,]$SSBmsy <- 220000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2014,]$Fmsy <- 0.32
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2015,]$SSBmsy <- 220000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2015,]$Fmsy <- 0.32
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2016,]$SSBmsy <- 220000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2016,]$Fmsy <- 0.32
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2017,]$method <- "SAM"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2017,]$SSBmsy <- 220000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2017,]$Fmsy <- 0.32
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2018,]$method <- "SAM"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2018,]$SSBmsy <- 220000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLNEAR" & dd_ssb$report_year==2018,]$Fmsy <- 0.32

dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2013,]$method <- "Statistical catch at age model"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2013,]$SSBmsy <- 220000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2013,]$ERmsybest <- 0.2
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2015,]$method <- "Statistical catch at age model"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2015,]$SSBmsy <- 220000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2015,]$ERmsybest <- 0.2
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2016,]$method <- "Statistical catch at age model"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2016,]$SSBmsy <- 220000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2016,]$ERmsybest <- 0.2
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2017,]$method <- "Statistical catch at age model"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2017,]$SSBmsy <- 220000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2017,]$ERmsybest <- 0.2
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2018,]$method <- "Statistical catch at age model"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2018,]$SSBmsy <- 220000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="CODICE" & dd_ssb$report_year==2018,]$ERmsybest <- 0.2
 
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADFAPL" & dd_ssb$report_year==2014,]$method <- "XSA using landings-at-age data and age-disaggregated indices"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADFAPL" & dd_ssb$report_year==2014,]$SSBmsy <- 35000

dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADICE" & dd_ssb$report_year==2014,]$method <- "Adapt-type model (in ADMB)"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADICE" & dd_ssb$report_year==2015,]$method <- "Adapt-type model (in ADMB)"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADICE" & dd_ssb$report_year==2016,]$method <- "Adapt-type model (in ADMB)"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADICE" & dd_ssb$report_year==2012,]$Fmsy <- 0.35
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADICE" & dd_ssb$report_year==2014,]$ERmsybest <- 0.4
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADICE" & dd_ssb$report_year==2014,]$SSBmsy <- 45000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADICE" & dd_ssb$report_year==2015,]$ERmsybest <- 0.4
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADICE" & dd_ssb$report_year==2015,]$SSBmsy <- 45000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADICE" & dd_ssb$report_year==2016,]$ERmsybest <- 0.4
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADICE" & dd_ssb$report_year==2016,]$SSBmsy <- 45000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADICE" & dd_ssb$report_year==2017,]$ERmsybest <- 0.4
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HADICE" & dd_ssb$report_year==2017,]$SSBmsy <- 45000

dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HERRIsum",]$method <- "VPA"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HERRIsum" & dd_ssb$report_year==2016,]$method <- "VPA"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HERRIsum" & dd_ssb$report_year==2016,]$SSBmsy <- 273000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HERRIsum" & dd_ssb$report_year==2017,]$method <- "VPA"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HERRIsum" & dd_ssb$report_year==2017,]$SSBmsy <- 273000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HERRIsum" & dd_ssb$report_year==2018,]$method <- "VPA"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="HERRIsum" & dd_ssb$report_year==2018,]$SSBmsy <- 273000
 
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLIEG",]$method <- "Statistical catch at age model"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLIEG",]$Fmsy <- NA
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLIEG" & dd_ssb$report_year==2014,]$SSBmsy <- 65000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLIEG" & dd_ssb$report_year==2014,]$ERmsybest <- 0.2
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLIEG" & dd_ssb$report_year==2015,]$SSBmsy <- 65000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLIEG" & dd_ssb$report_year==2015,]$ERmsybest <- 0.2
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLIEG" & dd_ssb$report_year==2016,]$SSBmsy <- 65000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLIEG" & dd_ssb$report_year==2016,]$ERmsybest <- 0.2
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLIEG" & dd_ssb$report_year==2018,]$SSBmsy <- 65000
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="POLLIEG" & dd_ssb$report_year==2018,]$ERmsybest <- 0.2

dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="LINGVa",]$Fmsy <- NA
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="LINGVa" & dd_ssb$report_year==2017,]$method <- "Analytical length-based assessment (Gadget model)"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="LINGVa" & dd_ssb$report_year==2017,]$SSBmsy <- 9930
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="LINGVa" & dd_ssb$report_year==2017,]$ERmsybest <- 0.18
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="LINGVa" & dd_ssb$report_year==2018,]$method <- "Analytical length-based assessment (Gadget model)"
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="LINGVa" & dd_ssb$report_year==2018,]$SSBmsy <- 9930
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="LINGVa" & dd_ssb$report_year==2018,]$ERmsybest <- 0.18
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="LINGVa" & dd_ssb$report_year==2014,]$Fmsy <- 0.24
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="LINGVa" & dd_ssb$report_year==2015,]$Fmsy <- 0.24
dd_ssb[dd_ssb$region=="Europe (non-EU)" & dd_ssb$stockid=="LINGVa" & dd_ssb$report_year==2016,]$Fmsy <- 0.24

dd_new2 <- rbind(dd_ssb, dd_no_ssb)
write.csv(dd_new2, paste(outputdir, "0e_0_data_2plus_assess_with_records_add_PDFs.csv", sep="/"), row.names=F)

########################################################################################################################################

## Stocks with 2 assessments
data_2_assess <- dd_new2[dd_new2$stockid %in% stockid_2assess,]
length(unique(data_2_assess$stockid))  # 188 stocks
length(unique(data_2_assess$assessid))  # 376 assesses
data_2_assess$if_diff_5yrs <- NA
data_2_assess$interval <- NA
data_2_assess$check_ty_correct <- NA
for (i in 1:length(unique(data_2_assess$stockid))){
  data_2_assess[data_2_assess$stockid==unique(data_2_assess$stockid)[i],]$if_diff_5yrs <- ifelse(max(data_2_assess[data_2_assess$stockid==unique(data_2_assess$stockid)[i],]$terminal_year)-min(data_2_assess[data_2_assess$stockid==unique(data_2_assess$stockid)[i],]$terminal_year)>5, 
                                                                                 "Y", "N")
  data_2_assess[data_2_assess$stockid==unique(data_2_assess$stockid)[i],]$interval <- max(data_2_assess[data_2_assess$stockid==unique(data_2_assess$stockid)[i],]$terminal_year)-min(data_2_assess[data_2_assess$stockid==unique(data_2_assess$stockid)[i],]$terminal_year)
  data_2_assess[data_2_assess$stockid==unique(data_2_assess$stockid)[i],]$check_ty_correct <- ifelse(min(data_2_assess[data_2_assess$stockid==unique(data_2_assess$stockid)[i],]$terminal_year)==data_2_assess[data_2_assess$stockid==unique(data_2_assess$stockid)[i],]$ty, 
                                                                                                 "Y", "N")
}
table(data_2_assess$if_diff_5yrs)  # 53 stocks with 2 assessments with break > 5 years
table(data_2_assess$check_ty_correct)  # 6 rows need to corrected

data_2_assess[data_2_assess$check_ty_correct=="N", c(46:47,50:61,64:69)] <- NA
data_2_assess[data_2_assess$check_ty_correct=="N" & data_2_assess$stockid=="SEELNSSA4",]$ty <- 2016
data_2_assess[data_2_assess$check_ty_correct=="N" & data_2_assess$stockid=="SEELNSSA4",]$ty_minus1 <- 2015
data_2_assess[data_2_assess$check_ty_correct=="N" & data_2_assess$stockid=="HADIS",]$ty <- 2015
data_2_assess[data_2_assess$check_ty_correct=="N" & data_2_assess$stockid=="HADIS",]$ty_minus1 <- 2014
data_2_assess[data_2_assess$check_ty_correct=="N" & data_2_assess$stockid=="REDDEEPI-II",]$ty <- 2013
data_2_assess[data_2_assess$check_ty_correct=="N" & data_2_assess$stockid=="REDDEEPI-II",]$ty_minus1 <- 2012
data_2_assess[data_2_assess$check_ty_correct=="N",]$key_ty <- paste(data_2_assess[data_2_assess$check_ty_correct=="N",]$assessid, data_2_assess[data_2_assess$check_ty_correct=="N",]$ty, sep="_")
data_2_assess[data_2_assess$check_ty_correct=="N",]$key_ty_minus1 <- paste(data_2_assess[data_2_assess$check_ty_correct=="N",]$assessid, data_2_assess[data_2_assess$check_ty_correct=="N",]$ty_minus1, sep="_")

write.csv(data_2_assess, paste(outputdir, "0e_1_data_2_assess_with_records.csv", sep="/"), row.names=F)

########################################################################################################################################

## Stocks with 3 assessments
data_3_assess <- dd_new2[dd_new2$stockid %in% stockid_3assess,]
length(unique(data_3_assess$stockid))  # 13 stocks
length(unique(data_3_assess$assessid))  # 39 assesses

ids <- unique(data_3_assess$stockid)

# assess 1
data_3_s1 <- data_3_assess[data_3_assess$stockid==ids[1],]

data_3_s1_1 <- data_3_s1[data_3_s1$terminal_year == 2012 | data_3_s1$terminal_year == 2012,]
data_3_s1_1$ty_order <- 1
data_3_s1_1$if_diff_5yrs <- "N"
data_3_s1_1$interval <- 0

data_3_s1_2 <- data_3_s1[data_3_s1$assessid == "ICCAT-ATBTUNAWATL-1950-2015-PONS" | data_3_s1$terminal_year == 2015,]
data_3_s1_2$ty_order <- 2
data_3_s1_2$if_diff_5yrs <- "N"
data_3_s1_2$interval <- 3

data_3_s1_3 <- data_3_s1[data_3_s1$assessid=="ICCAT-ATBTUNAWATL-1970-2013-SISIMP2016" | data_3_s1$terminal_year == 2015,]
data_3_s1_3$ty_order <- 3
data_3_s1_3$if_diff_5yrs <- "N"
data_3_s1_3$interval <- 3

data_3_s1 <- rbind(data_3_s1_1, data_3_s1_2, data_3_s1_3)

# assess 2
data_3_s2 <- data_3_assess[data_3_assess$stockid==ids[2],]

data_3_s2_1 <- data_3_s2[data_3_s2$terminal_year == 2010 | data_3_s2$terminal_year == 2016,]
data_3_s2_1$ty_order <- 1
data_3_s2_1$if_diff_5yrs <- "Y"
data_3_s2_1$interval <- 6
  
data_3_s2_2 <- data_3_s2[data_3_s2$terminal_year == 2010 | data_3_s2$terminal_year == 2017,]
data_3_s2_2$ty_order <- 2
data_3_s2_2$if_diff_5yrs <- "Y"
data_3_s2_2$interval <- 7

data_3_s2_3 <- data_3_s2[data_3_s2$terminal_year == 2016 | data_3_s2$terminal_year == 2017,]
data_3_s2_3$ty_order <- 3
data_3_s2_3$if_diff_5yrs <- "N"
data_3_s2_3$interval <- 1
data_3_s2_3$ty <- 2016
data_3_s2_3$ty_minus1 <- 2015
data_3_s2_3$key_ty <- paste(data_3_s2_3$assessid, data_3_s2_3$ty, sep="_")
data_3_s2_3$key_ty_minus1 <- paste(data_3_s2_3$assessid, data_3_s2_3$ty_minus1, sep="_")
data_3_s2_3[,c(46:47,50:61,64:69)] <- NA

data_3_s2 <- rbind(data_3_s2_1, data_3_s2_2, data_3_s2_3)

# assess 3
data_3_s3 <- data_3_assess[data_3_assess$stockid==ids[3],]

data_3_s3_1 <- data_3_s3[data_3_s3$terminal_year == 2013 | data_3_s3$terminal_year == 2015,]
data_3_s3_1$ty_order <- 1
data_3_s3_1$if_diff_5yrs <- "N"
data_3_s3_1$interval <- 2

data_3_s3_2 <- data_3_s3[data_3_s3$terminal_year == 2013 | data_3_s3$terminal_year == 2017,]
data_3_s3_2$ty_order <- 2
data_3_s3_2$if_diff_5yrs <- "N"
data_3_s3_2$interval <- 4

data_3_s3_3 <- data_3_s3[data_3_s3$terminal_year == 2015 | data_3_s3$terminal_year == 2017,]
data_3_s3_3$ty_order <- 3
data_3_s3_3$if_diff_5yrs <- "N"
data_3_s3_3$interval <- 2
data_3_s3_3$ty <- 2015
data_3_s3_3$ty_minus1 <- 2014
data_3_s3_3$key_ty <- paste(data_3_s3_3$assessid, data_3_s3_3$ty, sep="_")
data_3_s3_3$key_ty_minus1 <- paste(data_3_s3_3$assessid, data_3_s3_3$ty_minus1, sep="_")
data_3_s3_3[,c(46:47,50:61,64:69)] <- NA

data_3_s3 <- rbind(data_3_s3_1, data_3_s3_2, data_3_s3_3)

# assess 4
data_3_s4 <- data_3_assess[data_3_assess$stockid==ids[4],]

data_3_s4_1 <- data_3_s4[data_3_s4$terminal_year == 2006 | data_3_s4$terminal_year == 2014,]
data_3_s4_1$ty_order <- 1
data_3_s4_1$if_diff_5yrs <- "Y"
data_3_s4_1$interval <- 8

data_3_s4_2 <- data_3_s4[data_3_s4$terminal_year == 2006 | data_3_s4$terminal_year == 2017,]
data_3_s4_2$ty_order <- 2
data_3_s4_2$if_diff_5yrs <- "Y"
data_3_s4_2$interval <- 11

data_3_s4_3 <- data_3_s4[data_3_s4$terminal_year == 2014 | data_3_s4$terminal_year == 2017,]
data_3_s4_3$ty_order <- 3
data_3_s4_3$if_diff_5yrs <- "N"
data_3_s4_3$interval <- 3
data_3_s4_3$ty <- 2014
data_3_s4_3$ty_minus1 <- 2013
data_3_s4_3$key_ty <- paste(data_3_s4_3$assessid, data_3_s4_3$ty, sep="_")
data_3_s4_3$key_ty_minus1 <- paste(data_3_s4_3$assessid, data_3_s4_3$ty_minus1, sep="_")
data_3_s4_3[,c(46:47,50:61,64:69)] <- NA

data_3_s4 <- rbind(data_3_s4_1, data_3_s4_2, data_3_s4_3)

# assess 5
data_3_s5 <- data_3_assess[data_3_assess$stockid==ids[5],]

data_3_s5_1 <- data_3_s5[data_3_s5$terminal_year == 2007 | data_3_s5$terminal_year == 2008,]
data_3_s5_1$ty_order <- 1
data_3_s5_1$if_diff_5yrs <- "N"
data_3_s5_1$interval <- 1

data_3_s5_2 <- data_3_s5[data_3_s5$terminal_year == 2007 | data_3_s5$terminal_year == 2016,]
data_3_s5_2$ty_order <- 2
data_3_s5_2$if_diff_5yrs <- "Y"
data_3_s5_2$interval <- 9

data_3_s5_3 <- data_3_s5[data_3_s5$terminal_year == 2008 | data_3_s5$terminal_year == 2016,]
data_3_s5_3$ty_order <- 3
data_3_s5_3$if_diff_5yrs <- "Y"
data_3_s5_3$interval <- 8
data_3_s5_3$ty <- 2008
data_3_s5_3$ty_minus1 <- 2007
data_3_s5_3$key_ty <- paste(data_3_s5_3$assessid, data_3_s5_3$ty, sep="_")
data_3_s5_3$key_ty_minus1 <- paste(data_3_s5_3$assessid, data_3_s5_3$ty_minus1, sep="_")
data_3_s5_3[,c(46:47,50:61,64:69)] <- NA

data_3_s5 <- rbind(data_3_s5_1, data_3_s5_2, data_3_s5_3)

# assess 6
data_3_s6 <- data_3_assess[data_3_assess$stockid==ids[6],]

data_3_s6_1 <- data_3_s6[data_3_s6$terminal_year == 2007 | data_3_s6$terminal_year == 2014,]
data_3_s6_1$ty_order <- 1
data_3_s6_1$if_diff_5yrs <- "Y"
data_3_s6_1$interval <- max(data_3_s6_1$terminal_year) - min(data_3_s6_1$terminal_year)

data_3_s6_2 <- data_3_s6[data_3_s6$terminal_year == 2007 | data_3_s6$terminal_year == 2017,]
data_3_s6_2$ty_order <- 2
data_3_s6_2$if_diff_5yrs <- "Y"
data_3_s6_2$interval <- max(data_3_s6_2$terminal_year) - min(data_3_s6_2$terminal_year)

data_3_s6_3 <- data_3_s6[data_3_s6$terminal_year == 2014 | data_3_s6$terminal_year == 2017,]
data_3_s6_3$ty_order <- 3
data_3_s6_3$if_diff_5yrs <- "N"
data_3_s6_3$interval <- max(data_3_s6_3$terminal_year) - min(data_3_s6_3$terminal_year)
data_3_s6_3$ty <- 2014
data_3_s6_3$ty_minus1 <- data_3_s6_3$ty-1
data_3_s6_3$key_ty <- paste(data_3_s6_3$assessid, data_3_s6_3$ty, sep="_")
data_3_s6_3$key_ty_minus1 <- paste(data_3_s6_3$assessid, data_3_s6_3$ty_minus1, sep="_")
data_3_s6_3[,c(46:47,50:61,64:69)] <- NA

data_3_s6 <- rbind(data_3_s6_1, data_3_s6_2, data_3_s6_3)

# assess 7
data_3_s7 <- data_3_assess[data_3_assess$stockid==ids[7],]

data_3_s7_1 <- data_3_s7[data_3_s7$terminal_year == 2007 | data_3_s7$terminal_year == 2012,]
data_3_s7_1$ty_order <- 1
data_3_s7_1$if_diff_5yrs <- "N"
data_3_s7_1$interval <- max(data_3_s7_1$terminal_year) - min(data_3_s7_1$terminal_year)

data_3_s7_2 <- data_3_s7[data_3_s7$terminal_year == 2007 | data_3_s7$terminal_year == 2013,]
data_3_s7_2$ty_order <- 2
data_3_s7_2$if_diff_5yrs <- "Y"
data_3_s7_2$interval <- max(data_3_s7_2$terminal_year) - min(data_3_s7_2$terminal_year)

data_3_s7_3 <- data_3_s7[data_3_s7$terminal_year == 2012 | data_3_s7$terminal_year == 2013,]
data_3_s7_3$ty_order <- 3
data_3_s7_3$if_diff_5yrs <- "N"
data_3_s7_3$interval <- max(data_3_s7_3$terminal_year) - min(data_3_s7_3$terminal_year)
data_3_s7_3$ty <- 2012
data_3_s7_3$ty_minus1 <- data_3_s7_3$ty-1
data_3_s7_3$key_ty <- paste(data_3_s7_3$assessid, data_3_s7_3$ty, sep="_")
data_3_s7_3$key_ty_minus1 <- paste(data_3_s7_3$assessid, data_3_s7_3$ty_minus1, sep="_")
data_3_s7_3[,c(46:47,50:61,64:69)] <- NA

data_3_s7 <- rbind(data_3_s7_1, data_3_s7_2, data_3_s7_3)

# assess 8
data_3_s8 <- data_3_assess[data_3_assess$stockid==ids[8],]

data_3_s8_1 <- data_3_s8[data_3_s8$terminal_year == 2010 | data_3_s8$terminal_year == 2012,]
data_3_s8_1$ty_order <- 1
data_3_s8_1$if_diff_5yrs <- "N"
data_3_s8_1$interval <- max(data_3_s8_1$terminal_year) - min(data_3_s8_1$terminal_year)

data_3_s8_2 <- data_3_s8[data_3_s8$terminal_year == 2010 | data_3_s8$terminal_year == 2013,]
data_3_s8_2$ty_order <- 2
data_3_s8_2$if_diff_5yrs <- "N"
data_3_s8_2$interval <- max(data_3_s8_2$terminal_year) - min(data_3_s8_2$terminal_year)

data_3_s8_3 <- data_3_s8[data_3_s8$terminal_year == 2012 | data_3_s8$terminal_year == 2013,]
data_3_s8_3$ty_order <- 3
data_3_s8_3$if_diff_5yrs <- "N"
data_3_s8_3$interval <- max(data_3_s8_3$terminal_year) - min(data_3_s8_3$terminal_year)
data_3_s8_3$ty <- 2012
data_3_s8_3$ty_minus1 <- data_3_s8_3$ty-1
data_3_s8_3$key_ty <- paste(data_3_s8_3$assessid, data_3_s8_3$ty, sep="_")
data_3_s8_3$key_ty_minus1 <- paste(data_3_s8_3$assessid, data_3_s8_3$ty_minus1, sep="_")
data_3_s8_3[,c(46:47,50:61,64:69)] <- NA

data_3_s8 <- rbind(data_3_s8_1, data_3_s8_2, data_3_s8_3)

# assess 9
data_3_s9 <- data_3_assess[data_3_assess$stockid==ids[9],]
data_3_s9$ty <- 2015
data_3_s9$ty_minus1 <- 2014
data_3_s9$key_ty <- paste(data_3_s9$assessid, data_3_s9$ty, sep="_")
data_3_s9$key_ty_minus1 <- paste(data_3_s9$assessid, data_3_s9$ty_minus1, sep="_")
data_3_s9[,c(46:47,50:61,64:69)] <- NA

data_3_s9_1 <- data_3_s9[data_3_s9$terminal_year == 2015 | data_3_s9$terminal_year == 2016,]
data_3_s9_1$ty_order <- 1
data_3_s9_1$if_diff_5yrs <- "N"
data_3_s9_1$interval <- max(data_3_s9_1$terminal_year) - min(data_3_s9_1$terminal_year)

data_3_s9_2 <- data_3_s9[data_3_s9$terminal_year == 2015 | data_3_s9$terminal_year == 2017,]
data_3_s9_2$ty_order <- 2
data_3_s9_2$if_diff_5yrs <- "N"
data_3_s9_2$interval <- max(data_3_s9_2$terminal_year) - min(data_3_s9_2$terminal_year)

data_3_s9_3 <- data_3_s9[data_3_s9$terminal_year == 2016 | data_3_s9$terminal_year == 2017,]
data_3_s9_3$ty_order <- 3
data_3_s9_3$if_diff_5yrs <- "N"
data_3_s9_3$interval <- max(data_3_s9_3$terminal_year) - min(data_3_s9_3$terminal_year)
data_3_s9_3$ty <- 2016
data_3_s9_3$ty_minus1 <- data_3_s9_3$ty-1
data_3_s9_3$key_ty <- paste(data_3_s9_3$assessid, data_3_s9_3$ty, sep="_")
data_3_s9_3$key_ty_minus1 <- paste(data_3_s9_3$assessid, data_3_s9_3$ty_minus1, sep="_")
data_3_s9_3[,c(46:47,50:61,64:69)] <- NA

data_3_s9 <- rbind(data_3_s9_1, data_3_s9_2, data_3_s9_3)

# assess 10
data_3_s10 <- data_3_assess[data_3_assess$stockid==ids[10],]

data_3_s10_1 <- data_3_s10[data_3_s10$terminal_year == 2010 | data_3_s10$terminal_year == 2012,]
data_3_s10_1$ty_order <- 1
data_3_s10_1$if_diff_5yrs <- "N"
data_3_s10_1$interval <- max(data_3_s10_1$terminal_year) - min(data_3_s10_1$terminal_year)

data_3_s10_2 <- data_3_s10[data_3_s10$terminal_year == 2010 | data_3_s10$terminal_year == 2013,]
data_3_s10_2$ty_order <- 2
data_3_s10_2$if_diff_5yrs <- "N"
data_3_s10_2$interval <- max(data_3_s10_2$terminal_year) - min(data_3_s10_2$terminal_year)

data_3_s10_3 <- data_3_s10[data_3_s10$terminal_year == 2012 | data_3_s10$terminal_year == 2013,]
data_3_s10_3$ty_order <- 3
data_3_s10_3$if_diff_5yrs <- "N"
data_3_s10_3$interval <- max(data_3_s10_3$terminal_year) - min(data_3_s10_3$terminal_year)
data_3_s10_3$ty <- 2012
data_3_s10_3$ty_minus1 <- data_3_s10_3$ty-1
data_3_s10_3$key_ty <- paste(data_3_s10_3$assessid, data_3_s10_3$ty, sep="_")
data_3_s10_3$key_ty_minus1 <- paste(data_3_s10_3$assessid, data_3_s10_3$ty_minus1, sep="_")
data_3_s10_3[,c(46:47,50:61,64:69)] <- NA

data_3_s10 <- rbind(data_3_s10_1, data_3_s10_2, data_3_s10_3)

# assess 11
data_3_s11 <- data_3_assess[data_3_assess$stockid==ids[11],]

data_3_s11_1 <- data_3_s11[data_3_s11$terminal_year == 2011 | data_3_s11$terminal_year == 2015,]
data_3_s11_1$ty_order <- 1
data_3_s11_1$if_diff_5yrs <- "N"
data_3_s11_1$interval <- max(data_3_s11_1$terminal_year) - min(data_3_s11_1$terminal_year)

data_3_s11_2 <- data_3_s11[data_3_s11$terminal_year == 2011 | data_3_s11$terminal_year == 2016,]
data_3_s11_2$ty_order <- 2
data_3_s11_2$if_diff_5yrs <- "N"
data_3_s11_2$interval <- max(data_3_s11_2$terminal_year) - min(data_3_s11_2$terminal_year)

data_3_s11_3 <- data_3_s11[data_3_s11$terminal_year == 2015 | data_3_s11$terminal_year == 2016,]
data_3_s11_3$ty_order <- 3
data_3_s11_3$if_diff_5yrs <- "N"
data_3_s11_3$interval <- max(data_3_s11_3$terminal_year) - min(data_3_s11_3$terminal_year)
data_3_s11_3$ty <- 2015
data_3_s11_3$ty_minus1 <- data_3_s11_3$ty-1
data_3_s11_3$key_ty <- paste(data_3_s11_3$assessid, data_3_s11_3$ty, sep="_")
data_3_s11_3$key_ty_minus1 <- paste(data_3_s11_3$assessid, data_3_s11_3$ty_minus1, sep="_")
data_3_s11_3[,c(46:47,50:61,64:69)] <- NA

data_3_s11 <- rbind(data_3_s11_1, data_3_s11_2, data_3_s11_3)

# assess 12
data_3_s12 <- data_3_assess[data_3_assess$stockid==ids[12],]

data_3_s12_1 <- data_3_s12[data_3_s12$terminal_year == 2007 | data_3_s12$terminal_year == 2015,]
data_3_s12_1$ty_order <- 1
data_3_s12_1$if_diff_5yrs <- "Y"
data_3_s12_1$interval <- max(data_3_s12_1$terminal_year) - min(data_3_s12_1$terminal_year)

data_3_s12_2 <- data_3_s12[data_3_s12$terminal_year == 2007 | data_3_s12$terminal_year == 2016,]
data_3_s12_2$ty_order <- 2
data_3_s12_2$if_diff_5yrs <- "Y"
data_3_s12_2$interval <- max(data_3_s12_2$terminal_year) - min(data_3_s12_2$terminal_year)

data_3_s12_3 <- data_3_s12[data_3_s12$terminal_year == 2015 | data_3_s12$terminal_year == 2016,]
data_3_s12_3$ty_order <- 3
data_3_s12_3$if_diff_5yrs <- "N"
data_3_s12_3$interval <- max(data_3_s12_3$terminal_year) - min(data_3_s12_3$terminal_year)
data_3_s12_3$ty <- 2015
data_3_s12_3$ty_minus1 <- data_3_s12_3$ty-1
data_3_s12_3$key_ty <- paste(data_3_s12_3$assessid, data_3_s12_3$ty, sep="_")
data_3_s12_3$key_ty_minus1 <- paste(data_3_s12_3$assessid, data_3_s12_3$ty_minus1, sep="_")
data_3_s12_3[,c(46:47,50:61,64:69)] <- NA

data_3_s12 <- rbind(data_3_s12_1, data_3_s12_2, data_3_s12_3)

# assess 13
data_3_s13 <- data_3_assess[data_3_assess$stockid==ids[13],]

data_3_s13_1 <- data_3_s13[data_3_s13$terminal_year == 2007 | data_3_s13$terminal_year == 2011,]
data_3_s13_1$ty_order <- 1
data_3_s13_1$if_diff_5yrs <- "N"
data_3_s13_1$interval <- max(data_3_s13_1$terminal_year) - min(data_3_s13_1$terminal_year)

data_3_s13_2 <- data_3_s13[data_3_s13$terminal_year == 2007 | data_3_s13$terminal_year == 2015,]
data_3_s13_2$ty_order <- 2
data_3_s13_2$if_diff_5yrs <- "Y"
data_3_s13_2$interval <- max(data_3_s13_2$terminal_year) - min(data_3_s13_2$terminal_year)

data_3_s13_3 <- data_3_s13[data_3_s13$terminal_year == 2011 | data_3_s13$terminal_year == 2015,]
data_3_s13_3$ty_order <- 3
data_3_s13_3$if_diff_5yrs <- "N"
data_3_s13_3$interval <- max(data_3_s13_3$terminal_year) - min(data_3_s13_3$terminal_year)
data_3_s13_3$ty <- 2011
data_3_s13_3$ty_minus1 <- data_3_s13_3$ty-1
data_3_s13_3$key_ty <- paste(data_3_s13_3$assessid, data_3_s13_3$ty, sep="_")
data_3_s13_3$key_ty_minus1 <- paste(data_3_s13_3$assessid, data_3_s13_3$ty_minus1, sep="_")
data_3_s13_3[,c(46:47,50:61,64:69)] <- NA

data_3_s13 <- rbind(data_3_s13_1, data_3_s13_2, data_3_s13_3)

data_3 <- rbind(data_3_s1, data_3_s2, data_3_s3, data_3_s4, data_3_s5,
                data_3_s6, data_3_s7, data_3_s8, data_3_s9, data_3_s10,
                data_3_s11, data_3_s12, data_3_s13)

write.csv(data_3, paste(outputdir, "0e_2_data_3_assess_with_records.csv", sep="/"), row.names=F)

########################################################################################################################################

## Stocks with 4 assessments
data_4_assess <- dd_new2[dd_new2$stockid %in% stockid_4assess,]
length(unique(data_4_assess$stockid))  # 4 stocks
length(unique(data_4_assess$assessid))  # 16 assesses

ids <- unique(data_4_assess$stockid)

# assess 1
data_4_s1 <- data_4_assess[data_4_assess$stockid==ids[1],]

data_4_s1_1 <- data_4_s1[data_4_s1$terminal_year == 2006 | data_4_s1$assessid=="WGHANSA-ANCHOBAYB-1986-2015-ICESIMP2016",]
data_4_s1_1$ty_order <- 1
data_4_s1_1$if_diff_5yrs <- "Y"
data_4_s1_1$interval <- max(data_4_s1_1$terminal_year) - min(data_4_s1_1$terminal_year)

data_4_s1_2 <- data_4_s1[data_4_s1$terminal_year == 2006 | data_4_s1$assessid=="WGHANSA-ANCHOBAYB-1986-2016-ICESIMP2018",]
data_4_s1_2$ty_order <- 2
data_4_s1_2$if_diff_5yrs <- "Y"
data_4_s1_2$interval <- max(data_4_s1_2$terminal_year) - min(data_4_s1_2$terminal_year)

data_4_s1_3 <- data_4_s1[data_4_s1$terminal_year == 2006 | data_4_s1$terminal_year == 2017,]
data_4_s1_3$ty_order <- 3
data_4_s1_3$if_diff_5yrs <- "Y"
data_4_s1_3$interval <- max(data_4_s1_3$terminal_year) - min(data_4_s1_3$terminal_year)

data_4_s1_4 <- data_4_s1[data_4_s1$assessid=="WGHANSA-ANCHOBAYB-1986-2015-ICESIMP2016" | data_4_s1$assessid=="WGHANSA-ANCHOBAYB-1986-2016-ICESIMP2018",]
data_4_s1_4$ty_order <- 4
data_4_s1_4$if_diff_5yrs <- "N"
data_4_s1_4$interval <- max(data_4_s1_4$terminal_year) - min(data_4_s1_4$terminal_year)
data_4_s1_4$ty <- 2015
data_4_s1_4$ty_minus1 <- data_4_s1_4$ty-1
data_4_s1_4$key_ty <- paste(data_4_s1_4$assessid, data_4_s1_4$ty, sep="_")
data_4_s1_4$key_ty_minus1 <- paste(data_4_s1_4$assessid, data_4_s1_4$ty_minus1, sep="_")
data_4_s1_4[,c(46:47,50:61,64:69)] <- NA

data_4_s1_5 <- data_4_s1[data_4_s1$assessid=="WGHANSA-ANCHOBAYB-1986-2015-ICESIMP2016" | data_4_s1$terminal_year == 2017,]
data_4_s1_5$ty_order <- 5
data_4_s1_5$if_diff_5yrs <- "N"
data_4_s1_5$interval <- max(data_4_s1_5$terminal_year) - min(data_4_s1_5$terminal_year)
data_4_s1_5$ty <- 2015
data_4_s1_5$ty_minus1 <- data_4_s1_5$ty-1
data_4_s1_5$key_ty <- paste(data_4_s1_5$assessid, data_4_s1_5$ty, sep="_")
data_4_s1_5$key_ty_minus1 <- paste(data_4_s1_5$assessid, data_4_s1_5$ty_minus1, sep="_")
data_4_s1_5[,c(46:47,50:61,64:69)] <- NA

data_4_s1_6 <- data_4_s1[data_4_s1$assessid=="WGHANSA-ANCHOBAYB-1986-2016-ICESIMP2018" | data_4_s1$terminal_year == 2017,]
data_4_s1_6$ty_order <- 6
data_4_s1_6$if_diff_5yrs <- "N"
data_4_s1_6$interval <- max(data_4_s1_6$terminal_year) - min(data_4_s1_6$terminal_year)
data_4_s1_6$ty <- 2015
data_4_s1_6$ty_minus1 <- data_4_s1_6$ty-1
data_4_s1_6$key_ty <- paste(data_4_s1_6$assessid, data_4_s1_6$ty, sep="_")
data_4_s1_6$key_ty_minus1 <- paste(data_4_s1_6$assessid, data_4_s1_6$ty_minus1, sep="_")
data_4_s1_6[,c(46:47,50:61,64:69)] <- NA

data_4_s1 <- rbind(data_4_s1_1, data_4_s1_2, data_4_s1_3, data_4_s1_4, data_4_s1_5, data_4_s1_6)

# assess 2
data_4_s2 <- data_4_assess[data_4_assess$stockid==ids[2],]

data_4_s2_1 <- data_4_s2[data_4_s2$terminal_year == 2010 | data_4_s2$terminal_year == 2012,]
data_4_s2_1$ty_order <- 1
data_4_s2_1$if_diff_5yrs <- "N"
data_4_s2_1$interval <- max(data_4_s2_1$terminal_year) - min(data_4_s2_1$terminal_year)

data_4_s2_2 <- data_4_s2[data_4_s2$terminal_year == 2010 | data_4_s2$terminal_year == 2013,]
data_4_s2_2$ty_order <- 2
data_4_s2_2$if_diff_5yrs <- "N"
data_4_s2_2$interval <- max(data_4_s2_2$terminal_year) - min(data_4_s2_2$terminal_year)

data_4_s2_3 <- data_4_s2[data_4_s2$terminal_year == 2010 | data_4_s2$terminal_year == 2017,]
data_4_s2_3$ty_order <- 3
data_4_s2_3$if_diff_5yrs <- "Y"
data_4_s2_3$interval <- max(data_4_s2_3$terminal_year) - min(data_4_s2_3$terminal_year)

data_4_s2_4 <- data_4_s2[data_4_s2$terminal_year == 2012 | data_4_s2$terminal_year == 2013,]
data_4_s2_4$ty_order <- 4
data_4_s2_4$if_diff_5yrs <- "N"
data_4_s2_4$interval <- max(data_4_s2_4$terminal_year) - min(data_4_s2_4$terminal_year)
data_4_s2_4$ty <- 2012
data_4_s2_4$ty_minus1 <- data_4_s2_4$ty-1
data_4_s2_4$key_ty <- paste(data_4_s2_4$assessid, data_4_s2_4$ty, sep="_")
data_4_s2_4$key_ty_minus1 <- paste(data_4_s2_4$assessid, data_4_s2_4$ty_minus1, sep="_")
data_4_s2_4[,c(46:47,50:61,64:69)] <- NA

data_4_s2_5 <- data_4_s2[data_4_s2$terminal_year == 2012 | data_4_s2$terminal_year == 2017,]
data_4_s2_5$ty_order <- 5
data_4_s2_5$if_diff_5yrs <- "N"
data_4_s2_5$interval <- max(data_4_s2_5$terminal_year) - min(data_4_s2_5$terminal_year)
data_4_s2_5$ty <- 2012
data_4_s2_5$ty_minus1 <- data_4_s2_5$ty-1
data_4_s2_5$key_ty <- paste(data_4_s2_5$assessid, data_4_s2_5$ty, sep="_")
data_4_s2_5$key_ty_minus1 <- paste(data_4_s2_5$assessid, data_4_s2_5$ty_minus1, sep="_")
data_4_s2_5[,c(46:47,50:61,64:69)] <- NA

data_4_s2_6 <- data_4_s2[data_4_s2$terminal_year == 2013 | data_4_s2$terminal_year == 2017,]
data_4_s2_6$ty_order <- 6
data_4_s2_6$if_diff_5yrs <- "N"
data_4_s2_6$interval <- max(data_4_s2_6$terminal_year) - min(data_4_s2_6$terminal_year)
data_4_s2_6$ty <- 2013
data_4_s2_6$ty_minus1 <- data_4_s2_6$ty-1
data_4_s2_6$key_ty <- paste(data_4_s2_6$assessid, data_4_s2_6$ty, sep="_")
data_4_s2_6$key_ty_minus1 <- paste(data_4_s2_6$assessid, data_4_s2_6$ty_minus1, sep="_")
data_4_s2_6[,c(46:47,50:61,64:69)] <- NA

data_4_s2 <- rbind(data_4_s2_1, data_4_s2_2, data_4_s2_3, data_4_s2_4, data_4_s2_5, data_4_s2_6)

# assess 3
data_4_s3 <- data_4_assess[data_4_assess$stockid==ids[3],]
data_4_s3$ty <- 2014
data_4_s3$ty_minus1 <- data_4_s3$ty-1
data_4_s3$key_ty <- paste(data_4_s3$assessid, data_4_s3$ty, sep="_")
data_4_s3$key_ty_minus1 <- paste(data_4_s3$assessid, data_4_s3$ty_minus1, sep="_")
data_4_s3[,c(46:47,50:61,64:69)] <- NA

data_4_s3_1 <- data_4_s3[data_4_s3$terminal_year == 2014 | data_4_s3$terminal_year == 2015,]
data_4_s3_1$ty_order <- 1
data_4_s3_1$if_diff_5yrs <- "N"
data_4_s3_1$interval <- max(data_4_s3_1$terminal_year) - min(data_4_s3_1$terminal_year)

data_4_s3_2 <- data_4_s3[data_4_s3$terminal_year == 2014 | data_4_s3$terminal_year == 2016,]
data_4_s3_2$ty_order <- 2
data_4_s3_2$if_diff_5yrs <- "N"
data_4_s3_2$interval <- max(data_4_s3_2$terminal_year) - min(data_4_s3_2$terminal_year)

data_4_s3_3 <- data_4_s3[data_4_s3$terminal_year == 2014 | data_4_s3$terminal_year == 2017,]
data_4_s3_3$ty_order <- 3
data_4_s3_3$if_diff_5yrs <- "N"
data_4_s3_3$interval <- max(data_4_s3_3$terminal_year) - min(data_4_s3_3$terminal_year)

data_4_s3_4 <- data_4_s3[data_4_s3$terminal_year == 2015 | data_4_s3$terminal_year == 2016,]
data_4_s3_4$ty_order <- 4
data_4_s3_4$if_diff_5yrs <- "N"
data_4_s3_4$interval <- max(data_4_s3_4$terminal_year) - min(data_4_s3_4$terminal_year)
data_4_s3_4$ty <- 2015
data_4_s3_4$ty_minus1 <- data_4_s3_4$ty-1
data_4_s3_4$key_ty <- paste(data_4_s3_4$assessid, data_4_s3_4$ty, sep="_")
data_4_s3_4$key_ty_minus1 <- paste(data_4_s3_4$assessid, data_4_s3_4$ty_minus1, sep="_")
data_4_s3_4[,c(46:47,50:61,64:69)] <- NA

data_4_s3_5 <- data_4_s3[data_4_s3$terminal_year == 2015 | data_4_s3$terminal_year == 2017,]
data_4_s3_5$ty_order <- 5
data_4_s3_5$if_diff_5yrs <- "N"
data_4_s3_5$interval <- max(data_4_s3_5$terminal_year) - min(data_4_s3_5$terminal_year)
data_4_s3_5$ty <- 2015
data_4_s3_5$ty_minus1 <- data_4_s3_5$ty-1
data_4_s3_5$key_ty <- paste(data_4_s3_5$assessid, data_4_s3_5$ty, sep="_")
data_4_s3_5$key_ty_minus1 <- paste(data_4_s3_5$assessid, data_4_s3_5$ty_minus1, sep="_")
data_4_s3_5[,c(46:47,50:61,64:69)] <- NA

data_4_s3_6 <- data_4_s3[data_4_s3$terminal_year == 2016 | data_4_s3$terminal_year == 2017,]
data_4_s3_6$ty_order <- 6
data_4_s3_6$if_diff_5yrs <- "N"
data_4_s3_6$interval <- max(data_4_s3_6$terminal_year) - min(data_4_s3_6$terminal_year)
data_4_s3_6$ty <- 2016
data_4_s3_6$ty_minus1 <- data_4_s3_6$ty-1
data_4_s3_6$key_ty <- paste(data_4_s3_6$assessid, data_4_s3_6$ty, sep="_")
data_4_s3_6$key_ty_minus1 <- paste(data_4_s3_6$assessid, data_4_s3_6$ty_minus1, sep="_")
data_4_s3_6[,c(46:47,50:61,64:69)] <- NA

data_4_s3 <- rbind(data_4_s3_1, data_4_s3_2, data_4_s3_3, data_4_s3_4, data_4_s3_5, data_4_s3_6)

# assess 4
data_4_s4 <- data_4_assess[data_4_assess$stockid==ids[4],]
data_4_s4$ty <- 2013
data_4_s4$ty_minus1 <- data_4_s4$ty-1
data_4_s4$key_ty <- paste(data_4_s4$assessid, data_4_s4$ty, sep="_")
data_4_s4$key_ty_minus1 <- paste(data_4_s4$assessid, data_4_s4$ty_minus1, sep="_")
data_4_s4[,c(46:47,50:61,64:69)] <- NA

data_4_s4_1 <- data_4_s4[data_4_s4$terminal_year == 2013 | data_4_s4$terminal_year == 2014,]
data_4_s4_1$ty_order <- 1
data_4_s4_1$if_diff_5yrs <- "N"
data_4_s4_1$interval <- max(data_4_s4_1$terminal_year) - min(data_4_s4_1$terminal_year)

data_4_s4_2 <- data_4_s4[data_4_s4$terminal_year == 2013 | data_4_s4$terminal_year == 2015,]
data_4_s4_2$ty_order <- 2
data_4_s4_2$if_diff_5yrs <- "N"
data_4_s4_2$interval <- max(data_4_s4_2$terminal_year) - min(data_4_s4_2$terminal_year)

data_4_s4_3 <- data_4_s4[data_4_s4$terminal_year == 2013 | data_4_s4$terminal_year == 2016,]
data_4_s4_3$ty_order <- 3
data_4_s4_3$if_diff_5yrs <- "N"
data_4_s4_3$interval <- max(data_4_s4_3$terminal_year) - min(data_4_s4_3$terminal_year)

data_4_s4_4 <- data_4_s4[data_4_s4$terminal_year == 2014 | data_4_s4$terminal_year == 2015,]
data_4_s4_4$ty_order <- 4
data_4_s4_4$if_diff_5yrs <- "N"
data_4_s4_4$interval <- max(data_4_s4_4$terminal_year) - min(data_4_s4_4$terminal_year)
data_4_s4_4$ty <- 2014
data_4_s4_4$ty_minus1 <- data_4_s4_4$ty-1
data_4_s4_4$key_ty <- paste(data_4_s4_4$assessid, data_4_s4_4$ty, sep="_")
data_4_s4_4$key_ty_minus1 <- paste(data_4_s4_4$assessid, data_4_s4_4$ty_minus1, sep="_")
data_4_s4_4[,c(46:47,50:61,64:69)] <- NA

data_4_s4_5 <- data_4_s4[data_4_s4$terminal_year == 2014 | data_4_s4$terminal_year == 2016,]
data_4_s4_5$ty_order <- 5
data_4_s4_5$if_diff_5yrs <- "N"
data_4_s4_5$interval <- max(data_4_s4_5$terminal_year) - min(data_4_s4_5$terminal_year)
data_4_s4_5$ty <- 2014
data_4_s4_5$ty_minus1 <- data_4_s4_5$ty-1
data_4_s4_5$key_ty <- paste(data_4_s4_5$assessid, data_4_s4_5$ty, sep="_")
data_4_s4_5$key_ty_minus1 <- paste(data_4_s4_5$assessid, data_4_s4_5$ty_minus1, sep="_")
data_4_s4_5[,c(46:47,50:61,64:69)] <- NA

data_4_s4_6 <- data_4_s4[data_4_s4$terminal_year == 2015 | data_4_s4$terminal_year == 2016,]
data_4_s4_6$ty_order <- 6
data_4_s4_6$if_diff_5yrs <- "N"
data_4_s4_6$interval <- max(data_4_s4_6$terminal_year) - min(data_4_s4_6$terminal_year)
data_4_s4_6$ty <- 2015
data_4_s4_6$ty_minus1 <- data_4_s4_6$ty-1
data_4_s4_6$key_ty <- paste(data_4_s4_6$assessid, data_4_s4_6$ty, sep="_")
data_4_s4_6$key_ty_minus1 <- paste(data_4_s4_6$assessid, data_4_s4_6$ty_minus1, sep="_")
data_4_s4_6[,c(46:47,50:61,64:69)] <- NA

data_4_s4 <- rbind(data_4_s4_1, data_4_s4_2, data_4_s4_3, data_4_s4_4, data_4_s4_5, data_4_s4_6)

data_4 <- rbind(data_4_s1, data_4_s2, data_4_s3, data_4_s4)

write.csv(data_4, paste(outputdir, "0e_3_data_4_assess_with_records.csv", sep="/"), row.names=F)

########################################################################################################################################

## Stocks with 5 assessments
data_5_assess <- dd_new2[dd_new2$stockid %in% stockid_5assess,]
length(unique(data_5_assess$stockid))  # 8 stocks
length(unique(data_5_assess$assessid))  # 40 assesses

ids <- unique(data_5_assess$stockid)

# assess 1
data_5_s1 <- data_5_assess[data_5_assess$stockid==ids[1],]

data_5_s1_1 <- data_5_s1[data_5_s1$terminal_year == 2013 | data_5_s1$terminal_year == 2014,]
data_5_s1_1$ty_order <- 1
data_5_s1_1$if_diff_5yrs <- "N"
data_5_s1_1$interval <- max(data_5_s1_1$terminal_year) - min(data_5_s1_1$terminal_year)

data_5_s1_2 <- data_5_s1[data_5_s1$terminal_year == 2013 | data_5_s1$terminal_year == 2015,]
data_5_s1_2$ty_order <- 2
data_5_s1_2$if_diff_5yrs <- "N"
data_5_s1_2$interval <- max(data_5_s1_2$terminal_year) - min(data_5_s1_2$terminal_year)

data_5_s1_3 <- data_5_s1[data_5_s1$terminal_year == 2013 | data_5_s1$terminal_year == 2016,]
data_5_s1_3$ty_order <- 3
data_5_s1_3$if_diff_5yrs <- "N"
data_5_s1_3$interval <- max(data_5_s1_3$terminal_year) - min(data_5_s1_3$terminal_year)

data_5_s1_4 <- data_5_s1[data_5_s1$terminal_year == 2013 | data_5_s1$terminal_year == 2017,]
data_5_s1_4$ty_order <- 4
data_5_s1_4$if_diff_5yrs <- "N"
data_5_s1_4$interval <- max(data_5_s1_4$terminal_year) - min(data_5_s1_4$terminal_year)

data_5_s1_5 <- data_5_s1[data_5_s1$terminal_year == 2014 | data_5_s1$terminal_year == 2015,]
data_5_s1_5$ty_order <- 5
data_5_s1_5$if_diff_5yrs <- "N"
data_5_s1_5$interval <- max(data_5_s1_5$terminal_year) - min(data_5_s1_5$terminal_year)
data_5_s1_5$ty <- 2014
data_5_s1_5$ty_minus1 <- data_5_s1_5$ty-1
data_5_s1_5$key_ty <- paste(data_5_s1_5$assessid, data_5_s1_5$ty, sep="_")
data_5_s1_5$key_ty_minus1 <- paste(data_5_s1_5$assessid, data_5_s1_5$ty_minus1, sep="_")
data_5_s1_5[,c(46:47,50:61,64:69)] <- NA

data_5_s1_6 <- data_5_s1[data_5_s1$terminal_year == 2014 | data_5_s1$terminal_year == 2016,]
data_5_s1_6$ty_order <- 6
data_5_s1_6$if_diff_5yrs <- "N"
data_5_s1_6$interval <- max(data_5_s1_6$terminal_year) - min(data_5_s1_6$terminal_year)
data_5_s1_6$ty <- 2014
data_5_s1_6$ty_minus1 <- data_5_s1_6$ty-1
data_5_s1_6$key_ty <- paste(data_5_s1_6$assessid, data_5_s1_6$ty, sep="_")
data_5_s1_6$key_ty_minus1 <- paste(data_5_s1_6$assessid, data_5_s1_6$ty_minus1, sep="_")
data_5_s1_6[,c(46:47,50:61,64:69)] <- NA

data_5_s1_7 <- data_5_s1[data_5_s1$terminal_year == 2014 | data_5_s1$terminal_year == 2017,]
data_5_s1_7$ty_order <- 7
data_5_s1_7$if_diff_5yrs <- "N"
data_5_s1_7$interval <- max(data_5_s1_7$terminal_year) - min(data_5_s1_7$terminal_year)
data_5_s1_7$ty <- 2014
data_5_s1_7$ty_minus1 <- data_5_s1_7$ty-1
data_5_s1_7$key_ty <- paste(data_5_s1_7$assessid, data_5_s1_7$ty, sep="_")
data_5_s1_7$key_ty_minus1 <- paste(data_5_s1_7$assessid, data_5_s1_7$ty_minus1, sep="_")
data_5_s1_7[,c(46:47,50:61,64:69)] <- NA

data_5_s1_8 <- data_5_s1[data_5_s1$terminal_year == 2015 | data_5_s1$terminal_year == 2016,]
data_5_s1_8$ty_order <- 8
data_5_s1_8$if_diff_5yrs <- "N"
data_5_s1_8$interval <- max(data_5_s1_8$terminal_year) - min(data_5_s1_8$terminal_year)
data_5_s1_8$ty <- 2015
data_5_s1_8$ty_minus1 <- data_5_s1_8$ty-1
data_5_s1_8$key_ty <- paste(data_5_s1_8$assessid, data_5_s1_8$ty, sep="_")
data_5_s1_8$key_ty_minus1 <- paste(data_5_s1_8$assessid, data_5_s1_8$ty_minus1, sep="_")
data_5_s1_8[,c(46:47,50:61,64:69)] <- NA

data_5_s1_9 <- data_5_s1[data_5_s1$terminal_year == 2015 | data_5_s1$terminal_year == 2017,]
data_5_s1_9$ty_order <- 9
data_5_s1_9$if_diff_5yrs <- "N"
data_5_s1_9$interval <- max(data_5_s1_9$terminal_year) - min(data_5_s1_9$terminal_year)
data_5_s1_9$ty <- 2015
data_5_s1_9$ty_minus1 <- data_5_s1_9$ty-1
data_5_s1_9$key_ty <- paste(data_5_s1_9$assessid, data_5_s1_9$ty, sep="_")
data_5_s1_9$key_ty_minus1 <- paste(data_5_s1_9$assessid, data_5_s1_9$ty_minus1, sep="_")
data_5_s1_9[,c(46:47,50:61,64:69)] <- NA

data_5_s1_10 <- data_5_s1[data_5_s1$terminal_year == 2016 | data_5_s1$terminal_year == 2017,]
data_5_s1_10$ty_order <- 10
data_5_s1_10$if_diff_5yrs <- "N"
data_5_s1_10$interval <- max(data_5_s1_10$terminal_year) - min(data_5_s1_10$terminal_year)
data_5_s1_10$ty <- 2016
data_5_s1_10$ty_minus1 <- data_5_s1_10$ty-1
data_5_s1_10$key_ty <- paste(data_5_s1_10$assessid, data_5_s1_10$ty, sep="_")
data_5_s1_10$key_ty_minus1 <- paste(data_5_s1_10$assessid, data_5_s1_10$ty_minus1, sep="_")
data_5_s1_10[,c(46:47,50:61,64:69)] <- NA

data_5_s1 <- rbind(data_5_s1_1, data_5_s1_2, data_5_s1_3, data_5_s1_4, data_5_s1_5, data_5_s1_6, data_5_s1_7, data_5_s1_8, data_5_s1_9, data_5_s1_10)

# assess 2
data_5_s2 <- data_5_assess[data_5_assess$stockid==ids[2],]

data_5_s2_1 <- data_5_s2[data_5_s2$terminal_year == 2012 | data_5_s2$terminal_year == 2013,]
data_5_s2_1$ty_order <- 1
data_5_s2_1$if_diff_5yrs <- "N"
data_5_s2_1$interval <- max(data_5_s2_1$terminal_year) - min(data_5_s2_1$terminal_year)

data_5_s2_2 <- data_5_s2[data_5_s2$terminal_year == 2012 | data_5_s2$terminal_year == 2014,]
data_5_s2_2$ty_order <- 2
data_5_s2_2$if_diff_5yrs <- "N"
data_5_s2_2$interval <- max(data_5_s2_2$terminal_year) - min(data_5_s2_2$terminal_year)

data_5_s2_3 <- data_5_s2[data_5_s2$terminal_year == 2012 | data_5_s2$terminal_year == 2015,]
data_5_s2_3$ty_order <- 3
data_5_s2_3$if_diff_5yrs <- "N"
data_5_s2_3$interval <- max(data_5_s2_3$terminal_year) - min(data_5_s2_3$terminal_year)

data_5_s2_4 <- data_5_s2[data_5_s2$terminal_year == 2012 | data_5_s2$terminal_year == 2016,]
data_5_s2_4$ty_order <- 4
data_5_s2_4$if_diff_5yrs <- "N"
data_5_s2_4$interval <- max(data_5_s2_4$terminal_year) - min(data_5_s2_4$terminal_year)

data_5_s2_5 <- data_5_s2[data_5_s2$terminal_year == 2013 | data_5_s2$terminal_year == 2014,]
data_5_s2_5$ty_order <- 5
data_5_s2_5$if_diff_5yrs <- "N"
data_5_s2_5$interval <- max(data_5_s2_5$terminal_year) - min(data_5_s2_5$terminal_year)
data_5_s2_5$ty <- 2013
data_5_s2_5$ty_minus1 <- data_5_s2_5$ty-1
data_5_s2_5$key_ty <- paste(data_5_s2_5$assessid, data_5_s2_5$ty, sep="_")
data_5_s2_5$key_ty_minus1 <- paste(data_5_s2_5$assessid, data_5_s2_5$ty_minus1, sep="_")
data_5_s2_5[,c(46:47,50:61,64:69)] <- NA

data_5_s2_6 <- data_5_s2[data_5_s2$terminal_year == 2013 | data_5_s2$terminal_year == 2015,]
data_5_s2_6$ty_order <- 6
data_5_s2_6$if_diff_5yrs <- "N"
data_5_s2_6$interval <- max(data_5_s2_6$terminal_year) - min(data_5_s2_6$terminal_year)
data_5_s2_6$ty <- 2013
data_5_s2_6$ty_minus1 <- data_5_s2_6$ty-1
data_5_s2_6$key_ty <- paste(data_5_s2_6$assessid, data_5_s2_6$ty, sep="_")
data_5_s2_6$key_ty_minus1 <- paste(data_5_s2_6$assessid, data_5_s2_6$ty_minus1, sep="_")
data_5_s2_6[,c(46:47,50:61,64:69)] <- NA

data_5_s2_7 <- data_5_s2[data_5_s2$terminal_year == 2013 | data_5_s2$terminal_year == 2016,]
data_5_s2_7$ty_order <- 7
data_5_s2_7$if_diff_5yrs <- "N"
data_5_s2_7$interval <- max(data_5_s2_7$terminal_year) - min(data_5_s2_7$terminal_year)
data_5_s2_7$ty <- 2013
data_5_s2_7$ty_minus1 <- data_5_s2_7$ty-1
data_5_s2_7$key_ty <- paste(data_5_s2_7$assessid, data_5_s2_7$ty, sep="_")
data_5_s2_7$key_ty_minus1 <- paste(data_5_s2_7$assessid, data_5_s2_7$ty_minus1, sep="_")
data_5_s2_7[,c(46:47,50:61,64:69)] <- NA

data_5_s2_8 <- data_5_s2[data_5_s2$terminal_year == 2014 | data_5_s2$terminal_year == 2015,]
data_5_s2_8$ty_order <- 8
data_5_s2_8$if_diff_5yrs <- "N"
data_5_s2_8$interval <- max(data_5_s2_8$terminal_year) - min(data_5_s2_8$terminal_year)
data_5_s2_8$ty <- 2014
data_5_s2_8$ty_minus1 <- data_5_s2_8$ty-1
data_5_s2_8$key_ty <- paste(data_5_s2_8$assessid, data_5_s2_8$ty, sep="_")
data_5_s2_8$key_ty_minus1 <- paste(data_5_s2_8$assessid, data_5_s2_8$ty_minus1, sep="_")
data_5_s2_8[,c(46:47,50:61,64:69)] <- NA

data_5_s2_9 <- data_5_s2[data_5_s2$terminal_year == 2014 | data_5_s2$terminal_year == 2016,]
data_5_s2_9$ty_order <- 9
data_5_s2_9$if_diff_5yrs <- "N"
data_5_s2_9$interval <- max(data_5_s2_9$terminal_year) - min(data_5_s2_9$terminal_year)
data_5_s2_9$ty <- 2014
data_5_s2_9$ty_minus1 <- data_5_s2_9$ty-1
data_5_s2_9$key_ty <- paste(data_5_s2_9$assessid, data_5_s2_9$ty, sep="_")
data_5_s2_9$key_ty_minus1 <- paste(data_5_s2_9$assessid, data_5_s2_9$ty_minus1, sep="_")
data_5_s2_9[,c(46:47,50:61,64:69)] <- NA

data_5_s2_10 <- data_5_s2[data_5_s2$terminal_year == 2015 | data_5_s2$terminal_year == 2016,]
data_5_s2_10$ty_order <- 10
data_5_s2_10$if_diff_5yrs <- "N"
data_5_s2_10$interval <- max(data_5_s2_10$terminal_year) - min(data_5_s2_10$terminal_year)
data_5_s2_10$ty <- 2015
data_5_s2_10$ty_minus1 <- data_5_s2_10$ty-1
data_5_s2_10$key_ty <- paste(data_5_s2_10$assessid, data_5_s2_10$ty, sep="_")
data_5_s2_10$key_ty_minus1 <- paste(data_5_s2_10$assessid, data_5_s2_10$ty_minus1, sep="_")
data_5_s2_10[,c(46:47,50:61,64:69)] <- NA

data_5_s2 <- rbind(data_5_s2_1, data_5_s2_2, data_5_s2_3, data_5_s2_4, data_5_s2_5, data_5_s2_6, data_5_s2_7, data_5_s2_8, data_5_s2_9, data_5_s2_10)

# assess 3
data_5_s3 <- data_5_assess[data_5_assess$stockid==ids[3],]

data_5_s3_1 <- data_5_s3[data_5_s3$terminal_year == 2010 | data_5_s3$terminal_year == 2014,]
data_5_s3_1$ty_order <- 1
data_5_s3_1$if_diff_5yrs <- "N"
data_5_s3_1$interval <- max(data_5_s3_1$terminal_year) - min(data_5_s3_1$terminal_year)

data_5_s3_2 <- data_5_s3[data_5_s3$terminal_year == 2010 | data_5_s3$terminal_year == 2015,]
data_5_s3_2$ty_order <- 2
data_5_s3_2$if_diff_5yrs <- "N"
data_5_s3_2$interval <- max(data_5_s3_2$terminal_year) - min(data_5_s3_2$terminal_year)

data_5_s3_3 <- data_5_s3[data_5_s3$terminal_year == 2010 | data_5_s3$terminal_year == 2016,]
data_5_s3_3$ty_order <- 3
data_5_s3_3$if_diff_5yrs <- "Y"
data_5_s3_3$interval <- max(data_5_s3_3$terminal_year) - min(data_5_s3_3$terminal_year)

data_5_s3_4 <- data_5_s3[data_5_s3$terminal_year == 2010 | data_5_s3$terminal_year == 2017,]
data_5_s3_4$ty_order <- 4
data_5_s3_4$if_diff_5yrs <- "Y"
data_5_s3_4$interval <- max(data_5_s3_4$terminal_year) - min(data_5_s3_4$terminal_year)

data_5_s3_5 <- data_5_s3[data_5_s3$terminal_year == 2014 | data_5_s3$terminal_year == 2015,]
data_5_s3_5$ty_order <- 5
data_5_s3_5$if_diff_5yrs <- "N"
data_5_s3_5$interval <- max(data_5_s3_5$terminal_year) - min(data_5_s3_5$terminal_year)
data_5_s3_5$ty <- 2014
data_5_s3_5$ty_minus1 <- data_5_s3_5$ty-1
data_5_s3_5$key_ty <- paste(data_5_s3_5$assessid, data_5_s3_5$ty, sep="_")
data_5_s3_5$key_ty_minus1 <- paste(data_5_s3_5$assessid, data_5_s3_5$ty_minus1, sep="_")
data_5_s3_5[,c(46:47,50:61,64:69)] <- NA

data_5_s3_6 <- data_5_s3[data_5_s3$terminal_year == 2014 | data_5_s3$terminal_year == 2016,]
data_5_s3_6$ty_order <- 6
data_5_s3_6$if_diff_5yrs <- "N"
data_5_s3_6$interval <- max(data_5_s3_6$terminal_year) - min(data_5_s3_6$terminal_year)
data_5_s3_6$ty <- 2014
data_5_s3_6$ty_minus1 <- data_5_s3_6$ty-1
data_5_s3_6$key_ty <- paste(data_5_s3_6$assessid, data_5_s3_6$ty, sep="_")
data_5_s3_6$key_ty_minus1 <- paste(data_5_s3_6$assessid, data_5_s3_6$ty_minus1, sep="_")
data_5_s3_6[,c(46:47,50:61,64:69)] <- NA

data_5_s3_7 <- data_5_s3[data_5_s3$terminal_year == 2014 | data_5_s3$terminal_year == 2017,]
data_5_s3_7$ty_order <- 7
data_5_s3_7$if_diff_5yrs <- "N"
data_5_s3_7$interval <- max(data_5_s3_7$terminal_year) - min(data_5_s3_7$terminal_year)
data_5_s3_7$ty <- 2014
data_5_s3_7$ty_minus1 <- data_5_s3_7$ty-1
data_5_s3_7$key_ty <- paste(data_5_s3_7$assessid, data_5_s3_7$ty, sep="_")
data_5_s3_7$key_ty_minus1 <- paste(data_5_s3_7$assessid, data_5_s3_7$ty_minus1, sep="_")
data_5_s3_7[,c(46:47,50:61,64:69)] <- NA

data_5_s3_8 <- data_5_s3[data_5_s3$terminal_year == 2015 | data_5_s3$terminal_year == 2016,]
data_5_s3_8$ty_order <- 8
data_5_s3_8$if_diff_5yrs <- "N"
data_5_s3_8$interval <- max(data_5_s3_8$terminal_year) - min(data_5_s3_8$terminal_year)
data_5_s3_8$ty <- 2015
data_5_s3_8$ty_minus1 <- data_5_s3_8$ty-1
data_5_s3_8$key_ty <- paste(data_5_s3_8$assessid, data_5_s3_8$ty, sep="_")
data_5_s3_8$key_ty_minus1 <- paste(data_5_s3_8$assessid, data_5_s3_8$ty_minus1, sep="_")
data_5_s3_8[,c(46:47,50:61,64:69)] <- NA

data_5_s3_9 <- data_5_s3[data_5_s3$terminal_year == 2015 | data_5_s3$terminal_year == 2017,]
data_5_s3_9$ty_order <- 9
data_5_s3_9$if_diff_5yrs <- "N"
data_5_s3_9$interval <- max(data_5_s3_9$terminal_year) - min(data_5_s3_9$terminal_year)
data_5_s3_9$ty <- 2015
data_5_s3_9$ty_minus1 <- data_5_s3_9$ty-1
data_5_s3_9$key_ty <- paste(data_5_s3_9$assessid, data_5_s3_9$ty, sep="_")
data_5_s3_9$key_ty_minus1 <- paste(data_5_s3_9$assessid, data_5_s3_9$ty_minus1, sep="_")
data_5_s3_9[,c(46:47,50:61,64:69)] <- NA

data_5_s3_10 <- data_5_s3[data_5_s3$terminal_year == 2016 | data_5_s3$terminal_year == 2017,]
data_5_s3_10$ty_order <- 10
data_5_s3_10$if_diff_5yrs <- "N"
data_5_s3_10$interval <- max(data_5_s3_10$terminal_year) - min(data_5_s3_10$terminal_year)
data_5_s3_10$ty <- 2016
data_5_s3_10$ty_minus1 <- data_5_s3_10$ty-1
data_5_s3_10$key_ty <- paste(data_5_s3_10$assessid, data_5_s3_10$ty, sep="_")
data_5_s3_10$key_ty_minus1 <- paste(data_5_s3_10$assessid, data_5_s3_10$ty_minus1, sep="_")
data_5_s3_10[,c(46:47,50:61,64:69)] <- NA

data_5_s3 <- rbind(data_5_s3_1, data_5_s3_2, data_5_s3_3, data_5_s3_4, data_5_s3_5, data_5_s3_6, data_5_s3_7, data_5_s3_8, data_5_s3_9, data_5_s3_10)

# assess 4
data_5_s4 <- data_5_assess[data_5_assess$stockid==ids[4],]

data_5_s4_1 <- data_5_s4[data_5_s4$terminal_year == 2013 | data_5_s4$terminal_year == 2014,]
data_5_s4_1$ty_order <- 1
data_5_s4_1$if_diff_5yrs <- "N"
data_5_s4_1$interval <- max(data_5_s4_1$terminal_year) - min(data_5_s4_1$terminal_year)

data_5_s4_2 <- data_5_s4[data_5_s4$terminal_year == 2013 | data_5_s4$terminal_year == 2015,]
data_5_s4_2$ty_order <- 2
data_5_s4_2$if_diff_5yrs <- "N"
data_5_s4_2$interval <- max(data_5_s4_2$terminal_year) - min(data_5_s4_2$terminal_year)

data_5_s4_3 <- data_5_s4[data_5_s4$terminal_year == 2013 | data_5_s4$terminal_year == 2016,]
data_5_s4_3$ty_order <- 3
data_5_s4_3$if_diff_5yrs <- "N"
data_5_s4_3$interval <- max(data_5_s4_3$terminal_year) - min(data_5_s4_3$terminal_year)

data_5_s4_4 <- data_5_s4[data_5_s4$terminal_year == 2013 | data_5_s4$terminal_year == 2017,]
data_5_s4_4$ty_order <- 4
data_5_s4_4$if_diff_5yrs <- "N"
data_5_s4_4$interval <- max(data_5_s4_4$terminal_year) - min(data_5_s4_4$terminal_year)

data_5_s4_5 <- data_5_s4[data_5_s4$terminal_year == 2014 | data_5_s4$terminal_year == 2015,]
data_5_s4_5$ty_order <- 5
data_5_s4_5$if_diff_5yrs <- "N"
data_5_s4_5$interval <- max(data_5_s4_5$terminal_year) - min(data_5_s4_5$terminal_year)
data_5_s4_5$ty <- 2014
data_5_s4_5$ty_minus1 <- data_5_s4_5$ty-1
data_5_s4_5$key_ty <- paste(data_5_s4_5$assessid, data_5_s4_5$ty, sep="_")
data_5_s4_5$key_ty_minus1 <- paste(data_5_s4_5$assessid, data_5_s4_5$ty_minus1, sep="_")
data_5_s4_5[,c(46:47,50:61,64:69)] <- NA

data_5_s4_6 <- data_5_s4[data_5_s4$terminal_year == 2014 | data_5_s4$terminal_year == 2016,]
data_5_s4_6$ty_order <- 6
data_5_s4_6$if_diff_5yrs <- "N"
data_5_s4_6$interval <- max(data_5_s4_6$terminal_year) - min(data_5_s4_6$terminal_year)
data_5_s4_6$ty <- 2014
data_5_s4_6$ty_minus1 <- data_5_s4_6$ty-1
data_5_s4_6$key_ty <- paste(data_5_s4_6$assessid, data_5_s4_6$ty, sep="_")
data_5_s4_6$key_ty_minus1 <- paste(data_5_s4_6$assessid, data_5_s4_6$ty_minus1, sep="_")
data_5_s4_6[,c(46:47,50:61,64:69)] <- NA

data_5_s4_7 <- data_5_s4[data_5_s4$terminal_year == 2014 | data_5_s4$terminal_year == 2017,]
data_5_s4_7$ty_order <- 7
data_5_s4_7$if_diff_5yrs <- "N"
data_5_s4_7$interval <- max(data_5_s4_7$terminal_year) - min(data_5_s4_7$terminal_year)
data_5_s4_7$ty <- 2014
data_5_s4_7$ty_minus1 <- data_5_s4_7$ty-1
data_5_s4_7$key_ty <- paste(data_5_s4_7$assessid, data_5_s4_7$ty, sep="_")
data_5_s4_7$key_ty_minus1 <- paste(data_5_s4_7$assessid, data_5_s4_7$ty_minus1, sep="_")
data_5_s4_7[,c(46:47,50:61,64:69)] <- NA

data_5_s4_8 <- data_5_s4[data_5_s4$terminal_year == 2015 | data_5_s4$terminal_year == 2016,]
data_5_s4_8$ty_order <- 8
data_5_s4_8$if_diff_5yrs <- "N"
data_5_s4_8$interval <- max(data_5_s4_8$terminal_year) - min(data_5_s4_8$terminal_year)
data_5_s4_8$ty <- 2015
data_5_s4_8$ty_minus1 <- data_5_s4_8$ty-1
data_5_s4_8$key_ty <- paste(data_5_s4_8$assessid, data_5_s4_8$ty, sep="_")
data_5_s4_8$key_ty_minus1 <- paste(data_5_s4_8$assessid, data_5_s4_8$ty_minus1, sep="_")
data_5_s4_8[,c(46:47,50:61,64:69)] <- NA

data_5_s4_9 <- data_5_s4[data_5_s4$terminal_year == 2015 | data_5_s4$terminal_year == 2017,]
data_5_s4_9$ty_order <- 9
data_5_s4_9$if_diff_5yrs <- "N"
data_5_s4_9$interval <- max(data_5_s4_9$terminal_year) - min(data_5_s4_9$terminal_year)
data_5_s4_9$ty <- 2015
data_5_s4_9$ty_minus1 <- data_5_s4_9$ty-1
data_5_s4_9$key_ty <- paste(data_5_s4_9$assessid, data_5_s4_9$ty, sep="_")
data_5_s4_9$key_ty_minus1 <- paste(data_5_s4_9$assessid, data_5_s4_9$ty_minus1, sep="_")
data_5_s4_9[,c(46:47,50:61,64:69)] <- NA

data_5_s4_10 <- data_5_s4[data_5_s4$terminal_year == 2016 | data_5_s4$terminal_year == 2017,]
data_5_s4_10$ty_order <- 10
data_5_s4_10$if_diff_5yrs <- "N"
data_5_s4_10$interval <- max(data_5_s4_10$terminal_year) - min(data_5_s4_10$terminal_year)
data_5_s4_10$ty <- 2016
data_5_s4_10$ty_minus1 <- data_5_s4_10$ty-1
data_5_s4_10$key_ty <- paste(data_5_s4_10$assessid, data_5_s4_10$ty, sep="_")
data_5_s4_10$key_ty_minus1 <- paste(data_5_s4_10$assessid, data_5_s4_10$ty_minus1, sep="_")
data_5_s4_10[,c(46:47,50:61,64:69)] <- NA

data_5_s4 <- rbind(data_5_s4_1, data_5_s4_2, data_5_s4_3, data_5_s4_4, data_5_s4_5, data_5_s4_6, data_5_s4_7, data_5_s4_8, data_5_s4_9, data_5_s4_10)

# assess 5
data_5_s5 <- data_5_assess[data_5_assess$stockid==ids[5],]

data_5_s5_1 <- data_5_s5[data_5_s5$terminal_year == 2013 | data_5_s5$terminal_year == 2014,]
data_5_s5_1$ty_order <- 1
data_5_s5_1$if_diff_5yrs <- "N"
data_5_s5_1$interval <- max(data_5_s5_1$terminal_year) - min(data_5_s5_1$terminal_year)

data_5_s5_2 <- data_5_s5[data_5_s5$terminal_year == 2013 | data_5_s5$terminal_year == 2015,]
data_5_s5_2$ty_order <- 2
data_5_s5_2$if_diff_5yrs <- "N"
data_5_s5_2$interval <- max(data_5_s5_2$terminal_year) - min(data_5_s5_2$terminal_year)

data_5_s5_3 <- data_5_s5[data_5_s5$terminal_year == 2013 | data_5_s5$terminal_year == 2016,]
data_5_s5_3$ty_order <- 3
data_5_s5_3$if_diff_5yrs <- "N"
data_5_s5_3$interval <- max(data_5_s5_3$terminal_year) - min(data_5_s5_3$terminal_year)

data_5_s5_4 <- data_5_s5[data_5_s5$terminal_year == 2013 | data_5_s5$terminal_year == 2017,]
data_5_s5_4$ty_order <- 4
data_5_s5_4$if_diff_5yrs <- "N"
data_5_s5_4$interval <- max(data_5_s5_4$terminal_year) - min(data_5_s5_4$terminal_year)

data_5_s5_5 <- data_5_s5[data_5_s5$terminal_year == 2014 | data_5_s5$terminal_year == 2015,]
data_5_s5_5$ty_order <- 5
data_5_s5_5$if_diff_5yrs <- "N"
data_5_s5_5$interval <- max(data_5_s5_5$terminal_year) - min(data_5_s5_5$terminal_year)
data_5_s5_5$ty <- 2014
data_5_s5_5$ty_minus1 <- data_5_s5_5$ty-1
data_5_s5_5$key_ty <- paste(data_5_s5_5$assessid, data_5_s5_5$ty, sep="_")
data_5_s5_5$key_ty_minus1 <- paste(data_5_s5_5$assessid, data_5_s5_5$ty_minus1, sep="_")
data_5_s5_5[,c(46:47,50:61,64:69)] <- NA

data_5_s5_6 <- data_5_s5[data_5_s5$terminal_year == 2014 | data_5_s5$terminal_year == 2016,]
data_5_s5_6$ty_order <- 6
data_5_s5_6$if_diff_5yrs <- "N"
data_5_s5_6$interval <- max(data_5_s5_6$terminal_year) - min(data_5_s5_6$terminal_year)
data_5_s5_6$ty <- 2014
data_5_s5_6$ty_minus1 <- data_5_s5_6$ty-1
data_5_s5_6$key_ty <- paste(data_5_s5_6$assessid, data_5_s5_6$ty, sep="_")
data_5_s5_6$key_ty_minus1 <- paste(data_5_s5_6$assessid, data_5_s5_6$ty_minus1, sep="_")
data_5_s5_6[,c(46:47,50:61,64:69)] <- NA

data_5_s5_7 <- data_5_s5[data_5_s5$terminal_year == 2014 | data_5_s5$terminal_year == 2017,]
data_5_s5_7$ty_order <- 7
data_5_s5_7$if_diff_5yrs <- "N"
data_5_s5_7$interval <- max(data_5_s5_7$terminal_year) - min(data_5_s5_7$terminal_year)
data_5_s5_7$ty <- 2014
data_5_s5_7$ty_minus1 <- data_5_s5_7$ty-1
data_5_s5_7$key_ty <- paste(data_5_s5_7$assessid, data_5_s5_7$ty, sep="_")
data_5_s5_7$key_ty_minus1 <- paste(data_5_s5_7$assessid, data_5_s5_7$ty_minus1, sep="_")
data_5_s5_7[,c(46:47,50:61,64:69)] <- NA

data_5_s5_8 <- data_5_s5[data_5_s5$terminal_year == 2015 | data_5_s5$terminal_year == 2016,]
data_5_s5_8$ty_order <- 8
data_5_s5_8$if_diff_5yrs <- "N"
data_5_s5_8$interval <- max(data_5_s5_8$terminal_year) - min(data_5_s5_8$terminal_year)
data_5_s5_8$ty <- 2015
data_5_s5_8$ty_minus1 <- data_5_s5_8$ty-1
data_5_s5_8$key_ty <- paste(data_5_s5_8$assessid, data_5_s5_8$ty, sep="_")
data_5_s5_8$key_ty_minus1 <- paste(data_5_s5_8$assessid, data_5_s5_8$ty_minus1, sep="_")
data_5_s5_8[,c(46:47,50:61,64:69)] <- NA

data_5_s5_9 <- data_5_s5[data_5_s5$terminal_year == 2015 | data_5_s5$terminal_year == 2017,]
data_5_s5_9$ty_order <- 9
data_5_s5_9$if_diff_5yrs <- "N"
data_5_s5_9$interval <- max(data_5_s5_9$terminal_year) - min(data_5_s5_9$terminal_year)
data_5_s5_9$ty <- 2015
data_5_s5_9$ty_minus1 <- data_5_s5_9$ty-1
data_5_s5_9$key_ty <- paste(data_5_s5_9$assessid, data_5_s5_9$ty, sep="_")
data_5_s5_9$key_ty_minus1 <- paste(data_5_s5_9$assessid, data_5_s5_9$ty_minus1, sep="_")
data_5_s5_9[,c(46:47,50:61,64:69)] <- NA

data_5_s5_10 <- data_5_s5[data_5_s5$terminal_year == 2016 | data_5_s5$terminal_year == 2017,]
data_5_s5_10$ty_order <- 10
data_5_s5_10$if_diff_5yrs <- "N"
data_5_s5_10$interval <- max(data_5_s5_10$terminal_year) - min(data_5_s5_10$terminal_year)
data_5_s5_10$ty <- 2016
data_5_s5_10$ty_minus1 <- data_5_s5_10$ty-1
data_5_s5_10$key_ty <- paste(data_5_s5_10$assessid, data_5_s5_10$ty, sep="_")
data_5_s5_10$key_ty_minus1 <- paste(data_5_s5_10$assessid, data_5_s5_10$ty_minus1, sep="_")
data_5_s5_10[,c(46:47,50:61,64:69)] <- NA

data_5_s5 <- rbind(data_5_s5_1, data_5_s5_2, data_5_s5_3, data_5_s5_4, data_5_s5_5, data_5_s5_6, data_5_s5_7, data_5_s5_8, data_5_s5_9, data_5_s5_10)

# assess 6
data_5_s6 <- data_5_assess[data_5_assess$stockid==ids[6],]

data_5_s6_1 <- data_5_s6[data_5_s6$terminal_year == 2013 | data_5_s6$terminal_year == 2014,]
data_5_s6_1$ty_order <- 1
data_5_s6_1$if_diff_5yrs <- "N"
data_5_s6_1$interval <- max(data_5_s6_1$terminal_year) - min(data_5_s6_1$terminal_year)

data_5_s6_2 <- data_5_s6[data_5_s6$terminal_year == 2013 | data_5_s6$terminal_year == 2015,]
data_5_s6_2$ty_order <- 2
data_5_s6_2$if_diff_5yrs <- "N"
data_5_s6_2$interval <- max(data_5_s6_2$terminal_year) - min(data_5_s6_2$terminal_year)

data_5_s6_3 <- data_5_s6[data_5_s6$terminal_year == 2013 | data_5_s6$terminal_year == 2016,]
data_5_s6_3$ty_order <- 3
data_5_s6_3$if_diff_5yrs <- "N"
data_5_s6_3$interval <- max(data_5_s6_3$terminal_year) - min(data_5_s6_3$terminal_year)

data_5_s6_4 <- data_5_s6[data_5_s6$terminal_year == 2013 | data_5_s6$terminal_year == 2017,]
data_5_s6_4$ty_order <- 4
data_5_s6_4$if_diff_5yrs <- "N"
data_5_s6_4$interval <- max(data_5_s6_4$terminal_year) - min(data_5_s6_4$terminal_year)

data_5_s6_5 <- data_5_s6[data_5_s6$terminal_year == 2014 | data_5_s6$terminal_year == 2015,]
data_5_s6_5$ty_order <- 5
data_5_s6_5$if_diff_5yrs <- "N"
data_5_s6_5$interval <- max(data_5_s6_5$terminal_year) - min(data_5_s6_5$terminal_year)
data_5_s6_5$ty <- 2014
data_5_s6_5$ty_minus1 <- data_5_s6_5$ty-1
data_5_s6_5$key_ty <- paste(data_5_s6_5$assessid, data_5_s6_5$ty, sep="_")
data_5_s6_5$key_ty_minus1 <- paste(data_5_s6_5$assessid, data_5_s6_5$ty_minus1, sep="_")
data_5_s6_5[,c(46:47,50:61,64:69)] <- NA

data_5_s6_6 <- data_5_s6[data_5_s6$terminal_year == 2014 | data_5_s6$terminal_year == 2016,]
data_5_s6_6$ty_order <- 6
data_5_s6_6$if_diff_5yrs <- "N"
data_5_s6_6$interval <- max(data_5_s6_6$terminal_year) - min(data_5_s6_6$terminal_year)
data_5_s6_6$ty <- 2014
data_5_s6_6$ty_minus1 <- data_5_s6_6$ty-1
data_5_s6_6$key_ty <- paste(data_5_s6_6$assessid, data_5_s6_6$ty, sep="_")
data_5_s6_6$key_ty_minus1 <- paste(data_5_s6_6$assessid, data_5_s6_6$ty_minus1, sep="_")
data_5_s6_6[,c(46:47,50:61,64:69)] <- NA

data_5_s6_7 <- data_5_s6[data_5_s6$terminal_year == 2014 | data_5_s6$terminal_year == 2017,]
data_5_s6_7$ty_order <- 7
data_5_s6_7$if_diff_5yrs <- "N"
data_5_s6_7$interval <- max(data_5_s6_7$terminal_year) - min(data_5_s6_7$terminal_year)
data_5_s6_7$ty <- 2014
data_5_s6_7$ty_minus1 <- data_5_s6_7$ty-1
data_5_s6_7$key_ty <- paste(data_5_s6_7$assessid, data_5_s6_7$ty, sep="_")
data_5_s6_7$key_ty_minus1 <- paste(data_5_s6_7$assessid, data_5_s6_7$ty_minus1, sep="_")
data_5_s6_7[,c(46:47,50:61,64:69)] <- NA

data_5_s6_8 <- data_5_s6[data_5_s6$terminal_year == 2015 | data_5_s6$terminal_year == 2016,]
data_5_s6_8$ty_order <- 8
data_5_s6_8$if_diff_5yrs <- "N"
data_5_s6_8$interval <- max(data_5_s6_8$terminal_year) - min(data_5_s6_8$terminal_year)
data_5_s6_8$ty <- 2015
data_5_s6_8$ty_minus1 <- data_5_s6_8$ty-1
data_5_s6_8$key_ty <- paste(data_5_s6_8$assessid, data_5_s6_8$ty, sep="_")
data_5_s6_8$key_ty_minus1 <- paste(data_5_s6_8$assessid, data_5_s6_8$ty_minus1, sep="_")
data_5_s6_8[,c(46:47,50:61,64:69)] <- NA

data_5_s6_9 <- data_5_s6[data_5_s6$terminal_year == 2015 | data_5_s6$terminal_year == 2017,]
data_5_s6_9$ty_order <- 9
data_5_s6_9$if_diff_5yrs <- "N"
data_5_s6_9$interval <- max(data_5_s6_9$terminal_year) - min(data_5_s6_9$terminal_year)
data_5_s6_9$ty <- 2015
data_5_s6_9$ty_minus1 <- data_5_s6_9$ty-1
data_5_s6_9$key_ty <- paste(data_5_s6_9$assessid, data_5_s6_9$ty, sep="_")
data_5_s6_9$key_ty_minus1 <- paste(data_5_s6_9$assessid, data_5_s6_9$ty_minus1, sep="_")
data_5_s6_9[,c(46:47,50:61,64:69)] <- NA

data_5_s6_10 <- data_5_s6[data_5_s6$terminal_year == 2016 | data_5_s6$terminal_year == 2017,]
data_5_s6_10$ty_order <- 10
data_5_s6_10$if_diff_5yrs <- "N"
data_5_s6_10$interval <- max(data_5_s6_10$terminal_year) - min(data_5_s6_10$terminal_year)
data_5_s6_10$ty <- 2016
data_5_s6_10$ty_minus1 <- data_5_s6_10$ty-1
data_5_s6_10$key_ty <- paste(data_5_s6_10$assessid, data_5_s6_10$ty, sep="_")
data_5_s6_10$key_ty_minus1 <- paste(data_5_s6_10$assessid, data_5_s6_10$ty_minus1, sep="_")
data_5_s6_10[,c(46:47,50:61,64:69)] <- NA

data_5_s6 <- rbind(data_5_s6_1, data_5_s6_2, data_5_s6_3, data_5_s6_4, data_5_s6_5, data_5_s6_6, data_5_s6_7, data_5_s6_8, data_5_s6_9, data_5_s6_10)

# assess 7
data_5_s7 <- data_5_assess[data_5_assess$stockid==ids[7],]

data_5_s7_1 <- data_5_s7[data_5_s7$terminal_year == 2012 | data_5_s7$terminal_year == 2013,]
data_5_s7_1$ty_order <- 1
data_5_s7_1$if_diff_5yrs <- "N"
data_5_s7_1$interval <- max(data_5_s7_1$terminal_year) - min(data_5_s7_1$terminal_year)

data_5_s7_2 <- data_5_s7[data_5_s7$terminal_year == 2012 | data_5_s7$terminal_year == 2014,]
data_5_s7_2$ty_order <- 2
data_5_s7_2$if_diff_5yrs <- "N"
data_5_s7_2$interval <- max(data_5_s7_2$terminal_year) - min(data_5_s7_2$terminal_year)

data_5_s7_3 <- data_5_s7[data_5_s7$terminal_year == 2012 | data_5_s7$terminal_year == 2015,]
data_5_s7_3$ty_order <- 3
data_5_s7_3$if_diff_5yrs <- "N"
data_5_s7_3$interval <- max(data_5_s7_3$terminal_year) - min(data_5_s7_3$terminal_year)

data_5_s7_4 <- data_5_s7[data_5_s7$terminal_year == 2012 | data_5_s7$terminal_year == 2016,]
data_5_s7_4$ty_order <- 4
data_5_s7_4$if_diff_5yrs <- "N"
data_5_s7_4$interval <- max(data_5_s7_4$terminal_year) - min(data_5_s7_4$terminal_year)

data_5_s7_5 <- data_5_s7[data_5_s7$terminal_year == 2013 | data_5_s7$terminal_year == 2014,]
data_5_s7_5$ty_order <- 5
data_5_s7_5$if_diff_5yrs <- "N"
data_5_s7_5$interval <- max(data_5_s7_5$terminal_year) - min(data_5_s7_5$terminal_year)
data_5_s7_5$ty <- 2013
data_5_s7_5$ty_minus1 <- data_5_s7_5$ty-1
data_5_s7_5$key_ty <- paste(data_5_s7_5$assessid, data_5_s7_5$ty, sep="_")
data_5_s7_5$key_ty_minus1 <- paste(data_5_s7_5$assessid, data_5_s7_5$ty_minus1, sep="_")
data_5_s7_5[,c(46:47,50:61,64:69)] <- NA

data_5_s7_6 <- data_5_s7[data_5_s7$terminal_year == 2013 | data_5_s7$terminal_year == 2015,]
data_5_s7_6$ty_order <- 6
data_5_s7_6$if_diff_5yrs <- "N"
data_5_s7_6$interval <- max(data_5_s7_6$terminal_year) - min(data_5_s7_6$terminal_year)
data_5_s7_6$ty <- 2013
data_5_s7_6$ty_minus1 <- data_5_s7_6$ty-1
data_5_s7_6$key_ty <- paste(data_5_s7_6$assessid, data_5_s7_6$ty, sep="_")
data_5_s7_6$key_ty_minus1 <- paste(data_5_s7_6$assessid, data_5_s7_6$ty_minus1, sep="_")
data_5_s7_6[,c(46:47,50:61,64:69)] <- NA

data_5_s7_7 <- data_5_s7[data_5_s7$terminal_year == 2013 | data_5_s7$terminal_year == 2016,]
data_5_s7_7$ty_order <- 7
data_5_s7_7$if_diff_5yrs <- "N"
data_5_s7_7$interval <- max(data_5_s7_7$terminal_year) - min(data_5_s7_7$terminal_year)
data_5_s7_7$ty <- 2013
data_5_s7_7$ty_minus1 <- data_5_s7_7$ty-1
data_5_s7_7$key_ty <- paste(data_5_s7_7$assessid, data_5_s7_7$ty, sep="_")
data_5_s7_7$key_ty_minus1 <- paste(data_5_s7_7$assessid, data_5_s7_7$ty_minus1, sep="_")
data_5_s7_7[,c(46:47,50:61,64:69)] <- NA

data_5_s7_8 <- data_5_s7[data_5_s7$terminal_year == 2014 | data_5_s7$terminal_year == 2015,]
data_5_s7_8$ty_order <- 8
data_5_s7_8$if_diff_5yrs <- "N"
data_5_s7_8$interval <- max(data_5_s7_8$terminal_year) - min(data_5_s7_8$terminal_year)
data_5_s7_8$ty <- 2014
data_5_s7_8$ty_minus1 <- data_5_s7_8$ty-1
data_5_s7_8$key_ty <- paste(data_5_s7_8$assessid, data_5_s7_8$ty, sep="_")
data_5_s7_8$key_ty_minus1 <- paste(data_5_s7_8$assessid, data_5_s7_8$ty_minus1, sep="_")
data_5_s7_8[,c(46:47,50:61,64:69)] <- NA

data_5_s7_9 <- data_5_s7[data_5_s7$terminal_year == 2014 | data_5_s7$terminal_year == 2016,]
data_5_s7_9$ty_order <- 9
data_5_s7_9$if_diff_5yrs <- "N"
data_5_s7_9$interval <- max(data_5_s7_9$terminal_year) - min(data_5_s7_9$terminal_year)
data_5_s7_9$ty <- 2014
data_5_s7_9$ty_minus1 <- data_5_s7_9$ty-1
data_5_s7_9$key_ty <- paste(data_5_s7_9$assessid, data_5_s7_9$ty, sep="_")
data_5_s7_9$key_ty_minus1 <- paste(data_5_s7_9$assessid, data_5_s7_9$ty_minus1, sep="_")
data_5_s7_9[,c(46:47,50:61,64:69)] <- NA

data_5_s7_10 <- data_5_s7[data_5_s7$terminal_year == 2015 | data_5_s7$terminal_year == 2016,]
data_5_s7_10$ty_order <- 10
data_5_s7_10$if_diff_5yrs <- "N"
data_5_s7_10$interval <- max(data_5_s7_10$terminal_year) - min(data_5_s7_10$terminal_year)
data_5_s7_10$ty <- 2015
data_5_s7_10$ty_minus1 <- data_5_s7_10$ty-1
data_5_s7_10$key_ty <- paste(data_5_s7_10$assessid, data_5_s7_10$ty, sep="_")
data_5_s7_10$key_ty_minus1 <- paste(data_5_s7_10$assessid, data_5_s7_10$ty_minus1, sep="_")
data_5_s7_10[,c(46:47,50:61,64:69)] <- NA

data_5_s7 <- rbind(data_5_s7_1, data_5_s7_2, data_5_s7_3, data_5_s7_4, data_5_s7_5, data_5_s7_6, data_5_s7_7, data_5_s7_8, data_5_s7_9, data_5_s7_10)

# assess 8
data_5_s8 <- data_5_assess[data_5_assess$stockid==ids[8],]

data_5_s8_1 <- data_5_s8[data_5_s8$terminal_year == 2012 | data_5_s8$terminal_year == 2013,]
data_5_s8_1$ty_order <- 1
data_5_s8_1$if_diff_5yrs <- "N"
data_5_s8_1$interval <- max(data_5_s8_1$terminal_year) - min(data_5_s8_1$terminal_year)

data_5_s8_2 <- data_5_s8[data_5_s8$terminal_year == 2012 | data_5_s8$terminal_year == 2014,]
data_5_s8_2$ty_order <- 2
data_5_s8_2$if_diff_5yrs <- "N"
data_5_s8_2$interval <- max(data_5_s8_2$terminal_year) - min(data_5_s8_2$terminal_year)

data_5_s8_3 <- data_5_s8[data_5_s8$terminal_year == 2012 | data_5_s8$terminal_year == 2015,]
data_5_s8_3$ty_order <- 3
data_5_s8_3$if_diff_5yrs <- "N"
data_5_s8_3$interval <- max(data_5_s8_3$terminal_year) - min(data_5_s8_3$terminal_year)

data_5_s8_4 <- data_5_s8[data_5_s8$terminal_year == 2012 | data_5_s8$terminal_year == 2016,]
data_5_s8_4$ty_order <- 4
data_5_s8_4$if_diff_5yrs <- "N"
data_5_s8_4$interval <- max(data_5_s8_4$terminal_year) - min(data_5_s8_4$terminal_year)

data_5_s8_5 <- data_5_s8[data_5_s8$terminal_year == 2013 | data_5_s8$terminal_year == 2014,]
data_5_s8_5$ty_order <- 5
data_5_s8_5$if_diff_5yrs <- "N"
data_5_s8_5$interval <- max(data_5_s8_5$terminal_year) - min(data_5_s8_5$terminal_year)
data_5_s8_5$ty <- 2013
data_5_s8_5$ty_minus1 <- data_5_s8_5$ty-1
data_5_s8_5$key_ty <- paste(data_5_s8_5$assessid, data_5_s8_5$ty, sep="_")
data_5_s8_5$key_ty_minus1 <- paste(data_5_s8_5$assessid, data_5_s8_5$ty_minus1, sep="_")
data_5_s8_5[,c(46:47,50:61,64:69)] <- NA

data_5_s8_6 <- data_5_s8[data_5_s8$terminal_year == 2013 | data_5_s8$terminal_year == 2015,]
data_5_s8_6$ty_order <- 6
data_5_s8_6$if_diff_5yrs <- "N"
data_5_s8_6$interval <- max(data_5_s8_6$terminal_year) - min(data_5_s8_6$terminal_year)
data_5_s8_6$ty <- 2013
data_5_s8_6$ty_minus1 <- data_5_s8_6$ty-1
data_5_s8_6$key_ty <- paste(data_5_s8_6$assessid, data_5_s8_6$ty, sep="_")
data_5_s8_6$key_ty_minus1 <- paste(data_5_s8_6$assessid, data_5_s8_6$ty_minus1, sep="_")
data_5_s8_6[,c(46:47,50:61,64:69)] <- NA

data_5_s8_7 <- data_5_s8[data_5_s8$terminal_year == 2013 | data_5_s8$terminal_year == 2016,]
data_5_s8_7$ty_order <- 7
data_5_s8_7$if_diff_5yrs <- "N"
data_5_s8_7$interval <- max(data_5_s8_7$terminal_year) - min(data_5_s8_7$terminal_year)
data_5_s8_7$ty <- 2013
data_5_s8_7$ty_minus1 <- data_5_s8_7$ty-1
data_5_s8_7$key_ty <- paste(data_5_s8_7$assessid, data_5_s8_7$ty, sep="_")
data_5_s8_7$key_ty_minus1 <- paste(data_5_s8_7$assessid, data_5_s8_7$ty_minus1, sep="_")
data_5_s8_7[,c(46:47,50:61,64:69)] <- NA

data_5_s8_8 <- data_5_s8[data_5_s8$terminal_year == 2014 | data_5_s8$terminal_year == 2015,]
data_5_s8_8$ty_order <- 8
data_5_s8_8$if_diff_5yrs <- "N"
data_5_s8_8$interval <- max(data_5_s8_8$terminal_year) - min(data_5_s8_8$terminal_year)
data_5_s8_8$ty <- 2014
data_5_s8_8$ty_minus1 <- data_5_s8_8$ty-1
data_5_s8_8$key_ty <- paste(data_5_s8_8$assessid, data_5_s8_8$ty, sep="_")
data_5_s8_8$key_ty_minus1 <- paste(data_5_s8_8$assessid, data_5_s8_8$ty_minus1, sep="_")
data_5_s8_8[,c(46:47,50:61,64:69)] <- NA

data_5_s8_9 <- data_5_s8[data_5_s8$terminal_year == 2014 | data_5_s8$terminal_year == 2016,]
data_5_s8_9$ty_order <- 9
data_5_s8_9$if_diff_5yrs <- "N"
data_5_s8_9$interval <- max(data_5_s8_9$terminal_year) - min(data_5_s8_9$terminal_year)
data_5_s8_9$ty <- 2014
data_5_s8_9$ty_minus1 <- data_5_s8_9$ty-1
data_5_s8_9$key_ty <- paste(data_5_s8_9$assessid, data_5_s8_9$ty, sep="_")
data_5_s8_9$key_ty_minus1 <- paste(data_5_s8_9$assessid, data_5_s8_9$ty_minus1, sep="_")
data_5_s8_9[,c(46:47,50:61,64:69)] <- NA

data_5_s8_10 <- data_5_s8[data_5_s8$terminal_year == 2015 | data_5_s8$terminal_year == 2016,]
data_5_s8_10$ty_order <- 10
data_5_s8_10$if_diff_5yrs <- "N"
data_5_s8_10$interval <- max(data_5_s8_10$terminal_year) - min(data_5_s8_10$terminal_year)
data_5_s8_10$ty <- 2015
data_5_s8_10$ty_minus1 <- data_5_s8_10$ty-1
data_5_s8_10$key_ty <- paste(data_5_s8_10$assessid, data_5_s8_10$ty, sep="_")
data_5_s8_10$key_ty_minus1 <- paste(data_5_s8_10$assessid, data_5_s8_10$ty_minus1, sep="_")
data_5_s8_10[,c(46:47,50:61,64:69)] <- NA

data_5_s8 <- rbind(data_5_s8_1, data_5_s8_2, data_5_s8_3, data_5_s8_4, data_5_s8_5, data_5_s8_6, data_5_s8_7, data_5_s8_8, data_5_s8_9, data_5_s8_10)

data_5 <- rbind(data_5_s1, data_5_s2, data_5_s3, data_5_s4, data_5_s5,
                data_5_s6, data_5_s7, data_5_s8)

write.csv(data_5, paste(outputdir, "0e_4_data_5_assess_with_records.csv", sep="/"), row.names=F)

########################################################################################################################################

## Stocks with 6 assessments
data_6_assess <- dd_new2[dd_new2$stockid %in% stockid_6assess,]
length(unique(data_6_assess$stockid))  # 17 stocks
length(unique(data_6_assess$assessid))  # 102 assesses

ids <- unique(data_6_assess$stockid)

# assess 1
data_6_s1 <- data_6_assess[data_6_assess$stockid==ids[1],]

data_6_s1_1 <- data_6_s1[data_6_s1$terminal_year == 2010 | data_6_s1$terminal_year == 2012,]
data_6_s1_1$ty_order <- 1
data_6_s1_1$if_diff_5yrs <- "N"
data_6_s1_1$interval <- max(data_6_s1_1$terminal_year) - min(data_6_s1_1$terminal_year)

data_6_s1_2 <- data_6_s1[data_6_s1$terminal_year == 2010 | data_6_s1$terminal_year == 2014,]
data_6_s1_2$ty_order <- 2
data_6_s1_2$if_diff_5yrs <- "N"
data_6_s1_2$interval <- max(data_6_s1_2$terminal_year) - min(data_6_s1_2$terminal_year)

data_6_s1_3 <- data_6_s1[data_6_s1$terminal_year == 2010 | data_6_s1$terminal_year == 2015,]
data_6_s1_3$ty_order <- 3
data_6_s1_3$if_diff_5yrs <- "N"
data_6_s1_3$interval <- max(data_6_s1_3$terminal_year) - min(data_6_s1_3$terminal_year)

data_6_s1_4 <- data_6_s1[data_6_s1$terminal_year == 2010 | data_6_s1$terminal_year == 2016,]
data_6_s1_4$ty_order <- 4
data_6_s1_4$if_diff_5yrs <- "Y"
data_6_s1_4$interval <- max(data_6_s1_4$terminal_year) - min(data_6_s1_4$terminal_year)

data_6_s1_5 <- data_6_s1[data_6_s1$terminal_year == 2010 | data_6_s1$terminal_year == 2017,]
data_6_s1_5$ty_order <- 5
data_6_s1_5$if_diff_5yrs <- "Y"
data_6_s1_5$interval <- max(data_6_s1_5$terminal_year) - min(data_6_s1_5$terminal_year)

data_6_s1_6 <- data_6_s1[data_6_s1$terminal_year == 2012 | data_6_s1$terminal_year == 2014,]
data_6_s1_6$ty_order <- 6
data_6_s1_6$if_diff_5yrs <- "N"
data_6_s1_6$interval <- max(data_6_s1_6$terminal_year) - min(data_6_s1_6$terminal_year)
data_6_s1_6$ty <- 2012
data_6_s1_6$ty_minus1 <- data_6_s1_6$ty-1
data_6_s1_6$key_ty <- paste(data_6_s1_6$assessid, data_6_s1_6$ty, sep="_")
data_6_s1_6$key_ty_minus1 <- paste(data_6_s1_6$assessid, data_6_s1_6$ty_minus1, sep="_")
data_6_s1_6[,c(46:47,50:61,64:69)] <- NA

data_6_s1_7 <- data_6_s1[data_6_s1$terminal_year == 2012 | data_6_s1$terminal_year == 2015,]
data_6_s1_7$ty_order <- 7
data_6_s1_7$if_diff_5yrs <- "N"
data_6_s1_7$interval <- max(data_6_s1_7$terminal_year) - min(data_6_s1_7$terminal_year)
data_6_s1_7$ty <- 2012
data_6_s1_7$ty_minus1 <- data_6_s1_7$ty-1
data_6_s1_7$key_ty <- paste(data_6_s1_7$assessid, data_6_s1_7$ty, sep="_")
data_6_s1_7$key_ty_minus1 <- paste(data_6_s1_7$assessid, data_6_s1_7$ty_minus1, sep="_")
data_6_s1_7[,c(46:47,50:61,64:69)] <- NA

data_6_s1_8 <- data_6_s1[data_6_s1$terminal_year == 2012 | data_6_s1$terminal_year == 2016,]
data_6_s1_8$ty_order <- 8
data_6_s1_8$if_diff_5yrs <- "N"
data_6_s1_8$interval <- max(data_6_s1_8$terminal_year) - min(data_6_s1_8$terminal_year)
data_6_s1_8$ty <- 2012
data_6_s1_8$ty_minus1 <- data_6_s1_8$ty-1
data_6_s1_8$key_ty <- paste(data_6_s1_8$assessid, data_6_s1_8$ty, sep="_")
data_6_s1_8$key_ty_minus1 <- paste(data_6_s1_8$assessid, data_6_s1_8$ty_minus1, sep="_")
data_6_s1_8[,c(46:47,50:61,64:69)] <- NA

data_6_s1_9 <- data_6_s1[data_6_s1$terminal_year == 2012 | data_6_s1$terminal_year == 2017,]
data_6_s1_9$ty_order <- 9
data_6_s1_9$if_diff_5yrs <- "N"
data_6_s1_9$interval <- max(data_6_s1_9$terminal_year) - min(data_6_s1_9$terminal_year)
data_6_s1_9$ty <- 2012
data_6_s1_9$ty_minus1 <- data_6_s1_9$ty-1
data_6_s1_9$key_ty <- paste(data_6_s1_9$assessid, data_6_s1_9$ty, sep="_")
data_6_s1_9$key_ty_minus1 <- paste(data_6_s1_9$assessid, data_6_s1_9$ty_minus1, sep="_")
data_6_s1_9[,c(46:47,50:61,64:69)] <- NA

data_6_s1_10 <- data_6_s1[data_6_s1$terminal_year == 2014 | data_6_s1$terminal_year == 2015,]
data_6_s1_10$ty_order <- 10
data_6_s1_10$if_diff_5yrs <- "N"
data_6_s1_10$interval <- max(data_6_s1_10$terminal_year) - min(data_6_s1_10$terminal_year)
data_6_s1_10$ty <- 2014
data_6_s1_10$ty_minus1 <- data_6_s1_10$ty-1
data_6_s1_10$key_ty <- paste(data_6_s1_10$assessid, data_6_s1_10$ty, sep="_")
data_6_s1_10$key_ty_minus1 <- paste(data_6_s1_10$assessid, data_6_s1_10$ty_minus1, sep="_")
data_6_s1_10[,c(46:47,50:61,64:69)] <- NA

data_6_s1_11 <- data_6_s1[data_6_s1$terminal_year == 2014 | data_6_s1$terminal_year == 2016,]
data_6_s1_11$ty_order <- 11
data_6_s1_11$if_diff_5yrs <- "N"
data_6_s1_11$interval <- max(data_6_s1_11$terminal_year) - min(data_6_s1_11$terminal_year)
data_6_s1_11$ty <- 2014
data_6_s1_11$ty_minus1 <- data_6_s1_11$ty-1
data_6_s1_11$key_ty <- paste(data_6_s1_11$assessid, data_6_s1_11$ty, sep="_")
data_6_s1_11$key_ty_minus1 <- paste(data_6_s1_11$assessid, data_6_s1_11$ty_minus1, sep="_")
data_6_s1_11[,c(46:47,50:61,64:69)] <- NA

data_6_s1_12 <- data_6_s1[data_6_s1$terminal_year == 2014 | data_6_s1$terminal_year == 2017,]
data_6_s1_12$ty_order <- 12
data_6_s1_12$if_diff_5yrs <- "N"
data_6_s1_12$interval <- max(data_6_s1_12$terminal_year) - min(data_6_s1_12$terminal_year)
data_6_s1_12$ty <- 2014
data_6_s1_12$ty_minus1 <- data_6_s1_12$ty-1
data_6_s1_12$key_ty <- paste(data_6_s1_12$assessid, data_6_s1_12$ty, sep="_")
data_6_s1_12$key_ty_minus1 <- paste(data_6_s1_12$assessid, data_6_s1_12$ty_minus1, sep="_")
data_6_s1_12[,c(46:47,50:61,64:69)] <- NA

data_6_s1_13 <- data_6_s1[data_6_s1$terminal_year == 2015 | data_6_s1$terminal_year == 2016,]
data_6_s1_13$ty_order <- 13
data_6_s1_13$if_diff_5yrs <- "N"
data_6_s1_13$interval <- max(data_6_s1_13$terminal_year) - min(data_6_s1_13$terminal_year)
data_6_s1_13$ty <- 2015
data_6_s1_13$ty_minus1 <- data_6_s1_13$ty-1
data_6_s1_13$key_ty <- paste(data_6_s1_13$assessid, data_6_s1_13$ty, sep="_")
data_6_s1_13$key_ty_minus1 <- paste(data_6_s1_13$assessid, data_6_s1_13$ty_minus1, sep="_")
data_6_s1_13[,c(46:47,50:61,64:69)] <- NA

data_6_s1_14 <- data_6_s1[data_6_s1$terminal_year == 2015 | data_6_s1$terminal_year == 2017,]
data_6_s1_14$ty_order <- 14
data_6_s1_14$if_diff_5yrs <- "N"
data_6_s1_14$interval <- max(data_6_s1_14$terminal_year) - min(data_6_s1_14$terminal_year)
data_6_s1_14$ty <- 2015
data_6_s1_14$ty_minus1 <- data_6_s1_14$ty-1
data_6_s1_14$key_ty <- paste(data_6_s1_14$assessid, data_6_s1_14$ty, sep="_")
data_6_s1_14$key_ty_minus1 <- paste(data_6_s1_14$assessid, data_6_s1_14$ty_minus1, sep="_")
data_6_s1_14[,c(46:47,50:61,64:69)] <- NA

data_6_s1_15 <- data_6_s1[data_6_s1$terminal_year == 2016 | data_6_s1$terminal_year == 2017,]
data_6_s1_15$ty_order <- 15
data_6_s1_15$if_diff_5yrs <- "N"
data_6_s1_15$interval <- max(data_6_s1_15$terminal_year) - min(data_6_s1_15$terminal_year)
data_6_s1_15$ty <- 2016
data_6_s1_15$ty_minus1 <- data_6_s1_15$ty-1
data_6_s1_15$key_ty <- paste(data_6_s1_15$assessid, data_6_s1_15$ty, sep="_")
data_6_s1_15$key_ty_minus1 <- paste(data_6_s1_15$assessid, data_6_s1_15$ty_minus1, sep="_")
data_6_s1_15[,c(46:47,50:61,64:69)] <- NA

data_6_s1 <- rbind(data_6_s1_1, data_6_s1_2, data_6_s1_3, data_6_s1_4, data_6_s1_5, data_6_s1_6, data_6_s1_7, data_6_s1_8, data_6_s1_9, data_6_s1_10,
                   data_6_s1_11, data_6_s1_12, data_6_s1_13, data_6_s1_14, data_6_s1_15)

# assess 2
data_6_s2 <- data_6_assess[data_6_assess$stockid==ids[2],]
data_6_s2$ty <- 2012
data_6_s2$ty_minus1 <- data_6_s2$ty-1
data_6_s2$key_ty <- paste(data_6_s2$assessid, data_6_s2$ty, sep="_")
data_6_s2$key_ty_minus1 <- paste(data_6_s2$assessid, data_6_s2$ty_minus1, sep="_")
data_6_s2[,c(46:47,50:61,64:69)] <- NA

data_6_s2_1 <- data_6_s2[data_6_s2$terminal_year == 2012 | data_6_s2$terminal_year == 2013,]
data_6_s2_1$ty_order <- 1
data_6_s2_1$if_diff_5yrs <- "N"
data_6_s2_1$interval <- max(data_6_s2_1$terminal_year) - min(data_6_s2_1$terminal_year)

data_6_s2_2 <- data_6_s2[data_6_s2$terminal_year == 2012 | data_6_s2$terminal_year == 2014,]
data_6_s2_2$ty_order <- 2
data_6_s2_2$if_diff_5yrs <- "N"
data_6_s2_2$interval <- max(data_6_s2_2$terminal_year) - min(data_6_s2_2$terminal_year)

data_6_s2_3 <- data_6_s2[data_6_s2$terminal_year == 2012 | data_6_s2$terminal_year == 2015,]
data_6_s2_3$ty_order <- 3
data_6_s2_3$if_diff_5yrs <- "N"
data_6_s2_3$interval <- max(data_6_s2_3$terminal_year) - min(data_6_s2_3$terminal_year)

data_6_s2_4 <- data_6_s2[data_6_s2$terminal_year == 2012 | data_6_s2$terminal_year == 2016,]
data_6_s2_4$ty_order <- 4
data_6_s2_4$if_diff_5yrs <- "N"
data_6_s2_4$interval <- max(data_6_s2_4$terminal_year) - min(data_6_s2_4$terminal_year)

data_6_s2_5 <- data_6_s2[data_6_s2$terminal_year == 2012 | data_6_s2$terminal_year == 2017,]
data_6_s2_5$ty_order <- 5
data_6_s2_5$if_diff_5yrs <- "N"
data_6_s2_5$interval <- max(data_6_s2_5$terminal_year) - min(data_6_s2_5$terminal_year)

data_6_s2_6 <- data_6_s2[data_6_s2$terminal_year == 2013 | data_6_s2$terminal_year == 2014,]
data_6_s2_6$ty_order <- 6
data_6_s2_6$if_diff_5yrs <- "N"
data_6_s2_6$interval <- max(data_6_s2_6$terminal_year) - min(data_6_s2_6$terminal_year)
data_6_s2_6$ty <- 2013
data_6_s2_6$ty_minus1 <- data_6_s2_6$ty-1
data_6_s2_6$key_ty <- paste(data_6_s2_6$assessid, data_6_s2_6$ty, sep="_")
data_6_s2_6$key_ty_minus1 <- paste(data_6_s2_6$assessid, data_6_s2_6$ty_minus1, sep="_")
data_6_s2_6[,c(46:47,50:61,64:69)] <- NA

data_6_s2_7 <- data_6_s2[data_6_s2$terminal_year == 2013 | data_6_s2$terminal_year == 2015,]
data_6_s2_7$ty_order <- 7
data_6_s2_7$if_diff_5yrs <- "N"
data_6_s2_7$interval <- max(data_6_s2_7$terminal_year) - min(data_6_s2_7$terminal_year)
data_6_s2_7$ty <- 2013
data_6_s2_7$ty_minus1 <- data_6_s2_7$ty-1
data_6_s2_7$key_ty <- paste(data_6_s2_7$assessid, data_6_s2_7$ty, sep="_")
data_6_s2_7$key_ty_minus1 <- paste(data_6_s2_7$assessid, data_6_s2_7$ty_minus1, sep="_")
data_6_s2_7[,c(46:47,50:61,64:69)] <- NA

data_6_s2_8 <- data_6_s2[data_6_s2$terminal_year == 2013 | data_6_s2$terminal_year == 2016,]
data_6_s2_8$ty_order <- 8
data_6_s2_8$if_diff_5yrs <- "N"
data_6_s2_8$interval <- max(data_6_s2_8$terminal_year) - min(data_6_s2_8$terminal_year)
data_6_s2_8$ty <- 2013
data_6_s2_8$ty_minus1 <- data_6_s2_8$ty-1
data_6_s2_8$key_ty <- paste(data_6_s2_8$assessid, data_6_s2_8$ty, sep="_")
data_6_s2_8$key_ty_minus1 <- paste(data_6_s2_8$assessid, data_6_s2_8$ty_minus1, sep="_")
data_6_s2_8[,c(46:47,50:61,64:69)] <- NA

data_6_s2_9 <- data_6_s2[data_6_s2$terminal_year == 2013 | data_6_s2$terminal_year == 2017,]
data_6_s2_9$ty_order <- 9
data_6_s2_9$if_diff_5yrs <- "N"
data_6_s2_9$interval <- max(data_6_s2_9$terminal_year) - min(data_6_s2_9$terminal_year)
data_6_s2_9$ty <- 2013
data_6_s2_9$ty_minus1 <- data_6_s2_9$ty-1
data_6_s2_9$key_ty <- paste(data_6_s2_9$assessid, data_6_s2_9$ty, sep="_")
data_6_s2_9$key_ty_minus1 <- paste(data_6_s2_9$assessid, data_6_s2_9$ty_minus1, sep="_")
data_6_s2_9[,c(46:47,50:61,64:69)] <- NA

data_6_s2_10 <- data_6_s2[data_6_s2$terminal_year == 2014 | data_6_s2$terminal_year == 2015,]
data_6_s2_10$ty_order <- 10
data_6_s2_10$if_diff_5yrs <- "N"
data_6_s2_10$interval <- max(data_6_s2_10$terminal_year) - min(data_6_s2_10$terminal_year)
data_6_s2_10$ty <- 2014
data_6_s2_10$ty_minus1 <- data_6_s2_10$ty-1
data_6_s2_10$key_ty <- paste(data_6_s2_10$assessid, data_6_s2_10$ty, sep="_")
data_6_s2_10$key_ty_minus1 <- paste(data_6_s2_10$assessid, data_6_s2_10$ty_minus1, sep="_")
data_6_s2_10[,c(46:47,50:61,64:69)] <- NA

data_6_s2_11 <- data_6_s2[data_6_s2$terminal_year == 2014 | data_6_s2$terminal_year == 2016,]
data_6_s2_11$ty_order <- 11
data_6_s2_11$if_diff_5yrs <- "N"
data_6_s2_11$interval <- max(data_6_s2_11$terminal_year) - min(data_6_s2_11$terminal_year)
data_6_s2_11$ty <- 2014
data_6_s2_11$ty_minus1 <- data_6_s2_11$ty-1
data_6_s2_11$key_ty <- paste(data_6_s2_11$assessid, data_6_s2_11$ty, sep="_")
data_6_s2_11$key_ty_minus1 <- paste(data_6_s2_11$assessid, data_6_s2_11$ty_minus1, sep="_")
data_6_s2_11[,c(46:47,50:61,64:69)] <- NA

data_6_s2_12 <- data_6_s2[data_6_s2$terminal_year == 2014 | data_6_s2$terminal_year == 2017,]
data_6_s2_12$ty_order <- 12
data_6_s2_12$if_diff_5yrs <- "N"
data_6_s2_12$interval <- max(data_6_s2_12$terminal_year) - min(data_6_s2_12$terminal_year)
data_6_s2_12$ty <- 2014
data_6_s2_12$ty_minus1 <- data_6_s2_12$ty-1
data_6_s2_12$key_ty <- paste(data_6_s2_12$assessid, data_6_s2_12$ty, sep="_")
data_6_s2_12$key_ty_minus1 <- paste(data_6_s2_12$assessid, data_6_s2_12$ty_minus1, sep="_")
data_6_s2_12[,c(46:47,50:61,64:69)] <- NA

data_6_s2_13 <- data_6_s2[data_6_s2$terminal_year == 2015 | data_6_s2$terminal_year == 2016,]
data_6_s2_13$ty_order <- 13
data_6_s2_13$if_diff_5yrs <- "N"
data_6_s2_13$interval <- max(data_6_s2_13$terminal_year) - min(data_6_s2_13$terminal_year)
data_6_s2_13$ty <- 2015
data_6_s2_13$ty_minus1 <- data_6_s2_13$ty-1
data_6_s2_13$key_ty <- paste(data_6_s2_13$assessid, data_6_s2_13$ty, sep="_")
data_6_s2_13$key_ty_minus1 <- paste(data_6_s2_13$assessid, data_6_s2_13$ty_minus1, sep="_")
data_6_s2_13[,c(46:47,50:61,64:69)] <- NA

data_6_s2_14 <- data_6_s2[data_6_s2$terminal_year == 2015 | data_6_s2$terminal_year == 2017,]
data_6_s2_14$ty_order <- 14
data_6_s2_14$if_diff_5yrs <- "N"
data_6_s2_14$interval <- max(data_6_s2_14$terminal_year) - min(data_6_s2_14$terminal_year)
data_6_s2_14$ty <- 2015
data_6_s2_14$ty_minus1 <- data_6_s2_14$ty-1
data_6_s2_14$key_ty <- paste(data_6_s2_14$assessid, data_6_s2_14$ty, sep="_")
data_6_s2_14$key_ty_minus1 <- paste(data_6_s2_14$assessid, data_6_s2_14$ty_minus1, sep="_")
data_6_s2_14[,c(46:47,50:61,64:69)] <- NA

data_6_s2_15 <- data_6_s2[data_6_s2$terminal_year == 2016 | data_6_s2$terminal_year == 2017,]
data_6_s2_15$ty_order <- 15
data_6_s2_15$if_diff_5yrs <- "N"
data_6_s2_15$interval <- max(data_6_s2_15$terminal_year) - min(data_6_s2_15$terminal_year)
data_6_s2_15$ty <- 2016
data_6_s2_15$ty_minus1 <- data_6_s2_15$ty-1
data_6_s2_15$key_ty <- paste(data_6_s2_15$assessid, data_6_s2_15$ty, sep="_")
data_6_s2_15$key_ty_minus1 <- paste(data_6_s2_15$assessid, data_6_s2_15$ty_minus1, sep="_")
data_6_s2_15[,c(46:47,50:61,64:69)] <- NA

data_6_s2 <- rbind(data_6_s2_1, data_6_s2_2, data_6_s2_3, data_6_s2_4, data_6_s2_5, data_6_s2_6, data_6_s2_7, data_6_s2_8, data_6_s2_9, data_6_s2_10,
                   data_6_s2_11, data_6_s2_12, data_6_s2_13, data_6_s2_14, data_6_s2_15)

# assess 3
data_6_s3 <- data_6_assess[data_6_assess$stockid==ids[3],]

data_6_s3_1 <- data_6_s3[data_6_s3$terminal_year == 2011 | data_6_s3$terminal_year == 2012,]
data_6_s3_1$ty_order <- 1
data_6_s3_1$if_diff_5yrs <- "N"
data_6_s3_1$interval <- max(data_6_s3_1$terminal_year) - min(data_6_s3_1$terminal_year)

data_6_s3_2 <- data_6_s3[data_6_s3$terminal_year == 2011 | data_6_s3$terminal_year == 2013,]
data_6_s3_2$ty_order <- 2
data_6_s3_2$if_diff_5yrs <- "N"
data_6_s3_2$interval <- max(data_6_s3_2$terminal_year) - min(data_6_s3_2$terminal_year)

data_6_s3_3 <- data_6_s3[data_6_s3$terminal_year == 2011 | data_6_s3$terminal_year == 2014,]
data_6_s3_3$ty_order <- 3
data_6_s3_3$if_diff_5yrs <- "N"
data_6_s3_3$interval <- max(data_6_s3_3$terminal_year) - min(data_6_s3_3$terminal_year)

data_6_s3_4 <- data_6_s3[data_6_s3$terminal_year == 2011 | data_6_s3$terminal_year == 2015,]
data_6_s3_4$ty_order <- 4
data_6_s3_4$if_diff_5yrs <- "N"
data_6_s3_4$interval <- max(data_6_s3_4$terminal_year) - min(data_6_s3_4$terminal_year)

data_6_s3_5 <- data_6_s3[data_6_s3$terminal_year == 2011 | data_6_s3$terminal_year == 2016,]
data_6_s3_5$ty_order <- 5
data_6_s3_5$if_diff_5yrs <- "N"
data_6_s3_5$interval <- max(data_6_s3_5$terminal_year) - min(data_6_s3_5$terminal_year)

data_6_s3_6 <- data_6_s3[data_6_s3$terminal_year == 2012 | data_6_s3$terminal_year == 2013,]
data_6_s3_6$ty_order <- 6
data_6_s3_6$if_diff_5yrs <- "N"
data_6_s3_6$interval <- max(data_6_s3_6$terminal_year) - min(data_6_s3_6$terminal_year)
data_6_s3_6$ty <- 2012
data_6_s3_6$ty_minus1 <- data_6_s3_6$ty-1
data_6_s3_6$key_ty <- paste(data_6_s3_6$assessid, data_6_s3_6$ty, sep="_")
data_6_s3_6$key_ty_minus1 <- paste(data_6_s3_6$assessid, data_6_s3_6$ty_minus1, sep="_")
data_6_s3_6[,c(46:47,50:61,64:69)] <- NA

data_6_s3_7 <- data_6_s3[data_6_s3$terminal_year == 2012 | data_6_s3$terminal_year == 2014,]
data_6_s3_7$ty_order <- 7
data_6_s3_7$if_diff_5yrs <- "N"
data_6_s3_7$interval <- max(data_6_s3_7$terminal_year) - min(data_6_s3_7$terminal_year)
data_6_s3_7$ty <- 2012
data_6_s3_7$ty_minus1 <- data_6_s3_7$ty-1
data_6_s3_7$key_ty <- paste(data_6_s3_7$assessid, data_6_s3_7$ty, sep="_")
data_6_s3_7$key_ty_minus1 <- paste(data_6_s3_7$assessid, data_6_s3_7$ty_minus1, sep="_")
data_6_s3_7[,c(46:47,50:61,64:69)] <- NA

data_6_s3_8 <- data_6_s3[data_6_s3$terminal_year == 2012 | data_6_s3$terminal_year == 2015,]
data_6_s3_8$ty_order <- 8
data_6_s3_8$if_diff_5yrs <- "N"
data_6_s3_8$interval <- max(data_6_s3_8$terminal_year) - min(data_6_s3_8$terminal_year)
data_6_s3_8$ty <- 2012
data_6_s3_8$ty_minus1 <- data_6_s3_8$ty-1
data_6_s3_8$key_ty <- paste(data_6_s3_8$assessid, data_6_s3_8$ty, sep="_")
data_6_s3_8$key_ty_minus1 <- paste(data_6_s3_8$assessid, data_6_s3_8$ty_minus1, sep="_")
data_6_s3_8[,c(46:47,50:61,64:69)] <- NA

data_6_s3_9 <- data_6_s3[data_6_s3$terminal_year == 2012 | data_6_s3$terminal_year == 2016,]
data_6_s3_9$ty_order <- 9
data_6_s3_9$if_diff_5yrs <- "N"
data_6_s3_9$interval <- max(data_6_s3_9$terminal_year) - min(data_6_s3_9$terminal_year)
data_6_s3_9$ty <- 2012
data_6_s3_9$ty_minus1 <- data_6_s3_9$ty-1
data_6_s3_9$key_ty <- paste(data_6_s3_9$assessid, data_6_s3_9$ty, sep="_")
data_6_s3_9$key_ty_minus1 <- paste(data_6_s3_9$assessid, data_6_s3_9$ty_minus1, sep="_")
data_6_s3_9[,c(46:47,50:61,64:69)] <- NA

data_6_s3_10 <- data_6_s3[data_6_s3$terminal_year == 2013 | data_6_s3$terminal_year == 2014,]
data_6_s3_10$ty_order <- 10
data_6_s3_10$if_diff_5yrs <- "N"
data_6_s3_10$interval <- max(data_6_s3_10$terminal_year) - min(data_6_s3_10$terminal_year)
data_6_s3_10$ty <- 2013
data_6_s3_10$ty_minus1 <- data_6_s3_10$ty-1
data_6_s3_10$key_ty <- paste(data_6_s3_10$assessid, data_6_s3_10$ty, sep="_")
data_6_s3_10$key_ty_minus1 <- paste(data_6_s3_10$assessid, data_6_s3_10$ty_minus1, sep="_")
data_6_s3_10[,c(46:47,50:61,64:69)] <- NA

data_6_s3_11 <- data_6_s3[data_6_s3$terminal_year == 2013 | data_6_s3$terminal_year == 2015,]
data_6_s3_11$ty_order <- 11
data_6_s3_11$if_diff_5yrs <- "N"
data_6_s3_11$interval <- max(data_6_s3_11$terminal_year) - min(data_6_s3_11$terminal_year)
data_6_s3_11$ty <- 2013
data_6_s3_11$ty_minus1 <- data_6_s3_11$ty-1
data_6_s3_11$key_ty <- paste(data_6_s3_11$assessid, data_6_s3_11$ty, sep="_")
data_6_s3_11$key_ty_minus1 <- paste(data_6_s3_11$assessid, data_6_s3_11$ty_minus1, sep="_")
data_6_s3_11[,c(46:47,50:61,64:69)] <- NA

data_6_s3_12 <- data_6_s3[data_6_s3$terminal_year == 2013 | data_6_s3$terminal_year == 2016,]
data_6_s3_12$ty_order <- 12
data_6_s3_12$if_diff_5yrs <- "N"
data_6_s3_12$interval <- max(data_6_s3_12$terminal_year) - min(data_6_s3_12$terminal_year)
data_6_s3_12$ty <- 2013
data_6_s3_12$ty_minus1 <- data_6_s3_12$ty-1
data_6_s3_12$key_ty <- paste(data_6_s3_12$assessid, data_6_s3_12$ty, sep="_")
data_6_s3_12$key_ty_minus1 <- paste(data_6_s3_12$assessid, data_6_s3_12$ty_minus1, sep="_")
data_6_s3_12[,c(46:47,50:61,64:69)] <- NA

data_6_s3_13 <- data_6_s3[data_6_s3$terminal_year == 2014 | data_6_s3$terminal_year == 2015,]
data_6_s3_13$ty_order <- 13
data_6_s3_13$if_diff_5yrs <- "N"
data_6_s3_13$interval <- max(data_6_s3_13$terminal_year) - min(data_6_s3_13$terminal_year)
data_6_s3_13$ty <- 2014
data_6_s3_13$ty_minus1 <- data_6_s3_13$ty-1
data_6_s3_13$key_ty <- paste(data_6_s3_13$assessid, data_6_s3_13$ty, sep="_")
data_6_s3_13$key_ty_minus1 <- paste(data_6_s3_13$assessid, data_6_s3_13$ty_minus1, sep="_")
data_6_s3_13[,c(46:47,50:61,64:69)] <- NA

data_6_s3_14 <- data_6_s3[data_6_s3$terminal_year == 2014 | data_6_s3$terminal_year == 2016,]
data_6_s3_14$ty_order <- 14
data_6_s3_14$if_diff_5yrs <- "N"
data_6_s3_14$interval <- max(data_6_s3_14$terminal_year) - min(data_6_s3_14$terminal_year)
data_6_s3_14$ty <- 2014
data_6_s3_14$ty_minus1 <- data_6_s3_14$ty-1
data_6_s3_14$key_ty <- paste(data_6_s3_14$assessid, data_6_s3_14$ty, sep="_")
data_6_s3_14$key_ty_minus1 <- paste(data_6_s3_14$assessid, data_6_s3_14$ty_minus1, sep="_")
data_6_s3_14[,c(46:47,50:61,64:69)] <- NA

data_6_s3_15 <- data_6_s3[data_6_s3$terminal_year == 2015 | data_6_s3$terminal_year == 2016,]
data_6_s3_15$ty_order <- 15
data_6_s3_15$if_diff_5yrs <- "N"
data_6_s3_15$interval <- max(data_6_s3_15$terminal_year) - min(data_6_s3_15$terminal_year)
data_6_s3_15$ty <- 2015
data_6_s3_15$ty_minus1 <- data_6_s3_15$ty-1
data_6_s3_15$key_ty <- paste(data_6_s3_15$assessid, data_6_s3_15$ty, sep="_")
data_6_s3_15$key_ty_minus1 <- paste(data_6_s3_15$assessid, data_6_s3_15$ty_minus1, sep="_")
data_6_s3_15[,c(46:47,50:61,64:69)] <- NA

data_6_s3 <- rbind(data_6_s3_1, data_6_s3_2, data_6_s3_3, data_6_s3_4, data_6_s3_5, data_6_s3_6, data_6_s3_7, data_6_s3_8, data_6_s3_9, data_6_s3_10,
                   data_6_s3_11, data_6_s3_12, data_6_s3_13, data_6_s3_14, data_6_s3_15)

# assess 4
data_6_s4 <- data_6_assess[data_6_assess$stockid==ids[4],]

data_6_s4_1 <- data_6_s4[data_6_s4$terminal_year == 2011 | data_6_s4$terminal_year == 2012,]
data_6_s4_1$ty_order <- 1
data_6_s4_1$if_diff_5yrs <- "N"
data_6_s4_1$interval <- max(data_6_s4_1$terminal_year) - min(data_6_s4_1$terminal_year)

data_6_s4_2 <- data_6_s4[data_6_s4$terminal_year == 2011 | data_6_s4$terminal_year == 2013,]
data_6_s4_2$ty_order <- 2
data_6_s4_2$if_diff_5yrs <- "N"
data_6_s4_2$interval <- max(data_6_s4_2$terminal_year) - min(data_6_s4_2$terminal_year)

data_6_s4_3 <- data_6_s4[data_6_s4$terminal_year == 2011 | data_6_s4$terminal_year == 2014,]
data_6_s4_3$ty_order <- 3
data_6_s4_3$if_diff_5yrs <- "N"
data_6_s4_3$interval <- max(data_6_s4_3$terminal_year) - min(data_6_s4_3$terminal_year)

data_6_s4_4 <- data_6_s4[data_6_s4$terminal_year == 2011 | data_6_s4$terminal_year == 2015,]
data_6_s4_4$ty_order <- 4
data_6_s4_4$if_diff_5yrs <- "N"
data_6_s4_4$interval <- max(data_6_s4_4$terminal_year) - min(data_6_s4_4$terminal_year)

data_6_s4_5 <- data_6_s4[data_6_s4$terminal_year == 2011 | data_6_s4$terminal_year == 2016,]
data_6_s4_5$ty_order <- 5
data_6_s4_5$if_diff_5yrs <- "N"
data_6_s4_5$interval <- max(data_6_s4_5$terminal_year) - min(data_6_s4_5$terminal_year)

data_6_s4_6 <- data_6_s4[data_6_s4$terminal_year == 2012 | data_6_s4$terminal_year == 2013,]
data_6_s4_6$ty_order <- 6
data_6_s4_6$if_diff_5yrs <- "N"
data_6_s4_6$interval <- max(data_6_s4_6$terminal_year) - min(data_6_s4_6$terminal_year)
data_6_s4_6$ty <- 2012
data_6_s4_6$ty_minus1 <- data_6_s4_6$ty-1
data_6_s4_6$key_ty <- paste(data_6_s4_6$assessid, data_6_s4_6$ty, sep="_")
data_6_s4_6$key_ty_minus1 <- paste(data_6_s4_6$assessid, data_6_s4_6$ty_minus1, sep="_")
data_6_s4_6[,c(46:47,50:61,64:69)] <- NA

data_6_s4_7 <- data_6_s4[data_6_s4$terminal_year == 2012 | data_6_s4$terminal_year == 2014,]
data_6_s4_7$ty_order <- 7
data_6_s4_7$if_diff_5yrs <- "N"
data_6_s4_7$interval <- max(data_6_s4_7$terminal_year) - min(data_6_s4_7$terminal_year)
data_6_s4_7$ty <- 2012
data_6_s4_7$ty_minus1 <- data_6_s4_7$ty-1
data_6_s4_7$key_ty <- paste(data_6_s4_7$assessid, data_6_s4_7$ty, sep="_")
data_6_s4_7$key_ty_minus1 <- paste(data_6_s4_7$assessid, data_6_s4_7$ty_minus1, sep="_")
data_6_s4_7[,c(46:47,50:61,64:69)] <- NA

data_6_s4_8 <- data_6_s4[data_6_s4$terminal_year == 2012 | data_6_s4$terminal_year == 2015,]
data_6_s4_8$ty_order <- 8
data_6_s4_8$if_diff_5yrs <- "N"
data_6_s4_8$interval <- max(data_6_s4_8$terminal_year) - min(data_6_s4_8$terminal_year)
data_6_s4_8$ty <- 2012
data_6_s4_8$ty_minus1 <- data_6_s4_8$ty-1
data_6_s4_8$key_ty <- paste(data_6_s4_8$assessid, data_6_s4_8$ty, sep="_")
data_6_s4_8$key_ty_minus1 <- paste(data_6_s4_8$assessid, data_6_s4_8$ty_minus1, sep="_")
data_6_s4_8[,c(46:47,50:61,64:69)] <- NA

data_6_s4_9 <- data_6_s4[data_6_s4$terminal_year == 2012 | data_6_s4$terminal_year == 2016,]
data_6_s4_9$ty_order <- 9
data_6_s4_9$if_diff_5yrs <- "N"
data_6_s4_9$interval <- max(data_6_s4_9$terminal_year) - min(data_6_s4_9$terminal_year)
data_6_s4_9$ty <- 2012
data_6_s4_9$ty_minus1 <- data_6_s4_9$ty-1
data_6_s4_9$key_ty <- paste(data_6_s4_9$assessid, data_6_s4_9$ty, sep="_")
data_6_s4_9$key_ty_minus1 <- paste(data_6_s4_9$assessid, data_6_s4_9$ty_minus1, sep="_")
data_6_s4_9[,c(46:47,50:61,64:69)] <- NA

data_6_s4_10 <- data_6_s4[data_6_s4$terminal_year == 2013 | data_6_s4$terminal_year == 2014,]
data_6_s4_10$ty_order <- 10
data_6_s4_10$if_diff_5yrs <- "N"
data_6_s4_10$interval <- max(data_6_s4_10$terminal_year) - min(data_6_s4_10$terminal_year)
data_6_s4_10$ty <- 2013
data_6_s4_10$ty_minus1 <- data_6_s4_10$ty-1
data_6_s4_10$key_ty <- paste(data_6_s4_10$assessid, data_6_s4_10$ty, sep="_")
data_6_s4_10$key_ty_minus1 <- paste(data_6_s4_10$assessid, data_6_s4_10$ty_minus1, sep="_")
data_6_s4_10[,c(46:47,50:61,64:69)] <- NA

data_6_s4_11 <- data_6_s4[data_6_s4$terminal_year == 2013 | data_6_s4$terminal_year == 2015,]
data_6_s4_11$ty_order <- 11
data_6_s4_11$if_diff_5yrs <- "N"
data_6_s4_11$interval <- max(data_6_s4_11$terminal_year) - min(data_6_s4_11$terminal_year)
data_6_s4_11$ty <- 2013
data_6_s4_11$ty_minus1 <- data_6_s4_11$ty-1
data_6_s4_11$key_ty <- paste(data_6_s4_11$assessid, data_6_s4_11$ty, sep="_")
data_6_s4_11$key_ty_minus1 <- paste(data_6_s4_11$assessid, data_6_s4_11$ty_minus1, sep="_")
data_6_s4_11[,c(46:47,50:61,64:69)] <- NA

data_6_s4_12 <- data_6_s4[data_6_s4$terminal_year == 2013 | data_6_s4$terminal_year == 2016,]
data_6_s4_12$ty_order <- 12
data_6_s4_12$if_diff_5yrs <- "N"
data_6_s4_12$interval <- max(data_6_s4_12$terminal_year) - min(data_6_s4_12$terminal_year)
data_6_s4_12$ty <- 2013
data_6_s4_12$ty_minus1 <- data_6_s4_12$ty-1
data_6_s4_12$key_ty <- paste(data_6_s4_12$assessid, data_6_s4_12$ty, sep="_")
data_6_s4_12$key_ty_minus1 <- paste(data_6_s4_12$assessid, data_6_s4_12$ty_minus1, sep="_")
data_6_s4_12[,c(46:47,50:61,64:69)] <- NA

data_6_s4_13 <- data_6_s4[data_6_s4$terminal_year == 2014 | data_6_s4$terminal_year == 2015,]
data_6_s4_13$ty_order <- 13
data_6_s4_13$if_diff_5yrs <- "N"
data_6_s4_13$interval <- max(data_6_s4_13$terminal_year) - min(data_6_s4_13$terminal_year)
data_6_s4_13$ty <- 2014
data_6_s4_13$ty_minus1 <- data_6_s4_13$ty-1
data_6_s4_13$key_ty <- paste(data_6_s4_13$assessid, data_6_s4_13$ty, sep="_")
data_6_s4_13$key_ty_minus1 <- paste(data_6_s4_13$assessid, data_6_s4_13$ty_minus1, sep="_")
data_6_s4_13[,c(46:47,50:61,64:69)] <- NA

data_6_s4_14 <- data_6_s4[data_6_s4$terminal_year == 2014 | data_6_s4$terminal_year == 2016,]
data_6_s4_14$ty_order <- 14
data_6_s4_14$if_diff_5yrs <- "N"
data_6_s4_14$interval <- max(data_6_s4_14$terminal_year) - min(data_6_s4_14$terminal_year)
data_6_s4_14$ty <- 2014
data_6_s4_14$ty_minus1 <- data_6_s4_14$ty-1
data_6_s4_14$key_ty <- paste(data_6_s4_14$assessid, data_6_s4_14$ty, sep="_")
data_6_s4_14$key_ty_minus1 <- paste(data_6_s4_14$assessid, data_6_s4_14$ty_minus1, sep="_")
data_6_s4_14[,c(46:47,50:61,64:69)] <- NA

data_6_s4_15 <- data_6_s4[data_6_s4$terminal_year == 2015 | data_6_s4$terminal_year == 2016,]
data_6_s4_15$ty_order <- 15
data_6_s4_15$if_diff_5yrs <- "N"
data_6_s4_15$interval <- max(data_6_s4_15$terminal_year) - min(data_6_s4_15$terminal_year)
data_6_s4_15$ty <- 2015
data_6_s4_15$ty_minus1 <- data_6_s4_15$ty-1
data_6_s4_15$key_ty <- paste(data_6_s4_15$assessid, data_6_s4_15$ty, sep="_")
data_6_s4_15$key_ty_minus1 <- paste(data_6_s4_15$assessid, data_6_s4_15$ty_minus1, sep="_")
data_6_s4_15[,c(46:47,50:61,64:69)] <- NA

data_6_s4 <- rbind(data_6_s4_1, data_6_s4_2, data_6_s4_3, data_6_s4_4, data_6_s4_5, data_6_s4_6, data_6_s4_7, data_6_s4_8, data_6_s4_9, data_6_s4_10,
                   data_6_s4_11, data_6_s4_12, data_6_s4_13, data_6_s4_14, data_6_s4_15)

# assess 5
data_6_s5 <- data_6_assess[data_6_assess$stockid==ids[5],]

data_6_s5_1 <- data_6_s5[data_6_s5$terminal_year == 2011 | data_6_s5$terminal_year == 2013,]
data_6_s5_1$ty_order <- 1
data_6_s5_1$if_diff_5yrs <- "N"
data_6_s5_1$interval <- max(data_6_s5_1$terminal_year) - min(data_6_s5_1$terminal_year)

data_6_s5_2 <- data_6_s5[data_6_s5$terminal_year == 2011 | data_6_s5$terminal_year == 2014,]
data_6_s5_2$ty_order <- 2
data_6_s5_2$if_diff_5yrs <- "N"
data_6_s5_2$interval <- max(data_6_s5_2$terminal_year) - min(data_6_s5_2$terminal_year)

data_6_s5_3 <- data_6_s5[data_6_s5$terminal_year == 2011 | data_6_s5$terminal_year == 2015,]
data_6_s5_3$ty_order <- 3
data_6_s5_3$if_diff_5yrs <- "N"
data_6_s5_3$interval <- max(data_6_s5_3$terminal_year) - min(data_6_s5_3$terminal_year)

data_6_s5_4 <- data_6_s5[data_6_s5$terminal_year == 2011 | data_6_s5$terminal_year == 2016,]
data_6_s5_4$ty_order <- 4
data_6_s5_4$if_diff_5yrs <- "N"
data_6_s5_4$interval <- max(data_6_s5_4$terminal_year) - min(data_6_s5_4$terminal_year)

data_6_s5_5 <- data_6_s5[data_6_s5$terminal_year == 2011 | data_6_s5$terminal_year == 2017,]
data_6_s5_5$ty_order <- 5
data_6_s5_5$if_diff_5yrs <- "Y"
data_6_s5_5$interval <- max(data_6_s5_5$terminal_year) - min(data_6_s5_5$terminal_year)

data_6_s5_6 <- data_6_s5[data_6_s5$terminal_year == 2013 | data_6_s5$terminal_year == 2014,]
data_6_s5_6$ty_order <- 6
data_6_s5_6$if_diff_5yrs <- "N"
data_6_s5_6$interval <- max(data_6_s5_6$terminal_year) - min(data_6_s5_6$terminal_year)
data_6_s5_6$ty <- 2013
data_6_s5_6$ty_minus1 <- data_6_s5_6$ty-1
data_6_s5_6$key_ty <- paste(data_6_s5_6$assessid, data_6_s5_6$ty, sep="_")
data_6_s5_6$key_ty_minus1 <- paste(data_6_s5_6$assessid, data_6_s5_6$ty_minus1, sep="_")
data_6_s5_6[,c(46:47,50:61,64:69)] <- NA

data_6_s5_7 <- data_6_s5[data_6_s5$terminal_year == 2013 | data_6_s5$terminal_year == 2015,]
data_6_s5_7$ty_order <- 7
data_6_s5_7$if_diff_5yrs <- "N"
data_6_s5_7$interval <- max(data_6_s5_7$terminal_year) - min(data_6_s5_7$terminal_year)
data_6_s5_7$ty <- 2013
data_6_s5_7$ty_minus1 <- data_6_s5_7$ty-1
data_6_s5_7$key_ty <- paste(data_6_s5_7$assessid, data_6_s5_7$ty, sep="_")
data_6_s5_7$key_ty_minus1 <- paste(data_6_s5_7$assessid, data_6_s5_7$ty_minus1, sep="_")
data_6_s5_7[,c(46:47,50:61,64:69)] <- NA

data_6_s5_8 <- data_6_s5[data_6_s5$terminal_year == 2013 | data_6_s5$terminal_year == 2016,]
data_6_s5_8$ty_order <- 8
data_6_s5_8$if_diff_5yrs <- "N"
data_6_s5_8$interval <- max(data_6_s5_8$terminal_year) - min(data_6_s5_8$terminal_year)
data_6_s5_8$ty <- 2013
data_6_s5_8$ty_minus1 <- data_6_s5_8$ty-1
data_6_s5_8$key_ty <- paste(data_6_s5_8$assessid, data_6_s5_8$ty, sep="_")
data_6_s5_8$key_ty_minus1 <- paste(data_6_s5_8$assessid, data_6_s5_8$ty_minus1, sep="_")
data_6_s5_8[,c(46:47,50:61,64:69)] <- NA

data_6_s5_9 <- data_6_s5[data_6_s5$terminal_year == 2013 | data_6_s5$terminal_year == 2017,]
data_6_s5_9$ty_order <- 9
data_6_s5_9$if_diff_5yrs <- "N"
data_6_s5_9$interval <- max(data_6_s5_9$terminal_year) - min(data_6_s5_9$terminal_year)
data_6_s5_9$ty <- 2013
data_6_s5_9$ty_minus1 <- data_6_s5_9$ty-1
data_6_s5_9$key_ty <- paste(data_6_s5_9$assessid, data_6_s5_9$ty, sep="_")
data_6_s5_9$key_ty_minus1 <- paste(data_6_s5_9$assessid, data_6_s5_9$ty_minus1, sep="_")
data_6_s5_9[,c(46:47,50:61,64:69)] <- NA

data_6_s5_10 <- data_6_s5[data_6_s5$terminal_year == 2014 | data_6_s5$terminal_year == 2015,]
data_6_s5_10$ty_order <- 10
data_6_s5_10$if_diff_5yrs <- "N"
data_6_s5_10$interval <- max(data_6_s5_10$terminal_year) - min(data_6_s5_10$terminal_year)
data_6_s5_10$ty <- 2014
data_6_s5_10$ty_minus1 <- data_6_s5_10$ty-1
data_6_s5_10$key_ty <- paste(data_6_s5_10$assessid, data_6_s5_10$ty, sep="_")
data_6_s5_10$key_ty_minus1 <- paste(data_6_s5_10$assessid, data_6_s5_10$ty_minus1, sep="_")
data_6_s5_10[,c(46:47,50:61,64:69)] <- NA

data_6_s5_11 <- data_6_s5[data_6_s5$terminal_year == 2014 | data_6_s5$terminal_year == 2016,]
data_6_s5_11$ty_order <- 11
data_6_s5_11$if_diff_5yrs <- "N"
data_6_s5_11$interval <- max(data_6_s5_11$terminal_year) - min(data_6_s5_11$terminal_year)
data_6_s5_11$ty <- 2014
data_6_s5_11$ty_minus1 <- data_6_s5_11$ty-1
data_6_s5_11$key_ty <- paste(data_6_s5_11$assessid, data_6_s5_11$ty, sep="_")
data_6_s5_11$key_ty_minus1 <- paste(data_6_s5_11$assessid, data_6_s5_11$ty_minus1, sep="_")
data_6_s5_11[,c(46:47,50:61,64:69)] <- NA

data_6_s5_12 <- data_6_s5[data_6_s5$terminal_year == 2014 | data_6_s5$terminal_year == 2017,]
data_6_s5_12$ty_order <- 12
data_6_s5_12$if_diff_5yrs <- "N"
data_6_s5_12$interval <- max(data_6_s5_12$terminal_year) - min(data_6_s5_12$terminal_year)
data_6_s5_12$ty <- 2014
data_6_s5_12$ty_minus1 <- data_6_s5_12$ty-1
data_6_s5_12$key_ty <- paste(data_6_s5_12$assessid, data_6_s5_12$ty, sep="_")
data_6_s5_12$key_ty_minus1 <- paste(data_6_s5_12$assessid, data_6_s5_12$ty_minus1, sep="_")
data_6_s5_12[,c(46:47,50:61,64:69)] <- NA

data_6_s5_13 <- data_6_s5[data_6_s5$terminal_year == 2015 | data_6_s5$terminal_year == 2016,]
data_6_s5_13$ty_order <- 13
data_6_s5_13$if_diff_5yrs <- "N"
data_6_s5_13$interval <- max(data_6_s5_13$terminal_year) - min(data_6_s5_13$terminal_year)
data_6_s5_13$ty <- 2015
data_6_s5_13$ty_minus1 <- data_6_s5_13$ty-1
data_6_s5_13$key_ty <- paste(data_6_s5_13$assessid, data_6_s5_13$ty, sep="_")
data_6_s5_13$key_ty_minus1 <- paste(data_6_s5_13$assessid, data_6_s5_13$ty_minus1, sep="_")
data_6_s5_13[,c(46:47,50:61,64:69)] <- NA

data_6_s5_14 <- data_6_s5[data_6_s5$terminal_year == 2015 | data_6_s5$terminal_year == 2017,]
data_6_s5_14$ty_order <- 14
data_6_s5_14$if_diff_5yrs <- "N"
data_6_s5_14$interval <- max(data_6_s5_14$terminal_year) - min(data_6_s5_14$terminal_year)
data_6_s5_14$ty <- 2015
data_6_s5_14$ty_minus1 <- data_6_s5_14$ty-1
data_6_s5_14$key_ty <- paste(data_6_s5_14$assessid, data_6_s5_14$ty, sep="_")
data_6_s5_14$key_ty_minus1 <- paste(data_6_s5_14$assessid, data_6_s5_14$ty_minus1, sep="_")
data_6_s5_14[,c(46:47,50:61,64:69)] <- NA

data_6_s5_15 <- data_6_s5[data_6_s5$terminal_year == 2016 | data_6_s5$terminal_year == 2017,]
data_6_s5_15$ty_order <- 15
data_6_s5_15$if_diff_5yrs <- "N"
data_6_s5_15$interval <- max(data_6_s5_15$terminal_year) - min(data_6_s5_15$terminal_year)
data_6_s5_15$ty <- 2016
data_6_s5_15$ty_minus1 <- data_6_s5_15$ty-1
data_6_s5_15$key_ty <- paste(data_6_s5_15$assessid, data_6_s5_15$ty, sep="_")
data_6_s5_15$key_ty_minus1 <- paste(data_6_s5_15$assessid, data_6_s5_15$ty_minus1, sep="_")
data_6_s5_15[,c(46:47,50:61,64:69)] <- NA

data_6_s5 <- rbind(data_6_s5_1, data_6_s5_2, data_6_s5_3, data_6_s5_4, data_6_s5_5, data_6_s5_6, data_6_s5_7, data_6_s5_8, data_6_s5_9, data_6_s5_10,
                   data_6_s5_11, data_6_s5_12, data_6_s5_13, data_6_s5_14, data_6_s5_15)

# assess 6
data_6_s6 <- data_6_assess[data_6_assess$stockid==ids[6],]

data_6_s6_1 <- data_6_s6[data_6_s6$terminal_year == 2011 | data_6_s6$terminal_year == 2012,]
data_6_s6_1$ty_order <- 1
data_6_s6_1$if_diff_5yrs <- "N"
data_6_s6_1$interval <- max(data_6_s6_1$terminal_year) - min(data_6_s6_1$terminal_year)

data_6_s6_2 <- data_6_s6[data_6_s6$terminal_year == 2011 | data_6_s6$terminal_year == 2013,]
data_6_s6_2$ty_order <- 2
data_6_s6_2$if_diff_5yrs <- "N"
data_6_s6_2$interval <- max(data_6_s6_2$terminal_year) - min(data_6_s6_2$terminal_year)

data_6_s6_3 <- data_6_s6[data_6_s6$terminal_year == 2011 | data_6_s6$terminal_year == 2014,]
data_6_s6_3$ty_order <- 3
data_6_s6_3$if_diff_5yrs <- "N"
data_6_s6_3$interval <- max(data_6_s6_3$terminal_year) - min(data_6_s6_3$terminal_year)

data_6_s6_4 <- data_6_s6[data_6_s6$terminal_year == 2011 | data_6_s6$terminal_year == 2015,]
data_6_s6_4$ty_order <- 4
data_6_s6_4$if_diff_5yrs <- "N"
data_6_s6_4$interval <- max(data_6_s6_4$terminal_year) - min(data_6_s6_4$terminal_year)

data_6_s6_5 <- data_6_s6[data_6_s6$terminal_year == 2011 | data_6_s6$terminal_year == 2016,]
data_6_s6_5$ty_order <- 5
data_6_s6_5$if_diff_5yrs <- "N"
data_6_s6_5$interval <- max(data_6_s6_5$terminal_year) - min(data_6_s6_5$terminal_year)

data_6_s6_6 <- data_6_s6[data_6_s6$terminal_year == 2012 | data_6_s6$terminal_year == 2013,]
data_6_s6_6$ty_order <- 6
data_6_s6_6$if_diff_5yrs <- "N"
data_6_s6_6$interval <- max(data_6_s6_6$terminal_year) - min(data_6_s6_6$terminal_year)
data_6_s6_6$ty <- 2012
data_6_s6_6$ty_minus1 <- data_6_s6_6$ty-1
data_6_s6_6$key_ty <- paste(data_6_s6_6$assessid, data_6_s6_6$ty, sep="_")
data_6_s6_6$key_ty_minus1 <- paste(data_6_s6_6$assessid, data_6_s6_6$ty_minus1, sep="_")
data_6_s6_6[,c(46:47,50:61,64:69)] <- NA

data_6_s6_7 <- data_6_s6[data_6_s6$terminal_year == 2012 | data_6_s6$terminal_year == 2014,]
data_6_s6_7$ty_order <- 7
data_6_s6_7$if_diff_5yrs <- "N"
data_6_s6_7$interval <- max(data_6_s6_7$terminal_year) - min(data_6_s6_7$terminal_year)
data_6_s6_7$ty <- 2012
data_6_s6_7$ty_minus1 <- data_6_s6_7$ty-1
data_6_s6_7$key_ty <- paste(data_6_s6_7$assessid, data_6_s6_7$ty, sep="_")
data_6_s6_7$key_ty_minus1 <- paste(data_6_s6_7$assessid, data_6_s6_7$ty_minus1, sep="_")
data_6_s6_7[,c(46:47,50:61,64:69)] <- NA

data_6_s6_8 <- data_6_s6[data_6_s6$terminal_year == 2012 | data_6_s6$terminal_year == 2015,]
data_6_s6_8$ty_order <- 8
data_6_s6_8$if_diff_5yrs <- "N"
data_6_s6_8$interval <- max(data_6_s6_8$terminal_year) - min(data_6_s6_8$terminal_year)
data_6_s6_8$ty <- 2012
data_6_s6_8$ty_minus1 <- data_6_s6_8$ty-1
data_6_s6_8$key_ty <- paste(data_6_s6_8$assessid, data_6_s6_8$ty, sep="_")
data_6_s6_8$key_ty_minus1 <- paste(data_6_s6_8$assessid, data_6_s6_8$ty_minus1, sep="_")
data_6_s6_8[,c(46:47,50:61,64:69)] <- NA

data_6_s6_9 <- data_6_s6[data_6_s6$terminal_year == 2012 | data_6_s6$terminal_year == 2016,]
data_6_s6_9$ty_order <- 9
data_6_s6_9$if_diff_5yrs <- "N"
data_6_s6_9$interval <- max(data_6_s6_9$terminal_year) - min(data_6_s6_9$terminal_year)
data_6_s6_9$ty <- 2012
data_6_s6_9$ty_minus1 <- data_6_s6_9$ty-1
data_6_s6_9$key_ty <- paste(data_6_s6_9$assessid, data_6_s6_9$ty, sep="_")
data_6_s6_9$key_ty_minus1 <- paste(data_6_s6_9$assessid, data_6_s6_9$ty_minus1, sep="_")
data_6_s6_9[,c(46:47,50:61,64:69)] <- NA

data_6_s6_10 <- data_6_s6[data_6_s6$terminal_year == 2013 | data_6_s6$terminal_year == 2014,]
data_6_s6_10$ty_order <- 10
data_6_s6_10$if_diff_5yrs <- "N"
data_6_s6_10$interval <- max(data_6_s6_10$terminal_year) - min(data_6_s6_10$terminal_year)
data_6_s6_10$ty <- 2013
data_6_s6_10$ty_minus1 <- data_6_s6_10$ty-1
data_6_s6_10$key_ty <- paste(data_6_s6_10$assessid, data_6_s6_10$ty, sep="_")
data_6_s6_10$key_ty_minus1 <- paste(data_6_s6_10$assessid, data_6_s6_10$ty_minus1, sep="_")
data_6_s6_10[,c(46:47,50:61,64:69)] <- NA

data_6_s6_11 <- data_6_s6[data_6_s6$terminal_year == 2013 | data_6_s6$terminal_year == 2015,]
data_6_s6_11$ty_order <- 11
data_6_s6_11$if_diff_5yrs <- "N"
data_6_s6_11$interval <- max(data_6_s6_11$terminal_year) - min(data_6_s6_11$terminal_year)
data_6_s6_11$ty <- 2013
data_6_s6_11$ty_minus1 <- data_6_s6_11$ty-1
data_6_s6_11$key_ty <- paste(data_6_s6_11$assessid, data_6_s6_11$ty, sep="_")
data_6_s6_11$key_ty_minus1 <- paste(data_6_s6_11$assessid, data_6_s6_11$ty_minus1, sep="_")
data_6_s6_11[,c(46:47,50:61,64:69)] <- NA

data_6_s6_12 <- data_6_s6[data_6_s6$terminal_year == 2013 | data_6_s6$terminal_year == 2016,]
data_6_s6_12$ty_order <- 12
data_6_s6_12$if_diff_5yrs <- "N"
data_6_s6_12$interval <- max(data_6_s6_12$terminal_year) - min(data_6_s6_12$terminal_year)
data_6_s6_12$ty <- 2013
data_6_s6_12$ty_minus1 <- data_6_s6_12$ty-1
data_6_s6_12$key_ty <- paste(data_6_s6_12$assessid, data_6_s6_12$ty, sep="_")
data_6_s6_12$key_ty_minus1 <- paste(data_6_s6_12$assessid, data_6_s6_12$ty_minus1, sep="_")
data_6_s6_12[,c(46:47,50:61,64:69)] <- NA

data_6_s6_13 <- data_6_s6[data_6_s6$terminal_year == 2014 | data_6_s6$terminal_year == 2015,]
data_6_s6_13$ty_order <- 13
data_6_s6_13$if_diff_5yrs <- "N"
data_6_s6_13$interval <- max(data_6_s6_13$terminal_year) - min(data_6_s6_13$terminal_year)
data_6_s6_13$ty <- 2014
data_6_s6_13$ty_minus1 <- data_6_s6_13$ty-1
data_6_s6_13$key_ty <- paste(data_6_s6_13$assessid, data_6_s6_13$ty, sep="_")
data_6_s6_13$key_ty_minus1 <- paste(data_6_s6_13$assessid, data_6_s6_13$ty_minus1, sep="_")
data_6_s6_13[,c(46:47,50:61,64:69)] <- NA

data_6_s6_14 <- data_6_s6[data_6_s6$terminal_year == 2014 | data_6_s6$terminal_year == 2016,]
data_6_s6_14$ty_order <- 14
data_6_s6_14$if_diff_5yrs <- "N"
data_6_s6_14$interval <- max(data_6_s6_14$terminal_year) - min(data_6_s6_14$terminal_year)
data_6_s6_14$ty <- 2014
data_6_s6_14$ty_minus1 <- data_6_s6_14$ty-1
data_6_s6_14$key_ty <- paste(data_6_s6_14$assessid, data_6_s6_14$ty, sep="_")
data_6_s6_14$key_ty_minus1 <- paste(data_6_s6_14$assessid, data_6_s6_14$ty_minus1, sep="_")
data_6_s6_14[,c(46:47,50:61,64:69)] <- NA

data_6_s6_15 <- data_6_s6[data_6_s6$terminal_year == 2015 | data_6_s6$terminal_year == 2016,]
data_6_s6_15$ty_order <- 15
data_6_s6_15$if_diff_5yrs <- "N"
data_6_s6_15$interval <- max(data_6_s6_15$terminal_year) - min(data_6_s6_15$terminal_year)
data_6_s6_15$ty <- 2015
data_6_s6_15$ty_minus1 <- data_6_s6_15$ty-1
data_6_s6_15$key_ty <- paste(data_6_s6_15$assessid, data_6_s6_15$ty, sep="_")
data_6_s6_15$key_ty_minus1 <- paste(data_6_s6_15$assessid, data_6_s6_15$ty_minus1, sep="_")
data_6_s6_15[,c(46:47,50:61,64:69)] <- NA

data_6_s6 <- rbind(data_6_s6_1, data_6_s6_2, data_6_s6_3, data_6_s6_4, data_6_s6_5, data_6_s6_6, data_6_s6_7, data_6_s6_8, data_6_s6_9, data_6_s6_10,
                   data_6_s6_11, data_6_s6_12, data_6_s6_13, data_6_s6_14, data_6_s6_15)

# assess 7
data_6_s7 <- data_6_assess[data_6_assess$stockid==ids[7],]

data_6_s7_1 <- data_6_s7[data_6_s7$terminal_year == 2010 | data_6_s7$terminal_year == 2012,]
data_6_s7_1$ty_order <- 1
data_6_s7_1$if_diff_5yrs <- "N"
data_6_s7_1$interval <- max(data_6_s7_1$terminal_year) - min(data_6_s7_1$terminal_year)

data_6_s7_2 <- data_6_s7[data_6_s7$terminal_year == 2010 | data_6_s7$terminal_year == 2013,]
data_6_s7_2$ty_order <- 2
data_6_s7_2$if_diff_5yrs <- "N"
data_6_s7_2$interval <- max(data_6_s7_2$terminal_year) - min(data_6_s7_2$terminal_year)

data_6_s7_3 <- data_6_s7[data_6_s7$terminal_year == 2010 | data_6_s7$terminal_year == 2014,]
data_6_s7_3$ty_order <- 3
data_6_s7_3$if_diff_5yrs <- "N"
data_6_s7_3$interval <- max(data_6_s7_3$terminal_year) - min(data_6_s7_3$terminal_year)

data_6_s7_4 <- data_6_s7[data_6_s7$terminal_year == 2010 | data_6_s7$terminal_year == 2015,]
data_6_s7_4$ty_order <- 4
data_6_s7_4$if_diff_5yrs <- "N"
data_6_s7_4$interval <- max(data_6_s7_4$terminal_year) - min(data_6_s7_4$terminal_year)

data_6_s7_5 <- data_6_s7[data_6_s7$terminal_year == 2010 | data_6_s7$terminal_year == 2016,]
data_6_s7_5$ty_order <- 5
data_6_s7_5$if_diff_5yrs <- "Y"
data_6_s7_5$interval <- max(data_6_s7_5$terminal_year) - min(data_6_s7_5$terminal_year)

data_6_s7_6 <- data_6_s7[data_6_s7$terminal_year == 2012 | data_6_s7$terminal_year == 2013,]
data_6_s7_6$ty_order <- 6
data_6_s7_6$if_diff_5yrs <- "N"
data_6_s7_6$interval <- max(data_6_s7_6$terminal_year) - min(data_6_s7_6$terminal_year)
data_6_s7_6$ty <- 2012
data_6_s7_6$ty_minus1 <- data_6_s7_6$ty-1
data_6_s7_6$key_ty <- paste(data_6_s7_6$assessid, data_6_s7_6$ty, sep="_")
data_6_s7_6$key_ty_minus1 <- paste(data_6_s7_6$assessid, data_6_s7_6$ty_minus1, sep="_")
data_6_s7_6[,c(46:47,50:61,64:69)] <- NA

data_6_s7_7 <- data_6_s7[data_6_s7$terminal_year == 2012 | data_6_s7$terminal_year == 2014,]
data_6_s7_7$ty_order <- 7
data_6_s7_7$if_diff_5yrs <- "N"
data_6_s7_7$interval <- max(data_6_s7_7$terminal_year) - min(data_6_s7_7$terminal_year)
data_6_s7_7$ty <- 2012
data_6_s7_7$ty_minus1 <- data_6_s7_7$ty-1
data_6_s7_7$key_ty <- paste(data_6_s7_7$assessid, data_6_s7_7$ty, sep="_")
data_6_s7_7$key_ty_minus1 <- paste(data_6_s7_7$assessid, data_6_s7_7$ty_minus1, sep="_")
data_6_s7_7[,c(46:47,50:61,64:69)] <- NA

data_6_s7_8 <- data_6_s7[data_6_s7$terminal_year == 2012 | data_6_s7$terminal_year == 2015,]
data_6_s7_8$ty_order <- 8
data_6_s7_8$if_diff_5yrs <- "N"
data_6_s7_8$interval <- max(data_6_s7_8$terminal_year) - min(data_6_s7_8$terminal_year)
data_6_s7_8$ty <- 2012
data_6_s7_8$ty_minus1 <- data_6_s7_8$ty-1
data_6_s7_8$key_ty <- paste(data_6_s7_8$assessid, data_6_s7_8$ty, sep="_")
data_6_s7_8$key_ty_minus1 <- paste(data_6_s7_8$assessid, data_6_s7_8$ty_minus1, sep="_")
data_6_s7_8[,c(46:47,50:61,64:69)] <- NA

data_6_s7_9 <- data_6_s7[data_6_s7$terminal_year == 2012 | data_6_s7$terminal_year == 2016,]
data_6_s7_9$ty_order <- 9
data_6_s7_9$if_diff_5yrs <- "N"
data_6_s7_9$interval <- max(data_6_s7_9$terminal_year) - min(data_6_s7_9$terminal_year)
data_6_s7_9$ty <- 2012
data_6_s7_9$ty_minus1 <- data_6_s7_9$ty-1
data_6_s7_9$key_ty <- paste(data_6_s7_9$assessid, data_6_s7_9$ty, sep="_")
data_6_s7_9$key_ty_minus1 <- paste(data_6_s7_9$assessid, data_6_s7_9$ty_minus1, sep="_")
data_6_s7_9[,c(46:47,50:61,64:69)] <- NA

data_6_s7_10 <- data_6_s7[data_6_s7$terminal_year == 2013 | data_6_s7$terminal_year == 2014,]
data_6_s7_10$ty_order <- 10
data_6_s7_10$if_diff_5yrs <- "N"
data_6_s7_10$interval <- max(data_6_s7_10$terminal_year) - min(data_6_s7_10$terminal_year)
data_6_s7_10$ty <- 2013
data_6_s7_10$ty_minus1 <- data_6_s7_10$ty-1
data_6_s7_10$key_ty <- paste(data_6_s7_10$assessid, data_6_s7_10$ty, sep="_")
data_6_s7_10$key_ty_minus1 <- paste(data_6_s7_10$assessid, data_6_s7_10$ty_minus1, sep="_")
data_6_s7_10[,c(46:47,50:61,64:69)] <- NA

data_6_s7_11 <- data_6_s7[data_6_s7$terminal_year == 2013 | data_6_s7$terminal_year == 2015,]
data_6_s7_11$ty_order <- 11
data_6_s7_11$if_diff_5yrs <- "N"
data_6_s7_11$interval <- max(data_6_s7_11$terminal_year) - min(data_6_s7_11$terminal_year)
data_6_s7_11$ty <- 2013
data_6_s7_11$ty_minus1 <- data_6_s7_11$ty-1
data_6_s7_11$key_ty <- paste(data_6_s7_11$assessid, data_6_s7_11$ty, sep="_")
data_6_s7_11$key_ty_minus1 <- paste(data_6_s7_11$assessid, data_6_s7_11$ty_minus1, sep="_")
data_6_s7_11[,c(46:47,50:61,64:69)] <- NA

data_6_s7_12 <- data_6_s7[data_6_s7$terminal_year == 2013 | data_6_s7$terminal_year == 2016,]
data_6_s7_12$ty_order <- 12
data_6_s7_12$if_diff_5yrs <- "N"
data_6_s7_12$interval <- max(data_6_s7_12$terminal_year) - min(data_6_s7_12$terminal_year)
data_6_s7_12$ty <- 2013
data_6_s7_12$ty_minus1 <- data_6_s7_12$ty-1
data_6_s7_12$key_ty <- paste(data_6_s7_12$assessid, data_6_s7_12$ty, sep="_")
data_6_s7_12$key_ty_minus1 <- paste(data_6_s7_12$assessid, data_6_s7_12$ty_minus1, sep="_")
data_6_s7_12[,c(46:47,50:61,64:69)] <- NA

data_6_s7_13 <- data_6_s7[data_6_s7$terminal_year == 2014 | data_6_s7$terminal_year == 2015,]
data_6_s7_13$ty_order <- 13
data_6_s7_13$if_diff_5yrs <- "N"
data_6_s7_13$interval <- max(data_6_s7_13$terminal_year) - min(data_6_s7_13$terminal_year)
data_6_s7_13$ty <- 2014
data_6_s7_13$ty_minus1 <- data_6_s7_13$ty-1
data_6_s7_13$key_ty <- paste(data_6_s7_13$assessid, data_6_s7_13$ty, sep="_")
data_6_s7_13$key_ty_minus1 <- paste(data_6_s7_13$assessid, data_6_s7_13$ty_minus1, sep="_")
data_6_s7_13[,c(46:47,50:61,64:69)] <- NA

data_6_s7_14 <- data_6_s7[data_6_s7$terminal_year == 2014 | data_6_s7$terminal_year == 2016,]
data_6_s7_14$ty_order <- 14
data_6_s7_14$if_diff_5yrs <- "N"
data_6_s7_14$interval <- max(data_6_s7_14$terminal_year) - min(data_6_s7_14$terminal_year)
data_6_s7_14$ty <- 2014
data_6_s7_14$ty_minus1 <- data_6_s7_14$ty-1
data_6_s7_14$key_ty <- paste(data_6_s7_14$assessid, data_6_s7_14$ty, sep="_")
data_6_s7_14$key_ty_minus1 <- paste(data_6_s7_14$assessid, data_6_s7_14$ty_minus1, sep="_")
data_6_s7_14[,c(46:47,50:61,64:69)] <- NA

data_6_s7_15 <- data_6_s7[data_6_s7$terminal_year == 2015 | data_6_s7$terminal_year == 2016,]
data_6_s7_15$ty_order <- 15
data_6_s7_15$if_diff_5yrs <- "N"
data_6_s7_15$interval <- max(data_6_s7_15$terminal_year) - min(data_6_s7_15$terminal_year)
data_6_s7_15$ty <- 2015
data_6_s7_15$ty_minus1 <- data_6_s7_15$ty-1
data_6_s7_15$key_ty <- paste(data_6_s7_15$assessid, data_6_s7_15$ty, sep="_")
data_6_s7_15$key_ty_minus1 <- paste(data_6_s7_15$assessid, data_6_s7_15$ty_minus1, sep="_")
data_6_s7_15[,c(46:47,50:61,64:69)] <- NA

data_6_s7 <- rbind(data_6_s7_1, data_6_s7_2, data_6_s7_3, data_6_s7_4, data_6_s7_5, data_6_s7_6, data_6_s7_7, data_6_s7_8, data_6_s7_9, data_6_s7_10,
                   data_6_s7_11, data_6_s7_12, data_6_s7_13, data_6_s7_14, data_6_s7_15)

# assess 8
data_6_s8 <- data_6_assess[data_6_assess$stockid==ids[8],]

data_6_s8_1 <- data_6_s8[data_6_s8$terminal_year == 2011 | data_6_s8$terminal_year == 2012,]
data_6_s8_1$ty_order <- 1
data_6_s8_1$if_diff_5yrs <- "N"
data_6_s8_1$interval <- max(data_6_s8_1$terminal_year) - min(data_6_s8_1$terminal_year)

data_6_s8_2 <- data_6_s8[data_6_s8$terminal_year == 2011 | data_6_s8$terminal_year == 2013,]
data_6_s8_2$ty_order <- 2
data_6_s8_2$if_diff_5yrs <- "N"
data_6_s8_2$interval <- max(data_6_s8_2$terminal_year) - min(data_6_s8_2$terminal_year)

data_6_s8_3 <- data_6_s8[data_6_s8$terminal_year == 2011 | data_6_s8$terminal_year == 2014,]
data_6_s8_3$ty_order <- 3
data_6_s8_3$if_diff_5yrs <- "N"
data_6_s8_3$interval <- max(data_6_s8_3$terminal_year) - min(data_6_s8_3$terminal_year)

data_6_s8_4 <- data_6_s8[data_6_s8$terminal_year == 2011 | data_6_s8$terminal_year == 2015,]
data_6_s8_4$ty_order <- 4
data_6_s8_4$if_diff_5yrs <- "N"
data_6_s8_4$interval <- max(data_6_s8_4$terminal_year) - min(data_6_s8_4$terminal_year)

data_6_s8_5 <- data_6_s8[data_6_s8$terminal_year == 2011 | data_6_s8$terminal_year == 2016,]
data_6_s8_5$ty_order <- 5
data_6_s8_5$if_diff_5yrs <- "N"
data_6_s8_5$interval <- max(data_6_s8_5$terminal_year) - min(data_6_s8_5$terminal_year)

data_6_s8_6 <- data_6_s8[data_6_s8$terminal_year == 2012 | data_6_s8$terminal_year == 2013,]
data_6_s8_6$ty_order <- 6
data_6_s8_6$if_diff_5yrs <- "N"
data_6_s8_6$interval <- max(data_6_s8_6$terminal_year) - min(data_6_s8_6$terminal_year)
data_6_s8_6$ty <- 2012
data_6_s8_6$ty_minus1 <- data_6_s8_6$ty-1
data_6_s8_6$key_ty <- paste(data_6_s8_6$assessid, data_6_s8_6$ty, sep="_")
data_6_s8_6$key_ty_minus1 <- paste(data_6_s8_6$assessid, data_6_s8_6$ty_minus1, sep="_")
data_6_s8_6[,c(46:47,50:61,64:69)] <- NA

data_6_s8_7 <- data_6_s8[data_6_s8$terminal_year == 2012 | data_6_s8$terminal_year == 2014,]
data_6_s8_7$ty_order <- 7
data_6_s8_7$if_diff_5yrs <- "N"
data_6_s8_7$interval <- max(data_6_s8_7$terminal_year) - min(data_6_s8_7$terminal_year)
data_6_s8_7$ty <- 2012
data_6_s8_7$ty_minus1 <- data_6_s8_7$ty-1
data_6_s8_7$key_ty <- paste(data_6_s8_7$assessid, data_6_s8_7$ty, sep="_")
data_6_s8_7$key_ty_minus1 <- paste(data_6_s8_7$assessid, data_6_s8_7$ty_minus1, sep="_")
data_6_s8_7[,c(46:47,50:61,64:69)] <- NA

data_6_s8_8 <- data_6_s8[data_6_s8$terminal_year == 2012 | data_6_s8$terminal_year == 2015,]
data_6_s8_8$ty_order <- 8
data_6_s8_8$if_diff_5yrs <- "N"
data_6_s8_8$interval <- max(data_6_s8_8$terminal_year) - min(data_6_s8_8$terminal_year)
data_6_s8_8$ty <- 2012
data_6_s8_8$ty_minus1 <- data_6_s8_8$ty-1
data_6_s8_8$key_ty <- paste(data_6_s8_8$assessid, data_6_s8_8$ty, sep="_")
data_6_s8_8$key_ty_minus1 <- paste(data_6_s8_8$assessid, data_6_s8_8$ty_minus1, sep="_")
data_6_s8_8[,c(46:47,50:61,64:69)] <- NA

data_6_s8_9 <- data_6_s8[data_6_s8$terminal_year == 2012 | data_6_s8$terminal_year == 2016,]
data_6_s8_9$ty_order <- 9
data_6_s8_9$if_diff_5yrs <- "N"
data_6_s8_9$interval <- max(data_6_s8_9$terminal_year) - min(data_6_s8_9$terminal_year)
data_6_s8_9$ty <- 2012
data_6_s8_9$ty_minus1 <- data_6_s8_9$ty-1
data_6_s8_9$key_ty <- paste(data_6_s8_9$assessid, data_6_s8_9$ty, sep="_")
data_6_s8_9$key_ty_minus1 <- paste(data_6_s8_9$assessid, data_6_s8_9$ty_minus1, sep="_")
data_6_s8_9[,c(46:47,50:61,64:69)] <- NA

data_6_s8_10 <- data_6_s8[data_6_s8$terminal_year == 2013 | data_6_s8$terminal_year == 2014,]
data_6_s8_10$ty_order <- 10
data_6_s8_10$if_diff_5yrs <- "N"
data_6_s8_10$interval <- max(data_6_s8_10$terminal_year) - min(data_6_s8_10$terminal_year)
data_6_s8_10$ty <- 2013
data_6_s8_10$ty_minus1 <- data_6_s8_10$ty-1
data_6_s8_10$key_ty <- paste(data_6_s8_10$assessid, data_6_s8_10$ty, sep="_")
data_6_s8_10$key_ty_minus1 <- paste(data_6_s8_10$assessid, data_6_s8_10$ty_minus1, sep="_")
data_6_s8_10[,c(46:47,50:61,64:69)] <- NA

data_6_s8_11 <- data_6_s8[data_6_s8$terminal_year == 2013 | data_6_s8$terminal_year == 2015,]
data_6_s8_11$ty_order <- 11
data_6_s8_11$if_diff_5yrs <- "N"
data_6_s8_11$interval <- max(data_6_s8_11$terminal_year) - min(data_6_s8_11$terminal_year)
data_6_s8_11$ty <- 2013
data_6_s8_11$ty_minus1 <- data_6_s8_11$ty-1
data_6_s8_11$key_ty <- paste(data_6_s8_11$assessid, data_6_s8_11$ty, sep="_")
data_6_s8_11$key_ty_minus1 <- paste(data_6_s8_11$assessid, data_6_s8_11$ty_minus1, sep="_")
data_6_s8_11[,c(46:47,50:61,64:69)] <- NA

data_6_s8_12 <- data_6_s8[data_6_s8$terminal_year == 2013 | data_6_s8$terminal_year == 2016,]
data_6_s8_12$ty_order <- 12
data_6_s8_12$if_diff_5yrs <- "N"
data_6_s8_12$interval <- max(data_6_s8_12$terminal_year) - min(data_6_s8_12$terminal_year)
data_6_s8_12$ty <- 2013
data_6_s8_12$ty_minus1 <- data_6_s8_12$ty-1
data_6_s8_12$key_ty <- paste(data_6_s8_12$assessid, data_6_s8_12$ty, sep="_")
data_6_s8_12$key_ty_minus1 <- paste(data_6_s8_12$assessid, data_6_s8_12$ty_minus1, sep="_")
data_6_s8_12[,c(46:47,50:61,64:69)] <- NA

data_6_s8_13 <- data_6_s8[data_6_s8$terminal_year == 2014 | data_6_s8$terminal_year == 2015,]
data_6_s8_13$ty_order <- 13
data_6_s8_13$if_diff_5yrs <- "N"
data_6_s8_13$interval <- max(data_6_s8_13$terminal_year) - min(data_6_s8_13$terminal_year)
data_6_s8_13$ty <- 2014
data_6_s8_13$ty_minus1 <- data_6_s8_13$ty-1
data_6_s8_13$key_ty <- paste(data_6_s8_13$assessid, data_6_s8_13$ty, sep="_")
data_6_s8_13$key_ty_minus1 <- paste(data_6_s8_13$assessid, data_6_s8_13$ty_minus1, sep="_")
data_6_s8_13[,c(46:47,50:61,64:69)] <- NA

data_6_s8_14 <- data_6_s8[data_6_s8$terminal_year == 2014 | data_6_s8$terminal_year == 2016,]
data_6_s8_14$ty_order <- 14
data_6_s8_14$if_diff_5yrs <- "N"
data_6_s8_14$interval <- max(data_6_s8_14$terminal_year) - min(data_6_s8_14$terminal_year)
data_6_s8_14$ty <- 2014
data_6_s8_14$ty_minus1 <- data_6_s8_14$ty-1
data_6_s8_14$key_ty <- paste(data_6_s8_14$assessid, data_6_s8_14$ty, sep="_")
data_6_s8_14$key_ty_minus1 <- paste(data_6_s8_14$assessid, data_6_s8_14$ty_minus1, sep="_")
data_6_s8_14[,c(46:47,50:61,64:69)] <- NA

data_6_s8_15 <- data_6_s8[data_6_s8$terminal_year == 2015 | data_6_s8$terminal_year == 2016,]
data_6_s8_15$ty_order <- 15
data_6_s8_15$if_diff_5yrs <- "N"
data_6_s8_15$interval <- max(data_6_s8_15$terminal_year) - min(data_6_s8_15$terminal_year)
data_6_s8_15$ty <- 2015
data_6_s8_15$ty_minus1 <- data_6_s8_15$ty-1
data_6_s8_15$key_ty <- paste(data_6_s8_15$assessid, data_6_s8_15$ty, sep="_")
data_6_s8_15$key_ty_minus1 <- paste(data_6_s8_15$assessid, data_6_s8_15$ty_minus1, sep="_")
data_6_s8_15[,c(46:47,50:61,64:69)] <- NA

data_6_s8 <- rbind(data_6_s8_1, data_6_s8_2, data_6_s8_3, data_6_s8_4, data_6_s8_5, data_6_s8_6, data_6_s8_7, data_6_s8_8, data_6_s8_9, data_6_s8_10,
                   data_6_s8_11, data_6_s8_12, data_6_s8_13, data_6_s8_14, data_6_s8_15)

# assess 9
data_6_s9 <- data_6_assess[data_6_assess$stockid==ids[9],]

data_6_s9_1 <- data_6_s9[data_6_s9$terminal_year == 2012 | data_6_s9$terminal_year == 2013,]
data_6_s9_1$ty_order <- 1
data_6_s9_1$if_diff_5yrs <- "N"
data_6_s9_1$interval <- max(data_6_s9_1$terminal_year) - min(data_6_s9_1$terminal_year)

data_6_s9_2 <- data_6_s9[data_6_s9$terminal_year == 2012 | data_6_s9$terminal_year == 2014,]
data_6_s9_2$ty_order <- 2
data_6_s9_2$if_diff_5yrs <- "N"
data_6_s9_2$interval <- max(data_6_s9_2$terminal_year) - min(data_6_s9_2$terminal_year)

data_6_s9_3 <- data_6_s9[data_6_s9$terminal_year == 2012 | data_6_s9$terminal_year == 2015,]
data_6_s9_3$ty_order <- 3
data_6_s9_3$if_diff_5yrs <- "N"
data_6_s9_3$interval <- max(data_6_s9_3$terminal_year) - min(data_6_s9_3$terminal_year)

data_6_s9_4 <- data_6_s9[data_6_s9$terminal_year == 2012 | data_6_s9$terminal_year == 2016,]
data_6_s9_4$ty_order <- 4
data_6_s9_4$if_diff_5yrs <- "N"
data_6_s9_4$interval <- max(data_6_s9_4$terminal_year) - min(data_6_s9_4$terminal_year)

data_6_s9_5 <- data_6_s9[data_6_s9$terminal_year == 2012 | data_6_s9$terminal_year == 2017,]
data_6_s9_5$ty_order <- 5
data_6_s9_5$if_diff_5yrs <- "N"
data_6_s9_5$interval <- max(data_6_s9_5$terminal_year) - min(data_6_s9_5$terminal_year)

data_6_s9_6 <- data_6_s9[data_6_s9$terminal_year == 2013 | data_6_s9$terminal_year == 2014,]
data_6_s9_6$ty_order <- 6
data_6_s9_6$if_diff_5yrs <- "N"
data_6_s9_6$interval <- max(data_6_s9_6$terminal_year) - min(data_6_s9_6$terminal_year)
data_6_s9_6$ty <- 2013
data_6_s9_6$ty_minus1 <- data_6_s9_6$ty-1
data_6_s9_6$key_ty <- paste(data_6_s9_6$assessid, data_6_s9_6$ty, sep="_")
data_6_s9_6$key_ty_minus1 <- paste(data_6_s9_6$assessid, data_6_s9_6$ty_minus1, sep="_")
data_6_s9_6[,c(46:47,50:61,64:69)] <- NA

data_6_s9_7 <- data_6_s9[data_6_s9$terminal_year == 2013 | data_6_s9$terminal_year == 2015,]
data_6_s9_7$ty_order <- 7
data_6_s9_7$if_diff_5yrs <- "N"
data_6_s9_7$interval <- max(data_6_s9_7$terminal_year) - min(data_6_s9_7$terminal_year)
data_6_s9_7$ty <- 2013
data_6_s9_7$ty_minus1 <- data_6_s9_7$ty-1
data_6_s9_7$key_ty <- paste(data_6_s9_7$assessid, data_6_s9_7$ty, sep="_")
data_6_s9_7$key_ty_minus1 <- paste(data_6_s9_7$assessid, data_6_s9_7$ty_minus1, sep="_")
data_6_s9_7[,c(46:47,50:61,64:69)] <- NA

data_6_s9_8 <- data_6_s9[data_6_s9$terminal_year == 2013 | data_6_s9$terminal_year == 2016,]
data_6_s9_8$ty_order <- 8
data_6_s9_8$if_diff_5yrs <- "N"
data_6_s9_8$interval <- max(data_6_s9_8$terminal_year) - min(data_6_s9_8$terminal_year)
data_6_s9_8$ty <- 2013
data_6_s9_8$ty_minus1 <- data_6_s9_8$ty-1
data_6_s9_8$key_ty <- paste(data_6_s9_8$assessid, data_6_s9_8$ty, sep="_")
data_6_s9_8$key_ty_minus1 <- paste(data_6_s9_8$assessid, data_6_s9_8$ty_minus1, sep="_")
data_6_s9_8[,c(46:47,50:61,64:69)] <- NA

data_6_s9_9 <- data_6_s9[data_6_s9$terminal_year == 2013 | data_6_s9$terminal_year == 2017,]
data_6_s9_9$ty_order <- 9
data_6_s9_9$if_diff_5yrs <- "N"
data_6_s9_9$interval <- max(data_6_s9_9$terminal_year) - min(data_6_s9_9$terminal_year)
data_6_s9_9$ty <- 2013
data_6_s9_9$ty_minus1 <- data_6_s9_9$ty-1
data_6_s9_9$key_ty <- paste(data_6_s9_9$assessid, data_6_s9_9$ty, sep="_")
data_6_s9_9$key_ty_minus1 <- paste(data_6_s9_9$assessid, data_6_s9_9$ty_minus1, sep="_")
data_6_s9_9[,c(46:47,50:61,64:69)] <- NA

data_6_s9_10 <- data_6_s9[data_6_s9$terminal_year == 2014 | data_6_s9$terminal_year == 2015,]
data_6_s9_10$ty_order <- 10
data_6_s9_10$if_diff_5yrs <- "N"
data_6_s9_10$interval <- max(data_6_s9_10$terminal_year) - min(data_6_s9_10$terminal_year)
data_6_s9_10$ty <- 2014
data_6_s9_10$ty_minus1 <- data_6_s9_10$ty-1
data_6_s9_10$key_ty <- paste(data_6_s9_10$assessid, data_6_s9_10$ty, sep="_")
data_6_s9_10$key_ty_minus1 <- paste(data_6_s9_10$assessid, data_6_s9_10$ty_minus1, sep="_")
data_6_s9_10[,c(46:47,50:61,64:69)] <- NA

data_6_s9_11 <- data_6_s9[data_6_s9$terminal_year == 2014 | data_6_s9$terminal_year == 2016,]
data_6_s9_11$ty_order <- 11
data_6_s9_11$if_diff_5yrs <- "N"
data_6_s9_11$interval <- max(data_6_s9_11$terminal_year) - min(data_6_s9_11$terminal_year)
data_6_s9_11$ty <- 2014
data_6_s9_11$ty_minus1 <- data_6_s9_11$ty-1
data_6_s9_11$key_ty <- paste(data_6_s9_11$assessid, data_6_s9_11$ty, sep="_")
data_6_s9_11$key_ty_minus1 <- paste(data_6_s9_11$assessid, data_6_s9_11$ty_minus1, sep="_")
data_6_s9_11[,c(46:47,50:61,64:69)] <- NA

data_6_s9_12 <- data_6_s9[data_6_s9$terminal_year == 2014 | data_6_s9$terminal_year == 2017,]
data_6_s9_12$ty_order <- 12
data_6_s9_12$if_diff_5yrs <- "N"
data_6_s9_12$interval <- max(data_6_s9_12$terminal_year) - min(data_6_s9_12$terminal_year)
data_6_s9_12$ty <- 2014
data_6_s9_12$ty_minus1 <- data_6_s9_12$ty-1
data_6_s9_12$key_ty <- paste(data_6_s9_12$assessid, data_6_s9_12$ty, sep="_")
data_6_s9_12$key_ty_minus1 <- paste(data_6_s9_12$assessid, data_6_s9_12$ty_minus1, sep="_")
data_6_s9_12[,c(46:47,50:61,64:69)] <- NA

data_6_s9_13 <- data_6_s9[data_6_s9$terminal_year == 2015 | data_6_s9$terminal_year == 2016,]
data_6_s9_13$ty_order <- 13
data_6_s9_13$if_diff_5yrs <- "N"
data_6_s9_13$interval <- max(data_6_s9_13$terminal_year) - min(data_6_s9_13$terminal_year)
data_6_s9_13$ty <- 2015
data_6_s9_13$ty_minus1 <- data_6_s9_13$ty-1
data_6_s9_13$key_ty <- paste(data_6_s9_13$assessid, data_6_s9_13$ty, sep="_")
data_6_s9_13$key_ty_minus1 <- paste(data_6_s9_13$assessid, data_6_s9_13$ty_minus1, sep="_")
data_6_s9_13[,c(46:47,50:61,64:69)] <- NA

data_6_s9_14 <- data_6_s9[data_6_s9$terminal_year == 2015 | data_6_s9$terminal_year == 2017,]
data_6_s9_14$ty_order <- 14
data_6_s9_14$if_diff_5yrs <- "N"
data_6_s9_14$interval <- max(data_6_s9_14$terminal_year) - min(data_6_s9_14$terminal_year)
data_6_s9_14$ty <- 2015
data_6_s9_14$ty_minus1 <- data_6_s9_14$ty-1
data_6_s9_14$key_ty <- paste(data_6_s9_14$assessid, data_6_s9_14$ty, sep="_")
data_6_s9_14$key_ty_minus1 <- paste(data_6_s9_14$assessid, data_6_s9_14$ty_minus1, sep="_")
data_6_s9_14[,c(46:47,50:61,64:69)] <- NA

data_6_s9_15 <- data_6_s9[data_6_s9$terminal_year == 2016 | data_6_s9$terminal_year == 2017,]
data_6_s9_15$ty_order <- 15
data_6_s9_15$if_diff_5yrs <- "N"
data_6_s9_15$interval <- max(data_6_s9_15$terminal_year) - min(data_6_s9_15$terminal_year)
data_6_s9_15$ty <- 2016
data_6_s9_15$ty_minus1 <- data_6_s9_15$ty-1
data_6_s9_15$key_ty <- paste(data_6_s9_15$assessid, data_6_s9_15$ty, sep="_")
data_6_s9_15$key_ty_minus1 <- paste(data_6_s9_15$assessid, data_6_s9_15$ty_minus1, sep="_")
data_6_s9_15[,c(46:47,50:61,64:69)] <- NA

data_6_s9 <- rbind(data_6_s9_1, data_6_s9_2, data_6_s9_3, data_6_s9_4, data_6_s9_5, data_6_s9_6, data_6_s9_7, data_6_s9_8, data_6_s9_9, data_6_s9_10,
                   data_6_s9_11, data_6_s9_12, data_6_s9_13, data_6_s9_14, data_6_s9_15)

# assess 10
data_6_s10 <- data_6_assess[data_6_assess$stockid==ids[10],]

data_6_s10_1 <- data_6_s10[data_6_s10$terminal_year == 2012 | data_6_s10$terminal_year == 2013,]
data_6_s10_1$ty_order <- 1
data_6_s10_1$if_diff_5yrs <- "N"
data_6_s10_1$interval <- max(data_6_s10_1$terminal_year) - min(data_6_s10_1$terminal_year)

data_6_s10_2 <- data_6_s10[data_6_s10$terminal_year == 2012 | data_6_s10$terminal_year == 2014,]
data_6_s10_2$ty_order <- 2
data_6_s10_2$if_diff_5yrs <- "N"
data_6_s10_2$interval <- max(data_6_s10_2$terminal_year) - min(data_6_s10_2$terminal_year)

data_6_s10_3 <- data_6_s10[data_6_s10$terminal_year == 2012 | data_6_s10$terminal_year == 2015,]
data_6_s10_3$ty_order <- 3
data_6_s10_3$if_diff_5yrs <- "N"
data_6_s10_3$interval <- max(data_6_s10_3$terminal_year) - min(data_6_s10_3$terminal_year)

data_6_s10_4 <- data_6_s10[data_6_s10$terminal_year == 2012 | data_6_s10$terminal_year == 2016,]
data_6_s10_4$ty_order <- 4
data_6_s10_4$if_diff_5yrs <- "N"
data_6_s10_4$interval <- max(data_6_s10_4$terminal_year) - min(data_6_s10_4$terminal_year)

data_6_s10_5 <- data_6_s10[data_6_s10$terminal_year == 2012 | data_6_s10$terminal_year == 2017,]
data_6_s10_5$ty_order <- 5
data_6_s10_5$if_diff_5yrs <- "N"
data_6_s10_5$interval <- max(data_6_s10_5$terminal_year) - min(data_6_s10_5$terminal_year)

data_6_s10_6 <- data_6_s10[data_6_s10$terminal_year == 2013 | data_6_s10$terminal_year == 2014,]
data_6_s10_6$ty_order <- 6
data_6_s10_6$if_diff_5yrs <- "N"
data_6_s10_6$interval <- max(data_6_s10_6$terminal_year) - min(data_6_s10_6$terminal_year)
data_6_s10_6$ty <- 2013
data_6_s10_6$ty_minus1 <- data_6_s10_6$ty-1
data_6_s10_6$key_ty <- paste(data_6_s10_6$assessid, data_6_s10_6$ty, sep="_")
data_6_s10_6$key_ty_minus1 <- paste(data_6_s10_6$assessid, data_6_s10_6$ty_minus1, sep="_")
data_6_s10_6[,c(46:47,50:61,64:69)] <- NA

data_6_s10_7 <- data_6_s10[data_6_s10$terminal_year == 2013 | data_6_s10$terminal_year == 2015,]
data_6_s10_7$ty_order <- 7
data_6_s10_7$if_diff_5yrs <- "N"
data_6_s10_7$interval <- max(data_6_s10_7$terminal_year) - min(data_6_s10_7$terminal_year)
data_6_s10_7$ty <- 2013
data_6_s10_7$ty_minus1 <- data_6_s10_7$ty-1
data_6_s10_7$key_ty <- paste(data_6_s10_7$assessid, data_6_s10_7$ty, sep="_")
data_6_s10_7$key_ty_minus1 <- paste(data_6_s10_7$assessid, data_6_s10_7$ty_minus1, sep="_")
data_6_s10_7[,c(46:47,50:61,64:69)] <- NA

data_6_s10_8 <- data_6_s10[data_6_s10$terminal_year == 2013 | data_6_s10$terminal_year == 2016,]
data_6_s10_8$ty_order <- 8
data_6_s10_8$if_diff_5yrs <- "N"
data_6_s10_8$interval <- max(data_6_s10_8$terminal_year) - min(data_6_s10_8$terminal_year)
data_6_s10_8$ty <- 2013
data_6_s10_8$ty_minus1 <- data_6_s10_8$ty-1
data_6_s10_8$key_ty <- paste(data_6_s10_8$assessid, data_6_s10_8$ty, sep="_")
data_6_s10_8$key_ty_minus1 <- paste(data_6_s10_8$assessid, data_6_s10_8$ty_minus1, sep="_")
data_6_s10_8[,c(46:47,50:61,64:69)] <- NA

data_6_s10_9 <- data_6_s10[data_6_s10$terminal_year == 2013 | data_6_s10$terminal_year == 2017,]
data_6_s10_9$ty_order <- 9
data_6_s10_9$if_diff_5yrs <- "N"
data_6_s10_9$interval <- max(data_6_s10_9$terminal_year) - min(data_6_s10_9$terminal_year)
data_6_s10_9$ty <- 2013
data_6_s10_9$ty_minus1 <- data_6_s10_9$ty-1
data_6_s10_9$key_ty <- paste(data_6_s10_9$assessid, data_6_s10_9$ty, sep="_")
data_6_s10_9$key_ty_minus1 <- paste(data_6_s10_9$assessid, data_6_s10_9$ty_minus1, sep="_")
data_6_s10_9[,c(46:47,50:61,64:69)] <- NA

data_6_s10_10 <- data_6_s10[data_6_s10$terminal_year == 2014 | data_6_s10$terminal_year == 2015,]
data_6_s10_10$ty_order <- 10
data_6_s10_10$if_diff_5yrs <- "N"
data_6_s10_10$interval <- max(data_6_s10_10$terminal_year) - min(data_6_s10_10$terminal_year)
data_6_s10_10$ty <- 2014
data_6_s10_10$ty_minus1 <- data_6_s10_10$ty-1
data_6_s10_10$key_ty <- paste(data_6_s10_10$assessid, data_6_s10_10$ty, sep="_")
data_6_s10_10$key_ty_minus1 <- paste(data_6_s10_10$assessid, data_6_s10_10$ty_minus1, sep="_")
data_6_s10_10[,c(46:47,50:61,64:69)] <- NA

data_6_s10_11 <- data_6_s10[data_6_s10$terminal_year == 2014 | data_6_s10$terminal_year == 2016,]
data_6_s10_11$ty_order <- 11
data_6_s10_11$if_diff_5yrs <- "N"
data_6_s10_11$interval <- max(data_6_s10_11$terminal_year) - min(data_6_s10_11$terminal_year)
data_6_s10_11$ty <- 2014
data_6_s10_11$ty_minus1 <- data_6_s10_11$ty-1
data_6_s10_11$key_ty <- paste(data_6_s10_11$assessid, data_6_s10_11$ty, sep="_")
data_6_s10_11$key_ty_minus1 <- paste(data_6_s10_11$assessid, data_6_s10_11$ty_minus1, sep="_")
data_6_s10_11[,c(46:47,50:61,64:69)] <- NA

data_6_s10_12 <- data_6_s10[data_6_s10$terminal_year == 2014 | data_6_s10$terminal_year == 2017,]
data_6_s10_12$ty_order <- 12
data_6_s10_12$if_diff_5yrs <- "N"
data_6_s10_12$interval <- max(data_6_s10_12$terminal_year) - min(data_6_s10_12$terminal_year)
data_6_s10_12$ty <- 2014
data_6_s10_12$ty_minus1 <- data_6_s10_12$ty-1
data_6_s10_12$key_ty <- paste(data_6_s10_12$assessid, data_6_s10_12$ty, sep="_")
data_6_s10_12$key_ty_minus1 <- paste(data_6_s10_12$assessid, data_6_s10_12$ty_minus1, sep="_")
data_6_s10_12[,c(46:47,50:61,64:69)] <- NA

data_6_s10_13 <- data_6_s10[data_6_s10$terminal_year == 2015 | data_6_s10$terminal_year == 2016,]
data_6_s10_13$ty_order <- 13
data_6_s10_13$if_diff_5yrs <- "N"
data_6_s10_13$interval <- max(data_6_s10_13$terminal_year) - min(data_6_s10_13$terminal_year)
data_6_s10_13$ty <- 2015
data_6_s10_13$ty_minus1 <- data_6_s10_13$ty-1
data_6_s10_13$key_ty <- paste(data_6_s10_13$assessid, data_6_s10_13$ty, sep="_")
data_6_s10_13$key_ty_minus1 <- paste(data_6_s10_13$assessid, data_6_s10_13$ty_minus1, sep="_")
data_6_s10_13[,c(46:47,50:61,64:69)] <- NA

data_6_s10_14 <- data_6_s10[data_6_s10$terminal_year == 2015 | data_6_s10$terminal_year == 2017,]
data_6_s10_14$ty_order <- 14
data_6_s10_14$if_diff_5yrs <- "N"
data_6_s10_14$interval <- max(data_6_s10_14$terminal_year) - min(data_6_s10_14$terminal_year)
data_6_s10_14$ty <- 2015
data_6_s10_14$ty_minus1 <- data_6_s10_14$ty-1
data_6_s10_14$key_ty <- paste(data_6_s10_14$assessid, data_6_s10_14$ty, sep="_")
data_6_s10_14$key_ty_minus1 <- paste(data_6_s10_14$assessid, data_6_s10_14$ty_minus1, sep="_")
data_6_s10_14[,c(46:47,50:61,64:69)] <- NA

data_6_s10_15 <- data_6_s10[data_6_s10$terminal_year == 2016 | data_6_s10$terminal_year == 2017,]
data_6_s10_15$ty_order <- 15
data_6_s10_15$if_diff_5yrs <- "N"
data_6_s10_15$interval <- max(data_6_s10_15$terminal_year) - min(data_6_s10_15$terminal_year)
data_6_s10_15$ty <- 2016
data_6_s10_15$ty_minus1 <- data_6_s10_15$ty-1
data_6_s10_15$key_ty <- paste(data_6_s10_15$assessid, data_6_s10_15$ty, sep="_")
data_6_s10_15$key_ty_minus1 <- paste(data_6_s10_15$assessid, data_6_s10_15$ty_minus1, sep="_")
data_6_s10_15[,c(46:47,50:61,64:69)] <- NA

data_6_s10 <- rbind(data_6_s10_1, data_6_s10_2, data_6_s10_3, data_6_s10_4, data_6_s10_5, data_6_s10_6, data_6_s10_7, data_6_s10_8, data_6_s10_9, data_6_s10_10,
                    data_6_s10_11, data_6_s10_12, data_6_s10_13, data_6_s10_14, data_6_s10_15)

# assess 11
data_6_s11 <- data_6_assess[data_6_assess$stockid==ids[11],]

data_6_s11_1 <- data_6_s11[data_6_s11$terminal_year == 2011 | data_6_s11$terminal_year == 2012,]
data_6_s11_1$ty_order <- 1
data_6_s11_1$if_diff_5yrs <- "N"
data_6_s11_1$interval <- max(data_6_s11_1$terminal_year) - min(data_6_s11_1$terminal_year)

data_6_s11_2 <- data_6_s11[data_6_s11$terminal_year == 2011 | data_6_s11$terminal_year == 2013,]
data_6_s11_2$ty_order <- 2
data_6_s11_2$if_diff_5yrs <- "N"
data_6_s11_2$interval <- max(data_6_s11_2$terminal_year) - min(data_6_s11_2$terminal_year)

data_6_s11_3 <- data_6_s11[data_6_s11$terminal_year == 2011 | data_6_s11$terminal_year == 2014,]
data_6_s11_3$ty_order <- 3
data_6_s11_3$if_diff_5yrs <- "N"
data_6_s11_3$interval <- max(data_6_s11_3$terminal_year) - min(data_6_s11_3$terminal_year)

data_6_s11_4 <- data_6_s11[data_6_s11$terminal_year == 2011 | data_6_s11$terminal_year == 2015,]
data_6_s11_4$ty_order <- 4
data_6_s11_4$if_diff_5yrs <- "N"
data_6_s11_4$interval <- max(data_6_s11_4$terminal_year) - min(data_6_s11_4$terminal_year)

data_6_s11_5 <- data_6_s11[data_6_s11$terminal_year == 2011 | data_6_s11$terminal_year == 2016,]
data_6_s11_5$ty_order <- 5
data_6_s11_5$if_diff_5yrs <- "N"
data_6_s11_5$interval <- max(data_6_s11_5$terminal_year) - min(data_6_s11_5$terminal_year)

data_6_s11_6 <- data_6_s11[data_6_s11$terminal_year == 2012 | data_6_s11$terminal_year == 2013,]
data_6_s11_6$ty_order <- 6
data_6_s11_6$if_diff_5yrs <- "N"
data_6_s11_6$interval <- max(data_6_s11_6$terminal_year) - min(data_6_s11_6$terminal_year)
data_6_s11_6$ty <- 2012
data_6_s11_6$ty_minus1 <- data_6_s11_6$ty-1
data_6_s11_6$key_ty <- paste(data_6_s11_6$assessid, data_6_s11_6$ty, sep="_")
data_6_s11_6$key_ty_minus1 <- paste(data_6_s11_6$assessid, data_6_s11_6$ty_minus1, sep="_")
data_6_s11_6[,c(46:47,50:61,64:69)] <- NA

data_6_s11_7 <- data_6_s11[data_6_s11$terminal_year == 2012 | data_6_s11$terminal_year == 2014,]
data_6_s11_7$ty_order <- 7
data_6_s11_7$if_diff_5yrs <- "N"
data_6_s11_7$interval <- max(data_6_s11_7$terminal_year) - min(data_6_s11_7$terminal_year)
data_6_s11_7$ty <- 2012
data_6_s11_7$ty_minus1 <- data_6_s11_7$ty-1
data_6_s11_7$key_ty <- paste(data_6_s11_7$assessid, data_6_s11_7$ty, sep="_")
data_6_s11_7$key_ty_minus1 <- paste(data_6_s11_7$assessid, data_6_s11_7$ty_minus1, sep="_")
data_6_s11_7[,c(46:47,50:61,64:69)] <- NA

data_6_s11_8 <- data_6_s11[data_6_s11$terminal_year == 2012 | data_6_s11$terminal_year == 2015,]
data_6_s11_8$ty_order <- 8
data_6_s11_8$if_diff_5yrs <- "N"
data_6_s11_8$interval <- max(data_6_s11_8$terminal_year) - min(data_6_s11_8$terminal_year)
data_6_s11_8$ty <- 2012
data_6_s11_8$ty_minus1 <- data_6_s11_8$ty-1
data_6_s11_8$key_ty <- paste(data_6_s11_8$assessid, data_6_s11_8$ty, sep="_")
data_6_s11_8$key_ty_minus1 <- paste(data_6_s11_8$assessid, data_6_s11_8$ty_minus1, sep="_")
data_6_s11_8[,c(46:47,50:61,64:69)] <- NA

data_6_s11_9 <- data_6_s11[data_6_s11$terminal_year == 2012 | data_6_s11$terminal_year == 2016,]
data_6_s11_9$ty_order <- 9
data_6_s11_9$if_diff_5yrs <- "N"
data_6_s11_9$interval <- max(data_6_s11_9$terminal_year) - min(data_6_s11_9$terminal_year)
data_6_s11_9$ty <- 2012
data_6_s11_9$ty_minus1 <- data_6_s11_9$ty-1
data_6_s11_9$key_ty <- paste(data_6_s11_9$assessid, data_6_s11_9$ty, sep="_")
data_6_s11_9$key_ty_minus1 <- paste(data_6_s11_9$assessid, data_6_s11_9$ty_minus1, sep="_")
data_6_s11_9[,c(46:47,50:61,64:69)] <- NA

data_6_s11_10 <- data_6_s11[data_6_s11$terminal_year == 2013 | data_6_s11$terminal_year == 2014,]
data_6_s11_10$ty_order <- 10
data_6_s11_10$if_diff_5yrs <- "N"
data_6_s11_10$interval <- max(data_6_s11_10$terminal_year) - min(data_6_s11_10$terminal_year)
data_6_s11_10$ty <- 2013
data_6_s11_10$ty_minus1 <- data_6_s11_10$ty-1
data_6_s11_10$key_ty <- paste(data_6_s11_10$assessid, data_6_s11_10$ty, sep="_")
data_6_s11_10$key_ty_minus1 <- paste(data_6_s11_10$assessid, data_6_s11_10$ty_minus1, sep="_")
data_6_s11_10[,c(46:47,50:61,64:69)] <- NA

data_6_s11_11 <- data_6_s11[data_6_s11$terminal_year == 2013 | data_6_s11$terminal_year == 2015,]
data_6_s11_11$ty_order <- 11
data_6_s11_11$if_diff_5yrs <- "N"
data_6_s11_11$interval <- max(data_6_s11_11$terminal_year) - min(data_6_s11_11$terminal_year)
data_6_s11_11$ty <- 2013
data_6_s11_11$ty_minus1 <- data_6_s11_11$ty-1
data_6_s11_11$key_ty <- paste(data_6_s11_11$assessid, data_6_s11_11$ty, sep="_")
data_6_s11_11$key_ty_minus1 <- paste(data_6_s11_11$assessid, data_6_s11_11$ty_minus1, sep="_")
data_6_s11_11[,c(46:47,50:61,64:69)] <- NA

data_6_s11_12 <- data_6_s11[data_6_s11$terminal_year == 2013 | data_6_s11$terminal_year == 2016,]
data_6_s11_12$ty_order <- 12
data_6_s11_12$if_diff_5yrs <- "N"
data_6_s11_12$interval <- max(data_6_s11_12$terminal_year) - min(data_6_s11_12$terminal_year)
data_6_s11_12$ty <- 2013
data_6_s11_12$ty_minus1 <- data_6_s11_12$ty-1
data_6_s11_12$key_ty <- paste(data_6_s11_12$assessid, data_6_s11_12$ty, sep="_")
data_6_s11_12$key_ty_minus1 <- paste(data_6_s11_12$assessid, data_6_s11_12$ty_minus1, sep="_")
data_6_s11_12[,c(46:47,50:61,64:69)] <- NA

data_6_s11_13 <- data_6_s11[data_6_s11$terminal_year == 2014 | data_6_s11$terminal_year == 2015,]
data_6_s11_13$ty_order <- 13
data_6_s11_13$if_diff_5yrs <- "N"
data_6_s11_13$interval <- max(data_6_s11_13$terminal_year) - min(data_6_s11_13$terminal_year)
data_6_s11_13$ty <- 2014
data_6_s11_13$ty_minus1 <- data_6_s11_13$ty-1
data_6_s11_13$key_ty <- paste(data_6_s11_13$assessid, data_6_s11_13$ty, sep="_")
data_6_s11_13$key_ty_minus1 <- paste(data_6_s11_13$assessid, data_6_s11_13$ty_minus1, sep="_")
data_6_s11_13[,c(46:47,50:61,64:69)] <- NA

data_6_s11_14 <- data_6_s11[data_6_s11$terminal_year == 2014 | data_6_s11$terminal_year == 2016,]
data_6_s11_14$ty_order <- 14
data_6_s11_14$if_diff_5yrs <- "N"
data_6_s11_14$interval <- max(data_6_s11_14$terminal_year) - min(data_6_s11_14$terminal_year)
data_6_s11_14$ty <- 2014
data_6_s11_14$ty_minus1 <- data_6_s11_14$ty-1
data_6_s11_14$key_ty <- paste(data_6_s11_14$assessid, data_6_s11_14$ty, sep="_")
data_6_s11_14$key_ty_minus1 <- paste(data_6_s11_14$assessid, data_6_s11_14$ty_minus1, sep="_")
data_6_s11_14[,c(46:47,50:61,64:69)] <- NA

data_6_s11_15 <- data_6_s11[data_6_s11$terminal_year == 2015 | data_6_s11$terminal_year == 2016,]
data_6_s11_15$ty_order <- 15
data_6_s11_15$if_diff_5yrs <- "N"
data_6_s11_15$interval <- max(data_6_s11_15$terminal_year) - min(data_6_s11_15$terminal_year)
data_6_s11_15$ty <- 2015
data_6_s11_15$ty_minus1 <- data_6_s11_15$ty-1
data_6_s11_15$key_ty <- paste(data_6_s11_15$assessid, data_6_s11_15$ty, sep="_")
data_6_s11_15$key_ty_minus1 <- paste(data_6_s11_15$assessid, data_6_s11_15$ty_minus1, sep="_")
data_6_s11_15[,c(46:47,50:61,64:69)] <- NA

data_6_s11 <- rbind(data_6_s11_1, data_6_s11_2, data_6_s11_3, data_6_s11_4, data_6_s11_5, data_6_s11_6, data_6_s11_7, data_6_s11_8, data_6_s11_9, data_6_s11_10,
                    data_6_s11_11, data_6_s11_12, data_6_s11_13, data_6_s11_14, data_6_s11_15)

# assess 12
data_6_s12 <- data_6_assess[data_6_assess$stockid==ids[12],]

data_6_s12_1 <- data_6_s12[data_6_s12$terminal_year == 2011 | data_6_s12$terminal_year == 2012,]
data_6_s12_1$ty_order <- 1
data_6_s12_1$if_diff_5yrs <- "N"
data_6_s12_1$interval <- max(data_6_s12_1$terminal_year) - min(data_6_s12_1$terminal_year)

data_6_s12_2 <- data_6_s12[data_6_s12$terminal_year == 2011 | data_6_s12$terminal_year == 2013,]
data_6_s12_2$ty_order <- 2
data_6_s12_2$if_diff_5yrs <- "N"
data_6_s12_2$interval <- max(data_6_s12_2$terminal_year) - min(data_6_s12_2$terminal_year)

data_6_s12_3 <- data_6_s12[data_6_s12$terminal_year == 2011 | data_6_s12$terminal_year == 2014,]
data_6_s12_3$ty_order <- 3
data_6_s12_3$if_diff_5yrs <- "N"
data_6_s12_3$interval <- max(data_6_s12_3$terminal_year) - min(data_6_s12_3$terminal_year)

data_6_s12_4 <- data_6_s12[data_6_s12$terminal_year == 2011 | data_6_s12$terminal_year == 2015,]
data_6_s12_4$ty_order <- 4
data_6_s12_4$if_diff_5yrs <- "N"
data_6_s12_4$interval <- max(data_6_s12_4$terminal_year) - min(data_6_s12_4$terminal_year)

data_6_s12_5 <- data_6_s12[data_6_s12$terminal_year == 2011 | data_6_s12$terminal_year == 2016,]
data_6_s12_5$ty_order <- 5
data_6_s12_5$if_diff_5yrs <- "N"
data_6_s12_5$interval <- max(data_6_s12_5$terminal_year) - min(data_6_s12_5$terminal_year)

data_6_s12_6 <- data_6_s12[data_6_s12$terminal_year == 2012 | data_6_s12$terminal_year == 2013,]
data_6_s12_6$ty_order <- 6
data_6_s12_6$if_diff_5yrs <- "N"
data_6_s12_6$interval <- max(data_6_s12_6$terminal_year) - min(data_6_s12_6$terminal_year)
data_6_s12_6$ty <- 2012
data_6_s12_6$ty_minus1 <- data_6_s12_6$ty-1
data_6_s12_6$key_ty <- paste(data_6_s12_6$assessid, data_6_s12_6$ty, sep="_")
data_6_s12_6$key_ty_minus1 <- paste(data_6_s12_6$assessid, data_6_s12_6$ty_minus1, sep="_")
data_6_s12_6[,c(46:47,50:61,64:69)] <- NA

data_6_s12_7 <- data_6_s12[data_6_s12$terminal_year == 2012 | data_6_s12$terminal_year == 2014,]
data_6_s12_7$ty_order <- 7
data_6_s12_7$if_diff_5yrs <- "N"
data_6_s12_7$interval <- max(data_6_s12_7$terminal_year) - min(data_6_s12_7$terminal_year)
data_6_s12_7$ty <- 2012
data_6_s12_7$ty_minus1 <- data_6_s12_7$ty-1
data_6_s12_7$key_ty <- paste(data_6_s12_7$assessid, data_6_s12_7$ty, sep="_")
data_6_s12_7$key_ty_minus1 <- paste(data_6_s12_7$assessid, data_6_s12_7$ty_minus1, sep="_")
data_6_s12_7[,c(46:47,50:61,64:69)] <- NA

data_6_s12_8 <- data_6_s12[data_6_s12$terminal_year == 2012 | data_6_s12$terminal_year == 2015,]
data_6_s12_8$ty_order <- 8
data_6_s12_8$if_diff_5yrs <- "N"
data_6_s12_8$interval <- max(data_6_s12_8$terminal_year) - min(data_6_s12_8$terminal_year)
data_6_s12_8$ty <- 2012
data_6_s12_8$ty_minus1 <- data_6_s12_8$ty-1
data_6_s12_8$key_ty <- paste(data_6_s12_8$assessid, data_6_s12_8$ty, sep="_")
data_6_s12_8$key_ty_minus1 <- paste(data_6_s12_8$assessid, data_6_s12_8$ty_minus1, sep="_")
data_6_s12_8[,c(46:47,50:61,64:69)] <- NA

data_6_s12_9 <- data_6_s12[data_6_s12$terminal_year == 2012 | data_6_s12$terminal_year == 2016,]
data_6_s12_9$ty_order <- 9
data_6_s12_9$if_diff_5yrs <- "N"
data_6_s12_9$interval <- max(data_6_s12_9$terminal_year) - min(data_6_s12_9$terminal_year)
data_6_s12_9$ty <- 2012
data_6_s12_9$ty_minus1 <- data_6_s12_9$ty-1
data_6_s12_9$key_ty <- paste(data_6_s12_9$assessid, data_6_s12_9$ty, sep="_")
data_6_s12_9$key_ty_minus1 <- paste(data_6_s12_9$assessid, data_6_s12_9$ty_minus1, sep="_")
data_6_s12_9[,c(46:47,50:61,64:69)] <- NA

data_6_s12_10 <- data_6_s12[data_6_s12$terminal_year == 2013 | data_6_s12$terminal_year == 2014,]
data_6_s12_10$ty_order <- 10
data_6_s12_10$if_diff_5yrs <- "N"
data_6_s12_10$interval <- max(data_6_s12_10$terminal_year) - min(data_6_s12_10$terminal_year)
data_6_s12_10$ty <- 2013
data_6_s12_10$ty_minus1 <- data_6_s12_10$ty-1
data_6_s12_10$key_ty <- paste(data_6_s12_10$assessid, data_6_s12_10$ty, sep="_")
data_6_s12_10$key_ty_minus1 <- paste(data_6_s12_10$assessid, data_6_s12_10$ty_minus1, sep="_")
data_6_s12_10[,c(46:47,50:61,64:69)] <- NA

data_6_s12_11 <- data_6_s12[data_6_s12$terminal_year == 2013 | data_6_s12$terminal_year == 2015,]
data_6_s12_11$ty_order <- 11
data_6_s12_11$if_diff_5yrs <- "N"
data_6_s12_11$interval <- max(data_6_s12_11$terminal_year) - min(data_6_s12_11$terminal_year)
data_6_s12_11$ty <- 2013
data_6_s12_11$ty_minus1 <- data_6_s12_11$ty-1
data_6_s12_11$key_ty <- paste(data_6_s12_11$assessid, data_6_s12_11$ty, sep="_")
data_6_s12_11$key_ty_minus1 <- paste(data_6_s12_11$assessid, data_6_s12_11$ty_minus1, sep="_")
data_6_s12_11[,c(46:47,50:61,64:69)] <- NA

data_6_s12_12 <- data_6_s12[data_6_s12$terminal_year == 2013 | data_6_s12$terminal_year == 2016,]
data_6_s12_12$ty_order <- 12
data_6_s12_12$if_diff_5yrs <- "N"
data_6_s12_12$interval <- max(data_6_s12_12$terminal_year) - min(data_6_s12_12$terminal_year)
data_6_s12_12$ty <- 2013
data_6_s12_12$ty_minus1 <- data_6_s12_12$ty-1
data_6_s12_12$key_ty <- paste(data_6_s12_12$assessid, data_6_s12_12$ty, sep="_")
data_6_s12_12$key_ty_minus1 <- paste(data_6_s12_12$assessid, data_6_s12_12$ty_minus1, sep="_")
data_6_s12_12[,c(46:47,50:61,64:69)] <- NA

data_6_s12_13 <- data_6_s12[data_6_s12$terminal_year == 2014 | data_6_s12$terminal_year == 2015,]
data_6_s12_13$ty_order <- 13
data_6_s12_13$if_diff_5yrs <- "N"
data_6_s12_13$interval <- max(data_6_s12_13$terminal_year) - min(data_6_s12_13$terminal_year)
data_6_s12_13$ty <- 2014
data_6_s12_13$ty_minus1 <- data_6_s12_13$ty-1
data_6_s12_13$key_ty <- paste(data_6_s12_13$assessid, data_6_s12_13$ty, sep="_")
data_6_s12_13$key_ty_minus1 <- paste(data_6_s12_13$assessid, data_6_s12_13$ty_minus1, sep="_")
data_6_s12_13[,c(46:47,50:61,64:69)] <- NA

data_6_s12_14 <- data_6_s12[data_6_s12$terminal_year == 2014 | data_6_s12$terminal_year == 2016,]
data_6_s12_14$ty_order <- 14
data_6_s12_14$if_diff_5yrs <- "N"
data_6_s12_14$interval <- max(data_6_s12_14$terminal_year) - min(data_6_s12_14$terminal_year)
data_6_s12_14$ty <- 2014
data_6_s12_14$ty_minus1 <- data_6_s12_14$ty-1
data_6_s12_14$key_ty <- paste(data_6_s12_14$assessid, data_6_s12_14$ty, sep="_")
data_6_s12_14$key_ty_minus1 <- paste(data_6_s12_14$assessid, data_6_s12_14$ty_minus1, sep="_")
data_6_s12_14[,c(46:47,50:61,64:69)] <- NA

data_6_s12_15 <- data_6_s12[data_6_s12$terminal_year == 2015 | data_6_s12$terminal_year == 2016,]
data_6_s12_15$ty_order <- 15
data_6_s12_15$if_diff_5yrs <- "N"
data_6_s12_15$interval <- max(data_6_s12_15$terminal_year) - min(data_6_s12_15$terminal_year)
data_6_s12_15$ty <- 2015
data_6_s12_15$ty_minus1 <- data_6_s12_15$ty-1
data_6_s12_15$key_ty <- paste(data_6_s12_15$assessid, data_6_s12_15$ty, sep="_")
data_6_s12_15$key_ty_minus1 <- paste(data_6_s12_15$assessid, data_6_s12_15$ty_minus1, sep="_")
data_6_s12_15[,c(46:47,50:61,64:69)] <- NA

data_6_s12 <- rbind(data_6_s12_1, data_6_s12_2, data_6_s12_3, data_6_s12_4, data_6_s12_5, data_6_s12_6, data_6_s12_7, data_6_s12_8, data_6_s12_9, data_6_s12_10,
                    data_6_s12_11, data_6_s12_12, data_6_s12_13, data_6_s12_14, data_6_s12_15)

# assess 13
data_6_s13 <- data_6_assess[data_6_assess$stockid==ids[13],]

data_6_s13_1 <- data_6_s13[data_6_s13$terminal_year == 2011 | data_6_s13$terminal_year == 2012,]
data_6_s13_1$ty_order <- 1
data_6_s13_1$if_diff_5yrs <- "N"
data_6_s13_1$interval <- max(data_6_s13_1$terminal_year) - min(data_6_s13_1$terminal_year)

data_6_s13_2 <- data_6_s13[data_6_s13$terminal_year == 2011 | data_6_s13$terminal_year == 2013,]
data_6_s13_2$ty_order <- 2
data_6_s13_2$if_diff_5yrs <- "N"
data_6_s13_2$interval <- max(data_6_s13_2$terminal_year) - min(data_6_s13_2$terminal_year)

data_6_s13_3 <- data_6_s13[data_6_s13$terminal_year == 2011 | data_6_s13$terminal_year == 2014,]
data_6_s13_3$ty_order <- 3
data_6_s13_3$if_diff_5yrs <- "N"
data_6_s13_3$interval <- max(data_6_s13_3$terminal_year) - min(data_6_s13_3$terminal_year)

data_6_s13_4 <- data_6_s13[data_6_s13$terminal_year == 2011 | data_6_s13$terminal_year == 2015,]
data_6_s13_4$ty_order <- 4
data_6_s13_4$if_diff_5yrs <- "N"
data_6_s13_4$interval <- max(data_6_s13_4$terminal_year) - min(data_6_s13_4$terminal_year)

data_6_s13_5 <- data_6_s13[data_6_s13$terminal_year == 2011 | data_6_s13$terminal_year == 2016,]
data_6_s13_5$ty_order <- 5
data_6_s13_5$if_diff_5yrs <- "N"
data_6_s13_5$interval <- max(data_6_s13_5$terminal_year) - min(data_6_s13_5$terminal_year)

data_6_s13_6 <- data_6_s13[data_6_s13$terminal_year == 2012 | data_6_s13$terminal_year == 2013,]
data_6_s13_6$ty_order <- 6
data_6_s13_6$if_diff_5yrs <- "N"
data_6_s13_6$interval <- max(data_6_s13_6$terminal_year) - min(data_6_s13_6$terminal_year)
data_6_s13_6$ty <- 2012
data_6_s13_6$ty_minus1 <- data_6_s13_6$ty-1
data_6_s13_6$key_ty <- paste(data_6_s13_6$assessid, data_6_s13_6$ty, sep="_")
data_6_s13_6$key_ty_minus1 <- paste(data_6_s13_6$assessid, data_6_s13_6$ty_minus1, sep="_")
data_6_s13_6[,c(46:47,50:61,64:69)] <- NA

data_6_s13_7 <- data_6_s13[data_6_s13$terminal_year == 2012 | data_6_s13$terminal_year == 2014,]
data_6_s13_7$ty_order <- 7
data_6_s13_7$if_diff_5yrs <- "N"
data_6_s13_7$interval <- max(data_6_s13_7$terminal_year) - min(data_6_s13_7$terminal_year)
data_6_s13_7$ty <- 2012
data_6_s13_7$ty_minus1 <- data_6_s13_7$ty-1
data_6_s13_7$key_ty <- paste(data_6_s13_7$assessid, data_6_s13_7$ty, sep="_")
data_6_s13_7$key_ty_minus1 <- paste(data_6_s13_7$assessid, data_6_s13_7$ty_minus1, sep="_")
data_6_s13_7[,c(46:47,50:61,64:69)] <- NA

data_6_s13_8 <- data_6_s13[data_6_s13$terminal_year == 2012 | data_6_s13$terminal_year == 2015,]
data_6_s13_8$ty_order <- 8
data_6_s13_8$if_diff_5yrs <- "N"
data_6_s13_8$interval <- max(data_6_s13_8$terminal_year) - min(data_6_s13_8$terminal_year)
data_6_s13_8$ty <- 2012
data_6_s13_8$ty_minus1 <- data_6_s13_8$ty-1
data_6_s13_8$key_ty <- paste(data_6_s13_8$assessid, data_6_s13_8$ty, sep="_")
data_6_s13_8$key_ty_minus1 <- paste(data_6_s13_8$assessid, data_6_s13_8$ty_minus1, sep="_")
data_6_s13_8[,c(46:47,50:61,64:69)] <- NA

data_6_s13_9 <- data_6_s13[data_6_s13$terminal_year == 2012 | data_6_s13$terminal_year == 2016,]
data_6_s13_9$ty_order <- 9
data_6_s13_9$if_diff_5yrs <- "N"
data_6_s13_9$interval <- max(data_6_s13_9$terminal_year) - min(data_6_s13_9$terminal_year)
data_6_s13_9$ty <- 2012
data_6_s13_9$ty_minus1 <- data_6_s13_9$ty-1
data_6_s13_9$key_ty <- paste(data_6_s13_9$assessid, data_6_s13_9$ty, sep="_")
data_6_s13_9$key_ty_minus1 <- paste(data_6_s13_9$assessid, data_6_s13_9$ty_minus1, sep="_")
data_6_s13_9[,c(46:47,50:61,64:69)] <- NA

data_6_s13_10 <- data_6_s13[data_6_s13$terminal_year == 2013 | data_6_s13$terminal_year == 2014,]
data_6_s13_10$ty_order <- 10
data_6_s13_10$if_diff_5yrs <- "N"
data_6_s13_10$interval <- max(data_6_s13_10$terminal_year) - min(data_6_s13_10$terminal_year)
data_6_s13_10$ty <- 2013
data_6_s13_10$ty_minus1 <- data_6_s13_10$ty-1
data_6_s13_10$key_ty <- paste(data_6_s13_10$assessid, data_6_s13_10$ty, sep="_")
data_6_s13_10$key_ty_minus1 <- paste(data_6_s13_10$assessid, data_6_s13_10$ty_minus1, sep="_")
data_6_s13_10[,c(46:47,50:61,64:69)] <- NA

data_6_s13_11 <- data_6_s13[data_6_s13$terminal_year == 2013 | data_6_s13$terminal_year == 2015,]
data_6_s13_11$ty_order <- 11
data_6_s13_11$if_diff_5yrs <- "N"
data_6_s13_11$interval <- max(data_6_s13_11$terminal_year) - min(data_6_s13_11$terminal_year)
data_6_s13_11$ty <- 2013
data_6_s13_11$ty_minus1 <- data_6_s13_11$ty-1
data_6_s13_11$key_ty <- paste(data_6_s13_11$assessid, data_6_s13_11$ty, sep="_")
data_6_s13_11$key_ty_minus1 <- paste(data_6_s13_11$assessid, data_6_s13_11$ty_minus1, sep="_")
data_6_s13_11[,c(46:47,50:61,64:69)] <- NA

data_6_s13_12 <- data_6_s13[data_6_s13$terminal_year == 2013 | data_6_s13$terminal_year == 2016,]
data_6_s13_12$ty_order <- 12
data_6_s13_12$if_diff_5yrs <- "N"
data_6_s13_12$interval <- max(data_6_s13_12$terminal_year) - min(data_6_s13_12$terminal_year)
data_6_s13_12$ty <- 2013
data_6_s13_12$ty_minus1 <- data_6_s13_12$ty-1
data_6_s13_12$key_ty <- paste(data_6_s13_12$assessid, data_6_s13_12$ty, sep="_")
data_6_s13_12$key_ty_minus1 <- paste(data_6_s13_12$assessid, data_6_s13_12$ty_minus1, sep="_")
data_6_s13_12[,c(46:47,50:61,64:69)] <- NA

data_6_s13_13 <- data_6_s13[data_6_s13$terminal_year == 2014 | data_6_s13$terminal_year == 2015,]
data_6_s13_13$ty_order <- 13
data_6_s13_13$if_diff_5yrs <- "N"
data_6_s13_13$interval <- max(data_6_s13_13$terminal_year) - min(data_6_s13_13$terminal_year)
data_6_s13_13$ty <- 2014
data_6_s13_13$ty_minus1 <- data_6_s13_13$ty-1
data_6_s13_13$key_ty <- paste(data_6_s13_13$assessid, data_6_s13_13$ty, sep="_")
data_6_s13_13$key_ty_minus1 <- paste(data_6_s13_13$assessid, data_6_s13_13$ty_minus1, sep="_")
data_6_s13_13[,c(46:47,50:61,64:69)] <- NA

data_6_s13_14 <- data_6_s13[data_6_s13$terminal_year == 2014 | data_6_s13$terminal_year == 2016,]
data_6_s13_14$ty_order <- 14
data_6_s13_14$if_diff_5yrs <- "N"
data_6_s13_14$interval <- max(data_6_s13_14$terminal_year) - min(data_6_s13_14$terminal_year)
data_6_s13_14$ty <- 2014
data_6_s13_14$ty_minus1 <- data_6_s13_14$ty-1
data_6_s13_14$key_ty <- paste(data_6_s13_14$assessid, data_6_s13_14$ty, sep="_")
data_6_s13_14$key_ty_minus1 <- paste(data_6_s13_14$assessid, data_6_s13_14$ty_minus1, sep="_")
data_6_s13_14[,c(46:47,50:61,64:69)] <- NA

data_6_s13_15 <- data_6_s13[data_6_s13$terminal_year == 2015 | data_6_s13$terminal_year == 2016,]
data_6_s13_15$ty_order <- 15
data_6_s13_15$if_diff_5yrs <- "N"
data_6_s13_15$interval <- max(data_6_s13_15$terminal_year) - min(data_6_s13_15$terminal_year)
data_6_s13_15$ty <- 2015
data_6_s13_15$ty_minus1 <- data_6_s13_15$ty-1
data_6_s13_15$key_ty <- paste(data_6_s13_15$assessid, data_6_s13_15$ty, sep="_")
data_6_s13_15$key_ty_minus1 <- paste(data_6_s13_15$assessid, data_6_s13_15$ty_minus1, sep="_")
data_6_s13_15[,c(46:47,50:61,64:69)] <- NA

data_6_s13 <- rbind(data_6_s13_1, data_6_s13_2, data_6_s13_3, data_6_s13_4, data_6_s13_5, data_6_s13_6, data_6_s13_7, data_6_s13_8, data_6_s13_9, data_6_s13_10,
                    data_6_s13_11, data_6_s13_12, data_6_s13_13, data_6_s13_14, data_6_s13_15)

# assess 14
data_6_s14 <- data_6_assess[data_6_assess$stockid==ids[14],]

data_6_s14_1 <- data_6_s14[data_6_s14$terminal_year == 2006 | data_6_s14$terminal_year == 2012,]
data_6_s14_1$ty_order <- 1
data_6_s14_1$if_diff_5yrs <- "Y"
data_6_s14_1$interval <- max(data_6_s14_1$terminal_year) - min(data_6_s14_1$terminal_year)

data_6_s14_2 <- data_6_s14[data_6_s14$terminal_year == 2006 | data_6_s14$terminal_year == 2013,]
data_6_s14_2$ty_order <- 2
data_6_s14_2$if_diff_5yrs <- "Y"
data_6_s14_2$interval <- max(data_6_s14_2$terminal_year) - min(data_6_s14_2$terminal_year)

data_6_s14_3 <- data_6_s14[data_6_s14$terminal_year == 2006 | data_6_s14$terminal_year == 2014,]
data_6_s14_3$ty_order <- 3
data_6_s14_3$if_diff_5yrs <- "Y"
data_6_s14_3$interval <- max(data_6_s14_3$terminal_year) - min(data_6_s14_3$terminal_year)

data_6_s14_4 <- data_6_s14[data_6_s14$terminal_year == 2006 | data_6_s14$terminal_year == 2015,]
data_6_s14_4$ty_order <- 4
data_6_s14_4$if_diff_5yrs <- "Y"
data_6_s14_4$interval <- max(data_6_s14_4$terminal_year) - min(data_6_s14_4$terminal_year)

data_6_s14_5 <- data_6_s14[data_6_s14$terminal_year == 2006 | data_6_s14$terminal_year == 2016,]
data_6_s14_5$ty_order <- 5
data_6_s14_5$if_diff_5yrs <- "Y"
data_6_s14_5$interval <- max(data_6_s14_5$terminal_year) - min(data_6_s14_5$terminal_year)

data_6_s14_6 <- data_6_s14[data_6_s14$terminal_year == 2012 | data_6_s14$terminal_year == 2013,]
data_6_s14_6$ty_order <- 6
data_6_s14_6$if_diff_5yrs <- "N"
data_6_s14_6$interval <- max(data_6_s14_6$terminal_year) - min(data_6_s14_6$terminal_year)
data_6_s14_6$ty <- 2012
data_6_s14_6$ty_minus1 <- data_6_s14_6$ty-1
data_6_s14_6$key_ty <- paste(data_6_s14_6$assessid, data_6_s14_6$ty, sep="_")
data_6_s14_6$key_ty_minus1 <- paste(data_6_s14_6$assessid, data_6_s14_6$ty_minus1, sep="_")
data_6_s14_6[,c(46:47,50:61,64:69)] <- NA

data_6_s14_7 <- data_6_s14[data_6_s14$terminal_year == 2012 | data_6_s14$terminal_year == 2014,]
data_6_s14_7$ty_order <- 7
data_6_s14_7$if_diff_5yrs <- "N"
data_6_s14_7$interval <- max(data_6_s14_7$terminal_year) - min(data_6_s14_7$terminal_year)
data_6_s14_7$ty <- 2012
data_6_s14_7$ty_minus1 <- data_6_s14_7$ty-1
data_6_s14_7$key_ty <- paste(data_6_s14_7$assessid, data_6_s14_7$ty, sep="_")
data_6_s14_7$key_ty_minus1 <- paste(data_6_s14_7$assessid, data_6_s14_7$ty_minus1, sep="_")
data_6_s14_7[,c(46:47,50:61,64:69)] <- NA

data_6_s14_8 <- data_6_s14[data_6_s14$terminal_year == 2012 | data_6_s14$terminal_year == 2015,]
data_6_s14_8$ty_order <- 8
data_6_s14_8$if_diff_5yrs <- "N"
data_6_s14_8$interval <- max(data_6_s14_8$terminal_year) - min(data_6_s14_8$terminal_year)
data_6_s14_8$ty <- 2012
data_6_s14_8$ty_minus1 <- data_6_s14_8$ty-1
data_6_s14_8$key_ty <- paste(data_6_s14_8$assessid, data_6_s14_8$ty, sep="_")
data_6_s14_8$key_ty_minus1 <- paste(data_6_s14_8$assessid, data_6_s14_8$ty_minus1, sep="_")
data_6_s14_8[,c(46:47,50:61,64:69)] <- NA

data_6_s14_9 <- data_6_s14[data_6_s14$terminal_year == 2012 | data_6_s14$terminal_year == 2016,]
data_6_s14_9$ty_order <- 9
data_6_s14_9$if_diff_5yrs <- "N"
data_6_s14_9$interval <- max(data_6_s14_9$terminal_year) - min(data_6_s14_9$terminal_year)
data_6_s14_9$ty <- 2012
data_6_s14_9$ty_minus1 <- data_6_s14_9$ty-1
data_6_s14_9$key_ty <- paste(data_6_s14_9$assessid, data_6_s14_9$ty, sep="_")
data_6_s14_9$key_ty_minus1 <- paste(data_6_s14_9$assessid, data_6_s14_9$ty_minus1, sep="_")
data_6_s14_9[,c(46:47,50:61,64:69)] <- NA

data_6_s14_10 <- data_6_s14[data_6_s14$terminal_year == 2013 | data_6_s14$terminal_year == 2014,]
data_6_s14_10$ty_order <- 10
data_6_s14_10$if_diff_5yrs <- "N"
data_6_s14_10$interval <- max(data_6_s14_10$terminal_year) - min(data_6_s14_10$terminal_year)
data_6_s14_10$ty <- 2013
data_6_s14_10$ty_minus1 <- data_6_s14_10$ty-1
data_6_s14_10$key_ty <- paste(data_6_s14_10$assessid, data_6_s14_10$ty, sep="_")
data_6_s14_10$key_ty_minus1 <- paste(data_6_s14_10$assessid, data_6_s14_10$ty_minus1, sep="_")
data_6_s14_10[,c(46:47,50:61,64:69)] <- NA

data_6_s14_11 <- data_6_s14[data_6_s14$terminal_year == 2013 | data_6_s14$terminal_year == 2015,]
data_6_s14_11$ty_order <- 11
data_6_s14_11$if_diff_5yrs <- "N"
data_6_s14_11$interval <- max(data_6_s14_11$terminal_year) - min(data_6_s14_11$terminal_year)
data_6_s14_11$ty <- 2013
data_6_s14_11$ty_minus1 <- data_6_s14_11$ty-1
data_6_s14_11$key_ty <- paste(data_6_s14_11$assessid, data_6_s14_11$ty, sep="_")
data_6_s14_11$key_ty_minus1 <- paste(data_6_s14_11$assessid, data_6_s14_11$ty_minus1, sep="_")
data_6_s14_11[,c(46:47,50:61,64:69)] <- NA

data_6_s14_12 <- data_6_s14[data_6_s14$terminal_year == 2013 | data_6_s14$terminal_year == 2016,]
data_6_s14_12$ty_order <- 12
data_6_s14_12$if_diff_5yrs <- "N"
data_6_s14_12$interval <- max(data_6_s14_12$terminal_year) - min(data_6_s14_12$terminal_year)
data_6_s14_12$ty <- 2013
data_6_s14_12$ty_minus1 <- data_6_s14_12$ty-1
data_6_s14_12$key_ty <- paste(data_6_s14_12$assessid, data_6_s14_12$ty, sep="_")
data_6_s14_12$key_ty_minus1 <- paste(data_6_s14_12$assessid, data_6_s14_12$ty_minus1, sep="_")
data_6_s14_12[,c(46:47,50:61,64:69)] <- NA

data_6_s14_13 <- data_6_s14[data_6_s14$terminal_year == 2014 | data_6_s14$terminal_year == 2015,]
data_6_s14_13$ty_order <- 13
data_6_s14_13$if_diff_5yrs <- "N"
data_6_s14_13$interval <- max(data_6_s14_13$terminal_year) - min(data_6_s14_13$terminal_year)
data_6_s14_13$ty <- 2014
data_6_s14_13$ty_minus1 <- data_6_s14_13$ty-1
data_6_s14_13$key_ty <- paste(data_6_s14_13$assessid, data_6_s14_13$ty, sep="_")
data_6_s14_13$key_ty_minus1 <- paste(data_6_s14_13$assessid, data_6_s14_13$ty_minus1, sep="_")
data_6_s14_13[,c(46:47,50:61,64:69)] <- NA

data_6_s14_14 <- data_6_s14[data_6_s14$terminal_year == 2014 | data_6_s14$terminal_year == 2016,]
data_6_s14_14$ty_order <- 14
data_6_s14_14$if_diff_5yrs <- "N"
data_6_s14_14$interval <- max(data_6_s14_14$terminal_year) - min(data_6_s14_14$terminal_year)
data_6_s14_14$ty <- 2014
data_6_s14_14$ty_minus1 <- data_6_s14_14$ty-1
data_6_s14_14$key_ty <- paste(data_6_s14_14$assessid, data_6_s14_14$ty, sep="_")
data_6_s14_14$key_ty_minus1 <- paste(data_6_s14_14$assessid, data_6_s14_14$ty_minus1, sep="_")
data_6_s14_14[,c(46:47,50:61,64:69)] <- NA

data_6_s14_15 <- data_6_s14[data_6_s14$terminal_year == 2015 | data_6_s14$terminal_year == 2016,]
data_6_s14_15$ty_order <- 15
data_6_s14_15$if_diff_5yrs <- "N"
data_6_s14_15$interval <- max(data_6_s14_15$terminal_year) - min(data_6_s14_15$terminal_year)
data_6_s14_15$ty <- 2015
data_6_s14_15$ty_minus1 <- data_6_s14_15$ty-1
data_6_s14_15$key_ty <- paste(data_6_s14_15$assessid, data_6_s14_15$ty, sep="_")
data_6_s14_15$key_ty_minus1 <- paste(data_6_s14_15$assessid, data_6_s14_15$ty_minus1, sep="_")
data_6_s14_15[,c(46:47,50:61,64:69)] <- NA

data_6_s14 <- rbind(data_6_s14_1, data_6_s14_2, data_6_s14_3, data_6_s14_4, data_6_s14_5, data_6_s14_6, data_6_s14_7, data_6_s14_8, data_6_s14_9, data_6_s14_10,
                    data_6_s14_11, data_6_s14_12, data_6_s14_13, data_6_s14_14, data_6_s14_15)

# assess 15
data_6_s15 <- data_6_assess[data_6_assess$stockid==ids[15],]

data_6_s15_1 <- data_6_s15[data_6_s15$terminal_year == 2011 | data_6_s15$terminal_year == 2012,]
data_6_s15_1$ty_order <- 1
data_6_s15_1$if_diff_5yrs <- "N"
data_6_s15_1$interval <- max(data_6_s15_1$terminal_year) - min(data_6_s15_1$terminal_year)

data_6_s15_2 <- data_6_s15[data_6_s15$terminal_year == 2011 | data_6_s15$terminal_year == 2013,]
data_6_s15_2$ty_order <- 2
data_6_s15_2$if_diff_5yrs <- "N"
data_6_s15_2$interval <- max(data_6_s15_2$terminal_year) - min(data_6_s15_2$terminal_year)

data_6_s15_3 <- data_6_s15[data_6_s15$terminal_year == 2011 | data_6_s15$terminal_year == 2014,]
data_6_s15_3$ty_order <- 3
data_6_s15_3$if_diff_5yrs <- "N"
data_6_s15_3$interval <- max(data_6_s15_3$terminal_year) - min(data_6_s15_3$terminal_year)

data_6_s15_4 <- data_6_s15[data_6_s15$terminal_year == 2011 | data_6_s15$terminal_year == 2016,]
data_6_s15_4$ty_order <- 4
data_6_s15_4$if_diff_5yrs <- "N"
data_6_s15_4$interval <- max(data_6_s15_4$terminal_year) - min(data_6_s15_4$terminal_year)

data_6_s15_5 <- data_6_s15[data_6_s15$terminal_year == 2011 | data_6_s15$terminal_year == 2017,]
data_6_s15_5$ty_order <- 5
data_6_s15_5$if_diff_5yrs <- "Y"
data_6_s15_5$interval <- max(data_6_s15_5$terminal_year) - min(data_6_s15_5$terminal_year)

data_6_s15_6 <- data_6_s15[data_6_s15$terminal_year == 2012 | data_6_s15$terminal_year == 2013,]
data_6_s15_6$ty_order <- 6
data_6_s15_6$if_diff_5yrs <- "N"
data_6_s15_6$interval <- max(data_6_s15_6$terminal_year) - min(data_6_s15_6$terminal_year)
data_6_s15_6$ty <- 2012
data_6_s15_6$ty_minus1 <- data_6_s15_6$ty-1
data_6_s15_6$key_ty <- paste(data_6_s15_6$assessid, data_6_s15_6$ty, sep="_")
data_6_s15_6$key_ty_minus1 <- paste(data_6_s15_6$assessid, data_6_s15_6$ty_minus1, sep="_")
data_6_s15_6[,c(46:47,50:61,64:69)] <- NA

data_6_s15_7 <- data_6_s15[data_6_s15$terminal_year == 2012 | data_6_s15$terminal_year == 2014,]
data_6_s15_7$ty_order <- 7
data_6_s15_7$if_diff_5yrs <- "N"
data_6_s15_7$interval <- max(data_6_s15_7$terminal_year) - min(data_6_s15_7$terminal_year)
data_6_s15_7$ty <- 2012
data_6_s15_7$ty_minus1 <- data_6_s15_7$ty-1
data_6_s15_7$key_ty <- paste(data_6_s15_7$assessid, data_6_s15_7$ty, sep="_")
data_6_s15_7$key_ty_minus1 <- paste(data_6_s15_7$assessid, data_6_s15_7$ty_minus1, sep="_")
data_6_s15_7[,c(46:47,50:61,64:69)] <- NA

data_6_s15_8 <- data_6_s15[data_6_s15$terminal_year == 2012 | data_6_s15$terminal_year == 2016,]
data_6_s15_8$ty_order <- 8
data_6_s15_8$if_diff_5yrs <- "N"
data_6_s15_8$interval <- max(data_6_s15_8$terminal_year) - min(data_6_s15_8$terminal_year)
data_6_s15_8$ty <- 2012
data_6_s15_8$ty_minus1 <- data_6_s15_8$ty-1
data_6_s15_8$key_ty <- paste(data_6_s15_8$assessid, data_6_s15_8$ty, sep="_")
data_6_s15_8$key_ty_minus1 <- paste(data_6_s15_8$assessid, data_6_s15_8$ty_minus1, sep="_")
data_6_s15_8[,c(46:47,50:61,64:69)] <- NA

data_6_s15_9 <- data_6_s15[data_6_s15$terminal_year == 2012 | data_6_s15$terminal_year == 2017,]
data_6_s15_9$ty_order <- 9
data_6_s15_9$if_diff_5yrs <- "N"
data_6_s15_9$interval <- max(data_6_s15_9$terminal_year) - min(data_6_s15_9$terminal_year)
data_6_s15_9$ty <- 2012
data_6_s15_9$ty_minus1 <- data_6_s15_9$ty-1
data_6_s15_9$key_ty <- paste(data_6_s15_9$assessid, data_6_s15_9$ty, sep="_")
data_6_s15_9$key_ty_minus1 <- paste(data_6_s15_9$assessid, data_6_s15_9$ty_minus1, sep="_")
data_6_s15_9[,c(46:47,50:61,64:69)] <- NA

data_6_s15_10 <- data_6_s15[data_6_s15$terminal_year == 2013 | data_6_s15$terminal_year == 2014,]
data_6_s15_10$ty_order <- 10
data_6_s15_10$if_diff_5yrs <- "N"
data_6_s15_10$interval <- max(data_6_s15_10$terminal_year) - min(data_6_s15_10$terminal_year)
data_6_s15_10$ty <- 2013
data_6_s15_10$ty_minus1 <- data_6_s15_10$ty-1
data_6_s15_10$key_ty <- paste(data_6_s15_10$assessid, data_6_s15_10$ty, sep="_")
data_6_s15_10$key_ty_minus1 <- paste(data_6_s15_10$assessid, data_6_s15_10$ty_minus1, sep="_")
data_6_s15_10[,c(46:47,50:61,64:69)] <- NA

data_6_s15_11 <- data_6_s15[data_6_s15$terminal_year == 2013 | data_6_s15$terminal_year == 2016,]
data_6_s15_11$ty_order <- 11
data_6_s15_11$if_diff_5yrs <- "N"
data_6_s15_11$interval <- max(data_6_s15_11$terminal_year) - min(data_6_s15_11$terminal_year)
data_6_s15_11$ty <- 2013
data_6_s15_11$ty_minus1 <- data_6_s15_11$ty-1
data_6_s15_11$key_ty <- paste(data_6_s15_11$assessid, data_6_s15_11$ty, sep="_")
data_6_s15_11$key_ty_minus1 <- paste(data_6_s15_11$assessid, data_6_s15_11$ty_minus1, sep="_")
data_6_s15_11[,c(46:47,50:61,64:69)] <- NA

data_6_s15_12 <- data_6_s15[data_6_s15$terminal_year == 2013 | data_6_s15$terminal_year == 2017,]
data_6_s15_12$ty_order <- 12
data_6_s15_12$if_diff_5yrs <- "N"
data_6_s15_12$interval <- max(data_6_s15_12$terminal_year) - min(data_6_s15_12$terminal_year)
data_6_s15_12$ty <- 2013
data_6_s15_12$ty_minus1 <- data_6_s15_12$ty-1
data_6_s15_12$key_ty <- paste(data_6_s15_12$assessid, data_6_s15_12$ty, sep="_")
data_6_s15_12$key_ty_minus1 <- paste(data_6_s15_12$assessid, data_6_s15_12$ty_minus1, sep="_")
data_6_s15_12[,c(46:47,50:61,64:69)] <- NA

data_6_s15_13 <- data_6_s15[data_6_s15$terminal_year == 2014 | data_6_s15$terminal_year == 2016,]
data_6_s15_13$ty_order <- 13
data_6_s15_13$if_diff_5yrs <- "N"
data_6_s15_13$interval <- max(data_6_s15_13$terminal_year) - min(data_6_s15_13$terminal_year)
data_6_s15_13$ty <- 2014
data_6_s15_13$ty_minus1 <- data_6_s15_13$ty-1
data_6_s15_13$key_ty <- paste(data_6_s15_13$assessid, data_6_s15_13$ty, sep="_")
data_6_s15_13$key_ty_minus1 <- paste(data_6_s15_13$assessid, data_6_s15_13$ty_minus1, sep="_")
data_6_s15_13[,c(46:47,50:61,64:69)] <- NA

data_6_s15_14 <- data_6_s15[data_6_s15$terminal_year == 2014 | data_6_s15$terminal_year == 2017,]
data_6_s15_14$ty_order <- 14
data_6_s15_14$if_diff_5yrs <- "N"
data_6_s15_14$interval <- max(data_6_s15_14$terminal_year) - min(data_6_s15_14$terminal_year)
data_6_s15_14$ty <- 2014
data_6_s15_14$ty_minus1 <- data_6_s15_14$ty-1
data_6_s15_14$key_ty <- paste(data_6_s15_14$assessid, data_6_s15_14$ty, sep="_")
data_6_s15_14$key_ty_minus1 <- paste(data_6_s15_14$assessid, data_6_s15_14$ty_minus1, sep="_")
data_6_s15_14[,c(46:47,50:61,64:69)] <- NA

data_6_s15_15 <- data_6_s15[data_6_s15$terminal_year == 2016 | data_6_s15$terminal_year == 2017,]
data_6_s15_15$ty_order <- 15
data_6_s15_15$if_diff_5yrs <- "N"
data_6_s15_15$interval <- max(data_6_s15_15$terminal_year) - min(data_6_s15_15$terminal_year)
data_6_s15_15$ty <- 2015
data_6_s15_15$ty_minus1 <- data_6_s15_15$ty-1
data_6_s15_15$key_ty <- paste(data_6_s15_15$assessid, data_6_s15_15$ty, sep="_")
data_6_s15_15$key_ty_minus1 <- paste(data_6_s15_15$assessid, data_6_s15_15$ty_minus1, sep="_")
data_6_s15_15[,c(46:47,50:61,64:69)] <- NA

data_6_s15 <- rbind(data_6_s15_1, data_6_s15_2, data_6_s15_3, data_6_s15_4, data_6_s15_5, data_6_s15_6, data_6_s15_7, data_6_s15_8, data_6_s15_9, data_6_s15_10,
                    data_6_s15_11, data_6_s15_12, data_6_s15_13, data_6_s15_14, data_6_s15_15)

# assess 16
data_6_s16 <- data_6_assess[data_6_assess$stockid==ids[16],]

data_6_s16_1 <- data_6_s16[data_6_s16$terminal_year == 2013 | data_6_s16$assessid=="WGNSSK-HADNS-IIIa-VIa-1972-2015-ICESIMP2016",]
data_6_s16_1$ty_order <- 1
data_6_s16_1$if_diff_5yrs <- "N"
data_6_s16_1$interval <- max(data_6_s16_1$terminal_year) - min(data_6_s16_1$terminal_year)

data_6_s16_2 <- data_6_s16[data_6_s16$terminal_year == 2013 | data_6_s16$assessid=="WGNSSK-HADNS-IIIa-VIa-1972-2015-ICESIMP2018",]
data_6_s16_2$ty_order <- 2
data_6_s16_2$if_diff_5yrs <- "N"
data_6_s16_2$interval <- max(data_6_s16_2$terminal_year) - min(data_6_s16_2$terminal_year)

data_6_s16_3 <- data_6_s16[data_6_s16$terminal_year == 2013 | data_6_s16$terminal_year == 2015,]
data_6_s16_3$ty_order <- 3
data_6_s16_3$if_diff_5yrs <- "N"
data_6_s16_3$interval <- max(data_6_s16_3$terminal_year) - min(data_6_s16_3$terminal_year)

data_6_s16_4 <- data_6_s16[data_6_s16$terminal_year == 2013 | data_6_s16$terminal_year == 2016,]
data_6_s16_4$ty_order <- 4
data_6_s16_4$if_diff_5yrs <- "N"
data_6_s16_4$interval <- max(data_6_s16_4$terminal_year) - min(data_6_s16_4$terminal_year)

data_6_s16_5 <- data_6_s16[data_6_s16$terminal_year == 2013 | data_6_s16$terminal_year == 2018,]
data_6_s16_5$ty_order <- 5
data_6_s16_5$if_diff_5yrs <- "N"
data_6_s16_5$interval <- max(data_6_s16_5$terminal_year) - min(data_6_s16_5$terminal_year)

data_6_s16_6 <- data_6_s16[data_6_s16$assessid=="WGNSSK-HADNS-IIIa-VIa-1972-2015-ICESIMP2016" | data_6_s16$assessid=="WGNSSK-HADNS-IIIa-VIa-1972-2015-ICESIMP2018",]
data_6_s16_6$ty_order <- 6
data_6_s16_6$if_diff_5yrs <- "N"
data_6_s16_6$interval <- max(data_6_s16_6$terminal_year) - min(data_6_s16_6$terminal_year)
data_6_s16_6$ty <- 2014
data_6_s16_6$ty_minus1 <- data_6_s16_6$ty-1
data_6_s16_6$key_ty <- paste(data_6_s16_6$assessid, data_6_s16_6$ty, sep="_")
data_6_s16_6$key_ty_minus1 <- paste(data_6_s16_6$assessid, data_6_s16_6$ty_minus1, sep="_")
data_6_s16_6[,c(46:47,50:61,64:69)] <- NA

data_6_s16_7 <- data_6_s16[data_6_s16$assessid=="WGNSSK-HADNS-IIIa-VIa-1972-2015-ICESIMP2016" | data_6_s16$terminal_year == 2015,]
data_6_s16_7$ty_order <- 7
data_6_s16_7$if_diff_5yrs <- "N"
data_6_s16_7$interval <- max(data_6_s16_7$terminal_year) - min(data_6_s16_7$terminal_year)
data_6_s16_7$ty <- 2014
data_6_s16_7$ty_minus1 <- data_6_s16_7$ty-1
data_6_s16_7$key_ty <- paste(data_6_s16_7$assessid, data_6_s16_7$ty, sep="_")
data_6_s16_7$key_ty_minus1 <- paste(data_6_s16_7$assessid, data_6_s16_7$ty_minus1, sep="_")
data_6_s16_7[,c(46:47,50:61,64:69)] <- NA

data_6_s16_8 <- data_6_s16[data_6_s16$assessid=="WGNSSK-HADNS-IIIa-VIa-1972-2015-ICESIMP2016" | data_6_s16$terminal_year == 2016,]
data_6_s16_8$ty_order <- 8
data_6_s16_8$if_diff_5yrs <- "N"
data_6_s16_8$interval <- max(data_6_s16_8$terminal_year) - min(data_6_s16_8$terminal_year)
data_6_s16_8$ty <- 2014
data_6_s16_8$ty_minus1 <- data_6_s16_8$ty-1
data_6_s16_8$key_ty <- paste(data_6_s16_8$assessid, data_6_s16_8$ty, sep="_")
data_6_s16_8$key_ty_minus1 <- paste(data_6_s16_8$assessid, data_6_s16_8$ty_minus1, sep="_")
data_6_s16_8[,c(46:47,50:61,64:69)] <- NA

data_6_s16_9 <- data_6_s16[data_6_s16$assessid=="WGNSSK-HADNS-IIIa-VIa-1972-2015-ICESIMP2016" | data_6_s16$terminal_year == 2018,]
data_6_s16_9$ty_order <- 9
data_6_s16_9$if_diff_5yrs <- "N"
data_6_s16_9$interval <- max(data_6_s16_9$terminal_year) - min(data_6_s16_9$terminal_year)
data_6_s16_9$ty <- 2014
data_6_s16_9$ty_minus1 <- data_6_s16_9$ty-1
data_6_s16_9$key_ty <- paste(data_6_s16_9$assessid, data_6_s16_9$ty, sep="_")
data_6_s16_9$key_ty_minus1 <- paste(data_6_s16_9$assessid, data_6_s16_9$ty_minus1, sep="_")
data_6_s16_9[,c(46:47,50:61,64:69)] <- NA

data_6_s16_10 <- data_6_s16[data_6_s16$assessid=="WGNSSK-HADNS-IIIa-VIa-1972-2015-ICESIMP2018" | data_6_s16$terminal_year == 2015,]
data_6_s16_10$ty_order <- 10
data_6_s16_10$if_diff_5yrs <- "N"
data_6_s16_10$interval <- max(data_6_s16_10$terminal_year) - min(data_6_s16_10$terminal_year)
data_6_s16_10$ty <- 2014
data_6_s16_10$ty_minus1 <- data_6_s16_10$ty-1
data_6_s16_10$key_ty <- paste(data_6_s16_10$assessid, data_6_s16_10$ty, sep="_")
data_6_s16_10$key_ty_minus1 <- paste(data_6_s16_10$assessid, data_6_s16_10$ty_minus1, sep="_")
data_6_s16_10[,c(46:47,50:61,64:69)] <- NA

data_6_s16_11 <- data_6_s16[data_6_s16$assessid=="WGNSSK-HADNS-IIIa-VIa-1972-2015-ICESIMP2018" | data_6_s16$terminal_year == 2016,]
data_6_s16_11$ty_order <- 11
data_6_s16_11$if_diff_5yrs <- "N"
data_6_s16_11$interval <- max(data_6_s16_11$terminal_year) - min(data_6_s16_11$terminal_year)
data_6_s16_11$ty <- 2014
data_6_s16_11$ty_minus1 <- data_6_s16_11$ty-1
data_6_s16_11$key_ty <- paste(data_6_s16_11$assessid, data_6_s16_11$ty, sep="_")
data_6_s16_11$key_ty_minus1 <- paste(data_6_s16_11$assessid, data_6_s16_11$ty_minus1, sep="_")
data_6_s16_11[,c(46:47,50:61,64:69)] <- NA

data_6_s16_12 <- data_6_s16[data_6_s16$assessid=="WGNSSK-HADNS-IIIa-VIa-1972-2015-ICESIMP2018" | data_6_s16$terminal_year == 2018,]
data_6_s16_12$ty_order <- 12
data_6_s16_12$if_diff_5yrs <- "N"
data_6_s16_12$interval <- max(data_6_s16_12$terminal_year) - min(data_6_s16_12$terminal_year)
data_6_s16_12$ty <- 2014
data_6_s16_12$ty_minus1 <- data_6_s16_12$ty-1
data_6_s16_12$key_ty <- paste(data_6_s16_12$assessid, data_6_s16_12$ty, sep="_")
data_6_s16_12$key_ty_minus1 <- paste(data_6_s16_12$assessid, data_6_s16_12$ty_minus1, sep="_")
data_6_s16_12[,c(46:47,50:61,64:69)] <- NA

data_6_s16_13 <- data_6_s16[data_6_s16$terminal_year == 2015 | data_6_s16$terminal_year == 2016,]
data_6_s16_13$ty_order <- 13
data_6_s16_13$if_diff_5yrs <- "N"
data_6_s16_13$interval <- max(data_6_s16_13$terminal_year) - min(data_6_s16_13$terminal_year)
data_6_s16_13$ty <- 2015
data_6_s16_13$ty_minus1 <- data_6_s16_13$ty-1
data_6_s16_13$key_ty <- paste(data_6_s16_13$assessid, data_6_s16_13$ty, sep="_")
data_6_s16_13$key_ty_minus1 <- paste(data_6_s16_13$assessid, data_6_s16_13$ty_minus1, sep="_")
data_6_s16_13[,c(46:47,50:61,64:69)] <- NA

data_6_s16_14 <- data_6_s16[data_6_s16$terminal_year == 2015 | data_6_s16$terminal_year == 2018,]
data_6_s16_14$ty_order <- 14
data_6_s16_14$if_diff_5yrs <- "N"
data_6_s16_14$interval <- max(data_6_s16_14$terminal_year) - min(data_6_s16_14$terminal_year)
data_6_s16_14$ty <- 2015
data_6_s16_14$ty_minus1 <- data_6_s16_14$ty-1
data_6_s16_14$key_ty <- paste(data_6_s16_14$assessid, data_6_s16_14$ty, sep="_")
data_6_s16_14$key_ty_minus1 <- paste(data_6_s16_14$assessid, data_6_s16_14$ty_minus1, sep="_")
data_6_s16_14[,c(46:47,50:61,64:69)] <- NA

data_6_s16_15 <- data_6_s16[data_6_s16$terminal_year == 2016 | data_6_s16$terminal_year == 2018,]
data_6_s16_15$ty_order <- 15
data_6_s16_15$if_diff_5yrs <- "N"
data_6_s16_15$interval <- max(data_6_s16_15$terminal_year) - min(data_6_s16_15$terminal_year)
data_6_s16_15$ty <- 2016
data_6_s16_15$ty_minus1 <- data_6_s16_15$ty-1
data_6_s16_15$key_ty <- paste(data_6_s16_15$assessid, data_6_s16_15$ty, sep="_")
data_6_s16_15$key_ty_minus1 <- paste(data_6_s16_15$assessid, data_6_s16_15$ty_minus1, sep="_")
data_6_s16_15[,c(46:47,50:61,64:69)] <- NA

data_6_s16 <- rbind(data_6_s16_1, data_6_s16_2, data_6_s16_3, data_6_s16_4, data_6_s16_5, data_6_s16_6, data_6_s16_7, data_6_s16_8, data_6_s16_9, data_6_s16_10,
                    data_6_s16_11, data_6_s16_12, data_6_s16_13, data_6_s16_14, data_6_s16_15)

# assess 17
data_6_s17 <- data_6_assess[data_6_assess$stockid==ids[17],]

data_6_s17_1 <- data_6_s17[data_6_s17$terminal_year == 2011 | data_6_s17$terminal_year == 2012,]
data_6_s17_1$ty_order <- 1
data_6_s17_1$if_diff_5yrs <- "N"
data_6_s17_1$interval <- max(data_6_s17_1$terminal_year) - min(data_6_s17_1$terminal_year)

data_6_s17_2 <- data_6_s17[data_6_s17$terminal_year == 2011 | data_6_s17$terminal_year == 2013,]
data_6_s17_2$ty_order <- 2
data_6_s17_2$if_diff_5yrs <- "N"
data_6_s17_2$interval <- max(data_6_s17_2$terminal_year) - min(data_6_s17_2$terminal_year)

data_6_s17_3 <- data_6_s17[data_6_s17$terminal_year == 2011 | data_6_s17$terminal_year == 2014,]
data_6_s17_3$ty_order <- 3
data_6_s17_3$if_diff_5yrs <- "N"
data_6_s17_3$interval <- max(data_6_s17_3$terminal_year) - min(data_6_s17_3$terminal_year)

data_6_s17_4 <- data_6_s17[data_6_s17$terminal_year == 2011 | data_6_s17$terminal_year == 2015,]
data_6_s17_4$ty_order <- 4
data_6_s17_4$if_diff_5yrs <- "N"
data_6_s17_4$interval <- max(data_6_s17_4$terminal_year) - min(data_6_s17_4$terminal_year)

data_6_s17_5 <- data_6_s17[data_6_s17$terminal_year == 2011 | data_6_s17$terminal_year == 2016,]
data_6_s17_5$ty_order <- 5
data_6_s17_5$if_diff_5yrs <- "N"
data_6_s17_5$interval <- max(data_6_s17_5$terminal_year) - min(data_6_s17_5$terminal_year)

data_6_s17_6 <- data_6_s17[data_6_s17$terminal_year == 2012 | data_6_s17$terminal_year == 2013,]
data_6_s17_6$ty_order <- 6
data_6_s17_6$if_diff_5yrs <- "N"
data_6_s17_6$interval <- max(data_6_s17_6$terminal_year) - min(data_6_s17_6$terminal_year)
data_6_s17_6$ty <- 2012
data_6_s17_6$ty_minus1 <- data_6_s17_6$ty-1
data_6_s17_6$key_ty <- paste(data_6_s17_6$assessid, data_6_s17_6$ty, sep="_")
data_6_s17_6$key_ty_minus1 <- paste(data_6_s17_6$assessid, data_6_s17_6$ty_minus1, sep="_")
data_6_s17_6[,c(46:47,50:61,64:69)] <- NA

data_6_s17_7 <- data_6_s17[data_6_s17$terminal_year == 2012 | data_6_s17$terminal_year == 2014,]
data_6_s17_7$ty_order <- 7
data_6_s17_7$if_diff_5yrs <- "N"
data_6_s17_7$interval <- max(data_6_s17_7$terminal_year) - min(data_6_s17_7$terminal_year)
data_6_s17_7$ty <- 2012
data_6_s17_7$ty_minus1 <- data_6_s17_7$ty-1
data_6_s17_7$key_ty <- paste(data_6_s17_7$assessid, data_6_s17_7$ty, sep="_")
data_6_s17_7$key_ty_minus1 <- paste(data_6_s17_7$assessid, data_6_s17_7$ty_minus1, sep="_")
data_6_s17_7[,c(46:47,50:61,64:69)] <- NA

data_6_s17_8 <- data_6_s17[data_6_s17$terminal_year == 2012 | data_6_s17$terminal_year == 2015,]
data_6_s17_8$ty_order <- 8
data_6_s17_8$if_diff_5yrs <- "N"
data_6_s17_8$interval <- max(data_6_s17_8$terminal_year) - min(data_6_s17_8$terminal_year)
data_6_s17_8$ty <- 2012
data_6_s17_8$ty_minus1 <- data_6_s17_8$ty-1
data_6_s17_8$key_ty <- paste(data_6_s17_8$assessid, data_6_s17_8$ty, sep="_")
data_6_s17_8$key_ty_minus1 <- paste(data_6_s17_8$assessid, data_6_s17_8$ty_minus1, sep="_")
data_6_s17_8[,c(46:47,50:61,64:69)] <- NA

data_6_s17_9 <- data_6_s17[data_6_s17$terminal_year == 2012 | data_6_s17$terminal_year == 2016,]
data_6_s17_9$ty_order <- 9
data_6_s17_9$if_diff_5yrs <- "N"
data_6_s17_9$interval <- max(data_6_s17_9$terminal_year) - min(data_6_s17_9$terminal_year)
data_6_s17_9$ty <- 2012
data_6_s17_9$ty_minus1 <- data_6_s17_9$ty-1
data_6_s17_9$key_ty <- paste(data_6_s17_9$assessid, data_6_s17_9$ty, sep="_")
data_6_s17_9$key_ty_minus1 <- paste(data_6_s17_9$assessid, data_6_s17_9$ty_minus1, sep="_")
data_6_s17_9[,c(46:47,50:61,64:69)] <- NA

data_6_s17_10 <- data_6_s17[data_6_s17$terminal_year == 2013 | data_6_s17$terminal_year == 2014,]
data_6_s17_10$ty_order <- 10
data_6_s17_10$if_diff_5yrs <- "N"
data_6_s17_10$interval <- max(data_6_s17_10$terminal_year) - min(data_6_s17_10$terminal_year)
data_6_s17_10$ty <- 2013
data_6_s17_10$ty_minus1 <- data_6_s17_10$ty-1
data_6_s17_10$key_ty <- paste(data_6_s17_10$assessid, data_6_s17_10$ty, sep="_")
data_6_s17_10$key_ty_minus1 <- paste(data_6_s17_10$assessid, data_6_s17_10$ty_minus1, sep="_")
data_6_s17_10[,c(46:47,50:61,64:69)] <- NA

data_6_s17_11 <- data_6_s17[data_6_s17$terminal_year == 2013 | data_6_s17$terminal_year == 2015,]
data_6_s17_11$ty_order <- 11
data_6_s17_11$if_diff_5yrs <- "N"
data_6_s17_11$interval <- max(data_6_s17_11$terminal_year) - min(data_6_s17_11$terminal_year)
data_6_s17_11$ty <- 2013
data_6_s17_11$ty_minus1 <- data_6_s17_11$ty-1
data_6_s17_11$key_ty <- paste(data_6_s17_11$assessid, data_6_s17_11$ty, sep="_")
data_6_s17_11$key_ty_minus1 <- paste(data_6_s17_11$assessid, data_6_s17_11$ty_minus1, sep="_")
data_6_s17_11[,c(46:47,50:61,64:69)] <- NA

data_6_s17_12 <- data_6_s17[data_6_s17$terminal_year == 2013 | data_6_s17$terminal_year == 2016,]
data_6_s17_12$ty_order <- 12
data_6_s17_12$if_diff_5yrs <- "N"
data_6_s17_12$interval <- max(data_6_s17_12$terminal_year) - min(data_6_s17_12$terminal_year)
data_6_s17_12$ty <- 2013
data_6_s17_12$ty_minus1 <- data_6_s17_12$ty-1
data_6_s17_12$key_ty <- paste(data_6_s17_12$assessid, data_6_s17_12$ty, sep="_")
data_6_s17_12$key_ty_minus1 <- paste(data_6_s17_12$assessid, data_6_s17_12$ty_minus1, sep="_")
data_6_s17_12[,c(46:47,50:61,64:69)] <- NA

data_6_s17_13 <- data_6_s17[data_6_s17$terminal_year == 2014 | data_6_s17$terminal_year == 2015,]
data_6_s17_13$ty_order <- 13
data_6_s17_13$if_diff_5yrs <- "N"
data_6_s17_13$interval <- max(data_6_s17_13$terminal_year) - min(data_6_s17_13$terminal_year)
data_6_s17_13$ty <- 2014
data_6_s17_13$ty_minus1 <- data_6_s17_13$ty-1
data_6_s17_13$key_ty <- paste(data_6_s17_13$assessid, data_6_s17_13$ty, sep="_")
data_6_s17_13$key_ty_minus1 <- paste(data_6_s17_13$assessid, data_6_s17_13$ty_minus1, sep="_")
data_6_s17_13[,c(46:47,50:61,64:69)] <- NA

data_6_s17_14 <- data_6_s17[data_6_s17$terminal_year == 2014 | data_6_s17$terminal_year == 2016,]
data_6_s17_14$ty_order <- 14
data_6_s17_14$if_diff_5yrs <- "N"
data_6_s17_14$interval <- max(data_6_s17_14$terminal_year) - min(data_6_s17_14$terminal_year)
data_6_s17_14$ty <- 2014
data_6_s17_14$ty_minus1 <- data_6_s17_14$ty-1
data_6_s17_14$key_ty <- paste(data_6_s17_14$assessid, data_6_s17_14$ty, sep="_")
data_6_s17_14$key_ty_minus1 <- paste(data_6_s17_14$assessid, data_6_s17_14$ty_minus1, sep="_")
data_6_s17_14[,c(46:47,50:61,64:69)] <- NA

data_6_s17_15 <- data_6_s17[data_6_s17$terminal_year == 2015 | data_6_s17$terminal_year == 2016,]
data_6_s17_15$ty_order <- 15
data_6_s17_15$if_diff_5yrs <- "N"
data_6_s17_15$interval <- max(data_6_s17_15$terminal_year) - min(data_6_s17_15$terminal_year)
data_6_s17_15$ty <- 2015
data_6_s17_15$ty_minus1 <- data_6_s17_15$ty-1
data_6_s17_15$key_ty <- paste(data_6_s17_15$assessid, data_6_s17_15$ty, sep="_")
data_6_s17_15$key_ty_minus1 <- paste(data_6_s17_15$assessid, data_6_s17_15$ty_minus1, sep="_")
data_6_s17_15[,c(46:47,50:61,64:69)] <- NA

data_6_s17 <- rbind(data_6_s17_1, data_6_s17_2, data_6_s17_3, data_6_s17_4, data_6_s17_5, data_6_s17_6, data_6_s17_7, data_6_s17_8, data_6_s17_9, data_6_s17_10,
                    data_6_s17_11, data_6_s17_12, data_6_s17_13, data_6_s17_14, data_6_s17_15)

data_6 <- rbind(data_6_s1, data_6_s2, data_6_s3, data_6_s4, data_6_s5,
                data_6_s6, data_6_s7, data_6_s8, data_6_s9, data_6_s10, 
                data_6_s11, data_6_s12, data_6_s13, data_6_s14, data_6_s15, 
                data_6_s16, data_6_s17)

write.csv(data_6, paste(outputdir, "0e_5_data_6_assess_with_records.csv", sep="/"), row.names=F)

########################################################################################################################################

## Stocks with 7 assessments
data_7_assess <- dd_new2[dd_new2$stockid %in% stockid_7assess,]
length(unique(data_7_assess$stockid))  # 28 stocks
length(unique(data_7_assess$assessid))  # 196 assesses

ids <- unique(data_7_assess$stockid)

# assess 1
data_7_s1 <- data_7_assess[data_7_assess$stockid==ids[1],]

data_7_s1_1 <- data_7_s1[data_7_s1$terminal_year == 2010 | data_7_s1$terminal_year == 2012,]
data_7_s1_1$ty_order <- 1
data_7_s1_1$if_diff_5yrs <- "N"
data_7_s1_1$interval <- max(data_7_s1_1$terminal_year) - min(data_7_s1_1$terminal_year)

data_7_s1_2 <- data_7_s1[data_7_s1$terminal_year == 2010 | data_7_s1$terminal_year == 2013,]
data_7_s1_2$ty_order <- 2
data_7_s1_2$if_diff_5yrs <- "N"
data_7_s1_2$interval <- max(data_7_s1_2$terminal_year) - min(data_7_s1_2$terminal_year)

data_7_s1_3 <- data_7_s1[data_7_s1$terminal_year == 2010 | data_7_s1$terminal_year == 2014,]
data_7_s1_3$ty_order <- 3
data_7_s1_3$if_diff_5yrs <- "N"
data_7_s1_3$interval <- max(data_7_s1_3$terminal_year) - min(data_7_s1_3$terminal_year)

data_7_s1_4 <- data_7_s1[data_7_s1$terminal_year == 2010 | data_7_s1$terminal_year == 2015,]
data_7_s1_4$ty_order <- 4
data_7_s1_4$if_diff_5yrs <- "N"
data_7_s1_4$interval <- max(data_7_s1_4$terminal_year) - min(data_7_s1_4$terminal_year)

data_7_s1_5 <- data_7_s1[data_7_s1$terminal_year == 2010 | data_7_s1$terminal_year == 2016,]
data_7_s1_5$ty_order <- 5
data_7_s1_5$if_diff_5yrs <- "Y"
data_7_s1_5$interval <- max(data_7_s1_5$terminal_year) - min(data_7_s1_5$terminal_year)

data_7_s1_6 <- data_7_s1[data_7_s1$terminal_year == 2010 | data_7_s1$terminal_year == 2017,]
data_7_s1_6$ty_order <- 6
data_7_s1_6$if_diff_5yrs <- "Y"
data_7_s1_6$interval <- max(data_7_s1_6$terminal_year) - min(data_7_s1_6$terminal_year)

data_7_s1_7 <- data_7_s1[data_7_s1$terminal_year == 2012 | data_7_s1$terminal_year == 2013,]
data_7_s1_7$ty_order <- 7
data_7_s1_7$if_diff_5yrs <- "N"
data_7_s1_7$interval <- max(data_7_s1_7$terminal_year) - min(data_7_s1_7$terminal_year)
data_7_s1_7$ty <- 2012
data_7_s1_7$ty_minus1 <- data_7_s1_7$ty-1
data_7_s1_7$key_ty <- paste(data_7_s1_7$assessid, data_7_s1_7$ty, sep="_")
data_7_s1_7$key_ty_minus1 <- paste(data_7_s1_7$assessid, data_7_s1_7$ty_minus1, sep="_")
data_7_s1_7[,c(46:47,50:61,64:69)] <- NA

data_7_s1_8 <- data_7_s1[data_7_s1$terminal_year == 2012 | data_7_s1$terminal_year == 2014,]
data_7_s1_8$ty_order <- 8
data_7_s1_8$if_diff_5yrs <- "N"
data_7_s1_8$interval <- max(data_7_s1_8$terminal_year) - min(data_7_s1_8$terminal_year)
data_7_s1_8$ty <- 2012
data_7_s1_8$ty_minus1 <- data_7_s1_8$ty-1
data_7_s1_8$key_ty <- paste(data_7_s1_8$assessid, data_7_s1_8$ty, sep="_")
data_7_s1_8$key_ty_minus1 <- paste(data_7_s1_8$assessid, data_7_s1_8$ty_minus1, sep="_")
data_7_s1_8[,c(46:47,50:61,64:69)] <- NA

data_7_s1_9 <- data_7_s1[data_7_s1$terminal_year == 2012 | data_7_s1$terminal_year == 2015,]
data_7_s1_9$ty_order <- 9
data_7_s1_9$if_diff_5yrs <- "N"
data_7_s1_9$interval <- max(data_7_s1_9$terminal_year) - min(data_7_s1_9$terminal_year)
data_7_s1_9$ty <- 2012
data_7_s1_9$ty_minus1 <- data_7_s1_9$ty-1
data_7_s1_9$key_ty <- paste(data_7_s1_9$assessid, data_7_s1_9$ty, sep="_")
data_7_s1_9$key_ty_minus1 <- paste(data_7_s1_9$assessid, data_7_s1_9$ty_minus1, sep="_")
data_7_s1_9[,c(46:47,50:61,64:69)] <- NA

data_7_s1_10 <- data_7_s1[data_7_s1$terminal_year == 2012 | data_7_s1$terminal_year == 2016,]
data_7_s1_10$ty_order <- 10
data_7_s1_10$if_diff_5yrs <- "N"
data_7_s1_10$interval <- max(data_7_s1_10$terminal_year) - min(data_7_s1_10$terminal_year)
data_7_s1_10$ty <- 2012
data_7_s1_10$ty_minus1 <- data_7_s1_10$ty-1
data_7_s1_10$key_ty <- paste(data_7_s1_10$assessid, data_7_s1_10$ty, sep="_")
data_7_s1_10$key_ty_minus1 <- paste(data_7_s1_10$assessid, data_7_s1_10$ty_minus1, sep="_")
data_7_s1_10[,c(46:47,50:61,64:69)] <- NA

data_7_s1_11 <- data_7_s1[data_7_s1$terminal_year == 2012 | data_7_s1$terminal_year == 2017,]
data_7_s1_11$ty_order <- 11
data_7_s1_11$if_diff_5yrs <- "N"
data_7_s1_11$interval <- max(data_7_s1_11$terminal_year) - min(data_7_s1_11$terminal_year)
data_7_s1_11$ty <- 2012
data_7_s1_11$ty_minus1 <- data_7_s1_11$ty-1
data_7_s1_11$key_ty <- paste(data_7_s1_11$assessid, data_7_s1_11$ty, sep="_")
data_7_s1_11$key_ty_minus1 <- paste(data_7_s1_11$assessid, data_7_s1_11$ty_minus1, sep="_")
data_7_s1_11[,c(46:47,50:61,64:69)] <- NA

data_7_s1_12 <- data_7_s1[data_7_s1$terminal_year == 2013 | data_7_s1$terminal_year == 2014,]
data_7_s1_12$ty_order <- 12
data_7_s1_12$if_diff_5yrs <- "N"
data_7_s1_12$interval <- max(data_7_s1_12$terminal_year) - min(data_7_s1_12$terminal_year)
data_7_s1_12$ty <- 2013
data_7_s1_12$ty_minus1 <- data_7_s1_12$ty-1
data_7_s1_12$key_ty <- paste(data_7_s1_12$assessid, data_7_s1_12$ty, sep="_")
data_7_s1_12$key_ty_minus1 <- paste(data_7_s1_12$assessid, data_7_s1_12$ty_minus1, sep="_")
data_7_s1_12[,c(46:47,50:61,64:69)] <- NA

data_7_s1_13 <- data_7_s1[data_7_s1$terminal_year == 2013 | data_7_s1$terminal_year == 2015,]
data_7_s1_13$ty_order <- 13
data_7_s1_13$if_diff_5yrs <- "N"
data_7_s1_13$interval <- max(data_7_s1_13$terminal_year) - min(data_7_s1_13$terminal_year)
data_7_s1_13$ty <- 2013
data_7_s1_13$ty_minus1 <- data_7_s1_13$ty-1
data_7_s1_13$key_ty <- paste(data_7_s1_13$assessid, data_7_s1_13$ty, sep="_")
data_7_s1_13$key_ty_minus1 <- paste(data_7_s1_13$assessid, data_7_s1_13$ty_minus1, sep="_")
data_7_s1_13[,c(46:47,50:61,64:69)] <- NA

data_7_s1_14 <- data_7_s1[data_7_s1$terminal_year == 2013 | data_7_s1$terminal_year == 2016,]
data_7_s1_14$ty_order <- 14
data_7_s1_14$if_diff_5yrs <- "N"
data_7_s1_14$interval <- max(data_7_s1_14$terminal_year) - min(data_7_s1_14$terminal_year)
data_7_s1_14$ty <- 2013
data_7_s1_14$ty_minus1 <- data_7_s1_14$ty-1
data_7_s1_14$key_ty <- paste(data_7_s1_14$assessid, data_7_s1_14$ty, sep="_")
data_7_s1_14$key_ty_minus1 <- paste(data_7_s1_14$assessid, data_7_s1_14$ty_minus1, sep="_")
data_7_s1_14[,c(46:47,50:61,64:69)] <- NA

data_7_s1_15 <- data_7_s1[data_7_s1$terminal_year == 2013 | data_7_s1$terminal_year == 2017,]
data_7_s1_15$ty_order <- 15
data_7_s1_15$if_diff_5yrs <- "N"
data_7_s1_15$interval <- max(data_7_s1_15$terminal_year) - min(data_7_s1_15$terminal_year)
data_7_s1_15$ty <- 2013
data_7_s1_15$ty_minus1 <- data_7_s1_15$ty-1
data_7_s1_15$key_ty <- paste(data_7_s1_15$assessid, data_7_s1_15$ty, sep="_")
data_7_s1_15$key_ty_minus1 <- paste(data_7_s1_15$assessid, data_7_s1_15$ty_minus1, sep="_")
data_7_s1_15[,c(46:47,50:61,64:69)] <- NA

data_7_s1_16 <- data_7_s1[data_7_s1$terminal_year == 2014 | data_7_s1$terminal_year == 2015,]
data_7_s1_16$ty_order <- 16
data_7_s1_16$if_diff_5yrs <- "N"
data_7_s1_16$interval <- max(data_7_s1_16$terminal_year) - min(data_7_s1_16$terminal_year)
data_7_s1_16$ty <- 2014
data_7_s1_16$ty_minus1 <- data_7_s1_16$ty-1
data_7_s1_16$key_ty <- paste(data_7_s1_16$assessid, data_7_s1_16$ty, sep="_")
data_7_s1_16$key_ty_minus1 <- paste(data_7_s1_16$assessid, data_7_s1_16$ty_minus1, sep="_")
data_7_s1_16[,c(46:47,50:61,64:69)] <- NA

data_7_s1_17 <- data_7_s1[data_7_s1$terminal_year == 2014 | data_7_s1$terminal_year == 2016,]
data_7_s1_17$ty_order <- 17
data_7_s1_17$if_diff_5yrs <- "N"
data_7_s1_17$interval <- max(data_7_s1_17$terminal_year) - min(data_7_s1_17$terminal_year)
data_7_s1_17$ty <- 2014
data_7_s1_17$ty_minus1 <- data_7_s1_17$ty-1
data_7_s1_17$key_ty <- paste(data_7_s1_17$assessid, data_7_s1_17$ty, sep="_")
data_7_s1_17$key_ty_minus1 <- paste(data_7_s1_17$assessid, data_7_s1_17$ty_minus1, sep="_")
data_7_s1_17[,c(46:47,50:61,64:69)] <- NA

data_7_s1_18 <- data_7_s1[data_7_s1$terminal_year == 2014 | data_7_s1$terminal_year == 2017,]
data_7_s1_18$ty_order <- 18
data_7_s1_18$if_diff_5yrs <- "N"
data_7_s1_18$interval <- max(data_7_s1_18$terminal_year) - min(data_7_s1_18$terminal_year)
data_7_s1_18$ty <- 2014
data_7_s1_18$ty_minus1 <- data_7_s1_18$ty-1
data_7_s1_18$key_ty <- paste(data_7_s1_18$assessid, data_7_s1_18$ty, sep="_")
data_7_s1_18$key_ty_minus1 <- paste(data_7_s1_18$assessid, data_7_s1_18$ty_minus1, sep="_")
data_7_s1_18[,c(46:47,50:61,64:69)] <- NA

data_7_s1_19 <- data_7_s1[data_7_s1$terminal_year == 2015 | data_7_s1$terminal_year == 2016,]
data_7_s1_19$ty_order <- 19
data_7_s1_19$if_diff_5yrs <- "N"
data_7_s1_19$interval <- max(data_7_s1_19$terminal_year) - min(data_7_s1_19$terminal_year)
data_7_s1_19$ty <- 2015
data_7_s1_19$ty_minus1 <- data_7_s1_19$ty-1
data_7_s1_19$key_ty <- paste(data_7_s1_19$assessid, data_7_s1_19$ty, sep="_")
data_7_s1_19$key_ty_minus1 <- paste(data_7_s1_19$assessid, data_7_s1_19$ty_minus1, sep="_")
data_7_s1_19[,c(46:47,50:61,64:69)] <- NA

data_7_s1_20 <- data_7_s1[data_7_s1$terminal_year == 2015 | data_7_s1$terminal_year == 2017,]
data_7_s1_20$ty_order <- 20
data_7_s1_20$if_diff_5yrs <- "N"
data_7_s1_20$interval <- max(data_7_s1_20$terminal_year) - min(data_7_s1_20$terminal_year)
data_7_s1_20$ty <- 2015
data_7_s1_20$ty_minus1 <- data_7_s1_20$ty-1
data_7_s1_20$key_ty <- paste(data_7_s1_20$assessid, data_7_s1_20$ty, sep="_")
data_7_s1_20$key_ty_minus1 <- paste(data_7_s1_20$assessid, data_7_s1_20$ty_minus1, sep="_")
data_7_s1_20[,c(46:47,50:61,64:69)] <- NA

data_7_s1_21 <- data_7_s1[data_7_s1$terminal_year == 2016 | data_7_s1$terminal_year == 2017,]
data_7_s1_21$ty_order <- 21
data_7_s1_21$if_diff_5yrs <- "N"
data_7_s1_21$interval <- max(data_7_s1_21$terminal_year) - min(data_7_s1_21$terminal_year)
data_7_s1_21$ty <- 2016
data_7_s1_21$ty_minus1 <- data_7_s1_21$ty-1
data_7_s1_21$key_ty <- paste(data_7_s1_21$assessid, data_7_s1_21$ty, sep="_")
data_7_s1_21$key_ty_minus1 <- paste(data_7_s1_21$assessid, data_7_s1_21$ty_minus1, sep="_")
data_7_s1_21[,c(46:47,50:61,64:69)] <- NA

data_7_s1 <- rbind(data_7_s1_1, data_7_s1_2, data_7_s1_3, data_7_s1_4, data_7_s1_5, data_7_s1_6, data_7_s1_7, data_7_s1_8, data_7_s1_9, data_7_s1_10,
                   data_7_s1_11, data_7_s1_12, data_7_s1_13, data_7_s1_14, data_7_s1_15, data_7_s1_16, data_7_s1_17, data_7_s1_18, data_7_s1_19,
                   data_7_s1_20, data_7_s1_21)

# assess 2
data_7_s2 <- data_7_assess[data_7_assess$stockid==ids[2],]

data_7_s2_1 <- data_7_s2[data_7_s2$terminal_year == 2010 | data_7_s2$terminal_year == 2012,]
data_7_s2_1$ty_order <- 1
data_7_s2_1$if_diff_5yrs <- "N"
data_7_s2_1$interval <- max(data_7_s2_1$terminal_year) - min(data_7_s2_1$terminal_year)

data_7_s2_2 <- data_7_s2[data_7_s2$terminal_year == 2010 | data_7_s2$terminal_year == 2013,]
data_7_s2_2$ty_order <- 2
data_7_s2_2$if_diff_5yrs <- "N"
data_7_s2_2$interval <- max(data_7_s2_2$terminal_year) - min(data_7_s2_2$terminal_year)

data_7_s2_3 <- data_7_s2[data_7_s2$terminal_year == 2010 | data_7_s2$terminal_year == 2014,]
data_7_s2_3$ty_order <- 3
data_7_s2_3$if_diff_5yrs <- "N"
data_7_s2_3$interval <- max(data_7_s2_3$terminal_year) - min(data_7_s2_3$terminal_year)

data_7_s2_4 <- data_7_s2[data_7_s2$terminal_year == 2010 | data_7_s2$terminal_year == 2015,]
data_7_s2_4$ty_order <- 4
data_7_s2_4$if_diff_5yrs <- "N"
data_7_s2_4$interval <- max(data_7_s2_4$terminal_year) - min(data_7_s2_4$terminal_year)

data_7_s2_5 <- data_7_s2[data_7_s2$terminal_year == 2010 | data_7_s2$terminal_year == 2016,]
data_7_s2_5$ty_order <- 5
data_7_s2_5$if_diff_5yrs <- "Y"
data_7_s2_5$interval <- max(data_7_s2_5$terminal_year) - min(data_7_s2_5$terminal_year)

data_7_s2_6 <- data_7_s2[data_7_s2$terminal_year == 2010 | data_7_s2$terminal_year == 2017,]
data_7_s2_6$ty_order <- 6
data_7_s2_6$if_diff_5yrs <- "Y"
data_7_s2_6$interval <- max(data_7_s2_6$terminal_year) - min(data_7_s2_6$terminal_year)

data_7_s2_7 <- data_7_s2[data_7_s2$terminal_year == 2012 | data_7_s2$terminal_year == 2013,]
data_7_s2_7$ty_order <- 7
data_7_s2_7$if_diff_5yrs <- "N"
data_7_s2_7$interval <- max(data_7_s2_7$terminal_year) - min(data_7_s2_7$terminal_year)
data_7_s2_7$ty <- 2012
data_7_s2_7$ty_minus1 <- data_7_s2_7$ty-1
data_7_s2_7$key_ty <- paste(data_7_s2_7$assessid, data_7_s2_7$ty, sep="_")
data_7_s2_7$key_ty_minus1 <- paste(data_7_s2_7$assessid, data_7_s2_7$ty_minus1, sep="_")
data_7_s2_7[,c(46:47,50:61,64:69)] <- NA

data_7_s2_8 <- data_7_s2[data_7_s2$terminal_year == 2012 | data_7_s2$terminal_year == 2014,]
data_7_s2_8$ty_order <- 8
data_7_s2_8$if_diff_5yrs <- "N"
data_7_s2_8$interval <- max(data_7_s2_8$terminal_year) - min(data_7_s2_8$terminal_year)
data_7_s2_8$ty <- 2012
data_7_s2_8$ty_minus1 <- data_7_s2_8$ty-1
data_7_s2_8$key_ty <- paste(data_7_s2_8$assessid, data_7_s2_8$ty, sep="_")
data_7_s2_8$key_ty_minus1 <- paste(data_7_s2_8$assessid, data_7_s2_8$ty_minus1, sep="_")
data_7_s2_8[,c(46:47,50:61,64:69)] <- NA

data_7_s2_9 <- data_7_s2[data_7_s2$terminal_year == 2012 | data_7_s2$terminal_year == 2015,]
data_7_s2_9$ty_order <- 9
data_7_s2_9$if_diff_5yrs <- "N"
data_7_s2_9$interval <- max(data_7_s2_9$terminal_year) - min(data_7_s2_9$terminal_year)
data_7_s2_9$ty <- 2012
data_7_s2_9$ty_minus1 <- data_7_s2_9$ty-1
data_7_s2_9$key_ty <- paste(data_7_s2_9$assessid, data_7_s2_9$ty, sep="_")
data_7_s2_9$key_ty_minus1 <- paste(data_7_s2_9$assessid, data_7_s2_9$ty_minus1, sep="_")
data_7_s2_9[,c(46:47,50:61,64:69)] <- NA

data_7_s2_10 <- data_7_s2[data_7_s2$terminal_year == 2012 | data_7_s2$terminal_year == 2016,]
data_7_s2_10$ty_order <- 10
data_7_s2_10$if_diff_5yrs <- "N"
data_7_s2_10$interval <- max(data_7_s2_10$terminal_year) - min(data_7_s2_10$terminal_year)
data_7_s2_10$ty <- 2012
data_7_s2_10$ty_minus1 <- data_7_s2_10$ty-1
data_7_s2_10$key_ty <- paste(data_7_s2_10$assessid, data_7_s2_10$ty, sep="_")
data_7_s2_10$key_ty_minus1 <- paste(data_7_s2_10$assessid, data_7_s2_10$ty_minus1, sep="_")
data_7_s2_10[,c(46:47,50:61,64:69)] <- NA

data_7_s2_11 <- data_7_s2[data_7_s2$terminal_year == 2012 | data_7_s2$terminal_year == 2017,]
data_7_s2_11$ty_order <- 11
data_7_s2_11$if_diff_5yrs <- "N"
data_7_s2_11$interval <- max(data_7_s2_11$terminal_year) - min(data_7_s2_11$terminal_year)
data_7_s2_11$ty <- 2012
data_7_s2_11$ty_minus1 <- data_7_s2_11$ty-1
data_7_s2_11$key_ty <- paste(data_7_s2_11$assessid, data_7_s2_11$ty, sep="_")
data_7_s2_11$key_ty_minus1 <- paste(data_7_s2_11$assessid, data_7_s2_11$ty_minus1, sep="_")
data_7_s2_11[,c(46:47,50:61,64:69)] <- NA

data_7_s2_12 <- data_7_s2[data_7_s2$terminal_year == 2013 | data_7_s2$terminal_year == 2014,]
data_7_s2_12$ty_order <- 12
data_7_s2_12$if_diff_5yrs <- "N"
data_7_s2_12$interval <- max(data_7_s2_12$terminal_year) - min(data_7_s2_12$terminal_year)
data_7_s2_12$ty <- 2013
data_7_s2_12$ty_minus1 <- data_7_s2_12$ty-1
data_7_s2_12$key_ty <- paste(data_7_s2_12$assessid, data_7_s2_12$ty, sep="_")
data_7_s2_12$key_ty_minus1 <- paste(data_7_s2_12$assessid, data_7_s2_12$ty_minus1, sep="_")
data_7_s2_12[,c(46:47,50:61,64:69)] <- NA

data_7_s2_13 <- data_7_s2[data_7_s2$terminal_year == 2013 | data_7_s2$terminal_year == 2015,]
data_7_s2_13$ty_order <- 13
data_7_s2_13$if_diff_5yrs <- "N"
data_7_s2_13$interval <- max(data_7_s2_13$terminal_year) - min(data_7_s2_13$terminal_year)
data_7_s2_13$ty <- 2013
data_7_s2_13$ty_minus1 <- data_7_s2_13$ty-1
data_7_s2_13$key_ty <- paste(data_7_s2_13$assessid, data_7_s2_13$ty, sep="_")
data_7_s2_13$key_ty_minus1 <- paste(data_7_s2_13$assessid, data_7_s2_13$ty_minus1, sep="_")
data_7_s2_13[,c(46:47,50:61,64:69)] <- NA

data_7_s2_14 <- data_7_s2[data_7_s2$terminal_year == 2013 | data_7_s2$terminal_year == 2016,]
data_7_s2_14$ty_order <- 14
data_7_s2_14$if_diff_5yrs <- "N"
data_7_s2_14$interval <- max(data_7_s2_14$terminal_year) - min(data_7_s2_14$terminal_year)
data_7_s2_14$ty <- 2013
data_7_s2_14$ty_minus1 <- data_7_s2_14$ty-1
data_7_s2_14$key_ty <- paste(data_7_s2_14$assessid, data_7_s2_14$ty, sep="_")
data_7_s2_14$key_ty_minus1 <- paste(data_7_s2_14$assessid, data_7_s2_14$ty_minus1, sep="_")
data_7_s2_14[,c(46:47,50:61,64:69)] <- NA

data_7_s2_15 <- data_7_s2[data_7_s2$terminal_year == 2013 | data_7_s2$terminal_year == 2017,]
data_7_s2_15$ty_order <- 15
data_7_s2_15$if_diff_5yrs <- "N"
data_7_s2_15$interval <- max(data_7_s2_15$terminal_year) - min(data_7_s2_15$terminal_year)
data_7_s2_15$ty <- 2013
data_7_s2_15$ty_minus1 <- data_7_s2_15$ty-1
data_7_s2_15$key_ty <- paste(data_7_s2_15$assessid, data_7_s2_15$ty, sep="_")
data_7_s2_15$key_ty_minus1 <- paste(data_7_s2_15$assessid, data_7_s2_15$ty_minus1, sep="_")
data_7_s2_15[,c(46:47,50:61,64:69)] <- NA

data_7_s2_16 <- data_7_s2[data_7_s2$terminal_year == 2014 | data_7_s2$terminal_year == 2015,]
data_7_s2_16$ty_order <- 16
data_7_s2_16$if_diff_5yrs <- "N"
data_7_s2_16$interval <- max(data_7_s2_16$terminal_year) - min(data_7_s2_16$terminal_year)
data_7_s2_16$ty <- 2014
data_7_s2_16$ty_minus1 <- data_7_s2_16$ty-1
data_7_s2_16$key_ty <- paste(data_7_s2_16$assessid, data_7_s2_16$ty, sep="_")
data_7_s2_16$key_ty_minus1 <- paste(data_7_s2_16$assessid, data_7_s2_16$ty_minus1, sep="_")
data_7_s2_16[,c(46:47,50:61,64:69)] <- NA

data_7_s2_17 <- data_7_s2[data_7_s2$terminal_year == 2014 | data_7_s2$terminal_year == 2016,]
data_7_s2_17$ty_order <- 17
data_7_s2_17$if_diff_5yrs <- "N"
data_7_s2_17$interval <- max(data_7_s2_17$terminal_year) - min(data_7_s2_17$terminal_year)
data_7_s2_17$ty <- 2014
data_7_s2_17$ty_minus1 <- data_7_s2_17$ty-1
data_7_s2_17$key_ty <- paste(data_7_s2_17$assessid, data_7_s2_17$ty, sep="_")
data_7_s2_17$key_ty_minus1 <- paste(data_7_s2_17$assessid, data_7_s2_17$ty_minus1, sep="_")
data_7_s2_17[,c(46:47,50:61,64:69)] <- NA

data_7_s2_18 <- data_7_s2[data_7_s2$terminal_year == 2014 | data_7_s2$terminal_year == 2017,]
data_7_s2_18$ty_order <- 18
data_7_s2_18$if_diff_5yrs <- "N"
data_7_s2_18$interval <- max(data_7_s2_18$terminal_year) - min(data_7_s2_18$terminal_year)
data_7_s2_18$ty <- 2014
data_7_s2_18$ty_minus1 <- data_7_s2_18$ty-1
data_7_s2_18$key_ty <- paste(data_7_s2_18$assessid, data_7_s2_18$ty, sep="_")
data_7_s2_18$key_ty_minus1 <- paste(data_7_s2_18$assessid, data_7_s2_18$ty_minus1, sep="_")
data_7_s2_18[,c(46:47,50:61,64:69)] <- NA

data_7_s2_19 <- data_7_s2[data_7_s2$terminal_year == 2015 | data_7_s2$terminal_year == 2016,]
data_7_s2_19$ty_order <- 19
data_7_s2_19$if_diff_5yrs <- "N"
data_7_s2_19$interval <- max(data_7_s2_19$terminal_year) - min(data_7_s2_19$terminal_year)
data_7_s2_19$ty <- 2015
data_7_s2_19$ty_minus1 <- data_7_s2_19$ty-1
data_7_s2_19$key_ty <- paste(data_7_s2_19$assessid, data_7_s2_19$ty, sep="_")
data_7_s2_19$key_ty_minus1 <- paste(data_7_s2_19$assessid, data_7_s2_19$ty_minus1, sep="_")
data_7_s2_19[,c(46:47,50:61,64:69)] <- NA

data_7_s2_20 <- data_7_s2[data_7_s2$terminal_year == 2015 | data_7_s2$terminal_year == 2017,]
data_7_s2_20$ty_order <- 20
data_7_s2_20$if_diff_5yrs <- "N"
data_7_s2_20$interval <- max(data_7_s2_20$terminal_year) - min(data_7_s2_20$terminal_year)
data_7_s2_20$ty <- 2015
data_7_s2_20$ty_minus1 <- data_7_s2_20$ty-1
data_7_s2_20$key_ty <- paste(data_7_s2_20$assessid, data_7_s2_20$ty, sep="_")
data_7_s2_20$key_ty_minus1 <- paste(data_7_s2_20$assessid, data_7_s2_20$ty_minus1, sep="_")
data_7_s2_20[,c(46:47,50:61,64:69)] <- NA

data_7_s2_21 <- data_7_s2[data_7_s2$terminal_year == 2016 | data_7_s2$terminal_year == 2017,]
data_7_s2_21$ty_order <- 21
data_7_s2_21$if_diff_5yrs <- "N"
data_7_s2_21$interval <- max(data_7_s2_21$terminal_year) - min(data_7_s2_21$terminal_year)
data_7_s2_21$ty <- 2016
data_7_s2_21$ty_minus1 <- data_7_s2_21$ty-1
data_7_s2_21$key_ty <- paste(data_7_s2_21$assessid, data_7_s2_21$ty, sep="_")
data_7_s2_21$key_ty_minus1 <- paste(data_7_s2_21$assessid, data_7_s2_21$ty_minus1, sep="_")
data_7_s2_21[,c(46:47,50:61,64:69)] <- NA

data_7_s2 <- rbind(data_7_s2_1, data_7_s2_2, data_7_s2_3, data_7_s2_4, data_7_s2_5, data_7_s2_6, data_7_s2_7, data_7_s2_8, data_7_s2_9, data_7_s2_10,
                   data_7_s2_11, data_7_s2_12, data_7_s2_13, data_7_s2_14, data_7_s2_15, data_7_s2_16, data_7_s2_17, data_7_s2_18, data_7_s2_19,
                   data_7_s2_20, data_7_s2_21)

# assess 3
data_7_s3 <- data_7_assess[data_7_assess$stockid==ids[3],]

data_7_s3_1 <- data_7_s3[data_7_s3$terminal_year == 2010 | data_7_s3$terminal_year == 2012,]
data_7_s3_1$ty_order <- 1
data_7_s3_1$if_diff_5yrs <- "N"
data_7_s3_1$interval <- max(data_7_s3_1$terminal_year) - min(data_7_s3_1$terminal_year)

data_7_s3_2 <- data_7_s3[data_7_s3$terminal_year == 2010 | data_7_s3$terminal_year == 2013,]
data_7_s3_2$ty_order <- 2
data_7_s3_2$if_diff_5yrs <- "N"
data_7_s3_2$interval <- max(data_7_s3_2$terminal_year) - min(data_7_s3_2$terminal_year)

data_7_s3_3 <- data_7_s3[data_7_s3$terminal_year == 2010 | data_7_s3$terminal_year == 2014,]
data_7_s3_3$ty_order <- 3
data_7_s3_3$if_diff_5yrs <- "N"
data_7_s3_3$interval <- max(data_7_s3_3$terminal_year) - min(data_7_s3_3$terminal_year)

data_7_s3_4 <- data_7_s3[data_7_s3$terminal_year == 2010 | data_7_s3$terminal_year == 2015,]
data_7_s3_4$ty_order <- 4
data_7_s3_4$if_diff_5yrs <- "N"
data_7_s3_4$interval <- max(data_7_s3_4$terminal_year) - min(data_7_s3_4$terminal_year)

data_7_s3_5 <- data_7_s3[data_7_s3$terminal_year == 2010 | data_7_s3$terminal_year == 2016,]
data_7_s3_5$ty_order <- 5
data_7_s3_5$if_diff_5yrs <- "Y"
data_7_s3_5$interval <- max(data_7_s3_5$terminal_year) - min(data_7_s3_5$terminal_year)

data_7_s3_6 <- data_7_s3[data_7_s3$terminal_year == 2010 | data_7_s3$terminal_year == 2017,]
data_7_s3_6$ty_order <- 6
data_7_s3_6$if_diff_5yrs <- "Y"
data_7_s3_6$interval <- max(data_7_s3_6$terminal_year) - min(data_7_s3_6$terminal_year)

data_7_s3_7 <- data_7_s3[data_7_s3$terminal_year == 2012 | data_7_s3$terminal_year == 2013,]
data_7_s3_7$ty_order <- 7
data_7_s3_7$if_diff_5yrs <- "N"
data_7_s3_7$interval <- max(data_7_s3_7$terminal_year) - min(data_7_s3_7$terminal_year)
data_7_s3_7$ty <- 2012
data_7_s3_7$ty_minus1 <- data_7_s3_7$ty-1
data_7_s3_7$key_ty <- paste(data_7_s3_7$assessid, data_7_s3_7$ty, sep="_")
data_7_s3_7$key_ty_minus1 <- paste(data_7_s3_7$assessid, data_7_s3_7$ty_minus1, sep="_")
data_7_s3_7[,c(46:47,50:61,64:69)] <- NA

data_7_s3_8 <- data_7_s3[data_7_s3$terminal_year == 2012 | data_7_s3$terminal_year == 2014,]
data_7_s3_8$ty_order <- 8
data_7_s3_8$if_diff_5yrs <- "N"
data_7_s3_8$interval <- max(data_7_s3_8$terminal_year) - min(data_7_s3_8$terminal_year)
data_7_s3_8$ty <- 2012
data_7_s3_8$ty_minus1 <- data_7_s3_8$ty-1
data_7_s3_8$key_ty <- paste(data_7_s3_8$assessid, data_7_s3_8$ty, sep="_")
data_7_s3_8$key_ty_minus1 <- paste(data_7_s3_8$assessid, data_7_s3_8$ty_minus1, sep="_")
data_7_s3_8[,c(46:47,50:61,64:69)] <- NA

data_7_s3_9 <- data_7_s3[data_7_s3$terminal_year == 2012 | data_7_s3$terminal_year == 2015,]
data_7_s3_9$ty_order <- 9
data_7_s3_9$if_diff_5yrs <- "N"
data_7_s3_9$interval <- max(data_7_s3_9$terminal_year) - min(data_7_s3_9$terminal_year)
data_7_s3_9$ty <- 2012
data_7_s3_9$ty_minus1 <- data_7_s3_9$ty-1
data_7_s3_9$key_ty <- paste(data_7_s3_9$assessid, data_7_s3_9$ty, sep="_")
data_7_s3_9$key_ty_minus1 <- paste(data_7_s3_9$assessid, data_7_s3_9$ty_minus1, sep="_")
data_7_s3_9[,c(46:47,50:61,64:69)] <- NA

data_7_s3_10 <- data_7_s3[data_7_s3$terminal_year == 2012 | data_7_s3$terminal_year == 2016,]
data_7_s3_10$ty_order <- 10
data_7_s3_10$if_diff_5yrs <- "N"
data_7_s3_10$interval <- max(data_7_s3_10$terminal_year) - min(data_7_s3_10$terminal_year)
data_7_s3_10$ty <- 2012
data_7_s3_10$ty_minus1 <- data_7_s3_10$ty-1
data_7_s3_10$key_ty <- paste(data_7_s3_10$assessid, data_7_s3_10$ty, sep="_")
data_7_s3_10$key_ty_minus1 <- paste(data_7_s3_10$assessid, data_7_s3_10$ty_minus1, sep="_")
data_7_s3_10[,c(46:47,50:61,64:69)] <- NA

data_7_s3_11 <- data_7_s3[data_7_s3$terminal_year == 2012 | data_7_s3$terminal_year == 2017,]
data_7_s3_11$ty_order <- 11
data_7_s3_11$if_diff_5yrs <- "N"
data_7_s3_11$interval <- max(data_7_s3_11$terminal_year) - min(data_7_s3_11$terminal_year)
data_7_s3_11$ty <- 2012
data_7_s3_11$ty_minus1 <- data_7_s3_11$ty-1
data_7_s3_11$key_ty <- paste(data_7_s3_11$assessid, data_7_s3_11$ty, sep="_")
data_7_s3_11$key_ty_minus1 <- paste(data_7_s3_11$assessid, data_7_s3_11$ty_minus1, sep="_")
data_7_s3_11[,c(46:47,50:61,64:69)] <- NA

data_7_s3_12 <- data_7_s3[data_7_s3$terminal_year == 2013 | data_7_s3$terminal_year == 2014,]
data_7_s3_12$ty_order <- 12
data_7_s3_12$if_diff_5yrs <- "N"
data_7_s3_12$interval <- max(data_7_s3_12$terminal_year) - min(data_7_s3_12$terminal_year)
data_7_s3_12$ty <- 2013
data_7_s3_12$ty_minus1 <- data_7_s3_12$ty-1
data_7_s3_12$key_ty <- paste(data_7_s3_12$assessid, data_7_s3_12$ty, sep="_")
data_7_s3_12$key_ty_minus1 <- paste(data_7_s3_12$assessid, data_7_s3_12$ty_minus1, sep="_")
data_7_s3_12[,c(46:47,50:61,64:69)] <- NA

data_7_s3_13 <- data_7_s3[data_7_s3$terminal_year == 2013 | data_7_s3$terminal_year == 2015,]
data_7_s3_13$ty_order <- 13
data_7_s3_13$if_diff_5yrs <- "N"
data_7_s3_13$interval <- max(data_7_s3_13$terminal_year) - min(data_7_s3_13$terminal_year)
data_7_s3_13$ty <- 2013
data_7_s3_13$ty_minus1 <- data_7_s3_13$ty-1
data_7_s3_13$key_ty <- paste(data_7_s3_13$assessid, data_7_s3_13$ty, sep="_")
data_7_s3_13$key_ty_minus1 <- paste(data_7_s3_13$assessid, data_7_s3_13$ty_minus1, sep="_")
data_7_s3_13[,c(46:47,50:61,64:69)] <- NA

data_7_s3_14 <- data_7_s3[data_7_s3$terminal_year == 2013 | data_7_s3$terminal_year == 2016,]
data_7_s3_14$ty_order <- 14
data_7_s3_14$if_diff_5yrs <- "N"
data_7_s3_14$interval <- max(data_7_s3_14$terminal_year) - min(data_7_s3_14$terminal_year)
data_7_s3_14$ty <- 2013
data_7_s3_14$ty_minus1 <- data_7_s3_14$ty-1
data_7_s3_14$key_ty <- paste(data_7_s3_14$assessid, data_7_s3_14$ty, sep="_")
data_7_s3_14$key_ty_minus1 <- paste(data_7_s3_14$assessid, data_7_s3_14$ty_minus1, sep="_")
data_7_s3_14[,c(46:47,50:61,64:69)] <- NA

data_7_s3_15 <- data_7_s3[data_7_s3$terminal_year == 2013 | data_7_s3$terminal_year == 2017,]
data_7_s3_15$ty_order <- 15
data_7_s3_15$if_diff_5yrs <- "N"
data_7_s3_15$interval <- max(data_7_s3_15$terminal_year) - min(data_7_s3_15$terminal_year)
data_7_s3_15$ty <- 2013
data_7_s3_15$ty_minus1 <- data_7_s3_15$ty-1
data_7_s3_15$key_ty <- paste(data_7_s3_15$assessid, data_7_s3_15$ty, sep="_")
data_7_s3_15$key_ty_minus1 <- paste(data_7_s3_15$assessid, data_7_s3_15$ty_minus1, sep="_")
data_7_s3_15[,c(46:47,50:61,64:69)] <- NA

data_7_s3_16 <- data_7_s3[data_7_s3$terminal_year == 2014 | data_7_s3$terminal_year == 2015,]
data_7_s3_16$ty_order <- 16
data_7_s3_16$if_diff_5yrs <- "N"
data_7_s3_16$interval <- max(data_7_s3_16$terminal_year) - min(data_7_s3_16$terminal_year)
data_7_s3_16$ty <- 2014
data_7_s3_16$ty_minus1 <- data_7_s3_16$ty-1
data_7_s3_16$key_ty <- paste(data_7_s3_16$assessid, data_7_s3_16$ty, sep="_")
data_7_s3_16$key_ty_minus1 <- paste(data_7_s3_16$assessid, data_7_s3_16$ty_minus1, sep="_")
data_7_s3_16[,c(46:47,50:61,64:69)] <- NA

data_7_s3_17 <- data_7_s3[data_7_s3$terminal_year == 2014 | data_7_s3$terminal_year == 2016,]
data_7_s3_17$ty_order <- 17
data_7_s3_17$if_diff_5yrs <- "N"
data_7_s3_17$interval <- max(data_7_s3_17$terminal_year) - min(data_7_s3_17$terminal_year)
data_7_s3_17$ty <- 2014
data_7_s3_17$ty_minus1 <- data_7_s3_17$ty-1
data_7_s3_17$key_ty <- paste(data_7_s3_17$assessid, data_7_s3_17$ty, sep="_")
data_7_s3_17$key_ty_minus1 <- paste(data_7_s3_17$assessid, data_7_s3_17$ty_minus1, sep="_")
data_7_s3_17[,c(46:47,50:61,64:69)] <- NA

data_7_s3_18 <- data_7_s3[data_7_s3$terminal_year == 2014 | data_7_s3$terminal_year == 2017,]
data_7_s3_18$ty_order <- 18
data_7_s3_18$if_diff_5yrs <- "N"
data_7_s3_18$interval <- max(data_7_s3_18$terminal_year) - min(data_7_s3_18$terminal_year)
data_7_s3_18$ty <- 2014
data_7_s3_18$ty_minus1 <- data_7_s3_18$ty-1
data_7_s3_18$key_ty <- paste(data_7_s3_18$assessid, data_7_s3_18$ty, sep="_")
data_7_s3_18$key_ty_minus1 <- paste(data_7_s3_18$assessid, data_7_s3_18$ty_minus1, sep="_")
data_7_s3_18[,c(46:47,50:61,64:69)] <- NA

data_7_s3_19 <- data_7_s3[data_7_s3$terminal_year == 2015 | data_7_s3$terminal_year == 2016,]
data_7_s3_19$ty_order <- 19
data_7_s3_19$if_diff_5yrs <- "N"
data_7_s3_19$interval <- max(data_7_s3_19$terminal_year) - min(data_7_s3_19$terminal_year)
data_7_s3_19$ty <- 2015
data_7_s3_19$ty_minus1 <- data_7_s3_19$ty-1
data_7_s3_19$key_ty <- paste(data_7_s3_19$assessid, data_7_s3_19$ty, sep="_")
data_7_s3_19$key_ty_minus1 <- paste(data_7_s3_19$assessid, data_7_s3_19$ty_minus1, sep="_")
data_7_s3_19[,c(46:47,50:61,64:69)] <- NA

data_7_s3_20 <- data_7_s3[data_7_s3$terminal_year == 2015 | data_7_s3$terminal_year == 2017,]
data_7_s3_20$ty_order <- 20
data_7_s3_20$if_diff_5yrs <- "N"
data_7_s3_20$interval <- max(data_7_s3_20$terminal_year) - min(data_7_s3_20$terminal_year)
data_7_s3_20$ty <- 2015
data_7_s3_20$ty_minus1 <- data_7_s3_20$ty-1
data_7_s3_20$key_ty <- paste(data_7_s3_20$assessid, data_7_s3_20$ty, sep="_")
data_7_s3_20$key_ty_minus1 <- paste(data_7_s3_20$assessid, data_7_s3_20$ty_minus1, sep="_")
data_7_s3_20[,c(46:47,50:61,64:69)] <- NA

data_7_s3_21 <- data_7_s3[data_7_s3$terminal_year == 2016 | data_7_s3$terminal_year == 2017,]
data_7_s3_21$ty_order <- 21
data_7_s3_21$if_diff_5yrs <- "N"
data_7_s3_21$interval <- max(data_7_s3_21$terminal_year) - min(data_7_s3_21$terminal_year)
data_7_s3_21$ty <- 2016
data_7_s3_21$ty_minus1 <- data_7_s3_21$ty-1
data_7_s3_21$key_ty <- paste(data_7_s3_21$assessid, data_7_s3_21$ty, sep="_")
data_7_s3_21$key_ty_minus1 <- paste(data_7_s3_21$assessid, data_7_s3_21$ty_minus1, sep="_")
data_7_s3_21[,c(46:47,50:61,64:69)] <- NA

data_7_s3 <- rbind(data_7_s3_1, data_7_s3_2, data_7_s3_3, data_7_s3_4, data_7_s3_5, data_7_s3_6, data_7_s3_7, data_7_s3_8, data_7_s3_9, data_7_s3_10,
                   data_7_s3_11, data_7_s3_12, data_7_s3_13, data_7_s3_14, data_7_s3_15, data_7_s3_16, data_7_s3_17, data_7_s3_18, data_7_s3_19,
                   data_7_s3_20, data_7_s3_21)

# assess 4
data_7_s4 <- data_7_assess[data_7_assess$stockid==ids[4],]

data_7_s4_1 <- data_7_s4[data_7_s4$terminal_year == 2010 | data_7_s4$terminal_year == 2012,]
data_7_s4_1$ty_order <- 1
data_7_s4_1$if_diff_5yrs <- "N"
data_7_s4_1$interval <- max(data_7_s4_1$terminal_year) - min(data_7_s4_1$terminal_year)

data_7_s4_2 <- data_7_s4[data_7_s4$terminal_year == 2010 | data_7_s4$terminal_year == 2013,]
data_7_s4_2$ty_order <- 2
data_7_s4_2$if_diff_5yrs <- "N"
data_7_s4_2$interval <- max(data_7_s4_2$terminal_year) - min(data_7_s4_2$terminal_year)

data_7_s4_3 <- data_7_s4[data_7_s4$terminal_year == 2010 | data_7_s4$terminal_year == 2014,]
data_7_s4_3$ty_order <- 3
data_7_s4_3$if_diff_5yrs <- "N"
data_7_s4_3$interval <- max(data_7_s4_3$terminal_year) - min(data_7_s4_3$terminal_year)

data_7_s4_4 <- data_7_s4[data_7_s4$terminal_year == 2010 | data_7_s4$terminal_year == 2015,]
data_7_s4_4$ty_order <- 4
data_7_s4_4$if_diff_5yrs <- "N"
data_7_s4_4$interval <- max(data_7_s4_4$terminal_year) - min(data_7_s4_4$terminal_year)

data_7_s4_5 <- data_7_s4[data_7_s4$terminal_year == 2010 | data_7_s4$terminal_year == 2016,]
data_7_s4_5$ty_order <- 5
data_7_s4_5$if_diff_5yrs <- "Y"
data_7_s4_5$interval <- max(data_7_s4_5$terminal_year) - min(data_7_s4_5$terminal_year)

data_7_s4_6 <- data_7_s4[data_7_s4$terminal_year == 2010 | data_7_s4$terminal_year == 2017,]
data_7_s4_6$ty_order <- 6
data_7_s4_6$if_diff_5yrs <- "Y"
data_7_s4_6$interval <- max(data_7_s4_6$terminal_year) - min(data_7_s4_6$terminal_year)

data_7_s4_7 <- data_7_s4[data_7_s4$terminal_year == 2012 | data_7_s4$terminal_year == 2013,]
data_7_s4_7$ty_order <- 7
data_7_s4_7$if_diff_5yrs <- "N"
data_7_s4_7$interval <- max(data_7_s4_7$terminal_year) - min(data_7_s4_7$terminal_year)
data_7_s4_7$ty <- 2012
data_7_s4_7$ty_minus1 <- data_7_s4_7$ty-1
data_7_s4_7$key_ty <- paste(data_7_s4_7$assessid, data_7_s4_7$ty, sep="_")
data_7_s4_7$key_ty_minus1 <- paste(data_7_s4_7$assessid, data_7_s4_7$ty_minus1, sep="_")
data_7_s4_7[,c(46:47,50:61,64:69)] <- NA

data_7_s4_8 <- data_7_s4[data_7_s4$terminal_year == 2012 | data_7_s4$terminal_year == 2014,]
data_7_s4_8$ty_order <- 8
data_7_s4_8$if_diff_5yrs <- "N"
data_7_s4_8$interval <- max(data_7_s4_8$terminal_year) - min(data_7_s4_8$terminal_year)
data_7_s4_8$ty <- 2012
data_7_s4_8$ty_minus1 <- data_7_s4_8$ty-1
data_7_s4_8$key_ty <- paste(data_7_s4_8$assessid, data_7_s4_8$ty, sep="_")
data_7_s4_8$key_ty_minus1 <- paste(data_7_s4_8$assessid, data_7_s4_8$ty_minus1, sep="_")
data_7_s4_8[,c(46:47,50:61,64:69)] <- NA

data_7_s4_9 <- data_7_s4[data_7_s4$terminal_year == 2012 | data_7_s4$terminal_year == 2015,]
data_7_s4_9$ty_order <- 9
data_7_s4_9$if_diff_5yrs <- "N"
data_7_s4_9$interval <- max(data_7_s4_9$terminal_year) - min(data_7_s4_9$terminal_year)
data_7_s4_9$ty <- 2012
data_7_s4_9$ty_minus1 <- data_7_s4_9$ty-1
data_7_s4_9$key_ty <- paste(data_7_s4_9$assessid, data_7_s4_9$ty, sep="_")
data_7_s4_9$key_ty_minus1 <- paste(data_7_s4_9$assessid, data_7_s4_9$ty_minus1, sep="_")
data_7_s4_9[,c(46:47,50:61,64:69)] <- NA

data_7_s4_10 <- data_7_s4[data_7_s4$terminal_year == 2012 | data_7_s4$terminal_year == 2016,]
data_7_s4_10$ty_order <- 10
data_7_s4_10$if_diff_5yrs <- "N"
data_7_s4_10$interval <- max(data_7_s4_10$terminal_year) - min(data_7_s4_10$terminal_year)
data_7_s4_10$ty <- 2012
data_7_s4_10$ty_minus1 <- data_7_s4_10$ty-1
data_7_s4_10$key_ty <- paste(data_7_s4_10$assessid, data_7_s4_10$ty, sep="_")
data_7_s4_10$key_ty_minus1 <- paste(data_7_s4_10$assessid, data_7_s4_10$ty_minus1, sep="_")
data_7_s4_10[,c(46:47,50:61,64:69)] <- NA

data_7_s4_11 <- data_7_s4[data_7_s4$terminal_year == 2012 | data_7_s4$terminal_year == 2017,]
data_7_s4_11$ty_order <- 11
data_7_s4_11$if_diff_5yrs <- "N"
data_7_s4_11$interval <- max(data_7_s4_11$terminal_year) - min(data_7_s4_11$terminal_year)
data_7_s4_11$ty <- 2012
data_7_s4_11$ty_minus1 <- data_7_s4_11$ty-1
data_7_s4_11$key_ty <- paste(data_7_s4_11$assessid, data_7_s4_11$ty, sep="_")
data_7_s4_11$key_ty_minus1 <- paste(data_7_s4_11$assessid, data_7_s4_11$ty_minus1, sep="_")
data_7_s4_11[,c(46:47,50:61,64:69)] <- NA

data_7_s4_12 <- data_7_s4[data_7_s4$terminal_year == 2013 | data_7_s4$terminal_year == 2014,]
data_7_s4_12$ty_order <- 12
data_7_s4_12$if_diff_5yrs <- "N"
data_7_s4_12$interval <- max(data_7_s4_12$terminal_year) - min(data_7_s4_12$terminal_year)
data_7_s4_12$ty <- 2013
data_7_s4_12$ty_minus1 <- data_7_s4_12$ty-1
data_7_s4_12$key_ty <- paste(data_7_s4_12$assessid, data_7_s4_12$ty, sep="_")
data_7_s4_12$key_ty_minus1 <- paste(data_7_s4_12$assessid, data_7_s4_12$ty_minus1, sep="_")
data_7_s4_12[,c(46:47,50:61,64:69)] <- NA

data_7_s4_13 <- data_7_s4[data_7_s4$terminal_year == 2013 | data_7_s4$terminal_year == 2015,]
data_7_s4_13$ty_order <- 13
data_7_s4_13$if_diff_5yrs <- "N"
data_7_s4_13$interval <- max(data_7_s4_13$terminal_year) - min(data_7_s4_13$terminal_year)
data_7_s4_13$ty <- 2013
data_7_s4_13$ty_minus1 <- data_7_s4_13$ty-1
data_7_s4_13$key_ty <- paste(data_7_s4_13$assessid, data_7_s4_13$ty, sep="_")
data_7_s4_13$key_ty_minus1 <- paste(data_7_s4_13$assessid, data_7_s4_13$ty_minus1, sep="_")
data_7_s4_13[,c(46:47,50:61,64:69)] <- NA

data_7_s4_14 <- data_7_s4[data_7_s4$terminal_year == 2013 | data_7_s4$terminal_year == 2016,]
data_7_s4_14$ty_order <- 14
data_7_s4_14$if_diff_5yrs <- "N"
data_7_s4_14$interval <- max(data_7_s4_14$terminal_year) - min(data_7_s4_14$terminal_year)
data_7_s4_14$ty <- 2013
data_7_s4_14$ty_minus1 <- data_7_s4_14$ty-1
data_7_s4_14$key_ty <- paste(data_7_s4_14$assessid, data_7_s4_14$ty, sep="_")
data_7_s4_14$key_ty_minus1 <- paste(data_7_s4_14$assessid, data_7_s4_14$ty_minus1, sep="_")
data_7_s4_14[,c(46:47,50:61,64:69)] <- NA

data_7_s4_15 <- data_7_s4[data_7_s4$terminal_year == 2013 | data_7_s4$terminal_year == 2017,]
data_7_s4_15$ty_order <- 15
data_7_s4_15$if_diff_5yrs <- "N"
data_7_s4_15$interval <- max(data_7_s4_15$terminal_year) - min(data_7_s4_15$terminal_year)
data_7_s4_15$ty <- 2013
data_7_s4_15$ty_minus1 <- data_7_s4_15$ty-1
data_7_s4_15$key_ty <- paste(data_7_s4_15$assessid, data_7_s4_15$ty, sep="_")
data_7_s4_15$key_ty_minus1 <- paste(data_7_s4_15$assessid, data_7_s4_15$ty_minus1, sep="_")
data_7_s4_15[,c(46:47,50:61,64:69)] <- NA

data_7_s4_16 <- data_7_s4[data_7_s4$terminal_year == 2014 | data_7_s4$terminal_year == 2015,]
data_7_s4_16$ty_order <- 16
data_7_s4_16$if_diff_5yrs <- "N"
data_7_s4_16$interval <- max(data_7_s4_16$terminal_year) - min(data_7_s4_16$terminal_year)
data_7_s4_16$ty <- 2014
data_7_s4_16$ty_minus1 <- data_7_s4_16$ty-1
data_7_s4_16$key_ty <- paste(data_7_s4_16$assessid, data_7_s4_16$ty, sep="_")
data_7_s4_16$key_ty_minus1 <- paste(data_7_s4_16$assessid, data_7_s4_16$ty_minus1, sep="_")
data_7_s4_16[,c(46:47,50:61,64:69)] <- NA

data_7_s4_17 <- data_7_s4[data_7_s4$terminal_year == 2014 | data_7_s4$terminal_year == 2016,]
data_7_s4_17$ty_order <- 17
data_7_s4_17$if_diff_5yrs <- "N"
data_7_s4_17$interval <- max(data_7_s4_17$terminal_year) - min(data_7_s4_17$terminal_year)
data_7_s4_17$ty <- 2014
data_7_s4_17$ty_minus1 <- data_7_s4_17$ty-1
data_7_s4_17$key_ty <- paste(data_7_s4_17$assessid, data_7_s4_17$ty, sep="_")
data_7_s4_17$key_ty_minus1 <- paste(data_7_s4_17$assessid, data_7_s4_17$ty_minus1, sep="_")
data_7_s4_17[,c(46:47,50:61,64:69)] <- NA

data_7_s4_18 <- data_7_s4[data_7_s4$terminal_year == 2014 | data_7_s4$terminal_year == 2017,]
data_7_s4_18$ty_order <- 18
data_7_s4_18$if_diff_5yrs <- "N"
data_7_s4_18$interval <- max(data_7_s4_18$terminal_year) - min(data_7_s4_18$terminal_year)
data_7_s4_18$ty <- 2014
data_7_s4_18$ty_minus1 <- data_7_s4_18$ty-1
data_7_s4_18$key_ty <- paste(data_7_s4_18$assessid, data_7_s4_18$ty, sep="_")
data_7_s4_18$key_ty_minus1 <- paste(data_7_s4_18$assessid, data_7_s4_18$ty_minus1, sep="_")
data_7_s4_18[,c(46:47,50:61,64:69)] <- NA

data_7_s4_19 <- data_7_s4[data_7_s4$terminal_year == 2015 | data_7_s4$terminal_year == 2016,]
data_7_s4_19$ty_order <- 19
data_7_s4_19$if_diff_5yrs <- "N"
data_7_s4_19$interval <- max(data_7_s4_19$terminal_year) - min(data_7_s4_19$terminal_year)
data_7_s4_19$ty <- 2015
data_7_s4_19$ty_minus1 <- data_7_s4_19$ty-1
data_7_s4_19$key_ty <- paste(data_7_s4_19$assessid, data_7_s4_19$ty, sep="_")
data_7_s4_19$key_ty_minus1 <- paste(data_7_s4_19$assessid, data_7_s4_19$ty_minus1, sep="_")
data_7_s4_19[,c(46:47,50:61,64:69)] <- NA

data_7_s4_20 <- data_7_s4[data_7_s4$terminal_year == 2015 | data_7_s4$terminal_year == 2017,]
data_7_s4_20$ty_order <- 20
data_7_s4_20$if_diff_5yrs <- "N"
data_7_s4_20$interval <- max(data_7_s4_20$terminal_year) - min(data_7_s4_20$terminal_year)
data_7_s4_20$ty <- 2015
data_7_s4_20$ty_minus1 <- data_7_s4_20$ty-1
data_7_s4_20$key_ty <- paste(data_7_s4_20$assessid, data_7_s4_20$ty, sep="_")
data_7_s4_20$key_ty_minus1 <- paste(data_7_s4_20$assessid, data_7_s4_20$ty_minus1, sep="_")
data_7_s4_20[,c(46:47,50:61,64:69)] <- NA

data_7_s4_21 <- data_7_s4[data_7_s4$terminal_year == 2016 | data_7_s4$terminal_year == 2017,]
data_7_s4_21$ty_order <- 21
data_7_s4_21$if_diff_5yrs <- "N"
data_7_s4_21$interval <- max(data_7_s4_21$terminal_year) - min(data_7_s4_21$terminal_year)
data_7_s4_21$ty <- 2016
data_7_s4_21$ty_minus1 <- data_7_s4_21$ty-1
data_7_s4_21$key_ty <- paste(data_7_s4_21$assessid, data_7_s4_21$ty, sep="_")
data_7_s4_21$key_ty_minus1 <- paste(data_7_s4_21$assessid, data_7_s4_21$ty_minus1, sep="_")
data_7_s4_21[,c(46:47,50:61,64:69)] <- NA

data_7_s4 <- rbind(data_7_s4_1, data_7_s4_2, data_7_s4_3, data_7_s4_4, data_7_s4_5, data_7_s4_6, data_7_s4_7, data_7_s4_8, data_7_s4_9, data_7_s4_10,
                   data_7_s4_11, data_7_s4_12, data_7_s4_13, data_7_s4_14, data_7_s4_15, data_7_s4_16, data_7_s4_17, data_7_s4_18, data_7_s4_19,
                   data_7_s4_20, data_7_s4_21)

# assess 5
data_7_s5 <- data_7_assess[data_7_assess$stockid==ids[5],]

data_7_s5_1 <- data_7_s5[data_7_s5$terminal_year == 2010 | data_7_s5$terminal_year == 2012,]
data_7_s5_1$ty_order <- 1
data_7_s5_1$if_diff_5yrs <- "N"
data_7_s5_1$interval <- max(data_7_s5_1$terminal_year) - min(data_7_s5_1$terminal_year)

data_7_s5_2 <- data_7_s5[data_7_s5$terminal_year == 2010 | data_7_s5$terminal_year == 2013,]
data_7_s5_2$ty_order <- 2
data_7_s5_2$if_diff_5yrs <- "N"
data_7_s5_2$interval <- max(data_7_s5_2$terminal_year) - min(data_7_s5_2$terminal_year)

data_7_s5_3 <- data_7_s5[data_7_s5$terminal_year == 2010 | data_7_s5$terminal_year == 2014,]
data_7_s5_3$ty_order <- 3
data_7_s5_3$if_diff_5yrs <- "N"
data_7_s5_3$interval <- max(data_7_s5_3$terminal_year) - min(data_7_s5_3$terminal_year)

data_7_s5_4 <- data_7_s5[data_7_s5$terminal_year == 2010 | data_7_s5$terminal_year == 2015,]
data_7_s5_4$ty_order <- 4
data_7_s5_4$if_diff_5yrs <- "N"
data_7_s5_4$interval <- max(data_7_s5_4$terminal_year) - min(data_7_s5_4$terminal_year)

data_7_s5_5 <- data_7_s5[data_7_s5$terminal_year == 2010 | data_7_s5$terminal_year == 2016,]
data_7_s5_5$ty_order <- 5
data_7_s5_5$if_diff_5yrs <- "Y"
data_7_s5_5$interval <- max(data_7_s5_5$terminal_year) - min(data_7_s5_5$terminal_year)

data_7_s5_6 <- data_7_s5[data_7_s5$terminal_year == 2010 | data_7_s5$terminal_year == 2017,]
data_7_s5_6$ty_order <- 6
data_7_s5_6$if_diff_5yrs <- "Y"
data_7_s5_6$interval <- max(data_7_s5_6$terminal_year) - min(data_7_s5_6$terminal_year)

data_7_s5_7 <- data_7_s5[data_7_s5$terminal_year == 2012 | data_7_s5$terminal_year == 2013,]
data_7_s5_7$ty_order <- 7
data_7_s5_7$if_diff_5yrs <- "N"
data_7_s5_7$interval <- max(data_7_s5_7$terminal_year) - min(data_7_s5_7$terminal_year)
data_7_s5_7$ty <- 2012
data_7_s5_7$ty_minus1 <- data_7_s5_7$ty-1
data_7_s5_7$key_ty <- paste(data_7_s5_7$assessid, data_7_s5_7$ty, sep="_")
data_7_s5_7$key_ty_minus1 <- paste(data_7_s5_7$assessid, data_7_s5_7$ty_minus1, sep="_")
data_7_s5_7[,c(46:47,50:61,64:69)] <- NA

data_7_s5_8 <- data_7_s5[data_7_s5$terminal_year == 2012 | data_7_s5$terminal_year == 2014,]
data_7_s5_8$ty_order <- 8
data_7_s5_8$if_diff_5yrs <- "N"
data_7_s5_8$interval <- max(data_7_s5_8$terminal_year) - min(data_7_s5_8$terminal_year)
data_7_s5_8$ty <- 2012
data_7_s5_8$ty_minus1 <- data_7_s5_8$ty-1
data_7_s5_8$key_ty <- paste(data_7_s5_8$assessid, data_7_s5_8$ty, sep="_")
data_7_s5_8$key_ty_minus1 <- paste(data_7_s5_8$assessid, data_7_s5_8$ty_minus1, sep="_")
data_7_s5_8[,c(46:47,50:61,64:69)] <- NA

data_7_s5_9 <- data_7_s5[data_7_s5$terminal_year == 2012 | data_7_s5$terminal_year == 2015,]
data_7_s5_9$ty_order <- 9
data_7_s5_9$if_diff_5yrs <- "N"
data_7_s5_9$interval <- max(data_7_s5_9$terminal_year) - min(data_7_s5_9$terminal_year)
data_7_s5_9$ty <- 2012
data_7_s5_9$ty_minus1 <- data_7_s5_9$ty-1
data_7_s5_9$key_ty <- paste(data_7_s5_9$assessid, data_7_s5_9$ty, sep="_")
data_7_s5_9$key_ty_minus1 <- paste(data_7_s5_9$assessid, data_7_s5_9$ty_minus1, sep="_")
data_7_s5_9[,c(46:47,50:61,64:69)] <- NA

data_7_s5_10 <- data_7_s5[data_7_s5$terminal_year == 2012 | data_7_s5$terminal_year == 2016,]
data_7_s5_10$ty_order <- 10
data_7_s5_10$if_diff_5yrs <- "N"
data_7_s5_10$interval <- max(data_7_s5_10$terminal_year) - min(data_7_s5_10$terminal_year)
data_7_s5_10$ty <- 2012
data_7_s5_10$ty_minus1 <- data_7_s5_10$ty-1
data_7_s5_10$key_ty <- paste(data_7_s5_10$assessid, data_7_s5_10$ty, sep="_")
data_7_s5_10$key_ty_minus1 <- paste(data_7_s5_10$assessid, data_7_s5_10$ty_minus1, sep="_")
data_7_s5_10[,c(46:47,50:61,64:69)] <- NA

data_7_s5_11 <- data_7_s5[data_7_s5$terminal_year == 2012 | data_7_s5$terminal_year == 2017,]
data_7_s5_11$ty_order <- 11
data_7_s5_11$if_diff_5yrs <- "N"
data_7_s5_11$interval <- max(data_7_s5_11$terminal_year) - min(data_7_s5_11$terminal_year)
data_7_s5_11$ty <- 2012
data_7_s5_11$ty_minus1 <- data_7_s5_11$ty-1
data_7_s5_11$key_ty <- paste(data_7_s5_11$assessid, data_7_s5_11$ty, sep="_")
data_7_s5_11$key_ty_minus1 <- paste(data_7_s5_11$assessid, data_7_s5_11$ty_minus1, sep="_")
data_7_s5_11[,c(46:47,50:61,64:69)] <- NA

data_7_s5_12 <- data_7_s5[data_7_s5$terminal_year == 2013 | data_7_s5$terminal_year == 2014,]
data_7_s5_12$ty_order <- 12
data_7_s5_12$if_diff_5yrs <- "N"
data_7_s5_12$interval <- max(data_7_s5_12$terminal_year) - min(data_7_s5_12$terminal_year)
data_7_s5_12$ty <- 2013
data_7_s5_12$ty_minus1 <- data_7_s5_12$ty-1
data_7_s5_12$key_ty <- paste(data_7_s5_12$assessid, data_7_s5_12$ty, sep="_")
data_7_s5_12$key_ty_minus1 <- paste(data_7_s5_12$assessid, data_7_s5_12$ty_minus1, sep="_")
data_7_s5_12[,c(46:47,50:61,64:69)] <- NA

data_7_s5_13 <- data_7_s5[data_7_s5$terminal_year == 2013 | data_7_s5$terminal_year == 2015,]
data_7_s5_13$ty_order <- 13
data_7_s5_13$if_diff_5yrs <- "N"
data_7_s5_13$interval <- max(data_7_s5_13$terminal_year) - min(data_7_s5_13$terminal_year)
data_7_s5_13$ty <- 2013
data_7_s5_13$ty_minus1 <- data_7_s5_13$ty-1
data_7_s5_13$key_ty <- paste(data_7_s5_13$assessid, data_7_s5_13$ty, sep="_")
data_7_s5_13$key_ty_minus1 <- paste(data_7_s5_13$assessid, data_7_s5_13$ty_minus1, sep="_")
data_7_s5_13[,c(46:47,50:61,64:69)] <- NA

data_7_s5_14 <- data_7_s5[data_7_s5$terminal_year == 2013 | data_7_s5$terminal_year == 2016,]
data_7_s5_14$ty_order <- 14
data_7_s5_14$if_diff_5yrs <- "N"
data_7_s5_14$interval <- max(data_7_s5_14$terminal_year) - min(data_7_s5_14$terminal_year)
data_7_s5_14$ty <- 2013
data_7_s5_14$ty_minus1 <- data_7_s5_14$ty-1
data_7_s5_14$key_ty <- paste(data_7_s5_14$assessid, data_7_s5_14$ty, sep="_")
data_7_s5_14$key_ty_minus1 <- paste(data_7_s5_14$assessid, data_7_s5_14$ty_minus1, sep="_")
data_7_s5_14[,c(46:47,50:61,64:69)] <- NA

data_7_s5_15 <- data_7_s5[data_7_s5$terminal_year == 2013 | data_7_s5$terminal_year == 2017,]
data_7_s5_15$ty_order <- 15
data_7_s5_15$if_diff_5yrs <- "N"
data_7_s5_15$interval <- max(data_7_s5_15$terminal_year) - min(data_7_s5_15$terminal_year)
data_7_s5_15$ty <- 2013
data_7_s5_15$ty_minus1 <- data_7_s5_15$ty-1
data_7_s5_15$key_ty <- paste(data_7_s5_15$assessid, data_7_s5_15$ty, sep="_")
data_7_s5_15$key_ty_minus1 <- paste(data_7_s5_15$assessid, data_7_s5_15$ty_minus1, sep="_")
data_7_s5_15[,c(46:47,50:61,64:69)] <- NA

data_7_s5_16 <- data_7_s5[data_7_s5$terminal_year == 2014 | data_7_s5$terminal_year == 2015,]
data_7_s5_16$ty_order <- 16
data_7_s5_16$if_diff_5yrs <- "N"
data_7_s5_16$interval <- max(data_7_s5_16$terminal_year) - min(data_7_s5_16$terminal_year)
data_7_s5_16$ty <- 2014
data_7_s5_16$ty_minus1 <- data_7_s5_16$ty-1
data_7_s5_16$key_ty <- paste(data_7_s5_16$assessid, data_7_s5_16$ty, sep="_")
data_7_s5_16$key_ty_minus1 <- paste(data_7_s5_16$assessid, data_7_s5_16$ty_minus1, sep="_")
data_7_s5_16[,c(46:47,50:61,64:69)] <- NA

data_7_s5_17 <- data_7_s5[data_7_s5$terminal_year == 2014 | data_7_s5$terminal_year == 2016,]
data_7_s5_17$ty_order <- 17
data_7_s5_17$if_diff_5yrs <- "N"
data_7_s5_17$interval <- max(data_7_s5_17$terminal_year) - min(data_7_s5_17$terminal_year)
data_7_s5_17$ty <- 2014
data_7_s5_17$ty_minus1 <- data_7_s5_17$ty-1
data_7_s5_17$key_ty <- paste(data_7_s5_17$assessid, data_7_s5_17$ty, sep="_")
data_7_s5_17$key_ty_minus1 <- paste(data_7_s5_17$assessid, data_7_s5_17$ty_minus1, sep="_")
data_7_s5_17[,c(46:47,50:61,64:69)] <- NA

data_7_s5_18 <- data_7_s5[data_7_s5$terminal_year == 2014 | data_7_s5$terminal_year == 2017,]
data_7_s5_18$ty_order <- 18
data_7_s5_18$if_diff_5yrs <- "N"
data_7_s5_18$interval <- max(data_7_s5_18$terminal_year) - min(data_7_s5_18$terminal_year)
data_7_s5_18$ty <- 2014
data_7_s5_18$ty_minus1 <- data_7_s5_18$ty-1
data_7_s5_18$key_ty <- paste(data_7_s5_18$assessid, data_7_s5_18$ty, sep="_")
data_7_s5_18$key_ty_minus1 <- paste(data_7_s5_18$assessid, data_7_s5_18$ty_minus1, sep="_")
data_7_s5_18[,c(46:47,50:61,64:69)] <- NA

data_7_s5_19 <- data_7_s5[data_7_s5$terminal_year == 2015 | data_7_s5$terminal_year == 2016,]
data_7_s5_19$ty_order <- 19
data_7_s5_19$if_diff_5yrs <- "N"
data_7_s5_19$interval <- max(data_7_s5_19$terminal_year) - min(data_7_s5_19$terminal_year)
data_7_s5_19$ty <- 2015
data_7_s5_19$ty_minus1 <- data_7_s5_19$ty-1
data_7_s5_19$key_ty <- paste(data_7_s5_19$assessid, data_7_s5_19$ty, sep="_")
data_7_s5_19$key_ty_minus1 <- paste(data_7_s5_19$assessid, data_7_s5_19$ty_minus1, sep="_")
data_7_s5_19[,c(46:47,50:61,64:69)] <- NA

data_7_s5_20 <- data_7_s5[data_7_s5$terminal_year == 2015 | data_7_s5$terminal_year == 2017,]
data_7_s5_20$ty_order <- 20
data_7_s5_20$if_diff_5yrs <- "N"
data_7_s5_20$interval <- max(data_7_s5_20$terminal_year) - min(data_7_s5_20$terminal_year)
data_7_s5_20$ty <- 2015
data_7_s5_20$ty_minus1 <- data_7_s5_20$ty-1
data_7_s5_20$key_ty <- paste(data_7_s5_20$assessid, data_7_s5_20$ty, sep="_")
data_7_s5_20$key_ty_minus1 <- paste(data_7_s5_20$assessid, data_7_s5_20$ty_minus1, sep="_")
data_7_s5_20[,c(46:47,50:61,64:69)] <- NA

data_7_s5_21 <- data_7_s5[data_7_s5$terminal_year == 2016 | data_7_s5$terminal_year == 2017,]
data_7_s5_21$ty_order <- 21
data_7_s5_21$if_diff_5yrs <- "N"
data_7_s5_21$interval <- max(data_7_s5_21$terminal_year) - min(data_7_s5_21$terminal_year)
data_7_s5_21$ty <- 2016
data_7_s5_21$ty_minus1 <- data_7_s5_21$ty-1
data_7_s5_21$key_ty <- paste(data_7_s5_21$assessid, data_7_s5_21$ty, sep="_")
data_7_s5_21$key_ty_minus1 <- paste(data_7_s5_21$assessid, data_7_s5_21$ty_minus1, sep="_")
data_7_s5_21[,c(46:47,50:61,64:69)] <- NA

data_7_s5 <- rbind(data_7_s5_1, data_7_s5_2, data_7_s5_3, data_7_s5_4, data_7_s5_5, data_7_s5_6, data_7_s5_7, data_7_s5_8, data_7_s5_9, data_7_s5_10,
                   data_7_s5_11, data_7_s5_12, data_7_s5_13, data_7_s5_14, data_7_s5_15, data_7_s5_16, data_7_s5_17, data_7_s5_18, data_7_s5_19,
                   data_7_s5_20, data_7_s5_21)
# assess 6
data_7_s6 <- data_7_assess[data_7_assess$stockid==ids[6],]

data_7_s6_1 <- data_7_s6[data_7_s6$terminal_year == 2010 | data_7_s6$terminal_year == 2012,]
data_7_s6_1$ty_order <- 1
data_7_s6_1$if_diff_5yrs <- "N"
data_7_s6_1$interval <- max(data_7_s6_1$terminal_year) - min(data_7_s6_1$terminal_year)

data_7_s6_2 <- data_7_s6[data_7_s6$terminal_year == 2010 | data_7_s6$terminal_year == 2013,]
data_7_s6_2$ty_order <- 2
data_7_s6_2$if_diff_5yrs <- "N"
data_7_s6_2$interval <- max(data_7_s6_2$terminal_year) - min(data_7_s6_2$terminal_year)

data_7_s6_3 <- data_7_s6[data_7_s6$terminal_year == 2010 | data_7_s6$terminal_year == 2014,]
data_7_s6_3$ty_order <- 3
data_7_s6_3$if_diff_5yrs <- "N"
data_7_s6_3$interval <- max(data_7_s6_3$terminal_year) - min(data_7_s6_3$terminal_year)

data_7_s6_4 <- data_7_s6[data_7_s6$terminal_year == 2010 | data_7_s6$terminal_year == 2015,]
data_7_s6_4$ty_order <- 4
data_7_s6_4$if_diff_5yrs <- "N"
data_7_s6_4$interval <- max(data_7_s6_4$terminal_year) - min(data_7_s6_4$terminal_year)

data_7_s6_5 <- data_7_s6[data_7_s6$terminal_year == 2010 | data_7_s6$terminal_year == 2016,]
data_7_s6_5$ty_order <- 5
data_7_s6_5$if_diff_5yrs <- "Y"
data_7_s6_5$interval <- max(data_7_s6_5$terminal_year) - min(data_7_s6_5$terminal_year)

data_7_s6_6 <- data_7_s6[data_7_s6$terminal_year == 2010 | data_7_s6$terminal_year == 2017,]
data_7_s6_6$ty_order <- 6
data_7_s6_6$if_diff_5yrs <- "Y"
data_7_s6_6$interval <- max(data_7_s6_6$terminal_year) - min(data_7_s6_6$terminal_year)

data_7_s6_7 <- data_7_s6[data_7_s6$terminal_year == 2012 | data_7_s6$terminal_year == 2013,]
data_7_s6_7$ty_order <- 7
data_7_s6_7$if_diff_5yrs <- "N"
data_7_s6_7$interval <- max(data_7_s6_7$terminal_year) - min(data_7_s6_7$terminal_year)
data_7_s6_7$ty <- 2012
data_7_s6_7$ty_minus1 <- data_7_s6_7$ty-1
data_7_s6_7$key_ty <- paste(data_7_s6_7$assessid, data_7_s6_7$ty, sep="_")
data_7_s6_7$key_ty_minus1 <- paste(data_7_s6_7$assessid, data_7_s6_7$ty_minus1, sep="_")
data_7_s6_7[,c(46:47,50:61,64:69)] <- NA

data_7_s6_8 <- data_7_s6[data_7_s6$terminal_year == 2012 | data_7_s6$terminal_year == 2014,]
data_7_s6_8$ty_order <- 8
data_7_s6_8$if_diff_5yrs <- "N"
data_7_s6_8$interval <- max(data_7_s6_8$terminal_year) - min(data_7_s6_8$terminal_year)
data_7_s6_8$ty <- 2012
data_7_s6_8$ty_minus1 <- data_7_s6_8$ty-1
data_7_s6_8$key_ty <- paste(data_7_s6_8$assessid, data_7_s6_8$ty, sep="_")
data_7_s6_8$key_ty_minus1 <- paste(data_7_s6_8$assessid, data_7_s6_8$ty_minus1, sep="_")
data_7_s6_8[,c(46:47,50:61,64:69)] <- NA

data_7_s6_9 <- data_7_s6[data_7_s6$terminal_year == 2012 | data_7_s6$terminal_year == 2015,]
data_7_s6_9$ty_order <- 9
data_7_s6_9$if_diff_5yrs <- "N"
data_7_s6_9$interval <- max(data_7_s6_9$terminal_year) - min(data_7_s6_9$terminal_year)
data_7_s6_9$ty <- 2012
data_7_s6_9$ty_minus1 <- data_7_s6_9$ty-1
data_7_s6_9$key_ty <- paste(data_7_s6_9$assessid, data_7_s6_9$ty, sep="_")
data_7_s6_9$key_ty_minus1 <- paste(data_7_s6_9$assessid, data_7_s6_9$ty_minus1, sep="_")
data_7_s6_9[,c(46:47,50:61,64:69)] <- NA

data_7_s6_10 <- data_7_s6[data_7_s6$terminal_year == 2012 | data_7_s6$terminal_year == 2016,]
data_7_s6_10$ty_order <- 10
data_7_s6_10$if_diff_5yrs <- "N"
data_7_s6_10$interval <- max(data_7_s6_10$terminal_year) - min(data_7_s6_10$terminal_year)
data_7_s6_10$ty <- 2012
data_7_s6_10$ty_minus1 <- data_7_s6_10$ty-1
data_7_s6_10$key_ty <- paste(data_7_s6_10$assessid, data_7_s6_10$ty, sep="_")
data_7_s6_10$key_ty_minus1 <- paste(data_7_s6_10$assessid, data_7_s6_10$ty_minus1, sep="_")
data_7_s6_10[,c(46:47,50:61,64:69)] <- NA

data_7_s6_11 <- data_7_s6[data_7_s6$terminal_year == 2012 | data_7_s6$terminal_year == 2017,]
data_7_s6_11$ty_order <- 11
data_7_s6_11$if_diff_5yrs <- "N"
data_7_s6_11$interval <- max(data_7_s6_11$terminal_year) - min(data_7_s6_11$terminal_year)
data_7_s6_11$ty <- 2012
data_7_s6_11$ty_minus1 <- data_7_s6_11$ty-1
data_7_s6_11$key_ty <- paste(data_7_s6_11$assessid, data_7_s6_11$ty, sep="_")
data_7_s6_11$key_ty_minus1 <- paste(data_7_s6_11$assessid, data_7_s6_11$ty_minus1, sep="_")
data_7_s6_11[,c(46:47,50:61,64:69)] <- NA

data_7_s6_12 <- data_7_s6[data_7_s6$terminal_year == 2013 | data_7_s6$terminal_year == 2014,]
data_7_s6_12$ty_order <- 12
data_7_s6_12$if_diff_5yrs <- "N"
data_7_s6_12$interval <- max(data_7_s6_12$terminal_year) - min(data_7_s6_12$terminal_year)
data_7_s6_12$ty <- 2013
data_7_s6_12$ty_minus1 <- data_7_s6_12$ty-1
data_7_s6_12$key_ty <- paste(data_7_s6_12$assessid, data_7_s6_12$ty, sep="_")
data_7_s6_12$key_ty_minus1 <- paste(data_7_s6_12$assessid, data_7_s6_12$ty_minus1, sep="_")
data_7_s6_12[,c(46:47,50:61,64:69)] <- NA

data_7_s6_13 <- data_7_s6[data_7_s6$terminal_year == 2013 | data_7_s6$terminal_year == 2015,]
data_7_s6_13$ty_order <- 13
data_7_s6_13$if_diff_5yrs <- "N"
data_7_s6_13$interval <- max(data_7_s6_13$terminal_year) - min(data_7_s6_13$terminal_year)
data_7_s6_13$ty <- 2013
data_7_s6_13$ty_minus1 <- data_7_s6_13$ty-1
data_7_s6_13$key_ty <- paste(data_7_s6_13$assessid, data_7_s6_13$ty, sep="_")
data_7_s6_13$key_ty_minus1 <- paste(data_7_s6_13$assessid, data_7_s6_13$ty_minus1, sep="_")
data_7_s6_13[,c(46:47,50:61,64:69)] <- NA

data_7_s6_14 <- data_7_s6[data_7_s6$terminal_year == 2013 | data_7_s6$terminal_year == 2016,]
data_7_s6_14$ty_order <- 14
data_7_s6_14$if_diff_5yrs <- "N"
data_7_s6_14$interval <- max(data_7_s6_14$terminal_year) - min(data_7_s6_14$terminal_year)
data_7_s6_14$ty <- 2013
data_7_s6_14$ty_minus1 <- data_7_s6_14$ty-1
data_7_s6_14$key_ty <- paste(data_7_s6_14$assessid, data_7_s6_14$ty, sep="_")
data_7_s6_14$key_ty_minus1 <- paste(data_7_s6_14$assessid, data_7_s6_14$ty_minus1, sep="_")
data_7_s6_14[,c(46:47,50:61,64:69)] <- NA

data_7_s6_15 <- data_7_s6[data_7_s6$terminal_year == 2013 | data_7_s6$terminal_year == 2017,]
data_7_s6_15$ty_order <- 15
data_7_s6_15$if_diff_5yrs <- "N"
data_7_s6_15$interval <- max(data_7_s6_15$terminal_year) - min(data_7_s6_15$terminal_year)
data_7_s6_15$ty <- 2013
data_7_s6_15$ty_minus1 <- data_7_s6_15$ty-1
data_7_s6_15$key_ty <- paste(data_7_s6_15$assessid, data_7_s6_15$ty, sep="_")
data_7_s6_15$key_ty_minus1 <- paste(data_7_s6_15$assessid, data_7_s6_15$ty_minus1, sep="_")
data_7_s6_15[,c(46:47,50:61,64:69)] <- NA

data_7_s6_16 <- data_7_s6[data_7_s6$terminal_year == 2014 | data_7_s6$terminal_year == 2015,]
data_7_s6_16$ty_order <- 16
data_7_s6_16$if_diff_5yrs <- "N"
data_7_s6_16$interval <- max(data_7_s6_16$terminal_year) - min(data_7_s6_16$terminal_year)
data_7_s6_16$ty <- 2014
data_7_s6_16$ty_minus1 <- data_7_s6_16$ty-1
data_7_s6_16$key_ty <- paste(data_7_s6_16$assessid, data_7_s6_16$ty, sep="_")
data_7_s6_16$key_ty_minus1 <- paste(data_7_s6_16$assessid, data_7_s6_16$ty_minus1, sep="_")
data_7_s6_16[,c(46:47,50:61,64:69)] <- NA

data_7_s6_17 <- data_7_s6[data_7_s6$terminal_year == 2014 | data_7_s6$terminal_year == 2016,]
data_7_s6_17$ty_order <- 17
data_7_s6_17$if_diff_5yrs <- "N"
data_7_s6_17$interval <- max(data_7_s6_17$terminal_year) - min(data_7_s6_17$terminal_year)
data_7_s6_17$ty <- 2014
data_7_s6_17$ty_minus1 <- data_7_s6_17$ty-1
data_7_s6_17$key_ty <- paste(data_7_s6_17$assessid, data_7_s6_17$ty, sep="_")
data_7_s6_17$key_ty_minus1 <- paste(data_7_s6_17$assessid, data_7_s6_17$ty_minus1, sep="_")
data_7_s6_17[,c(46:47,50:61,64:69)] <- NA

data_7_s6_18 <- data_7_s6[data_7_s6$terminal_year == 2014 | data_7_s6$terminal_year == 2017,]
data_7_s6_18$ty_order <- 18
data_7_s6_18$if_diff_5yrs <- "N"
data_7_s6_18$interval <- max(data_7_s6_18$terminal_year) - min(data_7_s6_18$terminal_year)
data_7_s6_18$ty <- 2014
data_7_s6_18$ty_minus1 <- data_7_s6_18$ty-1
data_7_s6_18$key_ty <- paste(data_7_s6_18$assessid, data_7_s6_18$ty, sep="_")
data_7_s6_18$key_ty_minus1 <- paste(data_7_s6_18$assessid, data_7_s6_18$ty_minus1, sep="_")
data_7_s6_18[,c(46:47,50:61,64:69)] <- NA

data_7_s6_19 <- data_7_s6[data_7_s6$terminal_year == 2015 | data_7_s6$terminal_year == 2016,]
data_7_s6_19$ty_order <- 19
data_7_s6_19$if_diff_5yrs <- "N"
data_7_s6_19$interval <- max(data_7_s6_19$terminal_year) - min(data_7_s6_19$terminal_year)
data_7_s6_19$ty <- 2015
data_7_s6_19$ty_minus1 <- data_7_s6_19$ty-1
data_7_s6_19$key_ty <- paste(data_7_s6_19$assessid, data_7_s6_19$ty, sep="_")
data_7_s6_19$key_ty_minus1 <- paste(data_7_s6_19$assessid, data_7_s6_19$ty_minus1, sep="_")
data_7_s6_19[,c(46:47,50:61,64:69)] <- NA

data_7_s6_20 <- data_7_s6[data_7_s6$terminal_year == 2015 | data_7_s6$terminal_year == 2017,]
data_7_s6_20$ty_order <- 20
data_7_s6_20$if_diff_5yrs <- "N"
data_7_s6_20$interval <- max(data_7_s6_20$terminal_year) - min(data_7_s6_20$terminal_year)
data_7_s6_20$ty <- 2015
data_7_s6_20$ty_minus1 <- data_7_s6_20$ty-1
data_7_s6_20$key_ty <- paste(data_7_s6_20$assessid, data_7_s6_20$ty, sep="_")
data_7_s6_20$key_ty_minus1 <- paste(data_7_s6_20$assessid, data_7_s6_20$ty_minus1, sep="_")
data_7_s6_20[,c(46:47,50:61,64:69)] <- NA

data_7_s6_21 <- data_7_s6[data_7_s6$terminal_year == 2016 | data_7_s6$terminal_year == 2017,]
data_7_s6_21$ty_order <- 21
data_7_s6_21$if_diff_5yrs <- "N"
data_7_s6_21$interval <- max(data_7_s6_21$terminal_year) - min(data_7_s6_21$terminal_year)
data_7_s6_21$ty <- 2016
data_7_s6_21$ty_minus1 <- data_7_s6_21$ty-1
data_7_s6_21$key_ty <- paste(data_7_s6_21$assessid, data_7_s6_21$ty, sep="_")
data_7_s6_21$key_ty_minus1 <- paste(data_7_s6_21$assessid, data_7_s6_21$ty_minus1, sep="_")
data_7_s6_21[,c(46:47,50:61,64:69)] <- NA

data_7_s6 <- rbind(data_7_s6_1, data_7_s6_2, data_7_s6_3, data_7_s6_4, data_7_s6_5, data_7_s6_6, data_7_s6_7, data_7_s6_8, data_7_s6_9, data_7_s6_10,
                   data_7_s6_11, data_7_s6_12, data_7_s6_13, data_7_s6_14, data_7_s6_15, data_7_s6_16, data_7_s6_17, data_7_s6_18, data_7_s6_19,
                   data_7_s6_20, data_7_s6_21)

# assess 7
data_7_s7 <- data_7_assess[data_7_assess$stockid==ids[7],]

data_7_s7_1 <- data_7_s7[data_7_s7$terminal_year == 2010 | data_7_s7$terminal_year == 2012,]
data_7_s7_1$ty_order <- 1
data_7_s7_1$if_diff_5yrs <- "N"
data_7_s7_1$interval <- max(data_7_s7_1$terminal_year) - min(data_7_s7_1$terminal_year)

data_7_s7_2 <- data_7_s7[data_7_s7$terminal_year == 2010 | data_7_s7$terminal_year == 2013,]
data_7_s7_2$ty_order <- 2
data_7_s7_2$if_diff_5yrs <- "N"
data_7_s7_2$interval <- max(data_7_s7_2$terminal_year) - min(data_7_s7_2$terminal_year)

data_7_s7_3 <- data_7_s7[data_7_s7$terminal_year == 2010 | data_7_s7$terminal_year == 2014,]
data_7_s7_3$ty_order <- 3
data_7_s7_3$if_diff_5yrs <- "N"
data_7_s7_3$interval <- max(data_7_s7_3$terminal_year) - min(data_7_s7_3$terminal_year)

data_7_s7_4 <- data_7_s7[data_7_s7$terminal_year == 2010 | data_7_s7$terminal_year == 2015,]
data_7_s7_4$ty_order <- 4
data_7_s7_4$if_diff_5yrs <- "N"
data_7_s7_4$interval <- max(data_7_s7_4$terminal_year) - min(data_7_s7_4$terminal_year)

data_7_s7_5 <- data_7_s7[data_7_s7$terminal_year == 2010 | data_7_s7$terminal_year == 2016,]
data_7_s7_5$ty_order <- 5
data_7_s7_5$if_diff_5yrs <- "Y"
data_7_s7_5$interval <- max(data_7_s7_5$terminal_year) - min(data_7_s7_5$terminal_year)

data_7_s7_6 <- data_7_s7[data_7_s7$terminal_year == 2010 | data_7_s7$terminal_year == 2017,]
data_7_s7_6$ty_order <- 6
data_7_s7_6$if_diff_5yrs <- "Y"
data_7_s7_6$interval <- max(data_7_s7_6$terminal_year) - min(data_7_s7_6$terminal_year)

data_7_s7_7 <- data_7_s7[data_7_s7$terminal_year == 2012 | data_7_s7$terminal_year == 2013,]
data_7_s7_7$ty_order <- 7
data_7_s7_7$if_diff_5yrs <- "N"
data_7_s7_7$interval <- max(data_7_s7_7$terminal_year) - min(data_7_s7_7$terminal_year)
data_7_s7_7$ty <- 2012
data_7_s7_7$ty_minus1 <- data_7_s7_7$ty-1
data_7_s7_7$key_ty <- paste(data_7_s7_7$assessid, data_7_s7_7$ty, sep="_")
data_7_s7_7$key_ty_minus1 <- paste(data_7_s7_7$assessid, data_7_s7_7$ty_minus1, sep="_")
data_7_s7_7[,c(46:47,50:61,64:69)] <- NA

data_7_s7_8 <- data_7_s7[data_7_s7$terminal_year == 2012 | data_7_s7$terminal_year == 2014,]
data_7_s7_8$ty_order <- 8
data_7_s7_8$if_diff_5yrs <- "N"
data_7_s7_8$interval <- max(data_7_s7_8$terminal_year) - min(data_7_s7_8$terminal_year)
data_7_s7_8$ty <- 2012
data_7_s7_8$ty_minus1 <- data_7_s7_8$ty-1
data_7_s7_8$key_ty <- paste(data_7_s7_8$assessid, data_7_s7_8$ty, sep="_")
data_7_s7_8$key_ty_minus1 <- paste(data_7_s7_8$assessid, data_7_s7_8$ty_minus1, sep="_")
data_7_s7_8[,c(46:47,50:61,64:69)] <- NA

data_7_s7_9 <- data_7_s7[data_7_s7$terminal_year == 2012 | data_7_s7$terminal_year == 2015,]
data_7_s7_9$ty_order <- 9
data_7_s7_9$if_diff_5yrs <- "N"
data_7_s7_9$interval <- max(data_7_s7_9$terminal_year) - min(data_7_s7_9$terminal_year)
data_7_s7_9$ty <- 2012
data_7_s7_9$ty_minus1 <- data_7_s7_9$ty-1
data_7_s7_9$key_ty <- paste(data_7_s7_9$assessid, data_7_s7_9$ty, sep="_")
data_7_s7_9$key_ty_minus1 <- paste(data_7_s7_9$assessid, data_7_s7_9$ty_minus1, sep="_")
data_7_s7_9[,c(46:47,50:61,64:69)] <- NA

data_7_s7_10 <- data_7_s7[data_7_s7$terminal_year == 2012 | data_7_s7$terminal_year == 2016,]
data_7_s7_10$ty_order <- 10
data_7_s7_10$if_diff_5yrs <- "N"
data_7_s7_10$interval <- max(data_7_s7_10$terminal_year) - min(data_7_s7_10$terminal_year)
data_7_s7_10$ty <- 2012
data_7_s7_10$ty_minus1 <- data_7_s7_10$ty-1
data_7_s7_10$key_ty <- paste(data_7_s7_10$assessid, data_7_s7_10$ty, sep="_")
data_7_s7_10$key_ty_minus1 <- paste(data_7_s7_10$assessid, data_7_s7_10$ty_minus1, sep="_")
data_7_s7_10[,c(46:47,50:61,64:69)] <- NA

data_7_s7_11 <- data_7_s7[data_7_s7$terminal_year == 2012 | data_7_s7$terminal_year == 2017,]
data_7_s7_11$ty_order <- 11
data_7_s7_11$if_diff_5yrs <- "N"
data_7_s7_11$interval <- max(data_7_s7_11$terminal_year) - min(data_7_s7_11$terminal_year)
data_7_s7_11$ty <- 2012
data_7_s7_11$ty_minus1 <- data_7_s7_11$ty-1
data_7_s7_11$key_ty <- paste(data_7_s7_11$assessid, data_7_s7_11$ty, sep="_")
data_7_s7_11$key_ty_minus1 <- paste(data_7_s7_11$assessid, data_7_s7_11$ty_minus1, sep="_")
data_7_s7_11[,c(46:47,50:61,64:69)] <- NA

data_7_s7_12 <- data_7_s7[data_7_s7$terminal_year == 2013 | data_7_s7$terminal_year == 2014,]
data_7_s7_12$ty_order <- 12
data_7_s7_12$if_diff_5yrs <- "N"
data_7_s7_12$interval <- max(data_7_s7_12$terminal_year) - min(data_7_s7_12$terminal_year)
data_7_s7_12$ty <- 2013
data_7_s7_12$ty_minus1 <- data_7_s7_12$ty-1
data_7_s7_12$key_ty <- paste(data_7_s7_12$assessid, data_7_s7_12$ty, sep="_")
data_7_s7_12$key_ty_minus1 <- paste(data_7_s7_12$assessid, data_7_s7_12$ty_minus1, sep="_")
data_7_s7_12[,c(46:47,50:61,64:69)] <- NA

data_7_s7_13 <- data_7_s7[data_7_s7$terminal_year == 2013 | data_7_s7$terminal_year == 2015,]
data_7_s7_13$ty_order <- 13
data_7_s7_13$if_diff_5yrs <- "N"
data_7_s7_13$interval <- max(data_7_s7_13$terminal_year) - min(data_7_s7_13$terminal_year)
data_7_s7_13$ty <- 2013
data_7_s7_13$ty_minus1 <- data_7_s7_13$ty-1
data_7_s7_13$key_ty <- paste(data_7_s7_13$assessid, data_7_s7_13$ty, sep="_")
data_7_s7_13$key_ty_minus1 <- paste(data_7_s7_13$assessid, data_7_s7_13$ty_minus1, sep="_")
data_7_s7_13[,c(46:47,50:61,64:69)] <- NA

data_7_s7_14 <- data_7_s7[data_7_s7$terminal_year == 2013 | data_7_s7$terminal_year == 2016,]
data_7_s7_14$ty_order <- 14
data_7_s7_14$if_diff_5yrs <- "N"
data_7_s7_14$interval <- max(data_7_s7_14$terminal_year) - min(data_7_s7_14$terminal_year)
data_7_s7_14$ty <- 2013
data_7_s7_14$ty_minus1 <- data_7_s7_14$ty-1
data_7_s7_14$key_ty <- paste(data_7_s7_14$assessid, data_7_s7_14$ty, sep="_")
data_7_s7_14$key_ty_minus1 <- paste(data_7_s7_14$assessid, data_7_s7_14$ty_minus1, sep="_")
data_7_s7_14[,c(46:47,50:61,64:69)] <- NA

data_7_s7_15 <- data_7_s7[data_7_s7$terminal_year == 2013 | data_7_s7$terminal_year == 2017,]
data_7_s7_15$ty_order <- 15
data_7_s7_15$if_diff_5yrs <- "N"
data_7_s7_15$interval <- max(data_7_s7_15$terminal_year) - min(data_7_s7_15$terminal_year)
data_7_s7_15$ty <- 2013
data_7_s7_15$ty_minus1 <- data_7_s7_15$ty-1
data_7_s7_15$key_ty <- paste(data_7_s7_15$assessid, data_7_s7_15$ty, sep="_")
data_7_s7_15$key_ty_minus1 <- paste(data_7_s7_15$assessid, data_7_s7_15$ty_minus1, sep="_")
data_7_s7_15[,c(46:47,50:61,64:69)] <- NA

data_7_s7_16 <- data_7_s7[data_7_s7$terminal_year == 2014 | data_7_s7$terminal_year == 2015,]
data_7_s7_16$ty_order <- 16
data_7_s7_16$if_diff_5yrs <- "N"
data_7_s7_16$interval <- max(data_7_s7_16$terminal_year) - min(data_7_s7_16$terminal_year)
data_7_s7_16$ty <- 2014
data_7_s7_16$ty_minus1 <- data_7_s7_16$ty-1
data_7_s7_16$key_ty <- paste(data_7_s7_16$assessid, data_7_s7_16$ty, sep="_")
data_7_s7_16$key_ty_minus1 <- paste(data_7_s7_16$assessid, data_7_s7_16$ty_minus1, sep="_")
data_7_s7_16[,c(46:47,50:61,64:69)] <- NA

data_7_s7_17 <- data_7_s7[data_7_s7$terminal_year == 2014 | data_7_s7$terminal_year == 2016,]
data_7_s7_17$ty_order <- 17
data_7_s7_17$if_diff_5yrs <- "N"
data_7_s7_17$interval <- max(data_7_s7_17$terminal_year) - min(data_7_s7_17$terminal_year)
data_7_s7_17$ty <- 2014
data_7_s7_17$ty_minus1 <- data_7_s7_17$ty-1
data_7_s7_17$key_ty <- paste(data_7_s7_17$assessid, data_7_s7_17$ty, sep="_")
data_7_s7_17$key_ty_minus1 <- paste(data_7_s7_17$assessid, data_7_s7_17$ty_minus1, sep="_")
data_7_s7_17[,c(46:47,50:61,64:69)] <- NA

data_7_s7_18 <- data_7_s7[data_7_s7$terminal_year == 2014 | data_7_s7$terminal_year == 2017,]
data_7_s7_18$ty_order <- 18
data_7_s7_18$if_diff_5yrs <- "N"
data_7_s7_18$interval <- max(data_7_s7_18$terminal_year) - min(data_7_s7_18$terminal_year)
data_7_s7_18$ty <- 2014
data_7_s7_18$ty_minus1 <- data_7_s7_18$ty-1
data_7_s7_18$key_ty <- paste(data_7_s7_18$assessid, data_7_s7_18$ty, sep="_")
data_7_s7_18$key_ty_minus1 <- paste(data_7_s7_18$assessid, data_7_s7_18$ty_minus1, sep="_")
data_7_s7_18[,c(46:47,50:61,64:69)] <- NA

data_7_s7_19 <- data_7_s7[data_7_s7$terminal_year == 2015 | data_7_s7$terminal_year == 2016,]
data_7_s7_19$ty_order <- 19
data_7_s7_19$if_diff_5yrs <- "N"
data_7_s7_19$interval <- max(data_7_s7_19$terminal_year) - min(data_7_s7_19$terminal_year)
data_7_s7_19$ty <- 2015
data_7_s7_19$ty_minus1 <- data_7_s7_19$ty-1
data_7_s7_19$key_ty <- paste(data_7_s7_19$assessid, data_7_s7_19$ty, sep="_")
data_7_s7_19$key_ty_minus1 <- paste(data_7_s7_19$assessid, data_7_s7_19$ty_minus1, sep="_")
data_7_s7_19[,c(46:47,50:61,64:69)] <- NA

data_7_s7_20 <- data_7_s7[data_7_s7$terminal_year == 2015 | data_7_s7$terminal_year == 2017,]
data_7_s7_20$ty_order <- 20
data_7_s7_20$if_diff_5yrs <- "N"
data_7_s7_20$interval <- max(data_7_s7_20$terminal_year) - min(data_7_s7_20$terminal_year)
data_7_s7_20$ty <- 2015
data_7_s7_20$ty_minus1 <- data_7_s7_20$ty-1
data_7_s7_20$key_ty <- paste(data_7_s7_20$assessid, data_7_s7_20$ty, sep="_")
data_7_s7_20$key_ty_minus1 <- paste(data_7_s7_20$assessid, data_7_s7_20$ty_minus1, sep="_")
data_7_s7_20[,c(46:47,50:61,64:69)] <- NA

data_7_s7_21 <- data_7_s7[data_7_s7$terminal_year == 2016 | data_7_s7$terminal_year == 2017,]
data_7_s7_21$ty_order <- 21
data_7_s7_21$if_diff_5yrs <- "N"
data_7_s7_21$interval <- max(data_7_s7_21$terminal_year) - min(data_7_s7_21$terminal_year)
data_7_s7_21$ty <- 2016
data_7_s7_21$ty_minus1 <- data_7_s7_21$ty-1
data_7_s7_21$key_ty <- paste(data_7_s7_21$assessid, data_7_s7_21$ty, sep="_")
data_7_s7_21$key_ty_minus1 <- paste(data_7_s7_21$assessid, data_7_s7_21$ty_minus1, sep="_")
data_7_s7_21[,c(46:47,50:61,64:69)] <- NA

data_7_s7 <- rbind(data_7_s7_1, data_7_s7_2, data_7_s7_3, data_7_s7_4, data_7_s7_5, data_7_s7_6, data_7_s7_7, data_7_s7_8, data_7_s7_9, data_7_s7_10,
                   data_7_s7_11, data_7_s7_12, data_7_s7_13, data_7_s7_14, data_7_s7_15, data_7_s7_16, data_7_s7_17, data_7_s7_18, data_7_s7_19,
                   data_7_s7_20, data_7_s7_21)

# assess 8
data_7_s8 <- data_7_assess[data_7_assess$stockid==ids[8],]

data_7_s8_1 <- data_7_s8[data_7_s8$terminal_year == 2010 | data_7_s8$terminal_year == 2012,]
data_7_s8_1$ty_order <- 1
data_7_s8_1$if_diff_5yrs <- "N"
data_7_s8_1$interval <- max(data_7_s8_1$terminal_year) - min(data_7_s8_1$terminal_year)

data_7_s8_2 <- data_7_s8[data_7_s8$terminal_year == 2010 | data_7_s8$terminal_year == 2013,]
data_7_s8_2$ty_order <- 2
data_7_s8_2$if_diff_5yrs <- "N"
data_7_s8_2$interval <- max(data_7_s8_2$terminal_year) - min(data_7_s8_2$terminal_year)

data_7_s8_3 <- data_7_s8[data_7_s8$terminal_year == 2010 | data_7_s8$terminal_year == 2014,]
data_7_s8_3$ty_order <- 3
data_7_s8_3$if_diff_5yrs <- "N"
data_7_s8_3$interval <- max(data_7_s8_3$terminal_year) - min(data_7_s8_3$terminal_year)

data_7_s8_4 <- data_7_s8[data_7_s8$terminal_year == 2010 | data_7_s8$terminal_year == 2015,]
data_7_s8_4$ty_order <- 4
data_7_s8_4$if_diff_5yrs <- "N"
data_7_s8_4$interval <- max(data_7_s8_4$terminal_year) - min(data_7_s8_4$terminal_year)

data_7_s8_5 <- data_7_s8[data_7_s8$terminal_year == 2010 | data_7_s8$terminal_year == 2016,]
data_7_s8_5$ty_order <- 5
data_7_s8_5$if_diff_5yrs <- "Y"
data_7_s8_5$interval <- max(data_7_s8_5$terminal_year) - min(data_7_s8_5$terminal_year)

data_7_s8_6 <- data_7_s8[data_7_s8$terminal_year == 2010 | data_7_s8$terminal_year == 2017,]
data_7_s8_6$ty_order <- 6
data_7_s8_6$if_diff_5yrs <- "Y"
data_7_s8_6$interval <- max(data_7_s8_6$terminal_year) - min(data_7_s8_6$terminal_year)

data_7_s8_7 <- data_7_s8[data_7_s8$terminal_year == 2012 | data_7_s8$terminal_year == 2013,]
data_7_s8_7$ty_order <- 7
data_7_s8_7$if_diff_5yrs <- "N"
data_7_s8_7$interval <- max(data_7_s8_7$terminal_year) - min(data_7_s8_7$terminal_year)
data_7_s8_7$ty <- 2012
data_7_s8_7$ty_minus1 <- data_7_s8_7$ty-1
data_7_s8_7$key_ty <- paste(data_7_s8_7$assessid, data_7_s8_7$ty, sep="_")
data_7_s8_7$key_ty_minus1 <- paste(data_7_s8_7$assessid, data_7_s8_7$ty_minus1, sep="_")
data_7_s8_7[,c(46:47,50:61,64:69)] <- NA

data_7_s8_8 <- data_7_s8[data_7_s8$terminal_year == 2012 | data_7_s8$terminal_year == 2014,]
data_7_s8_8$ty_order <- 8
data_7_s8_8$if_diff_5yrs <- "N"
data_7_s8_8$interval <- max(data_7_s8_8$terminal_year) - min(data_7_s8_8$terminal_year)
data_7_s8_8$ty <- 2012
data_7_s8_8$ty_minus1 <- data_7_s8_8$ty-1
data_7_s8_8$key_ty <- paste(data_7_s8_8$assessid, data_7_s8_8$ty, sep="_")
data_7_s8_8$key_ty_minus1 <- paste(data_7_s8_8$assessid, data_7_s8_8$ty_minus1, sep="_")
data_7_s8_8[,c(46:47,50:61,64:69)] <- NA

data_7_s8_9 <- data_7_s8[data_7_s8$terminal_year == 2012 | data_7_s8$terminal_year == 2015,]
data_7_s8_9$ty_order <- 9
data_7_s8_9$if_diff_5yrs <- "N"
data_7_s8_9$interval <- max(data_7_s8_9$terminal_year) - min(data_7_s8_9$terminal_year)
data_7_s8_9$ty <- 2012
data_7_s8_9$ty_minus1 <- data_7_s8_9$ty-1
data_7_s8_9$key_ty <- paste(data_7_s8_9$assessid, data_7_s8_9$ty, sep="_")
data_7_s8_9$key_ty_minus1 <- paste(data_7_s8_9$assessid, data_7_s8_9$ty_minus1, sep="_")
data_7_s8_9[,c(46:47,50:61,64:69)] <- NA

data_7_s8_10 <- data_7_s8[data_7_s8$terminal_year == 2012 | data_7_s8$terminal_year == 2016,]
data_7_s8_10$ty_order <- 10
data_7_s8_10$if_diff_5yrs <- "N"
data_7_s8_10$interval <- max(data_7_s8_10$terminal_year) - min(data_7_s8_10$terminal_year)
data_7_s8_10$ty <- 2012
data_7_s8_10$ty_minus1 <- data_7_s8_10$ty-1
data_7_s8_10$key_ty <- paste(data_7_s8_10$assessid, data_7_s8_10$ty, sep="_")
data_7_s8_10$key_ty_minus1 <- paste(data_7_s8_10$assessid, data_7_s8_10$ty_minus1, sep="_")
data_7_s8_10[,c(46:47,50:61,64:69)] <- NA

data_7_s8_11 <- data_7_s8[data_7_s8$terminal_year == 2012 | data_7_s8$terminal_year == 2017,]
data_7_s8_11$ty_order <- 11
data_7_s8_11$if_diff_5yrs <- "N"
data_7_s8_11$interval <- max(data_7_s8_11$terminal_year) - min(data_7_s8_11$terminal_year)
data_7_s8_11$ty <- 2012
data_7_s8_11$ty_minus1 <- data_7_s8_11$ty-1
data_7_s8_11$key_ty <- paste(data_7_s8_11$assessid, data_7_s8_11$ty, sep="_")
data_7_s8_11$key_ty_minus1 <- paste(data_7_s8_11$assessid, data_7_s8_11$ty_minus1, sep="_")
data_7_s8_11[,c(46:47,50:61,64:69)] <- NA

data_7_s8_12 <- data_7_s8[data_7_s8$terminal_year == 2013 | data_7_s8$terminal_year == 2014,]
data_7_s8_12$ty_order <- 12
data_7_s8_12$if_diff_5yrs <- "N"
data_7_s8_12$interval <- max(data_7_s8_12$terminal_year) - min(data_7_s8_12$terminal_year)
data_7_s8_12$ty <- 2013
data_7_s8_12$ty_minus1 <- data_7_s8_12$ty-1
data_7_s8_12$key_ty <- paste(data_7_s8_12$assessid, data_7_s8_12$ty, sep="_")
data_7_s8_12$key_ty_minus1 <- paste(data_7_s8_12$assessid, data_7_s8_12$ty_minus1, sep="_")
data_7_s8_12[,c(46:47,50:61,64:69)] <- NA

data_7_s8_13 <- data_7_s8[data_7_s8$terminal_year == 2013 | data_7_s8$terminal_year == 2015,]
data_7_s8_13$ty_order <- 13
data_7_s8_13$if_diff_5yrs <- "N"
data_7_s8_13$interval <- max(data_7_s8_13$terminal_year) - min(data_7_s8_13$terminal_year)
data_7_s8_13$ty <- 2013
data_7_s8_13$ty_minus1 <- data_7_s8_13$ty-1
data_7_s8_13$key_ty <- paste(data_7_s8_13$assessid, data_7_s8_13$ty, sep="_")
data_7_s8_13$key_ty_minus1 <- paste(data_7_s8_13$assessid, data_7_s8_13$ty_minus1, sep="_")
data_7_s8_13[,c(46:47,50:61,64:69)] <- NA

data_7_s8_14 <- data_7_s8[data_7_s8$terminal_year == 2013 | data_7_s8$terminal_year == 2016,]
data_7_s8_14$ty_order <- 14
data_7_s8_14$if_diff_5yrs <- "N"
data_7_s8_14$interval <- max(data_7_s8_14$terminal_year) - min(data_7_s8_14$terminal_year)
data_7_s8_14$ty <- 2013
data_7_s8_14$ty_minus1 <- data_7_s8_14$ty-1
data_7_s8_14$key_ty <- paste(data_7_s8_14$assessid, data_7_s8_14$ty, sep="_")
data_7_s8_14$key_ty_minus1 <- paste(data_7_s8_14$assessid, data_7_s8_14$ty_minus1, sep="_")
data_7_s8_14[,c(46:47,50:61,64:69)] <- NA

data_7_s8_15 <- data_7_s8[data_7_s8$terminal_year == 2013 | data_7_s8$terminal_year == 2017,]
data_7_s8_15$ty_order <- 15
data_7_s8_15$if_diff_5yrs <- "N"
data_7_s8_15$interval <- max(data_7_s8_15$terminal_year) - min(data_7_s8_15$terminal_year)
data_7_s8_15$ty <- 2013
data_7_s8_15$ty_minus1 <- data_7_s8_15$ty-1
data_7_s8_15$key_ty <- paste(data_7_s8_15$assessid, data_7_s8_15$ty, sep="_")
data_7_s8_15$key_ty_minus1 <- paste(data_7_s8_15$assessid, data_7_s8_15$ty_minus1, sep="_")
data_7_s8_15[,c(46:47,50:61,64:69)] <- NA

data_7_s8_16 <- data_7_s8[data_7_s8$terminal_year == 2014 | data_7_s8$terminal_year == 2015,]
data_7_s8_16$ty_order <- 16
data_7_s8_16$if_diff_5yrs <- "N"
data_7_s8_16$interval <- max(data_7_s8_16$terminal_year) - min(data_7_s8_16$terminal_year)
data_7_s8_16$ty <- 2014
data_7_s8_16$ty_minus1 <- data_7_s8_16$ty-1
data_7_s8_16$key_ty <- paste(data_7_s8_16$assessid, data_7_s8_16$ty, sep="_")
data_7_s8_16$key_ty_minus1 <- paste(data_7_s8_16$assessid, data_7_s8_16$ty_minus1, sep="_")
data_7_s8_16[,c(46:47,50:61,64:69)] <- NA

data_7_s8_17 <- data_7_s8[data_7_s8$terminal_year == 2014 | data_7_s8$terminal_year == 2016,]
data_7_s8_17$ty_order <- 17
data_7_s8_17$if_diff_5yrs <- "N"
data_7_s8_17$interval <- max(data_7_s8_17$terminal_year) - min(data_7_s8_17$terminal_year)
data_7_s8_17$ty <- 2014
data_7_s8_17$ty_minus1 <- data_7_s8_17$ty-1
data_7_s8_17$key_ty <- paste(data_7_s8_17$assessid, data_7_s8_17$ty, sep="_")
data_7_s8_17$key_ty_minus1 <- paste(data_7_s8_17$assessid, data_7_s8_17$ty_minus1, sep="_")
data_7_s8_17[,c(46:47,50:61,64:69)] <- NA

data_7_s8_18 <- data_7_s8[data_7_s8$terminal_year == 2014 | data_7_s8$terminal_year == 2017,]
data_7_s8_18$ty_order <- 18
data_7_s8_18$if_diff_5yrs <- "N"
data_7_s8_18$interval <- max(data_7_s8_18$terminal_year) - min(data_7_s8_18$terminal_year)
data_7_s8_18$ty <- 2014
data_7_s8_18$ty_minus1 <- data_7_s8_18$ty-1
data_7_s8_18$key_ty <- paste(data_7_s8_18$assessid, data_7_s8_18$ty, sep="_")
data_7_s8_18$key_ty_minus1 <- paste(data_7_s8_18$assessid, data_7_s8_18$ty_minus1, sep="_")
data_7_s8_18[,c(46:47,50:61,64:69)] <- NA

data_7_s8_19 <- data_7_s8[data_7_s8$terminal_year == 2015 | data_7_s8$terminal_year == 2016,]
data_7_s8_19$ty_order <- 19
data_7_s8_19$if_diff_5yrs <- "N"
data_7_s8_19$interval <- max(data_7_s8_19$terminal_year) - min(data_7_s8_19$terminal_year)
data_7_s8_19$ty <- 2015
data_7_s8_19$ty_minus1 <- data_7_s8_19$ty-1
data_7_s8_19$key_ty <- paste(data_7_s8_19$assessid, data_7_s8_19$ty, sep="_")
data_7_s8_19$key_ty_minus1 <- paste(data_7_s8_19$assessid, data_7_s8_19$ty_minus1, sep="_")
data_7_s8_19[,c(46:47,50:61,64:69)] <- NA

data_7_s8_20 <- data_7_s8[data_7_s8$terminal_year == 2015 | data_7_s8$terminal_year == 2017,]
data_7_s8_20$ty_order <- 20
data_7_s8_20$if_diff_5yrs <- "N"
data_7_s8_20$interval <- max(data_7_s8_20$terminal_year) - min(data_7_s8_20$terminal_year)
data_7_s8_20$ty <- 2015
data_7_s8_20$ty_minus1 <- data_7_s8_20$ty-1
data_7_s8_20$key_ty <- paste(data_7_s8_20$assessid, data_7_s8_20$ty, sep="_")
data_7_s8_20$key_ty_minus1 <- paste(data_7_s8_20$assessid, data_7_s8_20$ty_minus1, sep="_")
data_7_s8_20[,c(46:47,50:61,64:69)] <- NA

data_7_s8_21 <- data_7_s8[data_7_s8$terminal_year == 2016 | data_7_s8$terminal_year == 2017,]
data_7_s8_21$ty_order <- 21
data_7_s8_21$if_diff_5yrs <- "N"
data_7_s8_21$interval <- max(data_7_s8_21$terminal_year) - min(data_7_s8_21$terminal_year)
data_7_s8_21$ty <- 2016
data_7_s8_21$ty_minus1 <- data_7_s8_21$ty-1
data_7_s8_21$key_ty <- paste(data_7_s8_21$assessid, data_7_s8_21$ty, sep="_")
data_7_s8_21$key_ty_minus1 <- paste(data_7_s8_21$assessid, data_7_s8_21$ty_minus1, sep="_")
data_7_s8_21[,c(46:47,50:61,64:69)] <- NA

data_7_s8 <- rbind(data_7_s8_1, data_7_s8_2, data_7_s8_3, data_7_s8_4, data_7_s8_5, data_7_s8_6, data_7_s8_7, data_7_s8_8, data_7_s8_9, data_7_s8_10,
                   data_7_s8_11, data_7_s8_12, data_7_s8_13, data_7_s8_14, data_7_s8_15, data_7_s8_16, data_7_s8_17, data_7_s8_18, data_7_s8_19,
                   data_7_s8_20, data_7_s8_21)

# assess 9
data_7_s9 <- data_7_assess[data_7_assess$stockid==ids[9],]

data_7_s9_1 <- data_7_s9[data_7_s9$terminal_year == 2010 | data_7_s9$terminal_year == 2012,]
data_7_s9_1$ty_order <- 1
data_7_s9_1$if_diff_5yrs <- "N"
data_7_s9_1$interval <- max(data_7_s9_1$terminal_year) - min(data_7_s9_1$terminal_year)

data_7_s9_2 <- data_7_s9[data_7_s9$terminal_year == 2010 | data_7_s9$terminal_year == 2013,]
data_7_s9_2$ty_order <- 2
data_7_s9_2$if_diff_5yrs <- "N"
data_7_s9_2$interval <- max(data_7_s9_2$terminal_year) - min(data_7_s9_2$terminal_year)

data_7_s9_3 <- data_7_s9[data_7_s9$terminal_year == 2010 | data_7_s9$terminal_year == 2014,]
data_7_s9_3$ty_order <- 3
data_7_s9_3$if_diff_5yrs <- "N"
data_7_s9_3$interval <- max(data_7_s9_3$terminal_year) - min(data_7_s9_3$terminal_year)

data_7_s9_4 <- data_7_s9[data_7_s9$terminal_year == 2010 | data_7_s9$terminal_year == 2015,]
data_7_s9_4$ty_order <- 4
data_7_s9_4$if_diff_5yrs <- "N"
data_7_s9_4$interval <- max(data_7_s9_4$terminal_year) - min(data_7_s9_4$terminal_year)

data_7_s9_5 <- data_7_s9[data_7_s9$terminal_year == 2010 | data_7_s9$terminal_year == 2016,]
data_7_s9_5$ty_order <- 5
data_7_s9_5$if_diff_5yrs <- "Y"
data_7_s9_5$interval <- max(data_7_s9_5$terminal_year) - min(data_7_s9_5$terminal_year)

data_7_s9_6 <- data_7_s9[data_7_s9$terminal_year == 2010 | data_7_s9$terminal_year == 2017,]
data_7_s9_6$ty_order <- 6
data_7_s9_6$if_diff_5yrs <- "Y"
data_7_s9_6$interval <- max(data_7_s9_6$terminal_year) - min(data_7_s9_6$terminal_year)

data_7_s9_7 <- data_7_s9[data_7_s9$terminal_year == 2012 | data_7_s9$terminal_year == 2013,]
data_7_s9_7$ty_order <- 7
data_7_s9_7$if_diff_5yrs <- "N"
data_7_s9_7$interval <- max(data_7_s9_7$terminal_year) - min(data_7_s9_7$terminal_year)
data_7_s9_7$ty <- 2012
data_7_s9_7$ty_minus1 <- data_7_s9_7$ty-1
data_7_s9_7$key_ty <- paste(data_7_s9_7$assessid, data_7_s9_7$ty, sep="_")
data_7_s9_7$key_ty_minus1 <- paste(data_7_s9_7$assessid, data_7_s9_7$ty_minus1, sep="_")
data_7_s9_7[,c(46:47,50:61,64:69)] <- NA

data_7_s9_8 <- data_7_s9[data_7_s9$terminal_year == 2012 | data_7_s9$terminal_year == 2014,]
data_7_s9_8$ty_order <- 8
data_7_s9_8$if_diff_5yrs <- "N"
data_7_s9_8$interval <- max(data_7_s9_8$terminal_year) - min(data_7_s9_8$terminal_year)
data_7_s9_8$ty <- 2012
data_7_s9_8$ty_minus1 <- data_7_s9_8$ty-1
data_7_s9_8$key_ty <- paste(data_7_s9_8$assessid, data_7_s9_8$ty, sep="_")
data_7_s9_8$key_ty_minus1 <- paste(data_7_s9_8$assessid, data_7_s9_8$ty_minus1, sep="_")
data_7_s9_8[,c(46:47,50:61,64:69)] <- NA

data_7_s9_9 <- data_7_s9[data_7_s9$terminal_year == 2012 | data_7_s9$terminal_year == 2015,]
data_7_s9_9$ty_order <- 9
data_7_s9_9$if_diff_5yrs <- "N"
data_7_s9_9$interval <- max(data_7_s9_9$terminal_year) - min(data_7_s9_9$terminal_year)
data_7_s9_9$ty <- 2012
data_7_s9_9$ty_minus1 <- data_7_s9_9$ty-1
data_7_s9_9$key_ty <- paste(data_7_s9_9$assessid, data_7_s9_9$ty, sep="_")
data_7_s9_9$key_ty_minus1 <- paste(data_7_s9_9$assessid, data_7_s9_9$ty_minus1, sep="_")
data_7_s9_9[,c(46:47,50:61,64:69)] <- NA

data_7_s9_10 <- data_7_s9[data_7_s9$terminal_year == 2012 | data_7_s9$terminal_year == 2016,]
data_7_s9_10$ty_order <- 10
data_7_s9_10$if_diff_5yrs <- "N"
data_7_s9_10$interval <- max(data_7_s9_10$terminal_year) - min(data_7_s9_10$terminal_year)
data_7_s9_10$ty <- 2012
data_7_s9_10$ty_minus1 <- data_7_s9_10$ty-1
data_7_s9_10$key_ty <- paste(data_7_s9_10$assessid, data_7_s9_10$ty, sep="_")
data_7_s9_10$key_ty_minus1 <- paste(data_7_s9_10$assessid, data_7_s9_10$ty_minus1, sep="_")
data_7_s9_10[,c(46:47,50:61,64:69)] <- NA

data_7_s9_11 <- data_7_s9[data_7_s9$terminal_year == 2012 | data_7_s9$terminal_year == 2017,]
data_7_s9_11$ty_order <- 11
data_7_s9_11$if_diff_5yrs <- "N"
data_7_s9_11$interval <- max(data_7_s9_11$terminal_year) - min(data_7_s9_11$terminal_year)
data_7_s9_11$ty <- 2012
data_7_s9_11$ty_minus1 <- data_7_s9_11$ty-1
data_7_s9_11$key_ty <- paste(data_7_s9_11$assessid, data_7_s9_11$ty, sep="_")
data_7_s9_11$key_ty_minus1 <- paste(data_7_s9_11$assessid, data_7_s9_11$ty_minus1, sep="_")
data_7_s9_11[,c(46:47,50:61,64:69)] <- NA

data_7_s9_12 <- data_7_s9[data_7_s9$terminal_year == 2013 | data_7_s9$terminal_year == 2014,]
data_7_s9_12$ty_order <- 12
data_7_s9_12$if_diff_5yrs <- "N"
data_7_s9_12$interval <- max(data_7_s9_12$terminal_year) - min(data_7_s9_12$terminal_year)
data_7_s9_12$ty <- 2013
data_7_s9_12$ty_minus1 <- data_7_s9_12$ty-1
data_7_s9_12$key_ty <- paste(data_7_s9_12$assessid, data_7_s9_12$ty, sep="_")
data_7_s9_12$key_ty_minus1 <- paste(data_7_s9_12$assessid, data_7_s9_12$ty_minus1, sep="_")
data_7_s9_12[,c(46:47,50:61,64:69)] <- NA

data_7_s9_13 <- data_7_s9[data_7_s9$terminal_year == 2013 | data_7_s9$terminal_year == 2015,]
data_7_s9_13$ty_order <- 13
data_7_s9_13$if_diff_5yrs <- "N"
data_7_s9_13$interval <- max(data_7_s9_13$terminal_year) - min(data_7_s9_13$terminal_year)
data_7_s9_13$ty <- 2013
data_7_s9_13$ty_minus1 <- data_7_s9_13$ty-1
data_7_s9_13$key_ty <- paste(data_7_s9_13$assessid, data_7_s9_13$ty, sep="_")
data_7_s9_13$key_ty_minus1 <- paste(data_7_s9_13$assessid, data_7_s9_13$ty_minus1, sep="_")
data_7_s9_13[,c(46:47,50:61,64:69)] <- NA

data_7_s9_14 <- data_7_s9[data_7_s9$terminal_year == 2013 | data_7_s9$terminal_year == 2016,]
data_7_s9_14$ty_order <- 14
data_7_s9_14$if_diff_5yrs <- "N"
data_7_s9_14$interval <- max(data_7_s9_14$terminal_year) - min(data_7_s9_14$terminal_year)
data_7_s9_14$ty <- 2013
data_7_s9_14$ty_minus1 <- data_7_s9_14$ty-1
data_7_s9_14$key_ty <- paste(data_7_s9_14$assessid, data_7_s9_14$ty, sep="_")
data_7_s9_14$key_ty_minus1 <- paste(data_7_s9_14$assessid, data_7_s9_14$ty_minus1, sep="_")
data_7_s9_14[,c(46:47,50:61,64:69)] <- NA

data_7_s9_15 <- data_7_s9[data_7_s9$terminal_year == 2013 | data_7_s9$terminal_year == 2017,]
data_7_s9_15$ty_order <- 15
data_7_s9_15$if_diff_5yrs <- "N"
data_7_s9_15$interval <- max(data_7_s9_15$terminal_year) - min(data_7_s9_15$terminal_year)
data_7_s9_15$ty <- 2013
data_7_s9_15$ty_minus1 <- data_7_s9_15$ty-1
data_7_s9_15$key_ty <- paste(data_7_s9_15$assessid, data_7_s9_15$ty, sep="_")
data_7_s9_15$key_ty_minus1 <- paste(data_7_s9_15$assessid, data_7_s9_15$ty_minus1, sep="_")
data_7_s9_15[,c(46:47,50:61,64:69)] <- NA

data_7_s9_16 <- data_7_s9[data_7_s9$terminal_year == 2014 | data_7_s9$terminal_year == 2015,]
data_7_s9_16$ty_order <- 16
data_7_s9_16$if_diff_5yrs <- "N"
data_7_s9_16$interval <- max(data_7_s9_16$terminal_year) - min(data_7_s9_16$terminal_year)
data_7_s9_16$ty <- 2014
data_7_s9_16$ty_minus1 <- data_7_s9_16$ty-1
data_7_s9_16$key_ty <- paste(data_7_s9_16$assessid, data_7_s9_16$ty, sep="_")
data_7_s9_16$key_ty_minus1 <- paste(data_7_s9_16$assessid, data_7_s9_16$ty_minus1, sep="_")
data_7_s9_16[,c(46:47,50:61,64:69)] <- NA

data_7_s9_17 <- data_7_s9[data_7_s9$terminal_year == 2014 | data_7_s9$terminal_year == 2016,]
data_7_s9_17$ty_order <- 17
data_7_s9_17$if_diff_5yrs <- "N"
data_7_s9_17$interval <- max(data_7_s9_17$terminal_year) - min(data_7_s9_17$terminal_year)
data_7_s9_17$ty <- 2014
data_7_s9_17$ty_minus1 <- data_7_s9_17$ty-1
data_7_s9_17$key_ty <- paste(data_7_s9_17$assessid, data_7_s9_17$ty, sep="_")
data_7_s9_17$key_ty_minus1 <- paste(data_7_s9_17$assessid, data_7_s9_17$ty_minus1, sep="_")
data_7_s9_17[,c(46:47,50:61,64:69)] <- NA

data_7_s9_18 <- data_7_s9[data_7_s9$terminal_year == 2014 | data_7_s9$terminal_year == 2017,]
data_7_s9_18$ty_order <- 18
data_7_s9_18$if_diff_5yrs <- "N"
data_7_s9_18$interval <- max(data_7_s9_18$terminal_year) - min(data_7_s9_18$terminal_year)
data_7_s9_18$ty <- 2014
data_7_s9_18$ty_minus1 <- data_7_s9_18$ty-1
data_7_s9_18$key_ty <- paste(data_7_s9_18$assessid, data_7_s9_18$ty, sep="_")
data_7_s9_18$key_ty_minus1 <- paste(data_7_s9_18$assessid, data_7_s9_18$ty_minus1, sep="_")
data_7_s9_18[,c(46:47,50:61,64:69)] <- NA

data_7_s9_19 <- data_7_s9[data_7_s9$terminal_year == 2015 | data_7_s9$terminal_year == 2016,]
data_7_s9_19$ty_order <- 19
data_7_s9_19$if_diff_5yrs <- "N"
data_7_s9_19$interval <- max(data_7_s9_19$terminal_year) - min(data_7_s9_19$terminal_year)
data_7_s9_19$ty <- 2015
data_7_s9_19$ty_minus1 <- data_7_s9_19$ty-1
data_7_s9_19$key_ty <- paste(data_7_s9_19$assessid, data_7_s9_19$ty, sep="_")
data_7_s9_19$key_ty_minus1 <- paste(data_7_s9_19$assessid, data_7_s9_19$ty_minus1, sep="_")
data_7_s9_19[,c(46:47,50:61,64:69)] <- NA

data_7_s9_20 <- data_7_s9[data_7_s9$terminal_year == 2015 | data_7_s9$terminal_year == 2017,]
data_7_s9_20$ty_order <- 20
data_7_s9_20$if_diff_5yrs <- "N"
data_7_s9_20$interval <- max(data_7_s9_20$terminal_year) - min(data_7_s9_20$terminal_year)
data_7_s9_20$ty <- 2015
data_7_s9_20$ty_minus1 <- data_7_s9_20$ty-1
data_7_s9_20$key_ty <- paste(data_7_s9_20$assessid, data_7_s9_20$ty, sep="_")
data_7_s9_20$key_ty_minus1 <- paste(data_7_s9_20$assessid, data_7_s9_20$ty_minus1, sep="_")
data_7_s9_20[,c(46:47,50:61,64:69)] <- NA

data_7_s9_21 <- data_7_s9[data_7_s9$terminal_year == 2016 | data_7_s9$terminal_year == 2017,]
data_7_s9_21$ty_order <- 21
data_7_s9_21$if_diff_5yrs <- "N"
data_7_s9_21$interval <- max(data_7_s9_21$terminal_year) - min(data_7_s9_21$terminal_year)
data_7_s9_21$ty <- 2016
data_7_s9_21$ty_minus1 <- data_7_s9_21$ty-1
data_7_s9_21$key_ty <- paste(data_7_s9_21$assessid, data_7_s9_21$ty, sep="_")
data_7_s9_21$key_ty_minus1 <- paste(data_7_s9_21$assessid, data_7_s9_21$ty_minus1, sep="_")
data_7_s9_21[,c(46:47,50:61,64:69)] <- NA

data_7_s9 <- rbind(data_7_s9_1, data_7_s9_2, data_7_s9_3, data_7_s9_4, data_7_s9_5, data_7_s9_6, data_7_s9_7, data_7_s9_8, data_7_s9_9, data_7_s9_10,
                   data_7_s9_11, data_7_s9_12, data_7_s9_13, data_7_s9_14, data_7_s9_15, data_7_s9_16, data_7_s9_17, data_7_s9_18, data_7_s9_19,
                   data_7_s9_20, data_7_s9_21)

# assess 10
data_7_s10 <- data_7_assess[data_7_assess$stockid==ids[10],]

data_7_s10_1 <- data_7_s10[data_7_s10$terminal_year == 2010 | data_7_s10$terminal_year == 2012,]
data_7_s10_1$ty_order <- 1
data_7_s10_1$if_diff_5yrs <- "N"
data_7_s10_1$interval <- max(data_7_s10_1$terminal_year) - min(data_7_s10_1$terminal_year)

data_7_s10_2 <- data_7_s10[data_7_s10$terminal_year == 2010 | data_7_s10$terminal_year == 2013,]
data_7_s10_2$ty_order <- 2
data_7_s10_2$if_diff_5yrs <- "N"
data_7_s10_2$interval <- max(data_7_s10_2$terminal_year) - min(data_7_s10_2$terminal_year)

data_7_s10_3 <- data_7_s10[data_7_s10$terminal_year == 2010 | data_7_s10$terminal_year == 2014,]
data_7_s10_3$ty_order <- 3
data_7_s10_3$if_diff_5yrs <- "N"
data_7_s10_3$interval <- max(data_7_s10_3$terminal_year) - min(data_7_s10_3$terminal_year)

data_7_s10_4 <- data_7_s10[data_7_s10$terminal_year == 2010 | data_7_s10$terminal_year == 2015,]
data_7_s10_4$ty_order <- 4
data_7_s10_4$if_diff_5yrs <- "N"
data_7_s10_4$interval <- max(data_7_s10_4$terminal_year) - min(data_7_s10_4$terminal_year)

data_7_s10_5 <- data_7_s10[data_7_s10$terminal_year == 2010 | data_7_s10$terminal_year == 2016,]
data_7_s10_5$ty_order <- 5
data_7_s10_5$if_diff_5yrs <- "Y"
data_7_s10_5$interval <- max(data_7_s10_5$terminal_year) - min(data_7_s10_5$terminal_year)

data_7_s10_6 <- data_7_s10[data_7_s10$terminal_year == 2010 | data_7_s10$terminal_year == 2017,]
data_7_s10_6$ty_order <- 6
data_7_s10_6$if_diff_5yrs <- "Y"
data_7_s10_6$interval <- max(data_7_s10_6$terminal_year) - min(data_7_s10_6$terminal_year)

data_7_s10_7 <- data_7_s10[data_7_s10$terminal_year == 2012 | data_7_s10$terminal_year == 2013,]
data_7_s10_7$ty_order <- 7
data_7_s10_7$if_diff_5yrs <- "N"
data_7_s10_7$interval <- max(data_7_s10_7$terminal_year) - min(data_7_s10_7$terminal_year)
data_7_s10_7$ty <- 2012
data_7_s10_7$ty_minus1 <- data_7_s10_7$ty-1
data_7_s10_7$key_ty <- paste(data_7_s10_7$assessid, data_7_s10_7$ty, sep="_")
data_7_s10_7$key_ty_minus1 <- paste(data_7_s10_7$assessid, data_7_s10_7$ty_minus1, sep="_")
data_7_s10_7[,c(46:47,50:61,64:69)] <- NA

data_7_s10_8 <- data_7_s10[data_7_s10$terminal_year == 2012 | data_7_s10$terminal_year == 2014,]
data_7_s10_8$ty_order <- 8
data_7_s10_8$if_diff_5yrs <- "N"
data_7_s10_8$interval <- max(data_7_s10_8$terminal_year) - min(data_7_s10_8$terminal_year)
data_7_s10_8$ty <- 2012
data_7_s10_8$ty_minus1 <- data_7_s10_8$ty-1
data_7_s10_8$key_ty <- paste(data_7_s10_8$assessid, data_7_s10_8$ty, sep="_")
data_7_s10_8$key_ty_minus1 <- paste(data_7_s10_8$assessid, data_7_s10_8$ty_minus1, sep="_")
data_7_s10_8[,c(46:47,50:61,64:69)] <- NA

data_7_s10_9 <- data_7_s10[data_7_s10$terminal_year == 2012 | data_7_s10$terminal_year == 2015,]
data_7_s10_9$ty_order <- 9
data_7_s10_9$if_diff_5yrs <- "N"
data_7_s10_9$interval <- max(data_7_s10_9$terminal_year) - min(data_7_s10_9$terminal_year)
data_7_s10_9$ty <- 2012
data_7_s10_9$ty_minus1 <- data_7_s10_9$ty-1
data_7_s10_9$key_ty <- paste(data_7_s10_9$assessid, data_7_s10_9$ty, sep="_")
data_7_s10_9$key_ty_minus1 <- paste(data_7_s10_9$assessid, data_7_s10_9$ty_minus1, sep="_")
data_7_s10_9[,c(46:47,50:61,64:69)] <- NA

data_7_s10_10 <- data_7_s10[data_7_s10$terminal_year == 2012 | data_7_s10$terminal_year == 2016,]
data_7_s10_10$ty_order <- 10
data_7_s10_10$if_diff_5yrs <- "N"
data_7_s10_10$interval <- max(data_7_s10_10$terminal_year) - min(data_7_s10_10$terminal_year)
data_7_s10_10$ty <- 2012
data_7_s10_10$ty_minus1 <- data_7_s10_10$ty-1
data_7_s10_10$key_ty <- paste(data_7_s10_10$assessid, data_7_s10_10$ty, sep="_")
data_7_s10_10$key_ty_minus1 <- paste(data_7_s10_10$assessid, data_7_s10_10$ty_minus1, sep="_")
data_7_s10_10[,c(46:47,50:61,64:69)] <- NA

data_7_s10_11 <- data_7_s10[data_7_s10$terminal_year == 2012 | data_7_s10$terminal_year == 2017,]
data_7_s10_11$ty_order <- 11
data_7_s10_11$if_diff_5yrs <- "N"
data_7_s10_11$interval <- max(data_7_s10_11$terminal_year) - min(data_7_s10_11$terminal_year)
data_7_s10_11$ty <- 2012
data_7_s10_11$ty_minus1 <- data_7_s10_11$ty-1
data_7_s10_11$key_ty <- paste(data_7_s10_11$assessid, data_7_s10_11$ty, sep="_")
data_7_s10_11$key_ty_minus1 <- paste(data_7_s10_11$assessid, data_7_s10_11$ty_minus1, sep="_")
data_7_s10_11[,c(46:47,50:61,64:69)] <- NA

data_7_s10_12 <- data_7_s10[data_7_s10$terminal_year == 2013 | data_7_s10$terminal_year == 2014,]
data_7_s10_12$ty_order <- 12
data_7_s10_12$if_diff_5yrs <- "N"
data_7_s10_12$interval <- max(data_7_s10_12$terminal_year) - min(data_7_s10_12$terminal_year)
data_7_s10_12$ty <- 2013
data_7_s10_12$ty_minus1 <- data_7_s10_12$ty-1
data_7_s10_12$key_ty <- paste(data_7_s10_12$assessid, data_7_s10_12$ty, sep="_")
data_7_s10_12$key_ty_minus1 <- paste(data_7_s10_12$assessid, data_7_s10_12$ty_minus1, sep="_")
data_7_s10_12[,c(46:47,50:61,64:69)] <- NA

data_7_s10_13 <- data_7_s10[data_7_s10$terminal_year == 2013 | data_7_s10$terminal_year == 2015,]
data_7_s10_13$ty_order <- 13
data_7_s10_13$if_diff_5yrs <- "N"
data_7_s10_13$interval <- max(data_7_s10_13$terminal_year) - min(data_7_s10_13$terminal_year)
data_7_s10_13$ty <- 2013
data_7_s10_13$ty_minus1 <- data_7_s10_13$ty-1
data_7_s10_13$key_ty <- paste(data_7_s10_13$assessid, data_7_s10_13$ty, sep="_")
data_7_s10_13$key_ty_minus1 <- paste(data_7_s10_13$assessid, data_7_s10_13$ty_minus1, sep="_")
data_7_s10_13[,c(46:47,50:61,64:69)] <- NA

data_7_s10_14 <- data_7_s10[data_7_s10$terminal_year == 2013 | data_7_s10$terminal_year == 2016,]
data_7_s10_14$ty_order <- 14
data_7_s10_14$if_diff_5yrs <- "N"
data_7_s10_14$interval <- max(data_7_s10_14$terminal_year) - min(data_7_s10_14$terminal_year)
data_7_s10_14$ty <- 2013
data_7_s10_14$ty_minus1 <- data_7_s10_14$ty-1
data_7_s10_14$key_ty <- paste(data_7_s10_14$assessid, data_7_s10_14$ty, sep="_")
data_7_s10_14$key_ty_minus1 <- paste(data_7_s10_14$assessid, data_7_s10_14$ty_minus1, sep="_")
data_7_s10_14[,c(46:47,50:61,64:69)] <- NA

data_7_s10_15 <- data_7_s10[data_7_s10$terminal_year == 2013 | data_7_s10$terminal_year == 2017,]
data_7_s10_15$ty_order <- 15
data_7_s10_15$if_diff_5yrs <- "N"
data_7_s10_15$interval <- max(data_7_s10_15$terminal_year) - min(data_7_s10_15$terminal_year)
data_7_s10_15$ty <- 2013
data_7_s10_15$ty_minus1 <- data_7_s10_15$ty-1
data_7_s10_15$key_ty <- paste(data_7_s10_15$assessid, data_7_s10_15$ty, sep="_")
data_7_s10_15$key_ty_minus1 <- paste(data_7_s10_15$assessid, data_7_s10_15$ty_minus1, sep="_")
data_7_s10_15[,c(46:47,50:61,64:69)] <- NA

data_7_s10_16 <- data_7_s10[data_7_s10$terminal_year == 2014 | data_7_s10$terminal_year == 2015,]
data_7_s10_16$ty_order <- 16
data_7_s10_16$if_diff_5yrs <- "N"
data_7_s10_16$interval <- max(data_7_s10_16$terminal_year) - min(data_7_s10_16$terminal_year)
data_7_s10_16$ty <- 2014
data_7_s10_16$ty_minus1 <- data_7_s10_16$ty-1
data_7_s10_16$key_ty <- paste(data_7_s10_16$assessid, data_7_s10_16$ty, sep="_")
data_7_s10_16$key_ty_minus1 <- paste(data_7_s10_16$assessid, data_7_s10_16$ty_minus1, sep="_")
data_7_s10_16[,c(46:47,50:61,64:69)] <- NA

data_7_s10_17 <- data_7_s10[data_7_s10$terminal_year == 2014 | data_7_s10$terminal_year == 2016,]
data_7_s10_17$ty_order <- 17
data_7_s10_17$if_diff_5yrs <- "N"
data_7_s10_17$interval <- max(data_7_s10_17$terminal_year) - min(data_7_s10_17$terminal_year)
data_7_s10_17$ty <- 2014
data_7_s10_17$ty_minus1 <- data_7_s10_17$ty-1
data_7_s10_17$key_ty <- paste(data_7_s10_17$assessid, data_7_s10_17$ty, sep="_")
data_7_s10_17$key_ty_minus1 <- paste(data_7_s10_17$assessid, data_7_s10_17$ty_minus1, sep="_")
data_7_s10_17[,c(46:47,50:61,64:69)] <- NA

data_7_s10_18 <- data_7_s10[data_7_s10$terminal_year == 2014 | data_7_s10$terminal_year == 2017,]
data_7_s10_18$ty_order <- 18
data_7_s10_18$if_diff_5yrs <- "N"
data_7_s10_18$interval <- max(data_7_s10_18$terminal_year) - min(data_7_s10_18$terminal_year)
data_7_s10_18$ty <- 2014
data_7_s10_18$ty_minus1 <- data_7_s10_18$ty-1
data_7_s10_18$key_ty <- paste(data_7_s10_18$assessid, data_7_s10_18$ty, sep="_")
data_7_s10_18$key_ty_minus1 <- paste(data_7_s10_18$assessid, data_7_s10_18$ty_minus1, sep="_")
data_7_s10_18[,c(46:47,50:61,64:69)] <- NA

data_7_s10_19 <- data_7_s10[data_7_s10$terminal_year == 2015 | data_7_s10$terminal_year == 2016,]
data_7_s10_19$ty_order <- 19
data_7_s10_19$if_diff_5yrs <- "N"
data_7_s10_19$interval <- max(data_7_s10_19$terminal_year) - min(data_7_s10_19$terminal_year)
data_7_s10_19$ty <- 2015
data_7_s10_19$ty_minus1 <- data_7_s10_19$ty-1
data_7_s10_19$key_ty <- paste(data_7_s10_19$assessid, data_7_s10_19$ty, sep="_")
data_7_s10_19$key_ty_minus1 <- paste(data_7_s10_19$assessid, data_7_s10_19$ty_minus1, sep="_")
data_7_s10_19[,c(46:47,50:61,64:69)] <- NA

data_7_s10_20 <- data_7_s10[data_7_s10$terminal_year == 2015 | data_7_s10$terminal_year == 2017,]
data_7_s10_20$ty_order <- 20
data_7_s10_20$if_diff_5yrs <- "N"
data_7_s10_20$interval <- max(data_7_s10_20$terminal_year) - min(data_7_s10_20$terminal_year)
data_7_s10_20$ty <- 2015
data_7_s10_20$ty_minus1 <- data_7_s10_20$ty-1
data_7_s10_20$key_ty <- paste(data_7_s10_20$assessid, data_7_s10_20$ty, sep="_")
data_7_s10_20$key_ty_minus1 <- paste(data_7_s10_20$assessid, data_7_s10_20$ty_minus1, sep="_")
data_7_s10_20[,c(46:47,50:61,64:69)] <- NA

data_7_s10_21 <- data_7_s10[data_7_s10$terminal_year == 2016 | data_7_s10$terminal_year == 2017,]
data_7_s10_21$ty_order <- 21
data_7_s10_21$if_diff_5yrs <- "N"
data_7_s10_21$interval <- max(data_7_s10_21$terminal_year) - min(data_7_s10_21$terminal_year)
data_7_s10_21$ty <- 2016
data_7_s10_21$ty_minus1 <- data_7_s10_21$ty-1
data_7_s10_21$key_ty <- paste(data_7_s10_21$assessid, data_7_s10_21$ty, sep="_")
data_7_s10_21$key_ty_minus1 <- paste(data_7_s10_21$assessid, data_7_s10_21$ty_minus1, sep="_")
data_7_s10_21[,c(46:47,50:61,64:69)] <- NA

data_7_s10 <- rbind(data_7_s10_1, data_7_s10_2, data_7_s10_3, data_7_s10_4, data_7_s10_5, data_7_s10_6, data_7_s10_7, data_7_s10_8, data_7_s10_9, data_7_s10_10,
                    data_7_s10_11, data_7_s10_12, data_7_s10_13, data_7_s10_14, data_7_s10_15, data_7_s10_16, data_7_s10_17, data_7_s10_18, data_7_s10_19,
                    data_7_s10_20, data_7_s10_21)

# assess 11
data_7_s11 <- data_7_assess[data_7_assess$stockid==ids[11],]

data_7_s11_1 <- data_7_s11[data_7_s11$terminal_year == 2010 | data_7_s11$terminal_year == 2012,]
data_7_s11_1$ty_order <- 1
data_7_s11_1$if_diff_5yrs <- "N"
data_7_s11_1$interval <- max(data_7_s11_1$terminal_year) - min(data_7_s11_1$terminal_year)

data_7_s11_2 <- data_7_s11[data_7_s11$terminal_year == 2010 | data_7_s11$terminal_year == 2013,]
data_7_s11_2$ty_order <- 2
data_7_s11_2$if_diff_5yrs <- "N"
data_7_s11_2$interval <- max(data_7_s11_2$terminal_year) - min(data_7_s11_2$terminal_year)

data_7_s11_3 <- data_7_s11[data_7_s11$terminal_year == 2010 | data_7_s11$terminal_year == 2014,]
data_7_s11_3$ty_order <- 3
data_7_s11_3$if_diff_5yrs <- "N"
data_7_s11_3$interval <- max(data_7_s11_3$terminal_year) - min(data_7_s11_3$terminal_year)

data_7_s11_4 <- data_7_s11[data_7_s11$terminal_year == 2010 | data_7_s11$terminal_year == 2015,]
data_7_s11_4$ty_order <- 4
data_7_s11_4$if_diff_5yrs <- "N"
data_7_s11_4$interval <- max(data_7_s11_4$terminal_year) - min(data_7_s11_4$terminal_year)

data_7_s11_5 <- data_7_s11[data_7_s11$terminal_year == 2010 | data_7_s11$terminal_year == 2016,]
data_7_s11_5$ty_order <- 5
data_7_s11_5$if_diff_5yrs <- "Y"
data_7_s11_5$interval <- max(data_7_s11_5$terminal_year) - min(data_7_s11_5$terminal_year)

data_7_s11_6 <- data_7_s11[data_7_s11$terminal_year == 2010 | data_7_s11$terminal_year == 2017,]
data_7_s11_6$ty_order <- 6
data_7_s11_6$if_diff_5yrs <- "Y"
data_7_s11_6$interval <- max(data_7_s11_6$terminal_year) - min(data_7_s11_6$terminal_year)

data_7_s11_7 <- data_7_s11[data_7_s11$terminal_year == 2012 | data_7_s11$terminal_year == 2013,]
data_7_s11_7$ty_order <- 7
data_7_s11_7$if_diff_5yrs <- "N"
data_7_s11_7$interval <- max(data_7_s11_7$terminal_year) - min(data_7_s11_7$terminal_year)
data_7_s11_7$ty <- 2012
data_7_s11_7$ty_minus1 <- data_7_s11_7$ty-1
data_7_s11_7$key_ty <- paste(data_7_s11_7$assessid, data_7_s11_7$ty, sep="_")
data_7_s11_7$key_ty_minus1 <- paste(data_7_s11_7$assessid, data_7_s11_7$ty_minus1, sep="_")
data_7_s11_7[,c(46:47,50:61,64:69)] <- NA

data_7_s11_8 <- data_7_s11[data_7_s11$terminal_year == 2012 | data_7_s11$terminal_year == 2014,]
data_7_s11_8$ty_order <- 8
data_7_s11_8$if_diff_5yrs <- "N"
data_7_s11_8$interval <- max(data_7_s11_8$terminal_year) - min(data_7_s11_8$terminal_year)
data_7_s11_8$ty <- 2012
data_7_s11_8$ty_minus1 <- data_7_s11_8$ty-1
data_7_s11_8$key_ty <- paste(data_7_s11_8$assessid, data_7_s11_8$ty, sep="_")
data_7_s11_8$key_ty_minus1 <- paste(data_7_s11_8$assessid, data_7_s11_8$ty_minus1, sep="_")
data_7_s11_8[,c(46:47,50:61,64:69)] <- NA

data_7_s11_9 <- data_7_s11[data_7_s11$terminal_year == 2012 | data_7_s11$terminal_year == 2015,]
data_7_s11_9$ty_order <- 9
data_7_s11_9$if_diff_5yrs <- "N"
data_7_s11_9$interval <- max(data_7_s11_9$terminal_year) - min(data_7_s11_9$terminal_year)
data_7_s11_9$ty <- 2012
data_7_s11_9$ty_minus1 <- data_7_s11_9$ty-1
data_7_s11_9$key_ty <- paste(data_7_s11_9$assessid, data_7_s11_9$ty, sep="_")
data_7_s11_9$key_ty_minus1 <- paste(data_7_s11_9$assessid, data_7_s11_9$ty_minus1, sep="_")
data_7_s11_9[,c(46:47,50:61,64:69)] <- NA

data_7_s11_10 <- data_7_s11[data_7_s11$terminal_year == 2012 | data_7_s11$terminal_year == 2016,]
data_7_s11_10$ty_order <- 10
data_7_s11_10$if_diff_5yrs <- "N"
data_7_s11_10$interval <- max(data_7_s11_10$terminal_year) - min(data_7_s11_10$terminal_year)
data_7_s11_10$ty <- 2012
data_7_s11_10$ty_minus1 <- data_7_s11_10$ty-1
data_7_s11_10$key_ty <- paste(data_7_s11_10$assessid, data_7_s11_10$ty, sep="_")
data_7_s11_10$key_ty_minus1 <- paste(data_7_s11_10$assessid, data_7_s11_10$ty_minus1, sep="_")
data_7_s11_10[,c(46:47,50:61,64:69)] <- NA

data_7_s11_11 <- data_7_s11[data_7_s11$terminal_year == 2012 | data_7_s11$terminal_year == 2017,]
data_7_s11_11$ty_order <- 11
data_7_s11_11$if_diff_5yrs <- "N"
data_7_s11_11$interval <- max(data_7_s11_11$terminal_year) - min(data_7_s11_11$terminal_year)
data_7_s11_11$ty <- 2012
data_7_s11_11$ty_minus1 <- data_7_s11_11$ty-1
data_7_s11_11$key_ty <- paste(data_7_s11_11$assessid, data_7_s11_11$ty, sep="_")
data_7_s11_11$key_ty_minus1 <- paste(data_7_s11_11$assessid, data_7_s11_11$ty_minus1, sep="_")
data_7_s11_11[,c(46:47,50:61,64:69)] <- NA

data_7_s11_12 <- data_7_s11[data_7_s11$terminal_year == 2013 | data_7_s11$terminal_year == 2014,]
data_7_s11_12$ty_order <- 12
data_7_s11_12$if_diff_5yrs <- "N"
data_7_s11_12$interval <- max(data_7_s11_12$terminal_year) - min(data_7_s11_12$terminal_year)
data_7_s11_12$ty <- 2013
data_7_s11_12$ty_minus1 <- data_7_s11_12$ty-1
data_7_s11_12$key_ty <- paste(data_7_s11_12$assessid, data_7_s11_12$ty, sep="_")
data_7_s11_12$key_ty_minus1 <- paste(data_7_s11_12$assessid, data_7_s11_12$ty_minus1, sep="_")
data_7_s11_12[,c(46:47,50:61,64:69)] <- NA

data_7_s11_13 <- data_7_s11[data_7_s11$terminal_year == 2013 | data_7_s11$terminal_year == 2015,]
data_7_s11_13$ty_order <- 13
data_7_s11_13$if_diff_5yrs <- "N"
data_7_s11_13$interval <- max(data_7_s11_13$terminal_year) - min(data_7_s11_13$terminal_year)
data_7_s11_13$ty <- 2013
data_7_s11_13$ty_minus1 <- data_7_s11_13$ty-1
data_7_s11_13$key_ty <- paste(data_7_s11_13$assessid, data_7_s11_13$ty, sep="_")
data_7_s11_13$key_ty_minus1 <- paste(data_7_s11_13$assessid, data_7_s11_13$ty_minus1, sep="_")
data_7_s11_13[,c(46:47,50:61,64:69)] <- NA

data_7_s11_14 <- data_7_s11[data_7_s11$terminal_year == 2013 | data_7_s11$terminal_year == 2016,]
data_7_s11_14$ty_order <- 14
data_7_s11_14$if_diff_5yrs <- "N"
data_7_s11_14$interval <- max(data_7_s11_14$terminal_year) - min(data_7_s11_14$terminal_year)
data_7_s11_14$ty <- 2013
data_7_s11_14$ty_minus1 <- data_7_s11_14$ty-1
data_7_s11_14$key_ty <- paste(data_7_s11_14$assessid, data_7_s11_14$ty, sep="_")
data_7_s11_14$key_ty_minus1 <- paste(data_7_s11_14$assessid, data_7_s11_14$ty_minus1, sep="_")
data_7_s11_14[,c(46:47,50:61,64:69)] <- NA

data_7_s11_15 <- data_7_s11[data_7_s11$terminal_year == 2013 | data_7_s11$terminal_year == 2017,]
data_7_s11_15$ty_order <- 15
data_7_s11_15$if_diff_5yrs <- "N"
data_7_s11_15$interval <- max(data_7_s11_15$terminal_year) - min(data_7_s11_15$terminal_year)
data_7_s11_15$ty <- 2013
data_7_s11_15$ty_minus1 <- data_7_s11_15$ty-1
data_7_s11_15$key_ty <- paste(data_7_s11_15$assessid, data_7_s11_15$ty, sep="_")
data_7_s11_15$key_ty_minus1 <- paste(data_7_s11_15$assessid, data_7_s11_15$ty_minus1, sep="_")
data_7_s11_15[,c(46:47,50:61,64:69)] <- NA

data_7_s11_16 <- data_7_s11[data_7_s11$terminal_year == 2014 | data_7_s11$terminal_year == 2015,]
data_7_s11_16$ty_order <- 16
data_7_s11_16$if_diff_5yrs <- "N"
data_7_s11_16$interval <- max(data_7_s11_16$terminal_year) - min(data_7_s11_16$terminal_year)
data_7_s11_16$ty <- 2014
data_7_s11_16$ty_minus1 <- data_7_s11_16$ty-1
data_7_s11_16$key_ty <- paste(data_7_s11_16$assessid, data_7_s11_16$ty, sep="_")
data_7_s11_16$key_ty_minus1 <- paste(data_7_s11_16$assessid, data_7_s11_16$ty_minus1, sep="_")
data_7_s11_16[,c(46:47,50:61,64:69)] <- NA

data_7_s11_17 <- data_7_s11[data_7_s11$terminal_year == 2014 | data_7_s11$terminal_year == 2016,]
data_7_s11_17$ty_order <- 17
data_7_s11_17$if_diff_5yrs <- "N"
data_7_s11_17$interval <- max(data_7_s11_17$terminal_year) - min(data_7_s11_17$terminal_year)
data_7_s11_17$ty <- 2014
data_7_s11_17$ty_minus1 <- data_7_s11_17$ty-1
data_7_s11_17$key_ty <- paste(data_7_s11_17$assessid, data_7_s11_17$ty, sep="_")
data_7_s11_17$key_ty_minus1 <- paste(data_7_s11_17$assessid, data_7_s11_17$ty_minus1, sep="_")
data_7_s11_17[,c(46:47,50:61,64:69)] <- NA

data_7_s11_18 <- data_7_s11[data_7_s11$terminal_year == 2014 | data_7_s11$terminal_year == 2017,]
data_7_s11_18$ty_order <- 18
data_7_s11_18$if_diff_5yrs <- "N"
data_7_s11_18$interval <- max(data_7_s11_18$terminal_year) - min(data_7_s11_18$terminal_year)
data_7_s11_18$ty <- 2014
data_7_s11_18$ty_minus1 <- data_7_s11_18$ty-1
data_7_s11_18$key_ty <- paste(data_7_s11_18$assessid, data_7_s11_18$ty, sep="_")
data_7_s11_18$key_ty_minus1 <- paste(data_7_s11_18$assessid, data_7_s11_18$ty_minus1, sep="_")
data_7_s11_18[,c(46:47,50:61,64:69)] <- NA

data_7_s11_19 <- data_7_s11[data_7_s11$terminal_year == 2015 | data_7_s11$terminal_year == 2016,]
data_7_s11_19$ty_order <- 19
data_7_s11_19$if_diff_5yrs <- "N"
data_7_s11_19$interval <- max(data_7_s11_19$terminal_year) - min(data_7_s11_19$terminal_year)
data_7_s11_19$ty <- 2015
data_7_s11_19$ty_minus1 <- data_7_s11_19$ty-1
data_7_s11_19$key_ty <- paste(data_7_s11_19$assessid, data_7_s11_19$ty, sep="_")
data_7_s11_19$key_ty_minus1 <- paste(data_7_s11_19$assessid, data_7_s11_19$ty_minus1, sep="_")
data_7_s11_19[,c(46:47,50:61,64:69)] <- NA

data_7_s11_20 <- data_7_s11[data_7_s11$terminal_year == 2015 | data_7_s11$terminal_year == 2017,]
data_7_s11_20$ty_order <- 20
data_7_s11_20$if_diff_5yrs <- "N"
data_7_s11_20$interval <- max(data_7_s11_20$terminal_year) - min(data_7_s11_20$terminal_year)
data_7_s11_20$ty <- 2015
data_7_s11_20$ty_minus1 <- data_7_s11_20$ty-1
data_7_s11_20$key_ty <- paste(data_7_s11_20$assessid, data_7_s11_20$ty, sep="_")
data_7_s11_20$key_ty_minus1 <- paste(data_7_s11_20$assessid, data_7_s11_20$ty_minus1, sep="_")
data_7_s11_20[,c(46:47,50:61,64:69)] <- NA

data_7_s11_21 <- data_7_s11[data_7_s11$terminal_year == 2016 | data_7_s11$terminal_year == 2017,]
data_7_s11_21$ty_order <- 21
data_7_s11_21$if_diff_5yrs <- "N"
data_7_s11_21$interval <- max(data_7_s11_21$terminal_year) - min(data_7_s11_21$terminal_year)
data_7_s11_21$ty <- 2016
data_7_s11_21$ty_minus1 <- data_7_s11_21$ty-1
data_7_s11_21$key_ty <- paste(data_7_s11_21$assessid, data_7_s11_21$ty, sep="_")
data_7_s11_21$key_ty_minus1 <- paste(data_7_s11_21$assessid, data_7_s11_21$ty_minus1, sep="_")
data_7_s11_21[,c(46:47,50:61,64:69)] <- NA

data_7_s11 <- rbind(data_7_s11_1, data_7_s11_2, data_7_s11_3, data_7_s11_4, data_7_s11_5, data_7_s11_6, data_7_s11_7, data_7_s11_8, data_7_s11_9, data_7_s11_10,
                    data_7_s11_11, data_7_s11_12, data_7_s11_13, data_7_s11_14, data_7_s11_15, data_7_s11_16, data_7_s11_17, data_7_s11_18, data_7_s11_19,
                    data_7_s11_20, data_7_s11_21)

# assess 12
data_7_s12 <- data_7_assess[data_7_assess$stockid==ids[12],]

data_7_s12_1 <- data_7_s12[data_7_s12$terminal_year == 2011 | data_7_s12$terminal_year == 2012,]
data_7_s12_1$ty_order <- 1
data_7_s12_1$if_diff_5yrs <- "N"
data_7_s12_1$interval <- max(data_7_s12_1$terminal_year) - min(data_7_s12_1$terminal_year)

data_7_s12_2 <- data_7_s12[data_7_s12$terminal_year == 2011 | data_7_s12$terminal_year == 2013,]
data_7_s12_2$ty_order <- 2
data_7_s12_2$if_diff_5yrs <- "N"
data_7_s12_2$interval <- max(data_7_s12_2$terminal_year) - min(data_7_s12_2$terminal_year)

data_7_s12_3 <- data_7_s12[data_7_s12$terminal_year == 2011 | data_7_s12$terminal_year == 2014,]
data_7_s12_3$ty_order <- 3
data_7_s12_3$if_diff_5yrs <- "N"
data_7_s12_3$interval <- max(data_7_s12_3$terminal_year) - min(data_7_s12_3$terminal_year)

data_7_s12_4 <- data_7_s12[data_7_s12$terminal_year == 2011 | data_7_s12$terminal_year == 2015,]
data_7_s12_4$ty_order <- 4
data_7_s12_4$if_diff_5yrs <- "N"
data_7_s12_4$interval <- max(data_7_s12_4$terminal_year) - min(data_7_s12_4$terminal_year)

data_7_s12_5 <- data_7_s12[data_7_s12$terminal_year == 2011 | data_7_s12$terminal_year == 2016,]
data_7_s12_5$ty_order <- 5
data_7_s12_5$if_diff_5yrs <- "N"
data_7_s12_5$interval <- max(data_7_s12_5$terminal_year) - min(data_7_s12_5$terminal_year)

data_7_s12_6 <- data_7_s12[data_7_s12$terminal_year == 2011 | data_7_s12$terminal_year == 2017,]
data_7_s12_6$ty_order <- 6
data_7_s12_6$if_diff_5yrs <- "Y"
data_7_s12_6$interval <- max(data_7_s12_6$terminal_year) - min(data_7_s12_6$terminal_year)

data_7_s12_7 <- data_7_s12[data_7_s12$terminal_year == 2012 | data_7_s12$terminal_year == 2013,]
data_7_s12_7$ty_order <- 7
data_7_s12_7$if_diff_5yrs <- "N"
data_7_s12_7$interval <- max(data_7_s12_7$terminal_year) - min(data_7_s12_7$terminal_year)
data_7_s12_7$ty <- 2012
data_7_s12_7$ty_minus1 <- data_7_s12_7$ty-1
data_7_s12_7$key_ty <- paste(data_7_s12_7$assessid, data_7_s12_7$ty, sep="_")
data_7_s12_7$key_ty_minus1 <- paste(data_7_s12_7$assessid, data_7_s12_7$ty_minus1, sep="_")
data_7_s12_7[,c(46:47,50:61,64:69)] <- NA

data_7_s12_8 <- data_7_s12[data_7_s12$terminal_year == 2012 | data_7_s12$terminal_year == 2014,]
data_7_s12_8$ty_order <- 8
data_7_s12_8$if_diff_5yrs <- "N"
data_7_s12_8$interval <- max(data_7_s12_8$terminal_year) - min(data_7_s12_8$terminal_year)
data_7_s12_8$ty <- 2012
data_7_s12_8$ty_minus1 <- data_7_s12_8$ty-1
data_7_s12_8$key_ty <- paste(data_7_s12_8$assessid, data_7_s12_8$ty, sep="_")
data_7_s12_8$key_ty_minus1 <- paste(data_7_s12_8$assessid, data_7_s12_8$ty_minus1, sep="_")
data_7_s12_8[,c(46:47,50:61,64:69)] <- NA

data_7_s12_9 <- data_7_s12[data_7_s12$terminal_year == 2012 | data_7_s12$terminal_year == 2015,]
data_7_s12_9$ty_order <- 9
data_7_s12_9$if_diff_5yrs <- "N"
data_7_s12_9$interval <- max(data_7_s12_9$terminal_year) - min(data_7_s12_9$terminal_year)
data_7_s12_9$ty <- 2012
data_7_s12_9$ty_minus1 <- data_7_s12_9$ty-1
data_7_s12_9$key_ty <- paste(data_7_s12_9$assessid, data_7_s12_9$ty, sep="_")
data_7_s12_9$key_ty_minus1 <- paste(data_7_s12_9$assessid, data_7_s12_9$ty_minus1, sep="_")
data_7_s12_9[,c(46:47,50:61,64:69)] <- NA

data_7_s12_10 <- data_7_s12[data_7_s12$terminal_year == 2012 | data_7_s12$terminal_year == 2016,]
data_7_s12_10$ty_order <- 10
data_7_s12_10$if_diff_5yrs <- "N"
data_7_s12_10$interval <- max(data_7_s12_10$terminal_year) - min(data_7_s12_10$terminal_year)
data_7_s12_10$ty <- 2012
data_7_s12_10$ty_minus1 <- data_7_s12_10$ty-1
data_7_s12_10$key_ty <- paste(data_7_s12_10$assessid, data_7_s12_10$ty, sep="_")
data_7_s12_10$key_ty_minus1 <- paste(data_7_s12_10$assessid, data_7_s12_10$ty_minus1, sep="_")
data_7_s12_10[,c(46:47,50:61,64:69)] <- NA

data_7_s12_11 <- data_7_s12[data_7_s12$terminal_year == 2012 | data_7_s12$terminal_year == 2017,]
data_7_s12_11$ty_order <- 11
data_7_s12_11$if_diff_5yrs <- "N"
data_7_s12_11$interval <- max(data_7_s12_11$terminal_year) - min(data_7_s12_11$terminal_year)
data_7_s12_11$ty <- 2012
data_7_s12_11$ty_minus1 <- data_7_s12_11$ty-1
data_7_s12_11$key_ty <- paste(data_7_s12_11$assessid, data_7_s12_11$ty, sep="_")
data_7_s12_11$key_ty_minus1 <- paste(data_7_s12_11$assessid, data_7_s12_11$ty_minus1, sep="_")
data_7_s12_11[,c(46:47,50:61,64:69)] <- NA

data_7_s12_12 <- data_7_s12[data_7_s12$terminal_year == 2013 | data_7_s12$terminal_year == 2014,]
data_7_s12_12$ty_order <- 12
data_7_s12_12$if_diff_5yrs <- "N"
data_7_s12_12$interval <- max(data_7_s12_12$terminal_year) - min(data_7_s12_12$terminal_year)
data_7_s12_12$ty <- 2013
data_7_s12_12$ty_minus1 <- data_7_s12_12$ty-1
data_7_s12_12$key_ty <- paste(data_7_s12_12$assessid, data_7_s12_12$ty, sep="_")
data_7_s12_12$key_ty_minus1 <- paste(data_7_s12_12$assessid, data_7_s12_12$ty_minus1, sep="_")
data_7_s12_12[,c(46:47,50:61,64:69)] <- NA

data_7_s12_13 <- data_7_s12[data_7_s12$terminal_year == 2013 | data_7_s12$terminal_year == 2015,]
data_7_s12_13$ty_order <- 13
data_7_s12_13$if_diff_5yrs <- "N"
data_7_s12_13$interval <- max(data_7_s12_13$terminal_year) - min(data_7_s12_13$terminal_year)
data_7_s12_13$ty <- 2013
data_7_s12_13$ty_minus1 <- data_7_s12_13$ty-1
data_7_s12_13$key_ty <- paste(data_7_s12_13$assessid, data_7_s12_13$ty, sep="_")
data_7_s12_13$key_ty_minus1 <- paste(data_7_s12_13$assessid, data_7_s12_13$ty_minus1, sep="_")
data_7_s12_13[,c(46:47,50:61,64:69)] <- NA

data_7_s12_14 <- data_7_s12[data_7_s12$terminal_year == 2013 | data_7_s12$terminal_year == 2016,]
data_7_s12_14$ty_order <- 14
data_7_s12_14$if_diff_5yrs <- "N"
data_7_s12_14$interval <- max(data_7_s12_14$terminal_year) - min(data_7_s12_14$terminal_year)
data_7_s12_14$ty <- 2013
data_7_s12_14$ty_minus1 <- data_7_s12_14$ty-1
data_7_s12_14$key_ty <- paste(data_7_s12_14$assessid, data_7_s12_14$ty, sep="_")
data_7_s12_14$key_ty_minus1 <- paste(data_7_s12_14$assessid, data_7_s12_14$ty_minus1, sep="_")
data_7_s12_14[,c(46:47,50:61,64:69)] <- NA

data_7_s12_15 <- data_7_s12[data_7_s12$terminal_year == 2013 | data_7_s12$terminal_year == 2017,]
data_7_s12_15$ty_order <- 15
data_7_s12_15$if_diff_5yrs <- "N"
data_7_s12_15$interval <- max(data_7_s12_15$terminal_year) - min(data_7_s12_15$terminal_year)
data_7_s12_15$ty <- 2013
data_7_s12_15$ty_minus1 <- data_7_s12_15$ty-1
data_7_s12_15$key_ty <- paste(data_7_s12_15$assessid, data_7_s12_15$ty, sep="_")
data_7_s12_15$key_ty_minus1 <- paste(data_7_s12_15$assessid, data_7_s12_15$ty_minus1, sep="_")
data_7_s12_15[,c(46:47,50:61,64:69)] <- NA

data_7_s12_16 <- data_7_s12[data_7_s12$terminal_year == 2014 | data_7_s12$terminal_year == 2015,]
data_7_s12_16$ty_order <- 16
data_7_s12_16$if_diff_5yrs <- "N"
data_7_s12_16$interval <- max(data_7_s12_16$terminal_year) - min(data_7_s12_16$terminal_year)
data_7_s12_16$ty <- 2014
data_7_s12_16$ty_minus1 <- data_7_s12_16$ty-1
data_7_s12_16$key_ty <- paste(data_7_s12_16$assessid, data_7_s12_16$ty, sep="_")
data_7_s12_16$key_ty_minus1 <- paste(data_7_s12_16$assessid, data_7_s12_16$ty_minus1, sep="_")
data_7_s12_16[,c(46:47,50:61,64:69)] <- NA

data_7_s12_17 <- data_7_s12[data_7_s12$terminal_year == 2014 | data_7_s12$terminal_year == 2016,]
data_7_s12_17$ty_order <- 17
data_7_s12_17$if_diff_5yrs <- "N"
data_7_s12_17$interval <- max(data_7_s12_17$terminal_year) - min(data_7_s12_17$terminal_year)
data_7_s12_17$ty <- 2014
data_7_s12_17$ty_minus1 <- data_7_s12_17$ty-1
data_7_s12_17$key_ty <- paste(data_7_s12_17$assessid, data_7_s12_17$ty, sep="_")
data_7_s12_17$key_ty_minus1 <- paste(data_7_s12_17$assessid, data_7_s12_17$ty_minus1, sep="_")
data_7_s12_17[,c(46:47,50:61,64:69)] <- NA

data_7_s12_18 <- data_7_s12[data_7_s12$terminal_year == 2014 | data_7_s12$terminal_year == 2017,]
data_7_s12_18$ty_order <- 18
data_7_s12_18$if_diff_5yrs <- "N"
data_7_s12_18$interval <- max(data_7_s12_18$terminal_year) - min(data_7_s12_18$terminal_year)
data_7_s12_18$ty <- 2014
data_7_s12_18$ty_minus1 <- data_7_s12_18$ty-1
data_7_s12_18$key_ty <- paste(data_7_s12_18$assessid, data_7_s12_18$ty, sep="_")
data_7_s12_18$key_ty_minus1 <- paste(data_7_s12_18$assessid, data_7_s12_18$ty_minus1, sep="_")
data_7_s12_18[,c(46:47,50:61,64:69)] <- NA

data_7_s12_19 <- data_7_s12[data_7_s12$terminal_year == 2015 | data_7_s12$terminal_year == 2016,]
data_7_s12_19$ty_order <- 19
data_7_s12_19$if_diff_5yrs <- "N"
data_7_s12_19$interval <- max(data_7_s12_19$terminal_year) - min(data_7_s12_19$terminal_year)
data_7_s12_19$ty <- 2015
data_7_s12_19$ty_minus1 <- data_7_s12_19$ty-1
data_7_s12_19$key_ty <- paste(data_7_s12_19$assessid, data_7_s12_19$ty, sep="_")
data_7_s12_19$key_ty_minus1 <- paste(data_7_s12_19$assessid, data_7_s12_19$ty_minus1, sep="_")
data_7_s12_19[,c(46:47,50:61,64:69)] <- NA

data_7_s12_20 <- data_7_s12[data_7_s12$terminal_year == 2015 | data_7_s12$terminal_year == 2017,]
data_7_s12_20$ty_order <- 20
data_7_s12_20$if_diff_5yrs <- "N"
data_7_s12_20$interval <- max(data_7_s12_20$terminal_year) - min(data_7_s12_20$terminal_year)
data_7_s12_20$ty <- 2015
data_7_s12_20$ty_minus1 <- data_7_s12_20$ty-1
data_7_s12_20$key_ty <- paste(data_7_s12_20$assessid, data_7_s12_20$ty, sep="_")
data_7_s12_20$key_ty_minus1 <- paste(data_7_s12_20$assessid, data_7_s12_20$ty_minus1, sep="_")
data_7_s12_20[,c(46:47,50:61,64:69)] <- NA

data_7_s12_21 <- data_7_s12[data_7_s12$terminal_year == 2016 | data_7_s12$terminal_year == 2017,]
data_7_s12_21$ty_order <- 21
data_7_s12_21$if_diff_5yrs <- "N"
data_7_s12_21$interval <- max(data_7_s12_21$terminal_year) - min(data_7_s12_21$terminal_year)
data_7_s12_21$ty <- 2016
data_7_s12_21$ty_minus1 <- data_7_s12_21$ty-1
data_7_s12_21$key_ty <- paste(data_7_s12_21$assessid, data_7_s12_21$ty, sep="_")
data_7_s12_21$key_ty_minus1 <- paste(data_7_s12_21$assessid, data_7_s12_21$ty_minus1, sep="_")
data_7_s12_21[,c(46:47,50:61,64:69)] <- NA

data_7_s12 <- rbind(data_7_s12_1, data_7_s12_2, data_7_s12_3, data_7_s12_4, data_7_s12_5, data_7_s12_6, data_7_s12_7, data_7_s12_8, data_7_s12_9, data_7_s12_10,
                    data_7_s12_11, data_7_s12_12, data_7_s12_13, data_7_s12_14, data_7_s12_15, data_7_s12_16, data_7_s12_17, data_7_s12_18, data_7_s12_19,
                    data_7_s12_20, data_7_s12_21)

# assess 13
data_7_s13 <- data_7_assess[data_7_assess$stockid==ids[13],]

data_7_s13_1 <- data_7_s13[data_7_s13$terminal_year == 2011 | data_7_s13$terminal_year == 2012,]
data_7_s13_1$ty_order <- 1
data_7_s13_1$if_diff_5yrs <- "N"

data_7_s13_2 <- data_7_s13[data_7_s13$terminal_year == 2011 | data_7_s13$terminal_year == 2013,]
data_7_s13_2$ty_order <- 2
data_7_s13_2$if_diff_5yrs <- "N"

data_7_s13_3 <- data_7_s13[data_7_s13$terminal_year == 2011 | data_7_s13$terminal_year == 2014,]
data_7_s13_3$ty_order <- 3
data_7_s13_3$if_diff_5yrs <- "N"

data_7_s13_4 <- data_7_s13[data_7_s13$terminal_year == 2011 | data_7_s13$terminal_year == 2015,]
data_7_s13_4$ty_order <- 4
data_7_s13_4$if_diff_5yrs <- "N"

data_7_s13_5 <- data_7_s13[data_7_s13$terminal_year == 2011 | data_7_s13$terminal_year == 2016,]
data_7_s13_5$ty_order <- 5
data_7_s13_5$if_diff_5yrs <- "N"

data_7_s13_6 <- data_7_s13[data_7_s13$terminal_year == 2011 | data_7_s13$terminal_year == 2017,]
data_7_s13_6$ty_order <- 6
data_7_s13_6$if_diff_5yrs <- "Y"

data_7_s13_7 <- data_7_s13[data_7_s13$terminal_year == 2012 | data_7_s13$terminal_year == 2013,]
data_7_s13_7$ty_order <- 7
data_7_s13_7$if_diff_5yrs <- "N"
data_7_s13_7$ty <- 2012
data_7_s13_7$ty_minus1 <- data_7_s13_7$ty-1
data_7_s13_7$key_ty <- paste(data_7_s13_7$assessid, data_7_s13_7$ty, sep="_")
data_7_s13_7$key_ty_minus1 <- paste(data_7_s13_7$assessid, data_7_s13_7$ty_minus1, sep="_")
data_7_s13_7[,c(46:47,50:61,64:69)] <- NA

data_7_s13_8 <- data_7_s13[data_7_s13$terminal_year == 2012 | data_7_s13$terminal_year == 2014,]
data_7_s13_8$ty_order <- 8
data_7_s13_8$if_diff_5yrs <- "N"
data_7_s13_8$ty <- 2012
data_7_s13_8$ty_minus1 <- data_7_s13_8$ty-1
data_7_s13_8$key_ty <- paste(data_7_s13_8$assessid, data_7_s13_8$ty, sep="_")
data_7_s13_8$key_ty_minus1 <- paste(data_7_s13_8$assessid, data_7_s13_8$ty_minus1, sep="_")
data_7_s13_8[,c(46:47,50:61,64:69)] <- NA

data_7_s13_9 <- data_7_s13[data_7_s13$terminal_year == 2012 | data_7_s13$terminal_year == 2015,]
data_7_s13_9$ty_order <- 9
data_7_s13_9$if_diff_5yrs <- "N"
data_7_s13_9$ty <- 2012
data_7_s13_9$ty_minus1 <- data_7_s13_9$ty-1
data_7_s13_9$key_ty <- paste(data_7_s13_9$assessid, data_7_s13_9$ty, sep="_")
data_7_s13_9$key_ty_minus1 <- paste(data_7_s13_9$assessid, data_7_s13_9$ty_minus1, sep="_")
data_7_s13_9[,c(46:47,50:61,64:69)] <- NA

data_7_s13_10 <- data_7_s13[data_7_s13$terminal_year == 2012 | data_7_s13$terminal_year == 2016,]
data_7_s13_10$ty_order <- 10
data_7_s13_10$if_diff_5yrs <- "N"
data_7_s13_10$ty <- 2012
data_7_s13_10$ty_minus1 <- data_7_s13_10$ty-1
data_7_s13_10$key_ty <- paste(data_7_s13_10$assessid, data_7_s13_10$ty, sep="_")
data_7_s13_10$key_ty_minus1 <- paste(data_7_s13_10$assessid, data_7_s13_10$ty_minus1, sep="_")
data_7_s13_10[,c(46:47,50:61,64:69)] <- NA

data_7_s13_11 <- data_7_s13[data_7_s13$terminal_year == 2012 | data_7_s13$terminal_year == 2017,]
data_7_s13_11$ty_order <- 11
data_7_s13_11$if_diff_5yrs <- "N"
data_7_s13_11$ty <- 2012
data_7_s13_11$ty_minus1 <- data_7_s13_11$ty-1
data_7_s13_11$key_ty <- paste(data_7_s13_11$assessid, data_7_s13_11$ty, sep="_")
data_7_s13_11$key_ty_minus1 <- paste(data_7_s13_11$assessid, data_7_s13_11$ty_minus1, sep="_")
data_7_s13_11[,c(46:47,50:61,64:69)] <- NA

data_7_s13_12 <- data_7_s13[data_7_s13$terminal_year == 2013 | data_7_s13$terminal_year == 2014,]
data_7_s13_12$ty_order <- 12
data_7_s13_12$if_diff_5yrs <- "N"
data_7_s13_12$ty <- 2013
data_7_s13_12$ty_minus1 <- data_7_s13_12$ty-1
data_7_s13_12$key_ty <- paste(data_7_s13_12$assessid, data_7_s13_12$ty, sep="_")
data_7_s13_12$key_ty_minus1 <- paste(data_7_s13_12$assessid, data_7_s13_12$ty_minus1, sep="_")
data_7_s13_12[,c(46:47,50:61,64:69)] <- NA

data_7_s13_13 <- data_7_s13[data_7_s13$terminal_year == 2013 | data_7_s13$terminal_year == 2015,]
data_7_s13_13$ty_order <- 13
data_7_s13_13$if_diff_5yrs <- "N"
data_7_s13_13$ty <- 2013
data_7_s13_13$ty_minus1 <- data_7_s13_13$ty-1
data_7_s13_13$key_ty <- paste(data_7_s13_13$assessid, data_7_s13_13$ty, sep="_")
data_7_s13_13$key_ty_minus1 <- paste(data_7_s13_13$assessid, data_7_s13_13$ty_minus1, sep="_")
data_7_s13_13[,c(46:47,50:61,64:69)] <- NA

data_7_s13_14 <- data_7_s13[data_7_s13$terminal_year == 2013 | data_7_s13$terminal_year == 2016,]
data_7_s13_14$ty_order <- 14
data_7_s13_14$if_diff_5yrs <- "N"
data_7_s13_14$ty <- 2013
data_7_s13_14$ty_minus1 <- data_7_s13_14$ty-1
data_7_s13_14$key_ty <- paste(data_7_s13_14$assessid, data_7_s13_14$ty, sep="_")
data_7_s13_14$key_ty_minus1 <- paste(data_7_s13_14$assessid, data_7_s13_14$ty_minus1, sep="_")
data_7_s13_14[,c(46:47,50:61,64:69)] <- NA

data_7_s13_15 <- data_7_s13[data_7_s13$terminal_year == 2013 | data_7_s13$terminal_year == 2017,]
data_7_s13_15$ty_order <- 15
data_7_s13_15$if_diff_5yrs <- "N"
data_7_s13_15$ty <- 2013
data_7_s13_15$ty_minus1 <- data_7_s13_15$ty-1
data_7_s13_15$key_ty <- paste(data_7_s13_15$assessid, data_7_s13_15$ty, sep="_")
data_7_s13_15$key_ty_minus1 <- paste(data_7_s13_15$assessid, data_7_s13_15$ty_minus1, sep="_")
data_7_s13_15[,c(46:47,50:61,64:69)] <- NA

data_7_s13_16 <- data_7_s13[data_7_s13$terminal_year == 2014 | data_7_s13$terminal_year == 2015,]
data_7_s13_16$ty_order <- 16
data_7_s13_16$if_diff_5yrs <- "N"
data_7_s13_16$ty <- 2014
data_7_s13_16$ty_minus1 <- data_7_s13_16$ty-1
data_7_s13_16$key_ty <- paste(data_7_s13_16$assessid, data_7_s13_16$ty, sep="_")
data_7_s13_16$key_ty_minus1 <- paste(data_7_s13_16$assessid, data_7_s13_16$ty_minus1, sep="_")
data_7_s13_16[,c(46:47,50:61,64:69)] <- NA

data_7_s13_17 <- data_7_s13[data_7_s13$terminal_year == 2014 | data_7_s13$terminal_year == 2016,]
data_7_s13_17$ty_order <- 17
data_7_s13_17$if_diff_5yrs <- "N"
data_7_s13_17$ty <- 2014
data_7_s13_17$ty_minus1 <- data_7_s13_17$ty-1
data_7_s13_17$key_ty <- paste(data_7_s13_17$assessid, data_7_s13_17$ty, sep="_")
data_7_s13_17$key_ty_minus1 <- paste(data_7_s13_17$assessid, data_7_s13_17$ty_minus1, sep="_")
data_7_s13_17[,c(46:47,50:61,64:69)] <- NA

data_7_s13_18 <- data_7_s13[data_7_s13$terminal_year == 2014 | data_7_s13$terminal_year == 2017,]
data_7_s13_18$ty_order <- 18
data_7_s13_18$if_diff_5yrs <- "N"
data_7_s13_18$ty <- 2014
data_7_s13_18$ty_minus1 <- data_7_s13_18$ty-1
data_7_s13_18$key_ty <- paste(data_7_s13_18$assessid, data_7_s13_18$ty, sep="_")
data_7_s13_18$key_ty_minus1 <- paste(data_7_s13_18$assessid, data_7_s13_18$ty_minus1, sep="_")
data_7_s13_18[,c(46:47,50:61,64:69)] <- NA

data_7_s13_19 <- data_7_s13[data_7_s13$terminal_year == 2015 | data_7_s13$terminal_year == 2016,]
data_7_s13_19$ty_order <- 19
data_7_s13_19$if_diff_5yrs <- "N"
data_7_s13_19$ty <- 2015
data_7_s13_19$ty_minus1 <- data_7_s13_19$ty-1
data_7_s13_19$key_ty <- paste(data_7_s13_19$assessid, data_7_s13_19$ty, sep="_")
data_7_s13_19$key_ty_minus1 <- paste(data_7_s13_19$assessid, data_7_s13_19$ty_minus1, sep="_")
data_7_s13_19[,c(46:47,50:61,64:69)] <- NA

data_7_s13_20 <- data_7_s13[data_7_s13$terminal_year == 2015 | data_7_s13$terminal_year == 2017,]
data_7_s13_20$ty_order <- 20
data_7_s13_20$if_diff_5yrs <- "N"
data_7_s13_20$ty <- 2015
data_7_s13_20$ty_minus1 <- data_7_s13_20$ty-1
data_7_s13_20$key_ty <- paste(data_7_s13_20$assessid, data_7_s13_20$ty, sep="_")
data_7_s13_20$key_ty_minus1 <- paste(data_7_s13_20$assessid, data_7_s13_20$ty_minus1, sep="_")
data_7_s13_20[,c(46:47,50:61,64:69)] <- NA

data_7_s13_21 <- data_7_s13[data_7_s13$terminal_year == 2016 | data_7_s13$terminal_year == 2017,]
data_7_s13_21$ty_order <- 21
data_7_s13_21$if_diff_5yrs <- "N"
data_7_s13_21$ty <- 2016
data_7_s13_21$ty_minus1 <- data_7_s13_21$ty-1
data_7_s13_21$key_ty <- paste(data_7_s13_21$assessid, data_7_s13_21$ty, sep="_")
data_7_s13_21$key_ty_minus1 <- paste(data_7_s13_21$assessid, data_7_s13_21$ty_minus1, sep="_")
data_7_s13_21[,c(46:47,50:61,64:69)] <- NA

data_7_s13_1$interval <- max(data_7_s13_1$terminal_year) - min(data_7_s13_1$terminal_year)
data_7_s13_2$interval <- max(data_7_s13_2$terminal_year) - min(data_7_s13_2$terminal_year)
data_7_s13_3$interval <- max(data_7_s13_3$terminal_year) - min(data_7_s13_3$terminal_year)
data_7_s13_4$interval <- max(data_7_s13_4$terminal_year) - min(data_7_s13_4$terminal_year)
data_7_s13_5$interval <- max(data_7_s13_5$terminal_year) - min(data_7_s13_5$terminal_year)
data_7_s13_6$interval <- max(data_7_s13_6$terminal_year) - min(data_7_s13_6$terminal_year)
data_7_s13_7$interval <- max(data_7_s13_7$terminal_year) - min(data_7_s13_7$terminal_year)
data_7_s13_8$interval <- max(data_7_s13_8$terminal_year) - min(data_7_s13_8$terminal_year)
data_7_s13_9$interval <- max(data_7_s13_9$terminal_year) - min(data_7_s13_9$terminal_year)
data_7_s13_10$interval <- max(data_7_s13_10$terminal_year) - min(data_7_s13_10$terminal_year)
data_7_s13_11$interval <- max(data_7_s13_11$terminal_year) - min(data_7_s13_11$terminal_year)
data_7_s13_12$interval <- max(data_7_s13_12$terminal_year) - min(data_7_s13_12$terminal_year)
data_7_s13_13$interval <- max(data_7_s13_13$terminal_year) - min(data_7_s13_13$terminal_year)
data_7_s13_14$interval <- max(data_7_s13_14$terminal_year) - min(data_7_s13_14$terminal_year)
data_7_s13_15$interval <- max(data_7_s13_15$terminal_year) - min(data_7_s13_15$terminal_year)
data_7_s13_16$interval <- max(data_7_s13_16$terminal_year) - min(data_7_s13_16$terminal_year)
data_7_s13_17$interval <- max(data_7_s13_17$terminal_year) - min(data_7_s13_17$terminal_year)
data_7_s13_18$interval <- max(data_7_s13_18$terminal_year) - min(data_7_s13_18$terminal_year)
data_7_s13_19$interval <- max(data_7_s13_19$terminal_year) - min(data_7_s13_19$terminal_year)
data_7_s13_20$interval <- max(data_7_s13_20$terminal_year) - min(data_7_s13_20$terminal_year)
data_7_s13_21$interval <- max(data_7_s13_21$terminal_year) - min(data_7_s13_21$terminal_year)

data_7_s13 <- rbind(data_7_s13_1, data_7_s13_2, data_7_s13_3, data_7_s13_4, data_7_s13_5, data_7_s13_6, data_7_s13_7, data_7_s13_8, data_7_s13_9, data_7_s13_10,
                    data_7_s13_11, data_7_s13_12, data_7_s13_13, data_7_s13_14, data_7_s13_15, data_7_s13_16, data_7_s13_17, data_7_s13_18, data_7_s13_19,
                    data_7_s13_20, data_7_s13_21)

# assess 14
data_7_s14 <- data_7_assess[data_7_assess$stockid==ids[14],]

data_7_s14_1 <- data_7_s14[data_7_s14$terminal_year == 2010 | data_7_s14$terminal_year == 2012,]
data_7_s14_1$ty_order <- 1
data_7_s14_1$if_diff_5yrs <- "N"

data_7_s14_2 <- data_7_s14[data_7_s14$terminal_year == 2010 | data_7_s14$terminal_year == 2013,]
data_7_s14_2$ty_order <- 2
data_7_s14_2$if_diff_5yrs <- "N"

data_7_s14_3 <- data_7_s14[data_7_s14$terminal_year == 2010 | data_7_s14$terminal_year == 2014,]
data_7_s14_3$ty_order <- 3
data_7_s14_3$if_diff_5yrs <- "N"

data_7_s14_4 <- data_7_s14[data_7_s14$terminal_year == 2010 | data_7_s14$terminal_year == 2015,]
data_7_s14_4$ty_order <- 4
data_7_s14_4$if_diff_5yrs <- "N"

data_7_s14_5 <- data_7_s14[data_7_s14$terminal_year == 2010 | data_7_s14$terminal_year == 2016,]
data_7_s14_5$ty_order <- 5
data_7_s14_5$if_diff_5yrs <- "Y"

data_7_s14_6 <- data_7_s14[data_7_s14$terminal_year == 2010 | data_7_s14$terminal_year == 2017,]
data_7_s14_6$ty_order <- 6
data_7_s14_6$if_diff_5yrs <- "Y"

data_7_s14_7 <- data_7_s14[data_7_s14$terminal_year == 2012 | data_7_s14$terminal_year == 2013,]
data_7_s14_7$ty_order <- 7
data_7_s14_7$if_diff_5yrs <- "N"
data_7_s14_7$ty <- 2012
data_7_s14_7$ty_minus1 <- data_7_s14_7$ty-1
data_7_s14_7$key_ty <- paste(data_7_s14_7$assessid, data_7_s14_7$ty, sep="_")
data_7_s14_7$key_ty_minus1 <- paste(data_7_s14_7$assessid, data_7_s14_7$ty_minus1, sep="_")
data_7_s14_7[,c(46:47,50:61,64:69)] <- NA

data_7_s14_8 <- data_7_s14[data_7_s14$terminal_year == 2012 | data_7_s14$terminal_year == 2014,]
data_7_s14_8$ty_order <- 8
data_7_s14_8$if_diff_5yrs <- "N"
data_7_s14_8$ty <- 2012
data_7_s14_8$ty_minus1 <- data_7_s14_8$ty-1
data_7_s14_8$key_ty <- paste(data_7_s14_8$assessid, data_7_s14_8$ty, sep="_")
data_7_s14_8$key_ty_minus1 <- paste(data_7_s14_8$assessid, data_7_s14_8$ty_minus1, sep="_")
data_7_s14_8[,c(46:47,50:61,64:69)] <- NA

data_7_s14_9 <- data_7_s14[data_7_s14$terminal_year == 2012 | data_7_s14$terminal_year == 2015,]
data_7_s14_9$ty_order <- 9
data_7_s14_9$if_diff_5yrs <- "N"
data_7_s14_9$ty <- 2012
data_7_s14_9$ty_minus1 <- data_7_s14_9$ty-1
data_7_s14_9$key_ty <- paste(data_7_s14_9$assessid, data_7_s14_9$ty, sep="_")
data_7_s14_9$key_ty_minus1 <- paste(data_7_s14_9$assessid, data_7_s14_9$ty_minus1, sep="_")
data_7_s14_9[,c(46:47,50:61,64:69)] <- NA

data_7_s14_10 <- data_7_s14[data_7_s14$terminal_year == 2012 | data_7_s14$terminal_year == 2016,]
data_7_s14_10$ty_order <- 10
data_7_s14_10$if_diff_5yrs <- "N"
data_7_s14_10$ty <- 2012
data_7_s14_10$ty_minus1 <- data_7_s14_10$ty-1
data_7_s14_10$key_ty <- paste(data_7_s14_10$assessid, data_7_s14_10$ty, sep="_")
data_7_s14_10$key_ty_minus1 <- paste(data_7_s14_10$assessid, data_7_s14_10$ty_minus1, sep="_")
data_7_s14_10[,c(46:47,50:61,64:69)] <- NA

data_7_s14_11 <- data_7_s14[data_7_s14$terminal_year == 2012 | data_7_s14$terminal_year == 2017,]
data_7_s14_11$ty_order <- 11
data_7_s14_11$if_diff_5yrs <- "N"
data_7_s14_11$ty <- 2012
data_7_s14_11$ty_minus1 <- data_7_s14_11$ty-1
data_7_s14_11$key_ty <- paste(data_7_s14_11$assessid, data_7_s14_11$ty, sep="_")
data_7_s14_11$key_ty_minus1 <- paste(data_7_s14_11$assessid, data_7_s14_11$ty_minus1, sep="_")
data_7_s14_11[,c(46:47,50:61,64:69)] <- NA

data_7_s14_12 <- data_7_s14[data_7_s14$terminal_year == 2013 | data_7_s14$terminal_year == 2014,]
data_7_s14_12$ty_order <- 12
data_7_s14_12$if_diff_5yrs <- "N"
data_7_s14_12$ty <- 2013
data_7_s14_12$ty_minus1 <- data_7_s14_12$ty-1
data_7_s14_12$key_ty <- paste(data_7_s14_12$assessid, data_7_s14_12$ty, sep="_")
data_7_s14_12$key_ty_minus1 <- paste(data_7_s14_12$assessid, data_7_s14_12$ty_minus1, sep="_")
data_7_s14_12[,c(46:47,50:61,64:69)] <- NA

data_7_s14_13 <- data_7_s14[data_7_s14$terminal_year == 2013 | data_7_s14$terminal_year == 2015,]
data_7_s14_13$ty_order <- 13
data_7_s14_13$if_diff_5yrs <- "N"
data_7_s14_13$ty <- 2013
data_7_s14_13$ty_minus1 <- data_7_s14_13$ty-1
data_7_s14_13$key_ty <- paste(data_7_s14_13$assessid, data_7_s14_13$ty, sep="_")
data_7_s14_13$key_ty_minus1 <- paste(data_7_s14_13$assessid, data_7_s14_13$ty_minus1, sep="_")
data_7_s14_13[,c(46:47,50:61,64:69)] <- NA

data_7_s14_14 <- data_7_s14[data_7_s14$terminal_year == 2013 | data_7_s14$terminal_year == 2016,]
data_7_s14_14$ty_order <- 14
data_7_s14_14$if_diff_5yrs <- "N"
data_7_s14_14$ty <- 2013
data_7_s14_14$ty_minus1 <- data_7_s14_14$ty-1
data_7_s14_14$key_ty <- paste(data_7_s14_14$assessid, data_7_s14_14$ty, sep="_")
data_7_s14_14$key_ty_minus1 <- paste(data_7_s14_14$assessid, data_7_s14_14$ty_minus1, sep="_")
data_7_s14_14[,c(46:47,50:61,64:69)] <- NA

data_7_s14_15 <- data_7_s14[data_7_s14$terminal_year == 2013 | data_7_s14$terminal_year == 2017,]
data_7_s14_15$ty_order <- 15
data_7_s14_15$if_diff_5yrs <- "N"
data_7_s14_15$ty <- 2013
data_7_s14_15$ty_minus1 <- data_7_s14_15$ty-1
data_7_s14_15$key_ty <- paste(data_7_s14_15$assessid, data_7_s14_15$ty, sep="_")
data_7_s14_15$key_ty_minus1 <- paste(data_7_s14_15$assessid, data_7_s14_15$ty_minus1, sep="_")
data_7_s14_15[,c(46:47,50:61,64:69)] <- NA

data_7_s14_16 <- data_7_s14[data_7_s14$terminal_year == 2014 | data_7_s14$terminal_year == 2015,]
data_7_s14_16$ty_order <- 16
data_7_s14_16$if_diff_5yrs <- "N"
data_7_s14_16$ty <- 2014
data_7_s14_16$ty_minus1 <- data_7_s14_16$ty-1
data_7_s14_16$key_ty <- paste(data_7_s14_16$assessid, data_7_s14_16$ty, sep="_")
data_7_s14_16$key_ty_minus1 <- paste(data_7_s14_16$assessid, data_7_s14_16$ty_minus1, sep="_")
data_7_s14_16[,c(46:47,50:61,64:69)] <- NA

data_7_s14_17 <- data_7_s14[data_7_s14$terminal_year == 2014 | data_7_s14$terminal_year == 2016,]
data_7_s14_17$ty_order <- 17
data_7_s14_17$if_diff_5yrs <- "N"
data_7_s14_17$ty <- 2014
data_7_s14_17$ty_minus1 <- data_7_s14_17$ty-1
data_7_s14_17$key_ty <- paste(data_7_s14_17$assessid, data_7_s14_17$ty, sep="_")
data_7_s14_17$key_ty_minus1 <- paste(data_7_s14_17$assessid, data_7_s14_17$ty_minus1, sep="_")
data_7_s14_17[,c(46:47,50:61,64:69)] <- NA

data_7_s14_18 <- data_7_s14[data_7_s14$terminal_year == 2014 | data_7_s14$terminal_year == 2017,]
data_7_s14_18$ty_order <- 18
data_7_s14_18$if_diff_5yrs <- "N"
data_7_s14_18$ty <- 2014
data_7_s14_18$ty_minus1 <- data_7_s14_18$ty-1
data_7_s14_18$key_ty <- paste(data_7_s14_18$assessid, data_7_s14_18$ty, sep="_")
data_7_s14_18$key_ty_minus1 <- paste(data_7_s14_18$assessid, data_7_s14_18$ty_minus1, sep="_")
data_7_s14_18[,c(46:47,50:61,64:69)] <- NA

data_7_s14_19 <- data_7_s14[data_7_s14$terminal_year == 2015 | data_7_s14$terminal_year == 2016,]
data_7_s14_19$ty_order <- 19
data_7_s14_19$if_diff_5yrs <- "N"
data_7_s14_19$ty <- 2015
data_7_s14_19$ty_minus1 <- data_7_s14_19$ty-1
data_7_s14_19$key_ty <- paste(data_7_s14_19$assessid, data_7_s14_19$ty, sep="_")
data_7_s14_19$key_ty_minus1 <- paste(data_7_s14_19$assessid, data_7_s14_19$ty_minus1, sep="_")
data_7_s14_19[,c(46:47,50:61,64:69)] <- NA

data_7_s14_20 <- data_7_s14[data_7_s14$terminal_year == 2015 | data_7_s14$terminal_year == 2017,]
data_7_s14_20$ty_order <- 20
data_7_s14_20$if_diff_5yrs <- "N"
data_7_s14_20$ty <- 2015
data_7_s14_20$ty_minus1 <- data_7_s14_20$ty-1
data_7_s14_20$key_ty <- paste(data_7_s14_20$assessid, data_7_s14_20$ty, sep="_")
data_7_s14_20$key_ty_minus1 <- paste(data_7_s14_20$assessid, data_7_s14_20$ty_minus1, sep="_")
data_7_s14_20[,c(46:47,50:61,64:69)] <- NA

data_7_s14_21 <- data_7_s14[data_7_s14$terminal_year == 2016 | data_7_s14$terminal_year == 2017,]
data_7_s14_21$ty_order <- 21
data_7_s14_21$if_diff_5yrs <- "N"
data_7_s14_21$ty <- 2016
data_7_s14_21$ty_minus1 <- data_7_s14_21$ty-1
data_7_s14_21$key_ty <- paste(data_7_s14_21$assessid, data_7_s14_21$ty, sep="_")
data_7_s14_21$key_ty_minus1 <- paste(data_7_s14_21$assessid, data_7_s14_21$ty_minus1, sep="_")
data_7_s14_21[,c(46:47,50:61,64:69)] <- NA

data_7_s14_1$interval <- max(data_7_s14_1$terminal_year) - min(data_7_s14_1$terminal_year)
data_7_s14_2$interval <- max(data_7_s14_2$terminal_year) - min(data_7_s14_2$terminal_year)
data_7_s14_3$interval <- max(data_7_s14_3$terminal_year) - min(data_7_s14_3$terminal_year)
data_7_s14_4$interval <- max(data_7_s14_4$terminal_year) - min(data_7_s14_4$terminal_year)
data_7_s14_5$interval <- max(data_7_s14_5$terminal_year) - min(data_7_s14_5$terminal_year)
data_7_s14_6$interval <- max(data_7_s14_6$terminal_year) - min(data_7_s14_6$terminal_year)
data_7_s14_7$interval <- max(data_7_s14_7$terminal_year) - min(data_7_s14_7$terminal_year)
data_7_s14_8$interval <- max(data_7_s14_8$terminal_year) - min(data_7_s14_8$terminal_year)
data_7_s14_9$interval <- max(data_7_s14_9$terminal_year) - min(data_7_s14_9$terminal_year)
data_7_s14_10$interval <- max(data_7_s14_10$terminal_year) - min(data_7_s14_10$terminal_year)
data_7_s14_11$interval <- max(data_7_s14_11$terminal_year) - min(data_7_s14_11$terminal_year)
data_7_s14_12$interval <- max(data_7_s14_12$terminal_year) - min(data_7_s14_12$terminal_year)
data_7_s14_13$interval <- max(data_7_s14_13$terminal_year) - min(data_7_s14_13$terminal_year)
data_7_s14_14$interval <- max(data_7_s14_14$terminal_year) - min(data_7_s14_14$terminal_year)
data_7_s14_15$interval <- max(data_7_s14_15$terminal_year) - min(data_7_s14_15$terminal_year)
data_7_s14_16$interval <- max(data_7_s14_16$terminal_year) - min(data_7_s14_16$terminal_year)
data_7_s14_17$interval <- max(data_7_s14_17$terminal_year) - min(data_7_s14_17$terminal_year)
data_7_s14_18$interval <- max(data_7_s14_18$terminal_year) - min(data_7_s14_18$terminal_year)
data_7_s14_19$interval <- max(data_7_s14_19$terminal_year) - min(data_7_s14_19$terminal_year)
data_7_s14_20$interval <- max(data_7_s14_20$terminal_year) - min(data_7_s14_20$terminal_year)
data_7_s14_21$interval <- max(data_7_s14_21$terminal_year) - min(data_7_s14_21$terminal_year)

data_7_s14 <- rbind(data_7_s14_1, data_7_s14_2, data_7_s14_3, data_7_s14_4, data_7_s14_5, data_7_s14_6, data_7_s14_7, data_7_s14_8, data_7_s14_9, data_7_s14_10,
                    data_7_s14_11, data_7_s14_12, data_7_s14_13, data_7_s14_14, data_7_s14_15, data_7_s14_16, data_7_s14_17, data_7_s14_18, data_7_s14_19,
                    data_7_s14_20, data_7_s14_21)

# assess 15
data_7_s15 <- data_7_assess[data_7_assess$stockid==ids[15],]

data_7_s15_1 <- data_7_s15[data_7_s15$terminal_year == 2011 | data_7_s15$terminal_year == 2012,]
data_7_s15_1$ty_order <- 1
data_7_s15_1$if_diff_5yrs <- "N"

data_7_s15_2 <- data_7_s15[data_7_s15$terminal_year == 2011 | data_7_s15$terminal_year == 2013,]
data_7_s15_2$ty_order <- 2
data_7_s15_2$if_diff_5yrs <- "N"

data_7_s15_3 <- data_7_s15[data_7_s15$terminal_year == 2011 | data_7_s15$terminal_year == 2014,]
data_7_s15_3$ty_order <- 3
data_7_s15_3$if_diff_5yrs <- "N"

data_7_s15_4 <- data_7_s15[data_7_s15$terminal_year == 2011 | data_7_s15$terminal_year == 2015,]
data_7_s15_4$ty_order <- 4
data_7_s15_4$if_diff_5yrs <- "N"

data_7_s15_5 <- data_7_s15[data_7_s15$terminal_year == 2011 | data_7_s15$terminal_year == 2016,]
data_7_s15_5$ty_order <- 5
data_7_s15_5$if_diff_5yrs <- "N"

data_7_s15_6 <- data_7_s15[data_7_s15$terminal_year == 2011 | data_7_s15$terminal_year == 2017,]
data_7_s15_6$ty_order <- 6
data_7_s15_6$if_diff_5yrs <- "Y"

data_7_s15_7 <- data_7_s15[data_7_s15$terminal_year == 2012 | data_7_s15$terminal_year == 2013,]
data_7_s15_7$ty_order <- 7
data_7_s15_7$if_diff_5yrs <- "N"
data_7_s15_7$ty <- 2012
data_7_s15_7$ty_minus1 <- data_7_s15_7$ty-1
data_7_s15_7$key_ty <- paste(data_7_s15_7$assessid, data_7_s15_7$ty, sep="_")
data_7_s15_7$key_ty_minus1 <- paste(data_7_s15_7$assessid, data_7_s15_7$ty_minus1, sep="_")
data_7_s15_7[,c(46:47,50:61,64:69)] <- NA

data_7_s15_8 <- data_7_s15[data_7_s15$terminal_year == 2012 | data_7_s15$terminal_year == 2014,]
data_7_s15_8$ty_order <- 8
data_7_s15_8$if_diff_5yrs <- "N"
data_7_s15_8$ty <- 2012
data_7_s15_8$ty_minus1 <- data_7_s15_8$ty-1
data_7_s15_8$key_ty <- paste(data_7_s15_8$assessid, data_7_s15_8$ty, sep="_")
data_7_s15_8$key_ty_minus1 <- paste(data_7_s15_8$assessid, data_7_s15_8$ty_minus1, sep="_")
data_7_s15_8[,c(46:47,50:61,64:69)] <- NA

data_7_s15_9 <- data_7_s15[data_7_s15$terminal_year == 2012 | data_7_s15$terminal_year == 2015,]
data_7_s15_9$ty_order <- 9
data_7_s15_9$if_diff_5yrs <- "N"
data_7_s15_9$ty <- 2012
data_7_s15_9$ty_minus1 <- data_7_s15_9$ty-1
data_7_s15_9$key_ty <- paste(data_7_s15_9$assessid, data_7_s15_9$ty, sep="_")
data_7_s15_9$key_ty_minus1 <- paste(data_7_s15_9$assessid, data_7_s15_9$ty_minus1, sep="_")
data_7_s15_9[,c(46:47,50:61,64:69)] <- NA

data_7_s15_10 <- data_7_s15[data_7_s15$terminal_year == 2012 | data_7_s15$terminal_year == 2016,]
data_7_s15_10$ty_order <- 10
data_7_s15_10$if_diff_5yrs <- "N"
data_7_s15_10$ty <- 2012
data_7_s15_10$ty_minus1 <- data_7_s15_10$ty-1
data_7_s15_10$key_ty <- paste(data_7_s15_10$assessid, data_7_s15_10$ty, sep="_")
data_7_s15_10$key_ty_minus1 <- paste(data_7_s15_10$assessid, data_7_s15_10$ty_minus1, sep="_")
data_7_s15_10[,c(46:47,50:61,64:69)] <- NA

data_7_s15_11 <- data_7_s15[data_7_s15$terminal_year == 2012 | data_7_s15$terminal_year == 2017,]
data_7_s15_11$ty_order <- 11
data_7_s15_11$if_diff_5yrs <- "N"
data_7_s15_11$ty <- 2012
data_7_s15_11$ty_minus1 <- data_7_s15_11$ty-1
data_7_s15_11$key_ty <- paste(data_7_s15_11$assessid, data_7_s15_11$ty, sep="_")
data_7_s15_11$key_ty_minus1 <- paste(data_7_s15_11$assessid, data_7_s15_11$ty_minus1, sep="_")
data_7_s15_11[,c(46:47,50:61,64:69)] <- NA

data_7_s15_12 <- data_7_s15[data_7_s15$terminal_year == 2013 | data_7_s15$terminal_year == 2014,]
data_7_s15_12$ty_order <- 12
data_7_s15_12$if_diff_5yrs <- "N"
data_7_s15_12$ty <- 2013
data_7_s15_12$ty_minus1 <- data_7_s15_12$ty-1
data_7_s15_12$key_ty <- paste(data_7_s15_12$assessid, data_7_s15_12$ty, sep="_")
data_7_s15_12$key_ty_minus1 <- paste(data_7_s15_12$assessid, data_7_s15_12$ty_minus1, sep="_")
data_7_s15_12[,c(46:47,50:61,64:69)] <- NA

data_7_s15_13 <- data_7_s15[data_7_s15$terminal_year == 2013 | data_7_s15$terminal_year == 2015,]
data_7_s15_13$ty_order <- 13
data_7_s15_13$if_diff_5yrs <- "N"
data_7_s15_13$ty <- 2013
data_7_s15_13$ty_minus1 <- data_7_s15_13$ty-1
data_7_s15_13$key_ty <- paste(data_7_s15_13$assessid, data_7_s15_13$ty, sep="_")
data_7_s15_13$key_ty_minus1 <- paste(data_7_s15_13$assessid, data_7_s15_13$ty_minus1, sep="_")
data_7_s15_13[,c(46:47,50:61,64:69)] <- NA

data_7_s15_14 <- data_7_s15[data_7_s15$terminal_year == 2013 | data_7_s15$terminal_year == 2016,]
data_7_s15_14$ty_order <- 14
data_7_s15_14$if_diff_5yrs <- "N"
data_7_s15_14$ty <- 2013
data_7_s15_14$ty_minus1 <- data_7_s15_14$ty-1
data_7_s15_14$key_ty <- paste(data_7_s15_14$assessid, data_7_s15_14$ty, sep="_")
data_7_s15_14$key_ty_minus1 <- paste(data_7_s15_14$assessid, data_7_s15_14$ty_minus1, sep="_")
data_7_s15_14[,c(46:47,50:61,64:69)] <- NA

data_7_s15_15 <- data_7_s15[data_7_s15$terminal_year == 2013 | data_7_s15$terminal_year == 2017,]
data_7_s15_15$ty_order <- 15
data_7_s15_15$if_diff_5yrs <- "N"
data_7_s15_15$ty <- 2013
data_7_s15_15$ty_minus1 <- data_7_s15_15$ty-1
data_7_s15_15$key_ty <- paste(data_7_s15_15$assessid, data_7_s15_15$ty, sep="_")
data_7_s15_15$key_ty_minus1 <- paste(data_7_s15_15$assessid, data_7_s15_15$ty_minus1, sep="_")
data_7_s15_15[,c(46:47,50:61,64:69)] <- NA

data_7_s15_16 <- data_7_s15[data_7_s15$terminal_year == 2014 | data_7_s15$terminal_year == 2015,]
data_7_s15_16$ty_order <- 16
data_7_s15_16$if_diff_5yrs <- "N"
data_7_s15_16$ty <- 2014
data_7_s15_16$ty_minus1 <- data_7_s15_16$ty-1
data_7_s15_16$key_ty <- paste(data_7_s15_16$assessid, data_7_s15_16$ty, sep="_")
data_7_s15_16$key_ty_minus1 <- paste(data_7_s15_16$assessid, data_7_s15_16$ty_minus1, sep="_")
data_7_s15_16[,c(46:47,50:61,64:69)] <- NA

data_7_s15_17 <- data_7_s15[data_7_s15$terminal_year == 2014 | data_7_s15$terminal_year == 2016,]
data_7_s15_17$ty_order <- 17
data_7_s15_17$if_diff_5yrs <- "N"
data_7_s15_17$ty <- 2014
data_7_s15_17$ty_minus1 <- data_7_s15_17$ty-1
data_7_s15_17$key_ty <- paste(data_7_s15_17$assessid, data_7_s15_17$ty, sep="_")
data_7_s15_17$key_ty_minus1 <- paste(data_7_s15_17$assessid, data_7_s15_17$ty_minus1, sep="_")
data_7_s15_17[,c(46:47,50:61,64:69)] <- NA

data_7_s15_18 <- data_7_s15[data_7_s15$terminal_year == 2014 | data_7_s15$terminal_year == 2017,]
data_7_s15_18$ty_order <- 18
data_7_s15_18$if_diff_5yrs <- "N"
data_7_s15_18$ty <- 2014
data_7_s15_18$ty_minus1 <- data_7_s15_18$ty-1
data_7_s15_18$key_ty <- paste(data_7_s15_18$assessid, data_7_s15_18$ty, sep="_")
data_7_s15_18$key_ty_minus1 <- paste(data_7_s15_18$assessid, data_7_s15_18$ty_minus1, sep="_")
data_7_s15_18[,c(46:47,50:61,64:69)] <- NA

data_7_s15_19 <- data_7_s15[data_7_s15$terminal_year == 2015 | data_7_s15$terminal_year == 2016,]
data_7_s15_19$ty_order <- 19
data_7_s15_19$if_diff_5yrs <- "N"
data_7_s15_19$ty <- 2015
data_7_s15_19$ty_minus1 <- data_7_s15_19$ty-1
data_7_s15_19$key_ty <- paste(data_7_s15_19$assessid, data_7_s15_19$ty, sep="_")
data_7_s15_19$key_ty_minus1 <- paste(data_7_s15_19$assessid, data_7_s15_19$ty_minus1, sep="_")
data_7_s15_19[,c(46:47,50:61,64:69)] <- NA

data_7_s15_20 <- data_7_s15[data_7_s15$terminal_year == 2015 | data_7_s15$terminal_year == 2017,]
data_7_s15_20$ty_order <- 20
data_7_s15_20$if_diff_5yrs <- "N"
data_7_s15_20$ty <- 2015
data_7_s15_20$ty_minus1 <- data_7_s15_20$ty-1
data_7_s15_20$key_ty <- paste(data_7_s15_20$assessid, data_7_s15_20$ty, sep="_")
data_7_s15_20$key_ty_minus1 <- paste(data_7_s15_20$assessid, data_7_s15_20$ty_minus1, sep="_")
data_7_s15_20[,c(46:47,50:61,64:69)] <- NA

data_7_s15_21 <- data_7_s15[data_7_s15$terminal_year == 2016 | data_7_s15$terminal_year == 2017,]
data_7_s15_21$ty_order <- 21
data_7_s15_21$if_diff_5yrs <- "N"
data_7_s15_21$ty <- 2016
data_7_s15_21$ty_minus1 <- data_7_s15_21$ty-1
data_7_s15_21$key_ty <- paste(data_7_s15_21$assessid, data_7_s15_21$ty, sep="_")
data_7_s15_21$key_ty_minus1 <- paste(data_7_s15_21$assessid, data_7_s15_21$ty_minus1, sep="_")
data_7_s15_21[,c(46:47,50:61,64:69)] <- NA

data_7_s15_1$interval <- max(data_7_s15_1$terminal_year) - min(data_7_s15_1$terminal_year)
data_7_s15_2$interval <- max(data_7_s15_2$terminal_year) - min(data_7_s15_2$terminal_year)
data_7_s15_3$interval <- max(data_7_s15_3$terminal_year) - min(data_7_s15_3$terminal_year)
data_7_s15_4$interval <- max(data_7_s15_4$terminal_year) - min(data_7_s15_4$terminal_year)
data_7_s15_5$interval <- max(data_7_s15_5$terminal_year) - min(data_7_s15_5$terminal_year)
data_7_s15_6$interval <- max(data_7_s15_6$terminal_year) - min(data_7_s15_6$terminal_year)
data_7_s15_7$interval <- max(data_7_s15_7$terminal_year) - min(data_7_s15_7$terminal_year)
data_7_s15_8$interval <- max(data_7_s15_8$terminal_year) - min(data_7_s15_8$terminal_year)
data_7_s15_9$interval <- max(data_7_s15_9$terminal_year) - min(data_7_s15_9$terminal_year)
data_7_s15_10$interval <- max(data_7_s15_10$terminal_year) - min(data_7_s15_10$terminal_year)
data_7_s15_11$interval <- max(data_7_s15_11$terminal_year) - min(data_7_s15_11$terminal_year)
data_7_s15_12$interval <- max(data_7_s15_12$terminal_year) - min(data_7_s15_12$terminal_year)
data_7_s15_13$interval <- max(data_7_s15_13$terminal_year) - min(data_7_s15_13$terminal_year)
data_7_s15_14$interval <- max(data_7_s15_14$terminal_year) - min(data_7_s15_14$terminal_year)
data_7_s15_15$interval <- max(data_7_s15_15$terminal_year) - min(data_7_s15_15$terminal_year)
data_7_s15_16$interval <- max(data_7_s15_16$terminal_year) - min(data_7_s15_16$terminal_year)
data_7_s15_17$interval <- max(data_7_s15_17$terminal_year) - min(data_7_s15_17$terminal_year)
data_7_s15_18$interval <- max(data_7_s15_18$terminal_year) - min(data_7_s15_18$terminal_year)
data_7_s15_19$interval <- max(data_7_s15_19$terminal_year) - min(data_7_s15_19$terminal_year)
data_7_s15_20$interval <- max(data_7_s15_20$terminal_year) - min(data_7_s15_20$terminal_year)
data_7_s15_21$interval <- max(data_7_s15_21$terminal_year) - min(data_7_s15_21$terminal_year)

data_7_s15 <- rbind(data_7_s15_1, data_7_s15_2, data_7_s15_3, data_7_s15_4, data_7_s15_5, data_7_s15_6, data_7_s15_7, data_7_s15_8, data_7_s15_9, data_7_s15_10,
                    data_7_s15_11, data_7_s15_12, data_7_s15_13, data_7_s15_14, data_7_s15_15, data_7_s15_16, data_7_s15_17, data_7_s15_18, data_7_s15_19,
                    data_7_s15_20, data_7_s15_21)

# assess 16
data_7_s16 <- data_7_assess[data_7_assess$stockid==ids[16],]

data_7_s16_1 <- data_7_s16[data_7_s16$terminal_year == 2010 | data_7_s16$terminal_year == 2012,]
data_7_s16_1$ty_order <- 1
data_7_s16_1$if_diff_5yrs <- "N"

data_7_s16_2 <- data_7_s16[data_7_s16$terminal_year == 2010 | data_7_s16$terminal_year == 2013,]
data_7_s16_2$ty_order <- 2
data_7_s16_2$if_diff_5yrs <- "N"

data_7_s16_3 <- data_7_s16[data_7_s16$terminal_year == 2010 | data_7_s16$terminal_year == 2014,]
data_7_s16_3$ty_order <- 3
data_7_s16_3$if_diff_5yrs <- "N"

data_7_s16_4 <- data_7_s16[data_7_s16$terminal_year == 2010 | data_7_s16$terminal_year == 2015,]
data_7_s16_4$ty_order <- 4
data_7_s16_4$if_diff_5yrs <- "N"

data_7_s16_5 <- data_7_s16[data_7_s16$terminal_year == 2010 | data_7_s16$terminal_year == 2016,]
data_7_s16_5$ty_order <- 5
data_7_s16_5$if_diff_5yrs <- "Y"

data_7_s16_6 <- data_7_s16[data_7_s16$terminal_year == 2010 | data_7_s16$terminal_year == 2017,]
data_7_s16_6$ty_order <- 6
data_7_s16_6$if_diff_5yrs <- "Y"

data_7_s16_7 <- data_7_s16[data_7_s16$terminal_year == 2012 | data_7_s16$terminal_year == 2013,]
data_7_s16_7$ty_order <- 7
data_7_s16_7$if_diff_5yrs <- "N"
data_7_s16_7$ty <- 2012
data_7_s16_7$ty_minus1 <- data_7_s16_7$ty-1
data_7_s16_7$key_ty <- paste(data_7_s16_7$assessid, data_7_s16_7$ty, sep="_")
data_7_s16_7$key_ty_minus1 <- paste(data_7_s16_7$assessid, data_7_s16_7$ty_minus1, sep="_")
data_7_s16_7[,c(46:47,50:61,64:69)] <- NA

data_7_s16_8 <- data_7_s16[data_7_s16$terminal_year == 2012 | data_7_s16$terminal_year == 2014,]
data_7_s16_8$ty_order <- 8
data_7_s16_8$if_diff_5yrs <- "N"
data_7_s16_8$ty <- 2012
data_7_s16_8$ty_minus1 <- data_7_s16_8$ty-1
data_7_s16_8$key_ty <- paste(data_7_s16_8$assessid, data_7_s16_8$ty, sep="_")
data_7_s16_8$key_ty_minus1 <- paste(data_7_s16_8$assessid, data_7_s16_8$ty_minus1, sep="_")
data_7_s16_8[,c(46:47,50:61,64:69)] <- NA

data_7_s16_9 <- data_7_s16[data_7_s16$terminal_year == 2012 | data_7_s16$terminal_year == 2015,]
data_7_s16_9$ty_order <- 9
data_7_s16_9$if_diff_5yrs <- "N"
data_7_s16_9$ty <- 2012
data_7_s16_9$ty_minus1 <- data_7_s16_9$ty-1
data_7_s16_9$key_ty <- paste(data_7_s16_9$assessid, data_7_s16_9$ty, sep="_")
data_7_s16_9$key_ty_minus1 <- paste(data_7_s16_9$assessid, data_7_s16_9$ty_minus1, sep="_")
data_7_s16_9[,c(46:47,50:61,64:69)] <- NA

data_7_s16_10 <- data_7_s16[data_7_s16$terminal_year == 2012 | data_7_s16$terminal_year == 2016,]
data_7_s16_10$ty_order <- 10
data_7_s16_10$if_diff_5yrs <- "N"
data_7_s16_10$ty <- 2012
data_7_s16_10$ty_minus1 <- data_7_s16_10$ty-1
data_7_s16_10$key_ty <- paste(data_7_s16_10$assessid, data_7_s16_10$ty, sep="_")
data_7_s16_10$key_ty_minus1 <- paste(data_7_s16_10$assessid, data_7_s16_10$ty_minus1, sep="_")
data_7_s16_10[,c(46:47,50:61,64:69)] <- NA

data_7_s16_11 <- data_7_s16[data_7_s16$terminal_year == 2012 | data_7_s16$terminal_year == 2017,]
data_7_s16_11$ty_order <- 11
data_7_s16_11$if_diff_5yrs <- "N"
data_7_s16_11$ty <- 2012
data_7_s16_11$ty_minus1 <- data_7_s16_11$ty-1
data_7_s16_11$key_ty <- paste(data_7_s16_11$assessid, data_7_s16_11$ty, sep="_")
data_7_s16_11$key_ty_minus1 <- paste(data_7_s16_11$assessid, data_7_s16_11$ty_minus1, sep="_")
data_7_s16_11[,c(46:47,50:61,64:69)] <- NA

data_7_s16_12 <- data_7_s16[data_7_s16$terminal_year == 2013 | data_7_s16$terminal_year == 2014,]
data_7_s16_12$ty_order <- 12
data_7_s16_12$if_diff_5yrs <- "N"
data_7_s16_12$ty <- 2013
data_7_s16_12$ty_minus1 <- data_7_s16_12$ty-1
data_7_s16_12$key_ty <- paste(data_7_s16_12$assessid, data_7_s16_12$ty, sep="_")
data_7_s16_12$key_ty_minus1 <- paste(data_7_s16_12$assessid, data_7_s16_12$ty_minus1, sep="_")
data_7_s16_12[,c(46:47,50:61,64:69)] <- NA

data_7_s16_13 <- data_7_s16[data_7_s16$terminal_year == 2013 | data_7_s16$terminal_year == 2015,]
data_7_s16_13$ty_order <- 13
data_7_s16_13$if_diff_5yrs <- "N"
data_7_s16_13$ty <- 2013
data_7_s16_13$ty_minus1 <- data_7_s16_13$ty-1
data_7_s16_13$key_ty <- paste(data_7_s16_13$assessid, data_7_s16_13$ty, sep="_")
data_7_s16_13$key_ty_minus1 <- paste(data_7_s16_13$assessid, data_7_s16_13$ty_minus1, sep="_")
data_7_s16_13[,c(46:47,50:61,64:69)] <- NA

data_7_s16_14 <- data_7_s16[data_7_s16$terminal_year == 2013 | data_7_s16$terminal_year == 2016,]
data_7_s16_14$ty_order <- 14
data_7_s16_14$if_diff_5yrs <- "N"
data_7_s16_14$ty <- 2013
data_7_s16_14$ty_minus1 <- data_7_s16_14$ty-1
data_7_s16_14$key_ty <- paste(data_7_s16_14$assessid, data_7_s16_14$ty, sep="_")
data_7_s16_14$key_ty_minus1 <- paste(data_7_s16_14$assessid, data_7_s16_14$ty_minus1, sep="_")
data_7_s16_14[,c(46:47,50:61,64:69)] <- NA

data_7_s16_15 <- data_7_s16[data_7_s16$terminal_year == 2013 | data_7_s16$terminal_year == 2017,]
data_7_s16_15$ty_order <- 15
data_7_s16_15$if_diff_5yrs <- "N"
data_7_s16_15$ty <- 2013
data_7_s16_15$ty_minus1 <- data_7_s16_15$ty-1
data_7_s16_15$key_ty <- paste(data_7_s16_15$assessid, data_7_s16_15$ty, sep="_")
data_7_s16_15$key_ty_minus1 <- paste(data_7_s16_15$assessid, data_7_s16_15$ty_minus1, sep="_")
data_7_s16_15[,c(46:47,50:61,64:69)] <- NA

data_7_s16_16 <- data_7_s16[data_7_s16$terminal_year == 2014 | data_7_s16$terminal_year == 2015,]
data_7_s16_16$ty_order <- 16
data_7_s16_16$if_diff_5yrs <- "N"
data_7_s16_16$ty <- 2014
data_7_s16_16$ty_minus1 <- data_7_s16_16$ty-1
data_7_s16_16$key_ty <- paste(data_7_s16_16$assessid, data_7_s16_16$ty, sep="_")
data_7_s16_16$key_ty_minus1 <- paste(data_7_s16_16$assessid, data_7_s16_16$ty_minus1, sep="_")
data_7_s16_16[,c(46:47,50:61,64:69)] <- NA

data_7_s16_17 <- data_7_s16[data_7_s16$terminal_year == 2014 | data_7_s16$terminal_year == 2016,]
data_7_s16_17$ty_order <- 17
data_7_s16_17$if_diff_5yrs <- "N"
data_7_s16_17$ty <- 2014
data_7_s16_17$ty_minus1 <- data_7_s16_17$ty-1
data_7_s16_17$key_ty <- paste(data_7_s16_17$assessid, data_7_s16_17$ty, sep="_")
data_7_s16_17$key_ty_minus1 <- paste(data_7_s16_17$assessid, data_7_s16_17$ty_minus1, sep="_")
data_7_s16_17[,c(46:47,50:61,64:69)] <- NA

data_7_s16_18 <- data_7_s16[data_7_s16$terminal_year == 2014 | data_7_s16$terminal_year == 2017,]
data_7_s16_18$ty_order <- 18
data_7_s16_18$if_diff_5yrs <- "N"
data_7_s16_18$ty <- 2014
data_7_s16_18$ty_minus1 <- data_7_s16_18$ty-1
data_7_s16_18$key_ty <- paste(data_7_s16_18$assessid, data_7_s16_18$ty, sep="_")
data_7_s16_18$key_ty_minus1 <- paste(data_7_s16_18$assessid, data_7_s16_18$ty_minus1, sep="_")
data_7_s16_18[,c(46:47,50:61,64:69)] <- NA

data_7_s16_19 <- data_7_s16[data_7_s16$terminal_year == 2015 | data_7_s16$terminal_year == 2016,]
data_7_s16_19$ty_order <- 19
data_7_s16_19$if_diff_5yrs <- "N"
data_7_s16_19$ty <- 2015
data_7_s16_19$ty_minus1 <- data_7_s16_19$ty-1
data_7_s16_19$key_ty <- paste(data_7_s16_19$assessid, data_7_s16_19$ty, sep="_")
data_7_s16_19$key_ty_minus1 <- paste(data_7_s16_19$assessid, data_7_s16_19$ty_minus1, sep="_")
data_7_s16_19[,c(46:47,50:61,64:69)] <- NA

data_7_s16_20 <- data_7_s16[data_7_s16$terminal_year == 2015 | data_7_s16$terminal_year == 2017,]
data_7_s16_20$ty_order <- 20
data_7_s16_20$if_diff_5yrs <- "N"
data_7_s16_20$ty <- 2015
data_7_s16_20$ty_minus1 <- data_7_s16_20$ty-1
data_7_s16_20$key_ty <- paste(data_7_s16_20$assessid, data_7_s16_20$ty, sep="_")
data_7_s16_20$key_ty_minus1 <- paste(data_7_s16_20$assessid, data_7_s16_20$ty_minus1, sep="_")
data_7_s16_20[,c(46:47,50:61,64:69)] <- NA

data_7_s16_21 <- data_7_s16[data_7_s16$terminal_year == 2016 | data_7_s16$terminal_year == 2017,]
data_7_s16_21$ty_order <- 21
data_7_s16_21$if_diff_5yrs <- "N"
data_7_s16_21$ty <- 2016
data_7_s16_21$ty_minus1 <- data_7_s16_21$ty-1
data_7_s16_21$key_ty <- paste(data_7_s16_21$assessid, data_7_s16_21$ty, sep="_")
data_7_s16_21$key_ty_minus1 <- paste(data_7_s16_21$assessid, data_7_s16_21$ty_minus1, sep="_")
data_7_s16_21[,c(46:47,50:61,64:69)] <- NA

data_7_s16_1$interval <- max(data_7_s16_1$terminal_year) - min(data_7_s16_1$terminal_year)
data_7_s16_2$interval <- max(data_7_s16_2$terminal_year) - min(data_7_s16_2$terminal_year)
data_7_s16_3$interval <- max(data_7_s16_3$terminal_year) - min(data_7_s16_3$terminal_year)
data_7_s16_4$interval <- max(data_7_s16_4$terminal_year) - min(data_7_s16_4$terminal_year)
data_7_s16_5$interval <- max(data_7_s16_5$terminal_year) - min(data_7_s16_5$terminal_year)
data_7_s16_6$interval <- max(data_7_s16_6$terminal_year) - min(data_7_s16_6$terminal_year)
data_7_s16_7$interval <- max(data_7_s16_7$terminal_year) - min(data_7_s16_7$terminal_year)
data_7_s16_8$interval <- max(data_7_s16_8$terminal_year) - min(data_7_s16_8$terminal_year)
data_7_s16_9$interval <- max(data_7_s16_9$terminal_year) - min(data_7_s16_9$terminal_year)
data_7_s16_10$interval <- max(data_7_s16_10$terminal_year) - min(data_7_s16_10$terminal_year)
data_7_s16_11$interval <- max(data_7_s16_11$terminal_year) - min(data_7_s16_11$terminal_year)
data_7_s16_12$interval <- max(data_7_s16_12$terminal_year) - min(data_7_s16_12$terminal_year)
data_7_s16_13$interval <- max(data_7_s16_13$terminal_year) - min(data_7_s16_13$terminal_year)
data_7_s16_14$interval <- max(data_7_s16_14$terminal_year) - min(data_7_s16_14$terminal_year)
data_7_s16_15$interval <- max(data_7_s16_15$terminal_year) - min(data_7_s16_15$terminal_year)
data_7_s16_16$interval <- max(data_7_s16_16$terminal_year) - min(data_7_s16_16$terminal_year)
data_7_s16_17$interval <- max(data_7_s16_17$terminal_year) - min(data_7_s16_17$terminal_year)
data_7_s16_18$interval <- max(data_7_s16_18$terminal_year) - min(data_7_s16_18$terminal_year)
data_7_s16_19$interval <- max(data_7_s16_19$terminal_year) - min(data_7_s16_19$terminal_year)
data_7_s16_20$interval <- max(data_7_s16_20$terminal_year) - min(data_7_s16_20$terminal_year)
data_7_s16_21$interval <- max(data_7_s16_21$terminal_year) - min(data_7_s16_21$terminal_year)

data_7_s16 <- rbind(data_7_s16_1, data_7_s16_2, data_7_s16_3, data_7_s16_4, data_7_s16_5, data_7_s16_6, data_7_s16_7, data_7_s16_8, data_7_s16_9, data_7_s16_10,
                    data_7_s16_11, data_7_s16_12, data_7_s16_13, data_7_s16_14, data_7_s16_15, data_7_s16_16, data_7_s16_17, data_7_s16_18, data_7_s16_19,
                    data_7_s16_20, data_7_s16_21)

# assess 17
data_7_s17 <- data_7_assess[data_7_assess$stockid==ids[17],]

data_7_s17_1 <- data_7_s17[data_7_s17$terminal_year == 2010 | data_7_s17$terminal_year == 2012,]
data_7_s17_1$ty_order <- 1
data_7_s17_1$if_diff_5yrs <- "N"

data_7_s17_2 <- data_7_s17[data_7_s17$terminal_year == 2010 | data_7_s17$terminal_year == 2013,]
data_7_s17_2$ty_order <- 2
data_7_s17_2$if_diff_5yrs <- "N"

data_7_s17_3 <- data_7_s17[data_7_s17$terminal_year == 2010 | data_7_s17$terminal_year == 2014,]
data_7_s17_3$ty_order <- 3
data_7_s17_3$if_diff_5yrs <- "N"

data_7_s17_4 <- data_7_s17[data_7_s17$terminal_year == 2010 | data_7_s17$terminal_year == 2015,]
data_7_s17_4$ty_order <- 4
data_7_s17_4$if_diff_5yrs <- "N"

data_7_s17_5 <- data_7_s17[data_7_s17$terminal_year == 2010 | data_7_s17$terminal_year == 2016,]
data_7_s17_5$ty_order <- 5
data_7_s17_5$if_diff_5yrs <- "Y"

data_7_s17_6 <- data_7_s17[data_7_s17$terminal_year == 2010 | data_7_s17$terminal_year == 2017,]
data_7_s17_6$ty_order <- 6
data_7_s17_6$if_diff_5yrs <- "Y"

data_7_s17_7 <- data_7_s17[data_7_s17$terminal_year == 2012 | data_7_s17$terminal_year == 2013,]
data_7_s17_7$ty_order <- 7
data_7_s17_7$if_diff_5yrs <- "N"
data_7_s17_7$ty <- 2012
data_7_s17_7$ty_minus1 <- data_7_s17_7$ty-1
data_7_s17_7$key_ty <- paste(data_7_s17_7$assessid, data_7_s17_7$ty, sep="_")
data_7_s17_7$key_ty_minus1 <- paste(data_7_s17_7$assessid, data_7_s17_7$ty_minus1, sep="_")
data_7_s17_7[,c(46:47,50:61,64:69)] <- NA

data_7_s17_8 <- data_7_s17[data_7_s17$terminal_year == 2012 | data_7_s17$terminal_year == 2014,]
data_7_s17_8$ty_order <- 8
data_7_s17_8$if_diff_5yrs <- "N"
data_7_s17_8$ty <- 2012
data_7_s17_8$ty_minus1 <- data_7_s17_8$ty-1
data_7_s17_8$key_ty <- paste(data_7_s17_8$assessid, data_7_s17_8$ty, sep="_")
data_7_s17_8$key_ty_minus1 <- paste(data_7_s17_8$assessid, data_7_s17_8$ty_minus1, sep="_")
data_7_s17_8[,c(46:47,50:61,64:69)] <- NA

data_7_s17_9 <- data_7_s17[data_7_s17$terminal_year == 2012 | data_7_s17$terminal_year == 2015,]
data_7_s17_9$ty_order <- 9
data_7_s17_9$if_diff_5yrs <- "N"
data_7_s17_9$ty <- 2012
data_7_s17_9$ty_minus1 <- data_7_s17_9$ty-1
data_7_s17_9$key_ty <- paste(data_7_s17_9$assessid, data_7_s17_9$ty, sep="_")
data_7_s17_9$key_ty_minus1 <- paste(data_7_s17_9$assessid, data_7_s17_9$ty_minus1, sep="_")
data_7_s17_9[,c(46:47,50:61,64:69)] <- NA

data_7_s17_10 <- data_7_s17[data_7_s17$terminal_year == 2012 | data_7_s17$terminal_year == 2016,]
data_7_s17_10$ty_order <- 10
data_7_s17_10$if_diff_5yrs <- "N"
data_7_s17_10$ty <- 2012
data_7_s17_10$ty_minus1 <- data_7_s17_10$ty-1
data_7_s17_10$key_ty <- paste(data_7_s17_10$assessid, data_7_s17_10$ty, sep="_")
data_7_s17_10$key_ty_minus1 <- paste(data_7_s17_10$assessid, data_7_s17_10$ty_minus1, sep="_")
data_7_s17_10[,c(46:47,50:61,64:69)] <- NA

data_7_s17_11 <- data_7_s17[data_7_s17$terminal_year == 2012 | data_7_s17$terminal_year == 2017,]
data_7_s17_11$ty_order <- 11
data_7_s17_11$if_diff_5yrs <- "N"
data_7_s17_11$ty <- 2012
data_7_s17_11$ty_minus1 <- data_7_s17_11$ty-1
data_7_s17_11$key_ty <- paste(data_7_s17_11$assessid, data_7_s17_11$ty, sep="_")
data_7_s17_11$key_ty_minus1 <- paste(data_7_s17_11$assessid, data_7_s17_11$ty_minus1, sep="_")
data_7_s17_11[,c(46:47,50:61,64:69)] <- NA

data_7_s17_12 <- data_7_s17[data_7_s17$terminal_year == 2013 | data_7_s17$terminal_year == 2014,]
data_7_s17_12$ty_order <- 12
data_7_s17_12$if_diff_5yrs <- "N"
data_7_s17_12$ty <- 2013
data_7_s17_12$ty_minus1 <- data_7_s17_12$ty-1
data_7_s17_12$key_ty <- paste(data_7_s17_12$assessid, data_7_s17_12$ty, sep="_")
data_7_s17_12$key_ty_minus1 <- paste(data_7_s17_12$assessid, data_7_s17_12$ty_minus1, sep="_")
data_7_s17_12[,c(46:47,50:61,64:69)] <- NA

data_7_s17_13 <- data_7_s17[data_7_s17$terminal_year == 2013 | data_7_s17$terminal_year == 2015,]
data_7_s17_13$ty_order <- 13
data_7_s17_13$if_diff_5yrs <- "N"
data_7_s17_13$ty <- 2013
data_7_s17_13$ty_minus1 <- data_7_s17_13$ty-1
data_7_s17_13$key_ty <- paste(data_7_s17_13$assessid, data_7_s17_13$ty, sep="_")
data_7_s17_13$key_ty_minus1 <- paste(data_7_s17_13$assessid, data_7_s17_13$ty_minus1, sep="_")
data_7_s17_13[,c(46:47,50:61,64:69)] <- NA

data_7_s17_14 <- data_7_s17[data_7_s17$terminal_year == 2013 | data_7_s17$terminal_year == 2016,]
data_7_s17_14$ty_order <- 14
data_7_s17_14$if_diff_5yrs <- "N"
data_7_s17_14$ty <- 2013
data_7_s17_14$ty_minus1 <- data_7_s17_14$ty-1
data_7_s17_14$key_ty <- paste(data_7_s17_14$assessid, data_7_s17_14$ty, sep="_")
data_7_s17_14$key_ty_minus1 <- paste(data_7_s17_14$assessid, data_7_s17_14$ty_minus1, sep="_")
data_7_s17_14[,c(46:47,50:61,64:69)] <- NA

data_7_s17_15 <- data_7_s17[data_7_s17$terminal_year == 2013 | data_7_s17$terminal_year == 2017,]
data_7_s17_15$ty_order <- 15
data_7_s17_15$if_diff_5yrs <- "N"
data_7_s17_15$ty <- 2013
data_7_s17_15$ty_minus1 <- data_7_s17_15$ty-1
data_7_s17_15$key_ty <- paste(data_7_s17_15$assessid, data_7_s17_15$ty, sep="_")
data_7_s17_15$key_ty_minus1 <- paste(data_7_s17_15$assessid, data_7_s17_15$ty_minus1, sep="_")
data_7_s17_15[,c(46:47,50:61,64:69)] <- NA

data_7_s17_16 <- data_7_s17[data_7_s17$terminal_year == 2014 | data_7_s17$terminal_year == 2015,]
data_7_s17_16$ty_order <- 16
data_7_s17_16$if_diff_5yrs <- "N"
data_7_s17_16$ty <- 2014
data_7_s17_16$ty_minus1 <- data_7_s17_16$ty-1
data_7_s17_16$key_ty <- paste(data_7_s17_16$assessid, data_7_s17_16$ty, sep="_")
data_7_s17_16$key_ty_minus1 <- paste(data_7_s17_16$assessid, data_7_s17_16$ty_minus1, sep="_")
data_7_s17_16[,c(46:47,50:61,64:69)] <- NA

data_7_s17_17 <- data_7_s17[data_7_s17$terminal_year == 2014 | data_7_s17$terminal_year == 2016,]
data_7_s17_17$ty_order <- 17
data_7_s17_17$if_diff_5yrs <- "N"
data_7_s17_17$ty <- 2014
data_7_s17_17$ty_minus1 <- data_7_s17_17$ty-1
data_7_s17_17$key_ty <- paste(data_7_s17_17$assessid, data_7_s17_17$ty, sep="_")
data_7_s17_17$key_ty_minus1 <- paste(data_7_s17_17$assessid, data_7_s17_17$ty_minus1, sep="_")
data_7_s17_17[,c(46:47,50:61,64:69)] <- NA

data_7_s17_18 <- data_7_s17[data_7_s17$terminal_year == 2014 | data_7_s17$terminal_year == 2017,]
data_7_s17_18$ty_order <- 18
data_7_s17_18$if_diff_5yrs <- "N"
data_7_s17_18$ty <- 2014
data_7_s17_18$ty_minus1 <- data_7_s17_18$ty-1
data_7_s17_18$key_ty <- paste(data_7_s17_18$assessid, data_7_s17_18$ty, sep="_")
data_7_s17_18$key_ty_minus1 <- paste(data_7_s17_18$assessid, data_7_s17_18$ty_minus1, sep="_")
data_7_s17_18[,c(46:47,50:61,64:69)] <- NA

data_7_s17_19 <- data_7_s17[data_7_s17$terminal_year == 2015 | data_7_s17$terminal_year == 2016,]
data_7_s17_19$ty_order <- 19
data_7_s17_19$if_diff_5yrs <- "N"
data_7_s17_19$ty <- 2015
data_7_s17_19$ty_minus1 <- data_7_s17_19$ty-1
data_7_s17_19$key_ty <- paste(data_7_s17_19$assessid, data_7_s17_19$ty, sep="_")
data_7_s17_19$key_ty_minus1 <- paste(data_7_s17_19$assessid, data_7_s17_19$ty_minus1, sep="_")
data_7_s17_19[,c(46:47,50:61,64:69)] <- NA

data_7_s17_20 <- data_7_s17[data_7_s17$terminal_year == 2015 | data_7_s17$terminal_year == 2017,]
data_7_s17_20$ty_order <- 20
data_7_s17_20$if_diff_5yrs <- "N"
data_7_s17_20$ty <- 2015
data_7_s17_20$ty_minus1 <- data_7_s17_20$ty-1
data_7_s17_20$key_ty <- paste(data_7_s17_20$assessid, data_7_s17_20$ty, sep="_")
data_7_s17_20$key_ty_minus1 <- paste(data_7_s17_20$assessid, data_7_s17_20$ty_minus1, sep="_")
data_7_s17_20[,c(46:47,50:61,64:69)] <- NA

data_7_s17_21 <- data_7_s17[data_7_s17$terminal_year == 2016 | data_7_s17$terminal_year == 2017,]
data_7_s17_21$ty_order <- 21
data_7_s17_21$if_diff_5yrs <- "N"
data_7_s17_21$ty <- 2016
data_7_s17_21$ty_minus1 <- data_7_s17_21$ty-1
data_7_s17_21$key_ty <- paste(data_7_s17_21$assessid, data_7_s17_21$ty, sep="_")
data_7_s17_21$key_ty_minus1 <- paste(data_7_s17_21$assessid, data_7_s17_21$ty_minus1, sep="_")
data_7_s17_21[,c(46:47,50:61,64:69)] <- NA

data_7_s17_1$interval <- max(data_7_s17_1$terminal_year) - min(data_7_s17_1$terminal_year)
data_7_s17_2$interval <- max(data_7_s17_2$terminal_year) - min(data_7_s17_2$terminal_year)
data_7_s17_3$interval <- max(data_7_s17_3$terminal_year) - min(data_7_s17_3$terminal_year)
data_7_s17_4$interval <- max(data_7_s17_4$terminal_year) - min(data_7_s17_4$terminal_year)
data_7_s17_5$interval <- max(data_7_s17_5$terminal_year) - min(data_7_s17_5$terminal_year)
data_7_s17_6$interval <- max(data_7_s17_6$terminal_year) - min(data_7_s17_6$terminal_year)
data_7_s17_7$interval <- max(data_7_s17_7$terminal_year) - min(data_7_s17_7$terminal_year)
data_7_s17_8$interval <- max(data_7_s17_8$terminal_year) - min(data_7_s17_8$terminal_year)
data_7_s17_9$interval <- max(data_7_s17_9$terminal_year) - min(data_7_s17_9$terminal_year)
data_7_s17_10$interval <- max(data_7_s17_10$terminal_year) - min(data_7_s17_10$terminal_year)
data_7_s17_11$interval <- max(data_7_s17_11$terminal_year) - min(data_7_s17_11$terminal_year)
data_7_s17_12$interval <- max(data_7_s17_12$terminal_year) - min(data_7_s17_12$terminal_year)
data_7_s17_13$interval <- max(data_7_s17_13$terminal_year) - min(data_7_s17_13$terminal_year)
data_7_s17_14$interval <- max(data_7_s17_14$terminal_year) - min(data_7_s17_14$terminal_year)
data_7_s17_15$interval <- max(data_7_s17_15$terminal_year) - min(data_7_s17_15$terminal_year)
data_7_s17_16$interval <- max(data_7_s17_16$terminal_year) - min(data_7_s17_16$terminal_year)
data_7_s17_17$interval <- max(data_7_s17_17$terminal_year) - min(data_7_s17_17$terminal_year)
data_7_s17_18$interval <- max(data_7_s17_18$terminal_year) - min(data_7_s17_18$terminal_year)
data_7_s17_19$interval <- max(data_7_s17_19$terminal_year) - min(data_7_s17_19$terminal_year)
data_7_s17_20$interval <- max(data_7_s17_20$terminal_year) - min(data_7_s17_20$terminal_year)
data_7_s17_21$interval <- max(data_7_s17_21$terminal_year) - min(data_7_s17_21$terminal_year)

data_7_s17 <- rbind(data_7_s17_1, data_7_s17_2, data_7_s17_3, data_7_s17_4, data_7_s17_5, data_7_s17_6, data_7_s17_7, data_7_s17_8, data_7_s17_9, data_7_s17_10,
                    data_7_s17_11, data_7_s17_12, data_7_s17_13, data_7_s17_14, data_7_s17_15, data_7_s17_16, data_7_s17_17, data_7_s17_18, data_7_s17_19,
                    data_7_s17_20, data_7_s17_21)

# assess 18
data_7_s18 <- data_7_assess[data_7_assess$stockid==ids[18],]

data_7_s18_1 <- data_7_s18[data_7_s18$terminal_year == 2010 | data_7_s18$terminal_year == 2012,]
data_7_s18_1$ty_order <- 1
data_7_s18_1$if_diff_5yrs <- "N"

data_7_s18_2 <- data_7_s18[data_7_s18$terminal_year == 2010 | data_7_s18$terminal_year == 2013,]
data_7_s18_2$ty_order <- 2
data_7_s18_2$if_diff_5yrs <- "N"

data_7_s18_3 <- data_7_s18[data_7_s18$terminal_year == 2010 | data_7_s18$terminal_year == 2014,]
data_7_s18_3$ty_order <- 3
data_7_s18_3$if_diff_5yrs <- "N"

data_7_s18_4 <- data_7_s18[data_7_s18$terminal_year == 2010 | data_7_s18$terminal_year == 2015,]
data_7_s18_4$ty_order <- 4
data_7_s18_4$if_diff_5yrs <- "N"

data_7_s18_5 <- data_7_s18[data_7_s18$terminal_year == 2010 | data_7_s18$terminal_year == 2016,]
data_7_s18_5$ty_order <- 5
data_7_s18_5$if_diff_5yrs <- "Y"

data_7_s18_6 <- data_7_s18[data_7_s18$terminal_year == 2010 | data_7_s18$terminal_year == 2017,]
data_7_s18_6$ty_order <- 6
data_7_s18_6$if_diff_5yrs <- "Y"

data_7_s18_7 <- data_7_s18[data_7_s18$terminal_year == 2012 | data_7_s18$terminal_year == 2013,]
data_7_s18_7$ty_order <- 7
data_7_s18_7$if_diff_5yrs <- "N"
data_7_s18_7$ty <- 2012
data_7_s18_7$ty_minus1 <- data_7_s18_7$ty-1
data_7_s18_7$key_ty <- paste(data_7_s18_7$assessid, data_7_s18_7$ty, sep="_")
data_7_s18_7$key_ty_minus1 <- paste(data_7_s18_7$assessid, data_7_s18_7$ty_minus1, sep="_")
data_7_s18_7[,c(46:47,50:61,64:69)] <- NA

data_7_s18_8 <- data_7_s18[data_7_s18$terminal_year == 2012 | data_7_s18$terminal_year == 2014,]
data_7_s18_8$ty_order <- 8
data_7_s18_8$if_diff_5yrs <- "N"
data_7_s18_8$ty <- 2012
data_7_s18_8$ty_minus1 <- data_7_s18_8$ty-1
data_7_s18_8$key_ty <- paste(data_7_s18_8$assessid, data_7_s18_8$ty, sep="_")
data_7_s18_8$key_ty_minus1 <- paste(data_7_s18_8$assessid, data_7_s18_8$ty_minus1, sep="_")
data_7_s18_8[,c(46:47,50:61,64:69)] <- NA

data_7_s18_9 <- data_7_s18[data_7_s18$terminal_year == 2012 | data_7_s18$terminal_year == 2015,]
data_7_s18_9$ty_order <- 9
data_7_s18_9$if_diff_5yrs <- "N"
data_7_s18_9$ty <- 2012
data_7_s18_9$ty_minus1 <- data_7_s18_9$ty-1
data_7_s18_9$key_ty <- paste(data_7_s18_9$assessid, data_7_s18_9$ty, sep="_")
data_7_s18_9$key_ty_minus1 <- paste(data_7_s18_9$assessid, data_7_s18_9$ty_minus1, sep="_")
data_7_s18_9[,c(46:47,50:61,64:69)] <- NA

data_7_s18_10 <- data_7_s18[data_7_s18$terminal_year == 2012 | data_7_s18$terminal_year == 2016,]
data_7_s18_10$ty_order <- 10
data_7_s18_10$if_diff_5yrs <- "N"
data_7_s18_10$ty <- 2012
data_7_s18_10$ty_minus1 <- data_7_s18_10$ty-1
data_7_s18_10$key_ty <- paste(data_7_s18_10$assessid, data_7_s18_10$ty, sep="_")
data_7_s18_10$key_ty_minus1 <- paste(data_7_s18_10$assessid, data_7_s18_10$ty_minus1, sep="_")
data_7_s18_10[,c(46:47,50:61,64:69)] <- NA

data_7_s18_11 <- data_7_s18[data_7_s18$terminal_year == 2012 | data_7_s18$terminal_year == 2017,]
data_7_s18_11$ty_order <- 11
data_7_s18_11$if_diff_5yrs <- "N"
data_7_s18_11$ty <- 2012
data_7_s18_11$ty_minus1 <- data_7_s18_11$ty-1
data_7_s18_11$key_ty <- paste(data_7_s18_11$assessid, data_7_s18_11$ty, sep="_")
data_7_s18_11$key_ty_minus1 <- paste(data_7_s18_11$assessid, data_7_s18_11$ty_minus1, sep="_")
data_7_s18_11[,c(46:47,50:61,64:69)] <- NA

data_7_s18_12 <- data_7_s18[data_7_s18$terminal_year == 2013 | data_7_s18$terminal_year == 2014,]
data_7_s18_12$ty_order <- 12
data_7_s18_12$if_diff_5yrs <- "N"
data_7_s18_12$ty <- 2013
data_7_s18_12$ty_minus1 <- data_7_s18_12$ty-1
data_7_s18_12$key_ty <- paste(data_7_s18_12$assessid, data_7_s18_12$ty, sep="_")
data_7_s18_12$key_ty_minus1 <- paste(data_7_s18_12$assessid, data_7_s18_12$ty_minus1, sep="_")
data_7_s18_12[,c(46:47,50:61,64:69)] <- NA

data_7_s18_13 <- data_7_s18[data_7_s18$terminal_year == 2013 | data_7_s18$terminal_year == 2015,]
data_7_s18_13$ty_order <- 13
data_7_s18_13$if_diff_5yrs <- "N"
data_7_s18_13$ty <- 2013
data_7_s18_13$ty_minus1 <- data_7_s18_13$ty-1
data_7_s18_13$key_ty <- paste(data_7_s18_13$assessid, data_7_s18_13$ty, sep="_")
data_7_s18_13$key_ty_minus1 <- paste(data_7_s18_13$assessid, data_7_s18_13$ty_minus1, sep="_")
data_7_s18_13[,c(46:47,50:61,64:69)] <- NA

data_7_s18_14 <- data_7_s18[data_7_s18$terminal_year == 2013 | data_7_s18$terminal_year == 2016,]
data_7_s18_14$ty_order <- 14
data_7_s18_14$if_diff_5yrs <- "N"
data_7_s18_14$ty <- 2013
data_7_s18_14$ty_minus1 <- data_7_s18_14$ty-1
data_7_s18_14$key_ty <- paste(data_7_s18_14$assessid, data_7_s18_14$ty, sep="_")
data_7_s18_14$key_ty_minus1 <- paste(data_7_s18_14$assessid, data_7_s18_14$ty_minus1, sep="_")
data_7_s18_14[,c(46:47,50:61,64:69)] <- NA

data_7_s18_15 <- data_7_s18[data_7_s18$terminal_year == 2013 | data_7_s18$terminal_year == 2017,]
data_7_s18_15$ty_order <- 15
data_7_s18_15$if_diff_5yrs <- "N"
data_7_s18_15$ty <- 2013
data_7_s18_15$ty_minus1 <- data_7_s18_15$ty-1
data_7_s18_15$key_ty <- paste(data_7_s18_15$assessid, data_7_s18_15$ty, sep="_")
data_7_s18_15$key_ty_minus1 <- paste(data_7_s18_15$assessid, data_7_s18_15$ty_minus1, sep="_")
data_7_s18_15[,c(46:47,50:61,64:69)] <- NA

data_7_s18_16 <- data_7_s18[data_7_s18$terminal_year == 2014 | data_7_s18$terminal_year == 2015,]
data_7_s18_16$ty_order <- 16
data_7_s18_16$if_diff_5yrs <- "N"
data_7_s18_16$ty <- 2014
data_7_s18_16$ty_minus1 <- data_7_s18_16$ty-1
data_7_s18_16$key_ty <- paste(data_7_s18_16$assessid, data_7_s18_16$ty, sep="_")
data_7_s18_16$key_ty_minus1 <- paste(data_7_s18_16$assessid, data_7_s18_16$ty_minus1, sep="_")
data_7_s18_16[,c(46:47,50:61,64:69)] <- NA

data_7_s18_17 <- data_7_s18[data_7_s18$terminal_year == 2014 | data_7_s18$terminal_year == 2016,]
data_7_s18_17$ty_order <- 17
data_7_s18_17$if_diff_5yrs <- "N"
data_7_s18_17$ty <- 2014
data_7_s18_17$ty_minus1 <- data_7_s18_17$ty-1
data_7_s18_17$key_ty <- paste(data_7_s18_17$assessid, data_7_s18_17$ty, sep="_")
data_7_s18_17$key_ty_minus1 <- paste(data_7_s18_17$assessid, data_7_s18_17$ty_minus1, sep="_")
data_7_s18_17[,c(46:47,50:61,64:69)] <- NA

data_7_s18_18 <- data_7_s18[data_7_s18$terminal_year == 2014 | data_7_s18$terminal_year == 2017,]
data_7_s18_18$ty_order <- 18
data_7_s18_18$if_diff_5yrs <- "N"
data_7_s18_18$ty <- 2014
data_7_s18_18$ty_minus1 <- data_7_s18_18$ty-1
data_7_s18_18$key_ty <- paste(data_7_s18_18$assessid, data_7_s18_18$ty, sep="_")
data_7_s18_18$key_ty_minus1 <- paste(data_7_s18_18$assessid, data_7_s18_18$ty_minus1, sep="_")
data_7_s18_18[,c(46:47,50:61,64:69)] <- NA

data_7_s18_19 <- data_7_s18[data_7_s18$terminal_year == 2015 | data_7_s18$terminal_year == 2016,]
data_7_s18_19$ty_order <- 19
data_7_s18_19$if_diff_5yrs <- "N"
data_7_s18_19$ty <- 2015
data_7_s18_19$ty_minus1 <- data_7_s18_19$ty-1
data_7_s18_19$key_ty <- paste(data_7_s18_19$assessid, data_7_s18_19$ty, sep="_")
data_7_s18_19$key_ty_minus1 <- paste(data_7_s18_19$assessid, data_7_s18_19$ty_minus1, sep="_")
data_7_s18_19[,c(46:47,50:61,64:69)] <- NA

data_7_s18_20 <- data_7_s18[data_7_s18$terminal_year == 2015 | data_7_s18$terminal_year == 2017,]
data_7_s18_20$ty_order <- 20
data_7_s18_20$if_diff_5yrs <- "N"
data_7_s18_20$ty <- 2015
data_7_s18_20$ty_minus1 <- data_7_s18_20$ty-1
data_7_s18_20$key_ty <- paste(data_7_s18_20$assessid, data_7_s18_20$ty, sep="_")
data_7_s18_20$key_ty_minus1 <- paste(data_7_s18_20$assessid, data_7_s18_20$ty_minus1, sep="_")
data_7_s18_20[,c(46:47,50:61,64:69)] <- NA

data_7_s18_21 <- data_7_s18[data_7_s18$terminal_year == 2016 | data_7_s18$terminal_year == 2017,]
data_7_s18_21$ty_order <- 21
data_7_s18_21$if_diff_5yrs <- "N"
data_7_s18_21$ty <- 2016
data_7_s18_21$ty_minus1 <- data_7_s18_21$ty-1
data_7_s18_21$key_ty <- paste(data_7_s18_21$assessid, data_7_s18_21$ty, sep="_")
data_7_s18_21$key_ty_minus1 <- paste(data_7_s18_21$assessid, data_7_s18_21$ty_minus1, sep="_")
data_7_s18_21[,c(46:47,50:61,64:69)] <- NA

data_7_s18_1$interval <- max(data_7_s18_1$terminal_year) - min(data_7_s18_1$terminal_year)
data_7_s18_2$interval <- max(data_7_s18_2$terminal_year) - min(data_7_s18_2$terminal_year)
data_7_s18_3$interval <- max(data_7_s18_3$terminal_year) - min(data_7_s18_3$terminal_year)
data_7_s18_4$interval <- max(data_7_s18_4$terminal_year) - min(data_7_s18_4$terminal_year)
data_7_s18_5$interval <- max(data_7_s18_5$terminal_year) - min(data_7_s18_5$terminal_year)
data_7_s18_6$interval <- max(data_7_s18_6$terminal_year) - min(data_7_s18_6$terminal_year)
data_7_s18_7$interval <- max(data_7_s18_7$terminal_year) - min(data_7_s18_7$terminal_year)
data_7_s18_8$interval <- max(data_7_s18_8$terminal_year) - min(data_7_s18_8$terminal_year)
data_7_s18_9$interval <- max(data_7_s18_9$terminal_year) - min(data_7_s18_9$terminal_year)
data_7_s18_10$interval <- max(data_7_s18_10$terminal_year) - min(data_7_s18_10$terminal_year)
data_7_s18_11$interval <- max(data_7_s18_11$terminal_year) - min(data_7_s18_11$terminal_year)
data_7_s18_12$interval <- max(data_7_s18_12$terminal_year) - min(data_7_s18_12$terminal_year)
data_7_s18_13$interval <- max(data_7_s18_13$terminal_year) - min(data_7_s18_13$terminal_year)
data_7_s18_14$interval <- max(data_7_s18_14$terminal_year) - min(data_7_s18_14$terminal_year)
data_7_s18_15$interval <- max(data_7_s18_15$terminal_year) - min(data_7_s18_15$terminal_year)
data_7_s18_16$interval <- max(data_7_s18_16$terminal_year) - min(data_7_s18_16$terminal_year)
data_7_s18_17$interval <- max(data_7_s18_17$terminal_year) - min(data_7_s18_17$terminal_year)
data_7_s18_18$interval <- max(data_7_s18_18$terminal_year) - min(data_7_s18_18$terminal_year)
data_7_s18_19$interval <- max(data_7_s18_19$terminal_year) - min(data_7_s18_19$terminal_year)
data_7_s18_20$interval <- max(data_7_s18_20$terminal_year) - min(data_7_s18_20$terminal_year)
data_7_s18_21$interval <- max(data_7_s18_21$terminal_year) - min(data_7_s18_21$terminal_year)

data_7_s18 <- rbind(data_7_s18_1, data_7_s18_2, data_7_s18_3, data_7_s18_4, data_7_s18_5, data_7_s18_6, data_7_s18_7, data_7_s18_8, data_7_s18_9, data_7_s18_10,
                    data_7_s18_11, data_7_s18_12, data_7_s18_13, data_7_s18_14, data_7_s18_15, data_7_s18_16, data_7_s18_17, data_7_s18_18, data_7_s18_19,
                    data_7_s18_20, data_7_s18_21)

# assess 19
data_7_s19 <- data_7_assess[data_7_assess$stockid==ids[19],]

data_7_s19_1 <- data_7_s19[data_7_s19$terminal_year == 2010 | data_7_s19$terminal_year == 2012,]
data_7_s19_1$ty_order <- 1
data_7_s19_1$if_diff_5yrs <- "N"

data_7_s19_2 <- data_7_s19[data_7_s19$terminal_year == 2010 | data_7_s19$terminal_year == 2013,]
data_7_s19_2$ty_order <- 2
data_7_s19_2$if_diff_5yrs <- "N"

data_7_s19_3 <- data_7_s19[data_7_s19$terminal_year == 2010 | data_7_s19$terminal_year == 2014,]
data_7_s19_3$ty_order <- 3
data_7_s19_3$if_diff_5yrs <- "N"

data_7_s19_4 <- data_7_s19[data_7_s19$terminal_year == 2010 | data_7_s19$terminal_year == 2015,]
data_7_s19_4$ty_order <- 4
data_7_s19_4$if_diff_5yrs <- "N"

data_7_s19_5 <- data_7_s19[data_7_s19$terminal_year == 2010 | data_7_s19$terminal_year == 2016,]
data_7_s19_5$ty_order <- 5
data_7_s19_5$if_diff_5yrs <- "Y"

data_7_s19_6 <- data_7_s19[data_7_s19$terminal_year == 2010 | data_7_s19$terminal_year == 2017,]
data_7_s19_6$ty_order <- 6
data_7_s19_6$if_diff_5yrs <- "Y"

data_7_s19_7 <- data_7_s19[data_7_s19$terminal_year == 2012 | data_7_s19$terminal_year == 2013,]
data_7_s19_7$ty_order <- 7
data_7_s19_7$if_diff_5yrs <- "N"
data_7_s19_7$ty <- 2012
data_7_s19_7$ty_minus1 <- data_7_s19_7$ty-1
data_7_s19_7$key_ty <- paste(data_7_s19_7$assessid, data_7_s19_7$ty, sep="_")
data_7_s19_7$key_ty_minus1 <- paste(data_7_s19_7$assessid, data_7_s19_7$ty_minus1, sep="_")
data_7_s19_7[,c(46:47,50:61,64:69)] <- NA

data_7_s19_8 <- data_7_s19[data_7_s19$terminal_year == 2012 | data_7_s19$terminal_year == 2014,]
data_7_s19_8$ty_order <- 8
data_7_s19_8$if_diff_5yrs <- "N"
data_7_s19_8$ty <- 2012
data_7_s19_8$ty_minus1 <- data_7_s19_8$ty-1
data_7_s19_8$key_ty <- paste(data_7_s19_8$assessid, data_7_s19_8$ty, sep="_")
data_7_s19_8$key_ty_minus1 <- paste(data_7_s19_8$assessid, data_7_s19_8$ty_minus1, sep="_")
data_7_s19_8[,c(46:47,50:61,64:69)] <- NA

data_7_s19_9 <- data_7_s19[data_7_s19$terminal_year == 2012 | data_7_s19$terminal_year == 2015,]
data_7_s19_9$ty_order <- 9
data_7_s19_9$if_diff_5yrs <- "N"
data_7_s19_9$ty <- 2012
data_7_s19_9$ty_minus1 <- data_7_s19_9$ty-1
data_7_s19_9$key_ty <- paste(data_7_s19_9$assessid, data_7_s19_9$ty, sep="_")
data_7_s19_9$key_ty_minus1 <- paste(data_7_s19_9$assessid, data_7_s19_9$ty_minus1, sep="_")
data_7_s19_9[,c(46:47,50:61,64:69)] <- NA

data_7_s19_10 <- data_7_s19[data_7_s19$terminal_year == 2012 | data_7_s19$terminal_year == 2016,]
data_7_s19_10$ty_order <- 10
data_7_s19_10$if_diff_5yrs <- "N"
data_7_s19_10$ty <- 2012
data_7_s19_10$ty_minus1 <- data_7_s19_10$ty-1
data_7_s19_10$key_ty <- paste(data_7_s19_10$assessid, data_7_s19_10$ty, sep="_")
data_7_s19_10$key_ty_minus1 <- paste(data_7_s19_10$assessid, data_7_s19_10$ty_minus1, sep="_")
data_7_s19_10[,c(46:47,50:61,64:69)] <- NA

data_7_s19_11 <- data_7_s19[data_7_s19$terminal_year == 2012 | data_7_s19$terminal_year == 2017,]
data_7_s19_11$ty_order <- 11
data_7_s19_11$if_diff_5yrs <- "N"
data_7_s19_11$ty <- 2012
data_7_s19_11$ty_minus1 <- data_7_s19_11$ty-1
data_7_s19_11$key_ty <- paste(data_7_s19_11$assessid, data_7_s19_11$ty, sep="_")
data_7_s19_11$key_ty_minus1 <- paste(data_7_s19_11$assessid, data_7_s19_11$ty_minus1, sep="_")
data_7_s19_11[,c(46:47,50:61,64:69)] <- NA

data_7_s19_12 <- data_7_s19[data_7_s19$terminal_year == 2013 | data_7_s19$terminal_year == 2014,]
data_7_s19_12$ty_order <- 12
data_7_s19_12$if_diff_5yrs <- "N"
data_7_s19_12$ty <- 2013
data_7_s19_12$ty_minus1 <- data_7_s19_12$ty-1
data_7_s19_12$key_ty <- paste(data_7_s19_12$assessid, data_7_s19_12$ty, sep="_")
data_7_s19_12$key_ty_minus1 <- paste(data_7_s19_12$assessid, data_7_s19_12$ty_minus1, sep="_")
data_7_s19_12[,c(46:47,50:61,64:69)] <- NA

data_7_s19_13 <- data_7_s19[data_7_s19$terminal_year == 2013 | data_7_s19$terminal_year == 2015,]
data_7_s19_13$ty_order <- 13
data_7_s19_13$if_diff_5yrs <- "N"
data_7_s19_13$ty <- 2013
data_7_s19_13$ty_minus1 <- data_7_s19_13$ty-1
data_7_s19_13$key_ty <- paste(data_7_s19_13$assessid, data_7_s19_13$ty, sep="_")
data_7_s19_13$key_ty_minus1 <- paste(data_7_s19_13$assessid, data_7_s19_13$ty_minus1, sep="_")
data_7_s19_13[,c(46:47,50:61,64:69)] <- NA

data_7_s19_14 <- data_7_s19[data_7_s19$terminal_year == 2013 | data_7_s19$terminal_year == 2016,]
data_7_s19_14$ty_order <- 14
data_7_s19_14$if_diff_5yrs <- "N"
data_7_s19_14$ty <- 2013
data_7_s19_14$ty_minus1 <- data_7_s19_14$ty-1
data_7_s19_14$key_ty <- paste(data_7_s19_14$assessid, data_7_s19_14$ty, sep="_")
data_7_s19_14$key_ty_minus1 <- paste(data_7_s19_14$assessid, data_7_s19_14$ty_minus1, sep="_")
data_7_s19_14[,c(46:47,50:61,64:69)] <- NA

data_7_s19_15 <- data_7_s19[data_7_s19$terminal_year == 2013 | data_7_s19$terminal_year == 2017,]
data_7_s19_15$ty_order <- 15
data_7_s19_15$if_diff_5yrs <- "N"
data_7_s19_15$ty <- 2013
data_7_s19_15$ty_minus1 <- data_7_s19_15$ty-1
data_7_s19_15$key_ty <- paste(data_7_s19_15$assessid, data_7_s19_15$ty, sep="_")
data_7_s19_15$key_ty_minus1 <- paste(data_7_s19_15$assessid, data_7_s19_15$ty_minus1, sep="_")
data_7_s19_15[,c(46:47,50:61,64:69)] <- NA

data_7_s19_16 <- data_7_s19[data_7_s19$terminal_year == 2014 | data_7_s19$terminal_year == 2015,]
data_7_s19_16$ty_order <- 16
data_7_s19_16$if_diff_5yrs <- "N"
data_7_s19_16$ty <- 2014
data_7_s19_16$ty_minus1 <- data_7_s19_16$ty-1
data_7_s19_16$key_ty <- paste(data_7_s19_16$assessid, data_7_s19_16$ty, sep="_")
data_7_s19_16$key_ty_minus1 <- paste(data_7_s19_16$assessid, data_7_s19_16$ty_minus1, sep="_")
data_7_s19_16[,c(46:47,50:61,64:69)] <- NA

data_7_s19_17 <- data_7_s19[data_7_s19$terminal_year == 2014 | data_7_s19$terminal_year == 2016,]
data_7_s19_17$ty_order <- 17
data_7_s19_17$if_diff_5yrs <- "N"
data_7_s19_17$ty <- 2014
data_7_s19_17$ty_minus1 <- data_7_s19_17$ty-1
data_7_s19_17$key_ty <- paste(data_7_s19_17$assessid, data_7_s19_17$ty, sep="_")
data_7_s19_17$key_ty_minus1 <- paste(data_7_s19_17$assessid, data_7_s19_17$ty_minus1, sep="_")
data_7_s19_17[,c(46:47,50:61,64:69)] <- NA

data_7_s19_18 <- data_7_s19[data_7_s19$terminal_year == 2014 | data_7_s19$terminal_year == 2017,]
data_7_s19_18$ty_order <- 18
data_7_s19_18$if_diff_5yrs <- "N"
data_7_s19_18$ty <- 2014
data_7_s19_18$ty_minus1 <- data_7_s19_18$ty-1
data_7_s19_18$key_ty <- paste(data_7_s19_18$assessid, data_7_s19_18$ty, sep="_")
data_7_s19_18$key_ty_minus1 <- paste(data_7_s19_18$assessid, data_7_s19_18$ty_minus1, sep="_")
data_7_s19_18[,c(46:47,50:61,64:69)] <- NA

data_7_s19_19 <- data_7_s19[data_7_s19$terminal_year == 2015 | data_7_s19$terminal_year == 2016,]
data_7_s19_19$ty_order <- 19
data_7_s19_19$if_diff_5yrs <- "N"
data_7_s19_19$ty <- 2015
data_7_s19_19$ty_minus1 <- data_7_s19_19$ty-1
data_7_s19_19$key_ty <- paste(data_7_s19_19$assessid, data_7_s19_19$ty, sep="_")
data_7_s19_19$key_ty_minus1 <- paste(data_7_s19_19$assessid, data_7_s19_19$ty_minus1, sep="_")
data_7_s19_19[,c(46:47,50:61,64:69)] <- NA

data_7_s19_20 <- data_7_s19[data_7_s19$terminal_year == 2015 | data_7_s19$terminal_year == 2017,]
data_7_s19_20$ty_order <- 20
data_7_s19_20$if_diff_5yrs <- "N"
data_7_s19_20$ty <- 2015
data_7_s19_20$ty_minus1 <- data_7_s19_20$ty-1
data_7_s19_20$key_ty <- paste(data_7_s19_20$assessid, data_7_s19_20$ty, sep="_")
data_7_s19_20$key_ty_minus1 <- paste(data_7_s19_20$assessid, data_7_s19_20$ty_minus1, sep="_")
data_7_s19_20[,c(46:47,50:61,64:69)] <- NA

data_7_s19_21 <- data_7_s19[data_7_s19$terminal_year == 2016 | data_7_s19$terminal_year == 2017,]
data_7_s19_21$ty_order <- 21
data_7_s19_21$if_diff_5yrs <- "N"
data_7_s19_21$ty <- 2016
data_7_s19_21$ty_minus1 <- data_7_s19_21$ty-1
data_7_s19_21$key_ty <- paste(data_7_s19_21$assessid, data_7_s19_21$ty, sep="_")
data_7_s19_21$key_ty_minus1 <- paste(data_7_s19_21$assessid, data_7_s19_21$ty_minus1, sep="_")
data_7_s19_21[,c(46:47,50:61,64:69)] <- NA

data_7_s19_1$interval <- max(data_7_s19_1$terminal_year) - min(data_7_s19_1$terminal_year)
data_7_s19_2$interval <- max(data_7_s19_2$terminal_year) - min(data_7_s19_2$terminal_year)
data_7_s19_3$interval <- max(data_7_s19_3$terminal_year) - min(data_7_s19_3$terminal_year)
data_7_s19_4$interval <- max(data_7_s19_4$terminal_year) - min(data_7_s19_4$terminal_year)
data_7_s19_5$interval <- max(data_7_s19_5$terminal_year) - min(data_7_s19_5$terminal_year)
data_7_s19_6$interval <- max(data_7_s19_6$terminal_year) - min(data_7_s19_6$terminal_year)
data_7_s19_7$interval <- max(data_7_s19_7$terminal_year) - min(data_7_s19_7$terminal_year)
data_7_s19_8$interval <- max(data_7_s19_8$terminal_year) - min(data_7_s19_8$terminal_year)
data_7_s19_9$interval <- max(data_7_s19_9$terminal_year) - min(data_7_s19_9$terminal_year)
data_7_s19_10$interval <- max(data_7_s19_10$terminal_year) - min(data_7_s19_10$terminal_year)
data_7_s19_11$interval <- max(data_7_s19_11$terminal_year) - min(data_7_s19_11$terminal_year)
data_7_s19_12$interval <- max(data_7_s19_12$terminal_year) - min(data_7_s19_12$terminal_year)
data_7_s19_13$interval <- max(data_7_s19_13$terminal_year) - min(data_7_s19_13$terminal_year)
data_7_s19_14$interval <- max(data_7_s19_14$terminal_year) - min(data_7_s19_14$terminal_year)
data_7_s19_15$interval <- max(data_7_s19_15$terminal_year) - min(data_7_s19_15$terminal_year)
data_7_s19_16$interval <- max(data_7_s19_16$terminal_year) - min(data_7_s19_16$terminal_year)
data_7_s19_17$interval <- max(data_7_s19_17$terminal_year) - min(data_7_s19_17$terminal_year)
data_7_s19_18$interval <- max(data_7_s19_18$terminal_year) - min(data_7_s19_18$terminal_year)
data_7_s19_19$interval <- max(data_7_s19_19$terminal_year) - min(data_7_s19_19$terminal_year)
data_7_s19_20$interval <- max(data_7_s19_20$terminal_year) - min(data_7_s19_20$terminal_year)
data_7_s19_21$interval <- max(data_7_s19_21$terminal_year) - min(data_7_s19_21$terminal_year)

data_7_s19 <- rbind(data_7_s19_1, data_7_s19_2, data_7_s19_3, data_7_s19_4, data_7_s19_5, data_7_s19_6, data_7_s19_7, data_7_s19_8, data_7_s19_9, data_7_s19_10,
                    data_7_s19_11, data_7_s19_12, data_7_s19_13, data_7_s19_14, data_7_s19_15, data_7_s19_16, data_7_s19_17, data_7_s19_18, data_7_s19_19,
                    data_7_s19_20, data_7_s19_21)

# assess 20
data_7_s20 <- data_7_assess[data_7_assess$stockid==ids[20],]

data_7_s20_1 <- data_7_s20[data_7_s20$terminal_year == 2010 | data_7_s20$terminal_year == 2012,]
data_7_s20_1$ty_order <- 1
data_7_s20_1$if_diff_5yrs <- "N"

data_7_s20_2 <- data_7_s20[data_7_s20$terminal_year == 2010 | data_7_s20$terminal_year == 2013,]
data_7_s20_2$ty_order <- 2
data_7_s20_2$if_diff_5yrs <- "N"

data_7_s20_3 <- data_7_s20[data_7_s20$terminal_year == 2010 | data_7_s20$terminal_year == 2014,]
data_7_s20_3$ty_order <- 3
data_7_s20_3$if_diff_5yrs <- "N"

data_7_s20_4 <- data_7_s20[data_7_s20$terminal_year == 2010 | data_7_s20$terminal_year == 2015,]
data_7_s20_4$ty_order <- 4
data_7_s20_4$if_diff_5yrs <- "N"

data_7_s20_5 <- data_7_s20[data_7_s20$terminal_year == 2010 | data_7_s20$terminal_year == 2016,]
data_7_s20_5$ty_order <- 5
data_7_s20_5$if_diff_5yrs <- "Y"

data_7_s20_6 <- data_7_s20[data_7_s20$terminal_year == 2010 | data_7_s20$terminal_year == 2017,]
data_7_s20_6$ty_order <- 6
data_7_s20_6$if_diff_5yrs <- "Y"

data_7_s20_7 <- data_7_s20[data_7_s20$terminal_year == 2012 | data_7_s20$terminal_year == 2013,]
data_7_s20_7$ty_order <- 7
data_7_s20_7$if_diff_5yrs <- "N"
data_7_s20_7$ty <- 2012
data_7_s20_7$ty_minus1 <- data_7_s20_7$ty-1
data_7_s20_7$key_ty <- paste(data_7_s20_7$assessid, data_7_s20_7$ty, sep="_")
data_7_s20_7$key_ty_minus1 <- paste(data_7_s20_7$assessid, data_7_s20_7$ty_minus1, sep="_")
data_7_s20_7[,c(46:47,50:61,64:69)] <- NA

data_7_s20_8 <- data_7_s20[data_7_s20$terminal_year == 2012 | data_7_s20$terminal_year == 2014,]
data_7_s20_8$ty_order <- 8
data_7_s20_8$if_diff_5yrs <- "N"
data_7_s20_8$ty <- 2012
data_7_s20_8$ty_minus1 <- data_7_s20_8$ty-1
data_7_s20_8$key_ty <- paste(data_7_s20_8$assessid, data_7_s20_8$ty, sep="_")
data_7_s20_8$key_ty_minus1 <- paste(data_7_s20_8$assessid, data_7_s20_8$ty_minus1, sep="_")
data_7_s20_8[,c(46:47,50:61,64:69)] <- NA

data_7_s20_9 <- data_7_s20[data_7_s20$terminal_year == 2012 | data_7_s20$terminal_year == 2015,]
data_7_s20_9$ty_order <- 9
data_7_s20_9$if_diff_5yrs <- "N"
data_7_s20_9$ty <- 2012
data_7_s20_9$ty_minus1 <- data_7_s20_9$ty-1
data_7_s20_9$key_ty <- paste(data_7_s20_9$assessid, data_7_s20_9$ty, sep="_")
data_7_s20_9$key_ty_minus1 <- paste(data_7_s20_9$assessid, data_7_s20_9$ty_minus1, sep="_")
data_7_s20_9[,c(46:47,50:61,64:69)] <- NA

data_7_s20_10 <- data_7_s20[data_7_s20$terminal_year == 2012 | data_7_s20$terminal_year == 2016,]
data_7_s20_10$ty_order <- 10
data_7_s20_10$if_diff_5yrs <- "N"
data_7_s20_10$ty <- 2012
data_7_s20_10$ty_minus1 <- data_7_s20_10$ty-1
data_7_s20_10$key_ty <- paste(data_7_s20_10$assessid, data_7_s20_10$ty, sep="_")
data_7_s20_10$key_ty_minus1 <- paste(data_7_s20_10$assessid, data_7_s20_10$ty_minus1, sep="_")
data_7_s20_10[,c(46:47,50:61,64:69)] <- NA

data_7_s20_11 <- data_7_s20[data_7_s20$terminal_year == 2012 | data_7_s20$terminal_year == 2017,]
data_7_s20_11$ty_order <- 11
data_7_s20_11$if_diff_5yrs <- "N"
data_7_s20_11$ty <- 2012
data_7_s20_11$ty_minus1 <- data_7_s20_11$ty-1
data_7_s20_11$key_ty <- paste(data_7_s20_11$assessid, data_7_s20_11$ty, sep="_")
data_7_s20_11$key_ty_minus1 <- paste(data_7_s20_11$assessid, data_7_s20_11$ty_minus1, sep="_")
data_7_s20_11[,c(46:47,50:61,64:69)] <- NA

data_7_s20_12 <- data_7_s20[data_7_s20$terminal_year == 2013 | data_7_s20$terminal_year == 2014,]
data_7_s20_12$ty_order <- 12
data_7_s20_12$if_diff_5yrs <- "N"
data_7_s20_12$ty <- 2013
data_7_s20_12$ty_minus1 <- data_7_s20_12$ty-1
data_7_s20_12$key_ty <- paste(data_7_s20_12$assessid, data_7_s20_12$ty, sep="_")
data_7_s20_12$key_ty_minus1 <- paste(data_7_s20_12$assessid, data_7_s20_12$ty_minus1, sep="_")
data_7_s20_12[,c(46:47,50:61,64:69)] <- NA

data_7_s20_13 <- data_7_s20[data_7_s20$terminal_year == 2013 | data_7_s20$terminal_year == 2015,]
data_7_s20_13$ty_order <- 13
data_7_s20_13$if_diff_5yrs <- "N"
data_7_s20_13$ty <- 2013
data_7_s20_13$ty_minus1 <- data_7_s20_13$ty-1
data_7_s20_13$key_ty <- paste(data_7_s20_13$assessid, data_7_s20_13$ty, sep="_")
data_7_s20_13$key_ty_minus1 <- paste(data_7_s20_13$assessid, data_7_s20_13$ty_minus1, sep="_")
data_7_s20_13[,c(46:47,50:61,64:69)] <- NA

data_7_s20_14 <- data_7_s20[data_7_s20$terminal_year == 2013 | data_7_s20$terminal_year == 2016,]
data_7_s20_14$ty_order <- 14
data_7_s20_14$if_diff_5yrs <- "N"
data_7_s20_14$ty <- 2013
data_7_s20_14$ty_minus1 <- data_7_s20_14$ty-1
data_7_s20_14$key_ty <- paste(data_7_s20_14$assessid, data_7_s20_14$ty, sep="_")
data_7_s20_14$key_ty_minus1 <- paste(data_7_s20_14$assessid, data_7_s20_14$ty_minus1, sep="_")
data_7_s20_14[,c(46:47,50:61,64:69)] <- NA

data_7_s20_15 <- data_7_s20[data_7_s20$terminal_year == 2013 | data_7_s20$terminal_year == 2017,]
data_7_s20_15$ty_order <- 15
data_7_s20_15$if_diff_5yrs <- "N"
data_7_s20_15$ty <- 2013
data_7_s20_15$ty_minus1 <- data_7_s20_15$ty-1
data_7_s20_15$key_ty <- paste(data_7_s20_15$assessid, data_7_s20_15$ty, sep="_")
data_7_s20_15$key_ty_minus1 <- paste(data_7_s20_15$assessid, data_7_s20_15$ty_minus1, sep="_")
data_7_s20_15[,c(46:47,50:61,64:69)] <- NA

data_7_s20_16 <- data_7_s20[data_7_s20$terminal_year == 2014 | data_7_s20$terminal_year == 2015,]
data_7_s20_16$ty_order <- 16
data_7_s20_16$if_diff_5yrs <- "N"
data_7_s20_16$ty <- 2014
data_7_s20_16$ty_minus1 <- data_7_s20_16$ty-1
data_7_s20_16$key_ty <- paste(data_7_s20_16$assessid, data_7_s20_16$ty, sep="_")
data_7_s20_16$key_ty_minus1 <- paste(data_7_s20_16$assessid, data_7_s20_16$ty_minus1, sep="_")
data_7_s20_16[,c(46:47,50:61,64:69)] <- NA

data_7_s20_17 <- data_7_s20[data_7_s20$terminal_year == 2014 | data_7_s20$terminal_year == 2016,]
data_7_s20_17$ty_order <- 17
data_7_s20_17$if_diff_5yrs <- "N"
data_7_s20_17$ty <- 2014
data_7_s20_17$ty_minus1 <- data_7_s20_17$ty-1
data_7_s20_17$key_ty <- paste(data_7_s20_17$assessid, data_7_s20_17$ty, sep="_")
data_7_s20_17$key_ty_minus1 <- paste(data_7_s20_17$assessid, data_7_s20_17$ty_minus1, sep="_")
data_7_s20_17[,c(46:47,50:61,64:69)] <- NA

data_7_s20_18 <- data_7_s20[data_7_s20$terminal_year == 2014 | data_7_s20$terminal_year == 2017,]
data_7_s20_18$ty_order <- 18
data_7_s20_18$if_diff_5yrs <- "N"
data_7_s20_18$ty <- 2014
data_7_s20_18$ty_minus1 <- data_7_s20_18$ty-1
data_7_s20_18$key_ty <- paste(data_7_s20_18$assessid, data_7_s20_18$ty, sep="_")
data_7_s20_18$key_ty_minus1 <- paste(data_7_s20_18$assessid, data_7_s20_18$ty_minus1, sep="_")
data_7_s20_18[,c(46:47,50:61,64:69)] <- NA

data_7_s20_19 <- data_7_s20[data_7_s20$terminal_year == 2015 | data_7_s20$terminal_year == 2016,]
data_7_s20_19$ty_order <- 19
data_7_s20_19$if_diff_5yrs <- "N"
data_7_s20_19$ty <- 2015
data_7_s20_19$ty_minus1 <- data_7_s20_19$ty-1
data_7_s20_19$key_ty <- paste(data_7_s20_19$assessid, data_7_s20_19$ty, sep="_")
data_7_s20_19$key_ty_minus1 <- paste(data_7_s20_19$assessid, data_7_s20_19$ty_minus1, sep="_")
data_7_s20_19[,c(46:47,50:61,64:69)] <- NA

data_7_s20_20 <- data_7_s20[data_7_s20$terminal_year == 2015 | data_7_s20$terminal_year == 2017,]
data_7_s20_20$ty_order <- 20
data_7_s20_20$if_diff_5yrs <- "N"
data_7_s20_20$ty <- 2015
data_7_s20_20$ty_minus1 <- data_7_s20_20$ty-1
data_7_s20_20$key_ty <- paste(data_7_s20_20$assessid, data_7_s20_20$ty, sep="_")
data_7_s20_20$key_ty_minus1 <- paste(data_7_s20_20$assessid, data_7_s20_20$ty_minus1, sep="_")
data_7_s20_20[,c(46:47,50:61,64:69)] <- NA

data_7_s20_21 <- data_7_s20[data_7_s20$terminal_year == 2016 | data_7_s20$terminal_year == 2017,]
data_7_s20_21$ty_order <- 21
data_7_s20_21$if_diff_5yrs <- "N"
data_7_s20_21$ty <- 2016
data_7_s20_21$ty_minus1 <- data_7_s20_21$ty-1
data_7_s20_21$key_ty <- paste(data_7_s20_21$assessid, data_7_s20_21$ty, sep="_")
data_7_s20_21$key_ty_minus1 <- paste(data_7_s20_21$assessid, data_7_s20_21$ty_minus1, sep="_")
data_7_s20_21[,c(46:47,50:61,64:69)] <- NA

data_7_s20_1$interval <- max(data_7_s20_1$terminal_year) - min(data_7_s20_1$terminal_year)
data_7_s20_2$interval <- max(data_7_s20_2$terminal_year) - min(data_7_s20_2$terminal_year)
data_7_s20_3$interval <- max(data_7_s20_3$terminal_year) - min(data_7_s20_3$terminal_year)
data_7_s20_4$interval <- max(data_7_s20_4$terminal_year) - min(data_7_s20_4$terminal_year)
data_7_s20_5$interval <- max(data_7_s20_5$terminal_year) - min(data_7_s20_5$terminal_year)
data_7_s20_6$interval <- max(data_7_s20_6$terminal_year) - min(data_7_s20_6$terminal_year)
data_7_s20_7$interval <- max(data_7_s20_7$terminal_year) - min(data_7_s20_7$terminal_year)
data_7_s20_8$interval <- max(data_7_s20_8$terminal_year) - min(data_7_s20_8$terminal_year)
data_7_s20_9$interval <- max(data_7_s20_9$terminal_year) - min(data_7_s20_9$terminal_year)
data_7_s20_10$interval <- max(data_7_s20_10$terminal_year) - min(data_7_s20_10$terminal_year)
data_7_s20_11$interval <- max(data_7_s20_11$terminal_year) - min(data_7_s20_11$terminal_year)
data_7_s20_12$interval <- max(data_7_s20_12$terminal_year) - min(data_7_s20_12$terminal_year)
data_7_s20_13$interval <- max(data_7_s20_13$terminal_year) - min(data_7_s20_13$terminal_year)
data_7_s20_14$interval <- max(data_7_s20_14$terminal_year) - min(data_7_s20_14$terminal_year)
data_7_s20_15$interval <- max(data_7_s20_15$terminal_year) - min(data_7_s20_15$terminal_year)
data_7_s20_16$interval <- max(data_7_s20_16$terminal_year) - min(data_7_s20_16$terminal_year)
data_7_s20_17$interval <- max(data_7_s20_17$terminal_year) - min(data_7_s20_17$terminal_year)
data_7_s20_18$interval <- max(data_7_s20_18$terminal_year) - min(data_7_s20_18$terminal_year)
data_7_s20_19$interval <- max(data_7_s20_19$terminal_year) - min(data_7_s20_19$terminal_year)
data_7_s20_20$interval <- max(data_7_s20_20$terminal_year) - min(data_7_s20_20$terminal_year)
data_7_s20_21$interval <- max(data_7_s20_21$terminal_year) - min(data_7_s20_21$terminal_year)

data_7_s20 <- rbind(data_7_s20_1, data_7_s20_2, data_7_s20_3, data_7_s20_4, data_7_s20_5, data_7_s20_6, data_7_s20_7, data_7_s20_8, data_7_s20_9, data_7_s20_10,
                    data_7_s20_11, data_7_s20_12, data_7_s20_13, data_7_s20_14, data_7_s20_15, data_7_s20_16, data_7_s20_17, data_7_s20_18, data_7_s20_19,
                    data_7_s20_20, data_7_s20_21)

# assess 21
data_7_s21 <- data_7_assess[data_7_assess$stockid==ids[21],]

data_7_s21_1 <- data_7_s21[data_7_s21$terminal_year == 2010 | data_7_s21$terminal_year == 2012,]
data_7_s21_1$ty_order <- 1
data_7_s21_1$if_diff_5yrs <- "N"

data_7_s21_2 <- data_7_s21[data_7_s21$terminal_year == 2010 | data_7_s21$terminal_year == 2013,]
data_7_s21_2$ty_order <- 2
data_7_s21_2$if_diff_5yrs <- "N"

data_7_s21_3 <- data_7_s21[data_7_s21$terminal_year == 2010 | data_7_s21$terminal_year == 2014,]
data_7_s21_3$ty_order <- 3
data_7_s21_3$if_diff_5yrs <- "N"

data_7_s21_4 <- data_7_s21[data_7_s21$terminal_year == 2010 | data_7_s21$terminal_year == 2015,]
data_7_s21_4$ty_order <- 4
data_7_s21_4$if_diff_5yrs <- "N"

data_7_s21_5 <- data_7_s21[data_7_s21$terminal_year == 2010 | data_7_s21$terminal_year == 2016,]
data_7_s21_5$ty_order <- 5
data_7_s21_5$if_diff_5yrs <- "Y"

data_7_s21_6 <- data_7_s21[data_7_s21$terminal_year == 2010 | data_7_s21$terminal_year == 2017,]
data_7_s21_6$ty_order <- 6
data_7_s21_6$if_diff_5yrs <- "Y"

data_7_s21_7 <- data_7_s21[data_7_s21$terminal_year == 2012 | data_7_s21$terminal_year == 2013,]
data_7_s21_7$ty_order <- 7
data_7_s21_7$if_diff_5yrs <- "N"
data_7_s21_7$ty <- 2012
data_7_s21_7$ty_minus1 <- data_7_s21_7$ty-1
data_7_s21_7$key_ty <- paste(data_7_s21_7$assessid, data_7_s21_7$ty, sep="_")
data_7_s21_7$key_ty_minus1 <- paste(data_7_s21_7$assessid, data_7_s21_7$ty_minus1, sep="_")
data_7_s21_7[,c(46:47,50:61,64:69)] <- NA

data_7_s21_8 <- data_7_s21[data_7_s21$terminal_year == 2012 | data_7_s21$terminal_year == 2014,]
data_7_s21_8$ty_order <- 8
data_7_s21_8$if_diff_5yrs <- "N"
data_7_s21_8$ty <- 2012
data_7_s21_8$ty_minus1 <- data_7_s21_8$ty-1
data_7_s21_8$key_ty <- paste(data_7_s21_8$assessid, data_7_s21_8$ty, sep="_")
data_7_s21_8$key_ty_minus1 <- paste(data_7_s21_8$assessid, data_7_s21_8$ty_minus1, sep="_")
data_7_s21_8[,c(46:47,50:61,64:69)] <- NA

data_7_s21_9 <- data_7_s21[data_7_s21$terminal_year == 2012 | data_7_s21$terminal_year == 2015,]
data_7_s21_9$ty_order <- 9
data_7_s21_9$if_diff_5yrs <- "N"
data_7_s21_9$ty <- 2012
data_7_s21_9$ty_minus1 <- data_7_s21_9$ty-1
data_7_s21_9$key_ty <- paste(data_7_s21_9$assessid, data_7_s21_9$ty, sep="_")
data_7_s21_9$key_ty_minus1 <- paste(data_7_s21_9$assessid, data_7_s21_9$ty_minus1, sep="_")
data_7_s21_9[,c(46:47,50:61,64:69)] <- NA

data_7_s21_10 <- data_7_s21[data_7_s21$terminal_year == 2012 | data_7_s21$terminal_year == 2016,]
data_7_s21_10$ty_order <- 10
data_7_s21_10$if_diff_5yrs <- "N"
data_7_s21_10$ty <- 2012
data_7_s21_10$ty_minus1 <- data_7_s21_10$ty-1
data_7_s21_10$key_ty <- paste(data_7_s21_10$assessid, data_7_s21_10$ty, sep="_")
data_7_s21_10$key_ty_minus1 <- paste(data_7_s21_10$assessid, data_7_s21_10$ty_minus1, sep="_")
data_7_s21_10[,c(46:47,50:61,64:69)] <- NA

data_7_s21_11 <- data_7_s21[data_7_s21$terminal_year == 2012 | data_7_s21$terminal_year == 2017,]
data_7_s21_11$ty_order <- 11
data_7_s21_11$if_diff_5yrs <- "N"
data_7_s21_11$ty <- 2012
data_7_s21_11$ty_minus1 <- data_7_s21_11$ty-1
data_7_s21_11$key_ty <- paste(data_7_s21_11$assessid, data_7_s21_11$ty, sep="_")
data_7_s21_11$key_ty_minus1 <- paste(data_7_s21_11$assessid, data_7_s21_11$ty_minus1, sep="_")
data_7_s21_11[,c(46:47,50:61,64:69)] <- NA

data_7_s21_12 <- data_7_s21[data_7_s21$terminal_year == 2013 | data_7_s21$terminal_year == 2014,]
data_7_s21_12$ty_order <- 12
data_7_s21_12$if_diff_5yrs <- "N"
data_7_s21_12$ty <- 2013
data_7_s21_12$ty_minus1 <- data_7_s21_12$ty-1
data_7_s21_12$key_ty <- paste(data_7_s21_12$assessid, data_7_s21_12$ty, sep="_")
data_7_s21_12$key_ty_minus1 <- paste(data_7_s21_12$assessid, data_7_s21_12$ty_minus1, sep="_")
data_7_s21_12[,c(46:47,50:61,64:69)] <- NA

data_7_s21_13 <- data_7_s21[data_7_s21$terminal_year == 2013 | data_7_s21$terminal_year == 2015,]
data_7_s21_13$ty_order <- 13
data_7_s21_13$if_diff_5yrs <- "N"
data_7_s21_13$ty <- 2013
data_7_s21_13$ty_minus1 <- data_7_s21_13$ty-1
data_7_s21_13$key_ty <- paste(data_7_s21_13$assessid, data_7_s21_13$ty, sep="_")
data_7_s21_13$key_ty_minus1 <- paste(data_7_s21_13$assessid, data_7_s21_13$ty_minus1, sep="_")
data_7_s21_13[,c(46:47,50:61,64:69)] <- NA

data_7_s21_14 <- data_7_s21[data_7_s21$terminal_year == 2013 | data_7_s21$terminal_year == 2016,]
data_7_s21_14$ty_order <- 14
data_7_s21_14$if_diff_5yrs <- "N"
data_7_s21_14$ty <- 2013
data_7_s21_14$ty_minus1 <- data_7_s21_14$ty-1
data_7_s21_14$key_ty <- paste(data_7_s21_14$assessid, data_7_s21_14$ty, sep="_")
data_7_s21_14$key_ty_minus1 <- paste(data_7_s21_14$assessid, data_7_s21_14$ty_minus1, sep="_")
data_7_s21_14[,c(46:47,50:61,64:69)] <- NA

data_7_s21_15 <- data_7_s21[data_7_s21$terminal_year == 2013 | data_7_s21$terminal_year == 2017,]
data_7_s21_15$ty_order <- 15
data_7_s21_15$if_diff_5yrs <- "N"
data_7_s21_15$ty <- 2013
data_7_s21_15$ty_minus1 <- data_7_s21_15$ty-1
data_7_s21_15$key_ty <- paste(data_7_s21_15$assessid, data_7_s21_15$ty, sep="_")
data_7_s21_15$key_ty_minus1 <- paste(data_7_s21_15$assessid, data_7_s21_15$ty_minus1, sep="_")
data_7_s21_15[,c(46:47,50:61,64:69)] <- NA

data_7_s21_16 <- data_7_s21[data_7_s21$terminal_year == 2014 | data_7_s21$terminal_year == 2015,]
data_7_s21_16$ty_order <- 16
data_7_s21_16$if_diff_5yrs <- "N"
data_7_s21_16$ty <- 2014
data_7_s21_16$ty_minus1 <- data_7_s21_16$ty-1
data_7_s21_16$key_ty <- paste(data_7_s21_16$assessid, data_7_s21_16$ty, sep="_")
data_7_s21_16$key_ty_minus1 <- paste(data_7_s21_16$assessid, data_7_s21_16$ty_minus1, sep="_")
data_7_s21_16[,c(46:47,50:61,64:69)] <- NA

data_7_s21_17 <- data_7_s21[data_7_s21$terminal_year == 2014 | data_7_s21$terminal_year == 2016,]
data_7_s21_17$ty_order <- 17
data_7_s21_17$if_diff_5yrs <- "N"
data_7_s21_17$ty <- 2014
data_7_s21_17$ty_minus1 <- data_7_s21_17$ty-1
data_7_s21_17$key_ty <- paste(data_7_s21_17$assessid, data_7_s21_17$ty, sep="_")
data_7_s21_17$key_ty_minus1 <- paste(data_7_s21_17$assessid, data_7_s21_17$ty_minus1, sep="_")
data_7_s21_17[,c(46:47,50:61,64:69)] <- NA

data_7_s21_18 <- data_7_s21[data_7_s21$terminal_year == 2014 | data_7_s21$terminal_year == 2017,]
data_7_s21_18$ty_order <- 18
data_7_s21_18$if_diff_5yrs <- "N"
data_7_s21_18$ty <- 2014
data_7_s21_18$ty_minus1 <- data_7_s21_18$ty-1
data_7_s21_18$key_ty <- paste(data_7_s21_18$assessid, data_7_s21_18$ty, sep="_")
data_7_s21_18$key_ty_minus1 <- paste(data_7_s21_18$assessid, data_7_s21_18$ty_minus1, sep="_")
data_7_s21_18[,c(46:47,50:61,64:69)] <- NA

data_7_s21_19 <- data_7_s21[data_7_s21$terminal_year == 2015 | data_7_s21$terminal_year == 2016,]
data_7_s21_19$ty_order <- 19
data_7_s21_19$if_diff_5yrs <- "N"
data_7_s21_19$ty <- 2015
data_7_s21_19$ty_minus1 <- data_7_s21_19$ty-1
data_7_s21_19$key_ty <- paste(data_7_s21_19$assessid, data_7_s21_19$ty, sep="_")
data_7_s21_19$key_ty_minus1 <- paste(data_7_s21_19$assessid, data_7_s21_19$ty_minus1, sep="_")
data_7_s21_19[,c(46:47,50:61,64:69)] <- NA

data_7_s21_20 <- data_7_s21[data_7_s21$terminal_year == 2015 | data_7_s21$terminal_year == 2017,]
data_7_s21_20$ty_order <- 20
data_7_s21_20$if_diff_5yrs <- "N"
data_7_s21_20$ty <- 2015
data_7_s21_20$ty_minus1 <- data_7_s21_20$ty-1
data_7_s21_20$key_ty <- paste(data_7_s21_20$assessid, data_7_s21_20$ty, sep="_")
data_7_s21_20$key_ty_minus1 <- paste(data_7_s21_20$assessid, data_7_s21_20$ty_minus1, sep="_")
data_7_s21_20[,c(46:47,50:61,64:69)] <- NA

data_7_s21_21 <- data_7_s21[data_7_s21$terminal_year == 2016 | data_7_s21$terminal_year == 2017,]
data_7_s21_21$ty_order <- 21
data_7_s21_21$if_diff_5yrs <- "N"
data_7_s21_21$ty <- 2016
data_7_s21_21$ty_minus1 <- data_7_s21_21$ty-1
data_7_s21_21$key_ty <- paste(data_7_s21_21$assessid, data_7_s21_21$ty, sep="_")
data_7_s21_21$key_ty_minus1 <- paste(data_7_s21_21$assessid, data_7_s21_21$ty_minus1, sep="_")
data_7_s21_21[,c(46:47,50:61,64:69)] <- NA

data_7_s21_1$interval <- max(data_7_s21_1$terminal_year) - min(data_7_s21_1$terminal_year)
data_7_s21_2$interval <- max(data_7_s21_2$terminal_year) - min(data_7_s21_2$terminal_year)
data_7_s21_3$interval <- max(data_7_s21_3$terminal_year) - min(data_7_s21_3$terminal_year)
data_7_s21_4$interval <- max(data_7_s21_4$terminal_year) - min(data_7_s21_4$terminal_year)
data_7_s21_5$interval <- max(data_7_s21_5$terminal_year) - min(data_7_s21_5$terminal_year)
data_7_s21_6$interval <- max(data_7_s21_6$terminal_year) - min(data_7_s21_6$terminal_year)
data_7_s21_7$interval <- max(data_7_s21_7$terminal_year) - min(data_7_s21_7$terminal_year)
data_7_s21_8$interval <- max(data_7_s21_8$terminal_year) - min(data_7_s21_8$terminal_year)
data_7_s21_9$interval <- max(data_7_s21_9$terminal_year) - min(data_7_s21_9$terminal_year)
data_7_s21_10$interval <- max(data_7_s21_10$terminal_year) - min(data_7_s21_10$terminal_year)
data_7_s21_11$interval <- max(data_7_s21_11$terminal_year) - min(data_7_s21_11$terminal_year)
data_7_s21_12$interval <- max(data_7_s21_12$terminal_year) - min(data_7_s21_12$terminal_year)
data_7_s21_13$interval <- max(data_7_s21_13$terminal_year) - min(data_7_s21_13$terminal_year)
data_7_s21_14$interval <- max(data_7_s21_14$terminal_year) - min(data_7_s21_14$terminal_year)
data_7_s21_15$interval <- max(data_7_s21_15$terminal_year) - min(data_7_s21_15$terminal_year)
data_7_s21_16$interval <- max(data_7_s21_16$terminal_year) - min(data_7_s21_16$terminal_year)
data_7_s21_17$interval <- max(data_7_s21_17$terminal_year) - min(data_7_s21_17$terminal_year)
data_7_s21_18$interval <- max(data_7_s21_18$terminal_year) - min(data_7_s21_18$terminal_year)
data_7_s21_19$interval <- max(data_7_s21_19$terminal_year) - min(data_7_s21_19$terminal_year)
data_7_s21_20$interval <- max(data_7_s21_20$terminal_year) - min(data_7_s21_20$terminal_year)
data_7_s21_21$interval <- max(data_7_s21_21$terminal_year) - min(data_7_s21_21$terminal_year)

data_7_s21 <- rbind(data_7_s21_1, data_7_s21_2, data_7_s21_3, data_7_s21_4, data_7_s21_5, data_7_s21_6, data_7_s21_7, data_7_s21_8, data_7_s21_9, data_7_s21_10,
                    data_7_s21_11, data_7_s21_12, data_7_s21_13, data_7_s21_14, data_7_s21_15, data_7_s21_16, data_7_s21_17, data_7_s21_18, data_7_s21_19,
                    data_7_s21_20, data_7_s21_21)

# assess 22
data_7_s22 <- data_7_assess[data_7_assess$stockid==ids[22],]

data_7_s22_1 <- data_7_s22[data_7_s22$terminal_year == 2010 | data_7_s22$terminal_year == 2012,]
data_7_s22_1$ty_order <- 1
data_7_s22_1$if_diff_5yrs <- "N"

data_7_s22_2 <- data_7_s22[data_7_s22$terminal_year == 2010 | data_7_s22$terminal_year == 2013,]
data_7_s22_2$ty_order <- 2
data_7_s22_2$if_diff_5yrs <- "N"

data_7_s22_3 <- data_7_s22[data_7_s22$terminal_year == 2010 | data_7_s22$terminal_year == 2014,]
data_7_s22_3$ty_order <- 3
data_7_s22_3$if_diff_5yrs <- "N"

data_7_s22_4 <- data_7_s22[data_7_s22$terminal_year == 2010 | data_7_s22$terminal_year == 2015,]
data_7_s22_4$ty_order <- 4
data_7_s22_4$if_diff_5yrs <- "N"

data_7_s22_5 <- data_7_s22[data_7_s22$terminal_year == 2010 | data_7_s22$terminal_year == 2016,]
data_7_s22_5$ty_order <- 5
data_7_s22_5$if_diff_5yrs <- "Y"

data_7_s22_6 <- data_7_s22[data_7_s22$terminal_year == 2010 | data_7_s22$terminal_year == 2017,]
data_7_s22_6$ty_order <- 6
data_7_s22_6$if_diff_5yrs <- "Y"

data_7_s22_7 <- data_7_s22[data_7_s22$terminal_year == 2012 | data_7_s22$terminal_year == 2013,]
data_7_s22_7$ty_order <- 7
data_7_s22_7$if_diff_5yrs <- "N"
data_7_s22_7$ty <- 2012
data_7_s22_7$ty_minus1 <- data_7_s22_7$ty-1
data_7_s22_7$key_ty <- paste(data_7_s22_7$assessid, data_7_s22_7$ty, sep="_")
data_7_s22_7$key_ty_minus1 <- paste(data_7_s22_7$assessid, data_7_s22_7$ty_minus1, sep="_")
data_7_s22_7[,c(46:47,50:61,64:69)] <- NA

data_7_s22_8 <- data_7_s22[data_7_s22$terminal_year == 2012 | data_7_s22$terminal_year == 2014,]
data_7_s22_8$ty_order <- 8
data_7_s22_8$if_diff_5yrs <- "N"
data_7_s22_8$ty <- 2012
data_7_s22_8$ty_minus1 <- data_7_s22_8$ty-1
data_7_s22_8$key_ty <- paste(data_7_s22_8$assessid, data_7_s22_8$ty, sep="_")
data_7_s22_8$key_ty_minus1 <- paste(data_7_s22_8$assessid, data_7_s22_8$ty_minus1, sep="_")
data_7_s22_8[,c(46:47,50:61,64:69)] <- NA

data_7_s22_9 <- data_7_s22[data_7_s22$terminal_year == 2012 | data_7_s22$terminal_year == 2015,]
data_7_s22_9$ty_order <- 9
data_7_s22_9$if_diff_5yrs <- "N"
data_7_s22_9$ty <- 2012
data_7_s22_9$ty_minus1 <- data_7_s22_9$ty-1
data_7_s22_9$key_ty <- paste(data_7_s22_9$assessid, data_7_s22_9$ty, sep="_")
data_7_s22_9$key_ty_minus1 <- paste(data_7_s22_9$assessid, data_7_s22_9$ty_minus1, sep="_")
data_7_s22_9[,c(46:47,50:61,64:69)] <- NA

data_7_s22_10 <- data_7_s22[data_7_s22$terminal_year == 2012 | data_7_s22$terminal_year == 2016,]
data_7_s22_10$ty_order <- 10
data_7_s22_10$if_diff_5yrs <- "N"
data_7_s22_10$ty <- 2012
data_7_s22_10$ty_minus1 <- data_7_s22_10$ty-1
data_7_s22_10$key_ty <- paste(data_7_s22_10$assessid, data_7_s22_10$ty, sep="_")
data_7_s22_10$key_ty_minus1 <- paste(data_7_s22_10$assessid, data_7_s22_10$ty_minus1, sep="_")
data_7_s22_10[,c(46:47,50:61,64:69)] <- NA

data_7_s22_11 <- data_7_s22[data_7_s22$terminal_year == 2012 | data_7_s22$terminal_year == 2017,]
data_7_s22_11$ty_order <- 11
data_7_s22_11$if_diff_5yrs <- "N"
data_7_s22_11$ty <- 2012
data_7_s22_11$ty_minus1 <- data_7_s22_11$ty-1
data_7_s22_11$key_ty <- paste(data_7_s22_11$assessid, data_7_s22_11$ty, sep="_")
data_7_s22_11$key_ty_minus1 <- paste(data_7_s22_11$assessid, data_7_s22_11$ty_minus1, sep="_")
data_7_s22_11[,c(46:47,50:61,64:69)] <- NA

data_7_s22_12 <- data_7_s22[data_7_s22$terminal_year == 2013 | data_7_s22$terminal_year == 2014,]
data_7_s22_12$ty_order <- 12
data_7_s22_12$if_diff_5yrs <- "N"
data_7_s22_12$ty <- 2013
data_7_s22_12$ty_minus1 <- data_7_s22_12$ty-1
data_7_s22_12$key_ty <- paste(data_7_s22_12$assessid, data_7_s22_12$ty, sep="_")
data_7_s22_12$key_ty_minus1 <- paste(data_7_s22_12$assessid, data_7_s22_12$ty_minus1, sep="_")
data_7_s22_12[,c(46:47,50:61,64:69)] <- NA

data_7_s22_13 <- data_7_s22[data_7_s22$terminal_year == 2013 | data_7_s22$terminal_year == 2015,]
data_7_s22_13$ty_order <- 13
data_7_s22_13$if_diff_5yrs <- "N"
data_7_s22_13$ty <- 2013
data_7_s22_13$ty_minus1 <- data_7_s22_13$ty-1
data_7_s22_13$key_ty <- paste(data_7_s22_13$assessid, data_7_s22_13$ty, sep="_")
data_7_s22_13$key_ty_minus1 <- paste(data_7_s22_13$assessid, data_7_s22_13$ty_minus1, sep="_")
data_7_s22_13[,c(46:47,50:61,64:69)] <- NA

data_7_s22_14 <- data_7_s22[data_7_s22$terminal_year == 2013 | data_7_s22$terminal_year == 2016,]
data_7_s22_14$ty_order <- 14
data_7_s22_14$if_diff_5yrs <- "N"
data_7_s22_14$ty <- 2013
data_7_s22_14$ty_minus1 <- data_7_s22_14$ty-1
data_7_s22_14$key_ty <- paste(data_7_s22_14$assessid, data_7_s22_14$ty, sep="_")
data_7_s22_14$key_ty_minus1 <- paste(data_7_s22_14$assessid, data_7_s22_14$ty_minus1, sep="_")
data_7_s22_14[,c(46:47,50:61,64:69)] <- NA

data_7_s22_15 <- data_7_s22[data_7_s22$terminal_year == 2013 | data_7_s22$terminal_year == 2017,]
data_7_s22_15$ty_order <- 15
data_7_s22_15$if_diff_5yrs <- "N"
data_7_s22_15$ty <- 2013
data_7_s22_15$ty_minus1 <- data_7_s22_15$ty-1
data_7_s22_15$key_ty <- paste(data_7_s22_15$assessid, data_7_s22_15$ty, sep="_")
data_7_s22_15$key_ty_minus1 <- paste(data_7_s22_15$assessid, data_7_s22_15$ty_minus1, sep="_")
data_7_s22_15[,c(46:47,50:61,64:69)] <- NA

data_7_s22_16 <- data_7_s22[data_7_s22$terminal_year == 2014 | data_7_s22$terminal_year == 2015,]
data_7_s22_16$ty_order <- 16
data_7_s22_16$if_diff_5yrs <- "N"
data_7_s22_16$ty <- 2014
data_7_s22_16$ty_minus1 <- data_7_s22_16$ty-1
data_7_s22_16$key_ty <- paste(data_7_s22_16$assessid, data_7_s22_16$ty, sep="_")
data_7_s22_16$key_ty_minus1 <- paste(data_7_s22_16$assessid, data_7_s22_16$ty_minus1, sep="_")
data_7_s22_16[,c(46:47,50:61,64:69)] <- NA

data_7_s22_17 <- data_7_s22[data_7_s22$terminal_year == 2014 | data_7_s22$terminal_year == 2016,]
data_7_s22_17$ty_order <- 17
data_7_s22_17$if_diff_5yrs <- "N"
data_7_s22_17$ty <- 2014
data_7_s22_17$ty_minus1 <- data_7_s22_17$ty-1
data_7_s22_17$key_ty <- paste(data_7_s22_17$assessid, data_7_s22_17$ty, sep="_")
data_7_s22_17$key_ty_minus1 <- paste(data_7_s22_17$assessid, data_7_s22_17$ty_minus1, sep="_")
data_7_s22_17[,c(46:47,50:61,64:69)] <- NA

data_7_s22_18 <- data_7_s22[data_7_s22$terminal_year == 2014 | data_7_s22$terminal_year == 2017,]
data_7_s22_18$ty_order <- 18
data_7_s22_18$if_diff_5yrs <- "N"
data_7_s22_18$ty <- 2014
data_7_s22_18$ty_minus1 <- data_7_s22_18$ty-1
data_7_s22_18$key_ty <- paste(data_7_s22_18$assessid, data_7_s22_18$ty, sep="_")
data_7_s22_18$key_ty_minus1 <- paste(data_7_s22_18$assessid, data_7_s22_18$ty_minus1, sep="_")
data_7_s22_18[,c(46:47,50:61,64:69)] <- NA

data_7_s22_19 <- data_7_s22[data_7_s22$terminal_year == 2015 | data_7_s22$terminal_year == 2016,]
data_7_s22_19$ty_order <- 19
data_7_s22_19$if_diff_5yrs <- "N"
data_7_s22_19$ty <- 2015
data_7_s22_19$ty_minus1 <- data_7_s22_19$ty-1
data_7_s22_19$key_ty <- paste(data_7_s22_19$assessid, data_7_s22_19$ty, sep="_")
data_7_s22_19$key_ty_minus1 <- paste(data_7_s22_19$assessid, data_7_s22_19$ty_minus1, sep="_")
data_7_s22_19[,c(46:47,50:61,64:69)] <- NA

data_7_s22_20 <- data_7_s22[data_7_s22$terminal_year == 2015 | data_7_s22$terminal_year == 2017,]
data_7_s22_20$ty_order <- 20
data_7_s22_20$if_diff_5yrs <- "N"
data_7_s22_20$ty <- 2015
data_7_s22_20$ty_minus1 <- data_7_s22_20$ty-1
data_7_s22_20$key_ty <- paste(data_7_s22_20$assessid, data_7_s22_20$ty, sep="_")
data_7_s22_20$key_ty_minus1 <- paste(data_7_s22_20$assessid, data_7_s22_20$ty_minus1, sep="_")
data_7_s22_20[,c(46:47,50:61,64:69)] <- NA

data_7_s22_21 <- data_7_s22[data_7_s22$terminal_year == 2016 | data_7_s22$terminal_year == 2017,]
data_7_s22_21$ty_order <- 21
data_7_s22_21$if_diff_5yrs <- "N"
data_7_s22_21$ty <- 2016
data_7_s22_21$ty_minus1 <- data_7_s22_21$ty-1
data_7_s22_21$key_ty <- paste(data_7_s22_21$assessid, data_7_s22_21$ty, sep="_")
data_7_s22_21$key_ty_minus1 <- paste(data_7_s22_21$assessid, data_7_s22_21$ty_minus1, sep="_")
data_7_s22_21[,c(46:47,50:61,64:69)] <- NA

data_7_s22_1$interval <- max(data_7_s22_1$terminal_year) - min(data_7_s22_1$terminal_year)
data_7_s22_2$interval <- max(data_7_s22_2$terminal_year) - min(data_7_s22_2$terminal_year)
data_7_s22_3$interval <- max(data_7_s22_3$terminal_year) - min(data_7_s22_3$terminal_year)
data_7_s22_4$interval <- max(data_7_s22_4$terminal_year) - min(data_7_s22_4$terminal_year)
data_7_s22_5$interval <- max(data_7_s22_5$terminal_year) - min(data_7_s22_5$terminal_year)
data_7_s22_6$interval <- max(data_7_s22_6$terminal_year) - min(data_7_s22_6$terminal_year)
data_7_s22_7$interval <- max(data_7_s22_7$terminal_year) - min(data_7_s22_7$terminal_year)
data_7_s22_8$interval <- max(data_7_s22_8$terminal_year) - min(data_7_s22_8$terminal_year)
data_7_s22_9$interval <- max(data_7_s22_9$terminal_year) - min(data_7_s22_9$terminal_year)
data_7_s22_10$interval <- max(data_7_s22_10$terminal_year) - min(data_7_s22_10$terminal_year)
data_7_s22_11$interval <- max(data_7_s22_11$terminal_year) - min(data_7_s22_11$terminal_year)
data_7_s22_12$interval <- max(data_7_s22_12$terminal_year) - min(data_7_s22_12$terminal_year)
data_7_s22_13$interval <- max(data_7_s22_13$terminal_year) - min(data_7_s22_13$terminal_year)
data_7_s22_14$interval <- max(data_7_s22_14$terminal_year) - min(data_7_s22_14$terminal_year)
data_7_s22_15$interval <- max(data_7_s22_15$terminal_year) - min(data_7_s22_15$terminal_year)
data_7_s22_16$interval <- max(data_7_s22_16$terminal_year) - min(data_7_s22_16$terminal_year)
data_7_s22_17$interval <- max(data_7_s22_17$terminal_year) - min(data_7_s22_17$terminal_year)
data_7_s22_18$interval <- max(data_7_s22_18$terminal_year) - min(data_7_s22_18$terminal_year)
data_7_s22_19$interval <- max(data_7_s22_19$terminal_year) - min(data_7_s22_19$terminal_year)
data_7_s22_20$interval <- max(data_7_s22_20$terminal_year) - min(data_7_s22_20$terminal_year)
data_7_s22_21$interval <- max(data_7_s22_21$terminal_year) - min(data_7_s22_21$terminal_year)

data_7_s22 <- rbind(data_7_s22_1, data_7_s22_2, data_7_s22_3, data_7_s22_4, data_7_s22_5, data_7_s22_6, data_7_s22_7, data_7_s22_8, data_7_s22_9, data_7_s22_10,
                    data_7_s22_11, data_7_s22_12, data_7_s22_13, data_7_s22_14, data_7_s22_15, data_7_s22_16, data_7_s22_17, data_7_s22_18, data_7_s22_19,
                    data_7_s22_20, data_7_s22_21)

# assess 23
data_7_s23 <- data_7_assess[data_7_assess$stockid==ids[23],]

data_7_s23_1 <- data_7_s23[data_7_s23$terminal_year == 2010 | data_7_s23$terminal_year == 2012,]
data_7_s23_1$ty_order <- 1
data_7_s23_1$if_diff_5yrs <- "N"

data_7_s23_2 <- data_7_s23[data_7_s23$terminal_year == 2010 | data_7_s23$terminal_year == 2013,]
data_7_s23_2$ty_order <- 2
data_7_s23_2$if_diff_5yrs <- "N"

data_7_s23_3 <- data_7_s23[data_7_s23$terminal_year == 2010 | data_7_s23$terminal_year == 2014,]
data_7_s23_3$ty_order <- 3
data_7_s23_3$if_diff_5yrs <- "N"

data_7_s23_4 <- data_7_s23[data_7_s23$terminal_year == 2010 | data_7_s23$terminal_year == 2015,]
data_7_s23_4$ty_order <- 4
data_7_s23_4$if_diff_5yrs <- "N"

data_7_s23_5 <- data_7_s23[data_7_s23$terminal_year == 2010 | data_7_s23$terminal_year == 2016,]
data_7_s23_5$ty_order <- 5
data_7_s23_5$if_diff_5yrs <- "Y"

data_7_s23_6 <- data_7_s23[data_7_s23$terminal_year == 2010 | data_7_s23$terminal_year == 2017,]
data_7_s23_6$ty_order <- 6
data_7_s23_6$if_diff_5yrs <- "Y"

data_7_s23_7 <- data_7_s23[data_7_s23$terminal_year == 2012 | data_7_s23$terminal_year == 2013,]
data_7_s23_7$ty_order <- 7
data_7_s23_7$if_diff_5yrs <- "N"
data_7_s23_7$ty <- 2012
data_7_s23_7$ty_minus1 <- data_7_s23_7$ty-1
data_7_s23_7$key_ty <- paste(data_7_s23_7$assessid, data_7_s23_7$ty, sep="_")
data_7_s23_7$key_ty_minus1 <- paste(data_7_s23_7$assessid, data_7_s23_7$ty_minus1, sep="_")
data_7_s23_7[,c(46:47,50:61,64:69)] <- NA

data_7_s23_8 <- data_7_s23[data_7_s23$terminal_year == 2012 | data_7_s23$terminal_year == 2014,]
data_7_s23_8$ty_order <- 8
data_7_s23_8$if_diff_5yrs <- "N"
data_7_s23_8$ty <- 2012
data_7_s23_8$ty_minus1 <- data_7_s23_8$ty-1
data_7_s23_8$key_ty <- paste(data_7_s23_8$assessid, data_7_s23_8$ty, sep="_")
data_7_s23_8$key_ty_minus1 <- paste(data_7_s23_8$assessid, data_7_s23_8$ty_minus1, sep="_")
data_7_s23_8[,c(46:47,50:61,64:69)] <- NA

data_7_s23_9 <- data_7_s23[data_7_s23$terminal_year == 2012 | data_7_s23$terminal_year == 2015,]
data_7_s23_9$ty_order <- 9
data_7_s23_9$if_diff_5yrs <- "N"
data_7_s23_9$ty <- 2012
data_7_s23_9$ty_minus1 <- data_7_s23_9$ty-1
data_7_s23_9$key_ty <- paste(data_7_s23_9$assessid, data_7_s23_9$ty, sep="_")
data_7_s23_9$key_ty_minus1 <- paste(data_7_s23_9$assessid, data_7_s23_9$ty_minus1, sep="_")
data_7_s23_9[,c(46:47,50:61,64:69)] <- NA

data_7_s23_10 <- data_7_s23[data_7_s23$terminal_year == 2012 | data_7_s23$terminal_year == 2016,]
data_7_s23_10$ty_order <- 10
data_7_s23_10$if_diff_5yrs <- "N"
data_7_s23_10$ty <- 2012
data_7_s23_10$ty_minus1 <- data_7_s23_10$ty-1
data_7_s23_10$key_ty <- paste(data_7_s23_10$assessid, data_7_s23_10$ty, sep="_")
data_7_s23_10$key_ty_minus1 <- paste(data_7_s23_10$assessid, data_7_s23_10$ty_minus1, sep="_")
data_7_s23_10[,c(46:47,50:61,64:69)] <- NA

data_7_s23_11 <- data_7_s23[data_7_s23$terminal_year == 2012 | data_7_s23$terminal_year == 2017,]
data_7_s23_11$ty_order <- 11
data_7_s23_11$if_diff_5yrs <- "N"
data_7_s23_11$ty <- 2012
data_7_s23_11$ty_minus1 <- data_7_s23_11$ty-1
data_7_s23_11$key_ty <- paste(data_7_s23_11$assessid, data_7_s23_11$ty, sep="_")
data_7_s23_11$key_ty_minus1 <- paste(data_7_s23_11$assessid, data_7_s23_11$ty_minus1, sep="_")
data_7_s23_11[,c(46:47,50:61,64:69)] <- NA

data_7_s23_12 <- data_7_s23[data_7_s23$terminal_year == 2013 | data_7_s23$terminal_year == 2014,]
data_7_s23_12$ty_order <- 12
data_7_s23_12$if_diff_5yrs <- "N"
data_7_s23_12$ty <- 2013
data_7_s23_12$ty_minus1 <- data_7_s23_12$ty-1
data_7_s23_12$key_ty <- paste(data_7_s23_12$assessid, data_7_s23_12$ty, sep="_")
data_7_s23_12$key_ty_minus1 <- paste(data_7_s23_12$assessid, data_7_s23_12$ty_minus1, sep="_")
data_7_s23_12[,c(46:47,50:61,64:69)] <- NA

data_7_s23_13 <- data_7_s23[data_7_s23$terminal_year == 2013 | data_7_s23$terminal_year == 2015,]
data_7_s23_13$ty_order <- 13
data_7_s23_13$if_diff_5yrs <- "N"
data_7_s23_13$ty <- 2013
data_7_s23_13$ty_minus1 <- data_7_s23_13$ty-1
data_7_s23_13$key_ty <- paste(data_7_s23_13$assessid, data_7_s23_13$ty, sep="_")
data_7_s23_13$key_ty_minus1 <- paste(data_7_s23_13$assessid, data_7_s23_13$ty_minus1, sep="_")
data_7_s23_13[,c(46:47,50:61,64:69)] <- NA

data_7_s23_14 <- data_7_s23[data_7_s23$terminal_year == 2013 | data_7_s23$terminal_year == 2016,]
data_7_s23_14$ty_order <- 14
data_7_s23_14$if_diff_5yrs <- "N"
data_7_s23_14$ty <- 2013
data_7_s23_14$ty_minus1 <- data_7_s23_14$ty-1
data_7_s23_14$key_ty <- paste(data_7_s23_14$assessid, data_7_s23_14$ty, sep="_")
data_7_s23_14$key_ty_minus1 <- paste(data_7_s23_14$assessid, data_7_s23_14$ty_minus1, sep="_")
data_7_s23_14[,c(46:47,50:61,64:69)] <- NA

data_7_s23_15 <- data_7_s23[data_7_s23$terminal_year == 2013 | data_7_s23$terminal_year == 2017,]
data_7_s23_15$ty_order <- 15
data_7_s23_15$if_diff_5yrs <- "N"
data_7_s23_15$ty <- 2013
data_7_s23_15$ty_minus1 <- data_7_s23_15$ty-1
data_7_s23_15$key_ty <- paste(data_7_s23_15$assessid, data_7_s23_15$ty, sep="_")
data_7_s23_15$key_ty_minus1 <- paste(data_7_s23_15$assessid, data_7_s23_15$ty_minus1, sep="_")
data_7_s23_15[,c(46:47,50:61,64:69)] <- NA

data_7_s23_16 <- data_7_s23[data_7_s23$terminal_year == 2014 | data_7_s23$terminal_year == 2015,]
data_7_s23_16$ty_order <- 16
data_7_s23_16$if_diff_5yrs <- "N"
data_7_s23_16$ty <- 2014
data_7_s23_16$ty_minus1 <- data_7_s23_16$ty-1
data_7_s23_16$key_ty <- paste(data_7_s23_16$assessid, data_7_s23_16$ty, sep="_")
data_7_s23_16$key_ty_minus1 <- paste(data_7_s23_16$assessid, data_7_s23_16$ty_minus1, sep="_")
data_7_s23_16[,c(46:47,50:61,64:69)] <- NA

data_7_s23_17 <- data_7_s23[data_7_s23$terminal_year == 2014 | data_7_s23$terminal_year == 2016,]
data_7_s23_17$ty_order <- 17
data_7_s23_17$if_diff_5yrs <- "N"
data_7_s23_17$ty <- 2014
data_7_s23_17$ty_minus1 <- data_7_s23_17$ty-1
data_7_s23_17$key_ty <- paste(data_7_s23_17$assessid, data_7_s23_17$ty, sep="_")
data_7_s23_17$key_ty_minus1 <- paste(data_7_s23_17$assessid, data_7_s23_17$ty_minus1, sep="_")
data_7_s23_17[,c(46:47,50:61,64:69)] <- NA

data_7_s23_18 <- data_7_s23[data_7_s23$terminal_year == 2014 | data_7_s23$terminal_year == 2017,]
data_7_s23_18$ty_order <- 18
data_7_s23_18$if_diff_5yrs <- "N"
data_7_s23_18$ty <- 2014
data_7_s23_18$ty_minus1 <- data_7_s23_18$ty-1
data_7_s23_18$key_ty <- paste(data_7_s23_18$assessid, data_7_s23_18$ty, sep="_")
data_7_s23_18$key_ty_minus1 <- paste(data_7_s23_18$assessid, data_7_s23_18$ty_minus1, sep="_")
data_7_s23_18[,c(46:47,50:61,64:69)] <- NA

data_7_s23_19 <- data_7_s23[data_7_s23$terminal_year == 2015 | data_7_s23$terminal_year == 2016,]
data_7_s23_19$ty_order <- 19
data_7_s23_19$if_diff_5yrs <- "N"
data_7_s23_19$ty <- 2015
data_7_s23_19$ty_minus1 <- data_7_s23_19$ty-1
data_7_s23_19$key_ty <- paste(data_7_s23_19$assessid, data_7_s23_19$ty, sep="_")
data_7_s23_19$key_ty_minus1 <- paste(data_7_s23_19$assessid, data_7_s23_19$ty_minus1, sep="_")
data_7_s23_19[,c(46:47,50:61,64:69)] <- NA

data_7_s23_20 <- data_7_s23[data_7_s23$terminal_year == 2015 | data_7_s23$terminal_year == 2017,]
data_7_s23_20$ty_order <- 20
data_7_s23_20$if_diff_5yrs <- "N"
data_7_s23_20$ty <- 2015
data_7_s23_20$ty_minus1 <- data_7_s23_20$ty-1
data_7_s23_20$key_ty <- paste(data_7_s23_20$assessid, data_7_s23_20$ty, sep="_")
data_7_s23_20$key_ty_minus1 <- paste(data_7_s23_20$assessid, data_7_s23_20$ty_minus1, sep="_")
data_7_s23_20[,c(46:47,50:61,64:69)] <- NA

data_7_s23_21 <- data_7_s23[data_7_s23$terminal_year == 2016 | data_7_s23$terminal_year == 2017,]
data_7_s23_21$ty_order <- 21
data_7_s23_21$if_diff_5yrs <- "N"
data_7_s23_21$ty <- 2016
data_7_s23_21$ty_minus1 <- data_7_s23_21$ty-1
data_7_s23_21$key_ty <- paste(data_7_s23_21$assessid, data_7_s23_21$ty, sep="_")
data_7_s23_21$key_ty_minus1 <- paste(data_7_s23_21$assessid, data_7_s23_21$ty_minus1, sep="_")
data_7_s23_21[,c(46:47,50:61,64:69)] <- NA

data_7_s23_1$interval <- max(data_7_s23_1$terminal_year) - min(data_7_s23_1$terminal_year)
data_7_s23_2$interval <- max(data_7_s23_2$terminal_year) - min(data_7_s23_2$terminal_year)
data_7_s23_3$interval <- max(data_7_s23_3$terminal_year) - min(data_7_s23_3$terminal_year)
data_7_s23_4$interval <- max(data_7_s23_4$terminal_year) - min(data_7_s23_4$terminal_year)
data_7_s23_5$interval <- max(data_7_s23_5$terminal_year) - min(data_7_s23_5$terminal_year)
data_7_s23_6$interval <- max(data_7_s23_6$terminal_year) - min(data_7_s23_6$terminal_year)
data_7_s23_7$interval <- max(data_7_s23_7$terminal_year) - min(data_7_s23_7$terminal_year)
data_7_s23_8$interval <- max(data_7_s23_8$terminal_year) - min(data_7_s23_8$terminal_year)
data_7_s23_9$interval <- max(data_7_s23_9$terminal_year) - min(data_7_s23_9$terminal_year)
data_7_s23_10$interval <- max(data_7_s23_10$terminal_year) - min(data_7_s23_10$terminal_year)
data_7_s23_11$interval <- max(data_7_s23_11$terminal_year) - min(data_7_s23_11$terminal_year)
data_7_s23_12$interval <- max(data_7_s23_12$terminal_year) - min(data_7_s23_12$terminal_year)
data_7_s23_13$interval <- max(data_7_s23_13$terminal_year) - min(data_7_s23_13$terminal_year)
data_7_s23_14$interval <- max(data_7_s23_14$terminal_year) - min(data_7_s23_14$terminal_year)
data_7_s23_15$interval <- max(data_7_s23_15$terminal_year) - min(data_7_s23_15$terminal_year)
data_7_s23_16$interval <- max(data_7_s23_16$terminal_year) - min(data_7_s23_16$terminal_year)
data_7_s23_17$interval <- max(data_7_s23_17$terminal_year) - min(data_7_s23_17$terminal_year)
data_7_s23_18$interval <- max(data_7_s23_18$terminal_year) - min(data_7_s23_18$terminal_year)
data_7_s23_19$interval <- max(data_7_s23_19$terminal_year) - min(data_7_s23_19$terminal_year)
data_7_s23_20$interval <- max(data_7_s23_20$terminal_year) - min(data_7_s23_20$terminal_year)
data_7_s23_21$interval <- max(data_7_s23_21$terminal_year) - min(data_7_s23_21$terminal_year)

data_7_s23 <- rbind(data_7_s23_1, data_7_s23_2, data_7_s23_3, data_7_s23_4, data_7_s23_5, data_7_s23_6, data_7_s23_7, data_7_s23_8, data_7_s23_9, data_7_s23_10,
                    data_7_s23_11, data_7_s23_12, data_7_s23_13, data_7_s23_14, data_7_s23_15, data_7_s23_16, data_7_s23_17, data_7_s23_18, data_7_s23_19,
                    data_7_s23_20, data_7_s23_21)

# assess 24
data_7_s24 <- data_7_assess[data_7_assess$stockid==ids[24],]

data_7_s24_1 <- data_7_s24[data_7_s24$terminal_year == 2011 | data_7_s24$terminal_year == 2012,]
data_7_s24_1$ty_order <- 1
data_7_s24_1$if_diff_5yrs <- "N"

data_7_s24_2 <- data_7_s24[data_7_s24$terminal_year == 2011 | data_7_s24$terminal_year == 2013,]
data_7_s24_2$ty_order <- 2
data_7_s24_2$if_diff_5yrs <- "N"

data_7_s24_3 <- data_7_s24[data_7_s24$terminal_year == 2011 | data_7_s24$terminal_year == 2014,]
data_7_s24_3$ty_order <- 3
data_7_s24_3$if_diff_5yrs <- "N"

data_7_s24_4 <- data_7_s24[data_7_s24$terminal_year == 2011 | data_7_s24$terminal_year == 2015,]
data_7_s24_4$ty_order <- 4
data_7_s24_4$if_diff_5yrs <- "N"

data_7_s24_5 <- data_7_s24[data_7_s24$terminal_year == 2011 | data_7_s24$terminal_year == 2016,]
data_7_s24_5$ty_order <- 5
data_7_s24_5$if_diff_5yrs <- "N"

data_7_s24_6 <- data_7_s24[data_7_s24$terminal_year == 2011 | data_7_s24$terminal_year == 2017,]
data_7_s24_6$ty_order <- 6
data_7_s24_6$if_diff_5yrs <- "Y"

data_7_s24_7 <- data_7_s24[data_7_s24$terminal_year == 2012 | data_7_s24$terminal_year == 2013,]
data_7_s24_7$ty_order <- 7
data_7_s24_7$if_diff_5yrs <- "N"
data_7_s24_7$ty <- 2012
data_7_s24_7$ty_minus1 <- data_7_s24_7$ty-1
data_7_s24_7$key_ty <- paste(data_7_s24_7$assessid, data_7_s24_7$ty, sep="_")
data_7_s24_7$key_ty_minus1 <- paste(data_7_s24_7$assessid, data_7_s24_7$ty_minus1, sep="_")
data_7_s24_7[,c(46:47,50:61,64:69)] <- NA

data_7_s24_8 <- data_7_s24[data_7_s24$terminal_year == 2012 | data_7_s24$terminal_year == 2014,]
data_7_s24_8$ty_order <- 8
data_7_s24_8$if_diff_5yrs <- "N"
data_7_s24_8$ty <- 2012
data_7_s24_8$ty_minus1 <- data_7_s24_8$ty-1
data_7_s24_8$key_ty <- paste(data_7_s24_8$assessid, data_7_s24_8$ty, sep="_")
data_7_s24_8$key_ty_minus1 <- paste(data_7_s24_8$assessid, data_7_s24_8$ty_minus1, sep="_")
data_7_s24_8[,c(46:47,50:61,64:69)] <- NA

data_7_s24_9 <- data_7_s24[data_7_s24$terminal_year == 2012 | data_7_s24$terminal_year == 2015,]
data_7_s24_9$ty_order <- 9
data_7_s24_9$if_diff_5yrs <- "N"
data_7_s24_9$ty <- 2012
data_7_s24_9$ty_minus1 <- data_7_s24_9$ty-1
data_7_s24_9$key_ty <- paste(data_7_s24_9$assessid, data_7_s24_9$ty, sep="_")
data_7_s24_9$key_ty_minus1 <- paste(data_7_s24_9$assessid, data_7_s24_9$ty_minus1, sep="_")
data_7_s24_9[,c(46:47,50:61,64:69)] <- NA

data_7_s24_10 <- data_7_s24[data_7_s24$terminal_year == 2012 | data_7_s24$terminal_year == 2016,]
data_7_s24_10$ty_order <- 10
data_7_s24_10$if_diff_5yrs <- "N"
data_7_s24_10$ty <- 2012
data_7_s24_10$ty_minus1 <- data_7_s24_10$ty-1
data_7_s24_10$key_ty <- paste(data_7_s24_10$assessid, data_7_s24_10$ty, sep="_")
data_7_s24_10$key_ty_minus1 <- paste(data_7_s24_10$assessid, data_7_s24_10$ty_minus1, sep="_")
data_7_s24_10[,c(46:47,50:61,64:69)] <- NA

data_7_s24_11 <- data_7_s24[data_7_s24$terminal_year == 2012 | data_7_s24$terminal_year == 2017,]
data_7_s24_11$ty_order <- 11
data_7_s24_11$if_diff_5yrs <- "N"
data_7_s24_11$ty <- 2012
data_7_s24_11$ty_minus1 <- data_7_s24_11$ty-1
data_7_s24_11$key_ty <- paste(data_7_s24_11$assessid, data_7_s24_11$ty, sep="_")
data_7_s24_11$key_ty_minus1 <- paste(data_7_s24_11$assessid, data_7_s24_11$ty_minus1, sep="_")
data_7_s24_11[,c(46:47,50:61,64:69)] <- NA

data_7_s24_12 <- data_7_s24[data_7_s24$terminal_year == 2013 | data_7_s24$terminal_year == 2014,]
data_7_s24_12$ty_order <- 12
data_7_s24_12$if_diff_5yrs <- "N"
data_7_s24_12$ty <- 2013
data_7_s24_12$ty_minus1 <- data_7_s24_12$ty-1
data_7_s24_12$key_ty <- paste(data_7_s24_12$assessid, data_7_s24_12$ty, sep="_")
data_7_s24_12$key_ty_minus1 <- paste(data_7_s24_12$assessid, data_7_s24_12$ty_minus1, sep="_")
data_7_s24_12[,c(46:47,50:61,64:69)] <- NA

data_7_s24_13 <- data_7_s24[data_7_s24$terminal_year == 2013 | data_7_s24$terminal_year == 2015,]
data_7_s24_13$ty_order <- 13
data_7_s24_13$if_diff_5yrs <- "N"
data_7_s24_13$ty <- 2013
data_7_s24_13$ty_minus1 <- data_7_s24_13$ty-1
data_7_s24_13$key_ty <- paste(data_7_s24_13$assessid, data_7_s24_13$ty, sep="_")
data_7_s24_13$key_ty_minus1 <- paste(data_7_s24_13$assessid, data_7_s24_13$ty_minus1, sep="_")
data_7_s24_13[,c(46:47,50:61,64:69)] <- NA

data_7_s24_14 <- data_7_s24[data_7_s24$terminal_year == 2013 | data_7_s24$terminal_year == 2016,]
data_7_s24_14$ty_order <- 14
data_7_s24_14$if_diff_5yrs <- "N"
data_7_s24_14$ty <- 2013
data_7_s24_14$ty_minus1 <- data_7_s24_14$ty-1
data_7_s24_14$key_ty <- paste(data_7_s24_14$assessid, data_7_s24_14$ty, sep="_")
data_7_s24_14$key_ty_minus1 <- paste(data_7_s24_14$assessid, data_7_s24_14$ty_minus1, sep="_")
data_7_s24_14[,c(46:47,50:61,64:69)] <- NA

data_7_s24_15 <- data_7_s24[data_7_s24$terminal_year == 2013 | data_7_s24$terminal_year == 2017,]
data_7_s24_15$ty_order <- 15
data_7_s24_15$if_diff_5yrs <- "N"
data_7_s24_15$ty <- 2013
data_7_s24_15$ty_minus1 <- data_7_s24_15$ty-1
data_7_s24_15$key_ty <- paste(data_7_s24_15$assessid, data_7_s24_15$ty, sep="_")
data_7_s24_15$key_ty_minus1 <- paste(data_7_s24_15$assessid, data_7_s24_15$ty_minus1, sep="_")
data_7_s24_15[,c(46:47,50:61,64:69)] <- NA

data_7_s24_16 <- data_7_s24[data_7_s24$terminal_year == 2014 | data_7_s24$terminal_year == 2015,]
data_7_s24_16$ty_order <- 16
data_7_s24_16$if_diff_5yrs <- "N"
data_7_s24_16$ty <- 2014
data_7_s24_16$ty_minus1 <- data_7_s24_16$ty-1
data_7_s24_16$key_ty <- paste(data_7_s24_16$assessid, data_7_s24_16$ty, sep="_")
data_7_s24_16$key_ty_minus1 <- paste(data_7_s24_16$assessid, data_7_s24_16$ty_minus1, sep="_")
data_7_s24_16[,c(46:47,50:61,64:69)] <- NA

data_7_s24_17 <- data_7_s24[data_7_s24$terminal_year == 2014 | data_7_s24$terminal_year == 2016,]
data_7_s24_17$ty_order <- 17
data_7_s24_17$if_diff_5yrs <- "N"
data_7_s24_17$ty <- 2014
data_7_s24_17$ty_minus1 <- data_7_s24_17$ty-1
data_7_s24_17$key_ty <- paste(data_7_s24_17$assessid, data_7_s24_17$ty, sep="_")
data_7_s24_17$key_ty_minus1 <- paste(data_7_s24_17$assessid, data_7_s24_17$ty_minus1, sep="_")
data_7_s24_17[,c(46:47,50:61,64:69)] <- NA

data_7_s24_18 <- data_7_s24[data_7_s24$terminal_year == 2014 | data_7_s24$terminal_year == 2017,]
data_7_s24_18$ty_order <- 18
data_7_s24_18$if_diff_5yrs <- "N"
data_7_s24_18$ty <- 2014
data_7_s24_18$ty_minus1 <- data_7_s24_18$ty-1
data_7_s24_18$key_ty <- paste(data_7_s24_18$assessid, data_7_s24_18$ty, sep="_")
data_7_s24_18$key_ty_minus1 <- paste(data_7_s24_18$assessid, data_7_s24_18$ty_minus1, sep="_")
data_7_s24_18[,c(46:47,50:61,64:69)] <- NA

data_7_s24_19 <- data_7_s24[data_7_s24$terminal_year == 2015 | data_7_s24$terminal_year == 2016,]
data_7_s24_19$ty_order <- 19
data_7_s24_19$if_diff_5yrs <- "N"
data_7_s24_19$ty <- 2015
data_7_s24_19$ty_minus1 <- data_7_s24_19$ty-1
data_7_s24_19$key_ty <- paste(data_7_s24_19$assessid, data_7_s24_19$ty, sep="_")
data_7_s24_19$key_ty_minus1 <- paste(data_7_s24_19$assessid, data_7_s24_19$ty_minus1, sep="_")
data_7_s24_19[,c(46:47,50:61,64:69)] <- NA

data_7_s24_20 <- data_7_s24[data_7_s24$terminal_year == 2015 | data_7_s24$terminal_year == 2017,]
data_7_s24_20$ty_order <- 20
data_7_s24_20$if_diff_5yrs <- "N"
data_7_s24_20$ty <- 2015
data_7_s24_20$ty_minus1 <- data_7_s24_20$ty-1
data_7_s24_20$key_ty <- paste(data_7_s24_20$assessid, data_7_s24_20$ty, sep="_")
data_7_s24_20$key_ty_minus1 <- paste(data_7_s24_20$assessid, data_7_s24_20$ty_minus1, sep="_")
data_7_s24_20[,c(46:47,50:61,64:69)] <- NA

data_7_s24_21 <- data_7_s24[data_7_s24$terminal_year == 2016 | data_7_s24$terminal_year == 2017,]
data_7_s24_21$ty_order <- 21
data_7_s24_21$if_diff_5yrs <- "N"
data_7_s24_21$ty <- 2016
data_7_s24_21$ty_minus1 <- data_7_s24_21$ty-1
data_7_s24_21$key_ty <- paste(data_7_s24_21$assessid, data_7_s24_21$ty, sep="_")
data_7_s24_21$key_ty_minus1 <- paste(data_7_s24_21$assessid, data_7_s24_21$ty_minus1, sep="_")
data_7_s24_21[,c(46:47,50:61,64:69)] <- NA

data_7_s24_1$interval <- max(data_7_s24_1$terminal_year) - min(data_7_s24_1$terminal_year)
data_7_s24_2$interval <- max(data_7_s24_2$terminal_year) - min(data_7_s24_2$terminal_year)
data_7_s24_3$interval <- max(data_7_s24_3$terminal_year) - min(data_7_s24_3$terminal_year)
data_7_s24_4$interval <- max(data_7_s24_4$terminal_year) - min(data_7_s24_4$terminal_year)
data_7_s24_5$interval <- max(data_7_s24_5$terminal_year) - min(data_7_s24_5$terminal_year)
data_7_s24_6$interval <- max(data_7_s24_6$terminal_year) - min(data_7_s24_6$terminal_year)
data_7_s24_7$interval <- max(data_7_s24_7$terminal_year) - min(data_7_s24_7$terminal_year)
data_7_s24_8$interval <- max(data_7_s24_8$terminal_year) - min(data_7_s24_8$terminal_year)
data_7_s24_9$interval <- max(data_7_s24_9$terminal_year) - min(data_7_s24_9$terminal_year)
data_7_s24_10$interval <- max(data_7_s24_10$terminal_year) - min(data_7_s24_10$terminal_year)
data_7_s24_11$interval <- max(data_7_s24_11$terminal_year) - min(data_7_s24_11$terminal_year)
data_7_s24_12$interval <- max(data_7_s24_12$terminal_year) - min(data_7_s24_12$terminal_year)
data_7_s24_13$interval <- max(data_7_s24_13$terminal_year) - min(data_7_s24_13$terminal_year)
data_7_s24_14$interval <- max(data_7_s24_14$terminal_year) - min(data_7_s24_14$terminal_year)
data_7_s24_15$interval <- max(data_7_s24_15$terminal_year) - min(data_7_s24_15$terminal_year)
data_7_s24_16$interval <- max(data_7_s24_16$terminal_year) - min(data_7_s24_16$terminal_year)
data_7_s24_17$interval <- max(data_7_s24_17$terminal_year) - min(data_7_s24_17$terminal_year)
data_7_s24_18$interval <- max(data_7_s24_18$terminal_year) - min(data_7_s24_18$terminal_year)
data_7_s24_19$interval <- max(data_7_s24_19$terminal_year) - min(data_7_s24_19$terminal_year)
data_7_s24_20$interval <- max(data_7_s24_20$terminal_year) - min(data_7_s24_20$terminal_year)
data_7_s24_21$interval <- max(data_7_s24_21$terminal_year) - min(data_7_s24_21$terminal_year)

data_7_s24 <- rbind(data_7_s24_1, data_7_s24_2, data_7_s24_3, data_7_s24_4, data_7_s24_5, data_7_s24_6, data_7_s24_7, data_7_s24_8, data_7_s24_9, data_7_s24_10,
                    data_7_s24_11, data_7_s24_12, data_7_s24_13, data_7_s24_14, data_7_s24_15, data_7_s24_16, data_7_s24_17, data_7_s24_18, data_7_s24_19,
                    data_7_s24_20, data_7_s24_21)

# assess 25
data_7_s25 <- data_7_assess[data_7_assess$stockid==ids[25],]

data_7_s25_1 <- data_7_s25[data_7_s25$terminal_year == 2011 | data_7_s25$terminal_year == 2012,]
data_7_s25_1$ty_order <- 1
data_7_s25_1$if_diff_5yrs <- "N"

data_7_s25_2 <- data_7_s25[data_7_s25$terminal_year == 2011 | data_7_s25$terminal_year == 2013,]
data_7_s25_2$ty_order <- 2
data_7_s25_2$if_diff_5yrs <- "N"

data_7_s25_3 <- data_7_s25[data_7_s25$terminal_year == 2011 | data_7_s25$terminal_year == 2014,]
data_7_s25_3$ty_order <- 3
data_7_s25_3$if_diff_5yrs <- "N"

data_7_s25_4 <- data_7_s25[data_7_s25$terminal_year == 2011 | data_7_s25$terminal_year == 2015,]
data_7_s25_4$ty_order <- 4
data_7_s25_4$if_diff_5yrs <- "N"

data_7_s25_5 <- data_7_s25[data_7_s25$terminal_year == 2011 | data_7_s25$terminal_year == 2016,]
data_7_s25_5$ty_order <- 5
data_7_s25_5$if_diff_5yrs <- "N"

data_7_s25_6 <- data_7_s25[data_7_s25$terminal_year == 2011 | data_7_s25$terminal_year == 2017,]
data_7_s25_6$ty_order <- 6
data_7_s25_6$if_diff_5yrs <- "Y"

data_7_s25_7 <- data_7_s25[data_7_s25$terminal_year == 2012 | data_7_s25$terminal_year == 2013,]
data_7_s25_7$ty_order <- 7
data_7_s25_7$if_diff_5yrs <- "N"
data_7_s25_7$ty <- 2012
data_7_s25_7$ty_minus1 <- data_7_s25_7$ty-1
data_7_s25_7$key_ty <- paste(data_7_s25_7$assessid, data_7_s25_7$ty, sep="_")
data_7_s25_7$key_ty_minus1 <- paste(data_7_s25_7$assessid, data_7_s25_7$ty_minus1, sep="_")
data_7_s25_7[,c(46:47,50:61,64:69)] <- NA

data_7_s25_8 <- data_7_s25[data_7_s25$terminal_year == 2012 | data_7_s25$terminal_year == 2014,]
data_7_s25_8$ty_order <- 8
data_7_s25_8$if_diff_5yrs <- "N"
data_7_s25_8$ty <- 2012
data_7_s25_8$ty_minus1 <- data_7_s25_8$ty-1
data_7_s25_8$key_ty <- paste(data_7_s25_8$assessid, data_7_s25_8$ty, sep="_")
data_7_s25_8$key_ty_minus1 <- paste(data_7_s25_8$assessid, data_7_s25_8$ty_minus1, sep="_")
data_7_s25_8[,c(46:47,50:61,64:69)] <- NA

data_7_s25_9 <- data_7_s25[data_7_s25$terminal_year == 2012 | data_7_s25$terminal_year == 2015,]
data_7_s25_9$ty_order <- 9
data_7_s25_9$if_diff_5yrs <- "N"
data_7_s25_9$ty <- 2012
data_7_s25_9$ty_minus1 <- data_7_s25_9$ty-1
data_7_s25_9$key_ty <- paste(data_7_s25_9$assessid, data_7_s25_9$ty, sep="_")
data_7_s25_9$key_ty_minus1 <- paste(data_7_s25_9$assessid, data_7_s25_9$ty_minus1, sep="_")
data_7_s25_9[,c(46:47,50:61,64:69)] <- NA

data_7_s25_10 <- data_7_s25[data_7_s25$terminal_year == 2012 | data_7_s25$terminal_year == 2016,]
data_7_s25_10$ty_order <- 10
data_7_s25_10$if_diff_5yrs <- "N"
data_7_s25_10$ty <- 2012
data_7_s25_10$ty_minus1 <- data_7_s25_10$ty-1
data_7_s25_10$key_ty <- paste(data_7_s25_10$assessid, data_7_s25_10$ty, sep="_")
data_7_s25_10$key_ty_minus1 <- paste(data_7_s25_10$assessid, data_7_s25_10$ty_minus1, sep="_")
data_7_s25_10[,c(46:47,50:61,64:69)] <- NA

data_7_s25_11 <- data_7_s25[data_7_s25$terminal_year == 2012 | data_7_s25$terminal_year == 2017,]
data_7_s25_11$ty_order <- 11
data_7_s25_11$if_diff_5yrs <- "N"
data_7_s25_11$ty <- 2012
data_7_s25_11$ty_minus1 <- data_7_s25_11$ty-1
data_7_s25_11$key_ty <- paste(data_7_s25_11$assessid, data_7_s25_11$ty, sep="_")
data_7_s25_11$key_ty_minus1 <- paste(data_7_s25_11$assessid, data_7_s25_11$ty_minus1, sep="_")
data_7_s25_11[,c(46:47,50:61,64:69)] <- NA

data_7_s25_12 <- data_7_s25[data_7_s25$terminal_year == 2013 | data_7_s25$terminal_year == 2014,]
data_7_s25_12$ty_order <- 12
data_7_s25_12$if_diff_5yrs <- "N"
data_7_s25_12$ty <- 2013
data_7_s25_12$ty_minus1 <- data_7_s25_12$ty-1
data_7_s25_12$key_ty <- paste(data_7_s25_12$assessid, data_7_s25_12$ty, sep="_")
data_7_s25_12$key_ty_minus1 <- paste(data_7_s25_12$assessid, data_7_s25_12$ty_minus1, sep="_")
data_7_s25_12[,c(46:47,50:61,64:69)] <- NA

data_7_s25_13 <- data_7_s25[data_7_s25$terminal_year == 2013 | data_7_s25$terminal_year == 2015,]
data_7_s25_13$ty_order <- 13
data_7_s25_13$if_diff_5yrs <- "N"
data_7_s25_13$ty <- 2013
data_7_s25_13$ty_minus1 <- data_7_s25_13$ty-1
data_7_s25_13$key_ty <- paste(data_7_s25_13$assessid, data_7_s25_13$ty, sep="_")
data_7_s25_13$key_ty_minus1 <- paste(data_7_s25_13$assessid, data_7_s25_13$ty_minus1, sep="_")
data_7_s25_13[,c(46:47,50:61,64:69)] <- NA

data_7_s25_14 <- data_7_s25[data_7_s25$terminal_year == 2013 | data_7_s25$terminal_year == 2016,]
data_7_s25_14$ty_order <- 14
data_7_s25_14$if_diff_5yrs <- "N"
data_7_s25_14$ty <- 2013
data_7_s25_14$ty_minus1 <- data_7_s25_14$ty-1
data_7_s25_14$key_ty <- paste(data_7_s25_14$assessid, data_7_s25_14$ty, sep="_")
data_7_s25_14$key_ty_minus1 <- paste(data_7_s25_14$assessid, data_7_s25_14$ty_minus1, sep="_")
data_7_s25_14[,c(46:47,50:61,64:69)] <- NA

data_7_s25_15 <- data_7_s25[data_7_s25$terminal_year == 2013 | data_7_s25$terminal_year == 2017,]
data_7_s25_15$ty_order <- 15
data_7_s25_15$if_diff_5yrs <- "N"
data_7_s25_15$ty <- 2013
data_7_s25_15$ty_minus1 <- data_7_s25_15$ty-1
data_7_s25_15$key_ty <- paste(data_7_s25_15$assessid, data_7_s25_15$ty, sep="_")
data_7_s25_15$key_ty_minus1 <- paste(data_7_s25_15$assessid, data_7_s25_15$ty_minus1, sep="_")
data_7_s25_15[,c(46:47,50:61,64:69)] <- NA

data_7_s25_16 <- data_7_s25[data_7_s25$terminal_year == 2014 | data_7_s25$terminal_year == 2015,]
data_7_s25_16$ty_order <- 16
data_7_s25_16$if_diff_5yrs <- "N"
data_7_s25_16$ty <- 2014
data_7_s25_16$ty_minus1 <- data_7_s25_16$ty-1
data_7_s25_16$key_ty <- paste(data_7_s25_16$assessid, data_7_s25_16$ty, sep="_")
data_7_s25_16$key_ty_minus1 <- paste(data_7_s25_16$assessid, data_7_s25_16$ty_minus1, sep="_")
data_7_s25_16[,c(46:47,50:61,64:69)] <- NA

data_7_s25_17 <- data_7_s25[data_7_s25$terminal_year == 2014 | data_7_s25$terminal_year == 2016,]
data_7_s25_17$ty_order <- 17
data_7_s25_17$if_diff_5yrs <- "N"
data_7_s25_17$ty <- 2014
data_7_s25_17$ty_minus1 <- data_7_s25_17$ty-1
data_7_s25_17$key_ty <- paste(data_7_s25_17$assessid, data_7_s25_17$ty, sep="_")
data_7_s25_17$key_ty_minus1 <- paste(data_7_s25_17$assessid, data_7_s25_17$ty_minus1, sep="_")
data_7_s25_17[,c(46:47,50:61,64:69)] <- NA

data_7_s25_18 <- data_7_s25[data_7_s25$terminal_year == 2014 | data_7_s25$terminal_year == 2017,]
data_7_s25_18$ty_order <- 18
data_7_s25_18$if_diff_5yrs <- "N"
data_7_s25_18$ty <- 2014
data_7_s25_18$ty_minus1 <- data_7_s25_18$ty-1
data_7_s25_18$key_ty <- paste(data_7_s25_18$assessid, data_7_s25_18$ty, sep="_")
data_7_s25_18$key_ty_minus1 <- paste(data_7_s25_18$assessid, data_7_s25_18$ty_minus1, sep="_")
data_7_s25_18[,c(46:47,50:61,64:69)] <- NA

data_7_s25_19 <- data_7_s25[data_7_s25$terminal_year == 2015 | data_7_s25$terminal_year == 2016,]
data_7_s25_19$ty_order <- 19
data_7_s25_19$if_diff_5yrs <- "N"
data_7_s25_19$ty <- 2015
data_7_s25_19$ty_minus1 <- data_7_s25_19$ty-1
data_7_s25_19$key_ty <- paste(data_7_s25_19$assessid, data_7_s25_19$ty, sep="_")
data_7_s25_19$key_ty_minus1 <- paste(data_7_s25_19$assessid, data_7_s25_19$ty_minus1, sep="_")
data_7_s25_19[,c(46:47,50:61,64:69)] <- NA

data_7_s25_20 <- data_7_s25[data_7_s25$terminal_year == 2015 | data_7_s25$terminal_year == 2017,]
data_7_s25_20$ty_order <- 20
data_7_s25_20$if_diff_5yrs <- "N"
data_7_s25_20$ty <- 2015
data_7_s25_20$ty_minus1 <- data_7_s25_20$ty-1
data_7_s25_20$key_ty <- paste(data_7_s25_20$assessid, data_7_s25_20$ty, sep="_")
data_7_s25_20$key_ty_minus1 <- paste(data_7_s25_20$assessid, data_7_s25_20$ty_minus1, sep="_")
data_7_s25_20[,c(46:47,50:61,64:69)] <- NA

data_7_s25_21 <- data_7_s25[data_7_s25$terminal_year == 2016 | data_7_s25$terminal_year == 2017,]
data_7_s25_21$ty_order <- 21
data_7_s25_21$if_diff_5yrs <- "N"
data_7_s25_21$ty <- 2016
data_7_s25_21$ty_minus1 <- data_7_s25_21$ty-1
data_7_s25_21$key_ty <- paste(data_7_s25_21$assessid, data_7_s25_21$ty, sep="_")
data_7_s25_21$key_ty_minus1 <- paste(data_7_s25_21$assessid, data_7_s25_21$ty_minus1, sep="_")
data_7_s25_21[,c(46:47,50:61,64:69)] <- NA

data_7_s25_1$interval <- max(data_7_s25_1$terminal_year) - min(data_7_s25_1$terminal_year)
data_7_s25_2$interval <- max(data_7_s25_2$terminal_year) - min(data_7_s25_2$terminal_year)
data_7_s25_3$interval <- max(data_7_s25_3$terminal_year) - min(data_7_s25_3$terminal_year)
data_7_s25_4$interval <- max(data_7_s25_4$terminal_year) - min(data_7_s25_4$terminal_year)
data_7_s25_5$interval <- max(data_7_s25_5$terminal_year) - min(data_7_s25_5$terminal_year)
data_7_s25_6$interval <- max(data_7_s25_6$terminal_year) - min(data_7_s25_6$terminal_year)
data_7_s25_7$interval <- max(data_7_s25_7$terminal_year) - min(data_7_s25_7$terminal_year)
data_7_s25_8$interval <- max(data_7_s25_8$terminal_year) - min(data_7_s25_8$terminal_year)
data_7_s25_9$interval <- max(data_7_s25_9$terminal_year) - min(data_7_s25_9$terminal_year)
data_7_s25_10$interval <- max(data_7_s25_10$terminal_year) - min(data_7_s25_10$terminal_year)
data_7_s25_11$interval <- max(data_7_s25_11$terminal_year) - min(data_7_s25_11$terminal_year)
data_7_s25_12$interval <- max(data_7_s25_12$terminal_year) - min(data_7_s25_12$terminal_year)
data_7_s25_13$interval <- max(data_7_s25_13$terminal_year) - min(data_7_s25_13$terminal_year)
data_7_s25_14$interval <- max(data_7_s25_14$terminal_year) - min(data_7_s25_14$terminal_year)
data_7_s25_15$interval <- max(data_7_s25_15$terminal_year) - min(data_7_s25_15$terminal_year)
data_7_s25_16$interval <- max(data_7_s25_16$terminal_year) - min(data_7_s25_16$terminal_year)
data_7_s25_17$interval <- max(data_7_s25_17$terminal_year) - min(data_7_s25_17$terminal_year)
data_7_s25_18$interval <- max(data_7_s25_18$terminal_year) - min(data_7_s25_18$terminal_year)
data_7_s25_19$interval <- max(data_7_s25_19$terminal_year) - min(data_7_s25_19$terminal_year)
data_7_s25_20$interval <- max(data_7_s25_20$terminal_year) - min(data_7_s25_20$terminal_year)
data_7_s25_21$interval <- max(data_7_s25_21$terminal_year) - min(data_7_s25_21$terminal_year)

data_7_s25 <- rbind(data_7_s25_1, data_7_s25_2, data_7_s25_3, data_7_s25_4, data_7_s25_5, data_7_s25_6, data_7_s25_7, data_7_s25_8, data_7_s25_9, data_7_s25_10,
                    data_7_s25_11, data_7_s25_12, data_7_s25_13, data_7_s25_14, data_7_s25_15, data_7_s25_16, data_7_s25_17, data_7_s25_18, data_7_s25_19,
                    data_7_s25_20, data_7_s25_21)

# assess 26
data_7_s26 <- data_7_assess[data_7_assess$stockid==ids[26],]

data_7_s26_1 <- data_7_s26[data_7_s26$terminal_year == 2011 | data_7_s26$terminal_year == 2012,]
data_7_s26_1$ty_order <- 1
data_7_s26_1$if_diff_5yrs <- "N"

data_7_s26_2 <- data_7_s26[data_7_s26$terminal_year == 2011 | data_7_s26$terminal_year == 2013,]
data_7_s26_2$ty_order <- 2
data_7_s26_2$if_diff_5yrs <- "N"

data_7_s26_3 <- data_7_s26[data_7_s26$terminal_year == 2011 | data_7_s26$terminal_year == 2014,]
data_7_s26_3$ty_order <- 3
data_7_s26_3$if_diff_5yrs <- "N"

data_7_s26_4 <- data_7_s26[data_7_s26$terminal_year == 2011 | data_7_s26$terminal_year == 2015,]
data_7_s26_4$ty_order <- 4
data_7_s26_4$if_diff_5yrs <- "N"

data_7_s26_5 <- data_7_s26[data_7_s26$terminal_year == 2011 | data_7_s26$terminal_year == 2016,]
data_7_s26_5$ty_order <- 5
data_7_s26_5$if_diff_5yrs <- "N"

data_7_s26_6 <- data_7_s26[data_7_s26$terminal_year == 2011 | data_7_s26$terminal_year == 2017,]
data_7_s26_6$ty_order <- 6
data_7_s26_6$if_diff_5yrs <- "Y"

data_7_s26_7 <- data_7_s26[data_7_s26$terminal_year == 2012 | data_7_s26$terminal_year == 2013,]
data_7_s26_7$ty_order <- 7
data_7_s26_7$if_diff_5yrs <- "N"
data_7_s26_7$ty <- 2012
data_7_s26_7$ty_minus1 <- data_7_s26_7$ty-1
data_7_s26_7$key_ty <- paste(data_7_s26_7$assessid, data_7_s26_7$ty, sep="_")
data_7_s26_7$key_ty_minus1 <- paste(data_7_s26_7$assessid, data_7_s26_7$ty_minus1, sep="_")
data_7_s26_7[,c(46:47,50:61,64:69)] <- NA

data_7_s26_8 <- data_7_s26[data_7_s26$terminal_year == 2012 | data_7_s26$terminal_year == 2014,]
data_7_s26_8$ty_order <- 8
data_7_s26_8$if_diff_5yrs <- "N"
data_7_s26_8$ty <- 2012
data_7_s26_8$ty_minus1 <- data_7_s26_8$ty-1
data_7_s26_8$key_ty <- paste(data_7_s26_8$assessid, data_7_s26_8$ty, sep="_")
data_7_s26_8$key_ty_minus1 <- paste(data_7_s26_8$assessid, data_7_s26_8$ty_minus1, sep="_")
data_7_s26_8[,c(46:47,50:61,64:69)] <- NA

data_7_s26_9 <- data_7_s26[data_7_s26$terminal_year == 2012 | data_7_s26$terminal_year == 2015,]
data_7_s26_9$ty_order <- 9
data_7_s26_9$if_diff_5yrs <- "N"
data_7_s26_9$ty <- 2012
data_7_s26_9$ty_minus1 <- data_7_s26_9$ty-1
data_7_s26_9$key_ty <- paste(data_7_s26_9$assessid, data_7_s26_9$ty, sep="_")
data_7_s26_9$key_ty_minus1 <- paste(data_7_s26_9$assessid, data_7_s26_9$ty_minus1, sep="_")
data_7_s26_9[,c(46:47,50:61,64:69)] <- NA

data_7_s26_10 <- data_7_s26[data_7_s26$terminal_year == 2012 | data_7_s26$terminal_year == 2016,]
data_7_s26_10$ty_order <- 10
data_7_s26_10$if_diff_5yrs <- "N"
data_7_s26_10$ty <- 2012
data_7_s26_10$ty_minus1 <- data_7_s26_10$ty-1
data_7_s26_10$key_ty <- paste(data_7_s26_10$assessid, data_7_s26_10$ty, sep="_")
data_7_s26_10$key_ty_minus1 <- paste(data_7_s26_10$assessid, data_7_s26_10$ty_minus1, sep="_")
data_7_s26_10[,c(46:47,50:61,64:69)] <- NA

data_7_s26_11 <- data_7_s26[data_7_s26$terminal_year == 2012 | data_7_s26$terminal_year == 2017,]
data_7_s26_11$ty_order <- 11
data_7_s26_11$if_diff_5yrs <- "N"
data_7_s26_11$ty <- 2012
data_7_s26_11$ty_minus1 <- data_7_s26_11$ty-1
data_7_s26_11$key_ty <- paste(data_7_s26_11$assessid, data_7_s26_11$ty, sep="_")
data_7_s26_11$key_ty_minus1 <- paste(data_7_s26_11$assessid, data_7_s26_11$ty_minus1, sep="_")
data_7_s26_11[,c(46:47,50:61,64:69)] <- NA

data_7_s26_12 <- data_7_s26[data_7_s26$terminal_year == 2013 | data_7_s26$terminal_year == 2014,]
data_7_s26_12$ty_order <- 12
data_7_s26_12$if_diff_5yrs <- "N"
data_7_s26_12$ty <- 2013
data_7_s26_12$ty_minus1 <- data_7_s26_12$ty-1
data_7_s26_12$key_ty <- paste(data_7_s26_12$assessid, data_7_s26_12$ty, sep="_")
data_7_s26_12$key_ty_minus1 <- paste(data_7_s26_12$assessid, data_7_s26_12$ty_minus1, sep="_")
data_7_s26_12[,c(46:47,50:61,64:69)] <- NA

data_7_s26_13 <- data_7_s26[data_7_s26$terminal_year == 2013 | data_7_s26$terminal_year == 2015,]
data_7_s26_13$ty_order <- 13
data_7_s26_13$if_diff_5yrs <- "N"
data_7_s26_13$ty <- 2013
data_7_s26_13$ty_minus1 <- data_7_s26_13$ty-1
data_7_s26_13$key_ty <- paste(data_7_s26_13$assessid, data_7_s26_13$ty, sep="_")
data_7_s26_13$key_ty_minus1 <- paste(data_7_s26_13$assessid, data_7_s26_13$ty_minus1, sep="_")
data_7_s26_13[,c(46:47,50:61,64:69)] <- NA

data_7_s26_14 <- data_7_s26[data_7_s26$terminal_year == 2013 | data_7_s26$terminal_year == 2016,]
data_7_s26_14$ty_order <- 14
data_7_s26_14$if_diff_5yrs <- "N"
data_7_s26_14$ty <- 2013
data_7_s26_14$ty_minus1 <- data_7_s26_14$ty-1
data_7_s26_14$key_ty <- paste(data_7_s26_14$assessid, data_7_s26_14$ty, sep="_")
data_7_s26_14$key_ty_minus1 <- paste(data_7_s26_14$assessid, data_7_s26_14$ty_minus1, sep="_")
data_7_s26_14[,c(46:47,50:61,64:69)] <- NA

data_7_s26_15 <- data_7_s26[data_7_s26$terminal_year == 2013 | data_7_s26$terminal_year == 2017,]
data_7_s26_15$ty_order <- 15
data_7_s26_15$if_diff_5yrs <- "N"
data_7_s26_15$ty <- 2013
data_7_s26_15$ty_minus1 <- data_7_s26_15$ty-1
data_7_s26_15$key_ty <- paste(data_7_s26_15$assessid, data_7_s26_15$ty, sep="_")
data_7_s26_15$key_ty_minus1 <- paste(data_7_s26_15$assessid, data_7_s26_15$ty_minus1, sep="_")
data_7_s26_15[,c(46:47,50:61,64:69)] <- NA

data_7_s26_16 <- data_7_s26[data_7_s26$terminal_year == 2014 | data_7_s26$terminal_year == 2015,]
data_7_s26_16$ty_order <- 16
data_7_s26_16$if_diff_5yrs <- "N"
data_7_s26_16$ty <- 2014
data_7_s26_16$ty_minus1 <- data_7_s26_16$ty-1
data_7_s26_16$key_ty <- paste(data_7_s26_16$assessid, data_7_s26_16$ty, sep="_")
data_7_s26_16$key_ty_minus1 <- paste(data_7_s26_16$assessid, data_7_s26_16$ty_minus1, sep="_")
data_7_s26_16[,c(46:47,50:61,64:69)] <- NA

data_7_s26_17 <- data_7_s26[data_7_s26$terminal_year == 2014 | data_7_s26$terminal_year == 2016,]
data_7_s26_17$ty_order <- 17
data_7_s26_17$if_diff_5yrs <- "N"
data_7_s26_17$ty <- 2014
data_7_s26_17$ty_minus1 <- data_7_s26_17$ty-1
data_7_s26_17$key_ty <- paste(data_7_s26_17$assessid, data_7_s26_17$ty, sep="_")
data_7_s26_17$key_ty_minus1 <- paste(data_7_s26_17$assessid, data_7_s26_17$ty_minus1, sep="_")
data_7_s26_17[,c(46:47,50:61,64:69)] <- NA

data_7_s26_18 <- data_7_s26[data_7_s26$terminal_year == 2014 | data_7_s26$terminal_year == 2017,]
data_7_s26_18$ty_order <- 18
data_7_s26_18$if_diff_5yrs <- "N"
data_7_s26_18$ty <- 2014
data_7_s26_18$ty_minus1 <- data_7_s26_18$ty-1
data_7_s26_18$key_ty <- paste(data_7_s26_18$assessid, data_7_s26_18$ty, sep="_")
data_7_s26_18$key_ty_minus1 <- paste(data_7_s26_18$assessid, data_7_s26_18$ty_minus1, sep="_")
data_7_s26_18[,c(46:47,50:61,64:69)] <- NA

data_7_s26_19 <- data_7_s26[data_7_s26$terminal_year == 2015 | data_7_s26$terminal_year == 2016,]
data_7_s26_19$ty_order <- 19
data_7_s26_19$if_diff_5yrs <- "N"
data_7_s26_19$ty <- 2015
data_7_s26_19$ty_minus1 <- data_7_s26_19$ty-1
data_7_s26_19$key_ty <- paste(data_7_s26_19$assessid, data_7_s26_19$ty, sep="_")
data_7_s26_19$key_ty_minus1 <- paste(data_7_s26_19$assessid, data_7_s26_19$ty_minus1, sep="_")
data_7_s26_19[,c(46:47,50:61,64:69)] <- NA

data_7_s26_20 <- data_7_s26[data_7_s26$terminal_year == 2015 | data_7_s26$terminal_year == 2017,]
data_7_s26_20$ty_order <- 20
data_7_s26_20$if_diff_5yrs <- "N"
data_7_s26_20$ty <- 2015
data_7_s26_20$ty_minus1 <- data_7_s26_20$ty-1
data_7_s26_20$key_ty <- paste(data_7_s26_20$assessid, data_7_s26_20$ty, sep="_")
data_7_s26_20$key_ty_minus1 <- paste(data_7_s26_20$assessid, data_7_s26_20$ty_minus1, sep="_")
data_7_s26_20[,c(46:47,50:61,64:69)] <- NA

data_7_s26_21 <- data_7_s26[data_7_s26$terminal_year == 2016 | data_7_s26$terminal_year == 2017,]
data_7_s26_21$ty_order <- 21
data_7_s26_21$if_diff_5yrs <- "N"
data_7_s26_21$ty <- 2016
data_7_s26_21$ty_minus1 <- data_7_s26_21$ty-1
data_7_s26_21$key_ty <- paste(data_7_s26_21$assessid, data_7_s26_21$ty, sep="_")
data_7_s26_21$key_ty_minus1 <- paste(data_7_s26_21$assessid, data_7_s26_21$ty_minus1, sep="_")
data_7_s26_21[,c(46:47,50:61,64:69)] <- NA

data_7_s26_1$interval <- max(data_7_s26_1$terminal_year) - min(data_7_s26_1$terminal_year)
data_7_s26_2$interval <- max(data_7_s26_2$terminal_year) - min(data_7_s26_2$terminal_year)
data_7_s26_3$interval <- max(data_7_s26_3$terminal_year) - min(data_7_s26_3$terminal_year)
data_7_s26_4$interval <- max(data_7_s26_4$terminal_year) - min(data_7_s26_4$terminal_year)
data_7_s26_5$interval <- max(data_7_s26_5$terminal_year) - min(data_7_s26_5$terminal_year)
data_7_s26_6$interval <- max(data_7_s26_6$terminal_year) - min(data_7_s26_6$terminal_year)
data_7_s26_7$interval <- max(data_7_s26_7$terminal_year) - min(data_7_s26_7$terminal_year)
data_7_s26_8$interval <- max(data_7_s26_8$terminal_year) - min(data_7_s26_8$terminal_year)
data_7_s26_9$interval <- max(data_7_s26_9$terminal_year) - min(data_7_s26_9$terminal_year)
data_7_s26_10$interval <- max(data_7_s26_10$terminal_year) - min(data_7_s26_10$terminal_year)
data_7_s26_11$interval <- max(data_7_s26_11$terminal_year) - min(data_7_s26_11$terminal_year)
data_7_s26_12$interval <- max(data_7_s26_12$terminal_year) - min(data_7_s26_12$terminal_year)
data_7_s26_13$interval <- max(data_7_s26_13$terminal_year) - min(data_7_s26_13$terminal_year)
data_7_s26_14$interval <- max(data_7_s26_14$terminal_year) - min(data_7_s26_14$terminal_year)
data_7_s26_15$interval <- max(data_7_s26_15$terminal_year) - min(data_7_s26_15$terminal_year)
data_7_s26_16$interval <- max(data_7_s26_16$terminal_year) - min(data_7_s26_16$terminal_year)
data_7_s26_17$interval <- max(data_7_s26_17$terminal_year) - min(data_7_s26_17$terminal_year)
data_7_s26_18$interval <- max(data_7_s26_18$terminal_year) - min(data_7_s26_18$terminal_year)
data_7_s26_19$interval <- max(data_7_s26_19$terminal_year) - min(data_7_s26_19$terminal_year)
data_7_s26_20$interval <- max(data_7_s26_20$terminal_year) - min(data_7_s26_20$terminal_year)
data_7_s26_21$interval <- max(data_7_s26_21$terminal_year) - min(data_7_s26_21$terminal_year)

data_7_s26 <- rbind(data_7_s26_1, data_7_s26_2, data_7_s26_3, data_7_s26_4, data_7_s26_5, data_7_s26_6, data_7_s26_7, data_7_s26_8, data_7_s26_9, data_7_s26_10,
                    data_7_s26_11, data_7_s26_12, data_7_s26_13, data_7_s26_14, data_7_s26_15, data_7_s26_16, data_7_s26_17, data_7_s26_18, data_7_s26_19,
                    data_7_s26_20, data_7_s26_21)

# assess 27
data_7_s27 <- data_7_assess[data_7_assess$stockid==ids[27],]

data_7_s27_1 <- data_7_s27[data_7_s27$terminal_year == 2011 | data_7_s27$terminal_year == 2012,]
data_7_s27_1$ty_order <- 1
data_7_s27_1$if_diff_5yrs <- "N"

data_7_s27_2 <- data_7_s27[data_7_s27$terminal_year == 2011 | data_7_s27$terminal_year == 2013,]
data_7_s27_2$ty_order <- 2
data_7_s27_2$if_diff_5yrs <- "N"

data_7_s27_3 <- data_7_s27[data_7_s27$terminal_year == 2011 | data_7_s27$terminal_year == 2014,]
data_7_s27_3$ty_order <- 3
data_7_s27_3$if_diff_5yrs <- "N"

data_7_s27_4 <- data_7_s27[data_7_s27$terminal_year == 2011 | data_7_s27$terminal_year == 2015,]
data_7_s27_4$ty_order <- 4
data_7_s27_4$if_diff_5yrs <- "N"

data_7_s27_5 <- data_7_s27[data_7_s27$terminal_year == 2011 | data_7_s27$terminal_year == 2016,]
data_7_s27_5$ty_order <- 5
data_7_s27_5$if_diff_5yrs <- "N"

data_7_s27_6 <- data_7_s27[data_7_s27$terminal_year == 2011 | data_7_s27$terminal_year == 2017,]
data_7_s27_6$ty_order <- 6
data_7_s27_6$if_diff_5yrs <- "Y"

data_7_s27_7 <- data_7_s27[data_7_s27$terminal_year == 2012 | data_7_s27$terminal_year == 2013,]
data_7_s27_7$ty_order <- 7
data_7_s27_7$if_diff_5yrs <- "N"
data_7_s27_7$ty <- 2012
data_7_s27_7$ty_minus1 <- data_7_s27_7$ty-1
data_7_s27_7$key_ty <- paste(data_7_s27_7$assessid, data_7_s27_7$ty, sep="_")
data_7_s27_7$key_ty_minus1 <- paste(data_7_s27_7$assessid, data_7_s27_7$ty_minus1, sep="_")
data_7_s27_7[,c(46:47,50:61,64:69)] <- NA

data_7_s27_8 <- data_7_s27[data_7_s27$terminal_year == 2012 | data_7_s27$terminal_year == 2014,]
data_7_s27_8$ty_order <- 8
data_7_s27_8$if_diff_5yrs <- "N"
data_7_s27_8$ty <- 2012
data_7_s27_8$ty_minus1 <- data_7_s27_8$ty-1
data_7_s27_8$key_ty <- paste(data_7_s27_8$assessid, data_7_s27_8$ty, sep="_")
data_7_s27_8$key_ty_minus1 <- paste(data_7_s27_8$assessid, data_7_s27_8$ty_minus1, sep="_")
data_7_s27_8[,c(46:47,50:61,64:69)] <- NA

data_7_s27_9 <- data_7_s27[data_7_s27$terminal_year == 2012 | data_7_s27$terminal_year == 2015,]
data_7_s27_9$ty_order <- 9
data_7_s27_9$if_diff_5yrs <- "N"
data_7_s27_9$ty <- 2012
data_7_s27_9$ty_minus1 <- data_7_s27_9$ty-1
data_7_s27_9$key_ty <- paste(data_7_s27_9$assessid, data_7_s27_9$ty, sep="_")
data_7_s27_9$key_ty_minus1 <- paste(data_7_s27_9$assessid, data_7_s27_9$ty_minus1, sep="_")
data_7_s27_9[,c(46:47,50:61,64:69)] <- NA

data_7_s27_10 <- data_7_s27[data_7_s27$terminal_year == 2012 | data_7_s27$terminal_year == 2016,]
data_7_s27_10$ty_order <- 10
data_7_s27_10$if_diff_5yrs <- "N"
data_7_s27_10$ty <- 2012
data_7_s27_10$ty_minus1 <- data_7_s27_10$ty-1
data_7_s27_10$key_ty <- paste(data_7_s27_10$assessid, data_7_s27_10$ty, sep="_")
data_7_s27_10$key_ty_minus1 <- paste(data_7_s27_10$assessid, data_7_s27_10$ty_minus1, sep="_")
data_7_s27_10[,c(46:47,50:61,64:69)] <- NA

data_7_s27_11 <- data_7_s27[data_7_s27$terminal_year == 2012 | data_7_s27$terminal_year == 2017,]
data_7_s27_11$ty_order <- 11
data_7_s27_11$if_diff_5yrs <- "N"
data_7_s27_11$ty <- 2012
data_7_s27_11$ty_minus1 <- data_7_s27_11$ty-1
data_7_s27_11$key_ty <- paste(data_7_s27_11$assessid, data_7_s27_11$ty, sep="_")
data_7_s27_11$key_ty_minus1 <- paste(data_7_s27_11$assessid, data_7_s27_11$ty_minus1, sep="_")
data_7_s27_11[,c(46:47,50:61,64:69)] <- NA

data_7_s27_12 <- data_7_s27[data_7_s27$terminal_year == 2013 | data_7_s27$terminal_year == 2014,]
data_7_s27_12$ty_order <- 12
data_7_s27_12$if_diff_5yrs <- "N"
data_7_s27_12$ty <- 2013
data_7_s27_12$ty_minus1 <- data_7_s27_12$ty-1
data_7_s27_12$key_ty <- paste(data_7_s27_12$assessid, data_7_s27_12$ty, sep="_")
data_7_s27_12$key_ty_minus1 <- paste(data_7_s27_12$assessid, data_7_s27_12$ty_minus1, sep="_")
data_7_s27_12[,c(46:47,50:61,64:69)] <- NA

data_7_s27_13 <- data_7_s27[data_7_s27$terminal_year == 2013 | data_7_s27$terminal_year == 2015,]
data_7_s27_13$ty_order <- 13
data_7_s27_13$if_diff_5yrs <- "N"
data_7_s27_13$ty <- 2013
data_7_s27_13$ty_minus1 <- data_7_s27_13$ty-1
data_7_s27_13$key_ty <- paste(data_7_s27_13$assessid, data_7_s27_13$ty, sep="_")
data_7_s27_13$key_ty_minus1 <- paste(data_7_s27_13$assessid, data_7_s27_13$ty_minus1, sep="_")
data_7_s27_13[,c(46:47,50:61,64:69)] <- NA

data_7_s27_14 <- data_7_s27[data_7_s27$terminal_year == 2013 | data_7_s27$terminal_year == 2016,]
data_7_s27_14$ty_order <- 14
data_7_s27_14$if_diff_5yrs <- "N"
data_7_s27_14$ty <- 2013
data_7_s27_14$ty_minus1 <- data_7_s27_14$ty-1
data_7_s27_14$key_ty <- paste(data_7_s27_14$assessid, data_7_s27_14$ty, sep="_")
data_7_s27_14$key_ty_minus1 <- paste(data_7_s27_14$assessid, data_7_s27_14$ty_minus1, sep="_")
data_7_s27_14[,c(46:47,50:61,64:69)] <- NA

data_7_s27_15 <- data_7_s27[data_7_s27$terminal_year == 2013 | data_7_s27$terminal_year == 2017,]
data_7_s27_15$ty_order <- 15
data_7_s27_15$if_diff_5yrs <- "N"
data_7_s27_15$ty <- 2013
data_7_s27_15$ty_minus1 <- data_7_s27_15$ty-1
data_7_s27_15$key_ty <- paste(data_7_s27_15$assessid, data_7_s27_15$ty, sep="_")
data_7_s27_15$key_ty_minus1 <- paste(data_7_s27_15$assessid, data_7_s27_15$ty_minus1, sep="_")
data_7_s27_15[,c(46:47,50:61,64:69)] <- NA

data_7_s27_16 <- data_7_s27[data_7_s27$terminal_year == 2014 | data_7_s27$terminal_year == 2015,]
data_7_s27_16$ty_order <- 16
data_7_s27_16$if_diff_5yrs <- "N"
data_7_s27_16$ty <- 2014
data_7_s27_16$ty_minus1 <- data_7_s27_16$ty-1
data_7_s27_16$key_ty <- paste(data_7_s27_16$assessid, data_7_s27_16$ty, sep="_")
data_7_s27_16$key_ty_minus1 <- paste(data_7_s27_16$assessid, data_7_s27_16$ty_minus1, sep="_")
data_7_s27_16[,c(46:47,50:61,64:69)] <- NA

data_7_s27_17 <- data_7_s27[data_7_s27$terminal_year == 2014 | data_7_s27$terminal_year == 2016,]
data_7_s27_17$ty_order <- 17
data_7_s27_17$if_diff_5yrs <- "N"
data_7_s27_17$ty <- 2014
data_7_s27_17$ty_minus1 <- data_7_s27_17$ty-1
data_7_s27_17$key_ty <- paste(data_7_s27_17$assessid, data_7_s27_17$ty, sep="_")
data_7_s27_17$key_ty_minus1 <- paste(data_7_s27_17$assessid, data_7_s27_17$ty_minus1, sep="_")
data_7_s27_17[,c(46:47,50:61,64:69)] <- NA

data_7_s27_18 <- data_7_s27[data_7_s27$terminal_year == 2014 | data_7_s27$terminal_year == 2017,]
data_7_s27_18$ty_order <- 18
data_7_s27_18$if_diff_5yrs <- "N"
data_7_s27_18$ty <- 2014
data_7_s27_18$ty_minus1 <- data_7_s27_18$ty-1
data_7_s27_18$key_ty <- paste(data_7_s27_18$assessid, data_7_s27_18$ty, sep="_")
data_7_s27_18$key_ty_minus1 <- paste(data_7_s27_18$assessid, data_7_s27_18$ty_minus1, sep="_")
data_7_s27_18[,c(46:47,50:61,64:69)] <- NA

data_7_s27_19 <- data_7_s27[data_7_s27$terminal_year == 2015 | data_7_s27$terminal_year == 2016,]
data_7_s27_19$ty_order <- 19
data_7_s27_19$if_diff_5yrs <- "N"
data_7_s27_19$ty <- 2015
data_7_s27_19$ty_minus1 <- data_7_s27_19$ty-1
data_7_s27_19$key_ty <- paste(data_7_s27_19$assessid, data_7_s27_19$ty, sep="_")
data_7_s27_19$key_ty_minus1 <- paste(data_7_s27_19$assessid, data_7_s27_19$ty_minus1, sep="_")
data_7_s27_19[,c(46:47,50:61,64:69)] <- NA

data_7_s27_20 <- data_7_s27[data_7_s27$terminal_year == 2015 | data_7_s27$terminal_year == 2017,]
data_7_s27_20$ty_order <- 20
data_7_s27_20$if_diff_5yrs <- "N"
data_7_s27_20$ty <- 2015
data_7_s27_20$ty_minus1 <- data_7_s27_20$ty-1
data_7_s27_20$key_ty <- paste(data_7_s27_20$assessid, data_7_s27_20$ty, sep="_")
data_7_s27_20$key_ty_minus1 <- paste(data_7_s27_20$assessid, data_7_s27_20$ty_minus1, sep="_")
data_7_s27_20[,c(46:47,50:61,64:69)] <- NA

data_7_s27_21 <- data_7_s27[data_7_s27$terminal_year == 2016 | data_7_s27$terminal_year == 2017,]
data_7_s27_21$ty_order <- 21
data_7_s27_21$if_diff_5yrs <- "N"
data_7_s27_21$ty <- 2016
data_7_s27_21$ty_minus1 <- data_7_s27_21$ty-1
data_7_s27_21$key_ty <- paste(data_7_s27_21$assessid, data_7_s27_21$ty, sep="_")
data_7_s27_21$key_ty_minus1 <- paste(data_7_s27_21$assessid, data_7_s27_21$ty_minus1, sep="_")
data_7_s27_21[,c(46:47,50:61,64:69)] <- NA

data_7_s27_1$interval <- max(data_7_s27_1$terminal_year) - min(data_7_s27_1$terminal_year)
data_7_s27_2$interval <- max(data_7_s27_2$terminal_year) - min(data_7_s27_2$terminal_year)
data_7_s27_3$interval <- max(data_7_s27_3$terminal_year) - min(data_7_s27_3$terminal_year)
data_7_s27_4$interval <- max(data_7_s27_4$terminal_year) - min(data_7_s27_4$terminal_year)
data_7_s27_5$interval <- max(data_7_s27_5$terminal_year) - min(data_7_s27_5$terminal_year)
data_7_s27_6$interval <- max(data_7_s27_6$terminal_year) - min(data_7_s27_6$terminal_year)
data_7_s27_7$interval <- max(data_7_s27_7$terminal_year) - min(data_7_s27_7$terminal_year)
data_7_s27_8$interval <- max(data_7_s27_8$terminal_year) - min(data_7_s27_8$terminal_year)
data_7_s27_9$interval <- max(data_7_s27_9$terminal_year) - min(data_7_s27_9$terminal_year)
data_7_s27_10$interval <- max(data_7_s27_10$terminal_year) - min(data_7_s27_10$terminal_year)
data_7_s27_11$interval <- max(data_7_s27_11$terminal_year) - min(data_7_s27_11$terminal_year)
data_7_s27_12$interval <- max(data_7_s27_12$terminal_year) - min(data_7_s27_12$terminal_year)
data_7_s27_13$interval <- max(data_7_s27_13$terminal_year) - min(data_7_s27_13$terminal_year)
data_7_s27_14$interval <- max(data_7_s27_14$terminal_year) - min(data_7_s27_14$terminal_year)
data_7_s27_15$interval <- max(data_7_s27_15$terminal_year) - min(data_7_s27_15$terminal_year)
data_7_s27_16$interval <- max(data_7_s27_16$terminal_year) - min(data_7_s27_16$terminal_year)
data_7_s27_17$interval <- max(data_7_s27_17$terminal_year) - min(data_7_s27_17$terminal_year)
data_7_s27_18$interval <- max(data_7_s27_18$terminal_year) - min(data_7_s27_18$terminal_year)
data_7_s27_19$interval <- max(data_7_s27_19$terminal_year) - min(data_7_s27_19$terminal_year)
data_7_s27_20$interval <- max(data_7_s27_20$terminal_year) - min(data_7_s27_20$terminal_year)
data_7_s27_21$interval <- max(data_7_s27_21$terminal_year) - min(data_7_s27_21$terminal_year)

data_7_s27 <- rbind(data_7_s27_1, data_7_s27_2, data_7_s27_3, data_7_s27_4, data_7_s27_5, data_7_s27_6, data_7_s27_7, data_7_s27_8, data_7_s27_9, data_7_s27_10,
                    data_7_s27_11, data_7_s27_12, data_7_s27_13, data_7_s27_14, data_7_s27_15, data_7_s27_16, data_7_s27_17, data_7_s27_18, data_7_s27_19,
                    data_7_s27_20, data_7_s27_21)

# assess 28
data_7_s28 <- data_7_assess[data_7_assess$stockid==ids[28],]

data_7_s28_1 <- data_7_s28[data_7_s28$terminal_year == 2011 | data_7_s28$terminal_year == 2012,]
data_7_s28_1$ty_order <- 1
data_7_s28_1$if_diff_5yrs <- "N"

data_7_s28_2 <- data_7_s28[data_7_s28$terminal_year == 2011 | data_7_s28$terminal_year == 2013,]
data_7_s28_2$ty_order <- 2
data_7_s28_2$if_diff_5yrs <- "N"

data_7_s28_3 <- data_7_s28[data_7_s28$terminal_year == 2011 | data_7_s28$terminal_year == 2014,]
data_7_s28_3$ty_order <- 3
data_7_s28_3$if_diff_5yrs <- "N"

data_7_s28_4 <- data_7_s28[data_7_s28$terminal_year == 2011 | data_7_s28$terminal_year == 2015,]
data_7_s28_4$ty_order <- 4
data_7_s28_4$if_diff_5yrs <- "N"

data_7_s28_5 <- data_7_s28[data_7_s28$terminal_year == 2011 | data_7_s28$terminal_year == 2016,]
data_7_s28_5$ty_order <- 5
data_7_s28_5$if_diff_5yrs <- "N"

data_7_s28_6 <- data_7_s28[data_7_s28$terminal_year == 2011 | data_7_s28$terminal_year == 2017,]
data_7_s28_6$ty_order <- 6
data_7_s28_6$if_diff_5yrs <- "Y"

data_7_s28_7 <- data_7_s28[data_7_s28$terminal_year == 2012 | data_7_s28$terminal_year == 2013,]
data_7_s28_7$ty_order <- 7
data_7_s28_7$if_diff_5yrs <- "N"
data_7_s28_7$ty <- 2012
data_7_s28_7$ty_minus1 <- data_7_s28_7$ty-1
data_7_s28_7$key_ty <- paste(data_7_s28_7$assessid, data_7_s28_7$ty, sep="_")
data_7_s28_7$key_ty_minus1 <- paste(data_7_s28_7$assessid, data_7_s28_7$ty_minus1, sep="_")
data_7_s28_7[,c(46:47,50:61,64:69)] <- NA

data_7_s28_8 <- data_7_s28[data_7_s28$terminal_year == 2012 | data_7_s28$terminal_year == 2014,]
data_7_s28_8$ty_order <- 8
data_7_s28_8$if_diff_5yrs <- "N"
data_7_s28_8$ty <- 2012
data_7_s28_8$ty_minus1 <- data_7_s28_8$ty-1
data_7_s28_8$key_ty <- paste(data_7_s28_8$assessid, data_7_s28_8$ty, sep="_")
data_7_s28_8$key_ty_minus1 <- paste(data_7_s28_8$assessid, data_7_s28_8$ty_minus1, sep="_")
data_7_s28_8[,c(46:47,50:61,64:69)] <- NA

data_7_s28_9 <- data_7_s28[data_7_s28$terminal_year == 2012 | data_7_s28$terminal_year == 2015,]
data_7_s28_9$ty_order <- 9
data_7_s28_9$if_diff_5yrs <- "N"
data_7_s28_9$ty <- 2012
data_7_s28_9$ty_minus1 <- data_7_s28_9$ty-1
data_7_s28_9$key_ty <- paste(data_7_s28_9$assessid, data_7_s28_9$ty, sep="_")
data_7_s28_9$key_ty_minus1 <- paste(data_7_s28_9$assessid, data_7_s28_9$ty_minus1, sep="_")
data_7_s28_9[,c(46:47,50:61,64:69)] <- NA

data_7_s28_10 <- data_7_s28[data_7_s28$terminal_year == 2012 | data_7_s28$terminal_year == 2016,]
data_7_s28_10$ty_order <- 10
data_7_s28_10$if_diff_5yrs <- "N"
data_7_s28_10$ty <- 2012
data_7_s28_10$ty_minus1 <- data_7_s28_10$ty-1
data_7_s28_10$key_ty <- paste(data_7_s28_10$assessid, data_7_s28_10$ty, sep="_")
data_7_s28_10$key_ty_minus1 <- paste(data_7_s28_10$assessid, data_7_s28_10$ty_minus1, sep="_")
data_7_s28_10[,c(46:47,50:61,64:69)] <- NA

data_7_s28_11 <- data_7_s28[data_7_s28$terminal_year == 2012 | data_7_s28$terminal_year == 2017,]
data_7_s28_11$ty_order <- 11
data_7_s28_11$if_diff_5yrs <- "N"
data_7_s28_11$ty <- 2012
data_7_s28_11$ty_minus1 <- data_7_s28_11$ty-1
data_7_s28_11$key_ty <- paste(data_7_s28_11$assessid, data_7_s28_11$ty, sep="_")
data_7_s28_11$key_ty_minus1 <- paste(data_7_s28_11$assessid, data_7_s28_11$ty_minus1, sep="_")
data_7_s28_11[,c(46:47,50:61,64:69)] <- NA

data_7_s28_12 <- data_7_s28[data_7_s28$terminal_year == 2013 | data_7_s28$terminal_year == 2014,]
data_7_s28_12$ty_order <- 12
data_7_s28_12$if_diff_5yrs <- "N"
data_7_s28_12$ty <- 2013
data_7_s28_12$ty_minus1 <- data_7_s28_12$ty-1
data_7_s28_12$key_ty <- paste(data_7_s28_12$assessid, data_7_s28_12$ty, sep="_")
data_7_s28_12$key_ty_minus1 <- paste(data_7_s28_12$assessid, data_7_s28_12$ty_minus1, sep="_")
data_7_s28_12[,c(46:47,50:61,64:69)] <- NA

data_7_s28_13 <- data_7_s28[data_7_s28$terminal_year == 2013 | data_7_s28$terminal_year == 2015,]
data_7_s28_13$ty_order <- 13
data_7_s28_13$if_diff_5yrs <- "N"
data_7_s28_13$ty <- 2013
data_7_s28_13$ty_minus1 <- data_7_s28_13$ty-1
data_7_s28_13$key_ty <- paste(data_7_s28_13$assessid, data_7_s28_13$ty, sep="_")
data_7_s28_13$key_ty_minus1 <- paste(data_7_s28_13$assessid, data_7_s28_13$ty_minus1, sep="_")
data_7_s28_13[,c(46:47,50:61,64:69)] <- NA

data_7_s28_14 <- data_7_s28[data_7_s28$terminal_year == 2013 | data_7_s28$terminal_year == 2016,]
data_7_s28_14$ty_order <- 14
data_7_s28_14$if_diff_5yrs <- "N"
data_7_s28_14$ty <- 2013
data_7_s28_14$ty_minus1 <- data_7_s28_14$ty-1
data_7_s28_14$key_ty <- paste(data_7_s28_14$assessid, data_7_s28_14$ty, sep="_")
data_7_s28_14$key_ty_minus1 <- paste(data_7_s28_14$assessid, data_7_s28_14$ty_minus1, sep="_")
data_7_s28_14[,c(46:47,50:61,64:69)] <- NA

data_7_s28_15 <- data_7_s28[data_7_s28$terminal_year == 2013 | data_7_s28$terminal_year == 2017,]
data_7_s28_15$ty_order <- 15
data_7_s28_15$if_diff_5yrs <- "N"
data_7_s28_15$ty <- 2013
data_7_s28_15$ty_minus1 <- data_7_s28_15$ty-1
data_7_s28_15$key_ty <- paste(data_7_s28_15$assessid, data_7_s28_15$ty, sep="_")
data_7_s28_15$key_ty_minus1 <- paste(data_7_s28_15$assessid, data_7_s28_15$ty_minus1, sep="_")
data_7_s28_15[,c(46:47,50:61,64:69)] <- NA

data_7_s28_16 <- data_7_s28[data_7_s28$terminal_year == 2014 | data_7_s28$terminal_year == 2015,]
data_7_s28_16$ty_order <- 16
data_7_s28_16$if_diff_5yrs <- "N"
data_7_s28_16$ty <- 2014
data_7_s28_16$ty_minus1 <- data_7_s28_16$ty-1
data_7_s28_16$key_ty <- paste(data_7_s28_16$assessid, data_7_s28_16$ty, sep="_")
data_7_s28_16$key_ty_minus1 <- paste(data_7_s28_16$assessid, data_7_s28_16$ty_minus1, sep="_")
data_7_s28_16[,c(46:47,50:61,64:69)] <- NA

data_7_s28_17 <- data_7_s28[data_7_s28$terminal_year == 2014 | data_7_s28$terminal_year == 2016,]
data_7_s28_17$ty_order <- 17
data_7_s28_17$if_diff_5yrs <- "N"
data_7_s28_17$ty <- 2014
data_7_s28_17$ty_minus1 <- data_7_s28_17$ty-1
data_7_s28_17$key_ty <- paste(data_7_s28_17$assessid, data_7_s28_17$ty, sep="_")
data_7_s28_17$key_ty_minus1 <- paste(data_7_s28_17$assessid, data_7_s28_17$ty_minus1, sep="_")
data_7_s28_17[,c(46:47,50:61,64:69)] <- NA

data_7_s28_18 <- data_7_s28[data_7_s28$terminal_year == 2014 | data_7_s28$terminal_year == 2017,]
data_7_s28_18$ty_order <- 18
data_7_s28_18$if_diff_5yrs <- "N"
data_7_s28_18$ty <- 2014
data_7_s28_18$ty_minus1 <- data_7_s28_18$ty-1
data_7_s28_18$key_ty <- paste(data_7_s28_18$assessid, data_7_s28_18$ty, sep="_")
data_7_s28_18$key_ty_minus1 <- paste(data_7_s28_18$assessid, data_7_s28_18$ty_minus1, sep="_")
data_7_s28_18[,c(46:47,50:61,64:69)] <- NA

data_7_s28_19 <- data_7_s28[data_7_s28$terminal_year == 2015 | data_7_s28$terminal_year == 2016,]
data_7_s28_19$ty_order <- 19
data_7_s28_19$if_diff_5yrs <- "N"
data_7_s28_19$ty <- 2015
data_7_s28_19$ty_minus1 <- data_7_s28_19$ty-1
data_7_s28_19$key_ty <- paste(data_7_s28_19$assessid, data_7_s28_19$ty, sep="_")
data_7_s28_19$key_ty_minus1 <- paste(data_7_s28_19$assessid, data_7_s28_19$ty_minus1, sep="_")
data_7_s28_19[,c(46:47,50:61,64:69)] <- NA

data_7_s28_20 <- data_7_s28[data_7_s28$terminal_year == 2015 | data_7_s28$terminal_year == 2017,]
data_7_s28_20$ty_order <- 20
data_7_s28_20$if_diff_5yrs <- "N"
data_7_s28_20$ty <- 2015
data_7_s28_20$ty_minus1 <- data_7_s28_20$ty-1
data_7_s28_20$key_ty <- paste(data_7_s28_20$assessid, data_7_s28_20$ty, sep="_")
data_7_s28_20$key_ty_minus1 <- paste(data_7_s28_20$assessid, data_7_s28_20$ty_minus1, sep="_")
data_7_s28_20[,c(46:47,50:61,64:69)] <- NA

data_7_s28_21 <- data_7_s28[data_7_s28$terminal_year == 2016 | data_7_s28$terminal_year == 2017,]
data_7_s28_21$ty_order <- 21
data_7_s28_21$if_diff_5yrs <- "N"
data_7_s28_21$ty <- 2016
data_7_s28_21$ty_minus1 <- data_7_s28_21$ty-1
data_7_s28_21$key_ty <- paste(data_7_s28_21$assessid, data_7_s28_21$ty, sep="_")
data_7_s28_21$key_ty_minus1 <- paste(data_7_s28_21$assessid, data_7_s28_21$ty_minus1, sep="_")
data_7_s28_21[,c(46:47,50:61,64:69)] <- NA

data_7_s28_1$interval <- max(data_7_s28_1$terminal_year) - min(data_7_s28_1$terminal_year)
data_7_s28_2$interval <- max(data_7_s28_2$terminal_year) - min(data_7_s28_2$terminal_year)
data_7_s28_3$interval <- max(data_7_s28_3$terminal_year) - min(data_7_s28_3$terminal_year)
data_7_s28_4$interval <- max(data_7_s28_4$terminal_year) - min(data_7_s28_4$terminal_year)
data_7_s28_5$interval <- max(data_7_s28_5$terminal_year) - min(data_7_s28_5$terminal_year)
data_7_s28_6$interval <- max(data_7_s28_6$terminal_year) - min(data_7_s28_6$terminal_year)
data_7_s28_7$interval <- max(data_7_s28_7$terminal_year) - min(data_7_s28_7$terminal_year)
data_7_s28_8$interval <- max(data_7_s28_8$terminal_year) - min(data_7_s28_8$terminal_year)
data_7_s28_9$interval <- max(data_7_s28_9$terminal_year) - min(data_7_s28_9$terminal_year)
data_7_s28_10$interval <- max(data_7_s28_10$terminal_year) - min(data_7_s28_10$terminal_year)
data_7_s28_11$interval <- max(data_7_s28_11$terminal_year) - min(data_7_s28_11$terminal_year)
data_7_s28_12$interval <- max(data_7_s28_12$terminal_year) - min(data_7_s28_12$terminal_year)
data_7_s28_13$interval <- max(data_7_s28_13$terminal_year) - min(data_7_s28_13$terminal_year)
data_7_s28_14$interval <- max(data_7_s28_14$terminal_year) - min(data_7_s28_14$terminal_year)
data_7_s28_15$interval <- max(data_7_s28_15$terminal_year) - min(data_7_s28_15$terminal_year)
data_7_s28_16$interval <- max(data_7_s28_16$terminal_year) - min(data_7_s28_16$terminal_year)
data_7_s28_17$interval <- max(data_7_s28_17$terminal_year) - min(data_7_s28_17$terminal_year)
data_7_s28_18$interval <- max(data_7_s28_18$terminal_year) - min(data_7_s28_18$terminal_year)
data_7_s28_19$interval <- max(data_7_s28_19$terminal_year) - min(data_7_s28_19$terminal_year)
data_7_s28_20$interval <- max(data_7_s28_20$terminal_year) - min(data_7_s28_20$terminal_year)
data_7_s28_21$interval <- max(data_7_s28_21$terminal_year) - min(data_7_s28_21$terminal_year)

data_7_s28 <- rbind(data_7_s28_1, data_7_s28_2, data_7_s28_3, data_7_s28_4, data_7_s28_5, data_7_s28_6, data_7_s28_7, data_7_s28_8, data_7_s28_9, data_7_s28_10,
                    data_7_s28_11, data_7_s28_12, data_7_s28_13, data_7_s28_14, data_7_s28_15, data_7_s28_16, data_7_s28_17, data_7_s28_18, data_7_s28_19,
                    data_7_s28_20, data_7_s28_21)

data_7 <- rbind(data_7_s1, data_7_s2, data_7_s3, data_7_s4, data_7_s5,
                data_7_s6, data_7_s7, data_7_s8, data_7_s9, data_7_s10, 
                data_7_s11, data_7_s12, data_7_s13, data_7_s14, data_7_s15,
                data_7_s16, data_7_s17, data_7_s18, data_7_s19, data_7_s20,
                data_7_s21, data_7_s22, data_7_s23, data_7_s24,data_7_s25,
                data_7_s26,data_7_s27,data_7_s28)

write.csv(data_7, paste(outputdir, "0e_6_data_7_assess_with_records.csv", sep="/"), row.names=F)

########################################################################################################################################

## Stocks with 8 assessments
data_8_assess <- dd_new2[dd_new2$stockid %in% stockid_8assess,]
length(unique(data_8_assess$stockid))  # 4 stocks
length(unique(data_8_assess$assessid))  # 32 assesses

ids <- unique(data_8_assess$stockid)

# assess 1
data_8_s1 <- data_8_assess[data_8_assess$stockid==ids[1],]

data_8_s1_1 <- data_8_s1[data_8_s1$terminal_year == 2010 | data_8_s1$terminal_year == 2011,]
data_8_s1_1$ty_order <- 1
data_8_s1_1$if_diff_5yrs <- "N"

data_8_s1_2 <- data_8_s1[data_8_s1$terminal_year == 2010 | data_8_s1$terminal_year == 2012,]
data_8_s1_2$ty_order <- 2
data_8_s1_2$if_diff_5yrs <- "N"

data_8_s1_3 <- data_8_s1[data_8_s1$terminal_year == 2010 | data_8_s1$terminal_year == 2013,]
data_8_s1_3$ty_order <- 3
data_8_s1_3$if_diff_5yrs <- "N"

data_8_s1_4 <- data_8_s1[data_8_s1$terminal_year == 2010 | data_8_s1$terminal_year == 2014,]
data_8_s1_4$ty_order <- 4
data_8_s1_4$if_diff_5yrs <- "N"

data_8_s1_5 <- data_8_s1[data_8_s1$terminal_year == 2010 | data_8_s1$terminal_year == 2015,]
data_8_s1_5$ty_order <- 5
data_8_s1_5$if_diff_5yrs <- "N"

data_8_s1_6 <- data_8_s1[data_8_s1$terminal_year == 2010 | data_8_s1$terminal_year == 2016,]
data_8_s1_6$ty_order <- 6
data_8_s1_6$if_diff_5yrs <- "Y"

data_8_s1_7 <- data_8_s1[data_8_s1$terminal_year == 2010 | data_8_s1$terminal_year == 2017,]
data_8_s1_7$ty_order <- 7
data_8_s1_7$if_diff_5yrs <- "Y"

data_8_s1_8 <- data_8_s1[data_8_s1$terminal_year == 2011 | data_8_s1$terminal_year == 2012,]
data_8_s1_8$ty_order <- 8
data_8_s1_8$if_diff_5yrs <- "N"
data_8_s1_8$ty <- 2011
data_8_s1_8$ty_minus1 <- data_8_s1_8$ty-1
data_8_s1_8$key_ty <- paste(data_8_s1_8$assessid, data_8_s1_8$ty, sep="_")
data_8_s1_8$key_ty_minus1 <- paste(data_8_s1_8$assessid, data_8_s1_8$ty_minus1, sep="_")
data_8_s1_8[,c(46:47,50:61,64:69)] <- NA

data_8_s1_9 <- data_8_s1[data_8_s1$terminal_year == 2011 | data_8_s1$terminal_year == 2013,]
data_8_s1_9$ty_order <- 9
data_8_s1_9$if_diff_5yrs <- "N"
data_8_s1_9$ty <- 2011
data_8_s1_9$ty_minus1 <- data_8_s1_9$ty-1
data_8_s1_9$key_ty <- paste(data_8_s1_9$assessid, data_8_s1_9$ty, sep="_")
data_8_s1_9$key_ty_minus1 <- paste(data_8_s1_9$assessid, data_8_s1_9$ty_minus1, sep="_")
data_8_s1_9[,c(46:47,50:61,64:69)] <- NA

data_8_s1_10 <- data_8_s1[data_8_s1$terminal_year == 2011 | data_8_s1$terminal_year == 2014,]
data_8_s1_10$ty_order <- 10
data_8_s1_10$if_diff_5yrs <- "N"
data_8_s1_10$ty <- 2011
data_8_s1_10$ty_minus1 <- data_8_s1_10$ty-1
data_8_s1_10$key_ty <- paste(data_8_s1_10$assessid, data_8_s1_10$ty, sep="_")
data_8_s1_10$key_ty_minus1 <- paste(data_8_s1_10$assessid, data_8_s1_10$ty_minus1, sep="_")
data_8_s1_10[,c(46:47,50:61,64:69)] <- NA

data_8_s1_11 <- data_8_s1[data_8_s1$terminal_year == 2011 | data_8_s1$terminal_year == 2015,]
data_8_s1_11$ty_order <- 11
data_8_s1_11$if_diff_5yrs <- "N"
data_8_s1_11$ty <- 2011
data_8_s1_11$ty_minus1 <- data_8_s1_11$ty-1
data_8_s1_11$key_ty <- paste(data_8_s1_11$assessid, data_8_s1_11$ty, sep="_")
data_8_s1_11$key_ty_minus1 <- paste(data_8_s1_11$assessid, data_8_s1_11$ty_minus1, sep="_")
data_8_s1_11[,c(46:47,50:61,64:69)] <- NA

data_8_s1_12 <- data_8_s1[data_8_s1$terminal_year == 2011 | data_8_s1$terminal_year == 2016,]
data_8_s1_12$ty_order <- 12
data_8_s1_12$if_diff_5yrs <- "N"
data_8_s1_12$ty <- 2011
data_8_s1_12$ty_minus1 <- data_8_s1_12$ty-1
data_8_s1_12$key_ty <- paste(data_8_s1_12$assessid, data_8_s1_12$ty, sep="_")
data_8_s1_12$key_ty_minus1 <- paste(data_8_s1_12$assessid, data_8_s1_12$ty_minus1, sep="_")
data_8_s1_12[,c(46:47,50:61,64:69)] <- NA

data_8_s1_13 <- data_8_s1[data_8_s1$terminal_year == 2011 | data_8_s1$terminal_year == 2017,]
data_8_s1_13$ty_order <- 13
data_8_s1_13$if_diff_5yrs <- "Y"
data_8_s1_13$ty <- 2011
data_8_s1_13$ty_minus1 <- data_8_s1_13$ty-1
data_8_s1_13$key_ty <- paste(data_8_s1_13$assessid, data_8_s1_13$ty, sep="_")
data_8_s1_13$key_ty_minus1 <- paste(data_8_s1_13$assessid, data_8_s1_13$ty_minus1, sep="_")
data_8_s1_13[,c(46:47,50:61,64:69)] <- NA

data_8_s1_14 <- data_8_s1[data_8_s1$terminal_year == 2012 | data_8_s1$terminal_year == 2013,]
data_8_s1_14$ty_order <- 14
data_8_s1_14$if_diff_5yrs <- "N"
data_8_s1_14$ty <- 2012
data_8_s1_14$ty_minus1 <- data_8_s1_14$ty-1
data_8_s1_14$key_ty <- paste(data_8_s1_14$assessid, data_8_s1_14$ty, sep="_")
data_8_s1_14$key_ty_minus1 <- paste(data_8_s1_14$assessid, data_8_s1_14$ty_minus1, sep="_")
data_8_s1_14[,c(46:47,50:61,64:69)] <- NA

data_8_s1_15 <- data_8_s1[data_8_s1$terminal_year == 2012 | data_8_s1$terminal_year == 2014,]
data_8_s1_15$ty_order <- 15
data_8_s1_15$if_diff_5yrs <- "N"
data_8_s1_15$ty <- 2012
data_8_s1_15$ty_minus1 <- data_8_s1_15$ty-1
data_8_s1_15$key_ty <- paste(data_8_s1_15$assessid, data_8_s1_15$ty, sep="_")
data_8_s1_15$key_ty_minus1 <- paste(data_8_s1_15$assessid, data_8_s1_15$ty_minus1, sep="_")
data_8_s1_15[,c(46:47,50:61,64:69)] <- NA

data_8_s1_16 <- data_8_s1[data_8_s1$terminal_year == 2012 | data_8_s1$terminal_year == 2015,]
data_8_s1_16$ty_order <- 16
data_8_s1_16$if_diff_5yrs <- "N"
data_8_s1_16$ty <- 2012
data_8_s1_16$ty_minus1 <- data_8_s1_16$ty-1
data_8_s1_16$key_ty <- paste(data_8_s1_16$assessid, data_8_s1_16$ty, sep="_")
data_8_s1_16$key_ty_minus1 <- paste(data_8_s1_16$assessid, data_8_s1_16$ty_minus1, sep="_")
data_8_s1_16[,c(46:47,50:61,64:69)] <- NA

data_8_s1_17 <- data_8_s1[data_8_s1$terminal_year == 2012 | data_8_s1$terminal_year == 2016,]
data_8_s1_17$ty_order <- 17
data_8_s1_17$if_diff_5yrs <- "N"
data_8_s1_17$ty <- 2012
data_8_s1_17$ty_minus1 <- data_8_s1_17$ty-1
data_8_s1_17$key_ty <- paste(data_8_s1_17$assessid, data_8_s1_17$ty, sep="_")
data_8_s1_17$key_ty_minus1 <- paste(data_8_s1_17$assessid, data_8_s1_17$ty_minus1, sep="_")
data_8_s1_17[,c(46:47,50:61,64:69)] <- NA

data_8_s1_18 <- data_8_s1[data_8_s1$terminal_year == 2012 | data_8_s1$terminal_year == 2017,]
data_8_s1_18$ty_order <- 18
data_8_s1_18$if_diff_5yrs <- "N"
data_8_s1_18$ty <- 2012
data_8_s1_18$ty_minus1 <- data_8_s1_18$ty-1
data_8_s1_18$key_ty <- paste(data_8_s1_18$assessid, data_8_s1_18$ty, sep="_")
data_8_s1_18$key_ty_minus1 <- paste(data_8_s1_18$assessid, data_8_s1_18$ty_minus1, sep="_")
data_8_s1_18[,c(46:47,50:61,64:69)] <- NA

data_8_s1_19 <- data_8_s1[data_8_s1$terminal_year == 2013 | data_8_s1$terminal_year == 2014,]
data_8_s1_19$ty_order <- 19
data_8_s1_19$if_diff_5yrs <- "N"
data_8_s1_19$ty <- 2013
data_8_s1_19$ty_minus1 <- data_8_s1_19$ty-1
data_8_s1_19$key_ty <- paste(data_8_s1_19$assessid, data_8_s1_19$ty, sep="_")
data_8_s1_19$key_ty_minus1 <- paste(data_8_s1_19$assessid, data_8_s1_19$ty_minus1, sep="_")
data_8_s1_19[,c(46:47,50:61,64:69)] <- NA

data_8_s1_20 <- data_8_s1[data_8_s1$terminal_year == 2013 | data_8_s1$terminal_year == 2015,]
data_8_s1_20$ty_order <- 20
data_8_s1_20$if_diff_5yrs <- "N"
data_8_s1_20$ty <- 2013
data_8_s1_20$ty_minus1 <- data_8_s1_20$ty-1
data_8_s1_20$key_ty <- paste(data_8_s1_20$assessid, data_8_s1_20$ty, sep="_")
data_8_s1_20$key_ty_minus1 <- paste(data_8_s1_20$assessid, data_8_s1_20$ty_minus1, sep="_")
data_8_s1_20[,c(46:47,50:61,64:69)] <- NA

data_8_s1_21 <- data_8_s1[data_8_s1$terminal_year == 2013 | data_8_s1$terminal_year == 2016,]
data_8_s1_21$ty_order <- 21
data_8_s1_21$if_diff_5yrs <- "N"
data_8_s1_21$ty <- 2013
data_8_s1_21$ty_minus1 <- data_8_s1_21$ty-1
data_8_s1_21$key_ty <- paste(data_8_s1_21$assessid, data_8_s1_21$ty, sep="_")
data_8_s1_21$key_ty_minus1 <- paste(data_8_s1_21$assessid, data_8_s1_21$ty_minus1, sep="_")
data_8_s1_21[,c(46:47,50:61,64:69)] <- NA

data_8_s1_22 <- data_8_s1[data_8_s1$terminal_year == 2013 | data_8_s1$terminal_year == 2017,]
data_8_s1_22$ty_order <- 22
data_8_s1_22$if_diff_5yrs <- "N"
data_8_s1_22$ty <- 2013
data_8_s1_22$ty_minus1 <- data_8_s1_22$ty-1
data_8_s1_22$key_ty <- paste(data_8_s1_22$assessid, data_8_s1_22$ty, sep="_")
data_8_s1_22$key_ty_minus1 <- paste(data_8_s1_22$assessid, data_8_s1_22$ty_minus1, sep="_")
data_8_s1_22[,c(46:47,50:61,64:69)] <- NA

data_8_s1_23 <- data_8_s1[data_8_s1$terminal_year == 2014 | data_8_s1$terminal_year == 2015,]
data_8_s1_23$ty_order <- 23
data_8_s1_23$if_diff_5yrs <- "N"
data_8_s1_23$ty <- 2014
data_8_s1_23$ty_minus1 <- data_8_s1_23$ty-1
data_8_s1_23$key_ty <- paste(data_8_s1_23$assessid, data_8_s1_23$ty, sep="_")
data_8_s1_23$key_ty_minus1 <- paste(data_8_s1_23$assessid, data_8_s1_23$ty_minus1, sep="_")
data_8_s1_23[,c(46:47,50:61,64:69)] <- NA

data_8_s1_24 <- data_8_s1[data_8_s1$terminal_year == 2014 |  data_8_s1$terminal_year == 2016,]
data_8_s1_24$ty_order <- 24
data_8_s1_24$if_diff_5yrs <- "N"
data_8_s1_24$ty <- 2014
data_8_s1_24$ty_minus1 <- data_8_s1_24$ty-1
data_8_s1_24$key_ty <- paste(data_8_s1_24$assessid, data_8_s1_24$ty, sep="_")
data_8_s1_24$key_ty_minus1 <- paste(data_8_s1_24$assessid, data_8_s1_24$ty_minus1, sep="_")
data_8_s1_24[,c(46:47,50:61,64:69)] <- NA

data_8_s1_25 <- data_8_s1[data_8_s1$terminal_year == 2014 |  data_8_s1$terminal_year == 2017,]
data_8_s1_25$ty_order <- 25
data_8_s1_25$if_diff_5yrs <- "N"
data_8_s1_25$ty <- 2014
data_8_s1_25$ty_minus1 <- data_8_s1_25$ty-1
data_8_s1_25$key_ty <- paste(data_8_s1_25$assessid, data_8_s1_25$ty, sep="_")
data_8_s1_25$key_ty_minus1 <- paste(data_8_s1_25$assessid, data_8_s1_25$ty_minus1, sep="_")
data_8_s1_25[,c(46:47,50:61,64:69)] <- NA

data_8_s1_26 <- data_8_s1[data_8_s1$terminal_year == 2015 |  data_8_s1$terminal_year == 2016,]
data_8_s1_26$ty_order <- 26
data_8_s1_26$if_diff_5yrs <- "N"
data_8_s1_26$ty <- 2015
data_8_s1_26$ty_minus1 <- data_8_s1_26$ty-1
data_8_s1_26$key_ty <- paste(data_8_s1_26$assessid, data_8_s1_26$ty, sep="_")
data_8_s1_26$key_ty_minus1 <- paste(data_8_s1_26$assessid, data_8_s1_26$ty_minus1, sep="_")
data_8_s1_26[,c(46:47,50:61,64:69)] <- NA

data_8_s1_27 <- data_8_s1[data_8_s1$terminal_year == 2015 |  data_8_s1$terminal_year == 2017,]
data_8_s1_27$ty_order <- 27
data_8_s1_27$if_diff_5yrs <- "N"
data_8_s1_27$ty <- 2015
data_8_s1_27$ty_minus1 <- data_8_s1_27$ty-1
data_8_s1_27$key_ty <- paste(data_8_s1_27$assessid, data_8_s1_27$ty, sep="_")
data_8_s1_27$key_ty_minus1 <- paste(data_8_s1_27$assessid, data_8_s1_27$ty_minus1, sep="_")
data_8_s1_27[,c(46:47,50:61,64:69)] <- NA

data_8_s1_28 <- data_8_s1[data_8_s1$terminal_year == 2016 |  data_8_s1$terminal_year == 2017,]
data_8_s1_28$ty_order <- 28
data_8_s1_28$if_diff_5yrs <- "N"
data_8_s1_28$ty <- 2016
data_8_s1_28$ty_minus1 <- data_8_s1_28$ty-1
data_8_s1_28$key_ty <- paste(data_8_s1_28$assessid, data_8_s1_28$ty, sep="_")
data_8_s1_28$key_ty_minus1 <- paste(data_8_s1_28$assessid, data_8_s1_28$ty_minus1, sep="_")
data_8_s1_28[,c(46:47,50:61,64:69)] <- NA

data_8_s1_1$interval <- max(data_8_s1_1$terminal_year) - min(data_8_s1_1$terminal_year)
data_8_s1_2$interval <- max(data_8_s1_2$terminal_year) - min(data_8_s1_2$terminal_year)
data_8_s1_3$interval <- max(data_8_s1_3$terminal_year) - min(data_8_s1_3$terminal_year)
data_8_s1_4$interval <- max(data_8_s1_4$terminal_year) - min(data_8_s1_4$terminal_year)
data_8_s1_5$interval <- max(data_8_s1_5$terminal_year) - min(data_8_s1_5$terminal_year)
data_8_s1_6$interval <- max(data_8_s1_6$terminal_year) - min(data_8_s1_6$terminal_year)
data_8_s1_7$interval <- max(data_8_s1_7$terminal_year) - min(data_8_s1_7$terminal_year)
data_8_s1_8$interval <- max(data_8_s1_8$terminal_year) - min(data_8_s1_8$terminal_year)
data_8_s1_9$interval <- max(data_8_s1_9$terminal_year) - min(data_8_s1_9$terminal_year)
data_8_s1_10$interval <- max(data_8_s1_10$terminal_year) - min(data_8_s1_10$terminal_year)
data_8_s1_11$interval <- max(data_8_s1_11$terminal_year) - min(data_8_s1_11$terminal_year)
data_8_s1_12$interval <- max(data_8_s1_12$terminal_year) - min(data_8_s1_12$terminal_year)
data_8_s1_13$interval <- max(data_8_s1_13$terminal_year) - min(data_8_s1_13$terminal_year)
data_8_s1_14$interval <- max(data_8_s1_14$terminal_year) - min(data_8_s1_14$terminal_year)
data_8_s1_15$interval <- max(data_8_s1_15$terminal_year) - min(data_8_s1_15$terminal_year)
data_8_s1_16$interval <- max(data_8_s1_16$terminal_year) - min(data_8_s1_16$terminal_year)
data_8_s1_17$interval <- max(data_8_s1_17$terminal_year) - min(data_8_s1_17$terminal_year)
data_8_s1_18$interval <- max(data_8_s1_18$terminal_year) - min(data_8_s1_18$terminal_year)
data_8_s1_19$interval <- max(data_8_s1_19$terminal_year) - min(data_8_s1_19$terminal_year)
data_8_s1_20$interval <- max(data_8_s1_20$terminal_year) - min(data_8_s1_20$terminal_year)
data_8_s1_21$interval <- max(data_8_s1_21$terminal_year) - min(data_8_s1_21$terminal_year)
data_8_s1_22$interval <- max(data_8_s1_22$terminal_year) - min(data_8_s1_22$terminal_year)
data_8_s1_23$interval <- max(data_8_s1_23$terminal_year) - min(data_8_s1_23$terminal_year)
data_8_s1_24$interval <- max(data_8_s1_24$terminal_year) - min(data_8_s1_24$terminal_year)
data_8_s1_25$interval <- max(data_8_s1_25$terminal_year) - min(data_8_s1_25$terminal_year)
data_8_s1_26$interval <- max(data_8_s1_26$terminal_year) - min(data_8_s1_26$terminal_year)
data_8_s1_27$interval <- max(data_8_s1_27$terminal_year) - min(data_8_s1_27$terminal_year)
data_8_s1_28$interval <- max(data_8_s1_28$terminal_year) - min(data_8_s1_28$terminal_year)

data_8_s1 <- rbind(data_8_s1_1, data_8_s1_2, data_8_s1_3, data_8_s1_4, data_8_s1_5, data_8_s1_6, data_8_s1_7, data_8_s1_8, data_8_s1_9, data_8_s1_10,
                   data_8_s1_11, data_8_s1_12, data_8_s1_13, data_8_s1_14, data_8_s1_15, data_8_s1_16, data_8_s1_17, data_8_s1_18, data_8_s1_19,
                   data_8_s1_20, data_8_s1_21, data_8_s1_22, data_8_s1_23, data_8_s1_24, data_8_s1_25, data_8_s1_26, data_8_s1_27, data_8_s1_28)

# assess 2
data_8_s2 <- data_8_assess[data_8_assess$stockid==ids[2],]

data_8_s2_1 <- data_8_s2[data_8_s2$terminal_year == 2011 | data_8_s2$terminal_year == 2012,]
data_8_s2_1$ty_order <- 1
data_8_s2_1$if_diff_5yrs <- "N"

data_8_s2_2 <- data_8_s2[data_8_s2$terminal_year == 2011 | data_8_s2$terminal_year == 2013,]
data_8_s2_2$ty_order <- 2
data_8_s2_2$if_diff_5yrs <- "N"

data_8_s2_3 <- data_8_s2[data_8_s2$terminal_year == 2011 | data_8_s2$terminal_year == 2014,]
data_8_s2_3$ty_order <- 3
data_8_s2_3$if_diff_5yrs <- "N"

data_8_s2_4 <- data_8_s2[data_8_s2$terminal_year == 2011 | data_8_s2$assessid == "WGBFAS-CODBA2224-1993-2016-ICESIMP2016",]
data_8_s2_4$ty_order <- 4
data_8_s2_4$if_diff_5yrs <- "N"

data_8_s2_5 <- data_8_s2[data_8_s2$terminal_year == 2011 | data_8_s2$assessid == "WGBFAS-CODBA2224-1993-2016-ICESIMP2018",]
data_8_s2_5$ty_order <- 5
data_8_s2_5$if_diff_5yrs <- "N"

data_8_s2_6 <- data_8_s2[data_8_s2$terminal_year == 2011 | data_8_s2$terminal_year == 2016,]
data_8_s2_6$ty_order <- 6
data_8_s2_6$if_diff_5yrs <- "N"

data_8_s2_7 <- data_8_s2[data_8_s2$terminal_year == 2011 | data_8_s2$terminal_year == 2017,]
data_8_s2_7$ty_order <- 7
data_8_s2_7$if_diff_5yrs <- "Y"

data_8_s2_8 <- data_8_s2[data_8_s2$terminal_year == 2012 | data_8_s2$terminal_year == 2013,]
data_8_s2_8$ty_order <- 8
data_8_s2_8$if_diff_5yrs <- "N"
data_8_s2_8$ty <- 2012
data_8_s2_8$ty_minus1 <- data_8_s2_8$ty-1
data_8_s2_8$key_ty <- paste(data_8_s2_8$assessid, data_8_s2_8$ty, sep="_")
data_8_s2_8$key_ty_minus1 <- paste(data_8_s2_8$assessid, data_8_s2_8$ty_minus1, sep="_")
data_8_s2_8[,c(46:47,50:61,64:69)] <- NA

data_8_s2_9 <- data_8_s2[data_8_s2$terminal_year == 2012 | data_8_s2$terminal_year == 2014,]
data_8_s2_9$ty_order <- 9
data_8_s2_9$if_diff_5yrs <- "N"
data_8_s2_9$ty <- 2012
data_8_s2_9$ty_minus1 <- data_8_s2_9$ty-1
data_8_s2_9$key_ty <- paste(data_8_s2_9$assessid, data_8_s2_9$ty, sep="_")
data_8_s2_9$key_ty_minus1 <- paste(data_8_s2_9$assessid, data_8_s2_9$ty_minus1, sep="_")
data_8_s2_9[,c(46:47,50:61,64:69)] <- NA

data_8_s2_10 <- data_8_s2[data_8_s2$terminal_year == 2012 | data_8_s2$assessid == "WGBFAS-CODBA2224-1993-2016-ICESIMP2016",]
data_8_s2_10$ty_order <- 10
data_8_s2_10$if_diff_5yrs <- "N"
data_8_s2_10$ty <- 2012
data_8_s2_10$ty_minus1 <- data_8_s2_10$ty-1
data_8_s2_10$key_ty <- paste(data_8_s2_10$assessid, data_8_s2_10$ty, sep="_")
data_8_s2_10$key_ty_minus1 <- paste(data_8_s2_10$assessid, data_8_s2_10$ty_minus1, sep="_")
data_8_s2_10[,c(46:47,50:61,64:69)] <- NA

data_8_s2_11 <- data_8_s2[data_8_s2$terminal_year == 2012 | data_8_s2$assessid == "WGBFAS-CODBA2224-1993-2016-ICESIMP2018",]
data_8_s2_11$ty_order <- 11
data_8_s2_11$if_diff_5yrs <- "N"
data_8_s2_11$ty <- 2012
data_8_s2_11$ty_minus1 <- data_8_s2_11$ty-1
data_8_s2_11$key_ty <- paste(data_8_s2_11$assessid, data_8_s2_11$ty, sep="_")
data_8_s2_11$key_ty_minus1 <- paste(data_8_s2_11$assessid, data_8_s2_11$ty_minus1, sep="_")
data_8_s2_11[,c(46:47,50:61,64:69)] <- NA

data_8_s2_12 <- data_8_s2[data_8_s2$terminal_year == 2012 | data_8_s2$terminal_year == 2016,]
data_8_s2_12$ty_order <- 12
data_8_s2_12$if_diff_5yrs <- "N"
data_8_s2_12$ty <- 2012
data_8_s2_12$ty_minus1 <- data_8_s2_12$ty-1
data_8_s2_12$key_ty <- paste(data_8_s2_12$assessid, data_8_s2_12$ty, sep="_")
data_8_s2_12$key_ty_minus1 <- paste(data_8_s2_12$assessid, data_8_s2_12$ty_minus1, sep="_")
data_8_s2_12[,c(46:47,50:61,64:69)] <- NA

data_8_s2_13 <- data_8_s2[data_8_s2$terminal_year == 2012 | data_8_s2$terminal_year == 2017,]
data_8_s2_13$ty_order <- 13
data_8_s2_13$if_diff_5yrs <- "N"
data_8_s2_13$ty <- 2012
data_8_s2_13$ty_minus1 <- data_8_s2_13$ty-1
data_8_s2_13$key_ty <- paste(data_8_s2_13$assessid, data_8_s2_13$ty, sep="_")
data_8_s2_13$key_ty_minus1 <- paste(data_8_s2_13$assessid, data_8_s2_13$ty_minus1, sep="_")
data_8_s2_13[,c(46:47,50:61,64:69)] <- NA

data_8_s2_14 <- data_8_s2[data_8_s2$terminal_year == 2013 | data_8_s2$terminal_year == 2014,]
data_8_s2_14$ty_order <- 14
data_8_s2_14$if_diff_5yrs <- "N"
data_8_s2_14$ty <- 2013
data_8_s2_14$ty_minus1 <- data_8_s2_14$ty-1
data_8_s2_14$key_ty <- paste(data_8_s2_14$assessid, data_8_s2_14$ty, sep="_")
data_8_s2_14$key_ty_minus1 <- paste(data_8_s2_14$assessid, data_8_s2_14$ty_minus1, sep="_")
data_8_s2_14[,c(46:47,50:61,64:69)] <- NA

data_8_s2_15 <- data_8_s2[data_8_s2$terminal_year == 2013 | data_8_s2$assessid == "WGBFAS-CODBA2224-1993-2016-ICESIMP2016",]
data_8_s2_15$ty_order <- 15
data_8_s2_15$if_diff_5yrs <- "N"
data_8_s2_15$ty <- 2013
data_8_s2_15$ty_minus1 <- data_8_s2_15$ty-1
data_8_s2_15$key_ty <- paste(data_8_s2_15$assessid, data_8_s2_15$ty, sep="_")
data_8_s2_15$key_ty_minus1 <- paste(data_8_s2_15$assessid, data_8_s2_15$ty_minus1, sep="_")
data_8_s2_15[,c(46:47,50:61,64:69)] <- NA

data_8_s2_16 <- data_8_s2[data_8_s2$terminal_year == 2013 | data_8_s2$assessid == "WGBFAS-CODBA2224-1993-2016-ICESIMP2018",]
data_8_s2_16$ty_order <- 16
data_8_s2_16$if_diff_5yrs <- "N"
data_8_s2_16$ty <- 2013
data_8_s2_16$ty_minus1 <- data_8_s2_16$ty-1
data_8_s2_16$key_ty <- paste(data_8_s2_16$assessid, data_8_s2_16$ty, sep="_")
data_8_s2_16$key_ty_minus1 <- paste(data_8_s2_16$assessid, data_8_s2_16$ty_minus1, sep="_")
data_8_s2_16[,c(46:47,50:61,64:69)] <- NA

data_8_s2_17 <- data_8_s2[data_8_s2$terminal_year == 2013 | data_8_s2$terminal_year == 2016,]
data_8_s2_17$ty_order <- 17
data_8_s2_17$if_diff_5yrs <- "N"
data_8_s2_17$ty <- 2013
data_8_s2_17$ty_minus1 <- data_8_s2_17$ty-1
data_8_s2_17$key_ty <- paste(data_8_s2_17$assessid, data_8_s2_17$ty, sep="_")
data_8_s2_17$key_ty_minus1 <- paste(data_8_s2_17$assessid, data_8_s2_17$ty_minus1, sep="_")
data_8_s2_17[,c(46:47,50:61,64:69)] <- NA

data_8_s2_18 <- data_8_s2[data_8_s2$terminal_year == 2013 | data_8_s2$terminal_year == 2017,]
data_8_s2_18$ty_order <- 18
data_8_s2_18$if_diff_5yrs <- "N"
data_8_s2_18$ty <- 2013
data_8_s2_18$ty_minus1 <- data_8_s2_18$ty-1
data_8_s2_18$key_ty <- paste(data_8_s2_18$assessid, data_8_s2_18$ty, sep="_")
data_8_s2_18$key_ty_minus1 <- paste(data_8_s2_18$assessid, data_8_s2_18$ty_minus1, sep="_")
data_8_s2_18[,c(46:47,50:61,64:69)] <- NA

data_8_s2_19 <- data_8_s2[data_8_s2$terminal_year == 2014 | data_8_s2$assessid == "WGBFAS-CODBA2224-1993-2016-ICESIMP2016",]
data_8_s2_19$ty_order <- 19
data_8_s2_19$if_diff_5yrs <- "N"
data_8_s2_19$ty <- 2014
data_8_s2_19$ty_minus1 <- data_8_s2_19$ty-1
data_8_s2_19$key_ty <- paste(data_8_s2_19$assessid, data_8_s2_19$ty, sep="_")
data_8_s2_19$key_ty_minus1 <- paste(data_8_s2_19$assessid, data_8_s2_19$ty_minus1, sep="_")
data_8_s2_19[,c(46:47,50:61,64:69)] <- NA

data_8_s2_20 <- data_8_s2[data_8_s2$terminal_year == 2014 | data_8_s2$assessid == "WGBFAS-CODBA2224-1993-2016-ICESIMP2018",]
data_8_s2_20$ty_order <- 20
data_8_s2_20$if_diff_5yrs <- "N"
data_8_s2_20$ty <- 2014
data_8_s2_20$ty_minus1 <- data_8_s2_20$ty-1
data_8_s2_20$key_ty <- paste(data_8_s2_20$assessid, data_8_s2_20$ty, sep="_")
data_8_s2_20$key_ty_minus1 <- paste(data_8_s2_20$assessid, data_8_s2_20$ty_minus1, sep="_")
data_8_s2_20[,c(46:47,50:61,64:69)] <- NA

data_8_s2_21 <- data_8_s2[data_8_s2$terminal_year == 2014 | data_8_s2$terminal_year == 2016,]
data_8_s2_21$ty_order <- 21
data_8_s2_21$if_diff_5yrs <- "N"
data_8_s2_21$ty <- 2014
data_8_s2_21$ty_minus1 <- data_8_s2_21$ty-1
data_8_s2_21$key_ty <- paste(data_8_s2_21$assessid, data_8_s2_21$ty, sep="_")
data_8_s2_21$key_ty_minus1 <- paste(data_8_s2_21$assessid, data_8_s2_21$ty_minus1, sep="_")
data_8_s2_21[,c(46:47,50:61,64:69)] <- NA

data_8_s2_22 <- data_8_s2[data_8_s2$terminal_year == 2014 | data_8_s2$terminal_year == 2017,]
data_8_s2_22$ty_order <- 22
data_8_s2_22$if_diff_5yrs <- "N"
data_8_s2_22$ty <- 2014
data_8_s2_22$ty_minus1 <- data_8_s2_22$ty-1
data_8_s2_22$key_ty <- paste(data_8_s2_22$assessid, data_8_s2_22$ty, sep="_")
data_8_s2_22$key_ty_minus1 <- paste(data_8_s2_22$assessid, data_8_s2_22$ty_minus1, sep="_")
data_8_s2_22[,c(46:47,50:61,64:69)] <- NA

data_8_s2_23 <- data_8_s2[data_8_s2$terminal_year == 2015,]
data_8_s2_23$ty_order <- 23
data_8_s2_23$if_diff_5yrs <- "N"
data_8_s2_23$ty <- 2015
data_8_s2_23$ty_minus1 <- data_8_s2_23$ty-1
data_8_s2_23$key_ty <- paste(data_8_s2_23$assessid, data_8_s2_23$ty, sep="_")
data_8_s2_23$key_ty_minus1 <- paste(data_8_s2_23$assessid, data_8_s2_23$ty_minus1, sep="_")
data_8_s2_23[,c(46:47,50:61,64:69)] <- NA

data_8_s2_24 <- data_8_s2[data_8_s2$assessid == "WGBFAS-CODBA2224-1993-2016-ICESIMP2016" |  data_8_s2$terminal_year == 2016,]
data_8_s2_24$ty_order <- 24
data_8_s2_24$if_diff_5yrs <- "N"
data_8_s2_24$ty <- 2015
data_8_s2_24$ty_minus1 <- data_8_s2_24$ty-1
data_8_s2_24$key_ty <- paste(data_8_s2_24$assessid, data_8_s2_24$ty, sep="_")
data_8_s2_24$key_ty_minus1 <- paste(data_8_s2_24$assessid, data_8_s2_24$ty_minus1, sep="_")
data_8_s2_24[,c(46:47,50:61,64:69)] <- NA

data_8_s2_25 <- data_8_s2[data_8_s2$assessid == "WGBFAS-CODBA2224-1993-2016-ICESIMP2016" |  data_8_s2$terminal_year == 2017,]
data_8_s2_25$ty_order <- 25
data_8_s2_25$if_diff_5yrs <- "N"
data_8_s2_25$ty <- 2015
data_8_s2_25$ty_minus1 <- data_8_s2_25$ty-1
data_8_s2_25$key_ty <- paste(data_8_s2_25$assessid, data_8_s2_25$ty, sep="_")
data_8_s2_25$key_ty_minus1 <- paste(data_8_s2_25$assessid, data_8_s2_25$ty_minus1, sep="_")
data_8_s2_25[,c(46:47,50:61,64:69)] <- NA

data_8_s2_26 <- data_8_s2[data_8_s2$assessid == "WGBFAS-CODBA2224-1993-2016-ICESIMP2018" |  data_8_s2$terminal_year == 2016,]
data_8_s2_26$ty_order <- 26
data_8_s2_26$if_diff_5yrs <- "N"
data_8_s2_26$ty <- 2015
data_8_s2_26$ty_minus1 <- data_8_s2_26$ty-1
data_8_s2_26$key_ty <- paste(data_8_s2_26$assessid, data_8_s2_26$ty, sep="_")
data_8_s2_26$key_ty_minus1 <- paste(data_8_s2_26$assessid, data_8_s2_26$ty_minus1, sep="_")
data_8_s2_26[,c(46:47,50:61,64:69)] <- NA

data_8_s2_27 <- data_8_s2[data_8_s2$assessid == "WGBFAS-CODBA2224-1993-2016-ICESIMP2018"  |  data_8_s2$terminal_year == 2017,]
data_8_s2_27$ty_order <- 27
data_8_s2_27$if_diff_5yrs <- "N"
data_8_s2_27$ty <- 2015
data_8_s2_27$ty_minus1 <- data_8_s2_27$ty-1
data_8_s2_27$key_ty <- paste(data_8_s2_27$assessid, data_8_s2_27$ty, sep="_")
data_8_s2_27$key_ty_minus1 <- paste(data_8_s2_27$assessid, data_8_s2_27$ty_minus1, sep="_")
data_8_s2_27[,c(46:47,50:61,64:69)] <- NA

data_8_s2_28 <- data_8_s2[data_8_s2$terminal_year == 2016 |  data_8_s2$terminal_year == 2017,]
data_8_s2_28$ty_order <- 28
data_8_s2_28$if_diff_5yrs <- "N"
data_8_s2_28$ty <- 2016
data_8_s2_28$ty_minus1 <- data_8_s2_28$ty-1
data_8_s2_28$key_ty <- paste(data_8_s2_28$assessid, data_8_s2_28$ty, sep="_")
data_8_s2_28$key_ty_minus1 <- paste(data_8_s2_28$assessid, data_8_s2_28$ty_minus1, sep="_")
data_8_s2_28[,c(46:47,50:61,64:69)] <- NA

data_8_s2_1$interval <- max(data_8_s2_1$terminal_year) - min(data_8_s2_1$terminal_year)
data_8_s2_2$interval <- max(data_8_s2_2$terminal_year) - min(data_8_s2_2$terminal_year)
data_8_s2_3$interval <- max(data_8_s2_3$terminal_year) - min(data_8_s2_3$terminal_year)
data_8_s2_4$interval <- max(data_8_s2_4$terminal_year) - min(data_8_s2_4$terminal_year)
data_8_s2_5$interval <- max(data_8_s2_5$terminal_year) - min(data_8_s2_5$terminal_year)
data_8_s2_6$interval <- max(data_8_s2_6$terminal_year) - min(data_8_s2_6$terminal_year)
data_8_s2_7$interval <- max(data_8_s2_7$terminal_year) - min(data_8_s2_7$terminal_year)
data_8_s2_8$interval <- max(data_8_s2_8$terminal_year) - min(data_8_s2_8$terminal_year)
data_8_s2_9$interval <- max(data_8_s2_9$terminal_year) - min(data_8_s2_9$terminal_year)
data_8_s2_10$interval <- max(data_8_s2_10$terminal_year) - min(data_8_s2_10$terminal_year)
data_8_s2_11$interval <- max(data_8_s2_11$terminal_year) - min(data_8_s2_11$terminal_year)
data_8_s2_12$interval <- max(data_8_s2_12$terminal_year) - min(data_8_s2_12$terminal_year)
data_8_s2_13$interval <- max(data_8_s2_13$terminal_year) - min(data_8_s2_13$terminal_year)
data_8_s2_14$interval <- max(data_8_s2_14$terminal_year) - min(data_8_s2_14$terminal_year)
data_8_s2_15$interval <- max(data_8_s2_15$terminal_year) - min(data_8_s2_15$terminal_year)
data_8_s2_16$interval <- max(data_8_s2_16$terminal_year) - min(data_8_s2_16$terminal_year)
data_8_s2_17$interval <- max(data_8_s2_17$terminal_year) - min(data_8_s2_17$terminal_year)
data_8_s2_18$interval <- max(data_8_s2_18$terminal_year) - min(data_8_s2_18$terminal_year)
data_8_s2_19$interval <- max(data_8_s2_19$terminal_year) - min(data_8_s2_19$terminal_year)
data_8_s2_20$interval <- max(data_8_s2_20$terminal_year) - min(data_8_s2_20$terminal_year)
data_8_s2_21$interval <- max(data_8_s2_21$terminal_year) - min(data_8_s2_21$terminal_year)
data_8_s2_22$interval <- max(data_8_s2_22$terminal_year) - min(data_8_s2_22$terminal_year)
data_8_s2_23$interval <- max(data_8_s2_23$terminal_year) - min(data_8_s2_23$terminal_year)
data_8_s2_24$interval <- max(data_8_s2_24$terminal_year) - min(data_8_s2_24$terminal_year)
data_8_s2_25$interval <- max(data_8_s2_25$terminal_year) - min(data_8_s2_25$terminal_year)
data_8_s2_26$interval <- max(data_8_s2_26$terminal_year) - min(data_8_s2_26$terminal_year)
data_8_s2_27$interval <- max(data_8_s2_27$terminal_year) - min(data_8_s2_27$terminal_year)
data_8_s2_28$interval <- max(data_8_s2_28$terminal_year) - min(data_8_s2_28$terminal_year)

data_8_s2 <- rbind(data_8_s2_1, data_8_s2_2, data_8_s2_3, data_8_s2_4, data_8_s2_5, data_8_s2_6, data_8_s2_7, data_8_s2_8, data_8_s2_9, data_8_s2_10,
                   data_8_s2_11, data_8_s2_12, data_8_s2_13, data_8_s2_14, data_8_s2_15, data_8_s2_16, data_8_s2_17, data_8_s2_18, data_8_s2_19,
                   data_8_s2_20, data_8_s2_21, data_8_s2_22, data_8_s2_23, data_8_s2_24, data_8_s2_25, data_8_s2_26, data_8_s2_27, data_8_s2_28)

# assess 3
data_8_s3 <- data_8_assess[data_8_assess$stockid==ids[3],]

data_8_s3_1 <- data_8_s3[data_8_s3$terminal_year == 2010 | data_8_s3$terminal_year == 2012,]
data_8_s3_1$ty_order <- 1
data_8_s3_1$if_diff_5yrs <- "N"

data_8_s3_2 <- data_8_s3[data_8_s3$terminal_year == 2010 | data_8_s3$terminal_year == 2013,]
data_8_s3_2$ty_order <- 2
data_8_s3_2$if_diff_5yrs <- "N"

data_8_s3_3 <- data_8_s3[data_8_s3$terminal_year == 2010 | data_8_s3$terminal_year == 2014,]
data_8_s3_3$ty_order <- 3
data_8_s3_3$if_diff_5yrs <- "N"

data_8_s3_4 <- data_8_s3[data_8_s3$terminal_year == 2010 | data_8_s3$assessid == "WGNSSK-CODIIIaW-IV-VIId-1962-2016-ICESIMP2016",]
data_8_s3_4$ty_order <- 4
data_8_s3_4$if_diff_5yrs <- "N"

data_8_s3_5 <- data_8_s3[data_8_s3$terminal_year == 2010 | data_8_s3$assessid == "WGNSSK-CODIIIaW-IV-VIId-1962-2016-ICESIMP2018",]
data_8_s3_5$ty_order <- 5
data_8_s3_5$if_diff_5yrs <- "N"

data_8_s3_6 <- data_8_s3[data_8_s3$terminal_year == 2010 | data_8_s3$terminal_year == 2016,]
data_8_s3_6$ty_order <- 6
data_8_s3_6$if_diff_5yrs <- "Y"

data_8_s3_7 <- data_8_s3[data_8_s3$terminal_year == 2010 | data_8_s3$terminal_year == 2017,]
data_8_s3_7$ty_order <- 7
data_8_s3_7$if_diff_5yrs <- "Y"

data_8_s3_8 <- data_8_s3[data_8_s3$terminal_year == 2012 | data_8_s3$terminal_year == 2013,]
data_8_s3_8$ty_order <- 8
data_8_s3_8$if_diff_5yrs <- "N"
data_8_s3_8$ty <- 2012
data_8_s3_8$ty_minus1 <- data_8_s3_8$ty-1
data_8_s3_8$key_ty <- paste(data_8_s3_8$assessid, data_8_s3_8$ty, sep="_")
data_8_s3_8$key_ty_minus1 <- paste(data_8_s3_8$assessid, data_8_s3_8$ty_minus1, sep="_")
data_8_s3_8[,c(46:47,50:61,64:69)] <- NA

data_8_s3_9 <- data_8_s3[data_8_s3$terminal_year == 2012 | data_8_s3$terminal_year == 2014,]
data_8_s3_9$ty_order <- 9
data_8_s3_9$if_diff_5yrs <- "N"
data_8_s3_9$ty <- 2012
data_8_s3_9$ty_minus1 <- data_8_s3_9$ty-1
data_8_s3_9$key_ty <- paste(data_8_s3_9$assessid, data_8_s3_9$ty, sep="_")
data_8_s3_9$key_ty_minus1 <- paste(data_8_s3_9$assessid, data_8_s3_9$ty_minus1, sep="_")
data_8_s3_9[,c(46:47,50:61,64:69)] <- NA

data_8_s3_10 <- data_8_s3[data_8_s3$terminal_year == 2012 | data_8_s3$assessid == "WGNSSK-CODIIIaW-IV-VIId-1962-2016-ICESIMP2016",]
data_8_s3_10$ty_order <- 10
data_8_s3_10$if_diff_5yrs <- "N"
data_8_s3_10$ty <- 2012
data_8_s3_10$ty_minus1 <- data_8_s3_10$ty-1
data_8_s3_10$key_ty <- paste(data_8_s3_10$assessid, data_8_s3_10$ty, sep="_")
data_8_s3_10$key_ty_minus1 <- paste(data_8_s3_10$assessid, data_8_s3_10$ty_minus1, sep="_")
data_8_s3_10[,c(46:47,50:61,64:69)] <- NA

data_8_s3_11 <- data_8_s3[data_8_s3$terminal_year == 2012 | data_8_s3$assessid == "WGNSSK-CODIIIaW-IV-VIId-1962-2016-ICESIMP2018",]
data_8_s3_11$ty_order <- 11
data_8_s3_11$if_diff_5yrs <- "N"
data_8_s3_11$ty <- 2012
data_8_s3_11$ty_minus1 <- data_8_s3_11$ty-1
data_8_s3_11$key_ty <- paste(data_8_s3_11$assessid, data_8_s3_11$ty, sep="_")
data_8_s3_11$key_ty_minus1 <- paste(data_8_s3_11$assessid, data_8_s3_11$ty_minus1, sep="_")
data_8_s3_11[,c(46:47,50:61,64:69)] <- NA

data_8_s3_12 <- data_8_s3[data_8_s3$terminal_year == 2012 | data_8_s3$terminal_year == 2016,]
data_8_s3_12$ty_order <- 12
data_8_s3_12$if_diff_5yrs <- "N"
data_8_s3_12$ty <- 2012
data_8_s3_12$ty_minus1 <- data_8_s3_12$ty-1
data_8_s3_12$key_ty <- paste(data_8_s3_12$assessid, data_8_s3_12$ty, sep="_")
data_8_s3_12$key_ty_minus1 <- paste(data_8_s3_12$assessid, data_8_s3_12$ty_minus1, sep="_")
data_8_s3_12[,c(46:47,50:61,64:69)] <- NA

data_8_s3_13 <- data_8_s3[data_8_s3$terminal_year == 2012 | data_8_s3$terminal_year == 2017,]
data_8_s3_13$ty_order <- 13
data_8_s3_13$if_diff_5yrs <- "N"
data_8_s3_13$ty <- 2012
data_8_s3_13$ty_minus1 <- data_8_s3_13$ty-1
data_8_s3_13$key_ty <- paste(data_8_s3_13$assessid, data_8_s3_13$ty, sep="_")
data_8_s3_13$key_ty_minus1 <- paste(data_8_s3_13$assessid, data_8_s3_13$ty_minus1, sep="_")
data_8_s3_13[,c(46:47,50:61,64:69)] <- NA

data_8_s3_14 <- data_8_s3[data_8_s3$terminal_year == 2013 | data_8_s3$terminal_year == 2014,]
data_8_s3_14$ty_order <- 14
data_8_s3_14$if_diff_5yrs <- "N"
data_8_s3_14$ty <- 2013
data_8_s3_14$ty_minus1 <- data_8_s3_14$ty-1
data_8_s3_14$key_ty <- paste(data_8_s3_14$assessid, data_8_s3_14$ty, sep="_")
data_8_s3_14$key_ty_minus1 <- paste(data_8_s3_14$assessid, data_8_s3_14$ty_minus1, sep="_")
data_8_s3_14[,c(46:47,50:61,64:69)] <- NA

data_8_s3_15 <- data_8_s3[data_8_s3$terminal_year == 2013 | data_8_s3$assessid == "WGNSSK-CODIIIaW-IV-VIId-1962-2016-ICESIMP2016",]
data_8_s3_15$ty_order <- 15
data_8_s3_15$if_diff_5yrs <- "N"
data_8_s3_15$ty <- 2013
data_8_s3_15$ty_minus1 <- data_8_s3_15$ty-1
data_8_s3_15$key_ty <- paste(data_8_s3_15$assessid, data_8_s3_15$ty, sep="_")
data_8_s3_15$key_ty_minus1 <- paste(data_8_s3_15$assessid, data_8_s3_15$ty_minus1, sep="_")
data_8_s3_15[,c(46:47,50:61,64:69)] <- NA

data_8_s3_16 <- data_8_s3[data_8_s3$terminal_year == 2013 | data_8_s3$assessid == "WGNSSK-CODIIIaW-IV-VIId-1962-2016-ICESIMP2018",]
data_8_s3_16$ty_order <- 16
data_8_s3_16$if_diff_5yrs <- "N"
data_8_s3_16$ty <- 2013
data_8_s3_16$ty_minus1 <- data_8_s3_16$ty-1
data_8_s3_16$key_ty <- paste(data_8_s3_16$assessid, data_8_s3_16$ty, sep="_")
data_8_s3_16$key_ty_minus1 <- paste(data_8_s3_16$assessid, data_8_s3_16$ty_minus1, sep="_")
data_8_s3_16[,c(46:47,50:61,64:69)] <- NA

data_8_s3_17 <- data_8_s3[data_8_s3$terminal_year == 2013 | data_8_s3$terminal_year == 2016,]
data_8_s3_17$ty_order <- 17
data_8_s3_17$if_diff_5yrs <- "N"
data_8_s3_17$ty <- 2013
data_8_s3_17$ty_minus1 <- data_8_s3_17$ty-1
data_8_s3_17$key_ty <- paste(data_8_s3_17$assessid, data_8_s3_17$ty, sep="_")
data_8_s3_17$key_ty_minus1 <- paste(data_8_s3_17$assessid, data_8_s3_17$ty_minus1, sep="_")
data_8_s3_17[,c(46:47,50:61,64:69)] <- NA

data_8_s3_18 <- data_8_s3[data_8_s3$terminal_year == 2013 | data_8_s3$terminal_year == 2017,]
data_8_s3_18$ty_order <- 18
data_8_s3_18$if_diff_5yrs <- "N"
data_8_s3_18$ty <- 2013
data_8_s3_18$ty_minus1 <- data_8_s3_18$ty-1
data_8_s3_18$key_ty <- paste(data_8_s3_18$assessid, data_8_s3_18$ty, sep="_")
data_8_s3_18$key_ty_minus1 <- paste(data_8_s3_18$assessid, data_8_s3_18$ty_minus1, sep="_")
data_8_s3_18[,c(46:47,50:61,64:69)] <- NA

data_8_s3_19 <- data_8_s3[data_8_s3$terminal_year == 2014 | data_8_s3$assessid == "WGNSSK-CODIIIaW-IV-VIId-1962-2016-ICESIMP2016",]
data_8_s3_19$ty_order <- 19
data_8_s3_19$if_diff_5yrs <- "N"
data_8_s3_19$ty <- 2014
data_8_s3_19$ty_minus1 <- data_8_s3_19$ty-1
data_8_s3_19$key_ty <- paste(data_8_s3_19$assessid, data_8_s3_19$ty, sep="_")
data_8_s3_19$key_ty_minus1 <- paste(data_8_s3_19$assessid, data_8_s3_19$ty_minus1, sep="_")
data_8_s3_19[,c(46:47,50:61,64:69)] <- NA

data_8_s3_20 <- data_8_s3[data_8_s3$terminal_year == 2014 | data_8_s3$assessid == "WGNSSK-CODIIIaW-IV-VIId-1962-2016-ICESIMP2018",]
data_8_s3_20$ty_order <- 20
data_8_s3_20$if_diff_5yrs <- "N"
data_8_s3_20$ty <- 2014
data_8_s3_20$ty_minus1 <- data_8_s3_20$ty-1
data_8_s3_20$key_ty <- paste(data_8_s3_20$assessid, data_8_s3_20$ty, sep="_")
data_8_s3_20$key_ty_minus1 <- paste(data_8_s3_20$assessid, data_8_s3_20$ty_minus1, sep="_")
data_8_s3_20[,c(46:47,50:61,64:69)] <- NA

data_8_s3_21 <- data_8_s3[data_8_s3$terminal_year == 2014 | data_8_s3$terminal_year == 2016,]
data_8_s3_21$ty_order <- 21
data_8_s3_21$if_diff_5yrs <- "N"
data_8_s3_21$ty <- 2014
data_8_s3_21$ty_minus1 <- data_8_s3_21$ty-1
data_8_s3_21$key_ty <- paste(data_8_s3_21$assessid, data_8_s3_21$ty, sep="_")
data_8_s3_21$key_ty_minus1 <- paste(data_8_s3_21$assessid, data_8_s3_21$ty_minus1, sep="_")
data_8_s3_21[,c(46:47,50:61,64:69)] <- NA

data_8_s3_22 <- data_8_s3[data_8_s3$terminal_year == 2014 | data_8_s3$terminal_year == 2017,]
data_8_s3_22$ty_order <- 22
data_8_s3_22$if_diff_5yrs <- "N"
data_8_s3_22$ty <- 2014
data_8_s3_22$ty_minus1 <- data_8_s3_22$ty-1
data_8_s3_22$key_ty <- paste(data_8_s3_22$assessid, data_8_s3_22$ty, sep="_")
data_8_s3_22$key_ty_minus1 <- paste(data_8_s3_22$assessid, data_8_s3_22$ty_minus1, sep="_")
data_8_s3_22[,c(46:47,50:61,64:69)] <- NA

data_8_s3_23 <- data_8_s3[data_8_s3$terminal_year == 2015,]
data_8_s3_23$ty_order <- 23
data_8_s3_23$if_diff_5yrs <- "N"
data_8_s3_23$ty <- 2015
data_8_s3_23$ty_minus1 <- data_8_s3_23$ty-1
data_8_s3_23$key_ty <- paste(data_8_s3_23$assessid, data_8_s3_23$ty, sep="_")
data_8_s3_23$key_ty_minus1 <- paste(data_8_s3_23$assessid, data_8_s3_23$ty_minus1, sep="_")
data_8_s3_23[,c(46:47,50:61,64:69)] <- NA

data_8_s3_24 <- data_8_s3[data_8_s3$assessid == "WGNSSK-CODIIIaW-IV-VIId-1962-2016-ICESIMP2016" |  data_8_s3$terminal_year == 2016,]
data_8_s3_24$ty_order <- 24
data_8_s3_24$if_diff_5yrs <- "N"
data_8_s3_24$ty <- 2015
data_8_s3_24$ty_minus1 <- data_8_s3_24$ty-1
data_8_s3_24$key_ty <- paste(data_8_s3_24$assessid, data_8_s3_24$ty, sep="_")
data_8_s3_24$key_ty_minus1 <- paste(data_8_s3_24$assessid, data_8_s3_24$ty_minus1, sep="_")
data_8_s3_24[,c(46:47,50:61,64:69)] <- NA

data_8_s3_25 <- data_8_s3[data_8_s3$assessid == "WGNSSK-CODIIIaW-IV-VIId-1962-2016-ICESIMP2016" |  data_8_s3$terminal_year == 2017,]
data_8_s3_25$ty_order <- 25
data_8_s3_25$if_diff_5yrs <- "N"
data_8_s3_25$ty <- 2015
data_8_s3_25$ty_minus1 <- data_8_s3_25$ty-1
data_8_s3_25$key_ty <- paste(data_8_s3_25$assessid, data_8_s3_25$ty, sep="_")
data_8_s3_25$key_ty_minus1 <- paste(data_8_s3_25$assessid, data_8_s3_25$ty_minus1, sep="_")
data_8_s3_25[,c(46:47,50:61,64:69)] <- NA

data_8_s3_26 <- data_8_s3[data_8_s3$assessid == "WGNSSK-CODIIIaW-IV-VIId-1962-2016-ICESIMP2018" |  data_8_s3$terminal_year == 2016,]
data_8_s3_26$ty_order <- 26
data_8_s3_26$if_diff_5yrs <- "N"
data_8_s3_26$ty <- 2015
data_8_s3_26$ty_minus1 <- data_8_s3_26$ty-1
data_8_s3_26$key_ty <- paste(data_8_s3_26$assessid, data_8_s3_26$ty, sep="_")
data_8_s3_26$key_ty_minus1 <- paste(data_8_s3_26$assessid, data_8_s3_26$ty_minus1, sep="_")
data_8_s3_26[,c(46:47,50:61,64:69)] <- NA

data_8_s3_27 <- data_8_s3[data_8_s3$assessid == "WGNSSK-CODIIIaW-IV-VIId-1962-2016-ICESIMP2018"  |  data_8_s3$terminal_year == 2017,]
data_8_s3_27$ty_order <- 27
data_8_s3_27$if_diff_5yrs <- "N"
data_8_s3_27$ty <- 2015
data_8_s3_27$ty_minus1 <- data_8_s3_27$ty-1
data_8_s3_27$key_ty <- paste(data_8_s3_27$assessid, data_8_s3_27$ty, sep="_")
data_8_s3_27$key_ty_minus1 <- paste(data_8_s3_27$assessid, data_8_s3_27$ty_minus1, sep="_")
data_8_s3_27[,c(46:47,50:61,64:69)] <- NA

data_8_s3_28 <- data_8_s3[data_8_s3$terminal_year == 2016 |  data_8_s3$terminal_year == 2017,]
data_8_s3_28$ty_order <- 28
data_8_s3_28$if_diff_5yrs <- "N"
data_8_s3_28$ty <- 2016
data_8_s3_28$ty_minus1 <- data_8_s3_28$ty-1
data_8_s3_28$key_ty <- paste(data_8_s3_28$assessid, data_8_s3_28$ty, sep="_")
data_8_s3_28$key_ty_minus1 <- paste(data_8_s3_28$assessid, data_8_s3_28$ty_minus1, sep="_")
data_8_s3_28[,c(46:47,50:61,64:69)] <- NA

data_8_s3_1$interval <- max(data_8_s3_1$terminal_year) - min(data_8_s3_1$terminal_year)
data_8_s3_2$interval <- max(data_8_s3_2$terminal_year) - min(data_8_s3_2$terminal_year)
data_8_s3_3$interval <- max(data_8_s3_3$terminal_year) - min(data_8_s3_3$terminal_year)
data_8_s3_4$interval <- max(data_8_s3_4$terminal_year) - min(data_8_s3_4$terminal_year)
data_8_s3_5$interval <- max(data_8_s3_5$terminal_year) - min(data_8_s3_5$terminal_year)
data_8_s3_6$interval <- max(data_8_s3_6$terminal_year) - min(data_8_s3_6$terminal_year)
data_8_s3_7$interval <- max(data_8_s3_7$terminal_year) - min(data_8_s3_7$terminal_year)
data_8_s3_8$interval <- max(data_8_s3_8$terminal_year) - min(data_8_s3_8$terminal_year)
data_8_s3_9$interval <- max(data_8_s3_9$terminal_year) - min(data_8_s3_9$terminal_year)
data_8_s3_10$interval <- max(data_8_s3_10$terminal_year) - min(data_8_s3_10$terminal_year)
data_8_s3_11$interval <- max(data_8_s3_11$terminal_year) - min(data_8_s3_11$terminal_year)
data_8_s3_12$interval <- max(data_8_s3_12$terminal_year) - min(data_8_s3_12$terminal_year)
data_8_s3_13$interval <- max(data_8_s3_13$terminal_year) - min(data_8_s3_13$terminal_year)
data_8_s3_14$interval <- max(data_8_s3_14$terminal_year) - min(data_8_s3_14$terminal_year)
data_8_s3_15$interval <- max(data_8_s3_15$terminal_year) - min(data_8_s3_15$terminal_year)
data_8_s3_16$interval <- max(data_8_s3_16$terminal_year) - min(data_8_s3_16$terminal_year)
data_8_s3_17$interval <- max(data_8_s3_17$terminal_year) - min(data_8_s3_17$terminal_year)
data_8_s3_18$interval <- max(data_8_s3_18$terminal_year) - min(data_8_s3_18$terminal_year)
data_8_s3_19$interval <- max(data_8_s3_19$terminal_year) - min(data_8_s3_19$terminal_year)
data_8_s3_20$interval <- max(data_8_s3_20$terminal_year) - min(data_8_s3_20$terminal_year)
data_8_s3_21$interval <- max(data_8_s3_21$terminal_year) - min(data_8_s3_21$terminal_year)
data_8_s3_22$interval <- max(data_8_s3_22$terminal_year) - min(data_8_s3_22$terminal_year)
data_8_s3_23$interval <- max(data_8_s3_23$terminal_year) - min(data_8_s3_23$terminal_year)
data_8_s3_24$interval <- max(data_8_s3_24$terminal_year) - min(data_8_s3_24$terminal_year)
data_8_s3_25$interval <- max(data_8_s3_25$terminal_year) - min(data_8_s3_25$terminal_year)
data_8_s3_26$interval <- max(data_8_s3_26$terminal_year) - min(data_8_s3_26$terminal_year)
data_8_s3_27$interval <- max(data_8_s3_27$terminal_year) - min(data_8_s3_27$terminal_year)
data_8_s3_28$interval <- max(data_8_s3_28$terminal_year) - min(data_8_s3_28$terminal_year)

data_8_s3_1$interval <- max(data_8_s3_1$terminal_year) - min(data_8_s3_1$terminal_year)
data_8_s3_2$interval <- max(data_8_s3_2$terminal_year) - min(data_8_s3_2$terminal_year)
data_8_s3_3$interval <- max(data_8_s3_3$terminal_year) - min(data_8_s3_3$terminal_year)
data_8_s3_4$interval <- max(data_8_s3_4$terminal_year) - min(data_8_s3_4$terminal_year)
data_8_s3_5$interval <- max(data_8_s3_5$terminal_year) - min(data_8_s3_5$terminal_year)
data_8_s3_6$interval <- max(data_8_s3_6$terminal_year) - min(data_8_s3_6$terminal_year)
data_8_s3_7$interval <- max(data_8_s3_7$terminal_year) - min(data_8_s3_7$terminal_year)
data_8_s3_8$interval <- max(data_8_s3_8$terminal_year) - min(data_8_s3_8$terminal_year)
data_8_s3_9$interval <- max(data_8_s3_9$terminal_year) - min(data_8_s3_9$terminal_year)
data_8_s3_10$interval <- max(data_8_s3_10$terminal_year) - min(data_8_s3_10$terminal_year)
data_8_s3_11$interval <- max(data_8_s3_11$terminal_year) - min(data_8_s3_11$terminal_year)
data_8_s3_12$interval <- max(data_8_s3_12$terminal_year) - min(data_8_s3_12$terminal_year)
data_8_s3_13$interval <- max(data_8_s3_13$terminal_year) - min(data_8_s3_13$terminal_year)
data_8_s3_14$interval <- max(data_8_s3_14$terminal_year) - min(data_8_s3_14$terminal_year)
data_8_s3_15$interval <- max(data_8_s3_15$terminal_year) - min(data_8_s3_15$terminal_year)
data_8_s3_16$interval <- max(data_8_s3_16$terminal_year) - min(data_8_s3_16$terminal_year)
data_8_s3_17$interval <- max(data_8_s3_17$terminal_year) - min(data_8_s3_17$terminal_year)
data_8_s3_18$interval <- max(data_8_s3_18$terminal_year) - min(data_8_s3_18$terminal_year)
data_8_s3_19$interval <- max(data_8_s3_19$terminal_year) - min(data_8_s3_19$terminal_year)
data_8_s3_20$interval <- max(data_8_s3_20$terminal_year) - min(data_8_s3_20$terminal_year)
data_8_s3_21$interval <- max(data_8_s3_21$terminal_year) - min(data_8_s3_21$terminal_year)
data_8_s3_22$interval <- max(data_8_s3_22$terminal_year) - min(data_8_s3_22$terminal_year)
data_8_s3_23$interval <- max(data_8_s3_23$terminal_year) - min(data_8_s3_23$terminal_year)
data_8_s3_24$interval <- max(data_8_s3_24$terminal_year) - min(data_8_s3_24$terminal_year)
data_8_s3_25$interval <- max(data_8_s3_25$terminal_year) - min(data_8_s3_25$terminal_year)
data_8_s3_26$interval <- max(data_8_s3_26$terminal_year) - min(data_8_s3_26$terminal_year)
data_8_s3_27$interval <- max(data_8_s3_27$terminal_year) - min(data_8_s3_27$terminal_year)
data_8_s3_28$interval <- max(data_8_s3_28$terminal_year) - min(data_8_s3_28$terminal_year)

data_8_s3 <- rbind(data_8_s3_1, data_8_s3_2, data_8_s3_3, data_8_s3_4, data_8_s3_5, data_8_s3_6, data_8_s3_7, data_8_s3_8, data_8_s3_9, data_8_s3_10,
                   data_8_s3_11, data_8_s3_12, data_8_s3_13, data_8_s3_14, data_8_s3_15, data_8_s3_16, data_8_s3_17, data_8_s3_18, data_8_s3_19,
                   data_8_s3_20, data_8_s3_21, data_8_s3_22, data_8_s3_23, data_8_s3_24, data_8_s3_25, data_8_s3_26, data_8_s3_27, data_8_s3_28)

# assess 4
data_8_s4 <- data_8_assess[data_8_assess$stockid==ids[4],]

data_8_s4_1 <- data_8_s4[data_8_s4$terminal_year == 2011 | data_8_s4$terminal_year == 2012,]
data_8_s4_1$ty_order <- 1
data_8_s4_1$if_diff_5yrs <- "N"

data_8_s4_2 <- data_8_s4[data_8_s4$terminal_year == 2011 | data_8_s4$terminal_year == 2013,]
data_8_s4_2$ty_order <- 2
data_8_s4_2$if_diff_5yrs <- "N"

data_8_s4_3 <- data_8_s4[data_8_s4$terminal_year == 2011 | data_8_s4$terminal_year == 2014,]
data_8_s4_3$ty_order <- 3
data_8_s4_3$if_diff_5yrs <- "N"

data_8_s4_4 <- data_8_s4[data_8_s4$terminal_year == 2011 | data_8_s4$assessid == "WGNSSK-POLLNS-VI-IIIa-1964-2016-ICESIMP2016",]
data_8_s4_4$ty_order <- 4
data_8_s4_4$if_diff_5yrs <- "N"

data_8_s4_5 <- data_8_s4[data_8_s4$terminal_year == 2011 | data_8_s4$assessid == "WGNSSK-POLLNS-VI-IIIa-1964-2016-ICESIMP2018",]
data_8_s4_5$ty_order <- 5
data_8_s4_5$if_diff_5yrs <- "N"

data_8_s4_6 <- data_8_s4[data_8_s4$terminal_year == 2011 | data_8_s4$terminal_year == 2016,]
data_8_s4_6$ty_order <- 6
data_8_s4_6$if_diff_5yrs <- "N"

data_8_s4_7 <- data_8_s4[data_8_s4$terminal_year == 2011 | data_8_s4$terminal_year == 2017,]
data_8_s4_7$ty_order <- 7
data_8_s4_7$if_diff_5yrs <- "Y"

data_8_s4_8 <- data_8_s4[data_8_s4$terminal_year == 2012 | data_8_s4$terminal_year == 2013,]
data_8_s4_8$ty_order <- 8
data_8_s4_8$if_diff_5yrs <- "N"
data_8_s4_8$ty <- 2012
data_8_s4_8$ty_minus1 <- data_8_s4_8$ty-1
data_8_s4_8$key_ty <- paste(data_8_s4_8$assessid, data_8_s4_8$ty, sep="_")
data_8_s4_8$key_ty_minus1 <- paste(data_8_s4_8$assessid, data_8_s4_8$ty_minus1, sep="_")
data_8_s4_8[,c(46:47,50:61,64:69)] <- NA

data_8_s4_9 <- data_8_s4[data_8_s4$terminal_year == 2012 | data_8_s4$terminal_year == 2014,]
data_8_s4_9$ty_order <- 9
data_8_s4_9$if_diff_5yrs <- "N"
data_8_s4_9$ty <- 2012
data_8_s4_9$ty_minus1 <- data_8_s4_9$ty-1
data_8_s4_9$key_ty <- paste(data_8_s4_9$assessid, data_8_s4_9$ty, sep="_")
data_8_s4_9$key_ty_minus1 <- paste(data_8_s4_9$assessid, data_8_s4_9$ty_minus1, sep="_")
data_8_s4_9[,c(46:47,50:61,64:69)] <- NA

data_8_s4_10 <- data_8_s4[data_8_s4$terminal_year == 2012 | data_8_s4$assessid == "WGNSSK-POLLNS-VI-IIIa-1964-2016-ICESIMP2016",]
data_8_s4_10$ty_order <- 10
data_8_s4_10$if_diff_5yrs <- "N"
data_8_s4_10$ty <- 2012
data_8_s4_10$ty_minus1 <- data_8_s4_10$ty-1
data_8_s4_10$key_ty <- paste(data_8_s4_10$assessid, data_8_s4_10$ty, sep="_")
data_8_s4_10$key_ty_minus1 <- paste(data_8_s4_10$assessid, data_8_s4_10$ty_minus1, sep="_")
data_8_s4_10[,c(46:47,50:61,64:69)] <- NA

data_8_s4_11 <- data_8_s4[data_8_s4$terminal_year == 2012 | data_8_s4$assessid == "WGNSSK-POLLNS-VI-IIIa-1964-2016-ICESIMP2018",]
data_8_s4_11$ty_order <- 11
data_8_s4_11$if_diff_5yrs <- "N"
data_8_s4_11$ty <- 2012
data_8_s4_11$ty_minus1 <- data_8_s4_11$ty-1
data_8_s4_11$key_ty <- paste(data_8_s4_11$assessid, data_8_s4_11$ty, sep="_")
data_8_s4_11$key_ty_minus1 <- paste(data_8_s4_11$assessid, data_8_s4_11$ty_minus1, sep="_")
data_8_s4_11[,c(46:47,50:61,64:69)] <- NA

data_8_s4_12 <- data_8_s4[data_8_s4$terminal_year == 2012 | data_8_s4$terminal_year == 2016,]
data_8_s4_12$ty_order <- 12
data_8_s4_12$if_diff_5yrs <- "N"
data_8_s4_12$ty <- 2012
data_8_s4_12$ty_minus1 <- data_8_s4_12$ty-1
data_8_s4_12$key_ty <- paste(data_8_s4_12$assessid, data_8_s4_12$ty, sep="_")
data_8_s4_12$key_ty_minus1 <- paste(data_8_s4_12$assessid, data_8_s4_12$ty_minus1, sep="_")
data_8_s4_12[,c(46:47,50:61,64:69)] <- NA

data_8_s4_13 <- data_8_s4[data_8_s4$terminal_year == 2012 | data_8_s4$terminal_year == 2017,]
data_8_s4_13$ty_order <- 13
data_8_s4_13$if_diff_5yrs <- "N"
data_8_s4_13$ty <- 2012
data_8_s4_13$ty_minus1 <- data_8_s4_13$ty-1
data_8_s4_13$key_ty <- paste(data_8_s4_13$assessid, data_8_s4_13$ty, sep="_")
data_8_s4_13$key_ty_minus1 <- paste(data_8_s4_13$assessid, data_8_s4_13$ty_minus1, sep="_")
data_8_s4_13[,c(46:47,50:61,64:69)] <- NA

data_8_s4_14 <- data_8_s4[data_8_s4$terminal_year == 2013 | data_8_s4$terminal_year == 2014,]
data_8_s4_14$ty_order <- 14
data_8_s4_14$if_diff_5yrs <- "N"
data_8_s4_14$ty <- 2013
data_8_s4_14$ty_minus1 <- data_8_s4_14$ty-1
data_8_s4_14$key_ty <- paste(data_8_s4_14$assessid, data_8_s4_14$ty, sep="_")
data_8_s4_14$key_ty_minus1 <- paste(data_8_s4_14$assessid, data_8_s4_14$ty_minus1, sep="_")
data_8_s4_14[,c(46:47,50:61,64:69)] <- NA

data_8_s4_15 <- data_8_s4[data_8_s4$terminal_year == 2013 | data_8_s4$assessid == "WGNSSK-POLLNS-VI-IIIa-1964-2016-ICESIMP2016",]
data_8_s4_15$ty_order <- 15
data_8_s4_15$if_diff_5yrs <- "N"
data_8_s4_15$ty <- 2013
data_8_s4_15$ty_minus1 <- data_8_s4_15$ty-1
data_8_s4_15$key_ty <- paste(data_8_s4_15$assessid, data_8_s4_15$ty, sep="_")
data_8_s4_15$key_ty_minus1 <- paste(data_8_s4_15$assessid, data_8_s4_15$ty_minus1, sep="_")
data_8_s4_15[,c(46:47,50:61,64:69)] <- NA

data_8_s4_16 <- data_8_s4[data_8_s4$terminal_year == 2013 | data_8_s4$assessid == "WGNSSK-POLLNS-VI-IIIa-1964-2016-ICESIMP2018",]
data_8_s4_16$ty_order <- 16
data_8_s4_16$if_diff_5yrs <- "N"
data_8_s4_16$ty <- 2013
data_8_s4_16$ty_minus1 <- data_8_s4_16$ty-1
data_8_s4_16$key_ty <- paste(data_8_s4_16$assessid, data_8_s4_16$ty, sep="_")
data_8_s4_16$key_ty_minus1 <- paste(data_8_s4_16$assessid, data_8_s4_16$ty_minus1, sep="_")
data_8_s4_16[,c(46:47,50:61,64:69)] <- NA

data_8_s4_17 <- data_8_s4[data_8_s4$terminal_year == 2013 | data_8_s4$terminal_year == 2016,]
data_8_s4_17$ty_order <- 17
data_8_s4_17$if_diff_5yrs <- "N"
data_8_s4_17$ty <- 2013
data_8_s4_17$ty_minus1 <- data_8_s4_17$ty-1
data_8_s4_17$key_ty <- paste(data_8_s4_17$assessid, data_8_s4_17$ty, sep="_")
data_8_s4_17$key_ty_minus1 <- paste(data_8_s4_17$assessid, data_8_s4_17$ty_minus1, sep="_")
data_8_s4_17[,c(46:47,50:61,64:69)] <- NA

data_8_s4_18 <- data_8_s4[data_8_s4$terminal_year == 2013 | data_8_s4$terminal_year == 2017,]
data_8_s4_18$ty_order <- 18
data_8_s4_18$if_diff_5yrs <- "N"
data_8_s4_18$ty <- 2013
data_8_s4_18$ty_minus1 <- data_8_s4_18$ty-1
data_8_s4_18$key_ty <- paste(data_8_s4_18$assessid, data_8_s4_18$ty, sep="_")
data_8_s4_18$key_ty_minus1 <- paste(data_8_s4_18$assessid, data_8_s4_18$ty_minus1, sep="_")
data_8_s4_18[,c(46:47,50:61,64:69)] <- NA

data_8_s4_19 <- data_8_s4[data_8_s4$terminal_year == 2014 | data_8_s4$assessid == "WGNSSK-POLLNS-VI-IIIa-1964-2016-ICESIMP2016",]
data_8_s4_19$ty_order <- 19
data_8_s4_19$if_diff_5yrs <- "N"
data_8_s4_19$ty <- 2014
data_8_s4_19$ty_minus1 <- data_8_s4_19$ty-1
data_8_s4_19$key_ty <- paste(data_8_s4_19$assessid, data_8_s4_19$ty, sep="_")
data_8_s4_19$key_ty_minus1 <- paste(data_8_s4_19$assessid, data_8_s4_19$ty_minus1, sep="_")
data_8_s4_19[,c(46:47,50:61,64:69)] <- NA

data_8_s4_20 <- data_8_s4[data_8_s4$terminal_year == 2014 | data_8_s4$assessid == "WGNSSK-POLLNS-VI-IIIa-1964-2016-ICESIMP2018",]
data_8_s4_20$ty_order <- 20
data_8_s4_20$if_diff_5yrs <- "N"
data_8_s4_20$ty <- 2014
data_8_s4_20$ty_minus1 <- data_8_s4_20$ty-1
data_8_s4_20$key_ty <- paste(data_8_s4_20$assessid, data_8_s4_20$ty, sep="_")
data_8_s4_20$key_ty_minus1 <- paste(data_8_s4_20$assessid, data_8_s4_20$ty_minus1, sep="_")
data_8_s4_20[,c(46:47,50:61,64:69)] <- NA

data_8_s4_21 <- data_8_s4[data_8_s4$terminal_year == 2014 | data_8_s4$terminal_year == 2016,]
data_8_s4_21$ty_order <- 21
data_8_s4_21$if_diff_5yrs <- "N"
data_8_s4_21$ty <- 2014
data_8_s4_21$ty_minus1 <- data_8_s4_21$ty-1
data_8_s4_21$key_ty <- paste(data_8_s4_21$assessid, data_8_s4_21$ty, sep="_")
data_8_s4_21$key_ty_minus1 <- paste(data_8_s4_21$assessid, data_8_s4_21$ty_minus1, sep="_")
data_8_s4_21[,c(46:47,50:61,64:69)] <- NA

data_8_s4_22 <- data_8_s4[data_8_s4$terminal_year == 2014 | data_8_s4$terminal_year == 2017,]
data_8_s4_22$ty_order <- 22
data_8_s4_22$if_diff_5yrs <- "N"
data_8_s4_22$ty <- 2014
data_8_s4_22$ty_minus1 <- data_8_s4_22$ty-1
data_8_s4_22$key_ty <- paste(data_8_s4_22$assessid, data_8_s4_22$ty, sep="_")
data_8_s4_22$key_ty_minus1 <- paste(data_8_s4_22$assessid, data_8_s4_22$ty_minus1, sep="_")
data_8_s4_22[,c(46:47,50:61,64:69)] <- NA

data_8_s4_23 <- data_8_s4[data_8_s4$terminal_year == 2015,]
data_8_s4_23$ty_order <- 23
data_8_s4_23$if_diff_5yrs <- "N"
data_8_s4_23$ty <- 2015
data_8_s4_23$ty_minus1 <- data_8_s4_23$ty-1
data_8_s4_23$key_ty <- paste(data_8_s4_23$assessid, data_8_s4_23$ty, sep="_")
data_8_s4_23$key_ty_minus1 <- paste(data_8_s4_23$assessid, data_8_s4_23$ty_minus1, sep="_")
data_8_s4_23[,c(46:47,50:61,64:69)] <- NA

data_8_s4_24 <- data_8_s4[data_8_s4$assessid == "WGNSSK-POLLNS-VI-IIIa-1964-2016-ICESIMP2016" |  data_8_s4$terminal_year == 2016,]
data_8_s4_24$ty_order <- 24
data_8_s4_24$if_diff_5yrs <- "N"
data_8_s4_24$ty <- 2015
data_8_s4_24$ty_minus1 <- data_8_s4_24$ty-1
data_8_s4_24$key_ty <- paste(data_8_s4_24$assessid, data_8_s4_24$ty, sep="_")
data_8_s4_24$key_ty_minus1 <- paste(data_8_s4_24$assessid, data_8_s4_24$ty_minus1, sep="_")
data_8_s4_24[,c(46:47,50:61,64:69)] <- NA

data_8_s4_25 <- data_8_s4[data_8_s4$assessid == "WGNSSK-POLLNS-VI-IIIa-1964-2016-ICESIMP2016" |  data_8_s4$terminal_year == 2017,]
data_8_s4_25$ty_order <- 25
data_8_s4_25$if_diff_5yrs <- "N"
data_8_s4_25$ty <- 2015
data_8_s4_25$ty_minus1 <- data_8_s4_25$ty-1
data_8_s4_25$key_ty <- paste(data_8_s4_25$assessid, data_8_s4_25$ty, sep="_")
data_8_s4_25$key_ty_minus1 <- paste(data_8_s4_25$assessid, data_8_s4_25$ty_minus1, sep="_")
data_8_s4_25[,c(46:47,50:61,64:69)] <- NA

data_8_s4_26 <- data_8_s4[data_8_s4$assessid == "WGNSSK-POLLNS-VI-IIIa-1964-2016-ICESIMP2018" |  data_8_s4$terminal_year == 2016,]
data_8_s4_26$ty_order <- 26
data_8_s4_26$if_diff_5yrs <- "N"
data_8_s4_26$ty <- 2015
data_8_s4_26$ty_minus1 <- data_8_s4_26$ty-1
data_8_s4_26$key_ty <- paste(data_8_s4_26$assessid, data_8_s4_26$ty, sep="_")
data_8_s4_26$key_ty_minus1 <- paste(data_8_s4_26$assessid, data_8_s4_26$ty_minus1, sep="_")
data_8_s4_26[,c(46:47,50:61,64:69)] <- NA

data_8_s4_27 <- data_8_s4[data_8_s4$assessid == "WGNSSK-POLLNS-VI-IIIa-1964-2016-ICESIMP2018"  |  data_8_s4$terminal_year == 2017,]
data_8_s4_27$ty_order <- 27
data_8_s4_27$if_diff_5yrs <- "N"
data_8_s4_27$ty <- 2015
data_8_s4_27$ty_minus1 <- data_8_s4_27$ty-1
data_8_s4_27$key_ty <- paste(data_8_s4_27$assessid, data_8_s4_27$ty, sep="_")
data_8_s4_27$key_ty_minus1 <- paste(data_8_s4_27$assessid, data_8_s4_27$ty_minus1, sep="_")
data_8_s4_27[,c(46:47,50:61,64:69)] <- NA

data_8_s4_28 <- data_8_s4[data_8_s4$terminal_year == 2016 |  data_8_s4$terminal_year == 2017,]
data_8_s4_28$ty_order <- 28
data_8_s4_28$if_diff_5yrs <- "N"
data_8_s4_28$ty <- 2016
data_8_s4_28$ty_minus1 <- data_8_s4_28$ty-1
data_8_s4_28$key_ty <- paste(data_8_s4_28$assessid, data_8_s4_28$ty, sep="_")
data_8_s4_28$key_ty_minus1 <- paste(data_8_s4_28$assessid, data_8_s4_28$ty_minus1, sep="_")
data_8_s4_28[,c(46:47,50:61,64:69)] <- NA

data_8_s4_1$interval <- max(data_8_s4_1$terminal_year) - min(data_8_s4_1$terminal_year)
data_8_s4_2$interval <- max(data_8_s4_2$terminal_year) - min(data_8_s4_2$terminal_year)
data_8_s4_3$interval <- max(data_8_s4_3$terminal_year) - min(data_8_s4_3$terminal_year)
data_8_s4_4$interval <- max(data_8_s4_4$terminal_year) - min(data_8_s4_4$terminal_year)
data_8_s4_5$interval <- max(data_8_s4_5$terminal_year) - min(data_8_s4_5$terminal_year)
data_8_s4_6$interval <- max(data_8_s4_6$terminal_year) - min(data_8_s4_6$terminal_year)
data_8_s4_7$interval <- max(data_8_s4_7$terminal_year) - min(data_8_s4_7$terminal_year)
data_8_s4_8$interval <- max(data_8_s4_8$terminal_year) - min(data_8_s4_8$terminal_year)
data_8_s4_9$interval <- max(data_8_s4_9$terminal_year) - min(data_8_s4_9$terminal_year)
data_8_s4_10$interval <- max(data_8_s4_10$terminal_year) - min(data_8_s4_10$terminal_year)
data_8_s4_11$interval <- max(data_8_s4_11$terminal_year) - min(data_8_s4_11$terminal_year)
data_8_s4_12$interval <- max(data_8_s4_12$terminal_year) - min(data_8_s4_12$terminal_year)
data_8_s4_13$interval <- max(data_8_s4_13$terminal_year) - min(data_8_s4_13$terminal_year)
data_8_s4_14$interval <- max(data_8_s4_14$terminal_year) - min(data_8_s4_14$terminal_year)
data_8_s4_15$interval <- max(data_8_s4_15$terminal_year) - min(data_8_s4_15$terminal_year)
data_8_s4_16$interval <- max(data_8_s4_16$terminal_year) - min(data_8_s4_16$terminal_year)
data_8_s4_17$interval <- max(data_8_s4_17$terminal_year) - min(data_8_s4_17$terminal_year)
data_8_s4_18$interval <- max(data_8_s4_18$terminal_year) - min(data_8_s4_18$terminal_year)
data_8_s4_19$interval <- max(data_8_s4_19$terminal_year) - min(data_8_s4_19$terminal_year)
data_8_s4_20$interval <- max(data_8_s4_20$terminal_year) - min(data_8_s4_20$terminal_year)
data_8_s4_21$interval <- max(data_8_s4_21$terminal_year) - min(data_8_s4_21$terminal_year)
data_8_s4_22$interval <- max(data_8_s4_22$terminal_year) - min(data_8_s4_22$terminal_year)
data_8_s4_23$interval <- max(data_8_s4_23$terminal_year) - min(data_8_s4_23$terminal_year)
data_8_s4_24$interval <- max(data_8_s4_24$terminal_year) - min(data_8_s4_24$terminal_year)
data_8_s4_25$interval <- max(data_8_s4_25$terminal_year) - min(data_8_s4_25$terminal_year)
data_8_s4_26$interval <- max(data_8_s4_26$terminal_year) - min(data_8_s4_26$terminal_year)
data_8_s4_27$interval <- max(data_8_s4_27$terminal_year) - min(data_8_s4_27$terminal_year)
data_8_s4_28$interval <- max(data_8_s4_28$terminal_year) - min(data_8_s4_28$terminal_year)

data_8_s4 <- rbind(data_8_s4_1, data_8_s4_2, data_8_s4_3, data_8_s4_4, data_8_s4_5, data_8_s4_6, data_8_s4_7, data_8_s4_8, data_8_s4_9, data_8_s4_10,
                   data_8_s4_11, data_8_s4_12, data_8_s4_13, data_8_s4_14, data_8_s4_15, data_8_s4_16, data_8_s4_17, data_8_s4_18, data_8_s4_19,
                   data_8_s4_20, data_8_s4_21, data_8_s4_22, data_8_s4_23, data_8_s4_24, data_8_s4_25, data_8_s4_26, data_8_s4_27, data_8_s4_28)

data_8 <- rbind(data_8_s1, data_8_s2, data_8_s3, data_8_s4)

write.csv(data_8, paste(outputdir, "0e_7_data_8_assess_with_records.csv", sep="/"), row.names=F)
