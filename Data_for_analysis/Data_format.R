# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Packages
library(plyr)
library(dplyr)

##############################################################################################################

# Define directories
outputdir <- "~/Desktop/Climate_Stock/Bi_Code/B_F_OFL_dist/Data_for_analysis"
data_council <- read.csv(paste(outputdir, "0h_data_council_model.csv", sep="/"), as.is=T)
dd <- read.csv(paste(outputdir, "0a_stocks_with_2+_assesses.csv", sep="/"), as.is=T)

table(data_council$council)

data_council[data_council$region=="Mediterranean-Black Sea",]$council <- "MBS"  # General Fisheries Commission for the Mediterranean
dd[dd$region=="Mediterranean-Black Sea" & dd$mgmt=="ICCAT",]$stockid  # ALBAMED
dd[dd$region=="Mediterranean-Black Sea" & dd$mgmt=="GFCM",]$stockid 
data_council[data_council$stockid=="ALBAMED",]$council <- "AOHS"  # none
ids <- data_council[data_council$council=="MBS",]$stockid
dd[dd$stockid %in% ids,]$country
unique(dd[dd$stockid %in% ids,]$stocklong)
               
data_council[data_council$council=="ASMFC",]$council <- "US non-federal"
data_council[data_council$council=="HMS",]$council <- "AOHS"  # Sharks
data_council[data_council$council=="Atlantic Ocean",]$council <- "AOHS"  # Tunas
data_council[data_council$council=="Pacific Ocean",]$council <- "POHS"  # 
data_council[data_council$council=="Indian Ocean",]$council <- "IOHS"


# Add stocks for AOHS (https://www.iccat.int/en/assess.html)
data_council[data_council$council=="AOHS",]
data_council[data_council$stockid=="BNOSESHARATL",]$B_by_Bmsy_old <- 372000/570753
data_council[data_council$stockid=="BNOSESHARATL",]$B_by_Bmsy_recent <- 170000/153709

data_council[data_council$stockid=="SBARSHARATL",]$ty <- 2009
data_council[data_council$stockid=="SBARSHARATL",]$B_recent <- 344000
data_council[data_council$stockid=="SBARSHARATL",]$B_old <- 312890
data_council[data_council$stockid=="SBARSHARATL",]$F_recent <- 0.068
data_council[data_council$stockid=="SBARSHARATL",]$F_old <- 0.013
data_council[data_council$stockid=="SBARSHARATL",]$Bmsy_recent <- 662000
data_council[data_council$stockid=="SBARSHARATL",]$Bmsy_old <- 477590
data_council[data_council$stockid=="SBARSHARATL",]$Fmsy_recent <- 0.07
data_council[data_council$stockid=="SBARSHARATL",]$Fmsy_old <- 0.021
data_council[data_council$stockid=="SBARSHARATL",]$B_by_Bmsy_recent <- 344000/662000
data_council[data_council$stockid=="SBARSHARATL",]$B_by_Bmsy_old <- 312890/477590
data_council[data_council$stockid=="SBARSHARATL",]$F_by_Fmsy_recent <- 0.068/0.07
data_council[data_council$stockid=="SBARSHARATL",]$F_by_Fmsy_old <- 0.013/0.021
data_council[data_council$stockid=="SBARSHARATL",]$OFL_ty_recent <- 344000*(1-exp(-0.07))
data_council[data_council$stockid=="SBARSHARATL",]$OFL_ty_old <- 312890*(1-exp(-0.021))
# Changes to the biology and life history inputs were minor with respect the last assessment. 
# Changes were that: the maximum age is now 31 (from 27); steepness is now 0.30 (from 0.29); 
# the theoretical maximum length has changed a few centimeters; 
# and the natural mortality at age has been updated to new values. 
# These changes may affect the potential productivity/resiliency of the stock in different ways 
# but the overall characteristics of shark with low fecundity, long gestation period, 
# and late age at maturity have remained.

data_council[data_council$stockid=="ATBTUNAEATL" | data_council$stockid=="BIGEYEATL" | 
               data_council$stockid=="YFINATL" | data_council$stockid=="ALBANATL" |
               data_council$stockid=="ATBTUNAWATL",]

dd[dd$stockid=="ATBTUNAEATL",]  # 2012, 2015
data_council[data_council$stockid=="ATBTUNAEATL",]$Fmsy_recent <- 0.112
data_council[data_council$stockid=="ATBTUNAEATL",]$OFL_ty_recent <- 468000*(1-exp(-0.112))

dd[dd$stockid=="BIGEYEATL",]  # 2009, 2014
data_council[data_council$stockid=="BIGEYEATL",]$Fmsy_old <- 0.214

dd[dd$stockid=="YFINATL",]  # 2010, 2014
data_council[data_council$stockid=="YFINATL",]

dd[dd$stockid=="ALBANATL",]  # 2011, 2015
data_council[data_council$stockid=="ALBANATL",]

dd[dd$stockid=="ATBTUNAWATL",]  # 2012, 2013, 2015
data_council[data_council$stockid=="ATBTUNAWATL",]$Bmsy_old[1] <- 93621
data_council[data_council$stockid=="ATBTUNAWATL",]$Bmsy_old[3] <- 93621
data_council[data_council$stockid=="ATBTUNAWATL",]$Fmsy_old[1] <- 0.06
data_council[data_council$stockid=="ATBTUNAWATL",]$Fmsy_old[3] <- 0.06
data_council[data_council$stockid=="ATBTUNAWATL",]$B_by_Bmsy_old[1] <- 28300/93621 
data_council[data_council$stockid=="ATBTUNAWATL",]$B_by_Bmsy_old[3] <- 28300/93621 
data_council[data_council$stockid=="ATBTUNAWATL",]$F_by_Fmsy_old[1] <-  0.077/0.06
data_council[data_council$stockid=="ATBTUNAWATL",]$F_by_Fmsy_old[3] <-  0.077/0.06
data_council[data_council$stockid=="ATBTUNAWATL",]$OFL_ty_old[1] <- 28300*(1-exp(-0.06))
data_council[data_council$stockid=="ATBTUNAWATL",]$OFL_ty_old[3] <- 28300*(1-exp(-0.06))


# Add stocks for POHS 
# (Western Pacific: https://www.wcpfc.int/current-stock-status-and-advice; only latest!!!)
# (Eastern Pacific: https://www.iattc.org/StockAssessmentReportsENG.htm; ONGOING!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
data_council[data_council$council=="POHS",]  
# Albacore tuna North Pacific Ocean, 2012, 2015
# Pacific bluefin tuna Pacific Ocean, 2012, 2014
# Striped marlin Western and Central North Pacific, 2010, 2013 (M=0.38 for age-4+)
# Bigeye tuna Central Western Pacific Ocean, 2006, 2012, 2015
dd[dd$stockid=="ALBANPAC",]
data_council[data_council$stockid=="ALBANPAC",]  

dd[dd$stockid=="PACBTUNA",]
data_council[data_council$stockid=="PACBTUNA",]  

dd[dd$stockid=="STMARLINWCNPAC",]
data_council[data_council$stockid=="STMARLINWCNPAC",]$Fmsy_recent <- 0.63
data_council[data_council$stockid=="STMARLINWCNPAC",]$F_by_Fmsy_recent <- 1.10
data_council[data_council$stockid=="STMARLINWCNPAC",]$B_by_Bmsy_recent <- 0.54
data_council[data_council$stockid=="STMARLINWCNPAC",]$OFL_ty_recent <- 6410*(1-exp(-0.6300))

dd[dd$stockid=="BIGEYECWPAC",]
data_council[data_council$stockid=="BIGEYECWPAC",]


# Add stocks for IOHS (https://www.ccsbt.org; No finding on Bmsy nor Fmsy for 2016 assessment)
dd[dd$stockid=="SBT",]
# Southern bluefin tuna Southern Oceans, 2013, 2016. 
# https://www.ccsbt.org/sites/default/files/userfiles/file/docs_english/meetings/meeting_reports/ccsbt_21/report_of_SC19.pdf
# https://www.ccsbt.org/sites/default/files/userfiles/file/docs_english/meetings/meeting_reports/ccsbt_23/report_of_SC21.pdf


# Add info for stocks in MBS (General Fisheries Commission for the Mediterranean website; ONGOING!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
data_council[data_council$council == "MBS",]$stockid
  
table(data_council$council)
length(unique((data_council$council)))  # 20 councils

data_council$council <- factor(data_council$council, 
                               levels = c("GMFMC", "MAFMC", "NEFMC", "NPFMC", "PFMC", "SAFMC", "US non-federal",
                                          "Europe (EU)", "Europe (non-EU)",  "Canada East Coast", "Canada West Coast", 
                                          "Australia", "New Zealand", "Japan", "South Africa", "South America",
                                          "AOHS", "IOHS", "POHS", "MBS"))

data_council$CouncilCode <- as.numeric(data_council$council)
table(data_council$CouncilCode)
table(data_council$council)

write.csv(data_council, paste(outputdir, "new_data_council_model.csv", sep="/"), row.names=F)

##############################################################################################################

## Atlantic mackerel - G4

load("/Users/rujiabi/Desktop/Climate_Stock/Bi_Code/B_F_OFL_dist/Data_for_analysis/Historical.Comparison.For.Olaf.RDATA")

# 2021 MT assessment (1968-2019)
current.ests$SSB
current.ests$F

# 2017 Benchmark (1968-2016)
prev.ests$Bench.2017

# 2005 SAW42 (1968-2004; M=0.2)
hist.ests$SAW42

##############################################################################################################

dd <- read.csv(paste(outputdir, "new_data_council_model.csv", sep="/"), as.is=T)

dd[dd$stockid=="GRAMBERGM",]$Fmsy_recent <- 0.22
dd[dd$stockid=="GRAMBERGM",]$B_by_Bmsy_recent <- dd[dd$stockid=="GRAMBERGM",]$B_recent/dd[dd$stockid=="GRAMBERGM",]$Bmsy_recent
dd[dd$stockid=="GRAMBERGM",]$B_by_Bmsy_old <- dd[dd$stockid=="GRAMBERGM",]$B_old/dd[dd$stockid=="GRAMBERGM",]$Bmsy_old
dd[dd$stockid=="GRAMBERGM",]$F_by_Fmsy_recent <- dd[dd$stockid=="GRAMBERGM",]$F_recent/dd[dd$stockid=="GRAMBERGM",]$Fmsy_recent
dd[dd$stockid=="GRAMBERGM",]$F_by_Fmsy_old <- dd[dd$stockid=="GRAMBERGM",]$F_old/dd[dd$stockid=="GRAMBERGM",]$Fmsy_old
dd[dd$stockid=="GRAMBERGM",]$OFL_ty_recent <- dd[dd$stockid=="GRAMBERGM",]$B_recent*dd[dd$stockid=="GRAMBERGM",]$Fmsy_recent
dd[dd$stockid=="GRAMBERGM",]$OFL_ty_old <- dd[dd$stockid=="GRAMBERGM",]$B_old*dd[dd$stockid=="GRAMBERGM",]$Fmsy_old

write.csv(dd, paste(outputdir, "new_data_council_model.csv", sep="/"), row.names=F)

