# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Packages
library(plyr)
library(dplyr)

load("/Users/rujiabi/Desktop/Climate_Stock/RAMLDB v4.491/DB Files With Assessment Data/R Data/DBdata[asmt][v4.491].RData")
outputdir <- "~/Desktop/Climate_Stock/Bi_Code/B_F_OFL_dist/Data_for_analysis"
dd <- read.csv("new_data_council_model.csv", as.is=T)
dat <- dd[,c(1,3,26)]
dim(dat)  # 1315 rows
dat <- dat[-which(duplicated(dat)),]
dim(dat)  # 277 stocks
 

## Add shapefiles
# Directories
keydir <- "~/Desktop/Climate_Stock/Bi_Code/SST/stock_boundaries"
shpdir <- "~/Desktop/Climate_Stock/Bi_Code/SST/stock_boundaries/ramldb_boundaries"
outputdir <- "~/Desktop/Climate_Stock/Bi_Code/B_F_OFL_dist/Data_preparation/output"

# Add country and area
dd0 <- read.csv(paste(outputdir, "0a_stocks_with_2+_assesses.csv", sep="/"), as.is=T)
dd1 <- dat %>%
  left_join(select(dd0, stockid, country, area, primary_country), by=c("stockid"))
dd1 <- dd1[-which(duplicated(dd1)),]

# Read key 
key <- read.csv(paste(keydir, "ramldb_stock_boundary_formatted.csv", sep="/"), as.is=T)
length(unique(key$stockid))  # 685 stocks
key <- subset(key, shp_source!="")

# Add shapefiles
fulldata <- dd1 %>% 
  left_join(select(key, stockid, shp_source, shp_path, zone_col, zones), by=c("stockid"))

table(fulldata$region)
table(fulldata$country)
table(fulldata$primary_country)

fulldata[is.na(fulldata$primary_country),]$country <- "USA"
fulldata[is.na(fulldata$primary_country),]$primary_country <- "USA"
