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

# Read RAMLDB keys
stock_key <- stock # 1374 stocks, including 1330 Current and 44 Deprecated
assessment_key <- assessment  # 2216 assessments, including 1330 most current (999), 883 out-of-date (0), and 3 not being used (-1)
method_key <- as.data.frame(assessmethod)  # 76 methods
method_key[method_key$methodshort=="Unknown",]
method_key <- method_key[-20,]  # remove the row with "Unknown biomass dynamics model"
assessor_key <- assessor  # 107 assessors
area_key <- area  # 838 areas
taxa_key_ram <- taxonomy  # 369 taxa

# Natural mortality
natural_mortality <- bioparams[bioparams$bioid=="M-1/yr",]  # Natural mortality, 417 rows, 347 stocks with 413 assessments, so need to remove duplicate rows
rownames(natural_mortality) <- NULL
natural_mortality[natural_mortality$assessid=="NAFO-SC-AMPL3LNO-1955-2007-BAUM" & natural_mortality$bioyear=="1997-2007",]$biovalue <- "0.2 in 1955-1988, 0.53 in 1989-1996, 0.2 in 1997-2007"
natural_mortality[natural_mortality$assessid=="NAFO-SC-AMPL3LNO-1955-2007-BAUM" & natural_mortality$bioyear=="1997-2007",]$bioyear <- NULL
natural_mortality <- natural_mortality[-c(43,64,228,229),]
natural_mortality[natural_mortality$biovalue ==" 0.174052 0.18232 ",]$biovalue <- "0.174052 and 0.18232"
natural_mortality[natural_mortality$biovalue ==" 0.6 0 ",]$biovalue <- "0.6 and 0"
natural_mortality[natural_mortality$biovalue =="4.00E-01",]$biovalue <- "0.4"

# Natural mortality notes
natural_mortality_notes <- bioparams[bioparams$bioid=="M-source-note",]   # 25 rows 

# Read values
ts <- timeseries  # 1048437 rows; 1373 stocks, 2215 assessments
metric <- tsmetrics
table(metric$tscategory)
values <- ts %>% 
  left_join(metric, by=c("tsid"="tsunique"))

# Check biomass availability (1837 assessments)
values_b <- values[values$tscategory=="TOTAL BIOMASS" | values$tscategory=="SPAWNING STOCK BIOMASS or CPUE",] 
assess_with_biomass <- data.frame(matrix(NA, nrow=length(unique(values_b$assessid)), ncol=3))
colnames(assess_with_biomass) <- c("assessid", "B_start", "B_end")
assess_with_biomass$assessid <- unique(values_b$assessid)
for (i in 1:length(unique(values_b$assessid))){
  assess_with_biomass[i,2] <- min(values_b[values_b$assessid==assess_with_biomass[i,1] & !is.na(values_b$tsvalue),]$tsyear)
  assess_with_biomass[i,3] <- max(values_b[values_b$assessid==assess_with_biomass[i,1] & !is.na(values_b$tsvalue),]$tsyear)
}

# Check fishing mortality availability (1597 assessments)
values_fr <- values[values$tscategory=="FISHING MORTALITY",] 
assess_with_fr <- data.frame(matrix(NA, nrow=length(unique(values_fr$assessid)), ncol=3))
colnames(assess_with_fr) <- c("assessid", "F_start", "F_end")
assess_with_fr$assessid <- unique(values_fr$assessid)
for (i in 1:length(unique(values_fr$assessid))){
  assess_with_fr[i,2] <- min(values_fr[values_fr$assessid==assess_with_fr[i,1] & !is.na(values_fr$tsvalue),]$tsyear)
  assess_with_fr[i,3] <- max(values_fr[values_fr$assessid==assess_with_fr[i,1] & !is.na(values_fr$tsvalue),]$tsyear)
}

# Check catch availability (2088 assessments)
values_c <- values[values$tscategory=="CATCH or LANDINGS",] 
assess_with_c <- data.frame(matrix(NA, nrow=length(unique(values_c$assessid)), ncol=3))
colnames(assess_with_c) <- c("assessid", "C_start", "C_end")
assess_with_c$assessid <- unique(values_c$assessid)
for (i in 1:length(unique(values_c$assessid))){
  assess_with_c[i,2] <- min(values_c[values_c$assessid==assess_with_c[i,1] & !is.na(values_c$tsvalue),]$tsyear)
  assess_with_c[i,3] <- max(values_c[values_c$assessid==assess_with_c[i,1] & !is.na(values_c$tsvalue),]$tsyear)
}

#####################################################################################################################################

# Build merged dataset
fulldata <- assessment_key %>% 
  # Format assessment info
  select(assessid, stockid, assessorid, stocklong, assessyear, assesssource, assessmethod, mostrecent, notes) %>% 
  rename(assessor=assessorid, years=assessyear, assesssource=assesssource, methodshort=assessmethod, assess_if_mostrecent=mostrecent, assess_notes=notes) %>% 
  mutate(start_year=as.numeric(substr(years, 1, 4)), report_year=as.numeric(substr(years, 6, 9))) %>% 
  
  # Add more assessment method info 
  left_join(method_key, by=c("methodshort")) %>%
  rename(method=category) %>% 
 # mutate(methodshort=revalue(methodshort, c("Unknown"="unknown"))) %>% 
 
   # Add accessor info: country and management agency
  left_join(select(assessor_key, -assessorfull), by=c("assessor"="assessorid")) %>% 
  
  # Add stock info: region, area id, scientific name
  left_join(select(stock_key, -c(stocklong, tsn, inmyersdb, myersstockid, commonname)), by="stockid") %>% 
  rename(species_orig=scientificname, state_of_stock=state) %>%
  mutate(species_orig=trimws(species_orig),
         region=revalue(region, c("Europe non EU"="Europe (non-EU)",
                                  "European Union"="Europe (EU)"))) %>% 
  
  # Add area info: area name
  left_join(select(area_key, -c(country, alternateareaname, areatype, areacode)), by="areaid") %>%
  rename(area=areaname) %>% 
  
  # Add common name
  left_join(select(taxa_key_ram, scientificname, commonname1, classname, ordername, family, genus), by=c("species_orig"="scientificname")) %>% 
  rename(comm_name=commonname1, class=classname, order=ordername) %>%       
  mutate(species=trimws(species_orig),
         species=revalue(species, c(#"Cervimunida Johni" = "Cervimunida johni",
                                    "Chrysophrys auratus" = "Pagrus auratus",
                                    "Clupea bentincki" = "Strangomera bentincki",
                                    "Clupea pallasii" = "Clupea pallasii pallasii",
                                    "Epinephelus niveatus" = "Hyporthodus niveatus",
                                    "Epinephelus flavolimbatus" = "Hyporthodus flavolimbatus",
                                    "Etrumeus teres" = "Etrumeus sadina",
                                    "Loligo pealeii" = "Doryteuthis pealeii",
                                    "Loligo reynaudii" = "Loligo vulgaris reynaudii",
                                    "Merluccius gayi" = "Merluccius gayi gayi",
                                    "Mullus barbatus" = "Mullus barbatus barbatus",
                                    "Neoplatycephalus richardsoni" = "Platycephalus richardsoni",
                                    "Psetta maxima" = "Scophthalmus maximus",
                                    #"Reinhardtius stomias" = "Atheresthes stomias",
                                    "Sardinops melanostictus" = "Sardinops sagax",
                                    #"Scomber australacius"="Scomber australasicus", 
                                    #"Solea vulgaris" = "Solea solea",
                                    #"Sprattus fuengensis" = "Sprattus fuegensis",
                                    "Tetrapturus albidus" = "Kajikia albida"))) %>% 
  
  # Add natural mortality
  left_join(select(natural_mortality, -c(stockid, stocklong, bioid)), by="assessid") %>% 
  rename(M=biovalue, M_year=bioyear, M_notes=bionotes) %>%
  
  # Add natural mortality source notes
  left_join(select(natural_mortality_notes, -c(stockid, stocklong, bioid, bioyear)), by="assessid") %>% 
  rename(M_source=biovalue, M_source_notes=bionotes) %>%
  
  # Add biomass period
  left_join(assess_with_biomass, by=c("assessid")) %>%
  
  # Add fishing mortality period
  left_join(assess_with_fr, by=c("assessid")) %>%
  
  # Add catch period
  left_join(assess_with_c, by=c("assessid")) %>%
  
  # Reorder columns
  select(assessid, stockid, mgmt, assessor, years, start_year, report_year, method, methodshort, methodlong, country, 
         region, area, areaid, stocklong, class, order, family, genus, species, species_orig, comm_name, assess_if_mostrecent, 
         assesssource, state_of_stock, primary_country, primary_FAOarea, assess_notes, M, M_year, M_notes, M_source, M_source_notes,
         B_start, B_end, F_start, F_end, C_start, C_end)

# Fix problem species
prob_spp <- sort(unique(fulldata$species_orig[is.na(fulldata$class)]))  # no

# Inspect completness
apply(fulldata, 2, function(x) sum(is.na(x)))  
# 1803 missing for natural mortality, 379 missing for B, 619 missing for F, 128 missing for C

##########################################################################################################################################################################

# Data summary
length(unique(fulldata$stockid))  # 1374 stocks
length(unique(fulldata$assessid))  # 2216 assessments

currentdata <- fulldata[fulldata$state_of_stock == "Current",]
length(unique(currentdata$stockid))  # 1330 stocks
length(unique(currentdata$assessid))  # 2163 assessments

length(unique(currentdata[currentdata$country=="USA",]$stockid))  # 412
length(unique(currentdata[currentdata$country=="Canada",]$stockid))  # 250
length(unique(currentdata[currentdata$region=="Europe (EU)",]$stockid))  # 208
length(unique(currentdata[currentdata$country=="Japan",]$stockid))  # 88

table(currentdata$region)

data <- currentdata

################################################################################################################################

num_assess <- as.data.frame(aggregate(assessid ~ stockid, data = data, FUN = length))

################################################################################################################################

# Find stockid with only one assessment
stockid_only_one <- num_assess[num_assess$assessid == 1,]$stockid  
length(stockid_only_one)  # 882 stocks with only one assessment
data_only_one <- filter(data, stockid %in% stockid_only_one)  

table(data_only_one$country)  # 325 US stocks
table(data_only_one$region)
table(data_only_one$area)

us_only_one <- data_only_one[data_only_one$country=="USA",]
list_us_only_one <- us_only_one[,c(2,15,12,5)]
colnames(list_us_only_one) <- c("stockid", "stocklong", "region","assessment years")

write.csv(list_us_only_one, paste(outputdir, "0a_list_us_only_one.csv", sep="/"), row.names=F)

################################################################################################################################

# Find stockid with at-least two assessments 
stockid_kept <- num_assess[num_assess$assessid > 1,]$stockid  
length(stockid_kept)  # 448 stocks with at-least two assessments

# Pick stocks with at-least two assessments
data_two_plus <- filter(data, stockid %in% stockid_kept)  
length(unique(data_two_plus$stockid))  # 448 stocks 
length(unique(data_two_plus$assessid))  # 1281 assessments
table(data_two_plus$assess_if_mostrecent) # 448 assessments as the most recent

# Add assessment order from most recent to oldest
data_two_plus$terminal_year <- NA
for (i in 1:nrow(data_two_plus)){
  data_two_plus$terminal_year[i] <- min(data_two_plus$B_end[i], data_two_plus$F_end[i], data_two_plus$C_end[i], na.rm=T)
}

# Set order
data_two_plus$assess_order <- 0  # the most recent = 1, earlier = 2,3......

data_order <- data_two_plus[1,]
for (i in 1:length(stockid_kept)){
  data_stock <- data_two_plus[data_two_plus$stockid==stockid_kept[i],]
  data_stock$assess_order <- rank(-data_stock$terminal_year)
  data_order <- rbind(data_order, data_stock)
}
data_order <- data_order[-1,]
rownames(data_order) <- NULL

################################################################################################################################

# Check weird stocks
table(data_order$assess_order) 
data_order_num_assess <- as.data.frame(aggregate(assessid ~ stockid, data = data_order, FUN = length))
table(data_order_num_assess$assessid)  # 320 stocks with 2 assessments, 46 stocks with 3 assessments, 10 stocks with 4 assessments, 
                                       # 14 stocks with 5 assessments, 18 stocks with 6 assessments, 35 stocks with 7 assessments, 
                                       # 5 stocks with 8 assessments

################################################################################################################################

# Stocks with 2 assessments
stockid_2assess <- data_order_num_assess[data_order_num_assess$assessid==2,]$stockid
data_2assess <- filter(data_order, stockid %in% stockid_2assess)  
list_data_2assess <- data_2assess[,c(2,15,12,5)]
colnames(list_data_2assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_2assess <- data.frame(matrix(NA, nrow=length(unique(list_data_2assess$stockid)), ncol=4))
colnames(stock_2assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_2assess$stockid <- unique(list_data_2assess$stockid)
for (i in 1:length(unique(list_data_2assess$stockid))){
  stock_2assess[i,2] <- list_data_2assess[list_data_2assess$stockid==stock_2assess[i,1],]$stocklong[1]
  stock_2assess[i,3] <- list_data_2assess[list_data_2assess$stockid==stock_2assess[i,1],]$region[1]
  stock_2assess[i,4] <- paste(list_data_2assess[list_data_2assess$stockid==stock_2assess[i,1],]$assessment_years[1], 
                              list_data_2assess[list_data_2assess$stockid==stock_2assess[i,1],]$assessment_years[2], sep=",")
}
write.csv(stock_2assess, paste(outputdir, "0a_list_stocks_with_2_assessments.csv", sep="/"), row.names=F)

# Stocks with 3 assessments
stockid_3assess <- data_order_num_assess[data_order_num_assess$assessid==3,]$stockid
data_3assess <- filter(data_order, stockid %in% stockid_3assess)  
list_data_3assess <- data_3assess[,c(2,15,12,5)]
colnames(list_data_3assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_3assess <- data.frame(matrix(NA, nrow=length(unique(list_data_3assess$stockid)), ncol=4))
colnames(stock_3assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_3assess$stockid <- unique(list_data_3assess$stockid)
for (i in 1:length(unique(list_data_3assess$stockid))){
  stock_3assess[i,2] <- list_data_3assess[list_data_3assess$stockid==stock_3assess[i,1],]$stocklong[1]
  stock_3assess[i,3] <- list_data_3assess[list_data_3assess$stockid==stock_3assess[i,1],]$region[1]
  stock_3assess[i,4] <- paste(list_data_3assess[list_data_3assess$stockid==stock_3assess[i,1],]$assessment_years[1], 
                              list_data_3assess[list_data_3assess$stockid==stock_3assess[i,1],]$assessment_years[2], 
                              list_data_3assess[list_data_3assess$stockid==stock_3assess[i,1],]$assessment_years[3], sep=",")
}
write.csv(stock_3assess, paste(outputdir, "0a_list_stocks_with_3_assessments.csv", sep="/"), row.names=F)

# Stocks with 4 assessments
stockid_4assess <- data_order_num_assess[data_order_num_assess$assessid==4,]$stockid
data_4assess <- filter(data_order, stockid %in% stockid_4assess)  
list_data_4assess <- data_4assess[,c(2,15,12,5)]
colnames(list_data_4assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_4assess <- data.frame(matrix(NA, nrow=length(unique(list_data_4assess$stockid)), ncol=4))
colnames(stock_4assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_4assess$stockid <- unique(list_data_4assess$stockid)
for (i in 1:length(unique(list_data_4assess$stockid))){
  stock_4assess[i,2] <- list_data_4assess[list_data_4assess$stockid==stock_4assess[i,1],]$stocklong[1]
  stock_4assess[i,3] <- list_data_4assess[list_data_4assess$stockid==stock_4assess[i,1],]$region[1]
  stock_4assess[i,4] <- paste(list_data_4assess[list_data_4assess$stockid==stock_4assess[i,1],]$assessment_years[1], 
                              list_data_4assess[list_data_4assess$stockid==stock_4assess[i,1],]$assessment_years[2], 
                              list_data_4assess[list_data_4assess$stockid==stock_4assess[i,1],]$assessment_years[3], 
                              list_data_4assess[list_data_4assess$stockid==stock_4assess[i,1],]$assessment_years[4], sep=",")
}
write.csv(stock_4assess, paste(outputdir, "0a_list_stocks_with_4_assessments.csv", sep="/"), row.names=F)

# Stocks with 5 assessments
stockid_5assess <- data_order_num_assess[data_order_num_assess$assessid==5,]$stockid
data_5assess <- filter(data_order, stockid %in% stockid_5assess)  
list_data_5assess <- data_5assess[,c(2,15,12,5)]
colnames(list_data_5assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_5assess <- data.frame(matrix(NA, nrow=length(unique(list_data_5assess$stockid)), ncol=4))
colnames(stock_5assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_5assess$stockid <- unique(list_data_5assess$stockid)
for (i in 1:length(unique(list_data_5assess$stockid))){
  stock_5assess[i,2] <- list_data_5assess[list_data_5assess$stockid==stock_5assess[i,1],]$stocklong[1]
  stock_5assess[i,3] <- list_data_5assess[list_data_5assess$stockid==stock_5assess[i,1],]$region[1]
  stock_5assess[i,4] <- paste(list_data_5assess[list_data_5assess$stockid==stock_5assess[i,1],]$assessment_years[1], 
                              list_data_5assess[list_data_5assess$stockid==stock_5assess[i,1],]$assessment_years[2], 
                              list_data_5assess[list_data_5assess$stockid==stock_5assess[i,1],]$assessment_years[3], 
                              list_data_5assess[list_data_5assess$stockid==stock_5assess[i,1],]$assessment_years[4], 
                              list_data_5assess[list_data_5assess$stockid==stock_5assess[i,1],]$assessment_years[5], sep=",")
}
write.csv(stock_5assess, paste(outputdir, "0a_list_stocks_with_5_assessments.csv", sep="/"), row.names=F)

# Stocks with 6 assessments
stockid_6assess <- data_order_num_assess[data_order_num_assess$assessid==6,]$stockid
data_6assess <- filter(data_order, stockid %in% stockid_6assess)  
list_data_6assess <- data_6assess[,c(2,15,12,5)]
colnames(list_data_6assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_6assess <- data.frame(matrix(NA, nrow=length(unique(list_data_6assess$stockid)), ncol=4))
colnames(stock_6assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_6assess$stockid <- unique(list_data_6assess$stockid)
for (i in 1:length(unique(list_data_6assess$stockid))){
  stock_6assess[i,2] <- list_data_6assess[list_data_6assess$stockid==stock_6assess[i,1],]$stocklong[1]
  stock_6assess[i,3] <- list_data_6assess[list_data_6assess$stockid==stock_6assess[i,1],]$region[1]
  stock_6assess[i,4] <- paste(list_data_6assess[list_data_6assess$stockid==stock_6assess[i,1],]$assessment_years[1], 
                              list_data_6assess[list_data_6assess$stockid==stock_6assess[i,1],]$assessment_years[2], 
                              list_data_6assess[list_data_6assess$stockid==stock_6assess[i,1],]$assessment_years[3], 
                              list_data_6assess[list_data_6assess$stockid==stock_6assess[i,1],]$assessment_years[4], 
                              list_data_6assess[list_data_6assess$stockid==stock_6assess[i,1],]$assessment_years[5], 
                              list_data_6assess[list_data_6assess$stockid==stock_6assess[i,1],]$assessment_years[6], sep=",")
}
write.csv(stock_6assess, paste(outputdir, "0a_list_stocks_with_6_assessments.csv", sep="/"), row.names=F)

# Stocks with 7 assessments
stockid_7assess <- data_order_num_assess[data_order_num_assess$assessid==7,]$stockid
data_7assess <- filter(data_order, stockid %in% stockid_7assess)  
list_data_7assess <- data_7assess[,c(2,15,12,5)]
colnames(list_data_7assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_7assess <- data.frame(matrix(NA, nrow=length(unique(list_data_7assess$stockid)), ncol=4))
colnames(stock_7assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_7assess$stockid <- unique(list_data_7assess$stockid)
for (i in 1:length(unique(list_data_7assess$stockid))){
  stock_7assess[i,2] <- list_data_7assess[list_data_7assess$stockid==stock_7assess[i,1],]$stocklong[1]
  stock_7assess[i,3] <- list_data_7assess[list_data_7assess$stockid==stock_7assess[i,1],]$region[1]
  stock_7assess[i,4] <- paste(list_data_7assess[list_data_7assess$stockid==stock_7assess[i,1],]$assessment_years[1], 
                              list_data_7assess[list_data_7assess$stockid==stock_7assess[i,1],]$assessment_years[2], 
                              list_data_7assess[list_data_7assess$stockid==stock_7assess[i,1],]$assessment_years[3], 
                              list_data_7assess[list_data_7assess$stockid==stock_7assess[i,1],]$assessment_years[4], 
                              list_data_7assess[list_data_7assess$stockid==stock_7assess[i,1],]$assessment_years[5], 
                              list_data_7assess[list_data_7assess$stockid==stock_7assess[i,1],]$assessment_years[6], 
                              list_data_7assess[list_data_7assess$stockid==stock_7assess[i,1],]$assessment_years[7], sep=",")
}
write.csv(stock_7assess, paste(outputdir, "0a_list_stocks_with_7_assessments.csv", sep="/"), row.names=F)

# Stocks with 8 assessments
stockid_8assess <- data_order_num_assess[data_order_num_assess$assessid==8,]$stockid
data_8assess <- filter(data_order, stockid %in% stockid_8assess)  
list_data_8assess <- data_8assess[,c(2,15,12,5)]
colnames(list_data_8assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_8assess <- data.frame(matrix(NA, nrow=length(unique(list_data_8assess$stockid)), ncol=4))
colnames(stock_8assess) <- c("stockid", "stocklong", "region","assessment_years")
stock_8assess$stockid <- unique(list_data_8assess$stockid)
for (i in 1:length(unique(list_data_8assess$stockid))){
  stock_8assess[i,2] <- list_data_8assess[list_data_8assess$stockid==stock_8assess[i,1],]$stocklong[1]
  stock_8assess[i,3] <- list_data_8assess[list_data_8assess$stockid==stock_8assess[i,1],]$region[1]
  stock_8assess[i,4] <- paste(list_data_8assess[list_data_8assess$stockid==stock_8assess[i,1],]$assessment_years[1], 
                              list_data_8assess[list_data_8assess$stockid==stock_8assess[i,1],]$assessment_years[2], 
                              list_data_8assess[list_data_8assess$stockid==stock_8assess[i,1],]$assessment_years[3], 
                              list_data_8assess[list_data_8assess$stockid==stock_8assess[i,1],]$assessment_years[4], 
                              list_data_8assess[list_data_8assess$stockid==stock_8assess[i,1],]$assessment_years[5], 
                              list_data_8assess[list_data_8assess$stockid==stock_8assess[i,1],]$assessment_years[6], 
                              list_data_8assess[list_data_8assess$stockid==stock_8assess[i,1],]$assessment_years[7], 
                              list_data_8assess[list_data_8assess$stockid==stock_8assess[i,1],]$assessment_years[8], sep=",")
}
write.csv(stock_8assess, paste(outputdir, "0a_list_stocks_with_8_assessments.csv", sep="/"), row.names=F)

################################################################################################################################

# Inspect completness
apply(data_order, 2, function(x) sum(is.na(x)))  
# 1026 missing for natural mortality, 231 missing for B, 240 missing for F, 18 missing for C

# Add terminal year of the oldest assessment for each comparison
data_order$ty <- NA
ids <- unique(data_order$stockid)
for (i in 1:length(ids)){
  data_order[data_order$stockid==ids[i],]$ty <- min(data_order[data_order$stockid==ids[i],]$terminal_year, na.rm=T)
}
data_order$ty_minus1 <- data_order$ty-1

# Export data with stocks having two assessments
write.csv(data_order, paste(outputdir, "0a_stocks_with_2+_assesses.csv", sep="/"), row.names=F)




