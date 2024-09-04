stock <- read.csv("277stocks_countries.csv")
dim(stock)  # 277

lme <- read.csv("stockid_lme.csv")
dim(lme)
library(dplyr)
lme_clean <- distinct(lme[,c(1,2)], stockid, .keep_all = TRUE)
dim(lme_clean)
dd <- merge(stock, lme_clean, by="stockid", all.x=TRUE)
dim(dd)
summary(dd)

check <- dd[is.na(dd$lme_number),]
areaid <- unique(check$area)

# ICES stock location: https://standardgraphs.ices.dk/stockList.aspx
dd[!is.na(dd$area) & dd$area==areaid[1],]$lme_number <- 62
dd[!is.na(dd$area) & dd$area==areaid[2],]$lme_number <- 62
dd[is.na(dd$area) & dd$stockid=="atmackerel",]$area <- "Gulf of Maine / Georges Bank"
dd[dd$stockid=="atmackerel",]$lme_number <- 7
dd[is.na(dd$area),]$lme_number <- 6
dd[!is.na(dd$area) & dd$area==areaid[4],]$lme_number <- 42
dd[!is.na(dd$area) & dd$area==areaid[5],]$lme_number <- -99
dd[!is.na(dd$area) & dd$area==areaid[6],]$lme_number <- 24
dd[!is.na(dd$area) & dd$area==areaid[7],]$lme_number <- 5
dd[!is.na(dd$area) & dd$area==areaid[8],]$lme_number <- NA
dd[!is.na(dd$area) & dd$area==areaid[9],]$lme_number <- 22
dd[!is.na(dd$area) & dd$area==areaid[10],]$lme_number <- 21
dd[!is.na(dd$area) & dd$area==areaid[11],]$lme_number <- 24
dd[!is.na(dd$area) & dd$area==areaid[12],]$lme_number <- 59
dd[!is.na(dd$area) & dd$area==areaid[13],]$lme_number <- 24
dd[!is.na(dd$area) & dd$area==areaid[14],]$lme_number <- 59
dd[!is.na(dd$area) & dd$area==areaid[15],]$lme_number <- 22
dd[!is.na(dd$area) & dd$area==areaid[16],]$lme_number <- 23
dd[!is.na(dd$area) & dd$area==areaid[17],]$lme_number <- 23
dd[!is.na(dd$area) & dd$area==areaid[18],]$lme_number <- 24
dd[!is.na(dd$area) & dd$area==areaid[19],]$lme_number <- 22
dd[!is.na(dd$area) & dd$area==areaid[20],]$lme_number <- 24
dd[!is.na(dd$area) & dd$area==areaid[21],]$lme_number <- 24
dd[!is.na(dd$area) & dd$area==areaid[22],]$lme_number <- 25
dd[!is.na(dd$area) & dd$area==areaid[23],]$lme_number <- 50
dd[!is.na(dd$area) & dd$area==areaid[24],]$lme_number <- 59
dd[!is.na(dd$area) & dd$area==areaid[25],]$lme_number <- 9
dd[!is.na(dd$area) & dd$area==areaid[26],]$lme_number <- 24
dd[!is.na(dd$area) & dd$area==areaid[27],]$lme_number <- 42
dd[!is.na(dd$area) & dd$area==areaid[28],]$lme_number <- 43
dd[!is.na(dd$area) & dd$area==areaid[29],]$lme_number <- 26
dd[!is.na(dd$area) & dd$area==areaid[30],]$lme_number <- 26
dd[!is.na(dd$area) & dd$area==areaid[31],]$lme_number <- 22
dd[!is.na(dd$area) & dd$area==areaid[32],]$lme_number <- 47
dd[!is.na(dd$area) & dd$area==areaid[33],]$lme_number <- 50
dd[!is.na(dd$area) & dd$area==areaid[34] & dd$council=="Japan",]$lme_number <- NA
dd[!is.na(dd$area) & dd$area==areaid[34] & dd$council=="POHS",]$lme_number <- -99
dd[!is.na(dd$area) & dd$area==areaid[35],]$lme_number <- 50
dd[!is.na(dd$area) & dd$area==areaid[36],]$lme_number <- 13
dd[!is.na(dd$area) & dd$area==areaid[37],]$lme_number <- 46
dd[!is.na(dd$area) & dd$area==areaid[38],]$lme_number <- 46
dd[!is.na(dd$area) & dd$area==areaid[39],]$lme_number <- 23
dd[!is.na(dd$area) & dd$area==areaid[40],]$lme_number <- 25
dd[!is.na(dd$area) & dd$area==areaid[41],]$lme_number <- 3
dd[!is.na(dd$area) & dd$area==areaid[42],]$lme_number <- 13
dd[!is.na(dd$area) & dd$area==areaid[43],]$lme_number <- 50
dd[!is.na(dd$area) & dd$area==areaid[44],]$lme_number <- 50
dd[!is.na(dd$area) & dd$area==areaid[45],]$lme_number <- 40
dd[!is.na(dd$area) & dd$area==areaid[46],]$lme_number <- 26
dd[!is.na(dd$area) & dd$area==areaid[47],]$lme_number <- NA
dd[!is.na(dd$area) & dd$area==areaid[48],]$lme_number <- 46
dd[!is.na(dd$area) & dd$area==areaid[49],]$lme_number <- 46
dd[!is.na(dd$area) & dd$area==areaid[50],]$lme_number <- 22
dd[!is.na(dd$area) & dd$area==areaid[51],]$lme_number <- 22
dd[!is.na(dd$area) & dd$area==areaid[52],]$lme_number <- 22
dd[!is.na(dd$area) & dd$area==areaid[53],]$lme_number <- 22
dd[!is.na(dd$area) & dd$area==areaid[54],]$lme_number <- 22
dd[!is.na(dd$area) & dd$area==areaid[55],]$lme_number <- 25
dd[!is.na(dd$area) & dd$area==areaid[56],]$lme_number <- 26
dd[!is.na(dd$area) & dd$area==areaid[57],]$lme_number <- 62
dd[!is.na(dd$area) & dd$area==areaid[58],]$lme_number <- 24
dd[!is.na(dd$area) & dd$area==areaid[59],]$lme_number <- -99
dd[!is.na(dd$area) & dd$area==areaid[60],]$lme_number <- 25
dd[!is.na(dd$area) & dd$area==areaid[61],]$lme_number <- 22
dd[!is.na(dd$area) & dd$area==areaid[62],]$lme_number <- 24

dd[dd$council=="AOHS",]$lme_number <- -98
dd[dd$council=="POHS",]$lme_number <- -99
dd[dd$council=="IOHS",]$lme_number <- -97

summary(dd)
dd[is.na(dd$lme_number),]$lme_number <- 49
#dd[!is.na(dd$area) & dd$area=="Northwest Pacific",]$lme_number <- 51

table(dd$lme_number)
# -99 -98 -97 -96   1   2   3   5   6   7   8   9  13  14  20  21  22  23  24  25  26  29  30  40 
# 4   7   1   1  14  16  15   9  14  25   4   6   7   6   7   1  15   7  30  10   5  10   1   1 

# 42  43  46  47  49  50  59  60  62 
# 7   4  20   2   5   7   7   3   6 

write.csv(dd, "277stocks_with_lme_code.csv", row.names=F)

