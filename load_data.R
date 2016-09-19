library(data.table)
library(bit64)
library(readr)
library(zoo)
library(ggplot2)
library(timeDate)
library(scales)
library(grDevices)


raw_15min = fread("Data/1462028400000_1473174000000_15min_usages.csv")
#siteID, deciveID, timestamp, unitPeriodUsage
names(raw_15min) <- c("siteID","deviceID","timestamp","unitPeriodUsage")

raw_15min$timestamp = as.POSIXct(raw_15min$timestamp/1000, origin="1970-01-01 00:00:00", tz='Asia/Seoul')
raw_15min$unitPeriodUsage = as.numeric(raw_15min$unitPeriodUsage)/1000.0 #mWh -> kWh
raw_15min$time = strftime(raw_15min$timestamp, format="%H:%M")

#add day column
raw_15min <- raw_15min[, ':='(day = as.Date(timestamp, tz="Asia/Seoul"))]


# save(raw_15min, file ="data/raw/raw_15min.RData")

## Loading saved raw data 
load("data/raw/raw_15min.RData")


# raw_api_log = fread("data/raw/log_prod1_0.txt")
# # 
# # untar("../data/raw/api_log1.tar.gz", files="../data/raw")
# 
# log_tmp = read_log("data/raw/log_prod1_0.txt")


###
### data validation check
###




data_NA <- data_15min[, .(siteID = siteID,
                          deviceID = deviceID,
                          count_all = nrow(.SD), 
                          count_na = sum(ifelse(is.na(unitPeriodUsage),1,0))), by=c("time", "siteID", "deviceID")]




rownum_of_day <- data_15min[, .(count = nrow(.SD)), by=c("siteID", "day")]
# nrow(rownum_of_day[count!=96])/nrow(rownum_of_day)*100 #9.75215%
summary(rownum_of_day$count)

# rownum_of_day[count==97]
# siteID        day count
# 1: 10000515 2016-05-02    97
# 2: 10000522 2016-05-02    97
# 3: 10000617 2016-05-02    97
# 4: 10001071 2016-05-02    97
# 5: 10002184 2016-05-02    97
# 6: 10002217 2016-05-02    97
# 7: 10002541 2016-05-02    97
# 8: 10002727 2016-07-27    97
# 9: 10003038 2016-07-27    97
# 10: 10003364 2016-05-02    97
# 11: 10003746 2016-05-02    97
# 12: 10004535 2016-05-02    97
# 13: 10005461 2016-05-02    97
# 14: 10006202 2016-05-02    97
# 15: 10008826 2016-05-02    97

data_15min[siteID %in% rownum_of_day[count==97]$siteID]

tmp<-data_15min[!is.na(raw_15min$unitPeriodUsage), .(count = nrow(.SD)), by=c("deviceID", "day")]
tmp2<-tmp[count==96]
tmp3<-tmp2[, .(count = nrow(.SD)), by="deviceID"]
# nrow(tmp3[count==128])/nrow(data_15min[, .(count = nrow(.SD)), by="deviceID"])*100 #5.71211%

nalocf_data <- data_15min
nalocf_data$unitPeriodUsage <- na.locf(nalocf_data$unitPeriodUsage)
tmp<-nalocf_data[!is.na(nalocf_data$unitPeriodUsage), .(count = nrow(.SD)), by=c("deviceID", "day")]
tmp2<-tmp[count==96]
tmp3<-tmp2[, .(count = nrow(.SD)), by="deviceID"]
# nrow(tmp3[count==128])/nrow(data_15min[, .(count = nrow(.SD)), by="deviceID"])*100 #6.005875%
