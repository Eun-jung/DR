library(data.table)
library(bit64)
library(readr)
library(zoo)
library(ggplot2)
library(timeDate)
library(scales)
library(grDevices)


raw_15min = fread("data/raw/1462028400000_1473174000000_15min_usages.csv")
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


# ###
# ### data validation check
# ###
# 
# 
# 
# 
# data_NA <- data_15min[, .(siteID = siteID,
#                           deviceID = deviceID,
#                           count_all = nrow(.SD), 
#                           count_na = sum(ifelse(is.na(unitPeriodUsage),1,0))), by=c("time", "siteID", "deviceID")]
# 
# 
# 
# 
# data_15min[siteID %in% rownum_of_day[count==97]$siteID]
# 
# tmp<-raw_15min[!is.na(raw_15min$unitPeriodUsage), .(count = nrow(.SD)), by=c("deviceID", "day")]
# tmp2<-tmp[count==96]
# tmp3<-tmp2[, .(count = nrow(.SD)), by="deviceID"]
# # nrow(tmp3[count==128])/nrow(data_15min[, .(count = nrow(.SD)), by="deviceID"])*100 #5.71211%
# 
# nalocf_data <- data_15min
# nalocf_data$unitPeriodUsage <- na.locf(nalocf_data$unitPeriodUsage)
# tmp<-nalocf_data[!is.na(nalocf_data$unitPeriodUsage), .(count = nrow(.SD)), by=c("deviceID", "day")]
# tmp2<-tmp[count==96]
# tmp3<-tmp2[, .(count = nrow(.SD)), by="deviceID"]
# # nrow(tmp3[count==128])/nrow(data_15min[, .(count = nrow(.SD)), by="deviceID"])*100 #6.005875%
