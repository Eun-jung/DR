library(data.table)
library(bit64)
library(readr)
library(zoo)
library(ggplot2)
library(timeDate)
library(scales)


raw_15min = fread("data/raw/1462028400000_1473174000000_15min_usages.csv")
#siteID, deciveID, timestamp, unitPeriodUsage
names(raw_15min) <- c("siteID","deviceID","timestamp","unitPeriodUsage")

raw_15min$timestamp = as.POSIXct(raw_15min$timestamp/1000, origin="1970-01-01 00:00:00", tz='Asia/Seoul')

# raw_api_log = fread("data/raw/log_prod1_0.txt")
# # 
# # untar("../data/raw/api_log1.tar.gz", files="../data/raw")
# 
# log_tmp = read_log("data/raw/log_prod1_0.txt")


###
### data validation check
###

# nrow(raw_15min[is.na(unitPeriodUsage)])/nrow(raw_15min)*100 #0.04828452%

#add day column
data_15min <- raw_15min[, ':='(day = as.Date(timestamp, tz="Asia/Seoul"))]

usage_NA <- data_15min[is.na(raw_15min$unitPeriodUsage),]
# usage_NA <- usage_NA[, .(count = nrow(.SD)), by=c("deviceID", "day")]
# summary(usage_NA$count)

#NA time distribution
time_dist <- usage_NA[, .(count = nrow(.SD)), by=strftime(timestamp, format="%H:%M")]
xlabel <- time_dist$strftime
xlabel <- xlabel[order(xlabel)]
time_dist$strftime <- as.POSIXct(time_dist$strftime, format = "%H:%M", origin="1970-01-01 00:00", tz='Asia/Seoul') 

plot_time_dist <- ggplot(time_dist, aes(x=strftime)) +
                  geom_point(data = time_dist, aes(y=count)) +
                  ggtitle("time distribution of NA rows") +
                  scale_x_datetime(labels = date_format("%H:%M", tz="Asia/Seoul"), breaks = date_breaks("15 min")) +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1))



rownum_of_day <- data_15min[, .(count = nrow(.SD)), by=c("deviceID", "day")]
# nrow(rownum_of_day[count!=96])/nrow(rownum_of_day)*100 #9.75215%
summary(rownum_of_day$count)

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
