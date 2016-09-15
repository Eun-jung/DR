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

## NA - time distribution

# usage_NA <- data_15min[is.na(raw_15min$unitPeriodUsage),]
count_NA_per_day_siteID = data_15min[, .(count = sum(is.na(.SD))), by=c("day", "siteID")]
# summary(count_NA_per_day_siteID$count)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00000  0.00000  0.00000  0.04483  0.00000 23.00000

# count_NA_per_day_siteID[count==23]
# day   siteID count
# 1: 2016-05-13 10005334    23

max_NA = data_15min[day == "2016-05-13" & siteID == 10005334]
max_NA[is.na(max_NA$unitPeriodUsage)]$unitPeriodUsage <- -99
# 
# label_15min <- seq(
#   from=as.POSIXct("0:00", format="%H:%M", tz="Asia/Seoul"),
#   to=as.POSIXct("23:45", format="%H:%M", tz="Asia/Seoul"),
#   by="15 min"
# )  
# label_15min <- data.table(time = strftime(label_15min, format="%H:%M"))

get.15min.label.dt <- function(label_day){
  
  label_day = as.Date(label_day, tz="Asia/Seoul")
  
  labels <- seq(
    from = as.POSIXct(paste(label_day, "00:00"), format="%Y-%m-%d %H:%M", tz="Asia/Seoul"),
    to = as.POSIXct(paste(label_day, "23:45"), format="%Y-%m-%d %H:%M", tz="Asia/Seoul"),
    by="15 min"
  )
  
  labels_dt = data.table(timestamp = labels)
  
  return(labels_dt)
}

# max_NA = merge(label_15min, max_NA, by="time", all.x=T)
# 
# p <- ggplot(data = max_NA, aes(x=time)) +
#       ylim(0, NA) + 
#       geom_point(aes(y=unitPeriodUsage)) +
#       theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#       geom_vline(xintercept=c(max_NA[, .I[unitPeriodUsage==-99]]), color="red") 
# 
# make.plot <- function(dt){
#   plot_dt = dt[[1]]
#   plot_dt_name = names(dt)
#   
#   p <- ggplot(data = plot_dt, aes(x = time)) +
#     ggtitle(plot_dt_name) + 
#     ylim(0, NA) + 
#     geom_point(aes(y = unitPeriodUsage)) +
#     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#     geom_vline(xintercept = c(plot_dt[, .I[unitPeriodUsage == -99]]), color="red") 
#   
#   return(p)
# }

add.NA.vline <- function(plot_body, plot_dt){
  p <- plot_body + 
    geom_vline(xintercept = c(plot_dt[, .I[unitPeriodUsage == -99]]), color="red") 
  
  return(p)
}

readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
  
  return(line)
}


plot_iteratively <- function(input_data, days = NULL, siteIDs = NULL){
  
  if(is.null(days)){
    uniqueDays =  unique(as.Date(input_data$timestamp, tz="Asia/Seoul"))
  } else{
    uniqueDays =  unique(as.Date(days, tz="Asia/Seoul"))
  }
  
  if(is.null(siteIDs)){
    uniqueSites =  unique(input_data$siteID)
  } else{
    uniqueSites =  unique(as.integer(siteIDs))
  }
  
  data_list = list()

  for(d in uniqueDays){
    for(s in uniqueSites){
      ## NA -> -99
      oneDay_dt = input_data[siteID == s & as.Date(timestamp, tz="Asia/Seoul") == d,
                             '.'(timestamp = timestamp,
                                 deviceID = deviceID,
                                 unitPeriodUsage = unitPeriodUsage)]
      oneDay_dt_name = paste(as.Date(d, tz="Asia/Seoul"), s, sep = "_")
      
#       #if oneDay_dt does not have whole 24-hour data, merge with label_15min
#       if(sum(oneDay_dt$time == label_15min$time) != 96){

        default_timestamp_dt <- get.15min.label.dt(d)
        oneDay_dt = merge(default_timestamp_dt, oneDay_dt, by="timestamp", all.x=T)
      # }
      
      oneDay_dt = oneDay_dt[, ':='(unitPeriodUsage = ifelse(is.na(unitPeriodUsage), -99, unitPeriodUsage))]
        
      data_list = append(data_list, setNames(list(oneDay_dt), oneDay_dt_name))
      
    }
  }
  
  for(dt_index in 1:length(data_list)){
    plot.new()
    plot_dt = data_list[dt_index][[1]]
    plot_dt_name = names(data_list[dt_index])
    
    # print(plot_dt)
    
    p <- ggplot(data = plot_dt, aes(x = strftime(timestamp, format="%H:%M"))) +
      ggtitle(plot_dt_name) + 
      ylim(0, NA) + 
      geom_point(aes(y = unitPeriodUsage)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Time") +
      ylab("usage(mWh)")
    
    p <- add.NA.vline(p, plot_dt)
    
    # p <- make.plot(dt)
    print(p)
    
    isEnter=FALSE
    while(!isEnter){
      input_key = readkey()
      
      if(input_key == 'q'){
        stop("force to quit")
      }else if(!is.na(input_key)){
        isEnter=TRUE
      }
    }
    
  }
}


# length(unique(count_NA_per_day_siteID[count==0]$siteID))
# [1] 9161
# > head(unique(count_NA_per_day_siteID[count==0]$siteID))
# [1] 10000399 10000474 10000478 10000480 10000482 10000484


raw_15min[siteID == 10005334 & day == "2016-05-13"]

count_NA_per_siteID = usage_NA[, .(count = nrow(.SD)), by=c("siteID")]
# summary(count_NA_per_siteID$count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    2.00   11.34    5.00  427.00 

# count_NA_per_siteID[count == 427]
# siteID count
# 1: 10005334   427




count_NA_per_15min <- usage_NA[, .(count = nrow(.SD)), by=strftime(timestamp, format="%H:%M", tz="Asia/Seoul")]
count_NA_per_15min$strftime <- as.POSIXct(count_NA_per_15min$strftime, format = "%H:%M", origin="1970-01-01 00:00", tz='Asia/Seoul') 

plot_count_NA_per_15min <- ggplot(count_NA_per_15min, aes(x=strftime)) +
                  geom_point(data = count_NA_per_15min, aes(y=count)) +
                  ggtitle("time distribution of NA rows") +
                  scale_x_datetime(labels = date_format("%H:%M", tz="Asia/Seoul"), breaks = date_breaks("15 min")) +
                  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# > head(count_NA_per_15min[order(count_NA_per_15min$count,decreasing=T)])
# strftime count
# 1:    01:45   616
# 2:    18:45   572
# 3:    23:45   559
# 4:    09:15   546
# 5:    09:00   522
# 6:    07:15   514


data_NA <- data_15min[, .(siteID = siteID,
                          deviceID = deviceID,
                          count_all = nrow(.SD), 
                          count_na = sum(ifelse(is.na(unitPeriodUsage),1,0))), by=c("time", "siteID", "deviceID")]

rownum_of_day <- data_15min[, .(count = nrow(.SD)), by=c("siteID", "day")]
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
