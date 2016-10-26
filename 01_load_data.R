library(data.table)
library(bit64)
library(readr)
library(zoo)
library(ggplot2)
library(timeDate)
library(scales)
library(grDevices)
library(chron)

destfile = "data/raw/raw_15min_161010.RData"

## if saved RData doesn't exist, then read csv files
if(!file.exists(destfile)){
  lgu5_9 = fread("data/raw/1462028400000_1473174000000_15min_usages.csv")
  names(lgu5_9) <- c("siteId","deviceId","timestamp","unitPeriodUsage")
  
  lgu5_9$timestamp = lgu5_9$timestamp/1000
  lgu5_9$group = "LGU"
  
  lgu1 = fread("data/raw/lgu1.csv")
  lgu2 = fread("data/raw/lgu2.csv")
  lgu3 = fread("data/raw/lgu3.csv")
  lgu4 = fread("data/raw/lgu4.csv")
  lgu9 = fread("data/raw/lgu9.csv")
  encored1_9 = fread("data/raw/encored15minUsage.csv")
  
  raw_list = list(lgu5_9, lgu1, lgu2, lgu3, lgu4, lgu9, encored1_9)
  
  raw_15min = rbindlist(raw_list)
  raw_15min = unique(raw_15min) ## eliminate redundant rows
  
  raw_15min$timestamp = as.POSIXct(raw_15min$timestamp, origin="1970-01-01 00:00:00", tz='Asia/Seoul')
  raw_15min$time = chron(times=strftime(raw_15min$timestamp, format="%H:%M:%S"))
  
  #add day column
  raw_15min[, ':='(day = as.Date(timestamp, tz="Asia/Seoul"))]
  raw_15min[, ':='(workingday = isWeekday(day))]
  
  ## Update 'holidays' to 'workingday = F' 
  HOLIDAYS = c('2016-01-01',
               '2016-02-08', '2016-02-09', '2016-02-10',
               '2016-03-01',
               '2016-04-13', 
               '2016-05-05', '2016-05-06',
               '2016-06-06',
               '2016-08-15',
               '2016-09-14', '2016-09-15', '2016-09-16',
               '2016-10-03')
  HOLIDAYS = as.Date(HOLIDAYS, tz="Asia/Seoul")
  raw_15min[day %in% HOLIDAYS, ':='(workingday = FALSE)] 
  
  save(raw_15min, file ="data/raw/raw_15min_161010.RData")
} else{
  ## Loading saved raw data 
  load("data/raw/raw_15min_161010.RData")
}




