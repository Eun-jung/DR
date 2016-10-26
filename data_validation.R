###
### data validation
###



#####
## 1. unitPeriodUsage NA 
#####

# nrow(raw_15min[is.na(unitPeriodUsage)])/nrow(raw_15min)*100 #0.04828452%

usage_NA <- raw_15min[is.na(raw_15min$unitPeriodUsage)]
count_NA_per_day_siteId = raw_15min[, .(NAcount = sum(is.na(unitPeriodUsage))), by=c("day", "siteId")]
hist(count_NA_per_day_siteId$NAcount)
nrow(count_NA_per_day_siteId[NAcount==0])/nrow(count_NA_per_day_siteId) # 0.9738741
# summary(count_NA_per_day_siteId$NAcount)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00000  0.00000  0.00000  0.04483  0.00000 23.00000

# count_NA_per_day_siteId[count==23]
# day   siteId count
# 1: 2016-05-13 10005334    23

# length(unique(count_NA_per_day_siteId[count==0]$siteId))
# [1] 9161
# > head(unique(count_NA_per_day_siteId[count==0]$siteId))
# [1] 10000399 10000474 10000478 10000480 10000482 10000484


#####
## 1-1. unitPeriodUsage NA - time distribution(15min)
#####

count_NA_per_15min <- usage_NA[, .(count = nrow(.SD)), by=strftime(timestamp, format="%H:%M", tz="Asia/Seoul")]
count_NA_per_15min$strftime <- as.POSIXct(count_NA_per_15min$strftime, format = "%H:%M", origin="1970-01-01 00:00", tz='Asia/Seoul') 

count_NA_per_15min <- usage_NA[, .(count = nrow(.SD)), by=time]

# > head(count_NA_per_15min[order(count_NA_per_15min$count,decreasing=T)])
# strftime count
# 1:    01:45   616
# 2:    18:45   572
# 3:    23:45   559
# 4:    09:15   546
# 5:    09:00   522
# 6:    07:15   514

plot_count_NA_per_15min <- ggplot(count_NA_per_15min, aes(x=time)) +
  geom_point(data = count_NA_per_15min, aes(y=count)) +
  ggtitle("time distribution of NA rows") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot_count_NA_per_15min



#####
## 1-2. unitPeriodUsage NA - plots of the siteId which has max NA counts
#####

count_NA_per_siteId = usage_NA[, .(count = nrow(.SD)), by=c("siteId")]
head(count_NA_per_siteId[order(count, decreasing=T)])
# summary(count_NA_per_siteId$count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    2.00   11.34    5.00  427.00 

# count_NA_per_siteId[count == 427]
# siteId count
# 1: 10005334   427

# max_NA = raw_15min[day == "2016-05-13" & siteId == 10005334]
# max_NA[is.na(max_NA$unitPeriodUsage)]$unitPeriodUsage <- -99

list_max_NA <- get.list.for.plot(raw_15min[siteId==10005334])
save.plot.iteratively(target_dir = "plot/max_NA_user/", dt_list = list_max_NA)



#####
## 2. rownum of day 
#####

rownum_of_day <- raw_15min[, .(count = nrow(.SD)), by=c("siteId", "deviceID", "day")]
# nrow(rownum_of_day[count!=96])/nrow(rownum_of_day)*100 #9.75215%
# summary(rownum_of_day$count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00   96.00   96.00   92.84   96.00   97.00 


#####
## 2-1. rownum of day - 97 rows
#####

rownum_97 = rownum_of_day[count==97]

# 모두 다른 siteId
# siteId deviceID        day count
# 1: 10000515     2339 2016-05-02    97
# 2: 10000522     2346 2016-05-02    97
# 3: 10000617     2441 2016-05-02    97
# 4: 10001071     2895 2016-05-02    97
# 5: 10002217     4041 2016-05-02    97
# 6: 10002184     4008 2016-05-02    97
# 7: 10002541     4365 2016-05-02    97
# 8: 10002727     4551 2016-07-27    97
# 9: 10003038     4862 2016-07-27    97
# 10: 10003364     5196 2016-05-02    97
# 11: 10003746     5578 2016-05-02    97
# 12: 10004535     6382 2016-05-02    97
# 13: 10005461     7346 2016-05-02    97
# 14: 10006202     8087 2016-05-02    97
# 15: 10008826    10753 2016-05-02    97

row_97_list = c()
for(i in 1:nrow(rownum_97)){
  dt = raw_15min[siteId == rownum_97[i]$siteId & day == rownum_97[i]$day]
  row_97_list <- append(row_97_list, list(dt[97]))
  print(paste(dt$timestamp[97], dt$siteId[97], dt$unitPeriodUsage[97]))
}
# [1] "2016-05-02 18:30:00 10000515 117.693"
# [1] "2016-05-02 18:30:00 10000522 NA"
# [1] "2016-05-02 18:30:00 10000617 134.645"
# [1] "2016-05-02 18:30:00 10001071 NA"
# [1] "2016-05-02 23:45:00 10002217 207.857"
# [1] "2016-05-02 18:30:00 10002184 42.751"
# [1] "2016-05-02 18:30:00 10002541 NA"
# [1] "2016-07-27 14:45:00 10002727 559.675"
# [1] "2016-07-27 01:45:00 10003038 63.273"
# [1] "2016-05-02 18:30:00 10003364 NA"
# [1] "2016-05-02 18:30:00 10003746 8.437"
# [1] "2016-05-02 18:30:00 10004535 177.378"
# [1] "2016-05-02 18:30:00 10005461 NA"
# [1] "2016-05-02 18:30:00 10006202 77.736"
# [1] "2016-05-02 18:30:00 10008826 NA"

tmp = raw_15min[siteId == 10003038 & day == "2016-07-27"]
tmp_list = get.list.for.plot(tmp)
# plot.iteratively(tmp_list)

for(row in row_97_list){
  tmp = raw_15min[siteId == row$siteId & day == row$day]
  tmp_list = get.list.for.plot(tmp)
  save.plot.iteratively(target_dir = "plot/15min_97blocks/", tmp_list)
}

