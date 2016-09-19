###
### data validation
###



#####
## 1. unitPeriodUsage NA 
#####

# nrow(raw_15min[is.na(unitPeriodUsage)])/nrow(raw_15min)*100 #0.04828452%

usage_NA <- raw_15min[is.na(raw_15min$unitPeriodUsage)]
count_NA_per_day_siteID = usage_NA[, .(count = nrow(.SD)), by=c("day", "siteID")]
hist(count_NA_per_day_siteID$count)
# summary(count_NA_per_day_siteID$count)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00000  0.00000  0.00000  0.04483  0.00000 23.00000

# count_NA_per_day_siteID[count==23]
# day   siteID count
# 1: 2016-05-13 10005334    23

# length(unique(count_NA_per_day_siteID[count==0]$siteID))
# [1] 9161
# > head(unique(count_NA_per_day_siteID[count==0]$siteID))
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
## 1-2. unitPeriodUsage NA - plots of the siteID which has max NA counts
#####

count_NA_per_siteID = usage_NA[, .(count = nrow(.SD)), by=c("siteID")]
head(count_NA_per_siteID[order(count, decreasing=T)])
# summary(count_NA_per_siteID$count)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    2.00   11.34    5.00  427.00 

# count_NA_per_siteID[count == 427]
# siteID count
# 1: 10005334   427

# max_NA = raw_15min[day == "2016-05-13" & siteID == 10005334]
# max_NA[is.na(max_NA$unitPeriodUsage)]$unitPeriodUsage <- -99
# 
# tmp_list <- get.list.for.plot(max_NA)
# plot.iteratively(tmp_list)

list_max_NA <- get.list.for.plot(raw_15min[siteID==10005334])
plot.iteratively(list_max_NA)


#####
## 2. rownum of day 
#####

rownum_of_day <- data_15min[, .(count = nrow(.SD)), by=c("siteID", "day")]
# nrow(rownum_of_day[count!=96])/nrow(rownum_of_day)*100 #9.75215%
summary(rownum_of_day$count)

# rownum_of_day[count==97] --> 모두 다른 siteID
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

tmp = rownum_of_day[count==97]
tmp_list = c()
for(i in 1:nrow(tmp)){
  dt = data_15min[siteID == tmp[i]$siteID & day == tmp[i]$day]
  tmp_list <- append(tmp_list, list(dt[97]))
}

raw_15min[siteID == tmp_list[[1]]$siteID & day == tmp_list[[1]]$day]

