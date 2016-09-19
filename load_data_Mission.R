### load Mission data and
### add Mission information
###

library(reshape2)

raw_mission = fread("data/raw/uplusMissionAll.csv")
setnames(raw_mission, old=c("siteid","missionId"), new=c("siteID","missionID"))

# > unique(raw_mission$missionId)
# [1] 138 144 156 159 162 166 167 171

#to do : 경은이한테 전체 mission info table 부탁하기
MISSION_INFO <- data.table(
                  c(138, "2016-07-20 14:00:00", "2016-07-20 15:00:00"),
                  c(144, "2016-07-26 14:00:00", "2016-07-26 15:00:00"),
                  c(156, "2016-08-04 14:00:00", "2016-08-04 15:00:00"),
                  c(159, "2016-08-10 15:00:00", "2016-08-10 16:00:00"),
                  c(162, "2016-08-17 14:00:00", "2016-08-17 15:00:00"),
                  c(166, "2016-08-25 14:00:00", "2016-08-25 15:00:00"),
                  c(167, "2016-09-01 15:00:00", "2016-09-01 16:00:00"),
                  c(171, "2016-09-06 14:00:00", "2016-09-06 15:00:00"))

MISSION_INFO <- data.table(t(MISSION_INFO))
names(MISSION_INFO) <- c("missionID","start_timestamp","end_timestamp")

MISSION_INFO$missionID <- as.integer(MISSION_INFO$missionID)
MISSION_INFO$start_timestamp <- as.POSIXct(MISSION_INFO$start_timestamp, tz='Asia/Seoul')
MISSION_INFO$end_timestamp <- as.POSIXct(MISSION_INFO$end_timestamp, tz='Asia/Seoul')

mission_dt = merge(raw_mission, MISSION_INFO, by="missionID", all.x=T)

enabled_mission_dt <- mission_dt[disconnectCount == 0]

# get.mission.info <- function(input_siteID, input_timestamp){
#   
#   mission_joined=c()
#   mission_succeed=c()
#   mission_reason=c()
#   mission_ongoing=c()
#   
#   for(index in 1:length(input_siteID)){
#     dt = enabled_mission_dt[siteID == input_siteID[index] &
#                               as.Date(start_timestamp, tz="Asia/Seoul") == as.Date(input_timestamp[index], tz="Asia/Seoul")]
#     #   
#     #   print(input_siteID)
#     #   print(input_timestamp)
#     
#     joined = ifelse(nrow(dt)==0, NA, dt$joined)
#     succeed = ifelse(nrow(dt)==0, NA, dt$succeed)
#     reason = ifelse(nrow(dt)==0, NA, dt$reason)
#     ongoing = ifelse(nrow(dt)==0, NA, (input_timestamp >= dt$start_timestamp & input_timestamp < dt$end_timestamp))
#     
#     mission_joined <- append(mission_joined, joined)
#     mission_succeed <- append(mission_succeed, succeed)
#     mission_reason <- append(mission_reason, reason)
#     mission_ongoing <- append(mission_ongoing, ongoing)
#   }
#   
# #   dt = enabled_mission_dt[siteID == input_siteID &
# #                           as.Date(start_timestamp, tz="Asia/Seoul") == as.Date(input_timestamp, tz="Asia/Seoul")]
# # #   
# # #   print(input_siteID)
# # #   print(input_timestamp)
# #   
# #   mission_joined = ifelse(nrow(dt)==0, NA, dt$joined)
# #   mission_succeed = ifelse(nrow(dt)==0, NA, dt$succeed)
# #   mission_reason = ifelse(nrow(dt)==0, NA, dt$reason)
# #   mission_ongoing = ifelse(nrow(dt)==0, NA, (input_timestamp >= dt$start_timestamp & input_timestamp < dt$end_timestamp))
# 
#   return(list(mission_joined = mission_joined, 
#               mission_succeed = mission_succeed,
#               mission_reason = mission_reason,
#               mission_ongoing = mission_ongoing))    
# }
# 
# get.mission.joined <- function(input_siteID, input_timestamp){
#   dt = enabled_mission_dt[siteID == input_siteID &
#                             as.Date(start_timestamp, tz="Asia/Seoul") == as.Date(input_timestamp, tz="Asia/Seoul")]
#   
#   joined = ifelse(nrow(dt)==0, NA, dt$joined)
#   
#   print(joined)
#   
#   return(is.integer(joined))
# }

data_15min = raw_15min

enabled_mission_dt$day <- as.Date(enabled_mission_dt$start_timestamp, tz="Asia/Seoul")
data_15min <- merge(data_15min, enabled_mission_dt, by=c("siteID","day"), all.x=T)

# save(data_15min, file ="data/data_15min.RData")

## Loading saved data(raw_15min + mission data) 
load("data/data_15min.RData")

# data_15min <- data_15min[joined==1, ':='(isOnMission = ifelse(timestamp >= start_timestamp & timestamp < end_timestamp, T, F))]
# 
# tmp = get.list.for.plot(data_15min[siteID==10001284 & day=="2016-07-20"])
# plot.iteratively(tmp)
