
list_MISSIONID = unique(mission_dt$missionID) #[1] 138 144 156 159 162 166 167 171

revised_missioninfo_dt = data.table(missionID = MISSION_INFO$missionID, start_timestamp = MISSION_INFO$start_timestamp, end_timestamp = MISSION_INFO$end_timestamp)
revised_missioninfo_dt$N_total <- 0
revised_missioninfo_dt$N_valid <- 0
revised_missioninfo_dt$N_joined <- 0
revised_missioninfo_dt$N_succeed <- 0
revised_missioninfo_dt$N_fail <- 0
revised_missioninfo_dt$load_CBL <- 0
revised_missioninfo_dt$load_mission <- 0
revised_missioninfo_dt$mean_reductionRate <- 0

each_mission_result_list = list()

st = Sys.time()
for(target_missionID in list_MISSIONID){

  MISSIONDAY = MISSION_INFO[missionID==target_missionID]$day
  MISSIONSTARTTIME = MISSION_INFO[missionID==target_missionID]$start_time
  MISSIONENDTIME = MISSION_INFO[missionID==target_missionID]$end_time
  
  target_missionDT = mission_dt[missionID == target_missionID]
  
#   ##number of customers
#   nrow(target_missionDT) #3469
#   nrow(target_missionDT[reason != "DISCONNECTED"]) #3456
#   nrow(target_missionDT[disconnectCount == "0" & reason != "DISCONNECTED"]) #3453
  #ENCORED said the number of valid users is 3279 *****Question
  target_missionDT = target_missionDT[disconnectCount == "0" & reason != "DISCONNECTED"]
  
  
#   ##participation rate
#   nrow(target_missionDT[joined == 1])/nrow(target_missionDT) # 0.07523782
#   nrow(target_missionDT[joined == 1]) #261
#   nrow(target_missionDT[joined == 1 & reason != "DISCONNECTED"]) #248 --> ENCORED used this but I don't know how to calculate ENCORED's participation rate(0.0756)
#   nrow(target_missionDT[joined == 1 & reason != "DISCONNECTED"])/nrow(target_missionDT[reason != "DISCONNECTED"]) #0.07175926
#   nrow(target_missionDT[joined == 1 & reason != "DISCONNECTED"])/nrow(target_missionDT) #0.07149034
#   
#   ##success 
#   nrow(target_missionDT[succeed == 1])/nrow(target_missionDT[joined == 1 ]) #0.48659
#   nrow(target_missionDT[succeed == 1])/nrow(target_missionDT[joined == 1 & reason != "DISCONNECTED"]) #0.5120968
#   nrow(target_missionDT[succeed == 1]) #127 --> ENCORED used this
#   nrow(target_missionDT[succeed == 0]) #134
#   
#   ##failure
#   nrow(target_missionDT[succeed == 0 & reason != "DISCONNECTED"]) #121 --> ENCORED used this
#   
  
  
  
  target_siteID = target_missionDT$siteID
  
  DEFAULT_START = MISSION_INFO[missionID == target_missionID]$day - 31
  DEFAULT_END = MISSION_INFO[missionID == target_missionID]$day #I wrote this term to know the effect after the mission but this work left undone(DEFAULT_END is still mission day)
  
  target_raw = data_15min[siteID %in% target_siteID & day >= DEFAULT_START & day <= DEFAULT_END & time >= MISSIONSTARTTIME & time < MISSIONENDTIME]
  # length(unique(target_raw$siteID)) #3453
  
  target_count_per_day_siteID = target_raw[, .(NAcount = sum(is.na(unitPeriodUsage)),
                                               rowcount = nrow(.SD)), by=c("day", "siteID")]
  target_count_per_day_siteID = target_count_per_day_siteID[,':='(isValid = (NAcount==0 & rowcount==4))]
  
  # nrow(target_count_per_day_siteID[NAcount==0])/nrow(target_count_per_day_siteID) # 0.9754041
  # length(unique(target_count_per_day_siteID[NAcount==0]$siteID)) # 3453
  # summary(target_count_per_day_siteID$rowcount)
  # # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # # 1.00   96.00   96.00   94.12   96.00   96.00 
  # length(unique(target_count_per_day_siteID[rowcount==96]$siteID)) # 3413 ; 
  # length(unique(target_count_per_day_siteID[rowcount==96 & NAcount==0]$siteID)) # 3413 
  
  # nrow(target_count_per_day_siteID[isValid==T])/nrow(target_count_per_day_siteID) #0.9291326
  
  revised_target_ID = unique(target_count_per_day_siteID[isValid==T]$siteID) #3413
  
  # target_raw = target_raw[, ':='(isValid = FALSE)]
  target_raw = merge(target_raw, target_count_per_day_siteID, by=c("day", "siteID"), all.x = T)
  revised_target_raw = target_raw[isValid==T & workingday == T]
  # nrow(revised_target_raw) # 7854432
  
  # target_raw = target_raw[c(day, siteID) %in% c(target_count_per_day_siteID[isValid == T]$day, target_count_per_day_siteID[isValid == T]$siteID)]
  
  ##aggregate 
  # target_dt = revised_target_raw[, .(timestamp = timestamp,
  #                                    day = day,
  #                                    time = time,
  #                                    timeIndex = as.integer(hours(time/15*60)),
  #                                    start_timestamp = start_timestamp,
  #                                    end_timestamp = end_timestamp,
  #                                    missionID = missionID,
  #                                    unitPeriodUsage = sum(unitPeriodUsage),
  #                                    workingday = workingday), by=c("siteID", "timestamp")] 
  
  target_dt = revised_target_raw[, ':='(unitPeriodUsage = sum(unitPeriodUsage)), by=c("siteID", "timestamp")]
  
  target_onMission_dt = target_dt[day <= MISSIONDAY & time >= MISSIONSTARTTIME & time < MISSIONENDTIME, .(unitPeriodUsage = sum(unitPeriodUsage)), by=c("day", "siteID")]
  CBL_raw = target_onMission_dt[order(siteID, day), tail(.SD, 11), by="siteID"]
  CBL_raw$isCBLday <- -1
  CBL_raw[day < MISSIONDAY] <- transform(CBL_raw[day < MISSIONDAY], isCBLday = ave(unitPeriodUsage, siteID, FUN = function(x) rank(-x, ties.method="first")<=6))
  # CBL_raw = CBL_raw[day < MISSIONDAY, ':='(isCBLday = frank(.SD, -unitPeriodUsgae, ties.method = "first")), by=c("day", "siteID")]
  #isCBLday; 1 : max 6 days, 0 : left 4 days, -1 : mission day
  CBL_dt = CBL_raw[isCBLday != 0, .(unitPeriodUsage = mean(unitPeriodUsage)), by=c("siteID", "isCBLday")]
  #     siteID  isCBLday unitPeriodUsage
  # 1: 10000484        1        301815.0
  # 2: 10000484       -1        140084.0
  # 3: 10000486        1        730245.7
  # 4: 10000493        1        458840.2
  # 5: 10000493       -1        763203.0
  # ---                                  
  # 6553: 10013197       -1        225803.0
  # 6554: 10013198        1       1284323.0
  # 6555: 10013198       -1        780540.0
  # 6556: 10013199        1        375077.3
  # 6557: 10013199       -1        337997.0
  
  result_dt = data.table(siteID = unique(CBL_dt$siteID))
  load_CBL_dt = CBL_dt[isCBLday == 1, .(siteID = siteID, load_CBL = unitPeriodUsage)]
  load_mission_dt = CBL_dt[isCBLday == -1, .(siteID = siteID, load_mission = unitPeriodUsage)]
  result_dt = merge(result_dt, load_CBL_dt, by="siteID")
  result_dt = merge(result_dt, load_mission_dt, by="siteID")
  
  result_dt$reductionRate <- 1 - result_dt$load_mission/result_dt$load_CBL
  
  result_dt = merge(result_dt, target_missionDT, by="siteID")
  
  revised_missioninfo_dt[missionID == target_missionID]$load_CBL <- sum(result_dt$load_CBL)
  revised_missioninfo_dt[missionID == target_missionID]$load_mission <- sum(result_dt$load_mission)
  revised_missioninfo_dt[missionID == target_missionID]$mean_reductionRate <- mean(result_dt$reductionRate)
  
  revised_missioninfo_dt[missionID == target_missionID]$N_total = nrow(target_missionDT)
  revised_missioninfo_dt[missionID == target_missionID]$N_valid = nrow(result_dt)
  revised_missioninfo_dt[missionID == target_missionID]$N_joined = nrow(result_dt[joined == 1 & reason != "DISCONNECTED"]) #244 (origin 248)
  revised_missioninfo_dt[missionID == target_missionID]$N_succeed = nrow(result_dt[succeed == 1]) #126 (origin 127)
  revised_missioninfo_dt[missionID == target_missionID]$N_fail = nrow(result_dt[succeed == 0]) #118 (origin 121)
  
  
  #add each mission result to each_mission_result_list 
  each_mission_result_list = append(each_mission_result_list, setNames(list(result_dt),target_missionID))
  
}

et = Sys.time()
print(et - st)

write.csv(revised_missioninfo_dt, file = "output/revised_missioninfo_dt.csv")
