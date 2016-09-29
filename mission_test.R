
list_MISSIONID = unique(mission_dt$missionID) #[1] 138 144 156 159 162 166 167 171

revised_missioninfo_dt = data.table(missionID = MISSION_INFO$missionID, start_timestamp = MISSION_INFO$start_timestamp, end_timestamp = MISSION_INFO$end_timestamp)
revised_missioninfo_dt = revised_missioninfo_dt[, ':='(N_control = 0,
                                                       N_total = 0,
                                                       N_valid = 0,
                                                       N_notJoined = 0,
                                                       N_joined = 0,
                                                       N_succeed = 0,
                                                       N_fail = 0,
                                                       reductionRate_control = 0,
                                                       reductionRate_valid = 0,
                                                       reductionRate_notJoined = 0,
                                                       reductionRate_joined = 0,
                                                       reductionRate_succeed = 0,
                                                       reductionRate_fail = 0)]
# revised_missioninfo_dt$N_total <- 0
# revised_missioninfo_dt$N_valid <- 0
# revised_missioninfo_dt$N_joined <- 0
# revised_missioninfo_dt$N_succeed <- 0
# revised_missioninfo_dt$N_fail <- 0
# revised_missioninfo_dt$load_CBL <- 0
# revised_missioninfo_dt$load_mission <- 0
# revised_missioninfo_dt$mean_reductionRate_valid <- 0
# revised_missioninfo_dt$mean_reductionRate_Notjoined <- 0
# revised_missioninfo_dt$mean_reductionRate_joined <- 0
# revised_missioninfo_dt$mean_reductionRate_succeed <- 0
# revised_missioninfo_dt$mean_reductionRate_fail <- 0

each_mission_result_list = list()

st = Sys.time()
for(target_missionID in list_MISSIONID){
  
  MISSIONDAY = MISSION_INFO[missionID==target_missionID]$day
  MISSIONSTARTTIME = MISSION_INFO[missionID==target_missionID]$start_time
  MISSIONENDTIME = MISSION_INFO[missionID==target_missionID]$end_time
  
  raw_target_missionDT = mission_dt[missionID == target_missionID]
  target_missionDT = raw_target_missionDT[disconnectCount == "0" & reason != "DISCONNECTED"]
  target_siteID = target_missionDT$siteID
  
  DEFAULT_START = MISSION_INFO[missionID == target_missionID]$day - 31
  DEFAULT_END = MISSION_INFO[missionID == target_missionID]$day #I wrote this term to know the effect after the mission but this work left undone(DEFAULT_END is still mission day)
  
  target_raw = data_15min[siteID %in% target_siteID & day >= DEFAULT_START & day <= DEFAULT_END]
  # target_raw = data_15min[siteID %in% target_siteID & day >= DEFAULT_START & day <= DEFAULT_END & time >= MISSIONSTARTTIME & time < MISSIONENDTIME]
  target_raw$isControl <- F
  control_raw = data_15min[!(siteID %in% raw_target_missionDT$siteID) & day >= DEFAULT_START & day <= DEFAULT_END]
  control_raw$isControl <- T
  
  target_raw = rbindlist(list(target_raw, control_raw), use.names = T)
  
  target_count_per_day_siteID = target_raw[time >= MISSIONSTARTTIME & time < MISSIONENDTIME, .(NAcount = sum(is.na(unitPeriodUsage)),
                                                                                               rowcount = nrow(.SD)), by=c("day", "siteID")]
  # target_count_per_day_siteID = target_count_per_day_siteID[,':='(isValid = (NAcount==0 & rowcount==96))]
  target_count_per_day_siteID = target_count_per_day_siteID[,':='(isValid = (NAcount==0 & rowcount==4))]
 
  target_raw = merge(target_raw, target_count_per_day_siteID, by=c("day", "siteID"), all.x = T)
  
  target_dt = target_raw[isValid == T & workingday == T, .(day = day, time = time, isControl = isControl, unitPeriodUsage = sum(unitPeriodUsage)), by=c("siteID", "timestamp")]
  
  # target_onMission_dt = target_dt[day <= MISSIONDAY, .(unitPeriodUsage = sum(unitPeriodUsage)/24.0), by=c("day", "siteID")]
  target_onMission_dt = target_dt[day <= MISSIONDAY & time >= MISSIONSTARTTIME & time < MISSIONENDTIME, .(isControl = all(isControl), unitPeriodUsage = sum(unitPeriodUsage)), by=c("day", "siteID")]
  
  
  ###
  ## to get CBL
  ###
  
  # extract valid last 10 days
  CBL_raw = target_onMission_dt[order(siteID, day) & day < MISSIONDAY, tail(.SD, 10), by="siteID"]
  CBL_raw = rbindlist(list(CBL_raw, target_onMission_dt[day == MISSIONDAY]), use.names = T)
  CBL_raw = CBL_raw[, ':='(count = nrow(.SD)), by="siteID"]
  CBL_raw = CBL_raw[count == 11]
  #isCBLday; 1 : 6 days(except top 2, bottom 2 days), 0 : left 4 days, -1 : mission day
#   CBL_raw$isCBLday <- -1
#   CBL_raw[day < MISSIONDAY] <- transform(CBL_raw[day < MISSIONDAY], isCBLday = ave(unitPeriodUsage, siteID, FUN = function(x) (rank(-x, ties.method="first")<=2)&rank(-x, ties.method="first")<=8))
  CBL_raw$isMissionday <- F
  CBL_raw[day == MISSIONDAY]$isMissionday <- T
  
  ## aggregate by CBL
  # CBL_dt = CBL_raw[isCBLday != 0, .(unitPeriodUsage = mean(unitPeriodUsage)), by=c("siteID", "isCBLday")]
  ## aggregate last 10 days
  CBL_dt = CBL_raw[, .(unitPeriodUsage = mean(unitPeriodUsage), isControl=all(isControl)), by=c("siteID", "isMissionday")]
  
  result_dt = data.table(siteID = unique(CBL_dt$siteID))
  # load_CBL_dt = CBL_dt[isCBLday == 1, .(siteID = siteID, load_CBL = unitPeriodUsage)]
  load_CBL_dt = CBL_dt[isMissionday == F, .(siteID = siteID, load_CBL = unitPeriodUsage, isControl = isControl)]
  # load_mission_dt = CBL_dt[isCBLday == -1, .(siteID = siteID, load_mission = unitPeriodUsage)]
  load_mission_dt = CBL_dt[isMissionday == T, .(siteID = siteID, load_mission = unitPeriodUsage)]
  result_dt = merge(result_dt, load_CBL_dt, by="siteID")
  result_dt = merge(result_dt, load_mission_dt, by="siteID")
  
  result_dt = result_dt[load_CBL != 0]
  
  result_dt$reductionRate <- 1 - result_dt$load_mission/result_dt$load_CBL
  
  result_dt = merge(result_dt, target_missionDT, by="siteID", all.x = T)

  revised_missioninfo_dt[missionID == target_missionID]$N_total = nrow(target_missionDT)
  revised_missioninfo_dt[missionID == target_missionID]$N_control = nrow(result_dt[isControl == T])
  revised_missioninfo_dt[missionID == target_missionID]$N_valid = nrow(result_dt[isControl == F])
  revised_missioninfo_dt[missionID == target_missionID]$N_notJoined = nrow(result_dt[joined == 0]) 
  revised_missioninfo_dt[missionID == target_missionID]$N_joined = nrow(result_dt[joined == 1]) 
  revised_missioninfo_dt[missionID == target_missionID]$N_succeed = nrow(result_dt[succeed == 1]) 
  revised_missioninfo_dt[missionID == target_missionID]$N_fail = nrow(result_dt[succeed == 0])
  
  revised_missioninfo_dt[missionID == target_missionID]$reductionRate_control <- 1 - sum(result_dt[isControl == T]$load_mission)/sum(result_dt[isControl == T]$load_CBL)
  revised_missioninfo_dt[missionID == target_missionID]$reductionRate_valid <- 1 - sum(result_dt[isControl == F]$load_mission)/sum(result_dt[isControl == F]$load_CBL)
  revised_missioninfo_dt[missionID == target_missionID]$reductionRate_notJoined <- 1 - sum(result_dt[joined == 0]$load_mission)/sum(result_dt[joined == 0]$load_CBL)
  revised_missioninfo_dt[missionID == target_missionID]$reductionRate_joined <- 1 - sum(result_dt[joined == 1]$load_mission)/sum(result_dt[joined == 1]$load_CBL)
  revised_missioninfo_dt[missionID == target_missionID]$reductionRate_succeed <- 1 - sum(result_dt[succeed == 1]$load_mission)/sum(result_dt[succeed == 1]$load_CBL)
  revised_missioninfo_dt[missionID == target_missionID]$reductionRate_fail <- 1 - sum(result_dt[succeed == 0]$load_mission)/sum(result_dt[succeed == 0]$load_CBL)
  
  #add each mission result to each_mission_result_list 
  each_mission_result_list = append(each_mission_result_list, setNames(list(result_dt),target_missionID))
  
}

et = Sys.time()
print(et - st)

write.csv(revised_missioninfo_dt, file = "output/mission_result.csv")
