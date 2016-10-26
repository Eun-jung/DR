
list_missionId = unique(mission_dt$missionId) #[1] 138 144 156 159 162 166 167 171

each_mission_result_list = list()

st = Sys.time()
for(target_missionId in list_missionId){
  
  MISSIONDAY = revised_missioninfo_dt[missionId==target_missionId]$day
  MISSIONSTARTTIME = revised_missioninfo_dt[missionId==target_missionId]$start_time
  MISSIONENDTIME = revised_missioninfo_dt[missionId==target_missionId]$end_time
  
  raw_target_missionDT = mission_dt[missionId == target_missionId]
  target_missionDT = raw_target_missionDT[reason != "DISCONNECTED" | is.na(reason)] #total 
  
  ## set default duration to get recent 10 days
  DEFAULT_START = revised_missioninfo_dt[missionId == target_missionId]$day - 31
  DEFAULT_END = revised_missioninfo_dt[missionId == target_missionId]$day #I wrote this term to know the effect after the mission but this work left undone(DEFAULT_END is still mission day)
  
  target_raw = data_15min[siteId %in% target_missionDT$siteId & day >= DEFAULT_START & day <= DEFAULT_END]
  # target_raw = data_15min[siteId %in% target_missionDT$siteId & day >= DEFAULT_START & day <= DEFAULT_END & time >= MISSIONSTARTTIME & time < MISSIONENDTIME]
  target_raw$isControl <- F ## add 'isControl' column 
  ## get control group who didn't receive the target mission message
  control_raw = data_15min[!(siteId %in% raw_target_missionDT$siteId) & day >= DEFAULT_START & day <= DEFAULT_END]
  control_raw$isControl <- T ## add 'isControl' column 
  
  ## bind mission group data table(target_raw) and control group data table(control_raw)
  target_raw = rbindlist(list(target_raw, control_raw), use.names = T)
  
  ###
  ## get valid siteIds which have 10 valid days before mission day
  ## valid day coditions : the day is working day
  ##                       the day have 4 recorded non-Na 15min-data-rows during mission time
  ###
  
  ## count recorded 15min data rows during mission time
  target_count_per_day_siteId = target_raw[time >= MISSIONSTARTTIME & time < MISSIONENDTIME,
                                           .(NAcount = sum(is.na(unitPeriodUsage)),
                                           rowcount = nrow(.SD)), by=c("day", "siteId")]
  target_count_per_day_siteId = target_count_per_day_siteId[,':='(isValid = (NAcount==0 & rowcount==4))]
  # target_count_per_day_siteId = target_count_per_day_siteId[,':='(isValid = (NAcount==0 & rowcount==96))]
  
  target_raw = merge(target_raw, target_count_per_day_siteId, by=c("day", "siteId"), all.x = T) ## merge existing target_raw data table and target_count_per_day_siteId to know which siteId is valid 
  target_dt = target_raw[isValid == T & workingday == T,
                         .(day = day, time = time, isControl = isControl, unitPeriodUsage = sum(unitPeriodUsage)),
                         by=c("siteId", "timestamp")]
  ## aggregate 15min-data into hourly data
  target_onMission_dt = target_dt[day <= MISSIONDAY & time >= MISSIONSTARTTIME & time < MISSIONENDTIME,
                                  .(isControl = all(isControl), unitPeriodUsage = sum(unitPeriodUsage)),
                                  by=c("day", "siteId")]
  
  
  ## to get 10 days before mission day
  CBL_raw = target_onMission_dt[order(siteId, day) & day < MISSIONDAY, tail(.SD, 10), by="siteId"] ## extract valid last 10 days
  CBL_raw = rbindlist(list(CBL_raw, target_onMission_dt[day == MISSIONDAY]), use.names = T)
  CBL_raw = CBL_raw[, ':='(count = nrow(.SD)), by="siteId"]
  CBL_raw = CBL_raw[count == 11]
#   ##isCBLday; 1 : 6 days(except top 2, bottom 2 days), 0 : left 4 days, -1 : mission day
#   CBL_raw$isCBLday <- -1
#   CBL_raw[day < MISSIONDAY] <- transform(CBL_raw[day < MISSIONDAY], isCBLday = ave(unitPeriodUsage, siteId, FUN = function(x) (rank(-x, ties.method="first")<=2)&rank(-x, ties.method="first")<=8))
  CBL_raw$isMissionday <- F
  CBL_raw[day == MISSIONDAY]$isMissionday <- T
  
  # CBL_dt = CBL_raw[isCBLday != 0, .(unitPeriodUsage = mean(unitPeriodUsage)), by=c("siteId", "isCBLday")] ## aggregate CBL days
  
  ## average last 10 days
  CBL_dt = CBL_raw[, .(unitPeriodUsage = mean(unitPeriodUsage), isControl=all(isControl)), by=c("siteId", "isMissionday")] 
  
  result_dt = data.table(siteId = unique(CBL_dt$siteId))
  # load_CBL_dt = CBL_dt[isCBLday == 1, .(siteId = siteId, load_CBL = unitPeriodUsage)]
  load_CBL_dt = CBL_dt[isMissionday == F, .(siteId = siteId, load_CBL = unitPeriodUsage, isControl = isControl)]
  # load_mission_dt = CBL_dt[isCBLday == -1, .(siteId = siteId, load_mission = unitPeriodUsage)]
  load_mission_dt = CBL_dt[isMissionday == T, .(siteId = siteId, load_mission = unitPeriodUsage)]
  result_dt = merge(result_dt, load_CBL_dt, by="siteId")
  result_dt = merge(result_dt, load_mission_dt, by="siteId")
  
  result_dt = result_dt[load_CBL != 0]
  
  result_dt$reductionRate <- 1 - result_dt$load_mission/result_dt$load_CBL
  
  result_dt = merge(result_dt, target_missionDT, by="siteId", all.x = T)

  revised_missioninfo_dt[missionId == target_missionId]$N_total = nrow(target_missionDT)
  revised_missioninfo_dt[missionId == target_missionId]$N_control = nrow(result_dt[isControl == T])
  revised_missioninfo_dt[missionId == target_missionId]$N_valid = nrow(result_dt[isControl == F])
  revised_missioninfo_dt[missionId == target_missionId]$N_notJoined = nrow(result_dt[joined == 0]) 
  revised_missioninfo_dt[missionId == target_missionId]$N_joined = nrow(result_dt[joined == 1]) 
  revised_missioninfo_dt[missionId == target_missionId]$N_succeed = nrow(result_dt[succeed == 1]) 
  revised_missioninfo_dt[missionId == target_missionId]$N_fail = nrow(result_dt[succeed == 0])
  
  revised_missioninfo_dt[missionId == target_missionId]$reductionRate_control <- 1 - sum(result_dt[isControl == T]$load_mission)/sum(result_dt[isControl == T]$load_CBL)
  revised_missioninfo_dt[missionId == target_missionId]$reductionRate_valid <- 1 - sum(result_dt[isControl == F]$load_mission)/sum(result_dt[isControl == F]$load_CBL)
  revised_missioninfo_dt[missionId == target_missionId]$reductionRate_notJoined <- 1 - sum(result_dt[joined == 0]$load_mission)/sum(result_dt[joined == 0]$load_CBL)
  revised_missioninfo_dt[missionId == target_missionId]$reductionRate_joined <- 1 - sum(result_dt[joined == 1]$load_mission)/sum(result_dt[joined == 1]$load_CBL)
  revised_missioninfo_dt[missionId == target_missionId]$reductionRate_succeed <- 1 - sum(result_dt[succeed == 1]$load_mission)/sum(result_dt[succeed == 1]$load_CBL)
  revised_missioninfo_dt[missionId == target_missionId]$reductionRate_fail <- 1 - sum(result_dt[succeed == 0]$load_mission)/sum(result_dt[succeed == 0]$load_CBL)
  
  #add each mission result to each_mission_result_list 
  each_mission_result_list = append(each_mission_result_list, setNames(list(result_dt),target_missionId))
  
}

et = Sys.time()
print(et - st)

# write.csv(revised_missioninfo_dt, file = "output/mission_result.csv")
