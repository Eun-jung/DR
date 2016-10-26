######
###
######


## if it has the "V1" column(row number column), then remove the column
remove.V1.column <- function(dt){
  if(grep("V1", colnames(dt)) > 0){
    dt[, V1:=NULL]
  }
  
  return(dt)
}


## get mission day and start/end time from timestamp column
## make day, start_time, end_time(default start_time + 1 hour) columns 
## remove existing timestamp column
make.day.time.columns <- function(dt, timestampColumnName){
  dt[, ':='(day = as.Date(get(timestampColumnName), tz='Asia/Seoul'),
            start_time = chron(times = strftime(get(timestampColumnName), format="%H:%M:%S")),
            end_time = chron(times = strftime(get(timestampColumnName), format="%H:%M:%S")) + 1/24)]
  dt = dt[, !c(timestampColumnName), with = FALSE]
  
  return(dt)
}


## merge missionHistory and missionResult
get.mission.dt <- function(missionHistory, missionResult){
  missionHistory = remove.V1.column(missionHistory)
  missionResult = remove.V1.column(missionResult)
  
  missionHistory = make.day.time.columns(missionHistory, "missionDate")
  
  missionDT = merge(missionResult, missionHistory, by="missionId", all.x=T)
  
  return(missionDT)
}


## make a default revised mission information data table
get.default.revised.missionInfo.dt <- function(missionHistory){
  missionHistory = remove.V1.column(missionHistory)
  missionHistory = make.day.time.columns(missionHistory, "missionDate")
  
  revised_dt = missionHistory
  revised_dt = revised_dt[, ':='(N_control = 0,
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
  
  return(revised_dt)
}


## merge electricity usage data and mission data
get.whole.dt <- function(usageDT, missionDT){
  wholeDT = merge(usageDT, missionDT, by=c("siteId","day"), all.x=T)
  
  return(wholeDT)
}


# mission_dt_LGU = get.mission.dt(fread("data/raw/missionHistory_LGU.csv"), fread("data/raw/missionResult_LGU.csv"))
# mission_dt_ENCORED = get.mission.dt(fread("data/raw/missionHistory_ENCORED.csv"), fread("data/raw/missionResult_ENCORED.csv"))
# 
# save(mission_dt_LGU, file = "data/raw/mission_dt_LGU_161010.RData")
# save(mission_dt_ENCORED, file = "data/raw/mission_dt_ENCORED_161010.RData")

load("data/raw/mission_dt_LGU_161010.RData")
load("data/raw/mission_dt_ENCORED_161010.RData")
