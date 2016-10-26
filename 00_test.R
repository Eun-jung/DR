source("01_load_data.R")
source("02_make_mission_table.R")

mission_dt = mission_dt_LGU

data_15min = get.whole.dt(raw_15min[group == "LGU"], mission_dt)
revised_missioninfo_dt = get.default.revised.missionInfo.dt(fread("data/raw/missionHistory_LGU.csv"))

source("mission_test.R")
write.csv(revised_missioninfo_dt, file = "output/mission_result_LGU.csv")




mission_dt = mission_dt_ENCORED

data_15min = get.whole.dt(raw_15min[group != "LGU"], mission_dt)
revised_missioninfo_dt = get.default.revised.missionInfo.dt(fread("data/raw/missionHistory_ENCORED.csv"))

source("mission_test.R")
write.csv(revised_missioninfo_dt, file = "output/mission_result_ENCORED.csv")