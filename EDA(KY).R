library(data.table)

mission_all = fread("Data/uplusMissionAll.csv", na.strings=c("NULL"))
str(mission_all) #to see this data's brief structure
names(mission_all)
#[1] "siteid"          "missionId"       "points"          "baseAmount"      "goalAmount"     
#[6] "actualUsage"     "joined"          "disconnectCount" "succeed"         "reason"  

# siteid
table(mission_all$siteid)
length(unique(mission_all$siteid)) # 5504 residential

# missionID
missions = unique(mission_all$missionId) # 8 missions
table(mission_all$missionId) # solicited customers per missions
mean(table(mission_all$missionId)) # mean 4030.625 --> 4030.625/ 5504 = about 73% solicitaion rate

num.joined = matrix(0,8,1)
num.solicited = matrix(0,8,1)
for(i in 1:length(missions)){
  rows <- which(mission_all$missionId==missions[i])
  num.joined[i] <- sum(mission_all$joined[rows])
  num.solicited[i] <- length(rows)
}

#(def)participation rate = joined / solicited
participation_rate <- num.joined/num.solicited
mean(participation_rate) #0.1079391 --> 10.79%


# points ... how calculate the points?
summary(mission_all$points)
table(mission_all$points)
barplot(table(mission_all$points))

# base Amount
summary(mission_all$baseAmount) # 613503.9
boxplot((mission_all$baseAmount))

# goal Amount
mean(mission_all$goalAmount) # 506030.7
range((mission_all$goalAmount)) # 27000 ~ 9072000
boxplot((mission_all$goalAmount))

saving_goals = mission_all$baseAmount - mission_all$goalAmount # --> saving_goals = base - goal 
range(saving_goals) # -228000 ~ 300000
negative_goal_indexes = which(saving_goals < 0)
mission_all[negative_goal_indexes] # --> inappropriate goal setting : 3 cases

# actualUsage
summary(mission_all$actualUsage)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     0  198700  297000  395800  462100 3976000   28724 
28724 / nrow(mission_all) # 89% ... don't know their actual usage... big loss of the information?

# joined
table(mission_all$joined)
# 0     1 
# 28716  3529 


# disconnectCount
table(mission_all$disconnectCount)
#     0     1     2     3     4     5     6     8     9    10 
# 32072    42    12    10     2     2    96     1     2     6 


# succeed
table(mission_all$succeed)
# 0     1   
# 1199  2322    # --> 2322 / (1199+2322) = 65.94% success rate 
sum(is.na(mission_all$succeed)) # 28724 cases ... don't know whether success or not
# 28724 / nrow(mission_all) = 89%

length(which(is.na(mission_all$actualUsage)))      
length(which(is.na(mission_all$succeed)))      
sum(which(is.na(mission_all$actualUsage)) - which(is.na(mission_all$succeed))) # when actualUsage == NA, success == NA

mission_all$goalAmount - mission_all$actualUsage

# reason : why the mission failed(succeed == 0) .. DISCONNECTED/ USAGE_EXCEEDED
summary(mission_all$reason)
table(mission_all$reason)

table(mission_all$succeed[which(mission_all$reason == "")])
table(mission_all$succeed[which(mission_all$reason == "DISCONNECTED")])
table(mission_all$succeed[which(mission_all$reason == "USAGE_EXCEEDED")])
