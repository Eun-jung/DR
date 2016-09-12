## Basic EDA by JY   2016. 9.9 ... just for fun :D

library(data.table)

mission_all = fread("data/raw/uplusMissionAll.csv", na.strings=c("NULL"))
str(mission_all)
# Classes ‘data.table’ and 'data.frame':        32245 obs. of  10 variables:
# $ siteid         : int  10004631 10005282 10005472 10006881 10008389 10008937 10009620 10011017 10011842 10013114 ...
# $ missionId      : int  138 138 138 138 138 138 138 138 138 138 ...
# $ points         : int  1600 1600 1600 1600 1600 1600 1600 1600 1700 1600 ...
# $ baseAmount     : int  500000 375000 290000 387000 235000 336000 229000 449000 557000 240000 ...
# $ goalAmount     : int  438200 321300 252800 344400 204100 281100 187300 388400 449000 203400 ...
# $ actualUsage    : chr  "0" "0" "0" "0" ...
# $ joined         : int  1 1 1 1 1 1 1 1 1 1 ...
# $ disconnectCount: int  8 9 9 4 10 10 10 10 10 10 ...
# $ succeed        : chr  "0" "0" "0" "0" ...
# $ reason         : chr  "DISCONNECTED" "DISCONNECTED" "DISCONNECTED" "DISCONNECTED" ...

# siteid
table(mission_all$siteid)
length(unique(mission_all$siteid)) # 5504 residential

# missionID
missions = table(mission_all$missionId) # 8 missions 
mean(missions) # mean 4030.625 --> 4030.625/ 5504 = about 73% paricipation rate 

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


