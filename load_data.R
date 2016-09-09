library(data.table)
library(bit64)
library(readr)

# install.packages("readr")

raw_data = fread("../data/raw/1462028400000_1473174000000_15min_usages.csv")
#siteid, ?, timestamp, ?

tmp = raw_data

tmp$V3 = as.POSIXct(tmp$V3/1000, origin="1970-01-01 00:00:00")

raw_api_log = fread("../data/raw/log_prod1_0.txt")
# 
# untar("../data/raw/api_log1.tar.gz", files="../data/raw")

log_tmp = read_log("../data/raw/log_prod1_0.txt")
