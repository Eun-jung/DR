###
### functions for table and plot
###

get.15min.label.dt <- function(label_day){
  
  label_day = as.Date(label_day, tz="Asia/Seoul")
  
  labels <- seq(
    from = as.POSIXct(paste(label_day, "00:00"), format="%Y-%m-%d %H:%M", tz="Asia/Seoul"),
    to = as.POSIXct(paste(label_day, "23:45"), format="%Y-%m-%d %H:%M", tz="Asia/Seoul"),
    by="15 min"
  )
  
  labels_dt = data.table(timestamp = labels)
  
  return(labels_dt)
}

add.NA.vline <- function(plot_body, plot_dt){
  p <- plot_body + 
    geom_vline(xintercept = c(plot_dt[, .I[unitPeriodUsage == -99]]), color="red") 
  
  return(p)
}

readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
  
  return(line)
}

get.list.for.plot <- function(input_data, days = NULL, siteIDs = NULL){
  
  st = Sys.time()
  
  if(is.null(days)){
    uniqueDays =  unique(as.Date(input_data$timestamp, tz="Asia/Seoul"))
  } else{
    uniqueDays =  unique(as.Date(days, tz="Asia/Seoul"))
  }
  
  if(is.null(siteIDs)){
    uniqueSites =  unique(input_data$siteID)
  } else{
    uniqueSites =  unique(as.integer(siteIDs))
  }
  
  data_list = list()
  
  for(d in uniqueDays){
    for(s in uniqueSites){
      
      oneDay_dt = input_data[siteID == s & day == d,
                             '.'(timestamp = timestamp,
                                 deviceID = deviceID,
                                 unitPeriodUsage = unitPeriodUsage,
                                 index = .I)]
      
      uniqueDevices = unique(oneDay_dt$deviceID)
      if(length(uniqueDevices)==1){
        uniqueDevices <- 0 #0000 : All devices
      }else{
        uniqueDevices <- append(uniqueDevices, 0) #0000 : All devices
      }
      
      for(dv in uniqueDevices){
        if(dv == 0){
          oneDay_oneDevice_dt = oneDay_dt[, ':='(unitPeriodUsage = sum(unitPeriodUsage)), by=index]
          oneDay_oneDevice_dtName = paste(as.Date(d, tz="Asia/Seoul"), s, "All", sep = "_")
        } else{
          oneDay_oneDevice_dt = oneDay_dt[deviceID == dv]
          oneDay_oneDevice_dtName = paste(as.Date(d, tz="Asia/Seoul"), s, dv, sep = "_")
        }
        
        default_timestamp_dt <- get.15min.label.dt(d)
        oneDay_oneDevice_dt = merge(default_timestamp_dt, oneDay_oneDevice_dt, by="timestamp", all.x=T)
        oneDay_oneDevice_dt$deviceID <- dv
        
        ## NA -> -99
        oneDay_oneDevice_dt = oneDay_oneDevice_dt[, ':='(unitPeriodUsage = ifelse(is.na(unitPeriodUsage), -99, unitPeriodUsage))]
        
        data_list = append(data_list, setNames(list(oneDay_oneDevice_dt), oneDay_oneDevice_dtName))
      }
    }
  }
  
  et = Sys.time()
  print(et-st)
  
  return(data_list)
}

# plot.iteratively <- function(data_list){
#  
#   for(dt_index in 1:length(data_list)){
#     # plot.new()
#     plot_dt = data_list[dt_index][[1]]
#     plot_dt_name = names(data_list[dt_index])
#     
#     # print(plot_dt)
#     
#     p <- ggplot(data = plot_dt, aes(x = strftime(timestamp, format="%H:%M"))) +
#       ggtitle(plot_dt_name) + 
#       ylim(0, NA) + 
#       geom_point(aes(y = unitPeriodUsage)) +
#       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#       xlab("Time") +
#       ylab("usage(kWh)")
#     
#     p <- add.NA.vline(p, plot_dt)
#     
#     if(nrow(plot_dt)==97){
#       p <- p + geom_vline(xintercept = c(plot_dt[, .I[timestamp == plot_dt[97]$timestamp]]), color="blue") 
#       p <- p + geom_point(aes(x=strftime(plot_dt[97]$timestamp, format="%H:%M"),y=plot_dt[97]$unitPeriodUsage, color="over"))
#     }
#     
#     # p <- make.plot(dt)
#     print(p)
#     
#     input_key = readkey()
#     if(input_key == 'q'){
#       stop("force to quit")
#     }
#     else if(!is.na(input_key)){
#       next
#     }
#     
#   }
# }

plot.iteratively <- function(data_list){
  
  for(dl_index in 1:length(data_list)){
    plot.a.day(data_list[dl_index])
    
    input_key = readkey()
    if(input_key == 'q'){
      stop("force to quit")
    }
    else if(!is.na(input_key)){
      next
    }
    
  }
}


plot.a.day <- function(data_table){

  plot_dt = data_table[[1]]
  plot_dt_name = names(data_table)
  
  # print(plot_dt_name)
  
  p <- ggplot(data = plot_dt, aes(x = strftime(timestamp, format="%H:%M"))) +
    ggtitle(plot_dt_name) + 
    ylim(0, NA) + 
    geom_point(aes(y = unitPeriodUsage)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Time") +
    ylab("usage(kWh)")
  
  p <- add.NA.vline(p, plot_dt)
  
  if(nrow(plot_dt)==97){
    p <- p + geom_vline(xintercept = c(plot_dt[, .I[index == 97]]-1), color="blue") 
    p <- p + geom_point(aes(x=strftime(plot_dt[index == 97]$timestamp, format="%H:%M"),y=plot_dt[index == 97]$unitPeriodUsage, color="over"))
  }

  # p <- make.plot(dt)
  print(p)
  
}


# dt_list<-get.list.for.plot(max_NA)
# plot_iteratively(dt_list)
# 
# tmp_<-raw_15min[siteID == tmp_list[[1]]$siteID & day == tmp_list[[1]]$day]
# tt<-get.list.for.plot(tmp_)
# plot.iteratively(tt)


