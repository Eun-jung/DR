


for (i in 1:length(each_mission_result_list)){
  plot_dt = each_mission_result_list[i][[1]]
  plot_name = names(each_mission_result_list[i])
  
  mission_plot <- ggplot(plot_dt, aes(x=siteId)) +
    ggtitle(paste(revised_missioninfo_dt[missionId == plot_name]$start_timestamp,"~",revised_missioninfo_dt[missionId == plot_name]$end_timestamp)) +
    geom_point(data = plot_dt[isControl == T], aes(y=reductionRate*100, color="control"), alpha = 0.3) + 
    geom_point(data = plot_dt[joined == 0], aes(y=reductionRate*100, color="not joined"), alpha = 0.3) + 
    geom_point(data = plot_dt[succeed == 1], aes(y=reductionRate*100, color="succeed"), alpha = 0.3) + 
    geom_point(data = plot_dt[succeed == 0], aes(y=reductionRate*100, color="fail"), alpha = 0.3) + 
    geom_hline(aes(yintercept = revised_missioninfo_dt[missionId == plot_name]$reductionRate_control*100, color="control")) + 
    geom_hline(aes(yintercept = revised_missioninfo_dt[missionId == plot_name]$reductionRate_notJoined*100, color="not joined")) +
    geom_hline(aes(yintercept = revised_missioninfo_dt[missionId == plot_name]$reductionRate_succeed*100, color="succeed")) +
    geom_hline(aes(yintercept = revised_missioninfo_dt[missionId == plot_name]$reductionRate_fail*100, color="fail")) +
    ylab("redunction rate(%)")
  
  ggsave(paste0("plot/reductionRate/",plot_name,".png"), width = 16, height = 12, dpi = 300, mission_plot, limitsize=FALSE)
}


for (i in 1:length(each_mission_result_list)){
  plot_dt = each_mission_result_list[i][[1]]
  plot_name = names(each_mission_result_list[i])
  
  mission_plot <- ggplot(plot_dt, aes(x=siteId)) +
    ggtitle(paste(revised_missioninfo_dt[missionId == plot_name]$start_timestamp,"~",revised_missioninfo_dt[missionId == plot_name]$end_timestamp)) +
    # geom_point(data = plot_dt[isControl == T], aes(y=reductionRate*100, color="control"), alpha = 0.3) + 
    geom_point(data = plot_dt[joined == 0], aes(y=reductionRate*100, color="not joined"), alpha = 0.3) + 
    geom_point(data = plot_dt[succeed == 1], aes(y=reductionRate*100, color="succeed"), alpha = 0.3) + 
    geom_point(data = plot_dt[succeed == 0], aes(y=reductionRate*100, color="fail"), alpha = 0.3) + 
    # geom_hline(aes(yintercept = revised_missioninfo_dt[missionId == plot_name]$reductionRate_control*100, color="control")) + 
    geom_hline(aes(yintercept = revised_missioninfo_dt[missionId == plot_name]$reductionRate_notJoined*100, color="not joined")) +
    geom_hline(aes(yintercept = revised_missioninfo_dt[missionId == plot_name]$reductionRate_succeed*100, color="succeed")) +
    geom_hline(aes(yintercept = revised_missioninfo_dt[missionId == plot_name]$reductionRate_fail*100, color="fail")) +
    ylab("redunction rate(%)")
  
  ggsave(paste0("plot/reductionRate/",plot_name,"_wo_control.png"), width = 16, height = 12, dpi = 300, mission_plot, limitsize=FALSE)
}


for (i in 1:length(each_mission_result_list)){
  print(names(each_mission_result_list[i]))
  print(summary(each_mission_result_list[i][[1]][isControl == T]$reductionRate))
  print(summary(each_mission_result_list[i][[1]][joined == 0]$reductionRate))
  print(summary(each_mission_result_list[i][[1]][succeed == 1]$reductionRate))
  print(summary(each_mission_result_list[i][[1]][succeed == 0]$reductionRate))
}
