## 04-SCFigures.R
## Creating figures from the SC results
require(tidyverse)
require(RColorBrewer)

## Import SC Results data:
Interv <- 2023
load(file="int/Player_pool_data.Rda")
load(file="res/SC-Results-Complete.Rda")

## Statistics to analyze:
BStats_Use <- BStats %>% dplyr::filter(Use)

## Overall Plots:
for (statval in BStats$stat) {
  ## Create Spaghetti Plots:
  plot_Spag <- ggplot(data=B.250_pool %>% dplyr::filter(Shift_Cat_2022 != "Medium"),
                      mapping=aes(x=Season, y=get(statval),
                                  group=Player_ID,
                                  alpha=Shift_Cat_2022,
                                  color=Shift_Cat_2022,
                                  linetype=Shift_Cat_2022)) +
    geom_line() + geom_point() +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    scale_y_continuous(name=paste0("Average player ",statval),
                       limits=unlist(BStats[BStats$stat==statval,c("min","max")])) +
    geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
    scale_color_brewer(name="Shift Rate, 2022",
                       type="qual", palette="Dark2",
                       direction=-1,
                       breaks=c("Low","High"),
                       labels=c("\U2264 20%","\U2265 80%")) +
    scale_alpha_manual(name="Shift Rate, 2022",
                       values=c(.5,1),
                       breaks=c("Low","High"),
                       labels=c("\U2264 20%","\U2265 80%")) +
    scale_linetype_manual(name="Shift Rate, 2022",
                          values=c("longdash","solid"),
                          breaks=c("Low","High"),
                          labels=c("\U2264 20%","\U2265 80%")) +
    theme_bw() +
    theme(legend.position="bottom") +
    coord_cartesian(ylim=unlist(BStats[BStats$stat==statval,c("min","max")])) +
    labs(title=paste0(statval," by player, min. 250 PA in each of 2021","\U2013","2023 and listed season"))
  ggsave(filename = paste0("figs/Shift Categories/Spag-plot-",statval,".png"),
         plot=plot_Spag)
  
  ## Create category-average plots:
  plot_CatAvg <- ggplot(data=Player_pool_avg, 
                        mapping=aes(x=Season, y=get(paste0(statval,"_Mean")), 
                                    group=Shift_Cat_2022, 
                                    color=Shift_Cat_2022, alpha=Shift_Cat_2022,
                                    linetype=Shift_Cat_2022)) +
    geom_line() + geom_point(size=2) +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
    scale_color_manual(name="Shift Rate, 2022",
                       values=brewer.pal(3, "Dark2")[c(1,3,2)],
                       breaks=c("Low","Medium","High"),
                       labels=c("\U2264 20%",paste0("20","\U2013","80%"),"\U2265 80%")) +
    scale_alpha_manual(name="Shift Rate, 2022",
                       values=c(0.5,0.3,1),
                       breaks=c("Low","Medium","High"),
                       labels=c("\U2264 20%",paste0("20","\U2013","80%"),"\U2265 80%")) +
    scale_linetype_manual(name="Shift Rate, 2022",
                          values=c("longdash","dotdash","solid"),
                          breaks=c("Low","Medium","High"),
                          labels=c("\U2264 20%",paste0("20","\U2013","80%"),"\U2265 80%")) +
    theme_bw() +
    theme(legend.position="bottom") +
    labs(title=paste0("Average ",statval," among players with min. 250 PA in each of 2021","\U2013",
                      "2023 and listed season"),
         y=paste0("Average player ",statval))
  plot_CatAvg_fixedscale <- plot_CatAvg +
    scale_y_continuous(name=paste0("Average player ",statval),
                       limits=unlist(BStats[BStats$stat==statval,c("min","max")])) +
    coord_cartesian(ylim=unlist(BStats[BStats$stat==statval,c("min","max")]))
  
  ggsave(filename = paste0("figs/Shift Categories/CatAvg-free-plot-",statval,".png"),
         plot=plot_CatAvg)
  ggsave(filename = paste0("figs/Shift Categories/CatAvg-fixed-plot-",statval,".png"),
         plot=plot_CatAvg_fixedscale)
  
  ## Create all-player SC plots:
  plot_SC <- ggplot(data=SCs_Results %>% dplyr::filter(Outcome==statval),
                    mapping=aes(x=Season, y=Diff, group=Player_ID,
                                color=Placebo_Unit, alpha=Placebo_Unit,
                                linetype=Placebo_Unit)) +
    geom_line() +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    scale_y_continuous(name="Difference, Synthetic - Observed",
                       limits=unlist(BStats[BStats$stat==statval,c("diff_min","diff_max")])) +
    coord_cartesian(ylim=unlist(BStats[BStats$stat==statval,c("diff_min","diff_max")])) +
    scale_color_brewer(name="",
                       type="qual", palette="Dark2",
                       direction=-1,
                       breaks=c(FALSE,TRUE),
                       labels=c("Intervention","Placebo")) +
    scale_alpha_manual(name="",
                       breaks=c(FALSE,TRUE),
                       labels=c("Intervention","Placebo"),
                       values=c(1,0.5)) +
    scale_linetype_manual(name="",
                          breaks=c(FALSE,TRUE),
                          labels=c("Intervention","Placebo"),
                          values=c("solid","longdash")) +
    geom_vline(xintercept=Interv-0.5,
               color="grey50", linetype="dashed") +
    theme_bw() + theme(legend.position="bottom") +
    labs(title=paste0("SCM estimates for ",statval," by player, ","\U2265",
                      "250 PA each season, 2015","\U2013","2023"))
  ggsave(filename=paste0("figs/SC Estimates/SC-plot-",statval,".png"),
         plot=plot_SC)
}

## Player-Specific Plots:
for (ID in Player_pool %>% dplyr::filter(Shift_Cat_2022=="High") %>% pull(Player_ID)) {
  ## Info on target player:
  Row <- Player_pool[Player_pool$Player_ID==ID,]
  Disp_name <- Row$Name_Disp
  print(paste0("Beginning figures for ",Disp_name))
  Seasons <- unique(SCs_Results %>% dplyr::filter(Player_ID==ID) %>% pull(Season))
  
  ## Create SC Plots for target player:
  for (statval in BStats_Use$stat) {
    ## Get data for player & placebos for that outcome only:
    SC_data <- SCs_Results %>% 
      dplyr::filter(Outcome==statval & 
                      (Player_ID==ID | Placebo_Unit) & 
                      (Season %in% Seasons))
    
    ## Spaghetti Plot:
    plot_player_spa <- ggplot(SC_data, 
                              mapping=aes(x=Season, y=Observed, 
                                          group=Player_ID,
                                          color=Placebo_Unit, alpha=Placebo_Unit, 
                                          linetype=Placebo_Unit)) +
      geom_line(linewidth=1.2) + geom_point() +
      scale_x_continuous(name="Season",
                         breaks=2015:2023,
                         minor_breaks=NULL) +
      scale_y_continuous(name=statval,
                         limits=unlist(BStats_Use[BStats_Use$stat==statval,c("min","max")])) +
      geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
      scale_color_brewer(name="Player Type",
                         type="qual", palette="Dark2",
                         direction=-1,
                         breaks=c(FALSE,TRUE),
                         labels=c(paste0("Target Player: ",Disp_name),
                                  "Control Players (2022 Shift Rate \U2264 20%)")) +
      scale_alpha_manual(name="Player Type",
                         values=c(1,0.5),
                         breaks=c(FALSE,TRUE),
                         labels=c(paste0("Target Player: ",Disp_name),
                                  "Control Players (2022 Shift Rate \U2264 20%)")) +
      scale_linetype_manual(name="Player Type",
                            values=c("solid","longdash"),
                            breaks=c(FALSE,TRUE),
                            labels=c(paste0("Target Player: ",Disp_name),
                                     "Control Players (2022 Shift Rate \U2264 20%)")) +
      theme_bw() + theme(legend.position="bottom") +
      coord_cartesian(ylim=unlist(BStats_Use[BStats_Use$stat==statval,c("min","max")])) +
      labs(title=paste0(statval," by player and season for ",Disp_name," and controls"))
    
    ggsave(filename = paste0("figs/Players/",Disp_name,"/Spaghetti-",statval,".png"),
           plot=plot_player_spa)
    
    ## Trend Plot:
    TrendPlot <- ggplot(data=SC_data %>% dplyr::filter(Player_ID==ID) %>%
                          dplyr::select(-c("Diff")) %>% 
                          pivot_longer(cols=c("Observed","Synthetic"), 
                                       names_to="Result", values_to="Value"), 
                        mapping=aes(x=Season, y=Value, 
                                    group=Result, linetype=Result, color=Result)) +
      geom_line(linewidth=1.2) + geom_point(shape=17, size=2.2) +
      theme_bw() + theme(legend.position="bottom") +
      scale_color_manual(name=NULL,
                         values=c("#002d72","#ff5910"),
                         breaks=c("Observed","Synthetic")) +
      scale_linetype_manual(name=NULL,
                            values=c("solid","dotted"),
                            breaks=c("Observed","Synthetic")) +
      scale_x_continuous(name="Season",
                         breaks=2015:2023,
                         minor_breaks=NULL) +
      scale_y_continuous(name=statval,
                         limits=unlist(BStats[BStats$stat==statval,c("min","max")])) +
      coord_cartesian(ylim=unlist(BStats[BStats$stat==statval,c("min","max")])) +
      geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
      labs(title=paste0("Synthetic and observed ",statval," for ",Disp_name))
    ggsave(filename=paste0("figs/Players/",Disp_name,"/Trend-",statval,".png"),
           plot=TrendPlot)
    
    ## Differences (SCM estimates) plot:
    plot_diff <- ggplot(data=SC_data,
                        mapping=aes(x=Season, y=Diff, group=Player_ID,
                                    color=Placebo_Unit, alpha=Placebo_Unit,
                                    linetype=Placebo_Unit)) +
      geom_line(linewidth=1.2) +
      scale_x_continuous(name="Season",
                         breaks=2015:2023,
                         minor_breaks=NULL) +
      scale_y_continuous(name="Difference, Synthetic - Observed",
                         limits=unlist(BStats[BStats$stat==statval,c("diff_min","diff_max")])) +
      coord_cartesian(ylim=unlist(BStats[BStats$stat==statval,c("diff_min","diff_max")])) +
      scale_color_brewer(name="",
                         type="qual", palette="Dark2",
                         direction=-1,
                         breaks=c(FALSE,TRUE),
                         labels=c(paste0("Target Player: ",Disp_name),
                                  "Placebo Players (2022 Shift Rate \U2264 20%)")) +
      scale_alpha_manual(name="",
                         breaks=c(FALSE,TRUE),
                         labels=c(paste0("Target Player: ",Disp_name),
                                  "Placebo Players (2022 Shift Rate \U2264 20%)"),
                         values=c(1,0.5)) +
      scale_linetype_manual(name="",
                            breaks=c(FALSE,TRUE),
                            labels=c(paste0("Target Player: ",Disp_name),
                                     "Placebo Players (2022 Shift Rate \U2264 20%)"),
                            values=c("solid","longdash")) +
      geom_vline(xintercept=Interv-0.5,
                 color="grey50", linetype="dashed") +
      theme_bw() + theme(legend.position="bottom") +
      labs(title=paste0("SCM estimates for ",statval," for ",Disp_name," and placebos"))
    ggsave(filename=paste0("figs/Players/",Disp_name,"/Diff-",statval,".png"),
           plot=plot_diff)
  }
}