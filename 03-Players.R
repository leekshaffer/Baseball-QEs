## 03-Players.R
## Analyzing batting stats for high vs. low shifted players
## Using players with at least 250 PA in each of 2021--2023
## and plotted seasons
## All analyses exclude 2020 and 2024
## SC analyses for each target player use as a donor pool
## players with min. 250 PA in all seasons in which target player had min. 250 PA

require(tidyverse)
require(tidysynth)

## Hitting stats to consider:
BStats <- tibble(stat=c("AVG","BABIP","K percent","OBP","SLG","OPS","wOBA"))

## Import Savant data:
load(file="int/Sav_data.Rda")

## Thresholds for high/low Shift rates:
Cuts <- c(20,80)

Interv <- 2023

## Create summary data set for players with which seasons they had >= 250PA (50 for 2020), 
### and their Shift Categories: 
S_cols_full <- paste0("S_",15:24)
Players_seasons <- B.250 %>% group_by(Name,Player_ID,Name_Match) %>%
  dplyr::summarize(S_15=(2015 %in% Season),
                   S_16=(2016 %in% Season),
                   S_17=(2017 %in% Season),
                   S_18=(2018 %in% Season),
                   S_19=(2019 %in% Season),
                   S_20=(2020 %in% Season),
                   S_21=(2021 %in% Season),
                   S_22=(2022 %in% Season),
                   S_23=(2023 %in% Season),
                   S_24=(2024 %in% Season)) %>%
  ungroup()
Players_seasons$Seasons_Dat <- apply(Players_seasons[,S_cols_full], 1, 
                                     function(x) paste(S_cols_full[x], collapse="-"))

Shift_summ <- Sav.Shifts %>% dplyr::filter(Season==2022) %>%
  group_by(Name_Match) %>%
  dplyr::rename(Shift_Perc_2022=PA_Shift_Percent) %>%
  dplyr::select(Name_Match,Shift_Perc_2022) %>%
  left_join(Sav.Shifts %>% group_by(Name_Match) %>%
              dplyr::summarize(Shift_Perc_Max=max(PA_Shift_Percent, na.rm=TRUE),
                               Shift_Perc_Min=min(PA_Shift_Percent, na.rm=TRUE))) %>%
  dplyr::mutate(Shift_Cat_2022=if_else(Shift_Perc_2022 >= Cuts[2],"High",
                                       if_else(Shift_Perc_2022 <= Cuts[1],"Low","Medium")),
                Shift_Cat_Max=if_else(Shift_Perc_Max >= Cuts[2],"High",
                                      if_else(Shift_Perc_Max <= Cuts[1],"Low","Medium")),
                Shift_Cat_Min=if_else(Shift_Perc_Min >= Cuts[2],"High",
                                      if_else(Shift_Perc_Min <= Cuts[1],"Low","Medium"))) %>%
  ungroup()

## Create player pool
### Has minimum season restrictions (now, 2021, 2022, and 2023 meet threshold)
### and creates display name
S_cols_all <- 21:23 ## Make sure these are in order of the variables
Player_pool <- Players_seasons %>% 
  dplyr::filter(grepl(paste("S_",S_cols_all, sep="", collapse=".*"),Seasons_Dat)) %>%
  left_join(Shift_summ, by=join_by(Name_Match)) %>%
  dplyr::mutate(Name_Disp=sub("(^.*),\\s(.*$)","\\2 \\1", Name))

Player_interv <- Player_pool %>% dplyr::filter(Shift_Cat_2022=="High")
Player_ctrl <- Player_pool %>% dplyr::filter(Shift_Cat_2022=="Low")

## League-wide averages by categories (across players w/ >= 250PA):
B.250_pool <- B.250 %>%
  left_join(Player_pool %>% dplyr::select(Name_Match,Shift_Cat_2022), 
                                 by=join_by(Name_Match)) %>%
  dplyr::filter(!is.na(Shift_Cat_2022), PA >= 250, !(Season %in% c(2020,2024)))
  
Player_pool_avg <- B.250_pool %>%
  group_by(Shift_Cat_2022,Season) %>%
  dplyr::summarize(across(all_of(BStats$stat), mean, .names="{col}_Mean")) %>%
  ungroup()

## Save player pool and shift-categorized result data:
save(list=c("Player_pool", "B.250_pool", "Player_pool_avg"),
     file="int/Player_pool_data.Rda")

## Get max and min for consistent plot y-axes:
BStats$min <- sapply(BStats$stat, function(x) min(B.250_pool[B.250_pool$Shift_Cat_2022 != "Medium",x], na.rm=TRUE))
BStats$max <- sapply(BStats$stat, function(x) max(B.250_pool[B.250_pool$Shift_Cat_2022 != "Medium",x], na.rm=TRUE))

for (statval in BStats$stat) {
  plot_Spag <- ggplot(data=B.250_pool %>% dplyr::filter(Shift_Cat_2022 != "Medium"),
                      mapping=aes(x=Season, y=get(statval),
                                  group=Player_ID,
                                  alpha=Shift_Cat_2022,
                                  color=Shift_Cat_2022)) +
    geom_line() + geom_point() +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    scale_y_continuous(name=paste0("Average player ",statval),
                       limits=unlist(BStats[BStats$stat==statval,c("min","max")])) +
    geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
    scale_color_brewer(name="Shift Rate, 2022",
                       type="qual", palette="Dark2",
                       breaks=c("Low","High"),
                       labels=c("\U2264 20%","\U2265 80%")) +
    scale_alpha_manual(name="Shift Rate, 2022",
                       values=c(.5,1),
                       breaks=c("Low","High"),
                       labels=c("\U2264 20%","\U2265 80%")) +
    theme_bw() +
    theme(legend.position="bottom") +
    coord_cartesian(ylim=unlist(BStats[BStats$stat==statval,c("min","max")])) +
    labs(title=paste0(statval," by player, min. 250 PA in each of 2021","\U2013","2023 and listed season"))
  ggsave(filename = paste0("figs/Shift Categories/Spag-plot-",statval,".png"),
         plot=plot_Spag)
    
  plot_CatAvg <- ggplot(data=Player_pool_avg, 
         mapping=aes(x=Season, y=get(paste0(statval,"_Mean")), 
                     group=Shift_Cat_2022, 
                     color=Shift_Cat_2022, alpha=Shift_Cat_2022)) +
    geom_line() + geom_point(size=2) +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
    scale_color_brewer(name="Shift Rate, 2022",
                       type="qual", palette="Dark2",
                       breaks=c("Low","Medium","High"),
                       labels=c("\U2264 20%",paste0("20","\U2013","80%"),"\U2265 80%")) +
    scale_alpha_manual(name="Shift Rate, 2022",
                       values=c(1,0.5,1),
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
}

## Run analyses for all intervention players:
S_cols_check <- 15:19 ## Make sure these are in order of the variables
SCs_Full <- NULL
### Stats to use and the graph parameters:
BStats_Use <- c("wOBA","OBP","OPS")
for (ID in Player_interv$Player_ID) {
    ## Reset data sets:
    Weights_Unit <- NULL
    Weights_Pred <- NULL
    BalTbl <- NULL
    SCs <- NULL
    MSPE <- NULL
  
    ## Info on target player:
    Row <- Player_interv[Player_interv$Player_ID==ID,]
    Disp_name <- Row$Name_Disp
    print(paste0("Beginning analysis for ",Disp_name))
    ## Find their seasons with >= 250 PA:
    Seasons_check <- (S_cols_check)[unlist(Row[1,paste0("S_",S_cols_check)])]
    
    ## Get control players with at least those seasons >= 250 PA:
    Player_donor <- Player_ctrl %>% 
      dplyr::filter(grepl(paste("S_",Seasons_check, sep="", collapse=".*"),Seasons_Dat))
    N_donor <- dim(Player_donor)[1]
    while (N_donor < 10) { ## Cut off a year if less than 10 donor units
      Seasons_check <- Seasons_check[2:length(Seasons_check)]
      Player_donor <- Player_ctrl %>% 
        dplyr::filter(grepl(paste(Seasons_check, sep="", collapse=".*"),Seasons_Dat))
      N_donor <- dim(Player_donor)[1]
    }
    
    ## Create data set for SC:
    SC_data <- B.250 %>% 
      dplyr::filter(Season %in% as.numeric(paste0("20",c(Seasons_check,S_cols_all)))) %>%
      dplyr::filter(Player_ID %in% c(ID,Player_donor$Player_ID)) %>%
      dplyr::mutate(Type=if_else(Player_ID==ID,"Target","Donor"))
    
  for (statval in BStats_Use) {
    ## Spaghetti Plot for Target Player:
    plot_player_spa <- ggplot(SC_data, mapping=aes(x=Season, y=get(statval), group=Player_ID, 
                                                    color=Type, alpha=Type)) +
      geom_line() + geom_point() +
      scale_x_continuous(name="Season",
                         breaks=2015:2023,
                         minor_breaks=NULL) +
      scale_y_continuous(name=statval,
                         limits=unlist(BStats[BStats$stat==statval,c("min","max")])) +
      geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
      scale_color_brewer(name="Player Type",
                         type="qual", palette="Dark2",
                         direction=-1,
                         breaks=c("Target","Donor"),
                         labels=c(paste0("Target Player: ",Disp_name),
                                  "Control Players (2022 Shift Rate \U2264 20%)")) +
      scale_alpha_manual(name="Player Type",
                         values=c(1,0.5),
                         breaks=c("Target","Donor"),
                         labels=c(paste0("Target Player: ",Disp_name),
                                  "Control Players (2022 Shift Rate \U2264 20%)")) +
      theme_bw() + theme(legend.position="bottom") +
      coord_cartesian(ylim=unlist(BStats[BStats$stat==statval,c("min","max")])) +
      labs(title=paste0(statval," by player, ","\U2265",
                        "250 PA each season, 2017","\U2013","2022"))
    ggsave(filename = paste0("figs/Players/",Disp_name,"/Spaghetti-",statval,".png"),
           plot=plot_player_spa)
    
    ## Set up SC:
    synth_player <- SC_data %>%
      synthetic_control(outcome={statval},
                        unit=Name_Match,
                        time=Season,
                        i_unit=Row$Name_Match,
                        i_time=Interv,
                        generate_placebos=F) %>%
      generate_predictor(time_window=2021,
                         val2021=get(statval),
                         PA2021=PA,
                         H2021=H,
                         Singles2021=`1B`,
                         HR2021=HR,
                         BBPerc2021=`BB percent`,
                         KPerc2021=`K percent`) %>%
      generate_predictor(time_window=2022,
                         val2022=get(statval),
                         PA2022=PA,
                         H2022=H,
                         Singles2022=`1B`,
                         HR2022=HR,
                         BBPerc2022=`BB percent`,
                         KPerc2022=`K percent`)
    if (!identical(Seasons_check, integer(0))) {
      if (length(Seasons_check)==1) {
        year <- as.numeric(paste0("20",Seasons_check))
        synth_player <- synth_player %>%
          generate_predictor(time_window=year,
                             valpre2020=get(statval),
                             PApre2020=PA,
                             Hpre2020=H,
                             Singlespre2020=`1B`,
                             HRpre2020=HR,
                             BBPercpre2020=`BB percent`,
                             KPercpre2020=`K percent`)
      } else {
        years <- as.numeric(paste0("20",Seasons_check))
        synth_player <- synth_player %>%
          generate_predictor(time_window=years,
                             PApre2020=mean(PA, na.rm=TRUE),
                             Hpre2020=mean(H, na.rm=TRUE),
                             Singlespre2020=mean(`1B`, na.rm=TRUE),
                             HRpre2020=mean(HR, na.rm=TRUE),
                             BBPercpre2020=mean(`BB percent`, na.rm=TRUE),
                             KPercpre2020=mean(`K percent`, na.rm=TRUE))
        for (year in years) {
          synth_player <- synth_player %>%
            generate_predictor(time_window=year,
                               "val.{year}" := get(statval))
        }
      }
    }
    synth_player <- synth_player %>%
      generate_weights(optimization_window=as.numeric(paste0("20",c(Seasons_check,S_cols_all)))[as.numeric(paste0("20",c(Seasons_check,S_cols_all))) < Interv],
                       margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6)
    for (index in 1:length(synth_player$`.meta`)) {
      synth_player$`.meta`[[index]]$outcome <- statval
    }
    synth_player <- synth_player %>%
      generate_control()
    
    WtPlot <- synth_player %>% plot_weights() + theme_bw()
    # TrendPlot <- synth_player %>% plot_trends()
    # TrendPlot$layers[[1]] <- NULL
    # TrendPlot <- TrendPlot + 
    #   geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") + 
    #   theme_bw() + theme(legend.position="bottom") +
    #   scale_y_continuous(name=statval,
    #                      limits=unlist(BStats[BStats$stat==statval,c("min","max")])) +
    #   coord_cartesian(ylim=unlist(BStats[BStats$stat==statval,c("min","max")]))
    DiffPlot <- synth_player %>% plot_differences()
    DiffPlot$layers[[2]] <- NULL
    DiffPlot$layers[[1]] <- NULL
    DiffPlot <- DiffPlot + geom_hline(yintercept=0, color="grey50", linetype="dashed") + 
      geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
      theme_bw()
    
    ggsave(filename=paste0("figs/Players/",Disp_name,"/Weights-",statval,".png"),
           plot=WtPlot)
    ggsave(filename=paste0("figs/Players/",Disp_name,"/Diff-",statval,".png"),
           plot=DiffPlot)
    
    ## Pull results and save:
    if (is.null(Weights_Unit)) {
      Weights_Unit <- synth_player %>% grab_unit_weights() %>% 
        dplyr::rename("{statval}_weight" := weight)
      Weights_Pred <- synth_player %>% grab_predictor_weights() %>%
        dplyr::rename("{statval}_weight" := weight)
    } else {
      Weights_Unit <- Weights_Unit %>%
        left_join(synth_player %>% grab_unit_weights() %>% 
                    dplyr::rename("{statval}_weight" := weight))
      Weights_Pred <- Weights_Pred %>%
        left_join(synth_player %>% grab_predictor_weights() %>%
                    dplyr::rename("{statval}_weight" := weight))
    }
    BalTbl[[statval]] <- synth_player %>% grab_balance_table()
    SCs <- SCs %>% bind_rows(synth_player %>% grab_synthetic_control(placebo=FALSE) %>%
                               dplyr::mutate(Outcome=statval, 
                                             Diff=real_y-synth_y,
                                             Intervention=time_unit >= Interv) %>% 
                               dplyr::rename(Season=time_unit, Observed=real_y,
                                             Synthetic=synth_y) %>%
                               dplyr::select(Outcome,Season,Intervention,
                                             Observed,Synthetic,Diff))
    
    ## Trend Plot:
    TrendPlot <- ggplot(data=SCs %>% pivot_longer(cols=c("Observed","Synthetic","Diff"), 
                                                  names_to="Result", values_to="Value") %>% 
                          dplyr::filter(Outcome==statval, Result != "Diff"), 
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
      labs(title=paste0("Synthetic and Observed ",statval," for ",Disp_name))
    ggsave(filename=paste0("figs/Players/",Disp_name,"/Trend-",statval,".png"),
           plot=TrendPlot)
  }
    MSPEs <- SCs %>% group_by(Outcome,Intervention) %>%
      dplyr::summarize(MSPE=mean(Diff^2)) %>%
      ungroup() %>%
      dplyr::mutate(Type=if_else(Intervention,"Post","Pre")) %>%
      pivot_wider(id_cols=Outcome,
                  names_from=Type,
                  values_from=MSPE) %>%
      dplyr::mutate(Ratio=Post/Pre)
    save(list=c("Weights_Unit","Weights_Pred","BalTbl","SCs","MSPEs"),
         file=paste0("res/Players/Player-SC-",Disp_name,".Rda"))
    SCs_Full <- SCs_Full %>% 
      bind_rows(SCs %>% mutate(Name=Row$Name, Name_Disp=Disp_name, Player_ID=ID))
}
SCs_Full <- SCs_Full %>% 
       dplyr::select(Name,Player_ID,Outcome,Season,Intervention,Observed,Synthetic,Diff,Name_Disp)
MSPEs_Full <- SCs_Full %>% group_by(Name,Player_ID,Name_Disp,Outcome,Intervention) %>%
  dplyr::summarize(MSPE=mean(Diff^2)) %>%
  ungroup() %>%
  dplyr::mutate(Type=if_else(Intervention,"Post","Pre")) %>%
  pivot_wider(id_cols=-c("Intervention"),
              names_from=Type,
              values_from=MSPE) %>%
  dplyr::mutate(Ratio=Post/Pre)

### Placebo Testing:
SCs_Plac <- NULL

for (ID in Player_ctrl$Player_ID) {
  SCs <- NULL
  
  ## Info on target player:
  Row <- Player_ctrl[Player_ctrl$Player_ID==ID,]
  Disp_name <- Row$Name_Disp
  print(paste0("Beginning placebo test for ",Disp_name))
  ## Find their seasons with >= 250 PA:
  Seasons_check <- (S_cols_check)[unlist(Row[1,paste0("S_",S_cols_check)])]
  
  ## Get control players with at least those seasons >= 250 PA:
  Player_donor <- Player_ctrl %>% 
    dplyr::filter(Player_ID != ID,
                  grepl(paste("S_",Seasons_check, sep="", collapse=".*"),Seasons_Dat))
  N_donor <- dim(Player_donor)[1]
  while (N_donor < 10) { ## Cut off a year if less than 10 donor units
    Seasons_check <- Seasons_check[2:length(Seasons_check)]
    Player_donor <- Player_ctrl %>% 
      dplyr::filter(Player_ID != ID,
                    grepl(paste(Seasons_check, sep="", collapse=".*"),Seasons_Dat))
    N_donor <- dim(Player_donor)[1]
  }
  
  ## Create data set for SC:
  SC_data <- B.250 %>% 
    dplyr::filter(Season %in% as.numeric(paste0("20",c(Seasons_check,S_cols_all)))) %>%
    dplyr::filter(Player_ID %in% c(ID,Player_donor$Player_ID)) %>%
    dplyr::mutate(Type=if_else(Player_ID==ID,"Target","Donor"))
  
  for (statval in BStats_Use) {
    ## Set up SC:
    synth_player <- SC_data %>%
      synthetic_control(outcome={statval},
                        unit=Name_Match,
                        time=Season,
                        i_unit=Row$Name_Match,
                        i_time=Interv,
                        generate_placebos=F) %>%
      generate_predictor(time_window=2021,
                         val2021=get(statval),
                         PA2021=PA,
                         H2021=H,
                         Singles2021=`1B`,
                         HR2021=HR,
                         BBPerc2021=`BB percent`,
                         KPerc2021=`K percent`) %>%
      generate_predictor(time_window=2022,
                         val2022=get(statval),
                         PA2022=PA,
                         H2022=H,
                         Singles2022=`1B`,
                         HR2022=HR,
                         BBPerc2022=`BB percent`,
                         KPerc2022=`K percent`)
    if (!identical(Seasons_check, integer(0))) {
      if (length(Seasons_check)==1) {
        year <- as.numeric(paste0("20",Seasons_check))
        synth_player <- synth_player %>%
          generate_predictor(time_window=year,
                             valpre2020=get(statval),
                             PApre2020=PA,
                             Hpre2020=H,
                             Singlespre2020=`1B`,
                             HRpre2020=HR,
                             BBPercpre2020=`BB percent`,
                             KPercpre2020=`K percent`)
      } else {
        years <- as.numeric(paste0("20",Seasons_check))
        synth_player <- synth_player %>%
          generate_predictor(time_window=years,
                             PApre2020=mean(PA, na.rm=TRUE),
                             Hpre2020=mean(H, na.rm=TRUE),
                             Singlespre2020=mean(`1B`, na.rm=TRUE),
                             HRpre2020=mean(HR, na.rm=TRUE),
                             BBPercpre2020=mean(`BB percent`, na.rm=TRUE),
                             KPercpre2020=mean(`K percent`, na.rm=TRUE))
        for (year in years) {
          synth_player <- synth_player %>%
            generate_predictor(time_window=year,
                               "val.{year}" := get(statval))
        }
      }
    }
    synth_player <- synth_player %>%
      generate_weights(optimization_window=as.numeric(paste0("20",c(Seasons_check,S_cols_all)))[as.numeric(paste0("20",c(Seasons_check,S_cols_all))) < Interv],
                       margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6)
    for (index in 1:length(synth_player$`.meta`)) {
      synth_player$`.meta`[[index]]$outcome <- statval
    }
    synth_player <- synth_player %>%
      generate_control()
    
    ## Pull results and save:
    SCs <- SCs %>% bind_rows(synth_player %>% grab_synthetic_control(placebo=FALSE) %>%
                               dplyr::mutate(Outcome=statval, 
                                             Diff=real_y-synth_y,
                                             Intervention=time_unit >= Interv))
  }
  SCs <- SCs %>% dplyr::rename(Season=time_unit,
                               Observed=real_y,
                               Synthetic=synth_y) %>%
    dplyr::select(Outcome,Season,Intervention,
                  Observed,Synthetic,Diff)
  SCs_Plac <- SCs_Plac %>% 
    bind_rows(SCs %>% mutate(Placebo_Name=Row$Name, Placebo_Disp=Disp_name, 
                             Placebo_ID=ID,
                             Placebo_Unit=TRUE))
}

MSPEs_Plac <- SCs_Plac %>% group_by(Placebo_Name,Placebo_ID,Placebo_Disp,Outcome,Intervention) %>%
  dplyr::summarize(MSPE=mean(Diff^2)) %>%
  ungroup() %>%
  dplyr::mutate(Type=if_else(Intervention,"Post","Pre")) %>%
  pivot_wider(id_cols=-c("Intervention"),
              names_from=Type,
              values_from=MSPE) %>%
  dplyr::mutate(Ratio=Post/Pre)

SCs_Results <- SCs_Full %>% dplyr::mutate(Placebo_Unit=FALSE) %>%
  bind_rows(SCs_Plac %>% dplyr::rename(Name=Placebo_Name, Player_ID=Placebo_ID, Name_Disp=Placebo_Disp))

for (statval in BStats_Use) {
  plot_SC <- ggplot(data=SCs_Results %>% dplyr::filter(Outcome==statval),
                     mapping=aes(x=Season, y=Diff, group=Player_ID,
                                 color=Placebo_Unit, alpha=Placebo_Unit)) +
    geom_line() +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    scale_y_continuous(name="Difference, Synthetic - Observed") +
                       # limits=c(-0.25, 0.25)) +
    scale_color_brewer(name="",
                       type="qual", palette="Dark2",
                       direction=-1,
                       breaks=c(FALSE,TRUE),
                       labels=c("Intervention","Placebo")) +
    scale_alpha_manual(name="",
                       breaks=c(FALSE,TRUE),
                       labels=c("Intervention","Placebo"),
                       values=c(1,0.5)) +
    geom_vline(xintercept=Interv-0.5,
               color="grey50", linetype="dashed") +
    theme_bw() + theme(legend.position="bottom") +
    # coord_cartesian(ylim=c(-0.25, 0.25)) +
    labs(title=paste0("Difference, Synthetic - Observed, for ",statval," by player, ","\U2265",
                      "250 PA each season, 2017","\U2013","2023"))
  ggsave(filename=paste0("figs/SC Estimates/SC-plot-",statval,".png"),
         plot=plot_SC)
}

MSPEs_PRes <- MSPEs_Plac %>% 
  left_join(SCs_Plac %>% dplyr::filter(Intervention) %>% 
              dplyr::select(Placebo_ID,Outcome,Season,Diff) %>%
              pivot_wider(values_from=Diff, names_from=Season,
                          names_prefix="Diff_"),
            by=c("Placebo_ID","Outcome"))
MSPEs_Res <- MSPEs_Full %>% 
  left_join(SCs_Full %>% dplyr::filter(Intervention) %>% 
              dplyr::select(Player_ID,Outcome,Season,Diff) %>%
              pivot_wider(values_from=Diff, names_from=Season,
                          names_prefix="Diff_"),
            by=c("Player_ID","Outcome"))

PVals <- function(row,PlacData,ColName) {
  outcome <- row["Outcome"]
  Pre_MSPE <- as.numeric(row["Pre"])
  Value <- abs(as.numeric(row[ColName]))
  Ratio_Value <- as.numeric(row["Ratio"])
  dat <- PlacData %>% dplyr::filter(Outcome==outcome)
  return(c(PVal=mean(c(unlist(abs(dat[,ColName])),Value) >= Value),
           PVal.ex20=mean(c(unlist(abs(dat[dat$Pre <= 20*Pre_MSPE,ColName])),Value) >= Value),
           PVal.ex5=mean(c(unlist(abs(dat[dat$Pre <= 5*Pre_MSPE,ColName])),Value) >= Value),
           PVal.ex2=mean(c(unlist(abs(dat[dat$Pre <= 2*Pre_MSPE,ColName])),Value) >= Value),
           PVal.ratio=mean(c(dat$Ratio,Ratio_Value) >= Ratio_Value)))
}

psig <- 0.05

MSPEs_Results <- MSPEs_Res %>%
  bind_cols(t(apply(MSPEs_Res, 1,
                  FUN=function(x) PVals(x, PlacData=MSPEs_PRes, ColName="Diff_2023")))) %>%
  mutate(Sig = PVal < psig,
         Sig.ex20 = PVal.ex20 < psig,
         Sig.ex5 = PVal.ex5 < psig,
         Sig.ex2 = PVal.ex2 < psig,
         Sig.ratio = PVal.ratio < psig)

MSPEs_Signif <- MSPEs_Results %>% group_by(Outcome) %>%
  dplyr::summarize(across(starts_with("Sig"),
                          .fns=mean))

save(list=c("MSPEs_PRes", "SCs_Results","MSPEs_Results","MSPEs_Signif"),
     file="res/SC-Results-Complete.Rda")
