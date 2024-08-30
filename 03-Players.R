## 03-Players.R
## Created: August 29, 2024
## Author: Lee Kennedy-Shaffer, PhD

require(tidyverse)
require(tidysynth)

## Hitting stats to consider:
BStats <- c("AVG","BABIP","K percent","OBP","SLG","OPS","wOBA")

## Import Savant data:

load(file="int/Sav_data.Rda")

## Add 2022 Shift Rates & min/max shift rates for all players:
Sav.dat <- Sav.Full  %>% 
  left_join(Sav.Full %>% group_by(Player_ID) %>% 
              dplyr::filter(Season==2022) %>%
              dplyr::select(Player_ID,PA_Shift_Percent) %>%
              dplyr::rename(Shift_Perc_2022=PA_Shift_Percent),
            by="Player_ID") %>%
  left_join(Sav.Full %>% group_by(Player_ID) %>%
              dplyr::summarize(Shift_Perc_Max=max(PA_Shift_Percent, na.rm=TRUE),
                               Shift_Perc_Min=min(PA_Shift_Percent, na.rm=TRUE)),
            by="Player_ID") %>%
  dplyr::mutate(Shift_Perc_Max=if_else(is.infinite(Shift_Perc_Max),NA,Shift_Perc_Max),
                Shift_Perc_Min=if_else(is.infinite(Shift_Perc_Min),NA,Shift_Perc_Min))

## Get list of players with >= 250 PA in desired seasons:
Seasons_use <- c(2017:2019,2021:2023)
Players_use <- B.250 %>% dplyr::filter(Season %in% Seasons_use) %>%
  group_by(Name,Name_Match,Player_ID) %>% 
  dplyr::summarize(n_seasons_use=n()) %>%
  dplyr::filter(n_seasons_use == length(Seasons_use)) %>%
  dplyr::select(!n_seasons_use)

## Create analysis data set and data summarized by shift rate category:
Cuts <- c(20,80)
Use_Stats <- Sav.dat %>% dplyr::filter(Player_ID %in% Players_use$Player_ID,
                                       Season %in% Seasons_use) %>%
  dplyr::mutate(Shift_Cat_2022=if_else(Shift_Perc_2022 >= Cuts[2],"High",
                                      if_else(Shift_Perc_2022 <= Cuts[1],"Low","Medium")),
                Shift_Cat_Max=if_else(Shift_Perc_Max >= Cuts[2],"High",
                                     if_else(Shift_Perc_Max <= Cuts[1],"Low","Medium")),
                Shift_Cat_Min=if_else(Shift_Perc_Min >= Cuts[2],"High",
                                      if_else(Shift_Perc_Min <= Cuts[1],"Low","Medium")))
Use_Stats_byCat <- Use_Stats %>% group_by(Season,Shift_Cat_2022) %>%
  dplyr::summarize(across(all_of(BStats), mean, .names="{col}_Mean"))

## Save shift-categorized result data:
save(Use_Stats_byCat, file="tbls/Shift_Category_Averages.Rda")

## Plots for all included players (spaghetti plot) and by shift rate category:
for (val in BStats) {
  ## Spaghetti:
  plot_spa <- ggplot(Use_Stats, mapping=aes(x=Season, y=get(val), group=Player_ID, 
                                            color=Shift_Cat_2022, alpha=Shift_Cat_2022)) +
    geom_line() + geom_point() +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    geom_vline(xintercept=2022.5, color="grey50", linetype="dashed") +
    scale_color_brewer(name="Shift Rate, 2022",
                       type="qual", palette="Dark2",
                       breaks=c("Low","Medium","High"),
                       labels=c("<20%",paste0("20","\U2013","80%"),">80%")) +
    scale_alpha_manual(name="Shift Rate, 2022",
                       values=c(1,0.5,1),
                       breaks=c("Low","Medium","High"),
                       labels=c("<20%",paste0("20","\U2013","80%"),">80%")) +
    theme_bw() +
    labs(title=paste0("Player ",val," by shift rate category, ","\U2265",
                      "250 PA each season, 2017","\U2013","2022"),
         y=val)
  ggsave(filename = paste0("figs/Spaghettis/spaghetti-plot-",val,".png"),
         plot=plot_spa)
  
  ## Summarized By Category:
  plot_CatAvg <- ggplot(Use_Stats_byCat, 
                        mapping=aes(x=Season, y=get(paste0(val,"_Mean")), group=Shift_Cat_2022, 
                                    color=Shift_Cat_2022, alpha=Shift_Cat_2022)) +
    geom_line() + geom_point(size=2) +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    geom_vline(xintercept=2022.5, color="grey50", linetype="dashed") +
    scale_color_brewer(name="Shift Rate, 2022",
                       type="qual", palette="Dark2",
                       breaks=c("Low","Medium","High"),
                       labels=c("<20%",paste0("20","\U2013","80%"),">80%")) +
    scale_alpha_manual(name="Shift Rate, 2022",
                       values=c(1,0.5,1),
                       breaks=c("Low","Medium","High"),
                       labels=c("<20%",paste0("20","\U2013","80%"),">80%")) +
    theme_bw() +
    labs(title=paste0("Average player ",val," by shift rate category, ","\U2265",
                      "250 PA each season, 2017","\U2013","2022"),
         y=paste0("Average player ",val))
  ggsave(filename = paste0("figs/Shift Categories/CatAvg-plot-",val,".png"),
         plot=plot_CatAvg)
}




### New way to do SC:

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
                   S_24=(2024 %in% Season))
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
                                      if_else(Shift_Perc_Min <= Cuts[1],"Low","Medium")))

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

S_cols_check <- 15:19 ## Make sure these are in order of the variables

for (statval in c("wOBA","OBP","OPS")) {
for (ID in Player_interv$Player_ID) {
  ## Info on target player:
  Row <- Player_interv[Player_interv$Player_ID==ID,]
  Disp_name <- Row$Name_Disp
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
  
  ## Spaghetti Plot for Target Player:
  plot_player_spa <- ggplot(SC_data, mapping=aes(x=Season, y=get(statval), group=Player_ID, 
                                                  color=Type, alpha=Type)) +
    geom_line() + geom_point() +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    geom_vline(xintercept=2022.5, color="grey50", linetype="dashed") +
    scale_color_brewer(name="Player Type",
                       type="qual", palette="Dark2",
                       breaks=c("Donor","Target"),
                       labels=c("Control Players (2022 Shift Rate <20%)",
                                paste0("Target Player: ",Disp_name))) +
    scale_alpha_manual(name="Player Type",
                       values=c(0.5,1),
                       breaks=c("Donor","Target"),
                       labels=c("Control Players (2022 Shift Rate <20%)",
                                paste0("Target Player: ",Disp_name))) +
    theme_bw() + theme(legend.position="bottom") +
    labs(title=paste0(statval," by player, ","\U2265",
                      "250 PA each season, 2017","\U2013","2022"),
         y=statval)
  ggsave(filename = paste0("figs/Players/",Disp_name,"/Spaghetti-",statval,".png"),
         plot=plot_player_spa)
  
  ## Set up SC:
  synth_player <- SC_data %>%
    synthetic_control(outcome={statval},
                      unit=Name_Match,
                      time=Season,
                      i_unit=Row$Name_Match,
                      i_time=2023,
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
    generate_weights(optimization_window=as.numeric(paste0("20",c(Seasons_check,S_cols_all)))[as.numeric(paste0("20",c(Seasons_check,S_cols_all))) < 2023],
                     margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6)
  for (index in 1:length(synth_player$`.meta`)) {
    synth_player$`.meta`[[index]]$outcome <- statval
  }
  synth_player <- synth_player %>%
    generate_control()
  
  ## Pull results and save:
  MajorWts <- synth_player %>% grab_unit_weights() %>% 
    dplyr::filter(weight > 0.001) %>% dplyr::arrange(desc(weight)) 
  BalTbl <- synth_player %>% grab_balance_table()
  SCs <- synth_player %>% grab_synthetic_control(placebo=FALSE) %>%
    mutate(diff=real_y-synth_y)
  MSPE <- c(Pre=mean((SCs %>% dplyr::filter(time_unit < 2023) %>% pull(diff))^2),
            Post=mean((SCs %>% dplyr::filter(time_unit >= 2023) %>% pull(diff))^2))
  MSPE <- c(MSPE, Ratio=MSPE["Post"]/MSPE["Pre"])
  
  save(list=c("MajorWts","BalTbl","SCs","MSPE"),
       file=paste0("int/Player-SC-",Disp_name,".Rda"))
  
  WtPlot <- synth_player %>% plot_weights() + theme_bw()
  TrendPlot <- synth_player %>% plot_trends()
  TrendPlot$layers[[1]] <- NULL
  TrendPlot <- TrendPlot + geom_vline(xintercept=2022.5, color="grey50", linetype="dashed") + 
    theme_bw()
  DiffPlot <- synth_player %>% plot_differences()
  DiffPlot$layers[[2]] <- NULL
  DiffPlot$layers[[1]] <- NULL
  DiffPlot <- DiffPlot + geom_hline(yintercept=0, color="grey50", linetype="dashed") + 
    geom_vline(xintercept=2022.5, color="grey50", linetype="dashed") +
    theme_bw()
  
  ggsave(filename=paste0("figs/Players/",Disp_name,"/Weights-",statval,".png"),
         plot=WtPlot)
  ggsave(filename=paste0("figs/Players/",Disp_name,"/Trend-",statval,".png"),
         plot=TrendPlot)
  ggsave(filename=paste0("figs/Players/",Disp_name,"/Diff-",statval,".png"),
         plot=DiffPlot)
}
}
  