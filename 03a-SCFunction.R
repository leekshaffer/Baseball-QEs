## 03a-SCFunction.R
## Sensitivity analysis function

library(tidyverse)
library(tidysynth)

Interv <- 2023

## Create summary data set for players with which seasons they had >= 250PA (50 for 2020), 
### and their Shift Categories: 
load(file="int/Sav_data.Rda")
S_cols_full <- paste0("S_",15:24)
Players_seasons <- B.250 %>% group_by(Name,Player_ID,Name_Match,Name_Disp) %>%
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

Shift_2022_Names <- Sav.Shifts %>% 
  dplyr::filter(Season==2022) %>%
  pull(Name_Match)
Shift_summ <- Sav.Shifts %>% 
  dplyr::filter(Name_Match %in% Shift_2022_Names) %>%
  group_by(Name_Match) %>%
  dplyr::summarize(Shift_Perc_2022=PA_Shift_Percent[Season==2022],
                   Shift_Perc_Min=min(PA_Shift_Percent, na.rm=TRUE),
                   Shift_Perc_Max=max(PA_Shift_Percent, na.rm=TRUE)) %>%
  ungroup() %>%
  left_join(Sav.Shifts %>% dplyr::filter(Season >= Interv) %>% 
              pivot_wider(id_cols=Name_Match, names_from=Season, 
                          names_prefix="Shade_Perc_",values_from=PA_Shade_Percent),
            by="Name_Match")
  

Create_Pool <- function(Cuts, Cut_Var, S_cols_all) {
  Shift_summ_cat <- Shift_summ %>%
    dplyr::mutate(Shift_Cat=if_else(get(Cut_Var) <= Cuts[1], "Low",
                                    if_else(get(Cut_Var) >= Cuts[2], 
                                            "High", "Medium")))
  
  Players_seasons %>% 
    dplyr::filter(grepl(paste("S_",S_cols_all, sep="", collapse=".*"), Seasons_Dat)) %>%
    left_join(Shift_summ_cat, by="Name_Match")
}

Run_SC <- function(Pool, S_cols_all, S_cols_check=15:19, 
                   Res_Yrs=Interv, outname,
                   Player_Weight_Plots=TRUE,
                   Stats=c("OBP","OPS","wOBA")) { 
  ## Function to run it for different pools of players
  ## Make sure S_cols_all matches restrictions on pool and S_cols_check is in order of data set
  SCs_Full <- NULL ## Clear any previous saved info
  for (ID in Pool %>% dplyr::filter(Shift_Cat=="High") %>% pull(Player_ID)) {
    ## Reset data sets:
    Weights_Unit <- NULL
    Weights_Pred <- NULL
    BalTbl <- NULL
    SCs <- NULL
    MSPE <- NULL
    
    ## Info on target player:
    Row <- Pool[Pool$Player_ID==ID,]
    Disp_name <- Row$Name_Disp
    print(paste0("Beginning analysis for ",Disp_name))
    ## Find their seasons with >= 250 PA:
    Seasons_check <- (S_cols_check)[unlist(Row[1,paste0("S_",S_cols_check)])]
    
    ## Get control players with at least those seasons >= 250 PA:
    Player_donor <- Pool %>% dplyr::filter(Shift_Cat=="Low") %>% 
      dplyr::filter(grepl(paste("S_",Seasons_check, sep="", collapse=".*"),Seasons_Dat))
    N_donor <- dim(Player_donor)[1]
    while (N_donor < 10) { ## Cut off a year if less than 10 donor units
      Seasons_check <- Seasons_check[2:length(Seasons_check)]
      Player_donor <- Pool %>% dplyr::filter(Shift_Cat=="Low") %>% 
        dplyr::filter(grepl(paste(Seasons_check, sep="", collapse=".*"),Seasons_Dat))
      N_donor <- dim(Player_donor)[1]
    }
    
    ## Create data set for SC:
    SC_data <- B.250 %>% 
      dplyr::filter(Season %in% as.numeric(paste0("20",c(Seasons_check,S_cols_all)))) %>%
      dplyr::filter(Player_ID %in% c(ID,Player_donor$Player_ID)) %>%
      dplyr::mutate(Type=if_else(Player_ID==ID,"Target","Donor"))
    
    ## Run the SC for each outcome:
    for (statval in Stats) {
      ## Set up SC:
      synth_player <- SC_data %>%
        synthetic_control(outcome={statval},
                          unit=Name_Disp,
                          time=Season,
                          i_unit=Disp_name,
                          i_time=Interv,
                          generate_placebos=F) %>%
        generate_predictor(time_window=2021,
                           val2021=get(statval),
                           PA2021=PA,
                           H2021=H,
                           Singles2021=`1B`,
                           HR2021=HR,
                           BBPerc2021=`BB percent`,
                           KPerc2021=`K percent`) 
      if (2022 %in% Res_Yrs) {
        synth_player <- synth_player %>%
          generate_predictor(time_window=2021,
                             Age2021=Age)
      } else {
        synth_player <- synth_player %>%
          generate_predictor(time_window=2022,
                             Age2022=Age,
                             val2022=get(statval),
                             PA2022=PA,
                             H2022=H,
                             Singles2022=`1B`,
                             HR2022=HR,
                             BBPerc2022=`BB percent`,
                             KPerc2022=`K percent`)
      }
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
        generate_weights(optimization_window=as.numeric(paste0("20",c(Seasons_check,S_cols_all)))[as.numeric(paste0("20",c(Seasons_check,S_cols_all))) < min(Res_Yrs)],
                         margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6)
      for (index in 1:length(synth_player$`.meta`)) {
        synth_player$`.meta`[[index]]$outcome <- statval
      }
      synth_player <- synth_player %>%
        generate_control()
      
      if (Player_Weight_Plots) {
        WtPlot <- synth_player %>% plot_weights() + theme_bw()
        ggsave(filename=paste0("figs/Players-",outname,"/",Disp_name,"/Weights-",statval,".png"),
               plot=WtPlot)
      }
      
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
                                               Intervention=time_unit >= min(Res_Yrs)) %>% 
                                 dplyr::rename(Season=time_unit, Observed=real_y,
                                               Synthetic=synth_y) %>%
                                 dplyr::select(Outcome,Season,Intervention,
                                               Observed,Synthetic,Diff))
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
         file=paste0("res/Players-",outname,"/Player-SC-",Disp_name,".Rda"))
    SCs_Full <- SCs_Full %>% 
      bind_rows(SCs %>% mutate(Name=Row$Name, Name_Disp=Disp_name, Player_ID=ID))
  }
  
  ### Get summary results across target players:
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
  
  for (ID in Pool %>% dplyr::filter(Shift_Cat=="Low") %>% pull(Player_ID))  {
    SCs <- NULL
    
    ## Info on target player:
    Row <- Pool[Pool$Player_ID==ID,]
    Disp_name <- Row$Name_Disp
    print(paste0("Beginning placebo test for ",Disp_name))
    ## Find their seasons with >= 250 PA:
    Seasons_check <- (S_cols_check)[unlist(Row[1,paste0("S_",S_cols_check)])]
    
    ## Get control players with at least those seasons >= 250 PA:
    Player_donor <- Pool %>% dplyr::filter(Shift_Cat=="Low") %>% 
      dplyr::filter(Player_ID != ID,
                    grepl(paste("S_",Seasons_check, sep="", collapse=".*"),Seasons_Dat))
    N_donor <- dim(Player_donor)[1]
    while (N_donor < 10) { ## Cut off a year if less than 10 donor units
      Seasons_check <- Seasons_check[2:length(Seasons_check)]
      Player_donor <- Pool %>% dplyr::filter(Shift_Cat=="Low") %>% 
        dplyr::filter(Player_ID != ID,
                      grepl(paste(Seasons_check, sep="", collapse=".*"),Seasons_Dat))
      N_donor <- dim(Player_donor)[1]
    }
    
    ## Create data set for SC:
    SC_data <- B.250 %>% 
      dplyr::filter(Season %in% as.numeric(paste0("20",c(Seasons_check,S_cols_all)))) %>%
      dplyr::filter(Player_ID %in% c(ID,Player_donor$Player_ID)) %>%
      dplyr::mutate(Type=if_else(Player_ID==ID,"Target","Donor"))
    
    for (statval in Stats) {
      ## Set up SC:
      synth_player <- SC_data %>%
        synthetic_control(outcome={statval},
                          unit=Name_Disp,
                          time=Season,
                          i_unit=Disp_name,
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
                           Age2022=Age,
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
                                               Intervention=time_unit >= min(Res_Yrs)))
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
  
  ### Save intermediate results data:
  # save(list=c("SCs_Full", "SCs_Plac", "MSPEs_Full"),
  #      file=paste0("res/",outname,"-Intermediate.Rda"))
  
  
  ### Get summary results with placebo estimates:
  MSPEs_Plac <- SCs_Plac %>% group_by(Placebo_Name,Placebo_ID,Placebo_Disp,Outcome,Intervention) %>%
    dplyr::summarize(MSPE=mean(Diff^2)) %>%
    ungroup() %>%
    dplyr::mutate(Type=if_else(Intervention,"Post","Pre")) %>%
    pivot_wider(id_cols=-c("Intervention"),
                names_from=Type,
                values_from=MSPE) %>%
    dplyr::mutate(Ratio=Post/Pre)
  
  SCs_Results <- SCs_Full %>% dplyr::mutate(Placebo_Unit=FALSE) %>%
    bind_rows(SCs_Plac %>% dplyr::rename(Name=Placebo_Name, Player_ID=Placebo_ID, 
                                         Name_Disp=Placebo_Disp))
  
  ### Get placebo test results (MSPEs, P-Values):
  MSPEs_PRes <- MSPEs_Plac %>% 
    left_join(SCs_Plac %>% dplyr::filter(Intervention) %>% 
                dplyr::select(Placebo_ID,Outcome,Season,Diff) %>%
                pivot_wider(values_from=Diff, names_from=Season,
                            names_prefix="Diff_"),
              by=c("Placebo_ID","Outcome")) %>% rowwise() %>%
    dplyr::mutate(Diff_Total=sum(c_across(paste0("Diff_",Res_Yrs))))
  MSPEs_Res <- MSPEs_Full %>% 
    left_join(SCs_Full %>% dplyr::filter(Intervention) %>% 
                dplyr::select(Player_ID,Outcome,Season,Diff) %>%
                pivot_wider(values_from=Diff, names_from=Season,
                            names_prefix="Diff_"),
              by=c("Player_ID","Outcome")) %>% rowwise() %>%
    dplyr::mutate(Diff_Total=sum(c_across(paste0("Diff_",Res_Yrs))))
  
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
  
  MSPEs_Results <- MSPEs_Res %>%
    bind_cols(t(apply(MSPEs_Res , 1,
                      FUN=function(x) PVals(x, PlacData=MSPEs_PRes, ColName="Diff_Total"))))
  
  ### Save results data:
  save(list=c("MSPEs_PRes", "SCs_Results","MSPEs_Results"),
       file=paste0("res/",outname,"-Results-Complete.Rda"))
  
  return(SCs_Results)
}