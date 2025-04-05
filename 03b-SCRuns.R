## 03b-SCRuns.R
## Analyzing batting stats for high vs. low shifted players
## Using players with at least 250 PA in each of 2021--2023
## and plotted seasons
## All analyses exclude 2020 and 2024
## SC analyses for each target player use as a donor pool
## players with min. 250 PA in all seasons in which target player had min. 250 PA

source("03a-SCFunction.R")

## Stat Selection
BStats <- tibble(stat=c("AVG","BABIP","BB percent","K percent","OBP","SLG","OPS","wOBA"))
BStats$Use <- BStats$stat %in%  c("wOBA","OBP","OPS")
Stat_Use <- BStats %>% dplyr::filter(Use) %>% pull(stat)

## PA-threshold data sets
PS_250 <- PS_dat()
PS_325 <- PS_dat(PA_cut=325)
PS_400 <- PS_dat(PA_cut=400)

## Primary Analysis
Player_pool_2023_full <- Create_Pool(PS=PS_250,
                                     Cuts=c(15,15),
                                     Cut_Var="Shift_Perc_2022",
                                     S_cols_all=21:23)
Player_pool_2023 <- Create_Pool(PS=PS_250,
                                Cuts=c(15,75),
                                Cut_Var="Shift_Perc_2022",
                                S_cols_all=21:23)
SC_Res_23_full <- Run_SC(Player_pool_2023_full, 
                         21:23, 15:19, Res_Yrs=2023,
                         outname="SC-2023-full",
                         Player_Weight_Plots=FALSE,
                         Stats=Stat_Use)

## Save a subset of results for the >= 75% cutoff:
Keep_Names <- Player_pool_2023 %>%
  dplyr::filter(Shift_Cat != "Medium") %>%
  dplyr::select(Player_ID) %>%
  distinct() %>% pull(Player_ID)
load("res/SC-2023-full-Complete.Rda")
SCs_Results <- SCs_Results %>% 
  dplyr::filter(Player_ID %in% Keep_Names)
MSPEs_Results <- MSPEs_Results %>%
  dplyr::filter(Player_ID %in% Keep_Names)
save(list=c("MSPEs_PRes", "SCs_Results","MSPEs_Results"),
     file=paste0("res/SC-2023-Complete.Rda"))

## Other intervention periods:
Player_pool_2023_24_full <- Create_Pool(PS=PS_250,
                                        Cuts=c(15,15),
                                        Cut_Var="Shift_Perc_2022",
                                        S_cols_all=21:24)
Player_pool_2023_24 <- Create_Pool(PS=PS_250,
                                   Cuts=c(15,75),
                                   Cut_Var="Shift_Perc_2022",
                                   S_cols_all=21:24)
SC_Res_23_24_full <- Run_SC(Player_pool_2023_24_full, 
                         21:24, 15:19, Res_Yrs=2023:2024,
                         outname="SC-2023-24-full",
                         Player_Weight_Plots=FALSE,
                         Stats=Stat_Use)

Keep_Names <- Player_pool_2023_24 %>%
  dplyr::filter(Shift_Cat != "Medium") %>%
  dplyr::select(Player_ID) %>%
  distinct() %>% pull(Player_ID)
load("res/SC-2023-24-full-Complete.Rda")
SCs_Results <- SCs_Results %>% 
  dplyr::filter(Player_ID %in% Keep_Names)
MSPEs_Results <- MSPEs_Results %>%
  dplyr::filter(Player_ID %in% Keep_Names)
save(list=c("MSPEs_PRes", "SCs_Results","MSPEs_Results"),
     file=paste0("res/SC-2023-24-Complete.Rda"))


Player_pool_2024_full <- Create_Pool(PS=PS_250,
                                     Cuts=c(15,15),
                                     Cut_Var="Shift_Perc_2022",
                                     S_cols_all=c(21,22,24))
Player_pool_2024 <- Create_Pool(PS=PS_250,
                                Cuts=c(15,75),
                                Cut_Var="Shift_Perc_2022",
                                S_cols_all=c(21,22,24))
SC_Res_24_full <- Run_SC(Player_pool_2024_full, 
                         c(21,22,24), 15:19, Res_Yrs=2024,
                    outname="SC-2024-full",
                    Player_Weight_Plots=FALSE,
                    Stats=Stat_Use)

Keep_Names <- Player_pool_2024 %>%
  dplyr::filter(Shift_Cat != "Medium") %>%
  dplyr::select(Player_ID) %>%
  distinct() %>% pull(Player_ID)
load("res/SC-2024-full-Complete.Rda")
SCs_Results <- SCs_Results %>% 
  dplyr::filter(Player_ID %in% Keep_Names)
MSPEs_Results <- MSPEs_Results %>%
  dplyr::filter(Player_ID %in% Keep_Names)
save(list=c("MSPEs_PRes", "SCs_Results","MSPEs_Results"),
     file=paste0("res/SC-2024-Complete.Rda"))


## Robustness Checks
### In-Unit: for Shift Rate between 15 and 30%
Player_pool_2023_inunit <- Player_pool_2023_full %>% 
  dplyr::filter(Shift_Perc_2022 <= 30)
Keep_Names <- Player_pool_2023_inunit %>%
  dplyr::select(Player_ID) %>%
  distinct() %>% pull(Player_ID)
load("res/SC-2023-full-Complete.Rda")
SCs_Results <- SCs_Results %>% 
  dplyr::filter(Player_ID %in% Keep_Names)
MSPEs_Results <- MSPEs_Results %>%
  dplyr::filter(Player_ID %in% Keep_Names)
save(list=c("MSPEs_PRes", "SCs_Results","MSPEs_Results"),
     file=paste0("res/SC-2023-inunit-Complete.Rda"))

### In-Time: for 2022
Player_pool_2022_intime <- Player_pool_2023
SC_Res_22 <- Run_SC(Player_pool_2022_intime, 21:23, 15:19, Res_Yrs=2022,
                    outname="SC-2022-intime",
                    Player_Weight_Plots=FALSE,
                    Stats=Stat_Use)

### Change Donor Threshold Lower
Player_pool_2023_low <- Create_Pool(PS=PS_250,
                                    Cuts=c(10,75),
                                    Cut_Var="Shift_Perc_2022",
                                    S_cols_all=21:23)
SC_Res_23_low <- Run_SC(Player_pool_2023_low, 
                         21:23, 15:19, Res_Yrs=2023,
                         outname="SC-2023-low", Player_Weight_Plots=FALSE,
                        Stats=Stat_Use)

### Change Donor Threshold Higher
Player_pool_2023_high <- Create_Pool(PS=PS_250,
                                     Cuts=c(25,75),
                                     Cut_Var="Shift_Perc_2022",
                                     S_cols_all=21:23)
SC_Res_23_high <- Run_SC(Player_pool_2023_high, 
                        21:23, 15:19, Res_Yrs=2023,
                        outname="SC-2023-high", Player_Weight_Plots=FALSE,
                        Stats=Stat_Use)

### Restrict Seasons Used
Player_pool_2023_restrict <- Create_Pool(PS=PS_250,
                                         Cuts=c(15,75),
                                         Cut_Var="Shift_Perc_2022",
                                         S_cols_all=21:23)
SC_Res_23_restrict <- Run_SC(Player_pool_2023_restrict,
                             21:23, 18:19, Res_Yrs=2023,
                             outname="SC-2023-restrict",
                             Player_Weight_Plots=FALSE,
                             Stats=Stat_Use)

### Set higher PA thresholds
Player_pool_2023_PA325 <- Create_Pool(PS=PS_325,
                                      Cuts=c(15,75),
                                      Cut_Var="Shift_Perc_2022",
                                      S_cols_all=21:23)
SC_Res_23_PA325 <- Run_SC(Player_pool_2023_PA325,
                          21:23, 15:19, Res_Yrs=2023,
                          outname="SC-2023-PA325",
                          Player_Weight_Plots=FALSE,
                          Stats=Stat_Use)

Player_pool_2023_PA400 <- Create_Pool(PS=PS_400,
                                      Cuts=c(15,75),
                                      Cut_Var="Shift_Perc_2022",
                                      S_cols_all=21:23)
SC_Res_23_PA400 <- Run_SC(Player_pool_2023_PA400,
                          21:23, 15:19, Res_Yrs=2023,
                          outname="SC-2023-PA400",
                          Player_Weight_Plots=FALSE,
                          Stats=Stat_Use)


## Summaries for Plotting:
## League-wide averages by categories (across players w/ >= 250PA):
B.250_pool <- B.250 %>%
  left_join(bind_rows(Player_pool_2023,Player_pool_2024) %>% distinct() %>% 
              dplyr::select(Name_Match,Shift_Cat), 
            by=join_by(Name_Match)) %>%
  dplyr::filter(!is.na(Shift_Cat), PA >= 250, Season != 2020)


Player_pool_avg <- B.250_pool %>%
  group_by(Shift_Cat,Season) %>%
  dplyr::summarize(across(all_of(BStats$stat), mean, .names="{col}")) %>%
  ungroup()
BStats <- BStats %>% 
  left_join(B.250_pool %>%
              dplyr::select(all_of(BStats$stat)) %>%
              pivot_longer(cols=everything(), names_to="stat") %>%
              group_by(stat) %>%
              dplyr::summarize(min=min(value, na.rm=TRUE), 
                               max=max(value, na.rm=TRUE)) %>% 
              ungroup()) %>%
  left_join(bind_rows(SC_Res_23_full,SC_Res_23_24_full,SC_Res_24_full) %>% group_by(Outcome) %>% 
              dplyr::summarize(diff_min=min(Diff, na.rm=TRUE),
                               diff_max=max(Diff, na.rm=TRUE)) %>%
              ungroup(),
            by=join_by(stat==Outcome)) %>%
  dplyr::select(stat,Use,min,max,diff_min,diff_max)

### Save internal data and parameters data:
save(list=c(paste("Player","pool",
                  c("2023", "2024", "2023_24",
                    "2023_full", "2024_full", "2023_24_full",
                    "2022_intime", "2023_inunit",
                    "2023_low", "2023_high", "2023_restrict",
                    "2023_PA325", "2023_PA400", "avg"),
                  sep="_"),
            "B.250_pool", "BStats"),
     file="int/Player_pool_data.Rda")

