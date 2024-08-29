## 03-Players.R
## Created: August 29, 2024
## Author: Lee Kennedy-Shaffer, PhD

require(tidyverse)

## Import Savant data:

load(file="int/Sav_data.Rda")

## Put on 2022 Shift Rates:
Sav.dat <- Sav.2016_22 %>% left_join(Sav.2016_22 %>% group_by(Player_ID) %>% 
                                       dplyr::filter(Season==2022) %>%
                                       dplyr::select(Player_ID,PA_Shift_Percent) %>%
                                       dplyr::rename(Shift_Perc_2022=PA_Shift_Percent),
                                     by="Player_ID") %>%
  left_join(Sav.2016_22 %>% group_by(Player_ID) %>%
              dplyr::summarize(Shift_Perc_Max=max(PA_Shift_Percent),
                               Shift_Perc_Min=min(PA_Shift_Percent)),
            by="Player_ID")

## Get list of players with >= 250 PA in desired seasons:
Seasons_use <- c(2017:2019,2021:2023)
Players_use <- B.250 %>% dplyr::filter(Season %in% Seasons_use) %>%
  group_by(Name,Name_Match,Player_ID) %>% 
  dplyr::summarize(n_seasons_use=n()) %>%
  dplyr::filter(n_seasons_use == length(Seasons_use)) %>%
  dplyr::select(!n_seasons_use)

## Create analysis data set:
Cuts <- c(20,80)
Use_Stats <- Sav.dat %>% dplyr::filter(Player_ID %in% Players_use$Player_ID) %>%
  dplyr::mutate(Shift_Cat_2022=if_else(Shift_Perc_2022 >= Cuts[2],"High",
                                      if_else(Shift_Perc_2022 <= Cuts[1],"Low","Medium")),
                Shift_Cat_Max=if_else(Shift_Perc_Max >= Cuts[2],"High",
                                     if_else(Shift_Perc_Max <= Cuts[1],"Low","Medium")),
                Shift_Cat_Min=if_else(Shift_Perc_Min >= Cuts[2],"High",
                                      if_else(Shift_Perc_Min <= Cuts[1],"Low","Medium")))
