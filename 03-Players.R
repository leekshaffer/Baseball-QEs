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
                      "250 PA each season, 2017","\U2013","2022"))
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



 
 
