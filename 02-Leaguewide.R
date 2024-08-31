## 02-leaguewide.R
## Analysis of batting stats by handedness for bases-empty situations
## Using FanGraphs data

require(tidyverse)
load(file="int/FG_data.Rda")
Interv <- 2023

## Hitting stats to consider:
BStats <- c("AVG","BABIP","K percent","OBP","SLG","OPS","wOBA")

## Plot trends with bases empty:
for (val in BStats) {
  plot_trend <- ggplot(data=FG.dat.empty %>% dplyr::filter(Season != 2020 & Season != 2024),
         mapping=aes(x=Season, y=get(val), group=Batter, color=Batter)) +
    geom_line() + geom_point(size=2) +
    theme_bw() +
    scale_color_brewer(name="Batter Handedness",
                       type="qual", palette="Dark2") +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
    labs(title=paste0("Trend in ",val," by batter handedness, bases empty, 2015",
                      "\U2013","2023"),
         y=val)
  ggsave(filename = paste0("figs/Trends/trend-plot-",val,".png"),
         plot=plot_trend)
}

## 2022 vs. 2023 2x2 table:
TwoByTwo <- FG.dat.empty %>% dplyr::filter(Season %in% c(2022,2023)) %>%
  dplyr::select(Season,Batter,all_of(BStats)) %>%
  pivot_wider(id_cols=Batter, names_from=Season,
              values_from=!c("Season","Batter"))
for (val in BStats) {
  TwoByTwo[paste(val,"Diff",sep="_")] <- TwoByTwo[paste(val,"2023",sep="_")]-TwoByTwo[paste(val,"2022",sep="_")]
}
TwoByTwo <- TwoByTwo %>%
  bind_rows(c(Batter="Diff, LHB-RHB",
              TwoByTwo %>% dplyr::filter(Batter=="LHB") %>% dplyr::select(!c("Batter")) - 
                TwoByTwo %>% dplyr::filter(Batter=="RHB") %>% dplyr::select(!c("Batter"))))
TwoByTwo %>% dplyr::select(Batter,ends_with("Diff"))
save(TwoByTwo,
     file="tbls/Full_TwoByTwo.Rda")

## Event study analysis:
FullES <- FG.dat.empty %>% dplyr::select(Season,Batter,all_of(BStats)) %>%
  group_by(Batter) %>% arrange(Batter,Season) %>%
  mutate(across(.cols=all_of(BStats),
         .fns=~lag(.x),
         .names="{.col}_lag"))
for (val in BStats) {
  FullES[paste(val,"Diff",sep="_")] <- FullES[val]-FullES[paste(val,"lag",sep="_")]
}
FullES <- FullES %>%
  dplyr::mutate(Type=if_else(Season >= Interv, "Intervention", "Placebo")) %>%
  dplyr::select(Season,Type,Batter,ends_with("Diff")) %>%
  pivot_wider(id_cols=c(Season,Type), names_from=Batter, values_from=ends_with("Diff"))
for (val in BStats) {
  FullES[paste(val,"ES",sep="_")] <- FullES[paste(val,"Diff_LHB",sep="_")]-FullES[paste(val,"Diff_RHB",sep="_")]
}
FullES %>% dplyr::select(Season,Type,ends_with("ES"))
save(FullES, file="tbls/Full_ES.Rda")

for (val in BStats) {
  plot_ES <- ggplot(data=FullES %>% dplyr::filter(Season != 2020 & Season != 2021 & Season != 2024), 
         mapping=aes(x=Season, y=get(paste(val,"ES",sep="_")), color=Type)) +
    geom_point(size=2) +
    geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
    geom_hline(yintercept=0, color="grey50", linetype="dashed") +
    theme_bw() +
    scale_color_brewer(name=NULL,
                       type="qual", palette="Dark2") +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    theme(legend.position="bottom") +
    labs(y=paste0("Change in ",val," from previous year, LHB","\U2013","RHB"),
         title=paste0("Event study analysis for ",val,
                      ", bases empty, 1-year difference, LHB","\U2013","RHB"))
  ggsave(filename = paste0("figs/Event Studies/ES-plot-",val,".png"),
         plot=plot_ES)
}
  
