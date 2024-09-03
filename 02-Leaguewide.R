## 02-leaguewide.R
## Analysis of batting stats by handedness for bases-empty situations
## Using FanGraphs data

require(tidyverse)
load(file="int/FG_data.Rda")
Interv <- 2023

## Hitting stats to consider:
BStats <- c("AVG","BABIP","K percent","OBP","SLG","OPS","wOBA")

FG.dat.withCF <- FG.dat.empty %>% dplyr::filter(Season != 2020 & Season <= Interv) %>%
  dplyr::select(all_of(c("Season","Batter",BStats)))
for (year in Interv) {
  FG.dat.withCF <- FG.dat.withCF %>%
    bind_rows(FG.dat.withCF %>% dplyr::filter(Season < Interv, Batter=="LHB") %>% 
                dplyr::mutate(Batter="Counterfactual LHB"),
                bind_cols(Season=year,
                       Batter="Counterfactual LHB",
                       FG.dat.withCF %>% dplyr::filter(Season==year-1, Batter=="LHB") %>% dplyr::select(all_of(BStats)) + 
                         FG.dat.withCF %>% dplyr::filter(Season==year, Batter=="RHB") %>% dplyr::select(all_of(BStats)) - 
                         FG.dat.withCF %>% dplyr::filter(Season==year-1, Batter=="RHB") %>% dplyr::select(all_of(BStats))))
}

## Plot trends with bases empty:
for (val in BStats) {
  plot_trend <- ggplot(data=FG.dat.withCF %>% dplyr::filter(Season != 2020 & Season <= Interv),
         mapping=aes(x=Season, y=get(val), group=Batter, color=Batter, linetype=Batter)) +
    geom_line(linewidth=1.2) + geom_point(shape=17, size=2.2) +
    theme_bw() + theme(legend.position="bottom") +
    scale_color_manual(name="Batter Handedness",
                       values=c("#a6611a","#92c5de","#a6611a"),
                       breaks=c("LHB","RHB","Counterfactual LHB")) +
    scale_linetype_manual(name="Batter Handedness",
                          values=c("solid","dotted","dotted"),
                          breaks=c("LHB","RHB","Counterfactual LHB")) +
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
  dplyr::filter(Season != 2020) %>%
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
  plot_ES <- ggplot(data=FullES %>% dplyr::filter(Season <= Interv), 
         mapping=aes(x=Season, y=get(paste(val,"ES",sep="_")), color=Type)) +
    geom_point(size=2.5) +
    geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
    geom_hline(yintercept=0, color="grey50", linetype="dashed") +
    theme_bw() + theme(legend.position="bottom") +
    scale_color_brewer(name=NULL,
                       type="qual", palette="Dark2") +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    labs(y=paste0("Change in ",val," from previous year, LHB","\U2013","RHB"),
         title=paste0("Event study analysis for ",val,
                      ", bases empty, 1-year difference, LHB","\U2013","RHB"))
  ggsave(filename = paste0("figs/Event Studies/ES-plot-",val,".png"),
         plot=plot_ES)
}

save(list=c("FG.dat.withCF","TwoByTwo","FullES"),
     file="int/DID_data.Rda")

