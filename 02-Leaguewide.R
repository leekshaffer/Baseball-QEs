## 02-leaguewide.R
## Analysis of batting stats by handedness for bases-empty situations
## Using FanGraphs data

require(tidyverse)
load(file="int/FG_data.Rda")
Interv <- 2023:2024

## Hitting stats to consider:
BStats <- c("AVG","BABIP","BB percent","K percent","OBP","SLG","OPS","wOBA")

FG.dat.withCF <- FG.dat.empty %>% dplyr::filter(Season != 2020 & Season <= max(Interv)) %>%
  dplyr::select(all_of(c("Season","Batter",BStats)))
FG.dat.withCF <- FG.dat.withCF %>%
  bind_rows(FG.dat.withCF %>% dplyr::filter(Season < min(Interv), Batter=="LHB") %>% 
              dplyr::mutate(Batter="Counterfactual LHB"))
for (year in Interv) {
  FG.dat.withCF <- FG.dat.withCF %>%
    bind_rows(bind_cols(Season=year,
                       Batter="Counterfactual LHB",
                       FG.dat.withCF %>% dplyr::filter(Season==year-1, Batter=="Counterfactual LHB") %>% dplyr::select(all_of(BStats)) + 
                         FG.dat.withCF %>% dplyr::filter(Season==year, Batter=="RHB") %>% dplyr::select(all_of(BStats)) - 
                         FG.dat.withCF %>% dplyr::filter(Season==year-1, Batter=="RHB") %>% dplyr::select(all_of(BStats))))
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
  bind_rows(c(Batter="Diff (LHB \U2212 RHB)",
              TwoByTwo %>% dplyr::filter(Batter=="LHB") %>% dplyr::select(!c("Batter")) - 
                TwoByTwo %>% dplyr::filter(Batter=="RHB") %>% dplyr::select(!c("Batter"))))
TwoByTwo %>% dplyr::select(Batter,ends_with("Diff"))

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
  dplyr::mutate(Type=if_else(Season %in% Interv, "Intervention", "Placebo")) %>%
  dplyr::select(Season,Type,Batter,ends_with("Diff")) %>%
  pivot_wider(id_cols=c(Season,Type), names_from=Batter, values_from=ends_with("Diff"))
for (val in BStats) {
  FullES[paste(val,"ES",sep="_")] <- FullES[paste(val,"Diff_LHB",sep="_")]-FullES[paste(val,"Diff_RHB",sep="_")]
}
FullES %>% dplyr::select(Season,Type,ends_with("ES"))

save(list=c("FG.dat.withCF","TwoByTwo","FullES"),
     file="int/DID_data.Rda")

