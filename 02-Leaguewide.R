## 02-leaguewide.R
## Analysis of batting stats by handedness for bases-empty situations
## Using FanGraphs data

require(tidyverse)
load(file="int/FG_data.Rda")
Interv <- 2023:2024

## Hitting stats to consider:
BStats <- c("AVG","BABIP","BB percent","K percent","OBP","SLG","OPS","wOBA")

FG.dat.ses <- FG.dat.empty %>%
  dplyr::mutate(HIP=H-HR,
                BIP=AB-HR-SO,
                OB=H+BB+IBB+HBP,
                OBdenom=AB+BB+IBB+HBP+SF,
                BABIP_est=HIP/BIP,
                OBP_est=OB/OBdenom,
                BABIP_var=BABIP_est*(1-BABIP_est)/BIP,
                OBP_var=OBP_est*(1-OBP_est)/OBdenom,
                BABIP_se=sqrt(BABIP_var),
                OBP_se=sqrt(OBP_var))

FG.tab.1.inputs <- FG.dat.ses %>% filter(Season %in% c(2022,2023)) %>% 
  dplyr::select(Batter,Season,BABIP,BABIP_est,BABIP_var,BABIP_se,
                OBP,OBP_est,OBP_var,OBP_se)

FG.tab.1.byHB <- FG.tab.1.inputs %>% group_by(Batter) %>%
  dplyr::summarize(Diff_BABIP=BABIP[Season==2023]-BABIP[Season==2022],
                   Diff_OBP=OBP[Season==2023]-OBP[Season==2022],
                   SE_BABIP=sqrt(sum(BABIP_var)),
                   SE_OBP=sqrt(sum(OBP_var)))
FG.tab.1.byYr <- FG.tab.1.inputs %>% group_by(Season) %>%
  dplyr::summarize(Diff_BABIP=BABIP[Batter=="LHB"]-BABIP[Batter=="RHB"],
                   Diff_OBP=OBP[Batter=="LHB"]-OBP[Batter=="RHB"],
                   SE_BABIP=sqrt(sum(BABIP_var)),
                   SE_OBP=sqrt(sum(OBP_var)))
FG.tab.1.DID <- FG.tab.1.byYr %>%
  dplyr::summarize(DID_BABIP=Diff_BABIP[Season==2023]-Diff_BABIP[Season==2022],
                   DID_OBP=Diff_OBP[Season==2023]-Diff_OBP[Season==2022],
                   SE_BABIP=sqrt(sum(SE_BABIP^2)),
                   SE_OBP=sqrt(sum(SE_OBP^2)))

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
TwoByTwo <- FG.dat.empty %>% dplyr::filter(Season %in% c(2022,Interv)) %>%
  dplyr::select(Season,Batter,all_of(BStats)) %>%
  pivot_wider(id_cols=Batter, names_from=Season,
              values_from=!c("Season","Batter"))
for (val in BStats) {
  for (year in Interv) {
    TwoByTwo[paste0(val,"_Diff-",year)] <- TwoByTwo[paste(val,year,sep="_")]-TwoByTwo[paste(val,year-1,sep="_")]
  }
}
TwoByTwo <- TwoByTwo %>%
  bind_rows(c(Batter="Diff (LHB \U2212 RHB)",
              TwoByTwo %>% dplyr::filter(Batter=="LHB") %>% dplyr::select(!c("Batter")) - 
                TwoByTwo %>% dplyr::filter(Batter=="RHB") %>% dplyr::select(!c("Batter"))))
TwoByTwo %>% dplyr::select(Batter,contains("Diff"))

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

save(list=c("FG.dat.withCF","TwoByTwo","FullES","FG.tab.1.inputs","FG.tab.1.byHB","FG.tab.1.byYr","FG.tab.1.DID"),
     file="int/DID_data.Rda")

