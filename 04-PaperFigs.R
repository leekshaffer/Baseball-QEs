## 04-PaperFigs.R
## Creation of figures for manuscript

require(tidyverse)
require(patchwork)

outdir <- "figs/Paper Figs/"
Interv <- 2023

## Analysis 1: League-Wide
load(file="int/DID_data.Rda")

plot_t_BABIP <- ggplot(data=FG.dat.withCF %>% dplyr::filter(Season != 2020 & Season <= Interv),
                       mapping=aes(x=Season, y=BABIP, 
                                   group=Batter, color=Batter, linetype=Batter)) +
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
    labs(title=paste0("Trend in BABIP by batter handedness, bases empty"),
         y="BABIP")

plot_t_OBP <- ggplot(data=FG.dat.withCF %>% dplyr::filter(Season != 2020 & Season <= Interv),
                       mapping=aes(x=Season, y=OBP, 
                                   group=Batter, color=Batter, linetype=Batter)) +
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
  labs(title=paste0("Trend in OBP by batter handedness, bases empty"),
       y="OBP")

plot_ES_BABIP <- ggplot(data=FullES %>% dplyr::filter(Season != 2020 & Season != 2021 & Season <= Interv), 
                  mapping=aes(x=Season, y=BABIP_ES, color=Type)) +
  geom_point(size=2.5) +
  geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
  geom_hline(yintercept=0, color="grey50", linetype="dashed") +
  theme_bw() + theme(legend.position="bottom") +
  scale_color_brewer(name="Analysis Type",
                     type="qual", palette="Dark2") +
  scale_x_continuous(name="Season",
                     breaks=2015:2023,
                     minor_breaks=NULL) +
  coord_cartesian(ylim=c(-.01,.01)) +
  labs(y=paste0("DID Estimate for BABIP"),
       title=paste0("DID Analysis for BABIP, comparing consecutive years"))

plot_ES_OBP <- ggplot(data=FullES %>% dplyr::filter(Season != 2020 & Season != 2021 & Season <= Interv), 
                        mapping=aes(x=Season, y=OBP_ES, color=Type)) +
  geom_point(size=2.5) +
  geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
  geom_hline(yintercept=0, color="grey50", linetype="dashed") +
  theme_bw() + theme(legend.position="bottom") +
  scale_color_brewer(name="Analysis Type",
                     type="qual", palette="Dark2") +
  scale_x_continuous(name="Season",
                     breaks=2015:2023,
                     minor_breaks=NULL) +
  coord_cartesian(ylim=c(-.01,.01)) +
  labs(y=paste0("DID Estimate for OBP"),
       title=paste0("DID Analysis for OBP, comparing consecutive years"))

ggsave(filename=paste0(outdir,"Figure1.png"),
       plot = plot_t_BABIP + 
         plot_t_OBP + 
         plot_ES_BABIP +
         plot_ES_OBP + 
         plot_layout(ncol=2, nrow=2, byrow=TRUE, guides="collect") &
         theme(legend.position="bottom"))
 


## Analysis 2: Player-Specific
load(file="int/Player_pool_data.Rda")
BStats <- tibble(stat=c("AVG","BABIP","K percent","OBP","SLG","OPS","wOBA"))
BStats$min <- sapply(BStats$stat, function(x) min(B.250_pool[B.250_pool$Shift_Cat_2022 != "Medium",x], na.rm=TRUE))
BStats$max <- sapply(BStats$stat, function(x) max(B.250_pool[B.250_pool$Shift_Cat_2022 != "Medium",x], na.rm=TRUE))

load(file="res/Players/Player-SC-Corey Seager.Rda")
Weights_Pred %>% dplyr::arrange(desc(OPS_weight))
Weights_Pred %>% dplyr::arrange(desc(wOBA_weight))
Weights_Pred %>% dplyr::arrange(desc(OBP_weight))

Seager_dat <- SCs %>% pivot_longer(cols=c("Observed","Synthetic","Diff"), 
                                names_to="Result", values_to="Value")
for (statval in c("OBP","OPS","wOBA")) {
  dat <- Seager_dat %>% dplyr::filter(Outcome==statval, Result != "Diff")
  assign(x=paste0("plot_",statval),
         value=ggplot(data=dat, mapping=aes(x=Season, y=Value, 
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
           labs(title=paste0("Synthetic and Observed ",statval," for Corey Seager")))
}

ggsave(filename=paste0(outdir,"Figure2.png"),
       plot=plot_OBP + plot_OPS + plot_wOBA + 
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") & 
         theme(legend.position="bottom"))
