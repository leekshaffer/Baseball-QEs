## 04-ManuscriptResults.R
## Creation of figures, tables, and key results for manuscript

require(tidyverse)
require(patchwork)

outdir <- "figs/Manuscript/"
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
    labs(title=paste0("A. Trend in BABIP by batter handedness, bases empty"),
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
  labs(title=paste0("B. Trend in OBP by batter handedness, bases empty"),
       y="OBP")

plot_ES_BABIP <- ggplot(data=FullES %>% dplyr::filter(Season != 2020 & Season <= Interv), 
                  mapping=aes(x=Season, y=BABIP_ES, color=Type)) +
  geom_point(size=2.5) +
  geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
  geom_hline(yintercept=0, color="grey50", linetype="dashed") +
  theme_bw() + theme(legend.position="bottom") +
  scale_color_brewer(name="Analysis Type",
                     type="qual", palette="Dark2",
                     direction=-1,
                     breaks=c("Placebo","Intervention")) +
  # scale_color_brewer(name="Analysis Type",
  #                    type="qual", palette="Dark2") +
  scale_x_continuous(name="Season",
                     breaks=2015:2023,
                     minor_breaks=NULL) +
  coord_cartesian(ylim=c(-.01,.01)) +
  labs(y=paste0("DID Estimate for BABIP"),
       title=paste0("C. DID analysis for BABIP, consecutive seasons"))

plot_ES_OBP <- ggplot(data=FullES %>% dplyr::filter(Season != 2020 & Season <= Interv), 
                        mapping=aes(x=Season, y=OBP_ES, color=Type)) +
  geom_point(size=2.5) +
  geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
  geom_hline(yintercept=0, color="grey50", linetype="dashed") +
  theme_bw() + theme(legend.position="bottom") +
  scale_color_brewer(name="Analysis Type",
                     type="qual", palette="Dark2",
                     direction=-1,
                     breaks=c("Placebo","Intervention")) +
  scale_x_continuous(name="Season",
                     breaks=2015:2023,
                     minor_breaks=NULL) +
  coord_cartesian(ylim=c(-.01,.01)) +
  labs(y=paste0("DID Estimate for OBP"),
       title=paste0("D. DID analysis for OBP, consecutive seasons"))

ggsave(filename=paste0(outdir,"Figure1.png"),
       plot = plot_t_BABIP + 
         plot_t_OBP + 
         plot_ES_BABIP +
         plot_ES_OBP + 
         plot_layout(ncol=2, nrow=2, byrow=TRUE, guides="collect") &
         theme(legend.position="bottom"),
       dpi=600, width=13, height=8, units="in")
 
### 2x2 tables for key outcomes:
Tbl1 <- TwoByTwo %>% dplyr::select(c("Batter",starts_with("BABIP"),starts_with("OBP")))
write.csv(x=Tbl1 %>%
            dplyr::mutate(across(.cols=-c("Batter"),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3))),
          file=paste0(outdir,"Table1.csv"),
          row.names=FALSE)

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
           labs(title=paste0("Synthetic and Observed ",statval)))
}

ggsave(filename=paste0(outdir,"Figure2.png"),
       plot=plot_OBP+labs(title="A. Synthetic and Observed OBP") + 
         plot_OPS+labs(title="B. Synthetic and Observed OPS") + 
         plot_wOBA+labs(title="C. Synthetic and Observed wOBA") + 
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") & 
         theme(legend.position="bottom"),
       dpi=600, width=13, height=8, units="in")

## Analysis 3: Full SC Results:
load(file="res/SC-Results-Complete.Rda")

Tbl2 <- MSPEs_Results %>% left_join(Player_pool %>% dplyr::select(Player_ID,Shift_Perc_2022)) %>%
  dplyr::mutate(`Shift Rate (2022)`=paste0(format(round(Shift_Perc_2022,1), digits=1, nsmall=1),"%")) %>%
  dplyr::select(Name_Disp, `Shift Rate (2022)`, Outcome, Diff_2023, PVal) %>%
  dplyr::rename(Player=Name_Disp, Estimate=Diff_2023, p=PVal) %>%
  pivot_wider(names_from=Outcome, values_from=c("Estimate","p"), names_vary="slowest") %>%
  dplyr::arrange(desc(`Shift Rate (2022)`))
  
write.csv(x=Tbl2 %>%
            dplyr::mutate(across(.cols=-c("Player","Shift Rate (2022)"),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3))),
          file=paste0(outdir,"Table2.csv"),
          row.names=FALSE)

SCs_Results %>% dplyr::filter(Season==Interv) %>%
  group_by(Outcome,Placebo_Unit) %>%
  dplyr::summarize(Mean=mean(Diff), Median=median(Diff),
                   Prop.Pos=mean(Diff > 0))

for (statval in c("OBP","OPS","wOBA")) {
  assign(x=paste0("plot_SC_",statval),
         value=ggplot(data=SCs_Results %>% dplyr::filter(Outcome==statval),
                  mapping=aes(x=Season, y=Diff, group=Player_ID,
                              color=Placebo_Unit, alpha=Placebo_Unit)) +
           geom_line() +
           scale_x_continuous(name="Season",
                              breaks=2015:2023, minor_breaks=NULL) +
           scale_y_continuous(name="Difference, Synthetic - Observed") +
           # limits=c(-0.25, 0.25)) 
           # + coord_cartesian(ylim=c(-0.25, 0.25)) +
           scale_color_brewer(name="Analysis Type",
                              type="qual", palette="Dark2",
                              direction=-1,
                              breaks=c(TRUE,FALSE),
                              labels=c("Placebo","Intervention")) +
           scale_alpha_manual(name="Analysis Type",
                              breaks=c(TRUE,FALSE),
                              labels=c("Placebo","Intervention"),
                              values=c(0.5,1)) +
           geom_vline(xintercept=Interv-0.5,
                      color="grey50", linetype="dashed") +
           theme_bw() + theme(legend.position="bottom") +
           labs(title=paste0("SCM estimates for ",statval," by player")))
}

ggsave(filename=paste0(outdir,"Figure3.png"),
       plot=plot_SC_OBP+labs(title="A. SCM estimates for OBP by player") + 
         plot_SC_OPS+labs(title="B. SCM estimates for OPS by player") + 
         plot_SC_wOBA+labs(title="C. SCM estimates for wOBA by playerA") + 
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") & 
         theme(legend.position="bottom"),
       dpi=600, width=13, height=8, units="in")
