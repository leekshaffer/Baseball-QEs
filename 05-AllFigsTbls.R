## 05-ManuscriptResults.R
## Creation of figures, tables, and key results for manuscript and repository

require(tidyverse)
require(patchwork)
source("04-FigureCommands.R")

MSoutdir <- "figs/Manuscript/"
Interv <- 2023

## Required data sets:
load(file="int/DID_data.Rda")
load(file="int/Player_pool_data.Rda")
load(file="res/SC-2023-Results-Complete.Rda")
MSPEs_PRes_2023 <- MSPEs_PRes
SCs_Results_2023 <- SCs_Results
MSPEs_Results_2023 <- MSPEs_Results
load(file="res/SC-2024-Results-Complete.Rda")
MSPEs_PRes_2024 <- MSPEs_PRes
SCs_Results_2024 <- SCs_Results
MSPEs_Results_2024 <- MSPEs_Results
load(file="res/SC-2023-24-Results-Complete.Rda")
MSPEs_PRes_2023_24 <- MSPEs_PRes
SCs_Results_2023_24 <- SCs_Results
MSPEs_Results_2023_24 <- MSPEs_Results

## Analysis 1: League-Wide

### Manuscript Figure 1
plot_BABIP <- plot_DIDs("BABIP", FG.dat.withCF, FullES, tagvals=c("A. ","C. "))
plot_OBP <- plot_DIDs("OBP", FG.dat.withCF, FullES, tagvals=c("B. ","D. "))

ggsave(filename=paste0(MSoutdir,"Figure1.png"),
       plot = plot_BABIP[["Trend"]] + theme(legend.position="inside",
                                            legend.position.inside=c(.201,.178),
                                            legend.background=element_rect(fill="white",
                                                                           color="grey50"),
                                            legend.title=element_text(size=rel(1.2)),
                                            legend.text=element_text(size=rel(1.2))) +
         plot_OBP[["Trend"]] + theme(legend.position="none") +
         plot_BABIP[["ES"]] + theme(legend.position="inside",
                                    legend.position.inside=c(.14,.86),
                                    legend.background=element_rect(fill="white",
                                                                   color="grey50"),
                                    legend.title=element_text(size=rel(1.2)),
                                    legend.text=element_text(size=rel(1.2))) +
         plot_OBP[["ES"]] + theme(legend.position="none") +
         plot_layout(ncol=2, nrow=2, byrow=TRUE, guides="keep"),
       dpi=600, width=12, height=8.1, units="in")

### Full set of DID results:
DIDoutdir <- "figs/DID Analysis/"
for (outval in BStats$stat) {
  plot_outval <- plot_DIDs(outval, FG.dat.withCF, FullES, tagvals=c("A. ","B. "))
  ggsave(filename=paste0(DIDoutdir,"DID-plot-",outval,".png"),
         plot = plot_outval[["Trend"]] + theme(legend.position="right",
                                              # legend.position.inside=c(.184,.215),
                                              legend.background=element_rect(fill="white",
                                                                             color="grey50")) +
           plot_outval[["ES"]] + theme(legend.position="right",
                                      # legend.position.inside=c(.126,.815),
                                      legend.background=element_rect(fill="white",
                                                                     color="grey50")) +
           plot_layout(ncol=1, nrow=2, byrow=TRUE, guides="keep"),
         dpi=600, width=8, height=8, units="in")
}

### 2x2 tables for key outcomes:
Tbl1 <- TwoByTwo %>% dplyr::select(c("Batter",starts_with("BABIP"),starts_with("OBP")))
write.csv(x=Tbl1 %>%
            dplyr::mutate(across(.cols=-c("Batter"),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3))),
          file=paste0(MSoutdir,"Table1.csv"),
          row.names=FALSE)

## Analysis 2: SCM
### Manuscript Figure 2:
Target <- "Corey Seager"

load(file=paste0("res/Players-SC-2023/Player-SC-",Target,".Rda"))
Weights_Pred %>% dplyr::arrange(desc(OPS_weight))
Weights_Pred %>% dplyr::arrange(desc(wOBA_weight))
Weights_Pred %>% dplyr::arrange(desc(OBP_weight))
SCs %>% dplyr::filter(Intervention)

ggsave(filename=paste0(MSoutdir,"Figure2-2023.png"),
       plot=plot_Comp("OBP", SCs_Results_2023, Target, "A. ") +
         plot_Comp("OPS", SCs_Results_2023, Target, "B. ") +
         plot_Comp("wOBA", SCs_Results_2023, Target, "C. ") +
         guide_area() +
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
         theme(legend.background=element_rect(fill="white",
                                              color="grey50"),
               legend.text=element_text(size=rel(1.2)),
               legend.title=element_text(size=rel(1.2)),
               legend.direction="vertical"),
       dpi=600, width=12, height=8.1, units="in")

#### Version with 2024 results too:
load(file=paste0("res/Players-SC-2023-24/Player-SC-",Target,".Rda"))
Weights_Pred %>% dplyr::arrange(desc(OPS_weight))
Weights_Pred %>% dplyr::arrange(desc(wOBA_weight))
Weights_Pred %>% dplyr::arrange(desc(OBP_weight))
SCs %>% dplyr::filter(Intervention)

ggsave(filename=paste0(MSoutdir,"Figure2-2023-24.png"),
       plot=plot_Comp("OBP", SCs_Results_2023_24, Target, "A. ") +
         plot_Comp("OPS", SCs_Results_2023_24, Target, "B. ") +
         plot_Comp("wOBA", SCs_Results_2023_24, Target, "C. ") +
         guide_area() +
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
         theme(legend.background=element_rect(fill="white",
                                              color="grey50"),
               legend.text=element_text(size=rel(1.2)),
               legend.title=element_text(size=rel(1.2)),
               legend.direction="vertical"),
       dpi=600, width=12, height=8.1, units="in")

#### Version with 2024 results only:
load(file=paste0("res/Players-SC-2024/Player-SC-",Target,".Rda"))
Weights_Pred %>% dplyr::arrange(desc(OPS_weight))
Weights_Pred %>% dplyr::arrange(desc(wOBA_weight))
Weights_Pred %>% dplyr::arrange(desc(OBP_weight))
SCs %>% dplyr::filter(Intervention)

ggsave(filename=paste0(MSoutdir,"Figure2-2024.png"),
       plot=plot_Comp("OBP", SCs_Results_2023_24, Target, "A. ") +
         plot_Comp("OPS", SCs_Results_2023_24, Target, "B. ") +
         plot_Comp("wOBA", SCs_Results_2023_24, Target, "C. ") +
         guide_area() +
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
         theme(legend.background=element_rect(fill="white",
                                              color="grey50"),
               legend.text=element_text(size=rel(1.2)),
               legend.title=element_text(size=rel(1.2)),
               legend.direction="vertical"),
       dpi=600, width=12, height=8.1, units="in")



### Manuscript Table 2:
Tbl2_2023 <- MSPEs_Results_2023 %>% 
  left_join(Player_pool_2023 %>% dplyr::select(Player_ID,Shift_Perc_2022)) %>%
  dplyr::mutate(`Shift Rate (2022)`=paste0(format(round(Shift_Perc_2022,1), digits=1, nsmall=1),"%")) %>%
  dplyr::select(Name_Disp, `Shift Rate (2022)`, Outcome, Diff_2023, PVal) %>%
  dplyr::rename(Player=Name_Disp, Estimate=Diff_2023, p=PVal) %>%
  pivot_wider(names_from=Outcome, values_from=c("Estimate","p"), names_vary="slowest") %>%
  dplyr::arrange(desc(`Shift Rate (2022)`))

Tbl2_2023_24 <- MSPEs_Results_2023_24 %>% 
  left_join(Player_pool_2023_24 %>% dplyr::select(Player_ID,Shift_Perc_2022)) %>%
  dplyr::mutate(`Shift Rate (2022)`=paste0(format(round(Shift_Perc_2022,1), digits=1, nsmall=1),"%")) %>%
  dplyr::select(Name_Disp, `Shift Rate (2022)`, Outcome, Diff_2023, Diff_2024, PVal) %>%
  dplyr::rename(Player=Name_Disp, Est_2023=Diff_2023, Est_2024=Diff_2024, p=PVal) %>%
  pivot_wider(names_from=Outcome, values_from=c("Est_2023","Est_2024","p"), names_vary="slowest") %>%
  dplyr::arrange(desc(`Shift Rate (2022)`))

Tbl2_2024 <- MSPEs_Results_2024 %>% 
  left_join(Player_pool_2024 %>% dplyr::select(Player_ID,Shift_Perc_2022)) %>%
  dplyr::mutate(`Shift Rate (2022)`=paste0(format(round(Shift_Perc_2022,1), digits=1, nsmall=1),"%")) %>%
  dplyr::select(Name_Disp, `Shift Rate (2022)`, Outcome, Diff_2024, PVal) %>%
  dplyr::rename(Player=Name_Disp, Estimate=Diff_2024, p=PVal) %>%
  pivot_wider(names_from=Outcome, values_from=c("Estimate","p"), names_vary="slowest") %>%
  dplyr::arrange(desc(`Shift Rate (2022)`))

write.csv(x=Tbl2_2023 %>%
            dplyr::mutate(across(.cols=-c("Player","Shift Rate (2022)"),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3))),
          file=paste0(MSoutdir,"Table2-2023.csv"),
          row.names=FALSE)

write.csv(x=Tbl2_2023_24 %>%
            dplyr::mutate(across(.cols=-c("Player","Shift Rate (2022)"),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3))),
          file=paste0(MSoutdir,"Table2-2023-24.csv"),
          row.names=FALSE)

write.csv(x=Tbl2_2024 %>%
            dplyr::mutate(across(.cols=-c("Player","Shift Rate (2022)"),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3))),
          file=paste0(MSoutdir,"Table2-2024.csv"),
          row.names=FALSE)

SCs_Results_2023 %>% dplyr::filter(Season==Interv) %>%
  group_by(Outcome,Placebo_Unit) %>%
  dplyr::summarize(Mean=mean(Diff), Median=median(Diff),
                   Prop.Pos=mean(Diff > 0))

SCs_Results_2023_24 %>% dplyr::filter(Season >= Interv) %>%
  group_by(Name,Player_ID,Outcome,Placebo_Unit) %>%
  dplyr::summarize(Diff_Total=sum(Diff)) %>%
  ungroup() %>% group_by(Outcome,Placebo_Unit) %>%
  dplyr::summarize(Mean=mean(Diff_Total), Median=median(Diff_Total),
                   Prop.Pos=mean(Diff_Total > 0))

SCs_Results_2024 %>% dplyr::filter(Season == 2024) %>%
  group_by(Outcome,Placebo_Unit) %>%
  dplyr::summarize(Mean=mean(Diff), Median=median(Diff),
                   Prop.Pos=mean(Diff > 0))

### Manuscript Figure 3:
ggsave(filename=paste0(MSoutdir,"Figure3-2023.png"),
       plot=plot_SC_ests_all("OBP", SCs_Results_2023, LW=0.8, tagval="A. ") + 
         plot_SC_ests_all("OPS", SCs_Results_2023, LW=0.8, tagval="B. ") + 
         plot_SC_ests_all("wOBA", SCs_Results_2023, LW=0.8, tagval="C. ") + 
         guide_area() +
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
         theme(legend.position="inside",
               legend.background=element_rect(fill="white", color="grey50"),
               legend.text=element_text(size=rel(1.2)),
               legend.title=element_text(size=rel(1.2)),
               legend.direction="vertical"),
       dpi=600, width=12, height=8.1, units="in")

ggsave(filename=paste0(MSoutdir,"Figure3-2023-24.png"),
       plot=plot_SC_ests_all("OBP", SCs_Results_2023_24, LW=0.8, tagval="A. ") + 
         plot_SC_ests_all("OPS", SCs_Results_2023_24, LW=0.8, tagval="B. ") + 
         plot_SC_ests_all("wOBA", SCs_Results_2023_24, LW=0.8, tagval="C. ") + 
         guide_area() +
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
         theme(legend.position="inside",
               legend.background=element_rect(fill="white", color="grey50"),
               legend.text=element_text(size=rel(1.2)),
               legend.title=element_text(size=rel(1.2)),
               legend.direction="vertical"),
       dpi=600, width=12, height=8.1, units="in")

ggsave(filename=paste0(MSoutdir,"Figure3-2024.png"),
       plot=plot_SC_ests_all("OBP", SCs_Results_2024, LW=0.8, tagval="A. ") + 
         plot_SC_ests_all("OPS", SCs_Results_2024, LW=0.8, tagval="B. ") + 
         plot_SC_ests_all("wOBA", SCs_Results_2024, LW=0.8, tagval="C. ") + 
         guide_area() +
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
         theme(legend.position="inside",
               legend.background=element_rect(fill="white", color="grey50"),
               legend.text=element_text(size=rel(1.2)),
               legend.title=element_text(size=rel(1.2)),
               legend.direction="vertical"),
       dpi=600, width=12, height=8.1, units="in")

### Full Set of Outcome Trajectory plots by Player and by Shift Category:
Trajoutdir <- "figs/Trajectories/"

for (outval in BStats$stat) {
  ## With a line for each player:
  ggsave(filename=paste0(Trajoutdir,"Traj-ByPlayer-plot-",outval,".png"),
         plot=plot_Traj(statval=outval,
                        Traj.dat=B.250_pool %>% dplyr::filter(Shift_Cat_2022 != "Medium"),
                        CatVar="Shift_Cat_2022",
                        CatName=NULL,
                        CatBreaks=c("High","Low"),
                        CatLabs=c("Target Players (2022 Shift Rate \U2265 80%)",
                                  "Control Players (2022 Shift Rate \U2264 20%)"),
                        CatCols=brewer.pal(3, "Dark2")[c(3,1)],
                        CatLTY=c("solid","longdash"),
                        CatAlpha=c(1,.4),
                        Type="All",
                        Fixed=TRUE) + 
           theme(legend.position="bottom",
                 legend.background=element_rect(fill="white", color="grey50"),
                 legend.direction="horizontal"),
         dpi=600, units="in", width=8, height=5)
  
  ## Category average only, fixed axes:
  ggsave(filename=paste0(Trajoutdir,"Traj-ByCategory-fixed-plot-",outval,".png"),
         plot=plot_Traj(statval=outval,
                        Traj.dat=Player_pool_avg,
                        CatVar="Shift_Cat_2022",
                        CatName=NULL,
                        CatBreaks=c("High","Medium","Low"),
                        CatLabs=c("Target Players (2022 Shift Rate \U2265 80%)",
                                  "Unused Players (2022 Shift Rate 20% \U2013 80%)",
                                  "Control Players (2022 Shift Rate \U2264 20%)"),
                        CatCols=brewer.pal(3, "Dark2")[c(3,2,1)],
                        CatLTY=c("solid","dotdash","longdash"),
                        CatAlpha=c(1,.3,.5),
                        Type="Average",
                        Fixed=TRUE) + 
           theme(legend.position="bottom",
                 legend.background=element_rect(fill="white", color="grey50"),
                 legend.direction="horizontal",
                 legend.text=element_text(size=rel(0.7))),
         dpi=600, units="in", width=8, height=5)
  
  ## Category average only, free axes:
  ggsave(filename=paste0(Trajoutdir,"Traj-ByCategory-free-plot-",outval,".png"),
         plot=plot_Traj(statval=outval,
                        Traj.dat=Player_pool_avg,
                        CatVar="Shift_Cat_2022",
                        CatName=NULL,
                        CatBreaks=c("High","Medium","Low"),
                        CatLabs=c("Target Players (2022 Shift Rate \U2265 80%)",
                                  "Unused Players (2022 Shift Rate 20% \U2013 80%)",
                                  "Control Players (2022 Shift Rate \U2264 20%)"),
                        CatCols=brewer.pal(3, "Dark2")[c(3,2,1)],
                        CatLTY=c("solid","dotdash","longdash"),
                        CatAlpha=c(1,.3,.5),
                        Type="Average",
                        Fixed=FALSE) + 
           theme(legend.position="bottom",
                 legend.background=element_rect(fill="white", color="grey50"),
                 legend.direction="horizontal",
                 legend.text=element_text(size=rel(0.7))),
         dpi=600, units="in", width=8, height=5)
}

### Full Set of all-player SCM estimate plots:
Plot_SC_Res <- function(outname, Pool) {
  load(file=paste0("res/",outname,"-Results-Complete.Rda"))
  SCMoutdir <- paste0("figs/",outname," Estimates/")
  for (outval in BStats %>% dplyr::filter(Use) %>% dplyr::pull(stat)) {
    ggsave(filename=paste0(SCMoutdir,"SCM-plot-",outval,".png"),
           plot=plot_SC_ests_all("OBP", SCs_Results) + 
             theme(legend.position="bottom",
                   legend.background=element_rect(fill="white", color="grey50"),
                   legend.direction="horizontal"),
           dpi=600, units="in", width=8, height=5)
  }
  
  ### Full Set of player-specific SCM estimate plots:
  for (ID in Pool %>% dplyr::filter(Shift_Cat_2022=="High") %>% pull(Player_ID)) {
    ## Info on target player:
    Row <- Pool[Pool$Player_ID==ID,]
    Target <- Row$Name_Disp
    print(paste0("Beginning figures for ",Target))
    Seasons <- unique(SCs_Results %>% dplyr::filter(Player_ID==ID) %>% pull(Season))
    Targetdir <- paste0("figs/Players-",outname,"/",Target,"/")
    for (outval in BStats %>% dplyr::filter(Use) %>% pull(stat)) {
      ## Get data for player & placebos for that outcome only:
      SC_data <- SCs_Results %>% 
        dplyr::filter(Outcome==outval & 
                        (Player_ID==ID | Placebo_Unit) & 
                        (Season %in% Seasons))
      
      ## Trajectory Plot:
      ggsave(filename=paste0(Targetdir,"Traj-plot-",outval,".png"),
             plot=plot_Traj(statval=outval,
                            Traj.dat=SC_data %>% dplyr::select(-c("Synthetic","Diff")) %>%
                              pivot_wider(names_from="Outcome",
                                          values_from="Observed"),
                            CatVar="Placebo_Unit",
                            CatName=NULL,
                            CatBreaks=c(FALSE,TRUE),
                            CatLabs=c(paste0("Target Player: ",Target),
                                      "Control Players (2022 Shift Rate \U2264 20%)"),
                            CatCols=brewer.pal(3, "Dark2")[c(3,1)],
                            CatLTY=c("solid","longdash"),
                            CatAlpha=c(1,.4),
                            Type="Target",
                            Fixed=TRUE,
                            Target=Target,
                            tagval=NULL) + 
               theme(legend.position="bottom",
                     legend.background=element_rect(fill="white", color="grey50"),
                     legend.direction="horizontal"),
             dpi=600, units="in", width=8, height=5)
      
      ## Estimates Plot:
      ggsave(filename=paste0(Targetdir,"SCM-plot-",outval,".png"),
             plot=plot_SC_ests(outval, SC_data,
                               LegName=NULL,
                               LegVar="Placebo_Unit",
                               LegLabs=c(paste0("Target Player: ",Target),
                                         "Placebo Players (2022 Shift Rate \U2264 20%)"), 
                               LegBreaks=c(FALSE,TRUE),
                               LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                               LegAlpha=c(1,0.5), 
                               LegLTY=c("solid","longdash"), 
                               title=paste0("SCM estimates for ",outval," for ",Target,
                                            " and placebos"),
                               LW=1.2,
                               tagval=NULL) + 
               theme(legend.position="bottom",
                     legend.background=element_rect(fill="white", color="grey50"),
                     legend.direction="horizontal"),
             dpi=600, units="in", width=8, height=5)
  
      ## Comparisons (Synthetic vs. Observed) Plot:
      ggsave(filename=paste0(Targetdir,"Comp-plot-",outval,".png"),
             plot=plot_Comp(outval, SC_data,
                               display_name=Target,
                               tagval=NULL) + 
               theme(legend.position="bottom",
                     legend.background=element_rect(fill="white", color="grey50"),
                     legend.direction="horizontal"),
             dpi=600, units="in", width=8, height=5) 
    }
  }
}

Plot_SC_Res("SC-2023", Player_pool_2023)
Plot_SC_Res("SC-2024", Player_pool_2024)
Plot_SC_Res("SC-2023-24", Player_pool_2023_24)


