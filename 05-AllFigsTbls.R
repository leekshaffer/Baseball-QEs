## 05-AllFigsTbls.R
## Creation of figures, tables, and key results for manuscript and repository

source("04-FigureCommands.R")
library(patchwork)

MSoutdir <- "figs/Manuscript/"
Interv <- 2023:2024
types <- c("2023","2024","2023_24") ## The core analysis year types
full_types <- c(types, paste(types,"full",sep="_"),
                "2023_inunit", "2022_intime",
                "2023_low", "2023_high", "2023_restrict",
                "2023_PA325","2023_PA400")

## Required data sets:
load(file="int/DID_data.Rda")
load(file="int/Player_pool_data.Rda")

for (type in full_types) {
  Shifts <- get(paste0("Player_pool_",type)) %>% dplyr::select(Player_ID, Shift_Perc_2022, Shift_Cat, 
                                                               Shift_Perc_Max)
  load(file=paste0("res/SC-",gsub("_","-",type),"-Complete.Rda"))
  assign(x=paste0("MSPEs_PRes_",type),
         value=MSPEs_PRes %>% left_join(Shifts, by=join_by(Placebo_ID==Player_ID)))
  assign(x=paste0("MSPEs_Results_",type),
         value=MSPEs_Results %>% left_join(Shifts, by=join_by(Player_ID)))
  assign(x=paste0("SCs_Results_",type),
         value=SCs_Results %>% left_join(Shifts, by=join_by(Player_ID)))
  rm(list=c("MSPEs_Results","MSPEs_PRes","SCs_Results"))
}

## Analysis 1: League-Wide

### Manuscript Figure 2
plot_BABIP <- plot_DIDs("BABIP", FG.dat.withCF, FullES, tagvals=c("A. ","C. "))
plot_OBP <- plot_DIDs("OBP", FG.dat.withCF, FullES, tagvals=c("B. ","D. "))

ggsave(filename=paste0(MSoutdir,"Figure2.png"),
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
Tbl2 <- TwoByTwo %>% dplyr::select(c("Batter",starts_with("BABIP"),starts_with("OBP")))
write.csv(x=Tbl2 %>%
            dplyr::mutate(across(.cols=-c("Batter"),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3))),
          file=paste0(MSoutdir,"Table2.csv"),
          row.names=FALSE)

## Analysis 2: SCM
### Manuscript Figure 3:
Target <- "Corey Seager"

for (type in c("2023_24","2024","2023","2023_low","2023_high","2023_restrict")) {
  print(paste0("Player-specific SCM for: ",Target," in ",gsub("_","-",type)))
  load(file=paste0("res/Players-SC-",
                   gsub("_","-",ifelse(type %in% types, paste(type,"full",sep="_"), type)),
                   "/Player-SC-",Target,".Rda"))
  Weights_Unit %>% dplyr::arrange(desc(OPS_weight))
  Weights_Unit %>% dplyr::arrange(desc(wOBA_weight))
  Weights_Unit %>% dplyr::arrange(desc(OBP_weight))
  
  assign(x=paste0("Tbl3_",type),
         value=Weights_Unit %>% 
           dplyr::filter(pmax(OBP_weight,OPS_weight,wOBA_weight) > 0.001) %>% 
           dplyr::mutate(across(.cols=contains("weight"), 
                                .fns=~ifelse(.x < 0.001, NA, .x))) %>%
           dplyr::arrange(desc(OBP_weight)))
  
  SCs %>% dplyr::filter(Intervention)
  write.csv(x=get(paste0("Tbl3_",type)) %>%
              dplyr::mutate(across(.cols=contains("weight"),
                                   .fns=~format(round(.x*100, digits=0), nsmall=0))),
            file=paste0(MSoutdir,"Table3-",gsub("_","-",type),".csv"),
            row.names=FALSE)
  ggsave(filename=paste0(MSoutdir,"Figure3-",gsub("_","-",type),".png"),
         plot=plot_Comp("OBP", get(x=paste0("SCs_Results_",type)), Target, "A. ") +
           plot_Comp("OPS", get(x=paste0("SCs_Results_",type)), Target, "B. ") +
           plot_Comp("wOBA", get(x=paste0("SCs_Results_",type)), Target, "C. ") +
           guide_area() +
           plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
           theme(legend.background=element_rect(fill="white",
                                                color="grey50"),
                 legend.text=element_text(size=rel(1.2)),
                 legend.title=element_text(size=rel(1.2)),
                 legend.direction="vertical"),
         dpi=600, width=12, height=8.1, units="in")
}

### Manuscript Table 4:
for (type in full_types) {
  assign(x=paste0("Tbl4_",type),
         value = get(x=paste0("MSPEs_Results_",type)) %>%
    dplyr::mutate(`Shift Rate (2022)`=paste0(format(round(Shift_Perc_2022,1), digits=1, nsmall=1),"%")) %>%
    dplyr::select(c("Name_Disp","Shift Rate (2022)","Donors","Outcome",starts_with("Diff"),"PVal")) %>%
    dplyr::rename(Player=Name_Disp, p=PVal) %>%
    dplyr::rename_with(.fn=~gsub("Diff","Est",.x),
                       .cols=starts_with("Diff")) %>%
    dplyr::select(-contains("Total")) %>%
    pivot_wider(names_from=Outcome,
                values_from=c(starts_with("Est"),"p"), names_vary="slowest") %>%
    dplyr::arrange(desc(`Shift Rate (2022)`)))
  
  write.csv(x=get(paste0("Tbl4_",type)) %>%
              dplyr::mutate(across(.cols=-c("Player","Shift Rate (2022)"),
                                   .fns=~format(round(.x, digits=3), digits=3, nsmall=3))),
            file=paste0(MSoutdir,"Table4-",gsub("_","-",type),".csv"),
            row.names=FALSE)
}

Tbl4_itprep <- MSPEs_Results_2022_intime %>% 
  dplyr::select(c("Name_Disp","Shift_Perc_2022","Donors","Outcome","Diff_2022","Diff_2023"))
Get_P <- function(row) {
  val22 <- as.numeric(row["Diff_2022"])
  val23 <- as.numeric(row["Diff_2023"])
  out <- row["Outcome"]
  vals_2022 <- abs(c(val22,
                     MSPEs_PRes_2022_intime %>% dplyr::filter(Outcome==out) %>%
                       pull(Diff_2022)))
  vals_2023 <- abs(c(val23,
                     MSPEs_PRes_2022_intime %>% dplyr::filter(Outcome==out) %>%
                       pull(Diff_2023)))
  return(c(P_2022=mean(vals_2022 >= abs(val22)),
           P_2023=mean(vals_2023 >= abs(val23))))
}
Tbl4_intime <- Tbl4_itprep %>% 
  bind_cols(as_tibble(t(apply(Tbl4_itprep, 1, Get_P)))) %>%
  arrange(desc(Shift_Perc_2022),Name_Disp)
Tbl4_2022_intime <- Tbl4_intime %>% 
  dplyr::rename(Player=Name_Disp,
                `Shift Rate (2022)`=Shift_Perc_2022,
                p=P_2022,
                Est_2022=Diff_2022) %>%
  pivot_wider(id_cols=c("Player","Shift Rate (2022)","Donors"),
              names_from="Outcome",
              values_from=c("Est_2022","p")) %>%
  dplyr::select(c("Player","Shift Rate (2022)","Donors",ends_with("OBP"),ends_with("OPS"),ends_with("wOBA")))
write.csv(x=Tbl4_2022_intime %>%
            dplyr::mutate(across(.cols=c(starts_with("Est_"),starts_with("p_")),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3))) %>%
            dplyr::mutate(`Shift Rate (2022)`=paste0(format(round(`Shift Rate (2022)`, digits=1), digits=1, nsmall=1),"%")),
          file=paste0(MSoutdir,"Table4-2022-intime.csv"),
          row.names=FALSE)
Tbl4_2023_intime <- Tbl4_intime %>% 
  dplyr::rename(Player=Name_Disp,
                `Shift Rate (2022)`=Shift_Perc_2022,
                p=P_2023,
                Est_2023=Diff_2023) %>%
  pivot_wider(id_cols=c("Player","Shift Rate (2022)","Donors"),
              names_from="Outcome",
              values_from=c("Est_2023","p")) %>%
  dplyr::select(c("Player","Shift Rate (2022)","Donors",ends_with("OBP"),ends_with("OPS"),ends_with("wOBA")))
write.csv(x=Tbl4_2023_intime %>%
            dplyr::mutate(across(.cols=c(starts_with("Est_"),starts_with("p_")),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3))) %>%
            dplyr::mutate(`Shift Rate (2022)`=paste0(format(round(`Shift Rate (2022)`, digits=1), digits=1, nsmall=1),"%")),
          file=paste0(MSoutdir,"Table4-2023-intime.csv"),
          row.names=FALSE)


### Manuscript Figure for Appx:
for (type in c(types,"2023_restrict","2023_PA325","2023_PA400")) {
  ggsave(filename=paste0(MSoutdir,"FigureA-",gsub("_","-",type),".png"),
         plot=plot_SC_ests_all("OBP", get(x=paste0("SCs_Results_",type)), LW=0.8, tagval="A. ") + 
           plot_SC_ests_all("OPS", get(x=paste0("SCs_Results_",type)), LW=0.8, tagval="B. ") + 
           plot_SC_ests_all("wOBA", get(x=paste0("SCs_Results_",type)), LW=0.8, tagval="C. ") + 
           guide_area() +
           plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
           theme(legend.position="inside",
                 legend.background=element_rect(fill="white", color="grey50"),
                 legend.text=element_text(size=rel(1.2)),
                 legend.title=element_text(size=rel(1.2)),
                 legend.direction="vertical"),
         dpi=600, width=12, height=8.1, units="in")
}
ggsave(filename=paste0(MSoutdir,"FigureA-2023-inunit.png"),
       plot=(plot_SC_ests(statval="OBP", SC.dat=SCs_Results_2023_inunit,
                          LegName="Analysis Type",
                          LegVar="Placebo_Unit",
                          LegLabs=c("In-Unit Placebo Players (15% < 2022 Shift Rate \U2264 30%)",
                                    "True Placebo Players (2022 Shift Rate \U2264 15%)"), 
                          LegBreaks=c(FALSE,TRUE),
                          LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                          LegAlpha=c(1,0.5), 
                          LegLTY=c("solid","longdash"), 
                          title=paste0("SCM estimates for OBP for all included players"),
                          LW=0.8, tagval="A. ") + 
               coord_cartesian(xlim=c(2015,2023))) +
         (plot_SC_ests(statval="OPS", SC.dat=SCs_Results_2023_inunit, 
                       LegName="Analysis Type",
                       LegVar="Placebo_Unit",
                       LegLabs=c("In-Unit Placebo Players (15% < 2022 Shift Rate \U2264 30%)",
                                 "True Placebo Players (2022 Shift Rate \U2264 15%)"), 
                       LegBreaks=c(FALSE,TRUE),
                       LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                       LegAlpha=c(1,0.5), 
                       LegLTY=c("solid","longdash"), 
                       title=paste0("SCM estimates for OPS for all included players"),
                       LW=0.8, tagval="B. ") + 
            coord_cartesian(xlim=c(2015,2023))) +
         (plot_SC_ests(statval="wOBA", SC.dat=SCs_Results_2023_inunit,
                       LegName="Analysis Type",
                       LegVar="Placebo_Unit",
                       LegLabs=c("In-Unit Placebo Players (15% < 2022 Shift Rate \U2264 30%)",
                                 "True Placebo Players (2022 Shift Rate \U2264 15%)"), 
                       LegBreaks=c(FALSE,TRUE),
                       LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                       LegAlpha=c(1,0.5), 
                       LegLTY=c("solid","longdash"), 
                       title=paste0("SCM estimates for wOBA for all included players"),
                       LW=0.8, tagval="C. ") + 
            coord_cartesian(xlim=c(2015,2023))) +
         guide_area() +
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
         theme(legend.position="inside",
               legend.background=element_rect(fill="white", color="grey50"),
               legend.text=element_text(size=rel(1.2)),
               legend.title=element_text(size=rel(1.2)),
               legend.direction="vertical"),
       dpi=600, width=12, height=8.1, units="in")
ggsave(filename=paste0(MSoutdir,"FigureA-2022-intime.png"),
       plot=(plot_SC_ests_all(statval="OBP", SC.dat=SCs_Results_2022_intime %>% dplyr::filter(Season <= 2022),
                          LW=0.8, tagval="A. ") + 
          geom_vline(xintercept=2021.5, color="grey50", linetype="dotted") +
            coord_cartesian(xlim=c(2015,2022))) +
         (plot_SC_ests_all(statval="OPS", SC.dat=SCs_Results_2022_intime %>% dplyr::filter(Season <= 2022), 
                           LW=0.8, tagval="B. ") + 
          geom_vline(xintercept=2021.5, color="grey50", linetype="dotted") +
            coord_cartesian(xlim=c(2015,2022))) +
         (plot_SC_ests_all(statval="wOBA", SC.dat=SCs_Results_2022_intime %>% dplyr::filter(Season <= 2022),
                           LW=0.8, tagval="C. ") + 
          geom_vline(xintercept=2021.5, color="grey50", linetype="dotted") +
            coord_cartesian(xlim=c(2015,2022))) +
         guide_area() +
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
         theme(legend.position="inside",
               legend.background=element_rect(fill="white", color="grey50"),
               legend.text=element_text(size=rel(1.2)),
               legend.title=element_text(size=rel(1.2)),
               legend.direction="vertical"),
       dpi=600, width=12, height=8.1, units="in")
ggsave(filename=paste0(MSoutdir,"FigureA-2023-intime.png"),
       plot=(plot_SC_ests_all(statval="OBP", SC.dat=SCs_Results_2022_intime %>% dplyr::filter(Season <= 2023),
                              LW=0.8, tagval="A. ") + 
               geom_vline(xintercept=c(2021.5,2022.5), color="grey50", linetype="dotted") +
               coord_cartesian(xlim=c(2015,2023))) +
         (plot_SC_ests_all(statval="OPS", SC.dat=SCs_Results_2022_intime %>% dplyr::filter(Season <= 2023), 
                           LW=0.8, tagval="B. ") + 
            geom_vline(xintercept=c(2021.5,2022.5), color="grey50", linetype="dotted") +
            coord_cartesian(xlim=c(2015,2023))) +
         (plot_SC_ests_all(statval="wOBA", SC.dat=SCs_Results_2022_intime %>% dplyr::filter(Season <= 2023),
                           LW=0.8, tagval="C. ") + 
            geom_vline(xintercept=c(2021.5,2022.5), color="grey50", linetype="dotted") +
            coord_cartesian(xlim=c(2015,2023))) +
         guide_area() +
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
         theme(legend.position="inside",
               legend.background=element_rect(fill="white", color="grey50"),
               legend.text=element_text(size=rel(1.2)),
               legend.title=element_text(size=rel(1.2)),
               legend.direction="vertical"),
       dpi=600, width=12, height=8.1, units="in")
ggsave(filename=paste0(MSoutdir,"FigureA-2023-low.png"),
       plot=(plot_SC_ests(statval="OBP", SC.dat=SCs_Results_2023_low,
                          LegName="Analysis Type",
                          LegVar="Placebo_Unit",
                          LegLabs=c("Target Players (2022 Shift Rate \U2265 75%)",
                                    "Placebo Players (2022 Shift Rate \U2264 10%)"), 
                          LegBreaks=c(FALSE,TRUE),
                          LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                          LegAlpha=c(1,0.5), 
                          LegLTY=c("solid","longdash"), 
                          title=paste0("SCM estimates for OBP for all included players"),
                          LW=0.8, tagval="A. ") + 
               coord_cartesian(xlim=c(2015,2023))) +
         (plot_SC_ests(statval="OPS", SC.dat=SCs_Results_2023_low, 
                       LegName="Analysis Type",
                       LegVar="Placebo_Unit",
                       LegLabs=c("Target Players (2022 Shift Rate \U2265 75%)",
                                 "Placebo Players (2022 Shift Rate \U2264 10%)"), 
                       LegBreaks=c(FALSE,TRUE),
                       LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                       LegAlpha=c(1,0.5), 
                       LegLTY=c("solid","longdash"), 
                       title=paste0("SCM estimates for OPS for all included players"),
                       LW=0.8, tagval="B. ") + 
            coord_cartesian(xlim=c(2015,2023))) +
         (plot_SC_ests(statval="wOBA", SC.dat=SCs_Results_2023_low,
                       LegName="Analysis Type",
                       LegVar="Placebo_Unit",
                       LegLabs=c("Target Players (2022 Shift Rate \U2265 75%)",
                                 "Placebo Players (2022 Shift Rate \U2264 10%)"), 
                       LegBreaks=c(FALSE,TRUE),
                       LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                       LegAlpha=c(1,0.5), 
                       LegLTY=c("solid","longdash"), 
                       title=paste0("SCM estimates for wOBA for all included players"),
                       LW=0.8, tagval="C. ") + 
            coord_cartesian(xlim=c(2015,2023))) +
         guide_area() +
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
         theme(legend.position="inside",
               legend.background=element_rect(fill="white", color="grey50"),
               legend.text=element_text(size=rel(1.2)),
               legend.title=element_text(size=rel(1.2)),
               legend.direction="vertical"),
       dpi=600, width=12, height=8.1, units="in")
ggsave(filename=paste0(MSoutdir,"FigureA-2023-high.png"),
       plot=(plot_SC_ests(statval="OBP", SC.dat=SCs_Results_2023_low,
                          LegName="Analysis Type",
                          LegVar="Placebo_Unit",
                          LegLabs=c("Target Players (2022 Shift Rate \U2265 75%)",
                                    "Placebo Players (2022 Shift Rate \U2264 25%)"), 
                          LegBreaks=c(FALSE,TRUE),
                          LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                          LegAlpha=c(1,0.5), 
                          LegLTY=c("solid","longdash"), 
                          title=paste0("SCM estimates for OBP for all included players"),
                          LW=0.8, tagval="A. ") + 
               coord_cartesian(xlim=c(2015,2023))) +
         (plot_SC_ests(statval="OPS", SC.dat=SCs_Results_2023_high, 
                       LegName="Analysis Type",
                       LegVar="Placebo_Unit",
                       LegLabs=c("Target Players (2022 Shift Rate \U2265 75%)",
                                 "Placebo Players (2022 Shift Rate \U2264 25%)"), 
                       LegBreaks=c(FALSE,TRUE),
                       LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                       LegAlpha=c(1,0.5), 
                       LegLTY=c("solid","longdash"), 
                       title=paste0("SCM estimates for OPS for all included players"),
                       LW=0.8, tagval="B. ") + 
            coord_cartesian(xlim=c(2015,2023))) +
         (plot_SC_ests(statval="wOBA", SC.dat=SCs_Results_2023_high,
                       LegName="Analysis Type",
                       LegVar="Placebo_Unit",
                       LegLabs=c("Target Players (2022 Shift Rate \U2265 75%)",
                                 "Placebo Players (2022 Shift Rate \U2264 25%)"), 
                       LegBreaks=c(FALSE,TRUE),
                       LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                       LegAlpha=c(1,0.5), 
                       LegLTY=c("solid","longdash"), 
                       title=paste0("SCM estimates for wOBA for all included players"),
                       LW=0.8, tagval="C. ") + 
            coord_cartesian(xlim=c(2015,2023))) +
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
                        Traj.dat=B.250_pool %>% dplyr::filter(Shift_Cat != "Medium"),
                        CatVar="Shift_Cat",
                        CatName=NULL,
                        CatBreaks=c("High","Low"),
                        CatLabs=c("Target Players (2022 Shift Rate \U2265 75%)",
                                  "Control Players (2022 Shift Rate \U2264 15%)"),
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
                        CatVar="Shift_Cat",
                        CatName=NULL,
                        CatBreaks=c("High","Medium","Low"),
                        CatLabs=c("Target Players (2022 Shift Rate \U2265 75%)",
                                  "Unused Players (2022 Shift Rate 15% \U2013 75%)",
                                  "Control Players (2022 Shift Rate \U2264 15%)"),
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
                        CatVar="Shift_Cat",
                        CatName=NULL,
                        CatBreaks=c("High","Medium","Low"),
                        CatLabs=c("Target Players (2022 Shift Rate \U2265 75%)",
                                  "Unused Players (2022 Shift Rate 15% \U2013 75%)",
                                  "Control Players (2022 Shift Rate \U2264 15%)"),
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
for (type in full_types) {
  for (outval in BStats %>% dplyr::filter(Use) %>% dplyr::pull(stat)) {
    ggsave(filename=paste0("figs/All-Player-SC/",gsub("_","-",type),"-",outval,".png"),
           plot=plot_SC_ests_all(outval, get(x=paste0("SCs_Results_",type))) + 
             theme(legend.position="bottom",
                   legend.background=element_rect(fill="white", color="grey50"),
                   legend.direction="horizontal"),
           dpi=600, units="in", width=8, height=5)
  }
}

### Full Set of player-specific SCM estimate plots:
for (type in types) {
  for (ID in unique(get(x=paste0("SCs_Results_",type)) %>% dplyr::filter(!Placebo_Unit) %>% 
                    pull(Player_ID))) {
    ## Info on target player:
    Target <- unique(get(x=paste0("SCs_Results_",type)) %>% 
                       dplyr::filter(Player_ID==ID) %>% pull(Name_Disp))
    print(paste0("Beginning figures for ",Target))
    Seasons <- unique(get(x=paste0("SCs_Results_",type)) %>% dplyr::filter(Player_ID==ID) %>% 
                        pull(Season))
    Targetdir <- paste0("figs/Players-SC-",gsub("_","-",type),"/",Target,"/")
    for (outval in BStats %>% dplyr::filter(Use) %>% pull(stat)) {
      ## Get data for player & placebos for that outcome only:
      SC_data <- get(x=paste0("SCs_Results_",type)) %>% 
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
                                      "Control Players (2022 Shift Rate \U2264 15%)"),
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
                                         "Placebo Players (2022 Shift Rate \U2264 15%)"), 
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

### Manuscript Figure 4:
for (type in c(types,"2023_low","2023_high","2023_restrict","2023_PA325","2023_PA400")) {
  ggsave(filename=paste0(MSoutdir,"Figure4-",gsub("_","-",type),".png"),
         plot=plot_SC_Shift("OBP", get(x=paste0("SCs_Results_",type)),
                            LW=0.8, Placebo_Inc=FALSE, tagval="A. ") + 
           plot_SC_Shift("OPS", get(x=paste0("SCs_Results_",type)),
                         LW=0.8, Placebo_Inc=FALSE, tagval="B. ") + 
           plot_SC_Shift("wOBA", get(x=paste0("SCs_Results_",type)),
                         LW=0.8, Placebo_Inc=FALSE, tagval="C. ") + 
           plot_layout(nrow=2, ncol=2, byrow=TRUE),
         dpi=600, width=12, height=8.1, units="in")
}
ggsave(filename=paste0(MSoutdir,"Figure4-2023-intime.png"),
       plot=plot_SC_Shift("OBP", SCs_Results_2022_intime %>% dplyr::filter(Season==2023),
                          LW=0.8, Placebo_Inc=FALSE, tagval="A. ") + 
         plot_SC_Shift("OPS", SCs_Results_2022_intime %>% dplyr::filter(Season==2023),
                       LW=0.8, Placebo_Inc=FALSE, tagval="B. ") + 
         plot_SC_Shift("wOBA", SCs_Results_2022_intime %>% dplyr::filter(Season==2023),
                       LW=0.8, Placebo_Inc=FALSE, tagval="C. ") + 
         plot_layout(nrow=2, ncol=2, byrow=TRUE),
       dpi=600, width=12, height=8.1, units="in")
for (type in paste(types,"full",sep="_")) {
  ggsave(filename=paste0(MSoutdir,"Figure4-",gsub("_","-",type),".png"),
         plot=plot_SC_Shift("OBP", get(x=paste0("SCs_Results_",type)),
                            LW=0.8, Placebo_Inc=TRUE, tagval="A. ") + 
           plot_SC_Shift("OPS", get(x=paste0("SCs_Results_",type)),
                         LW=0.8, Placebo_Inc=TRUE, tagval="B. ") + 
           plot_SC_Shift("wOBA", get(x=paste0("SCs_Results_",type)),
                         LW=0.8, Placebo_Inc=TRUE, tagval="C. ") + 
           plot_layout(nrow=2, ncol=2, byrow=TRUE),
         dpi=600, width=12, height=8.1, units="in")
}

### Summary Result Tables:
Outcomes <- c("OBP","OPS","wOBA")
SCs_Results_2022_intime_2022 <- SCs_Results_2022_intime %>% dplyr::filter(Season==2022)
SCs_Results_2022_intime_2023 <- SCs_Results_2022_intime %>% dplyr::filter(Season==2023)
SCs_Results_2023_24_2023 <- SCs_Results_2023_24 %>% dplyr::filter(Season==2023)
SCs_Results_2023_24_2024 <- SCs_Results_2023_24 %>% dplyr::filter(Season==2024)
SCs_Results_2023_24_Avg <- SCs_Results_2023_24 %>% 
  dplyr::filter(Season %in% c(2023,2024)) %>%
  group_by(Name,Player_ID,Outcome,Donors,Intervention,Name_Disp,Placebo_Unit,Shift_Perc_2022,Shift_Cat,Shift_Perc_Max) %>%
  dplyr::summarize(Diff=mean(Diff)) %>%
  dplyr::mutate(Season=202324)
Names <- c("2023_full", "2024_full", "2023", 
           "2023_24_2023", "2023_24_2024", "2023_24_Avg", "2024",
           "2023_inunit", "2022_intime_2022", "2022_intime_2023",
           "2023_low", "2023_high", "2023_restrict",
           "2023_PA325", "2023_PA400")
Models <- paste("SCs","Results", Names,
                sep="_")
LM_Res <- NULL
for (outcome in Outcomes) {
  Row <- sapply(Models[Models != "SCs_Results_2023_inunit"], FUN=function(x) coef(lm(Diff~Shift_Perc_2022,
                                    data=get(x) %>% dplyr::filter(Intervention, Outcome==outcome, `Shift_Perc_2022` >= 30)))["Shift_Perc_2022"]*10)
  Row["SCs_Results_2023_inunit"] <- coef(lm(Diff~Shift_Perc_2022,
                                            data=get("SCs_Results_2023_inunit") %>% dplyr::filter(Intervention, Outcome==outcome)))["Shift_Perc_2022"]*10
  names(Row) <- Names
  LM_Res <- LM_Res %>% bind_rows(Row)
}
LM_Res <- LM_Res %>% dplyr::mutate(Outcome=Outcomes) %>%
  dplyr::select(Outcome,everything())
LM_Res

Summ_Res <- NULL
for (model in Models) {
  Summ_Res <- Summ_Res %>% 
    bind_rows(get(model) %>% dplyr::filter(Intervention) %>%
                group_by(Outcome,Season,Placebo_Unit) %>%
                dplyr::summarize(Mean=mean(Diff),
                                 Median=median(Diff),
                                 SD=sd(Diff),
                                 Prop.Pos=mean(Diff > 0)) %>%
                dplyr::mutate(Model=Names[Models==model])) %>%
    dplyr::select(Model,everything())
}
Summ_Res

Means <- Summ_Res %>% 
  pivot_wider(id_cols=c("Outcome","Placebo_Unit"),
              names_from="Model",
              values_from="Mean") %>%
  dplyr::mutate(Category=factor(Placebo_Unit, levels=c(FALSE,TRUE),
                                labels=c("High-Shift","Low-Shift"))) %>%
  dplyr::select(c("Outcome","Category","2023","2024","2023_24_Avg",
                  "2023_inunit","2022_intime_2022","2023_low","2023_high",
                  "2023_restrict","2022_intime_2023",
                  "2023_PA325","2023_PA400"))
colnames(Means) <- c("Outcome","Category","2023","2024","2023-24 Average",
                     "In-Unit","In-Time","10% Threshold","25% Threshold",
                     "2018 Onward","Excluding 2022",
                     "325+ PAs","400+ PAs")
Medians <- Summ_Res %>% 
  pivot_wider(id_cols=c("Outcome","Placebo_Unit"),
              names_from="Model",
              values_from="Median") %>%
  dplyr::mutate(Category=factor(Placebo_Unit, levels=c(FALSE,TRUE),
                                labels=c("High-Shift","Low-Shift"))) %>%
  dplyr::select(c("Outcome","Category","2023","2024","2023_24_Avg",
                  "2023_inunit","2022_intime_2022","2023_low","2023_high",
                  "2023_restrict","2022_intime_2023",
                  "2023_PA325","2023_PA400"))
colnames(Medians) <- colnames(Means)
PropPos <- Summ_Res %>% 
  dplyr::mutate(`Prop.Pos`=100*`Prop.Pos`) %>%
  pivot_wider(id_cols=c("Outcome","Placebo_Unit"),
              names_from="Model",
              values_from="Prop.Pos") %>%
  dplyr::mutate(Category=factor(Placebo_Unit, levels=c(FALSE,TRUE),
                                labels=c("High-Shift","Low-Shift"))) %>%
  dplyr::select(c("Outcome","Category","2023","2024","2023_24_Avg",
                  "2023_inunit","2022_intime_2022","2023_low","2023_high",
                  "2023_restrict","2022_intime_2023",
                  "2023_PA325","2023_PA400"))
colnames(PropPos) <- colnames(Means)

write.csv(x=Means %>%
            dplyr::mutate(across(.cols=where(is.numeric),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3))),
          file=paste0(MSoutdir,"TableA5-Means.csv"),
          row.names=FALSE)
write.csv(x=Medians %>%
            dplyr::mutate(across(.cols=where(is.numeric),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3))),
          file=paste0(MSoutdir,"TableA5-Medians.csv"),
          row.names=FALSE)
write.csv(x=PropPos %>%
            dplyr::mutate(across(.cols=where(is.numeric),
                                 .fns=~paste0(format(round(.x, digits=1), digits=1, nsmall=1),"%"))),
          file=paste0(MSoutdir,"TableA5-PropPos.csv"),
          row.names=FALSE)

save(list=c("LM_Res","Summ_Res"),
     file="res/Summary_Tables.Rda")
