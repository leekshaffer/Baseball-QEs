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
load(file="res/SC-Results-Complete.Rda")

## Analysis 1: League-Wide

### Manuscript Figure 1
plot_BABIP <- plot_DIDs("BABIP", FG.dat.withCF, FullES, tagvals=c("A. ","C. "))
plot_OBP <- plot_DIDs("OBP", FG.dat.withCF, FullES, tagvals=c("B. ","D. "))

ggsave(filename=paste0(MSoutdir,"Figure1.png"),
       plot = plot_BABIP[["Trend"]] + theme(legend.position="inside",
                                            legend.position.inside=c(.184,.215),
                                            legend.background=element_rect(fill="white",
                                                                           color="grey50"),
                                            legend.title=element_text(size=rel(1.2)),
                                            legend.text=element_text(size=rel(1.2))) +
         plot_OBP[["Trend"]] + theme(legend.position="none") +
         plot_BABIP[["ES"]] + theme(legend.position="inside",
                                    legend.position.inside=c(.126,.815),
                                    legend.background=element_rect(fill="white",
                                                                   color="grey50"),
                                    legend.title=element_text(size=rel(1.2)),
                                    legend.text=element_text(size=rel(1.2))) +
         plot_OBP[["ES"]] + theme(legend.position="none") +
         plot_layout(ncol=2, nrow=2, byrow=TRUE, guides="keep"),
       # plot_layout(ncol=2, nrow=2, byrow=TRUE, guides="collect") &
       # theme(legend.position="bottom"),
       dpi=600, width=13, height=8, units="in")

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

## Analysis 2: Player-Specific
Target <- "Corey Seager"

load(file=paste0("res/Players/Player-SC-",Target,".Rda"))
Weights_Pred %>% dplyr::arrange(desc(OPS_weight))
Weights_Pred %>% dplyr::arrange(desc(wOBA_weight))
Weights_Pred %>% dplyr::arrange(desc(OBP_weight))
SCs %>% dplyr::filter(Intervention)

ggsave(filename=paste0(MSoutdir,"Figure2.png"),
       plot=plot_Comp("OBP", SCs_Results, Target, "A. ") +
         plot_Comp("OPS", SCs_Results, Target, "B. ") +
         plot_Comp("wOBA", SCs_Results, Target, "C. ") +
         guide_area() +
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
         theme(legend.background=element_rect(fill="white",
                                              color="grey50"),
               legend.text=element_text(size=rel(1.2)),
               legend.title=element_text(size=rel(1.2)),
               legend.direction="vertical"),
       dpi=600, width=13, height=8, units="in")


## Analysis 3: Full SC Results:
Tbl2 <- MSPEs_Results %>% left_join(Player_pool %>% dplyr::select(Player_ID,Shift_Perc_2022)) %>%
  dplyr::mutate(`Shift Rate (2022)`=paste0(format(round(Shift_Perc_2022,1), digits=1, nsmall=1),"%")) %>%
  dplyr::select(Name_Disp, `Shift Rate (2022)`, Outcome, Diff_2023, PVal) %>%
  dplyr::rename(Player=Name_Disp, Estimate=Diff_2023, p=PVal) %>%
  pivot_wider(names_from=Outcome, values_from=c("Estimate","p"), names_vary="slowest") %>%
  dplyr::arrange(desc(`Shift Rate (2022)`))

write.csv(x=Tbl2 %>%
            dplyr::mutate(across(.cols=-c("Player","Shift Rate (2022)"),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3))),
          file=paste0(MSoutdir,"Table2.csv"),
          row.names=FALSE)

SCs_Results %>% dplyr::filter(Season==Interv) %>%
  group_by(Outcome,Placebo_Unit) %>%
  dplyr::summarize(Mean=mean(Diff), Median=median(Diff),
                   Prop.Pos=mean(Diff > 0))

ggsave(filename=paste0(MSoutdir,"Figure3.png"),
       plot=plot_SC_ests("OBP", SCs_Results, "A. ") + 
         plot_SC_ests("OPS", SCs_Results, "B. ") + 
         plot_SC_ests("wOBA", SCs_Results, "C. ") + 
         guide_area() +
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") &
         theme(legend.position="inside",
               legend.background=element_rect(fill="white", color="grey50"),
               legend.text=element_text(size=rel(1.2)),
               legend.title=element_text(size=rel(1.2)),
               legend.direction="vertical"),
       dpi=600, width=13, height=8, units="in")
