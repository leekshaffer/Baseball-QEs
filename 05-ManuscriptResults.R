## 04-ManuscriptResults.R
## Creation of figures, tables, and key results for manuscript

require(tidyverse)
require(patchwork)
require(RColorBrewer)

outdir <- "figs/Manuscript/"
Interv <- 2023

## Analysis 1: League-Wide
load(file="int/DID_data.Rda")

plot_DIDs <- function(statval, tagvals=NULL) {
  plot_trend <- ggplot(data=FG.dat.withCF %>% dplyr::filter(Season != 2020 & Season <= Interv),
                       mapping=aes(x=Season, y=get(statval), 
                                   group=Batter, color=Batter, linetype=Batter,
                                   shape=Batter)) +
    geom_line(linewidth=1.2) + geom_point(size=2.2) +
    theme_bw() + theme(legend.position="bottom") +
    scale_color_manual(name="Batter Handedness",
                       values=c("#a6611a","#92c5de","#a6611a"),
                       breaks=c("LHB","RHB","Counterfactual LHB")) +
    scale_linetype_manual(name="Batter Handedness",
                          values=c("solid","dotted","dotted"),
                          breaks=c("LHB","RHB","Counterfactual LHB")) +
    scale_shape_manual(name="Batter Handedness",
                       values=c(15,17,15),
                       breaks=c("LHB","RHB","Counterfactual LHB")) +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
    labs(title=paste0(tagvals[1],"Trend in ",statval," by batter handedness, bases empty"),
         y=statval)
  
  plot_ES <- ggplot(data=FullES %>% dplyr::filter(Season != 2020 & Season <= Interv), 
                    mapping=aes(x=Season, y=get(paste0(statval,"_ES")), 
                                color=Type, shape=Type)) +
    geom_point(size=2.8) +
    geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
    geom_hline(yintercept=0, color="grey50", linetype="dashed") +
    theme_bw() + theme(legend.position="bottom") +
    # scale_color_brewer(name="Analysis Type",
    #                    type="qual", palette="Dark2",
    #                    direction=-1,
    #                    breaks=c("Placebo","Intervention")) +
    scale_color_manual(name="Analysis Type",
                       values=brewer.pal(3,"Dark2")[c(1,3)],
                       breaks=c("Placebo","Intervention")) +
    scale_shape_manual(name="Analysis Type",
                       values=c(18,19),
                       breaks=c("Placebo","Intervention")) +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    coord_cartesian(ylim=c(-.01,.01)) +
    labs(y=paste0("DID Estimate for ",statval),
         title=paste0(tagvals[2],"DID analysis for ",statval,", consecutive seasons"))
  return(list(Trend=plot_trend, 
              ES=plot_ES))
    
}

plot_BABIP <- plot_DIDs("BABIP", tagvals=c("A. ","C. "))
plot_OBP <- plot_DIDs("OBP", tagvals=c("B. ","D. "))

ggsave(filename=paste0(outdir,"Figure1.png"),
       plot = plot_BABIP[["Trend"]] + 
         plot_OBP[["Trend"]] + 
         plot_BABIP[["ES"]] +
         plot_OBP[["ES"]] + 
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
load(file="res/SC-Results-Complete.Rda")
Target <- "Corey Seager"

load(file=paste0("res/Players/Player-SC-",Target,".Rda"))
Weights_Pred %>% dplyr::arrange(desc(OPS_weight))
Weights_Pred %>% dplyr::arrange(desc(wOBA_weight))
Weights_Pred %>% dplyr::arrange(desc(OBP_weight))

plot_Comp <- function(statval, display_name, tagval=NULL) {
  SCs_targ <- SCs_Results %>% 
    dplyr::filter(Outcome==statval & Name_Disp==display_name) %>%
    pivot_longer(cols=c("Observed","Synthetic"),
                 names_to="Result", values_to="Value")
  ggplot(data=SCs_targ,
         mapping=aes(x=Season, y=Value, 
                     group=Result, linetype=Result, color=Result, shape=Result)) +
    geom_line(linewidth=1.2) + geom_point(size=2.4) +
    theme_bw() + theme(legend.position="bottom") +
    scale_color_manual(name=NULL,
                       values=c("#002d72","#ff5910"),
                       breaks=c("Observed","Synthetic")) +
    scale_linetype_manual(name=NULL,
                          values=c("solid","dotted"),
                          breaks=c("Observed","Synthetic")) +
    scale_shape_manual(name=NULL,
                       values=c(15,17),
                       breaks=c("Observed","Synthetic")) +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    scale_y_continuous(name=statval,
                       limits=unlist(BStats[BStats$stat==statval,c("min","max")])) +
    coord_cartesian(ylim=unlist(BStats[BStats$stat==statval,c("min","max")])) +
    geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
    labs(title=paste0(tagval,"Synthetic and Observed ",statval," for ",display_name))
}

ggsave(filename=paste0(outdir,"Figure2.png"),
       plot=plot_Comp("OBP", Target, "A. ") + 
         plot_Comp("OPS", Target, "B. ") + 
         plot_Comp("wOBA", Target, "C. ") + 
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") & 
         theme(legend.position="bottom"),
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
          file=paste0(outdir,"Table2.csv"),
          row.names=FALSE)

SCs_Results %>% dplyr::filter(Season==Interv) %>%
  group_by(Outcome,Placebo_Unit) %>%
  dplyr::summarize(Mean=mean(Diff), Median=median(Diff),
                   Prop.Pos=mean(Diff > 0))

plot_SC_ests <- function(statval, tagval=NULL) {
  ggplot(data=SCs_Results %>% dplyr::filter(Outcome==statval),
         mapping=aes(x=Season, y=Diff, group=Player_ID,
                     color=Placebo_Unit, alpha=Placebo_Unit,
                     linetype=Placebo_Unit)) +
    geom_line() +
    scale_x_continuous(name="Season",
                       breaks=2015:2023, minor_breaks=NULL) +
    scale_y_continuous(name="Difference, Synthetic - Observed",
                       limits=unlist(BStats[BStats$stat==statval,c("diff_min","diff_max")])) +
    coord_cartesian(ylim=unlist(BStats[BStats$stat==statval,c("diff_min","diff_max")])) +
    scale_color_manual(name=NULL,
                       breaks=c(FALSE,TRUE),
                       labels=c("Target Players (2022 Shift Rate \U2265 80%)",
                                "Placebo Players (2022 Shift Rate \U2264 20%)"),
                       values=brewer.pal(3, "Dark2")[c(3,1)]) +
    scale_alpha_manual(name=NULL,
                       breaks=c(FALSE,TRUE),
                       labels=c("Target Players (2022 Shift Rate \U2265 80%)",
                                "Placebo Players (2022 Shift Rate \U2264 20%)"),
                       values=c(1,0.5)) +
    scale_linetype_manual(name=NULL,
                          breaks=c(FALSE,TRUE),
                          labels=c("Target Players (2022 Shift Rate \U2265 80%)",
                                   "Placebo Players (2022 Shift Rate \U2264 20%)"),
                          values=c("solid","longdash")) +
    geom_vline(xintercept=Interv-0.5,
               color="grey50", linetype="dashed") +
    theme_bw() + theme(legend.position="bottom") +
    labs(title=paste0(tagval,"SCM estimates for ",statval," for all included players"))
}

ggsave(filename=paste0(outdir,"Figure3.png"),
       plot=plot_SC_ests("OBP", "A. ") + 
         plot_SC_ests("OPS", "B. ") + 
         plot_SC_ests("wOBA", "C. ") + 
         plot_layout(nrow=2, ncol=2, byrow=TRUE, guides="collect") & 
         theme(legend.position="bottom"),
       dpi=600, width=13, height=8, units="in")
