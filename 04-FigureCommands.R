## 04-FigureCommands.R
## Functions to generate figures from results

require(tidyverse)
require(RColorBrewer)
Interv <- 2023

## DID Analysis Plots:

plot_DIDs <- function(statval, DID.CF.dat, DID.ES.dat, tagvals=NULL) {
  plot_trend <- ggplot(data=DID.CF.dat %>% dplyr::filter(Season != 2020 & Season <= Interv),
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
  
  plot_ES <- ggplot(data=DID.ES.dat %>% dplyr::filter(Season != 2020 & Season <= Interv), 
                    mapping=aes(x=Season, y=get(paste0(statval,"_ES")), 
                                color=Type, shape=Type)) +
    geom_point(size=2.8) +
    geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
    geom_hline(yintercept=0, color="grey50", linetype="dashed") +
    theme_bw() + theme(legend.position="bottom") +
    scale_color_manual(name="Analysis Type",
                       values=brewer.pal(3,"Dark2")[c(3,1)],
                       breaks=c("Intervention","Placebo")) +
    scale_shape_manual(name="Analysis Type",
                       values=c(19,18),
                       breaks=c("Intervention","Placebo")) +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL) +
    coord_cartesian(ylim=c(-.01,.01)) +
    labs(y=paste0("DID Estimate for ",statval),
         title=paste0(tagvals[2],"DID analysis for ",statval,", consecutive seasons"))
  return(list(Trend=plot_trend, 
              ES=plot_ES))
  
}

## SC Analysis Plots:

### Overall Outcome Trajectory Plots by Player:
plot_Traj <- function(statval, Traj.dat, 
                      CatVar, CatName, CatBreaks, CatLabs,
                      CatCols, CatLTY, CatAlpha,
                      Type="Average", Fixed=TRUE, 
                      Target=NULL, tagval=NULL) {
  ## Type can be "Average", "All", or "Target"
  if (Type=="Average") {
    plot <- ggplot(data=Traj.dat,
                   mapping=aes(x=Season,
                               y=get(statval),
                               group=get(CatVar),
                               alpha=get(CatVar),
                               color=get(CatVar),
                               linetype=get(CatVar)))
    LW <- 1.2
  } else {
    plot <- ggplot(data=Traj.dat,
                   mapping=aes(x=Season,
                               y=get(statval),
                               group=Player_ID,
                               alpha=get(CatVar),
                               color=get(CatVar),
                               linetype=get(CatVar)))
    LW <- 1
  }
  plot <- plot + geom_line(linewidth=LW) + geom_point() +
    scale_x_continuous(name="Season",
                       breaks=2015:2023,
                       minor_breaks=NULL)
  if (Fixed) {
    plot <- plot +
      scale_y_continuous(name=paste0("Average player ",statval),
                         limits=unlist(BStats[BStats$stat==statval,c("min","max")])) +
      coord_cartesian(ylim=unlist(BStats[BStats$stat==statval,c("min","max")]))
      
  } else {
    plot <- plot +
      scale_y_continuous(name=paste0("Average player ",statval))
  }
    plot <- plot + 
      geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
      scale_color_manual(name=CatName,
                         values=CatCols,
                         breaks=CatBreaks,
                         labels=CatLabs) +
      scale_alpha_manual(name=CatName,
                         values=CatAlpha,
                         breaks=CatBreaks,
                         labels=CatLabs) +
      scale_linetype_manual(name=CatName,
                            values=CatLTY,
                            breaks=CatBreaks,
                            labels=CatLabs) +
      theme_bw() +
      theme(legend.position="bottom")
    if (Type=="Average") {
      plot <- plot +
        labs(title=paste0(tagval,"Average player ", statval," by category and season, min. 250 PA"))
    } else if (Type=="Target") {
      plot <- plot + 
        labs(title=paste0(tagval, statval," by player and season for ",Target," and controls"))
    } else {
      plot <- plot +
        labs(title=paste0(tagval, statval," by player and season, min. 250 PA"))
    }
    return(plot)
}

### Comparison Plots (Synthetic & Observed Outcomes for a player):
plot_Comp <- function(statval, SC.dat, display_name, tagval=NULL) {
  SCs_targ <- SC.dat %>% 
    dplyr::filter(Outcome==statval & Name_Disp==display_name) %>%
    pivot_longer(cols=c("Observed","Synthetic"),
                 names_to="Result", values_to="Value")
  ggplot(data=SCs_targ,
         mapping=aes(x=Season, y=Value, 
                     group=Result, linetype=Result, color=Result, shape=Result)) +
    geom_line(linewidth=1.2) + geom_point(size=2.4) +
    theme_bw() + theme(legend.position="bottom") +
    scale_color_manual(name="Outcome Value",
                       values=c("#002d72","#ff5910"),
                       breaks=c("Observed","Synthetic")) +
    scale_linetype_manual(name="Outcome Value",
                          values=c("solid","dotted"),
                          breaks=c("Observed","Synthetic")) +
    scale_shape_manual(name="Outcome Value",
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

### SC Estimates/Differences Plots (Synthetic-Observed):
plot_SC_ests <- function(statval, SC.dat, 
                         LegName, LegVar, LegLabs, LegBreaks,
                         LegCols, LegAlpha, LegLTY, title,
                         LW=1,
                         tagval=NULL) {
  ggplot(data=SC.dat %>% dplyr::filter(Outcome==statval),
         mapping=aes(x=Season, y=Diff, group=Player_ID,
                     color=get(LegVar), alpha=get(LegVar),
                     linetype=get(LegVar))) +
    geom_line(linewidth=LW) +
    scale_x_continuous(name="Season",
                       breaks=2015:2023, minor_breaks=NULL) +
    scale_y_continuous(name="Difference, Synthetic - Observed",
                       limits=unlist(BStats[BStats$stat==statval,c("diff_min","diff_max")])) +
    coord_cartesian(ylim=unlist(BStats[BStats$stat==statval,c("diff_min","diff_max")])) +
    scale_color_manual(name=LegName,
                       breaks=LegBreaks,
                       labels=LegLabs,
                       values=LegCols) +
    scale_alpha_manual(name=LegName,
                       breaks=LegBreaks,
                       labels=LegLabs,
                       values=LegAlpha) +
    scale_linetype_manual(name=LegName,
                          breaks=LegBreaks,
                          labels=LegLabs,
                          values=LegLTY) +
    geom_vline(xintercept=Interv-0.5,
               color="grey50", linetype="dashed") +
    theme_bw() + theme(legend.position="bottom") +
    labs(title=paste0(tagval,title))
}

plot_SC_ests_all <- function(statval, SC.dat, tagval=NULL) {
  plot_SC_ests(statval, SC.dat,
               LegName="Analysis Type",
               LegVar="Placebo_Unit",
               LegLabs=c("Target Players (2022 Shift Rate \U2265 80%)",
                         "Placebo Players (2022 Shift Rate \U2264 20%)"), 
               LegBreaks=c(FALSE,TRUE),
               LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
               LegAlpha=c(1,0.5), 
               LegLTY=c("solid","longdash"), 
               title=paste0("SCM estimates for ",statval," for all included players"),
               LW=1,
               tagval)
}
