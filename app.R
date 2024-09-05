library(shiny)
library(tidyverse)
library(DT)

## Data needed to run:
Interv <- 2023
load(file="int/Player_pool_data.Rda")
load(file="int/DID_data.Rda")
load(file="res/SC-Results-Complete.Rda")

## Lists
Player_Choices <- Player_pool %>% dplyr::filter(Shift_Cat_2022=="High") %>% pull(Name_Disp)
BStats_Use <- tibble(stat=c("OBP","OPS","wOBA"))
BStats_Use$min <- sapply(BStats_Use$stat, function(x) min(B.250_pool[B.250_pool$Shift_Cat_2022 != "Medium",x], na.rm=TRUE))
BStats_Use$max <- sapply(BStats_Use$stat, function(x) max(B.250_pool[B.250_pool$Shift_Cat_2022 != "Medium",x], na.rm=TRUE))
BStats_Use$diff_min <- sapply(BStats_Use$stat,
                              function(x) min(SCs_Results %>% dplyr::filter(Outcome==x) %>% pull(Diff), na.rm=TRUE))
BStats_Use$diff_max <- sapply(BStats_Use$stat,
                              function(x) max(SCs_Results %>% dplyr::filter(Outcome==x) %>% pull(Diff), na.rm=TRUE))
All_token <- "-All-"

## Data functions for player & outcome combination:
### Effect Estimates & P-Values Table:
ests_tbl <- function(display_name,statval) {
  if (display_name==All_token) {
    Tbl <- SCs_Results %>% dplyr::filter(Outcome %in% BStats_Use$stat & 
                                           Intervention & !(Placebo_Unit)) %>% 
      dplyr::select(Name_Disp,Outcome,Season,Observed,Synthetic,Diff) %>%
      left_join(MSPEs_Results %>% dplyr::select(Name_Disp,Outcome,PVal), by=c("Name_Disp","Outcome")) %>%
      dplyr::rename(Player=Name_Disp, `Observed Value`=Observed, `Synthetic Control Value`=Synthetic,
                    `Effect Estimate`=Diff, `Placebo P-Value`=PVal) %>%
      dplyr::mutate(across(.cols=-c("Player","Outcome","Season"),
                           .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
  } else {
    Tbl <- SCs_Results %>% dplyr::filter(Outcome %in% BStats_Use$stat & Name_Disp==display_name & Intervention) %>% 
      dplyr::select(Name_Disp,Outcome,Season,Observed,Synthetic,Diff) %>%
      left_join(MSPEs_Results %>% dplyr::select(Name_Disp,Outcome,PVal), by=c("Name_Disp","Outcome")) %>%
      dplyr::rename(Player=Name_Disp, `Observed Value`=Observed, `Synthetic Control Value`=Synthetic,
                    `Effect Estimate`=Diff, `Placebo P-Value`=PVal) %>%
      dplyr::mutate(across(.cols=-c("Player","Outcome","Season"),
                           .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
  }
  if (statval==All_token) {
    Tbl %>% dplyr::arrange(Player,Season,Outcome)
  } else {
    Tbl %>% dplyr::filter(Outcome==statval) %>% dplyr::arrange(Player,Season)
  }
}

DID_tbl <- function(statval) {
  if (statval==All_token) {
    statval <- BStats_Use$stat
  }
  Tbl <- TwoByTwo %>% dplyr::select(c("Batter",starts_with(statval)))
  colnames(Tbl) <- c("Batter Handedness",
                     paste(rep(statval, each=3),
                         c("2022","2023","Difference"),
                         sep=" "))
  Tbl %>% dplyr::mutate(across(.cols=-c("Batter Handedness"),
                               .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
}

wts_tbl <- function(display_name,statval) {
  load(file=paste0("res/Players/Player-SC-",display_name,".Rda"))
  Tbl <- Weights_Unit %>% dplyr::rename(`Control Player`=unit,
                                        `wOBA Weight`=wOBA_weight,
                                        `OBP Weight`=OBP_weight,
                                        `OPS Weight`=OPS_weight)
  if (statval==All_token) {
    statval <- BStats_Use$stat
  }
  Tbl %>% dplyr::select(c("Control Player",paste(statval,"Weight", sep=" "))) %>%
      arrange(across(-c("Control Player"), desc)) %>%
      dplyr::mutate(across(.cols=-c("Control Player"),
                           .fns=~paste0(format(round(.x*100, digits=2), digits=2, nsmall=2),"%")))
}

plot_spag <- function(display_name,statval) {
  if (display_name==All_token) {
    ggplot(SCs_Results %>% dplyr::filter(Outcome==statval) %>%
             dplyr::mutate(Type=if_else(Placebo_Unit,"Donor","Target")), 
           mapping=aes(x=Season, y=Observed, group=Player_ID,
                                    color=Type, alpha=Type, linetype=Type)) +
      geom_line(linewidth=1) + geom_point() +
      scale_x_continuous(name="Season",
                         breaks=2015:2023,
                         minor_breaks=NULL) +
      scale_y_continuous(name=statval,
                         limits=unlist(BStats_Use[BStats_Use$stat==statval,c("min","max")])) +
      geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
      scale_color_brewer(name="",
                         type="qual", palette="Dark2",
                         direction=1,
                         breaks=c("Target","Donor"),
                         labels=c("Target Players (2022 Shift Rate \U2265 80%)",
                                  "Control Players (2022 Shift Rate \U2264 20%)")) +
      scale_alpha_manual(name="",
                         values=c(1,0.4),
                         breaks=c("Target","Donor"),
                         labels=c("Target Players (2022 Shift Rate \U2265 80%)",
                                  "Control Players (2022 Shift Rate \U2264 20%)")) +
      scale_linetype_manual(name="",
                            values=c("solid","longdash"),
                            breaks=c("Target","Donor"),
                            labels=c("Target Players (2022 Shift Rate \U2265 80%)",
                                     "Control Players (2022 Shift Rate \U2264 20%)")) +
      theme_bw() + theme(legend.position="bottom") +
      coord_cartesian(ylim=unlist(BStats_Use[BStats_Use$stat==statval,c("min","max")])) +
      labs(title=paste0(statval," by player and season for all included players"))
  } else {
    load(file=paste0("res/Players/Player-SC-",display_name,".Rda"))
    SC_use <- B.250_pool %>% dplyr::filter(Name_Disp %in% c(display_name,Weights_Unit$unit),
                                           Season %in% unique(SCs$Season)) %>%
      dplyr::mutate(Type=if_else(Name_Disp==display_name,"Target","Donor"))
    ggplot(SC_use, mapping=aes(x=Season, y=get(statval), group=Player_ID, 
                                color=Type, alpha=Type, linetype=Type)) +
      geom_line(linewidth=1.2) + geom_point() +
      scale_x_continuous(name="Season",
                         breaks=2015:2023,
                         minor_breaks=NULL) +
      scale_y_continuous(name=statval,
                         limits=unlist(BStats_Use[BStats_Use$stat==statval,c("min","max")])) +
      geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
      scale_color_brewer(name="",
                         type="qual", palette="Dark2",
                         direction=1,
                         breaks=c("Target","Donor"),
                         labels=c(paste0("Target Player: ",display_name),
                                  "Control Players (2022 Shift Rate \U2264 20%)")) +
      scale_alpha_manual(name="",
                         values=c(1,0.4),
                         breaks=c("Target","Donor"),
                         labels=c(paste0("Target Player: ",display_name),
                                  "Control Players (2022 Shift Rate \U2264 20%)")) +
      scale_linetype_manual(name="",
                         values=c("solid","longdash"),
                         breaks=c("Target","Donor"),
                         labels=c(paste0("Target Player: ",display_name),
                                  "Control Players (2022 Shift Rate \U2264 20%)")) +
      theme_bw() + theme(legend.position="bottom") +
      coord_cartesian(ylim=unlist(BStats_Use[BStats_Use$stat==statval,c("min","max")])) +
      labs(title=paste0(statval," by player and season for ",display_name," and controls"))
  }
}

plot_trend <- function(display_name,statval) {
  if (display_name==All_token) {
    return()
  } else {
    load(file=paste0("res/Players/Player-SC-",display_name,".Rda"))
    ggplot(data=SCs %>% pivot_longer(cols=c("Observed","Synthetic","Diff"), 
                                                  names_to="Result", values_to="Value") %>% 
                          dplyr::filter(Outcome==statval, Result != "Diff"), 
                        mapping=aes(x=Season, y=Value, 
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
                         limits=unlist(BStats_Use[BStats_Use$stat==statval,c("min","max")])) +
      coord_cartesian(ylim=unlist(BStats_Use[BStats_Use$stat==statval,c("min","max")])) +
      geom_vline(xintercept=Interv-0.5, color="grey50", linetype="dashed") +
      labs(title=paste0("Synthetic and observed ",statval," for ",display_name))
  }
}

plot_diff <- function(display_name,statval) {
  if (display_name==All_token) {
    SC_use <- SCs_Results %>% dplyr::filter(Outcome==statval)
    TargLabel <- "Target Players (2022 Shift Rate \U2265 80%)"
    LW <- 1
  } else {
    SC_use <- SCs_Results %>% 
      dplyr::filter(Outcome==statval & Name_Disp==display_name) %>%
      bind_rows(SCs_Results %>% dplyr::filter(Outcome==statval & Placebo_Unit))
    TargLabel <- paste0("Target Player: ",display_name)
    LW <- 1.2
  }
    ggplot(data=SC_use,
                        mapping=aes(x=Season, y=Diff, group=Player_ID,
                                    color=Placebo_Unit, alpha=Placebo_Unit,
                                    linetype=Placebo_Unit)) +
      geom_line(linewidth=LW) +
      scale_x_continuous(name="Season",
                         breaks=2015:2023,
                         minor_breaks=NULL) +
      scale_y_continuous(name="Difference, Synthetic - Observed",
                         limits=c(BStats_Use$diff_min[BStats_Use$stat==statval],
                                  BStats_Use$diff_max[BStats_Use$stat==statval])) +
      coord_cartesian(ylim=c(BStats_Use$diff_min[BStats_Use$stat==statval],
                             BStats_Use$diff_max[BStats_Use$stat==statval])) +
      scale_color_brewer(name="",
                         type="qual", palette="Dark2",
                         direction=-1,
                         breaks=c(FALSE,TRUE),
                         labels=c(TargLabel,
                                  "Placebo Players (2022 Shift Rate \U2264 20%)")) +
      scale_alpha_manual(name="",
                         breaks=c(FALSE,TRUE),
                         labels=c(TargLabel,
                                  "Placebo Players (2022 Shift Rate \U2264 20%)"),
                         values=c(1,0.4)) +
      scale_linetype_manual(name="",
                         breaks=c(FALSE,TRUE),
                         labels=c(TargLabel,
                                  "Placebo Players (2022 Shift Rate \U2264 20%)"),
                         values=c("solid","longdash")) +
      geom_vline(xintercept=Interv-0.5,
                 color="grey50", linetype="dashed") +
      theme_bw() + theme(legend.position="bottom") +
      labs(title=paste0("SCM estimates for ",statval," for ",
                        if_else(display_name==All_token, "all included players", paste0(display_name," and placebos"))))
}


## UI:
ui <- fluidPage(
  
  # Application title
  titlePanel("Effect of the Shift Ban on Highly-Shifted MLB Players, 2023"),
  sidebarLayout(
    sidebarPanel(
      selectInput("InName", label = h3("Choose Player"), 
                  choices = as.list(c(All_token,Player_Choices)), 
                  selected = "Corey Seager"),
      selectInput("InStat", label = h3("Choose Outcome"),
                  choices = as.list(c(All_token,BStats_Use$stat))),
      # h4("Created by Lee Kennedy-Shaffer, 2024"),
      # h5("Code Available:"),
      # h6("https://github.com/leekshaffer/baseball-qes"),
      h5("Data Sources:"),
      h6("Baseball Savant Custom Leaderboard, https://baseballsavant.mlb.com/leaderboard/custom;"),
      h6("Baseball Savant Batter Positioning Leaderboard, https://baseballsavant.mlb.com/visuals/batter-positioning;")
    ),
    mainPanel(
      tabPanel(title="Effect Estimate(s)", 
               DT::dataTableOutput('tbl1')),
      tabPanel(title="SCM Weights", 
               DT::dataTableOutput('tbl2')),
      plotOutput("plot1", width="95%"),
      plotOutput("plot2", width="95%"),
      plotOutput("plot3", width="95%")
    )
  )
)

## Server:
server <- function(input, output) {
  output$tbl1 <- DT::renderDataTable({
    if (input$InName==All_token) {
      DT::datatable(ests_tbl(input$InName, input$InStat), 
                    options = list(lengthMenu = list(c(3, 6, 9, -1), c('3', '6', '9', 'All')),
                                   pageLength = 6))
    } else {
      DT::datatable(ests_tbl(input$InName, input$InStat), 
                    options = list(paging = FALSE,
                                   searching = FALSE))
    }
  })
  
  output$tbl2 <- DT::renderDataTable({
    if (input$InName==All_token) {
      DT::datatable(DID_tbl(input$InStat),
                    options = list(paging = FALSE,
                                   searching = FALSE))
    } else {
      DT::datatable(wts_tbl(input$InName, input$InStat), 
                    options = list(lengthMenu = list(c(3, 6, 9, -1), c('3', '6', '9', 'All')),
                                   pageLength = 3))
    }
  })
  
  output$plot1 <- renderPlot({
    if (input$InStat==All_token) {
      plot_diff(display_name=input$InName,
                statval=BStats_Use$stat[1])
    } else {
      plot_spag(display_name=input$InName,
                statval=input$InStat)
    }
  })
  
  output$plot2 <- renderPlot({
    if (input$InStat==All_token) {
      plot_diff(display_name=input$InName,
                statval=BStats_Use$stat[2])
    } else {
      if (input$InName==All_token) {
        plot_diff(display_name=input$InName,
                  statval=input$InStat)
      } else {
        plot_trend(display_name=input$InName,
                  statval=input$InStat)
      }
    }
  })
  
  output$plot3 <- renderPlot({
    if (input$InStat==All_token) {
      plot_diff(display_name=input$InName,
                statval=BStats_Use$stat[3])
    } else {
      if (input$InName==All_token) {
      } else {
      plot_diff(display_name=input$InName,
                 statval=input$InStat)
      }
    }
  })
}

## Run the App:
shinyApp(ui = ui, server = server)


