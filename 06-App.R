library(shiny)
library(tidyverse)
library(DT)
library(bslib)

## Data needed to run:
Interv <- 2023
types <- c("2023","2024","2023_24") ## The analysis year types
load(file="int/Player_pool_data.Rda")
load(file="int/DID_data.Rda")
for (type in c(types, paste(types,"full",sep="_"))) {
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
source("./04-FigureCommands.R", local=TRUE)

## Lists
Player_Choices <- unique(bind_rows(Player_pool_2023,Player_pool_2023_24,Player_pool_2024) %>% 
  dplyr::filter(Shift_Cat=="High") %>% pull(Name_Disp))
BStats_Use <- BStats %>% dplyr::filter(Use)
All_token_SC <- "-All-"
All_token_DID <- "-All (Table Only)-"

## Data functions for player & outcome combination:
### Get player info from name:
player_info <- function(display_name,targS) {
  B.250_row <- B.250_pool %>% dplyr::filter(Season==2022 & Name_Disp==display_name)
  Pool_row <- get(paste0("Player_pool_",gsub("-","_",targS))) %>% 
    dplyr::filter(Name_Disp==display_name)
  list(First=B.250_row$name_first, Last=B.250_row$name_last,
       Shift_Perc_2022=Pool_row$Shift_Perc_2022,
       Shade_Perc_2023=Pool_row$Shade_Perc_2023,
       Shade_Perc_2024=Pool_row$Shade_Perc_2024,
       FG_ID=B.250_row$key_fangraphs,
       BR_ID=B.250_row$key_bbref,
       MLB_ID=B.250_row$Player_ID,
       LastInit=tolower(substr(B.250_row$name_last, 1, 1)),
       BR_URL=paste0("https://www.baseball-reference.com/players/",
                     tolower(substr(B.250_row$name_last, 1, 1)),
                     "/", B.250_row$key_bbref, ".shtml"),
       FG_URL=paste0("https://www.fangraphs.com/players/",
                     B.250_row$name_first,"-",B.250_row$name_last,
                     "/",B.250_row$key_fangraphs, "/stats"),
       MLB_BPL_URL=paste0("https://baseballsavant.mlb.com/visuals/batter-positioning?playerId=",
                          B.250_row$Player_ID,
                          "&teamId=&opponent=&firstBase=0&shift=1&season=2022&attempts=250"))
}

### Effect Estimates & P-Values Table:
ests_tbl <- function(display_name,statval,targS) {
  if (display_name==All_token_SC) {
    SCs_Res_int <- get(paste0("SCs_Results_", gsub("-","_",targS), "_full"))
    MSPEs_Res_int <- get(paste0("MSPEs_Results_", gsub("-","_",targS), "_full"))
    Tbl <- SCs_Res_int %>% dplyr::filter(Outcome %in% BStats_Use$stat & 
                                           Intervention & !(Placebo_Unit)) %>% 
      dplyr::select(Name_Disp,Shift_Perc_2022,Outcome,Season,Observed,Synthetic,Diff) %>%
      left_join(MSPEs_Res_int %>% dplyr::select(Name_Disp,Outcome,PVal), by=c("Name_Disp","Outcome")) %>%
      dplyr::rename(Player=Name_Disp, `2022 Shifts (%)`=Shift_Perc_2022, `Observed Value`=Observed, 
                    `Synthetic Control Value`=Synthetic,
                    `Effect Estimate`=Diff, `Placebo P-Value`=PVal) %>%
      dplyr::mutate(across(.cols=-c("Player","Outcome","Season"),
                           .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
    if (statval==All_token_SC) {
      Tbl %>% dplyr::arrange(desc(`2022 Shifts (%)`),Player,Season,Outcome)
    } else {
      Tbl %>% dplyr::filter(Outcome==statval) %>% dplyr::arrange(desc(`2022 Shifts (%)`),Player,Season)
    }
  } else {
    SCs_Res_int <- get(paste0("SCs_Results_",gsub("-","_",targS)))
    MSPEs_Res_int <- get(paste0("MSPEs_Results_",gsub("-","_",targS)))
    Tbl <- SCs_Res_int %>% dplyr::filter(Outcome %in% BStats_Use$stat & Name_Disp==display_name & Intervention) %>% 
      dplyr::select(Name_Disp,Outcome,Season,Observed,Synthetic,Diff) %>%
      left_join(MSPEs_Res_int %>% dplyr::select(Name_Disp,Outcome,PVal), by=c("Name_Disp","Outcome")) %>%
      dplyr::rename(Player=Name_Disp,
                    `Observed Value`=Observed, `Synthetic Control Value`=Synthetic,
                    `Effect Estimate`=Diff, `Placebo P-Value`=PVal) %>%
      dplyr::mutate(across(.cols=-c("Player","Outcome","Season"),
                           .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
    if (statval==All_token_SC) {
      Tbl %>% dplyr::arrange(Player,Season,Outcome)
    } else {
      Tbl %>% dplyr::filter(Outcome==statval) %>% dplyr::arrange(Player,Season)
    }
  }
}

DID_tbl <- function(statval) {
  if (statval==All_token_DID) {
    Tbl <- TwoByTwo %>% pivot_longer(cols=-c("Batter"), names_to=c("Outcome","Year"), 
                                     names_sep="_", values_to="Value") %>% 
      pivot_wider(id_cols=c("Outcome","Batter"), names_from="Year", values_from="Value") %>%
      dplyr::rename(`Average, 2022`=`2022`,
                    `Average, 2023`=`2023`,
                    `Average, 2024`=`2024`,
                    "Diff., 2023 \U2212 2022"=`Diff-2023`,
                    "Diff., 2024 \U2212 2023"=`Diff-2024`) %>%
      dplyr::arrange(Outcome) %>%
      dplyr::mutate(across(.cols=-c("Outcome","Batter"),
                           .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
  } else {
    Tbl <- TwoByTwo %>% dplyr::select(c("Batter",starts_with(statval)))
    colnames(Tbl) <- c("Batter Handedness",
                       paste(rep(statval, each=3),
                             c("2022","2023","2024","Diff., 2023 \U2212 2022","Diff., 2024 \U2212 2023"),
                             sep=" "))
    Tbl <- Tbl %>% dplyr::mutate(across(.cols=-c("Batter Handedness"),
                                        .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
  }
  return(Tbl)
}

wts_tbl <- function(display_name,statval,targS) {
  load(file=paste0("res/Players-SC-", targS, "-full",
                   "/Player-SC-", display_name, ".Rda"))
  Tbl <- Weights_Unit %>% dplyr::rename(`Control Player`=unit,
                                        `wOBA Weight`=wOBA_weight,
                                        `OBP Weight`=OBP_weight,
                                        `OPS Weight`=OPS_weight)
  if (statval==All_token_SC) {
    statval <- BStats_Use$stat
  }
  Tbl %>% dplyr::select(c("Control Player",paste(statval,"Weight", sep=" "))) %>%
    arrange(across(-c("Control Player"), desc)) %>%
    dplyr::mutate(across(.cols=-c("Control Player"),
                         .fns=~paste0(format(round(.x*100, digits=2), digits=2, nsmall=2),"%")))
}

## UI:
ui <- fluidPage(
  tabsetPanel(
    id = "tabset",
    tabPanel("Analysis 1 (DID)", 
             page_sidebar(
               title = "Effect of the Shift Ban on Left-Handed MLB Batters",
               sidebar=sidebar(
                 selectInput("DIDInStat", label = h4("Choose Outcome"),
                             choices = as.list(c(All_token_DID,BStats$stat)),
                             selected = All_token_DID),
                     h5("Data Sources:"),
                     a("FanGraphs Splits Leaderboard",
                       href="https://www.fangraphs.com/leaders/splits-leaderboards")
               ),
               accordion(
                 open = c("Table 1: Two-By-Two DID Results"),
                 accordion_panel("Table 1: Two-By-Two DID Results",
                                 DT::dataTableOutput("DIDtbl1")),
                 accordion_panel("Plot 1: Outcome Value Trends By Batter Handedness",
                                 plotOutput("DIDplot1", width="95%")),
                 accordion_panel("Plot 2: DID Event Study Estimates",
                                 plotOutput("DIDplot2", width="95%"))
               )
             )
    ),
    
    tabPanel("Analysis 2 (SCM): All Players",
             page_sidebar(
               title = "Effect of the Shift Ban on Shifted MLB Players",
               sidebar=sidebar(
                 selectInput("SCAllInStat", label = h4("Choose Outcome"),
                             choices = as.list(c(BStats_Use$stat)),
                             selected = "OBP"),
                 selectInput("AllTargetSeason", label = h4("Choose Target Season"),
                             choices = as.list(gsub("_","-",types)),
                             selected = "2023"),
                 # h4("Created by Lee Kennedy-Shaffer, 2024"),
                 # h5("Code Available:"),
                 # h6("https://github.com/leekshaffer/baseball-qes"),
                 h5("Data Sources:"),
                 a("Baseball Savant Custom Leaderboard",
                   href="https://baseballsavant.mlb.com/leaderboard/custom"),
                 br(),
                 a("Baseball Savant Batter Positioning Leaderboard",
                   href="https://baseballsavant.mlb.com/visuals/batter-positioning")
               ),
               
               accordion(
                 open=c("Shift Data Link for 2022", "Table 1: SCM Estimates"),
                 accordion_panel(
                   "Shift Data Link for 2022",
                   uiOutput("AllURLs")),
                 accordion_panel(
                   "Table 1: SCM Estimates",
                   DT::dataTableOutput("Alltbl1")),
                 accordion_panel(
                   "Plot 1: Outcome Value Trajectories",
                   plotOutput("Allplot1", width="95%")),
                 accordion_panel(
                   "Plot 2: SCM Estimates",
                   plotOutput("Allplot2", width="95%")),
                 accordion_panel(
                   "Plot 3: SCM Estimates by 2022 Shift Rate",
                   plotOutput("Allplot3", width="95%"))
                 )
             )
    ),
    
    tabPanel("Analysis 2 (SCM): By Player",
             page_sidebar(
               title = "Effect of the Shift Ban on High-Shift MLB Players",
               sidebar=sidebar(
                 selectInput("InName", label = h4("Choose Player"),
                             choices = as.list(c(Player_Choices)),
                             selected = "Corey Seager"),
                 selectInput("InStat", label = h4("Choose Outcome"),
                             choices = as.list(c(All_token_SC,BStats_Use$stat)),
                             selected = All_token_SC),
                 selectInput("TargetSeason", label = h4("Choose Target Season"),
                             choices = as.list(gsub("_","-",types)),
                             selected = "2023"),
                 # h4("Created by Lee Kennedy-Shaffer, 2024"),
                 # h5("Code Available:"),
                 # h6("https://github.com/leekshaffer/baseball-qes"),
                 h5("Data Sources:"),
                 a("Baseball Savant Custom Leaderboard",
                   href="https://baseballsavant.mlb.com/leaderboard/custom"),
                 br(),
                 a("Baseball Savant Batter Positioning Leaderboard",
                   href="https://baseballsavant.mlb.com/visuals/batter-positioning")
               ),
               
               accordion(
                 open=c("Player Links and Info", "Table 1: SCM Estimates"),
                 accordion_panel(
                   "Player Links and Info",
                   uiOutput("URLs")),
                 accordion_panel(
                   "Table 1: SCM Estimates",
                   DT::dataTableOutput("tbl1")),
                 accordion_panel(
                   "Table 2: SCM Weights Table",
                   DT::dataTableOutput("tbl2")),
                 accordion_panel(
                   title=textOutput("plot1_title_out"),
                   plotOutput("plot1", width="95%"),
                   value="plot1"),
                 accordion_panel(
                   title=textOutput("plot2_title_out"),
                   plotOutput("plot2", width="95%"),
                   value="plot2"),
                 accordion_panel(
                   title=textOutput("plot3_title_out"),
                   plotOutput("plot3", width="95%"),
                   value="plot3")
               )
             )
    )
    )
  )

## Server:
server <- function(input, output) {
  ## SCM outputs:
  plot1_title <- reactive({
    if (input$InStat==All_token_SC) {
      return("Plot 1: SCM Estimates for OBP")
    } else {
      return("Plot 1: Outcome Value Trajectories")
    }
  })
  output$plot1_title_out <- renderText(plot1_title())
  plot2_title <- reactive({
    if (input$InStat==All_token_SC) {
      return("Plot 2: SCM Estimates for OPS")
    } else {
      return("Plot 2: Synthetic and Observed Outcomes")
    }
  })
  output$plot2_title_out <- renderText(plot2_title())
  plot3_title <- reactive({
    if (input$InStat==All_token_SC) {
      return("Plot 3: SCM Estimates for wOBA")
    } else {
      return("Plot 3: SCM Estimates")
    }
  })
  output$plot3_title_out <- renderText(plot3_title())
  
  output$URLs <- renderUI({
      Info <- player_info(input$InName,input$TargetSeason)
      if (input$TargetSeason %in% c("2023","2024")) {
        TL2 <- tagList(a(paste0(input$TargetSeason," Shade Rate: ",
                                format(round(Info[[paste0("Shade_Perc_",input$TargetSeason)]],
                                             digits=1),
                                       nsmall=1),"%")))
      } else if (input$TargetSeason=="2023-24") {
        TL2 <- tagList(a(paste0("2023 Shade Rate: ",
                                format(round(Info$Shade_Perc_2023,
                                             digits=1),
                                       nsmall=1),"%")),
                       br(),
                       a(paste0("2024 Shade Rate: ",
                                format(round(Info$Shade_Perc_2024,
                                             digits=1),
                                       nsmall=1),"%")))
      }
      tagList(
        a("Batter Positioning Leaderboard, 2022",
          href=Info$MLB_BPL_URL),
        br(),
        a("FanGraphs Player Page",
          href=Info$FG_URL),
        br(),
        a("Baseball Reference Player Page",
          href=Info$BR_URL),
        br(),
        a(paste0("2022 Shift Rate: ",format(round(Info$Shift_Perc_2022, digits=1),nsmall=1),"%")),
        br(),
        TL2
        )
  })
  
  output$AllURLs <- renderUI({
    tagList(
      a("Batter Positioning Leaderboard, 2022", 
        href=paste0("https://baseballsavant.mlb.com/visuals/batter-positioning?",
                    "playerId=545361&teamId=&opponent=&firstBase=0",
                    "&shift=1&season=2022&attempts=250&batSide=R")))
  })
  
  output$tbl1 <- DT::renderDataTable({
    validate(
      need(file.exists(paste0("res/Players-SC-", input$TargetSeason, "-full",
                              "/Player-SC-",input$InName,".Rda")),
           message="This target season selection is not available for this player.")
    )
    DT::datatable(ests_tbl(input$InName, input$InStat, input$TargetSeason), 
                  options = list(paging = FALSE,
                                 searching = FALSE))
  })
  
  output$Alltbl1 <- DT::renderDataTable({
    validate(
      need(file.exists(paste0("res/SC-",input$AllTargetSeason,"-Complete.Rda")),
           message="This target season selection is not available.")
    )
    DT::datatable(ests_tbl(All_token_SC, input$SCAllInStat, input$AllTargetSeason), 
                  options = list(lengthMenu = list(c(3, 6, 9, -1), c('3', '6', '9', 'All')),
                                 pageLength = 9))
  })
  
  output$tbl2 <- DT::renderDataTable({
    validate(
      need(file.exists(paste0("res/Players-SC-", input$TargetSeason, "-full",
                              "/Player-SC-",input$InName,".Rda")),
           message="This target season selection is not available for this player.")
    )
    DT::datatable(wts_tbl(input$InName, input$InStat, input$TargetSeason), 
                  options = list(lengthMenu = list(c(3, 6, 9, -1), c('3', '6', '9', 'All')),
                                 pageLength = 3))
  })
  
  output$plot1 <- renderPlot({
    Target <- input$InName
    targS <- input$TargetSeason
    validate(
      need(file.exists(paste0("res/Players-SC-", targS, "-full", 
                              "/Player-SC-", Target, ".Rda")),
           message="This target season selection is not available for this player.")
    )
    load(file=paste0("res/Players-SC-", targS, "-full", 
                     "/Player-SC-",Target,".Rda"))
    if (input$InStat==All_token_SC) {
        SC_data <- get(paste0("SCs_Results_",gsub("-","_",targS))) %>% 
          dplyr::filter(Outcome==BStats_Use$stat[1] & 
                          (Name_Disp %in% c(Target, Weights_Unit$unit)) & 
                          (Season %in% unique(SCs$Season)))
        plot_SC_ests(BStats_Use$stat[1], SC_data,
                     LegName=NULL,
                     LegVar="Placebo_Unit",
                     LegLabs=c(paste0("Target Player: ",Target),
                               "Placebo Players (2022 Shift Rate \U2264 15%)"), 
                     LegBreaks=c(FALSE,TRUE),
                     LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                     LegAlpha=c(1,0.5), 
                     LegLTY=c("solid","longdash"), 
                     title=paste0("SCM estimates for ",BStats_Use$stat[1]," for ",Target,
                                  " and placebos"),
                     LW=1.2,
                     tagval=NULL) + 
          theme(legend.position="bottom",
                legend.background=element_rect(fill="white", color="grey50"),
                legend.direction="horizontal")
    } else {
        SC_data <- get(paste0("SCs_Results_",gsub("-","_",targS))) %>%
          dplyr::filter(Outcome==input$InStat & 
                          (Name_Disp %in% c(Target, Weights_Unit$unit)) & 
                          (Season %in% unique(SCs$Season)))
        plot_Traj(statval=input$InStat,
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
                  Target=input$InName,
                  tagval=NULL) + 
          theme(legend.position="bottom",
                legend.background=element_rect(fill="white", color="grey50"),
                legend.direction="horizontal")
    }
  })
  
  output$Allplot1 <- renderPlot({
    validate(
      need(file.exists(paste0("res/SC-",input$AllTargetSeason,"-Complete.Rda")),
           message="This target season selection is not available.")
    )
    # if (input$SCAllInStat==All_token_SC) {
    #   SCs_Res_int <- get(paste0("SCs_Results_",gsub("-","_",input$AllTargetSeason)))
    #     plot_SC_ests_all(BStats_Use$stat[1], SCs_Res_int) + 
    #       theme(legend.position="bottom",
    #             legend.background=element_rect(fill="white", color="grey50"),
    #             legend.direction="horizontal")
    # } else {
      MaxSeason <- ifelse(input$AllTargetSeason=="2023-24",2024,
                           as.numeric(input$AllTargetSeason))
        plot_Traj(statval=input$SCAllInStat,
                  Traj.dat=B.250_pool %>% 
                    dplyr::filter(Shift_Cat != "Medium",
                                  Season <= MaxSeason),
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
                legend.direction="horizontal")
    # }
  })
  
  output$plot2 <- renderPlot({
    Target <- input$InName
    targS <- input$TargetSeason
    validate(
      need(file.exists(paste0("res/Players-SC-", targS, "-full",
                              "/Player-SC-",Target,".Rda")),
           message="This target season selection is not available for this player.")
    )
    load(file=paste0("res/Players-SC-", targS, "-full",
                     "/Player-SC-",Target,".Rda"))
    if (input$InStat==All_token_SC) {
        SC_data <- get(paste0("SCs_Results_",gsub("-","_",targS))) %>%
          dplyr::filter(Outcome==BStats_Use$stat[2] & 
                          (Name_Disp %in% c(Target, Weights_Unit$unit)) & 
                          (Season %in% unique(SCs$Season)))
        plot_SC_ests(BStats_Use$stat[2], SC_data,
                     LegName=NULL,
                     LegVar="Placebo_Unit",
                     LegLabs=c(paste0("Target Player: ",Target),
                               "Placebo Players (2022 Shift Rate \U2264 15%)"), 
                     LegBreaks=c(FALSE,TRUE),
                     LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                     LegAlpha=c(1,0.5), 
                     LegLTY=c("solid","longdash"), 
                     title=paste0("SCM estimates for ",BStats_Use$stat[2]," for ",Target,
                                  " and placebos"),
                     LW=1.2,
                     tagval=NULL) + 
          theme(legend.position="bottom",
                legend.background=element_rect(fill="white", color="grey50"),
                legend.direction="horizontal")
    } else {
        SC_data <- get(paste0("SCs_Results_",gsub("-","_",targS))) %>%
          dplyr::filter(Outcome==input$InStat & 
                          (Name_Disp %in% c(Target, Weights_Unit$unit)) & 
                          (Season %in% unique(SCs$Season)))
        plot_Comp(input$InStat, SC_data,
                  display_name=Target,
                  tagval=NULL) + 
          theme(legend.position="bottom",
                legend.background=element_rect(fill="white", color="grey50"),
                legend.direction="horizontal") +
          geom_point(size=3)
    }
  })
  
  output$Allplot2 <- renderPlot({
    validate(
      need(file.exists(paste0("res/SC-",input$AllTargetSeason,"-Complete.Rda")),
           message="This target season selection is not available.")
    )
    SCs_Res_int <- get(paste0("SCs_Results_",gsub("-","_",input$AllTargetSeason)))
    # if (input$SCAllInStat==All_token_SC) {
    #     plot_SC_ests_all(BStats_Use$stat[2], SCs_Res_int) + 
    #       theme(legend.position="bottom",
    #             legend.background=element_rect(fill="white", color="grey50"),
    #             legend.direction="horizontal")
    # } else {
        plot_SC_ests_all(input$SCAllInStat, SCs_Res_int) + 
          theme(legend.position="bottom",
                legend.background=element_rect(fill="white", color="grey50"),
                legend.direction="horizontal")
    # }
  })
  
  output$Allplot3 <- renderPlot({
    validate(
      need(file.exists(paste0("res/SC-", input$AllTargetSeason, 
                              "-full-Complete.Rda")),
           message="This target season selection is not available.")
    )
    SCs_Res_int <- get(paste0("SCs_Results_",
                              gsub("-", "_", input$AllTargetSeason),
                              "_full"))
    if (input$AllTargetSeason=="2023") {
      plot_SC_Shift(input$SCAllInStat, SCs_Res_int,
                  LW=1, Placebo_Inc=TRUE, tagval=NULL)
    } else {
      plot_SC_Shift(input$SCAllInStat, SCs_Res_int,
                    LW=1, Placebo_Inc=FALSE, tagval=NULL)
    }
  })
  
  output$plot3 <- renderPlot({
    Target <- input$InName
    targS <- input$TargetSeason
    validate(
      need(file.exists(paste0("res/Players-SC-", targS, "-full", 
                              "/Player-SC-", Target, ".Rda")),
           message="This target season selection is not available for this player.")
    )
    load(file=paste0("res/Players-SC-", targS, "-full", 
                     "/Player-SC-", Target, ".Rda"))
    if (input$InStat==All_token_SC) {
        SC_data <- get(paste0("SCs_Results_",gsub("-","_",targS))) %>%
          dplyr::filter(Outcome==BStats_Use$stat[3] & 
                          (Name_Disp %in% c(Target, Weights_Unit$unit)) & 
                          (Season %in% unique(SCs$Season)))
        plot_SC_ests(BStats_Use$stat[3], SC_data,
                     LegName=NULL,
                     LegVar="Placebo_Unit",
                     LegLabs=c(paste0("Target Player: ",Target),
                               "Placebo Players (2022 Shift Rate \U2264 15%)"), 
                     LegBreaks=c(FALSE,TRUE),
                     LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                     LegAlpha=c(1,0.5), 
                     LegLTY=c("solid","longdash"), 
                     title=paste0("SCM estimates for ",BStats_Use$stat[3]," for ",Target,
                                  " and placebos"),
                     LW=1.2,
                     tagval=NULL) + 
          theme(legend.position="bottom",
                legend.background=element_rect(fill="white", color="grey50"),
                legend.direction="horizontal")
    } else {
        SC_data <- get(paste0("SCs_Results_",gsub("-","_",targS))) %>%
          dplyr::filter(Outcome==input$InStat & 
                          (Name_Disp %in% c(Target, Weights_Unit$unit)) & 
                          (Season %in% unique(SCs$Season)))
        plot_SC_ests(input$InStat, SC_data,
                     LegName=NULL,
                     LegVar="Placebo_Unit",
                     LegLabs=c(paste0("Target Player: ",Target),
                               "Placebo Players (2022 Shift Rate \U2264 15%)"), 
                     LegBreaks=c(FALSE,TRUE),
                     LegCols=brewer.pal(3, "Dark2")[c(3,1)], 
                     LegAlpha=c(1,0.5), 
                     LegLTY=c("solid","longdash"), 
                     title=paste0("SCM estimates for ",input$InStat," for ",Target,
                                  " and placebos"),
                     LW=1.2,
                     tagval=NULL) + 
          theme(legend.position="bottom",
                legend.background=element_rect(fill="white", color="grey50"),
                legend.direction="horizontal")
    }
  })
  
  ## DID tab outputs:
  output$DIDtbl1 <- DT::renderDataTable({
    DT::datatable(DID_tbl(input$DIDInStat),
                  options = list(lengthMenu = list(c(6, 12, -1), c('6', '12', 'All')),
                                 pageLength = -1,
                                 searching = TRUE))
  })
  
  output$DIDplot1 <- renderPlot({
    if (input$DIDInStat==All_token_DID) {
    } else {
      plot_DIDs(input$DIDInStat, DID.CF.dat=FG.dat.withCF, DID.ES.dat=FullES,
                ES.lim = c(-0.025, 0.025))[["Trend"]] +
        theme(legend.background=element_rect(fill="white",
                                             color="grey50"),
              legend.position="bottom",
              legend.text=element_text(size=rel(1.1))) +
        geom_point(size=3)
    }
  })
  
  output$DIDplot2 <- renderPlot({
    if (input$DIDInStat==All_token_DID) {
    } else {
      plot_DIDs(input$DIDInStat, DID.CF.dat=FG.dat.withCF, DID.ES.dat=FullES,
                ES.lim = c(-0.025, 0.025))[["ES"]] +
        theme(legend.background=element_rect(fill="white",
                                             color="grey50"),
              legend.position="bottom",
              legend.text=element_text(size=rel(1.2))) +
        geom_point(size=4)
    }
  })
}

## Run the App:
shinyApp(ui = ui, server = server)
