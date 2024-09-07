library(shiny)
library(tidyverse)
library(DT)

## Data needed to run:
Interv <- 2023
load(file="int/DID_data.Rda")
source("./04-FigureCommands.R", local=TRUE)
All_token_DID <- "-All (Table Only)-"
BStats <- tibble(stat=colnames(FG.dat.withCF %>% select(-c("Season","Batter"))))

## Data functions for player & outcome combination:
DID_tbl <- function(statval) {
  if (statval==All_token_DID) {
    Tbl <- TwoByTwo %>% pivot_longer(cols=-c("Batter"), names_to=c("Outcome","Year"), 
                                     names_sep="_", values_to="Value") %>% 
      pivot_wider(id_cols=c("Outcome","Batter"), names_from="Year", values_from="Value") %>%
      dplyr::rename(`Average, 2022`=`2022`,
                    `Average, 2023`=`2023`,
                    `Difference, 2023-2022`=`Diff`) %>%
      dplyr::arrange(Outcome) %>%
      dplyr::mutate(across(.cols=-c("Outcome","Batter"),
                           .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
  } else {
    Tbl <- TwoByTwo %>% dplyr::select(c("Batter",starts_with(statval)))
    colnames(Tbl) <- c("Batter Handedness",
                       paste(rep(statval, each=3),
                             c("2022","2023","Difference, 2023-2022"),
                             sep=" "))
    Tbl <- Tbl %>% dplyr::mutate(across(.cols=-c("Batter Handedness"),
                                 .fns=~format(round(.x, digits=3), digits=3, nsmall=3)))
  }
  return(Tbl)
}

## UI:
ui <- fluidPage(
  
  # Application title
  titlePanel("DID Analysis: Effect of the Shift Ban on High-Shifted MLB Players, 2023"),
  sidebarLayout(
    sidebarPanel(
      selectInput("InStat", label = h3("Choose Outcome"),
                  choices = as.list(c(All_token_DID,BStats$stat)),
                  selected = All_token_DID),
      # h4("Created by Lee Kennedy-Shaffer, 2024"),
      # h5("Code Available:"),
      # h6("https://github.com/leekshaffer/baseball-qes"),
      h5("Data Sources:"),
      a("FanGraphs Splits Leaderboard",
        href="https://www.fangraphs.com/leaders/splits-leaderboards")
    ),
    mainPanel(
      # uiOutput("URLs"),
      tabPanel(title="DID Effect Estimate(s)", 
               DT::dataTableOutput('tbl1')),
      # tabPanel(title="SCM Weights", 
      #          DT::dataTableOutput('tbl2')),
      plotOutput("plot1", width="95%"),
      plotOutput("plot2", width="95%")
      # plotOutput("plot3", width="95%")
    )
  )
)

## Server:
server <- function(input, output) {
  output$tbl1 <- DT::renderDataTable({
    DT::datatable(DID_tbl(input$InStat),
                  options = list(lengthMenu = list(c(6, 12, -1), c('6', '12', 'All')),
                                 pageLength = 6,
                                 searching = TRUE))
  })
  
  output$plot1 <- renderPlot({
    if (input$InStat==All_token_DID) {
    } else {
      plot_DIDs(input$InStat, DID.CF.dat=FG.dat.withCF, DID.ES.dat=FullES,
                ES.lim = c(-0.025, 0.025))[["Trend"]] +
        theme(legend.background=element_rect(fill="white",
                                             color="grey50"),
              legend.position="bottom",
              legend.text=element_text(size=rel(1.1)))
    }
  })
  
  output$plot2 <- renderPlot({
    if (input$InStat==All_token_DID) {
    } else {
      plot_DIDs(input$InStat, DID.CF.dat=FG.dat.withCF, DID.ES.dat=FullES,
                ES.lim = c(-0.025, 0.025))[["ES"]] +
        theme(legend.background=element_rect(fill="white",
                                             color="grey50"),
              legend.position="bottom",
              legend.text=element_text(size=rel(1.2)))
    }
  })
}

## Run the App:
shinyApp(ui = ui, server = server)


