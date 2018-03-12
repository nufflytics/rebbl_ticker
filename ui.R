library(shiny)

shiny::fluidPage(
  tags$head(shiny::includeCSS("www/ticker.css")),
  #fluidRow(column(3,checkboxGroupInput("leagues", "League", c("REL" = "REBBL - REL", "Gman" = "REBBL - Gman", "Big O" = "REBBL - Big O"), selected = c("REL" = "REBBL - REL", "Gman" = "REBBL - Gman", "Big O" = "REBBL - Big O"))),
  #column(3,dateInput("since", "Since", value = lubridate::today()))),
  uiOutput("score_ticker")
)
