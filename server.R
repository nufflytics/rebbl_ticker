
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(nufflytics)

api_key <- readRDS("data/api.key")

get_ticker <- function(leagues, start_date) {
  games <- map(leagues, ~api_matches(api_key, ., limit = 1000, start = start_date)) %>% map("matches") %>% discard(~is.null(.))
  
  home_teams <- games %>% modify_depth(2, pluck, "teams", 1, "teamname") %>% unlist
  home_score <- games %>% modify_depth(2, pluck, "teams", 1, "score") %>% unlist
  
  away_teams <- games %>% modify_depth(2, pluck, "teams", 2, "teamname") %>% unlist
  away_score <- games %>% modify_depth(2, pluck, "teams", 2, "score") %>% unlist
  
  
  inner_ticker <- pmap(
    list(home_teams, home_score, away_score, away_teams), 
    ~list(
      div(class = "ticker__item", 
          glue::glue("{..1} {..2} v {..3} {..4}")
      ),
      div(class = "ticker__item",
          img(src = "REBBL.png")
      )
    )
  )
  
  div(class = "ticker-wrap",
      div(class = "ticker", 
          div(class = "ticker__item",
              img(src = "REBBL.png")
          ),
          inner_ticker
      )
  )
}

shinyServer(function(input, output, session) {
  
 a_ticker <- reactive({
   invalidateLater(180000, session)
   
   get_ticker(input$leagues, input$since)
 })
  
  output$score_ticker <- renderUI({
    a_ticker()
  })
})
