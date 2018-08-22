
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(nufflytics)
library(lubridate)

league_img <- function(league) {
  case_when(
    league == "REL" ~ img(src='REL.png') %>% as.character,
    league == "GMan" ~ img(src='Gman.png')%>% as.character,
    league == "Big O" ~ img(src='BigO.png')%>% as.character
  )
}

api_key <- readRDS("data/api.key")

leagues <- glue::glue_collapse(c("REBBL - REL", "REBBL - Gman", "REBBL - Big O"), sep = ",")

check_ticker <- function() {
  api_matches(api_key, leagues, limit = 3) %>% 
    .$matches %>% 
    map_int("id") %>% 
    max  #Get largest match_id
}

get_ticker <- function() {
  games <- api_matches(api_key, leagues, limit = 1000, start = now("UTC")-days(1))$matches
  
  home_teams <- games %>% map(pluck, "teams", 1, "teamname") %>% unlist
  home_score <- games %>% map(pluck, "teams", 1, "score") %>% unlist
  
  away_teams <- games %>% map(pluck, "teams", 2, "teamname") %>% unlist
  away_score <- games %>% map(pluck, "teams", 2, "score") %>% unlist
  
  match_league <- games %>% map_chr("leaguename") %>% stringr::str_remove("REBBL - ")
  
  inner_ticker <- pmap(
    list(home_teams, home_score, away_score, away_teams, match_league), 
    ~list(
      div(class = "ticker__item", 
          HTML(glue::glue("{..1} {..2} {league_img(..5)} {..3} {..4}"))
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
  
  a_ticker <- reactivePoll(300000, session, check_ticker, get_ticker)
  
  output$score_ticker <- renderUI({
    a_ticker()
  })
})
