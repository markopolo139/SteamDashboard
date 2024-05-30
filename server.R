#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library("dplyr")
library("plotly")
library("tidyverse")
library("tidytext")
library("ggplot2")
library(shinydashboard)
library(DT)

data <- read.csv("./games.csv")
data <- data %>% filter(Estimated.owners != "0 - 0" ) %>%
  filter(Release.date != "NA") %>% filter(Positive + Negative > 1000) %>%
  mutate(Release.date = as.Date(Release.date, format = "%b %d, %Y"))

function(input, output, session) {

  data_for_years <- reactive({
      data <- data %>% filter(as.Date(Release.date) >= input$years[1] & as.Date(Release.date) <= input$years[2])
      data
  })

  output$table <- renderDataTable({
      datatable(
        data_for_years() %>% select(AppID, Name, Metacritic.score, Price, About.the.game),
        options = list(
          pageLength = 10,
          columnDefs = list(
            list(visible = FALSE, targets = 4)
          ),
          rowCallback = JS(
            "function(row, data) {",
            "  var info = data[4];",
            "  $(row).attr('title', info);",
            "}"
          )
        ),
        colnames = c("Id", "Name", "Metacritic.score", "Price", "About"),
        selection = 'single',
        rownames = FALSE
      )
  })

  number_of_game <- reactive({
    input$number_of_games
  })

  output$bestGames <- renderUI({
      data <- data_for_years() %>% filter(!is.na(Metacritic.score))

      best_windows <- data %>% filter(Windows == "True") %>%
        arrange(desc(Metacritic.score)) %>% slice(1:number_of_game())
      best_mac <- data %>% filter(Mac == "True") %>%
        arrange(desc(Metacritic.score)) %>% slice(1:number_of_game())
      best_linux <- data %>% filter(Linux == "True") %>%
        arrange(desc(Metacritic.score)) %>% slice(1:number_of_game())

      fluidRow(
        column(4,
               h4("Best Windows Game"),
               if (nrow(best_windows) == 0) {
                   p("No games found")
               } else {
                   lapply(seq_len(nrow(best_windows)), function(i) {
                     p(best_windows[i, "Name"], " score:", best_windows[i, "Metacritic.score"])
                   })
               }
        ),
        column(4,
               h4("Best Mac Game"),
               if (nrow(best_mac) == 0) {
                 p("No games found")
               } else {
                 lapply(seq_len(nrow(best_mac)), function(i) {
                   p(best_mac[i, "Name"], " score:", best_mac[i, "Metacritic.score"])
                 })
               }
        ),
        column(4,
               h4("Best Linux Game"),
               if (nrow(best_linux) == 0) {
                 p("No games found")
               } else {
                 lapply(seq_len(nrow(best_linux)), function(i) {
                   p(best_linux[i, "Name"], " score:", best_linux[i, "Metacritic.score"])
                 })
               }
        )
      )
  })

  output$dlc_price <- renderPlotly({
    data <- data_for_years() %>% filter(!is.na(DLC.count)) %>% filter(DLC.count < 75) %>% filter(Price < 260)

    data$DLC.count_bin <- cut(data$DLC.count, breaks = c(0:9, Inf), labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))

    gg <- ggplot(data, aes(x = DLC.count_bin, y = Price)) +
      geom_boxplot(outlier.shape = NA) +
      labs(x = "Number of DLC", y = "Price($)") +
      scale_x_discrete(labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10+"))

    ggplotly(gg)
  })

  output$play_time_price <- renderPlotly({
    data <- data_for_years() %>% filter(!is.na(Average.playtime.forever)) %>% filter(Average.playtime.forever > 5000) %>% mutate(Average.playtime.forever.hours = Average.playtime.forever / 60)

    gg <- ggplot(
      data, aes(x = Price, y = Average.playtime.forever.hours,
                text = paste("Game: ", Name))
    ) +
    geom_point() +
    labs(x = "Price($)", y = "Average playtime in hours")

    ggplotly(gg)
  })

  output$released_games <- renderText({
    num_released_games <- nrow(data_for_years())
    paste("Number of released games:", num_released_games)
  })

  output$description <- renderUI({
    s <- input$table_rows_selected

    if (length(s) == 0) {
        return("No game selected")
    }

    selected_game <- data_for_years()[s, ]

    fluidRow(
      column(
        4,
        p("Positive Reviews:", selected_game$Positive),
        p("Negative Reviews:", selected_game$Negative),
        p("Recommendations:", selected_game$Recommendations),
        p("Achievements:", selected_game$Achievements),
        p("Metacritic score:", selected_game$Metacritic.score),
        p("Genre:", selected_game$Genre),
        p("Developer:", selected_game$Developer),
        p("Publisher:", selected_game$Publisher),
        p("Number of DLC:", selected_game$DLC.count)
      ),
      column(8, tags$img(src = selected_game$Header.image, alt="game_img", style = "width: 100%;"))
    )
  })

  output$free_to_play <- renderUI({
    data <- data_for_years() %>%
      filter(str_detect(Genres, "Free to Play")) %>%
      filter(str_detect(Genres, input$selected_genre)) %>%
      arrange(desc(Metacritic.score)) %>%
      slice(1)

    if (nrow(data) > 0) {
      paste("Best game in", input$selected_genre, "genre:", data$Name, "with score:", data$Metacritic.score)
    } else {
      "No games found in the selected genre"
    }
  })

  output$about <- renderUI({
    fluidRow(
      column(
        width = 12,
        p("Welcome to the Steam Games Analysis Dashboard!"),
        p("This interactive dashboard allows you to explore trends and insights within the world of Steam games. You can filter games based on release dates, genres, and platforms, and visualize various metrics such as Metacritic scores, prices, and player playtime."),
        h3("Features:"),
        tags$ul(
          tags$li("Dashboard Tab: Explore the best games by platform, view detailed game descriptions, and analyze the number of released games within a selected time frame."),
          tags$li("Trends Analysis Tab: Dive into trends related to game prices, number of downloadable content (DLC), and average playtime."),
        ),
        h3("How to Use:"),
        tags$ol(
          tags$li("Years Range Slider: Adjust the slider to select a range of years for filtering the data."),
          tags$li("Genre Selector: Choose a genre from the dropdown menu to explore the best free-to-play games within that genre."),
          tags$li("Table Interaction: Click on a game in the table to view its detailed description and image.")
        ),
        p("Explore the tabs to gain insights into the world of Steam games!"),
        p("Authors: Marek Seget, Krzysztof Bryszak")
      )
    )
  })
}
