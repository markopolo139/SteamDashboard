library(shiny)
library(shinydashboard)
library(plotly)

slider <- div(class = "custom-slider",
              sliderInput("years",
                          label = div(style = "display: inline-block; width: 80px; font-size: 2rem", "Years:"),
                          min = as.Date("1999-04-01"),
                          max = as.Date("2023-09-09"),
                          value = c(as.Date("1999-04-01"), as.Date("2023-09-09")),
                          timeFormat = "%b %Y",
                          width = "100%")
)

genres <- c("Action", "RPG", "Adventure", "Strategy", "Indie", "Sports", "Simulation")

custom_css <- "
      .custom-slider .shiny-input-container { display: flex; width: 100%; padding: 0 30px}
      .custom-slider .irs { width: 100%}
      .custom-slider .control-label { display: flex; justify-content: center; align-items: center; color: white !important}
      .main-header .logo { height: 58px; }
      .sidebar-toggle { display: none; width: 0;}
      .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar, .js-irs-0 .irs-to, .js-irs-0 .irs-from {background: red !important}
      .js-irs-0 .irs-grid-text, .js-irs-0 .irs-max, .js-irs-0 .irs-min {color: white !important}
      .navbar-custom-menu .navbar-nav, .navbar-custom-menu .navbar-nav>li, .navbar-custom-menu {float: none !important}
      #selected_genre+ div>.selectize-dropdown{bottom: 100% !important; top:auto!important;}
"

dashboardPage(
  dashboardHeader(
    title = "Steam Recommender",
    tags$li(class = "dropdown",
            tags$div(style = "width: 100%; height: 58px;", slider)
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Trends Analysis", tabName = "trends", icon = icon("th")),
      menuItem("About", tabName = "about", icon = icon("question-sign", lib = "glyphicon"))
    ),
    div(
      img(src="put_logo_white.png", style = "max-height: 100%; max-width: 100%"),
      style = "width: 100%; padding: 0 10px; position: absolute; bottom: 0"
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(custom_css))
    ),
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          column(
            width = 6,
            style = "padding: 0",
            box(
              title = "Best Games by Platform",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              uiOutput("bestGames"),
              footer = numericInput("number_of_games", label = "Number Of games", min = 1, max = 5, value = 1)
            ),
            box(
              title = "Selected game description",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              uiOutput("description")
            )
          ),
          box(dataTableOutput("table"))
        ),
        fluidRow(
          box(
            title = "Number of Released Games in selected period of time",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            uiOutput("released_games")
          ),
          box(
            title = "Best free to play games for selected genre",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            uiOutput("free_to_play"),
            footer = selectInput("selected_genre", label = "Genre", genres)
          )
        )
      ),
      tabItem(
        tabName = "trends",
        fluidRow(
          box(
            title = "Price of games by number of DLC",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("dlc_price")),
          box(
            title = "Average play time in relation to price",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("play_time_price"))
        ),
      ),
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            uiOutput("about")
          )
        )
      )
    )
  )
)
