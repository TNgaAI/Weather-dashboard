library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(lubridate)

ui <- dashboardPage(
  dashboardHeader(title = "Weather Dashboard"),
  dashboardSidebar(
    tags$div(
      style = "text-align: center; padding-top: 20px; padding-bottom: 20px;",
      tags$img(src = "https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/2f9e8780-1e9b-4920-85b8-bb5270cb1e1e/dg37osc-1d6b2c2a-f025-4881-b742-47b6408f1d6c.jpg/v1/fill/w_1280,h_1703,q_75,strp/gojo_satoru_jujutsu_kaisen_season_2_by_nico2713_dg37osc-fullview.jpg?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7ImhlaWdodCI6Ijw9MTcwMyIsInBhdGgiOiJcL2ZcLzJmOWU4NzgwLTFlOWItNDkyMC04NWI4LWJiNTI3MGNiMWUxZVwvZGczN29zYy0xZDZiMmMyYS1mMDI1LTQ4ODEtYjc0Mi00N2I2NDA4ZjFkNmMuanBnIiwid2lkdGgiOiI8PTEyODAifV1dLCJhdWQiOlsidXJuOnNlcnZpY2U6aW1hZ2Uub3BlcmF0aW9ucyJdfQ.SZT_kvya8EULAQ19CNjJupErJC_tyEyBwaDb_qJRpwg", height = 100, width = 100),  # Thay đổi 'avatar.jpg' bằng URL hoặc đường dẫn đến avatar của bạn
      br(),
      tags$p("Thúy Nga Online", style = "font-weight: bold; color: green;")
    ),
    sidebarMenu(
      menuItem("Weather", tabName = "weather", icon = icon("sun")),
      menuItem("Forecast", tabName = "forecast", icon = icon("info-circle")),
      textInput("city_search", "Search City", ""),
      actionButton("go", "Go")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML('
        .shiny-split-layout > div {
          overflow: hidden;
        }
        .content-wrapper {
          background-color: #C0EBED;
        }
        #location {
          display: inline;
        }
        .custom-box {
          height: 98px;
          margin-bottom: 20px;
          text-align: center;
          font-size: 15px; /* Increased font size in the box */
        }
        .box-header {
          font-weight: bold;
          font-size: 16px; /* Increased font size in the box header */
        }
        .shiny-text-output {
          font-size: 18px; /* Increased font size for shiny text output */
          margin-top: 10px;
        }
        .flex-container {
          display: flex;
          align-items: center;
        }
        .date-time {
          font-size: 20px;
          margin-left: 5px; /* Adjust margin-left to separate icon from text */
        }
      '))
    ),
    tabItems(
      tabItem(
        tabName = "weather",
        tags$span(style = "font-size: 50px; margin-bottom: 40px; margin-left: 27px", "Current Weather"),
        fluidRow(
          column(
            width = 7,
            div(style = "margin-left: 60px;",
                div(style = "display: flex; align-items: center;",
                    icon("location-crosshairs", class = "fas fa-location-crosshairs", style = "font-size: 35px; margin-right: 5px"),
                    div(id = "location", class = "shiny-html-output", style = "font-size: 40px; font-weight: bold;")
                ),
                tags$span(style = "color: brown;",
                          div(class = "flex-container",
                              icon("calendar", class = "fas fa-calendar", style = "font-size: 25px; margin-right: 5px; margin-left: 28px"),
                              div(id = "date", class = "shiny-text-output date-time")
                          )
                ),
                tags$span(style = "color: brown;",
                          div(class = "flex-container",
                              icon("clock", class = "fas fa-clock", style = "font-size: 25px; margin-right: 5px; margin-left: 28px"),
                              div(id = "time", class = "shiny-text-output date-time")
                          )
                ),
                div(style = "font-size: 20px; margin-top: 10px;",
                    tags$span(style = "font-size: 25px; margin-right: 3px;",
                              icon("temperature-three-quarters", class = "fas fa-temperature-three-quarters"),
                              "Current Temperature:"
                    ),
                    div(id = "temp", class = "shiny-html-output", style = "font-size: 25px;")
                ),
                div(class = "line-split")
            ),
            div(style = "margin-left: 50px; margin-right: 40px; margin-top: 60px;",
                fluidRow(
                  column(
                    width = 4,
                    div(class = "box box-solid box-danger custom-box",
                        div(class = "box-header", "Feels Like"),
                        div(id = "feels_like", class = "shiny-text-output")
                    )
                  ),
                  column(
                    width = 4,
                    div(class = "box box-solid box-info custom-box",
                        div(class = "box-header", "Humidity"),
                        div(id = "humidity", class = "shiny-text-output")
                    )
                  ),
                  column(
                    width = 4,
                    div(class = "box box-solid box-success custom-box",
                        div(class = "box-header", "Weather Condition"),
                        div(id = "condition", class = "shiny-text-output")
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 4,
                    div(class = "box box-solid box-warning custom-box",
                        div(class = "box-header", "Visibility"),
                        div(id = "visibility", class = "shiny-text-output")
                    )
                  ),
                  column(
                    width = 4,
                    div(class = "box box-solid box-primary custom-box",
                        div(class = "box-header", "Wind Speed"),
                        div(id = "windSpeed", class = "shiny-text-output")
                    )
                  ),
                  column(
                    width = 4,
                    div(class = "box box-solid box-info custom-box",
                        div(class = "box-header", "Air Pressure"),
                        div(id = "pressure", class = "shiny-text-output")
                    )
                  )
                )
            )
          ),
          column(
            width = 5,
            leafletOutput("map", width = "88%", height = "500px"),
            br(),
          )
        )
      ),
      tabItem(
        tabName = "forecast",
        tags$span(style = "font-size: 30px; font-weight: bold;",
                  div(id = "location2", class = "shiny-text-output")
        ),
        fluidRow(
          column(
            width = 2,
            div(
              class = "box",
              div(class = "box-body",
                  selectInput("feature", "Features:", choices = c(
                    "temp", "feels_like", "temp_min", "temp_max",
                    "pressure", "sea_level", "grnd_level",
                    "humidity", "speed", "deg", "gust"
                  ))
              )
            )
          ),
          column(
            width = 10,
            div(class = "box",
                div(class = "box-body",
                    plotlyOutput("line_chart", height = 400)
                )
            )
          )
        )
      )
    )
  )
)
