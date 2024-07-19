server <- function(input, output, session) {
  api_key <- "edd8fc19655341ddb5d1817eff33674d"
  
  # Function to get weather data
  get_weather_data <- function(lat, lon) {
    url <- paste0("https://api.openweathermap.org/data/2.5/weather?lat=", lat, "&lon=", lon, "&units=metric&appid=", api_key)
    response <- httr::GET(url)
    if (response$status_code == 200) {
      return(httr::content(response, "parsed"))
    } else {
      return(NULL)
    }
  }
  
  # Function to get forecast data
  get_forecast_data <- function(lat, lon) {
    url <- paste0("https://api.openweathermap.org/data/2.5/forecast?lat=", lat, "&lon=", lon, "&units=metric&appid=", api_key)
    response <- httr::GET(url)
    if (response$status_code == 200) {
      return(httr::content(response, "parsed"))
    } else {
      return(NULL)
    }
  }
  
  # Initial coordinates
  x_lat <- 21.0285
  y_lon <- 105.8524
  
  # Reactive values for weather and forecast data
  selected_weather <- reactiveVal(NULL)
  selected_forecast <- reactiveVal(NULL)
  
  # Function to update weather and forecast data
  get_weather_forecast <- function(lat, lon) {
    weather_data <- get_weather_data(lat, lon)
    forecast_data <- get_forecast_data(lat, lon)
    selected_weather(weather_data)
    selected_forecast(forecast_data)
  }
  
  # Set default location
  get_weather_forecast(x_lat, y_lon)
  
  # Update UI elements based on selected data
  observe({
    if (!is.null(selected_weather())) {
      output$location <- renderText({
        selected_weather()$name
      })
      
      output$date <- renderText({
        format(with_tz(Sys.time(), "Asia/Ho_Chi_Minh"), "%Y/%m/%d")
      })
      
      output$time <- renderText({
        format(with_tz(Sys.time(), "Asia/Ho_Chi_Minh"), "%H:%M:%S")
      })
      
      output$temp <- renderText({
        paste(selected_weather()$main$temp, "°C")
      })
      
      output$feels_like <- renderText({
        paste(selected_weather()$main$feels_like, "°C")
      })
      
      output$condition <- renderText({
        selected_weather()$weather[[1]]$description
      })
      
      output$humidity <- renderText({
        paste(selected_weather()$main$humidity, "%")
      })
      
      output$visibility <- renderText({
        paste(selected_weather()$visibility, "m")
      })
      
      output$windSpeed <- renderText({
        paste(selected_weather()$wind$speed, "m/s")
      })
      
      output$pressure <- renderText({
        paste(selected_weather()$main$pressure, "hPa")
      })
      
      output$location2 <- renderText({
        selected_weather()$name
      })
    }
  })
  
  # Render map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = y_lon, lat = x_lat, zoom = 10)
  })
  
  # Handle city search
  observeEvent(input$go, {
    city_name <- input$city_search
    if (nzchar(city_name)) {
      search_city(city_name)
    }
  })
  
  # Function to search city and update weather and forecast
  search_city <- function(city_name) {
    geocode_url <- paste0("https://api.openweathermap.org/data/2.5/weather?q=", city_name, "&units=metric&appid=", api_key)
    response <- httr::GET(geocode_url)
    
    if (response$status_code == 200) {
      geo_data <- httr::content(response, "parsed")
      if (!is.null(geo_data$coord)) {
        lat <- geo_data$coord$lat
        lon <- geo_data$coord$lon
        leafletProxy("map") %>%
          clearMarkers() %>%
          setView(lng = lon, lat = lat, zoom = 10) %>%
          addMarkers(lng = lon, lat = lat, popup = city_name)
        
        get_weather_forecast(lat, lon)
      } else {
        showNotification("City not found. Please enter another city name.", duration = 5000)
      }
    } else {
      showNotification("Failed to fetch city data. Please try again later.", duration = 5000)
    }
  }
  
  # Handle map click
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lon <- click$lng
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lng = lon, lat = lat, popup = "Selected Location")
    
    get_weather_forecast(lat, lon)
  })
  
  # Render forecast plot
  output$line_chart <- renderPlotly({
    if (!is.null(selected_forecast())) {
      forecast_data <- selected_forecast()
      times <- sapply(forecast_data$list, function(x) x$dt_txt)
      values <- sapply(forecast_data$list, function(x) {
        if (input$feature %in% c("speed", "deg", "gust")) {
          return(x$wind[[input$feature]])
        } else {
          return(x$main[[input$feature]])
        }
      })
      
      plot_ly(
        x = times,
        y = values,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = 'blue'),
        marker = list(color = 'red')
      ) %>%
        layout(
          title = paste("Forecast for", input$feature),
          xaxis = list(title = "Time"),
          yaxis = list(title = input$feature)
        )
    } else {
      plotly_empty()
    }
  })
}
