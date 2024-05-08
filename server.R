require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
require(htmltools)
source("model_prediction.R")

#Test generate_city_weather_bike_data() function
test_weather_data_generation<-function(){
  city_weather_bike_df <- generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

city_names_vector <- read_csv("selected_cities.csv")['CITY_ASCII']

# Create RShiny server
shinyServer(function(input, output){
  
  selected_city <- reactiveVal()
  
  observeEvent(input$city_dropdown, {
    validate(need(input$city_dropdown %in% city_names_vector,
               "Select a city for which data are available."))
               selected_city(input$city_dropdown)
  })

  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  cities_max_bike <- city_weather_bike_df %>% 
                      group_by(CITY_ASCII) %>% 
                      reframe(max=max(BIKE_PREDICTION), LAT=LAT, LNG=LNG,
                              pred=BIKE_PREDICTION_LEVEL, 
                              LABEL=lapply(LABEL,HTML), 
                              DETAILED_LABEL=DETAILED_LABEL,
                              FORECASTDATE=date(FORECASTDATETIME),
                              FORECASTHOUR=hour(FORECASTDATETIME),
                              FORECASTDATETIME=FORECASTDATETIME,
                              CURDATE=as.Date(Sys.time(), 
                                              format="%Y-%m-%d") + 1,
                              CURHOUR=as.numeric(format(Sys.time(), "%H")) -
                              (as.numeric(format(Sys.time(), "%H")) %% 3) ) %>%
                      filter(CURDATE == FORECASTDATE & FORECASTHOUR == CURHOUR) 
  
  # City selection
  observeEvent(input$city_dropdown, {

    if(input$city_dropdown == 'All') {
  #World map display
      output$city_bike_map <- renderLeaflet({
        leaflet(cities_max_bike) %>% addTiles() %>% 
          addCircleMarkers(lng = ~cities_max_bike$LNG, 
                           lat = ~cities_max_bike$LAT,
                           radius= ~ifelse(
                             cities_max_bike$pred == 'small', 6, 
                              ifelse(cities_max_bike$pred == 'medium',10,12)),
                           color = ~color_levels(cities_max_bike$pred),
                           label = ~cities_max_bike$LABEL,
                           labelOptions = labelOptions(noHide = TRUE)
          )
      }) 
    
  }
  
  else {
    #City map display
    output$city_bike_map <- renderLeaflet({
      temp <- cities_max_bike %>% filter(CITY_ASCII == input$city_dropdown) 
        leaflet(temp) %>% 
        setView(lng = temp$LNG, lat = temp$LAT, zoom=10) %>%
        addTiles() %>% 
        addMarkers(lng = ~cities_max_bike$LNG, lat = ~cities_max_bike$LAT,
                         label = ~HTML(temp$DETAILED_LABEL),
                   labelOptions = labelOptions(noHide = TRUE)
        )
    })
    #Temperature forecast display
    output$temp_line <- renderPlot({
      city_weather_bike_df %>%
        filter(date(FORECASTDATETIME) <= as.Date(
          format(Sys.time(), format="%Y-%m-%d"))+2,
               CITY_ASCII==input$city_dropdown) %>%
        ggplot(aes(y=TEMPERATURE2, x=round_date(
          as_datetime(FORECASTDATETIME), unit="hour"))) +
        geom_line(color="red") +
        geom_point() +
        geom_text(aes(label=round(TEMPERATURE2,0)),nudge_x=-30, nudge_y=2) +
        labs(y="Degrees Celcius", x="Hours from Now", 
             title="Local Temperature Forecast")
    })
    
    #Demand forecast display
    output$bike_line <- renderPlot({
      city_weather_bike_df %>%
        filter(date(FORECASTDATETIME) <= as.Date(
          format(Sys.time(), format="%Y-%m-%d"))+4,
               CITY_ASCII==input$city_dropdown) %>%
      ggplot(aes(x=round_date(as_datetime(FORECASTDATETIME),unit="hour"), 
        y=round(BIKE_PREDICTION, digits=0))) +
        geom_line(color="green") +
        geom_point() +
        geom_text(aes(label=round(BIKE_PREDICTION, digits=0)), 
                  nudge_x=-30,nudge_y=50) +
        labs(y="Bicycle Demand", x="Hours from Now", 
             title="Demand Model Estimates")
    })
      #Interactive demand forecast
      output$bike_date_output <- renderPrint({
        if (is_empty(input$plot_click[[1]])) {
          print("Click graph to select time.")
        } else {
               print(paste0(round_date(
                 as_datetime(input$plot_click[[1]]),unit="minute"))) 
               print(paste0("Demand forecast: ", 
                            as.integer(round(input$plot_click[[2]]))))
        }
      })
  }
})
})

