require(tidyverse)
require(httr)
library(readr)
library(configr)

minMaxScaler <- function(var){
  var <- (var - min(var)) / (max(var) - min(var))
}

#Fetch keys from external file
config <- read.config('keys.ini')
api_key <- config$open_weather_api$api_key
wapi_id <- config$open_weather_api$wapi_id

city_forecast <- function(city_names){

  city <- c()
  weather <- c()
  temperature <- c()
  visibility <- c()
  humidity <- c()
  wind_speed <- c()
  seasons <- c()
  hours <- c()
  forecast_date <-c()
  weather_labels<-c()
  weather_details_labels<-c()
  
  # Scrape for 5-day forecasts
  for (city_name in city_names){
    open_weather_url = 'https://api.openweathermap.org/data/2.5/forecast'
    forecast_query <- list(id = wapi_id, q = city_name, 
                           appid = api_key, units="metric")
    raw_json <- GET(open_weather_url, query=forecast_query)
    
    json_list <-content(raw_json, as="parsed")
    forecasts <- json_list$list
    
    for(result in forecasts) {
      city <- c(city, city_name)
      weather <- c(weather, result$weather[[1]]$main)
      
      #Predictor variables
      temperature <- c(temperature, result$main$temp)
      visibility <- c(visibility, result$visibility)
      humidity <- c(humidity, result$main$humidity)
      wind_speed <- c(wind_speed, result$wind$speed)
      
      forecast_dt <- result$dt_txt
      hour <- as.numeric(strftime(forecast_dt, format="%H"))
      month <- as.numeric(strftime(forecast_dt, format="%m"))
      forecast_date <- c(forecast_date, forecast_dt)
      
      season <- "Spring"
      if (month >= 3 && month <= 5)
        season <- "SPRING"
      else if(month >= 6  &&  month <= 8)
        season <- "SUMMER"
      else if (month >= 9  && month <= 11)
        season <- "AUTUMN"
      else
        season <- "WINTER"
      
      # HTML labels for Leaflet
      weather_label <- paste(sep = "",
                             "<b>", city_name, "</b></br>", 
                             "<b>", result$weather[[1]]$main, "</b></br>")
      # Detailed HTML labels
      weather_detail_label <- paste(sep = "", weather_label,
                                    "Temperature: ", result$main$temp, " C </br>",
                                    "Visibility: ", result$visibility, " m </br>",
                                    "Humidity: ", result$main$humidity, " % </br>", 
                                    "Wind Speed: ", result$wind$speed, " m/s </br>", 
                                    "Datetime: ", forecast_dt, " </br>")
      
      weather_labels <- c(weather_labels, weather_label)
      weather_details_labels <- c(weather_details_labels, weather_detail_label)
      
      seasons <- c(seasons, season)
      hours <- c(hours, hour)
    }
  }
  
  # Create and return a tibble
  weather_df <- tibble(CITY_ASCII=city, WEATHER=weather, 
                       TEMPERATURE=minMaxScaler(temperature),
                       TEMPERATURE2=temperature,
                       VISIBILITY=visibility/10000, 
                       HUMIDITY=minMaxScaler(humidity), 
                       WIND_SPEED=minMaxScaler(wind_speed), 
                       SEASONS=season, HOURS=hours, 
                       FORECASTDATETIME=forecast_date, 
                       DATE=forecast_date,
                       LABEL=weather_labels, 
                       DETAILED_LABEL=weather_details_labels)
  return(weather_df)
}

