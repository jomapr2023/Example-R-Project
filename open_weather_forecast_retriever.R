require(tidyverse)
require(httr)
library(readr)
library(configr)

#Fetch keys from external file
config <- read.config('keys.ini')
api_key <- config$open_weather_api$api_key
wapi_id <- config$open_weather_api$wapi_id

minMaxScaler <- function(var){
  var <- (var - min(var)) / (max(var) - min(var))
}

season <- function(month){
  quarter <- ""
  if (month >= 3 && month <= 5)
    quarter <- "SPRING"
  else if(month >= 6  &&  month <= 8)
    quarter <- "SUMMER"
  else if (month >= 9  && month <= 11)
    quarter <- "AUTUMN"
  else
    quarter <- "WINTER"
  return(quarter)
}

city_forecast <- function(city_names){
 
  weather_df <- tibble(CITY_ASCII=character(), 
                       WEATHER=character(), 
                       TEMPERATURE=numeric(),
                       VISIBILITY=numeric(), 
                       HUMIDITY=numeric(), 
                       WIND_SPEED=numeric(),
                       HOURS=numeric(), 
                       MONTH=numeric(),
                       FORECASTDATETIME=numeric(),
                       LABEL=character(),
                       DETAILED_LABEL=character(),
                       SEASONS=character())
  
  # Scrape for 5-day forecasts
  for (city_name in city_names){
    open_weather_url = 'https://api.openweathermap.org/data/2.5/forecast'
    forecast_query <- list(id = wapi_id, q = city_name, 
                           appid = api_key, units="metric")
    raw_json <- GET(open_weather_url, query=forecast_query)
    
    json_list <-content(raw_json, as="parsed")
    forecasts <- json_list$list
    
    for(result in forecasts) {
      observation <- list(CITY_ASCII=city_name,
                          WEATHER=result$weather[[1]]$main, 
                          TEMPERATURE=result$main$temp,
                          TEMPERATURE2=result$main$temp,
                          VISIBILITY=result$visibility, 
                          HUMIDITY=result$main$humidity,
                          WIND_SPEED=result$wind$speed,
                          HOURS=as.numeric(strftime(result$dt_txt, format="%H")),
                          MONTH=as.numeric(strftime(result$dt_txt, format="%m")),
                          FORECASTDATETIME=result$dt_txt,
                          DATE=result$dt_txt,
                          SEASONS=season(as.numeric(strftime(result$dt_txt, 
                                                             format="%m"))),
                          LABEL=paste(sep = "",
                                      "<b>", city_name, "</b></br>", 
                                      "<b>", result$weather[[1]]$main, "</b></br>"),
                          DETAILED_LABEL=paste(sep = "", 
                                               "<b>", city_name, "</b></br>", 
                                               "<b>", result$weather[[1]]$main, "</b></br>",
                                               "Temperature: ", result$main$temp, " C </br>",
                                               "Visibility: ", result$visibility, " m </br>",
                                               "Humidity: ", result$main$humidity, " % </br>", 
                                               "Wind Speed: ", result$wind$speed, " m/s </br>", 
                                               "Datetime: ", result$dt_txt, " </br>"))
      #Check for missing values
      if (any(sapply(observation, is.null))==0){
        weather_df <- weather_df %>% rbind(observation)
      }
    }
    
  }
  #Rescale for use with champion model 
  weather_df['VISIBILITY'] <- weather_df$visibility/10000
  weather_df['HUMIDITY'] <- minMaxScaler(weather_df$HUMIDITY)
  weather_df['WIND_SPEED'] <- minMaxScaler(weather_df$WIND_SPEED)
  weather_df['TEMPERATURE'] <- minMaxScaler(weather_df$TEMPERATURE)
  
  return(weather_df)
}
