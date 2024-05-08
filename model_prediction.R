require(tidyverse)
require(tidymodels)
require(stringr)
require(poissonreg)
require(ranger)
require(xgboost)
require(httr)
require(fastDummies)
source("open_weather_forecast_retriever.R")

champ <- readRDS("./champion_model.R")

#Format inputs for use with saved model
predict_bike_demand <- function(df){
  out <- df
  out <- out %>% 
    mutate(HOUR_FACTOR = factor(HOURS, levels = 0:23)) %>%
    mutate(SEASON_FACTOR = factor(SEASONS))
  out <- dummy_cols(out, select_columns = "HOUR_FACTOR", 
                    omit_colname_prefix = TRUE,
                    remove_selected_columns = TRUE)
  out <- dummy_cols(out, select_columns = "SEASON_FACTOR", 
                    omit_colname_prefix = TRUE,
                    remove_selected_columns = TRUE) 
  
  if (("WINTER" %in% names(out))==FALSE){
    out["WINTER"]<-0
  }
  
  if (("AUTUMN" %in% names(out))==FALSE){
    out["AUTUMN"]<-0
  }
  
  if (("SUMMER" %in% names(out))==FALSE){
    out["SUMMER"]<-0
  }
  
  if (("SPRING" %in% names(out))==FALSE){
    out["SPRING"]<-0
  }
  
  preds <- champ %>% predict(out) %>% mutate(.pred=.pred*as.logical(.pred>=0))
  out["BIKE_PREDICTION"] <- preds
  
  out <- out %>% 
    mutate("BIKE_PREDICTION_LEVEL" = if_else(
      out["BIKE_PREDICTION"] <= 1000, "small", if_else(
      out["BIKE_PREDICTION"]  < 3000, "medium", "large")))
  return(out)
}



# Apply model to real-time data
generate_city_weather_bike_data <- function (){
  cities_df <- read_csv("selected_cities.csv")
  weather_df <- city_forecast(cities_df$CITY_ASCII)
  results <- predict_bike_demand(weather_df)
  
  cities_bike_pred <- cities_df %>% left_join(results) %>% 
    select("CITY_ASCII", "LNG", "LAT", "TEMPERATURE2", "HUMIDITY", "BIKE_PREDICTION", 
           "BIKE_PREDICTION_LEVEL", "LABEL", "DETAILED_LABEL", "FORECASTDATETIME")
  return(cities_bike_pred)
}
