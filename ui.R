require(leaflet)
require(readr)

city_names_vector <- read_csv("selected_cities.csv")['CITY_ASCII']

shinyUI(
  fluidPage(padding=5,
  titlePanel("Bike-sharing demand forecast"), 
  sidebarLayout(
    
    mainPanel(
    leafletOutput("city_bike_map", height=800)),
    
    sidebarPanel(
      selectInput(inputId="city_dropdown", label="City Select",
                  choices=c("All", city_names_vector),
                  selected="New York"), 
      
      #Temperature plot
      plotOutput(outputId="temp_line", height=250, width=400),
      
      #Interactive demand forecast plot
      plotOutput(outputId="bike_line", click="plot_click", height=250),
      verbatimTextOutput(outputId="bike_date_output"),
      
    ))
))