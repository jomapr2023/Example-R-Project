# Workflow in R: Model Selection to Dashboard Implementation
This folder contains R code for validating and testing models of bicycle demand based on weather data from Seoul, 
applying the model to live data from open weather api, and displaying the results in a Shiny dashboard application.

## Usage:
The  R files should run as-is in RStudio. Model_selection.ipynb outputs the champion model as an R object, and can 
also run as-is in a Jupyter notebook on an R kernel, though the gradient boost model can take close to an hour to 
finish tuning on lower end devices. I do not endorse using the output for any real world decision making. The model 
is accurate for Seoul in the time period on which it was tuned. Real world applications would require more testing and 
local data for other cities.

## Requirements: 
- R
- tidyverse
- tidymodels
- stringr
- poissonreg
- ranger
- xgboost,
- httr
- fastDummies
- shiny
- ggplot2
- leaflet
- scales
- htmltools
- readr
- configr
- vip
- Jupyter Notebook

## Acknowledgements:
This project is based on a course project from IBM's Applied Data Science with R Specialization,
available here: </href> https://www.coursera.org/learn/data-science-with-r-capstone-project/home/week/5 </href>.

Comments are welcome. This is one of my earliest projects. 



