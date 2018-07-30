# Tsibble time


library(forecast) # devtools::install_github("robjhyndman/forecast")
library(tsibble) # devtools::install_github("tidyverts/tsibble")
library(fable) # devtools::install_github("tidyverts/fable")
library(tsibblestats) # devtools::install_github("tidyverts/tsibblestats")
library(tsibbledata) # devtools::install_github("tidyverts/tsibbledata")
library(tidyverse)
library(sugrrants)

# traditional forcast examples
# install.packages("nycflights13")

# tsibble
weather <- nycflights13::weather %>% 
    select(origin, time_hour, temp, humid, precip)
weather_tsbl <- as_tsibble(weather, key = id(origin), index = time_hour)
weather_tsbl

full_weather <- weather_tsbl %>%
    fill_na(precip = 0) %>% 
    group_by(origin) %>% 
    tidyr::fill(temp, humid, .direction = "down")
full_weather

full_weather %>%
    group_by(origin) %>%
    index_by(year_month = yearmonth(time_hour)) %>% # monthly aggregates
    summarise(
        avg_temp = mean(temp, na.rm = TRUE),
        med_temp = median(temp, na.rm = TRUE),
        ttl_precip = sum(precip, na.rm = TRUE)
    )

# fable
library( datasets )
u = UKLungDeaths %>%
    ETS(log(mdeaths)) %>%
    forecast %>%
    autoplot
