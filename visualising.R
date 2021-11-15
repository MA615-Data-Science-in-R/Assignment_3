# Load relevant packages and data

library(tidyverse)
library(drat)
library(lubridate)
library(hurricaneexposuredata)
library(hurricaneexposure)

addRepo("geanders")

data("hurr_tracks")

data("rain")

head(hurr_tracks)

head(rain, 15)

unique(rain$storm_id)

# Define function to look at rainfall, wind 

hmapper <- function(hurr){
  
  rmap = map_counties(storm = hurr, metric = "rainfall") +
    ggtitle(hurr) +
    theme(plot.title = element_text(hjust = 0.5))
  
  wmap = map_counties(storm = hurr, metric = "wind") +
    ggtitle(hurr) +
    theme(plot.title = element_text(hjust = 0.5))
  
  expos = map_rain_exposure(storm =hurr, 
                            rain_limit = 175, 
                            dist_limit = 500, 
                            days_included =-5:3) +
    ggtitle(hurr) +
    theme(plot.title = element_text(hjust = 0.5))
  
  ml <-  list(rmap, wmap, expos)
  names(ml) <- c("rmap", "wmap", "expos")
  
  return(ml)
}


hmap <- hmapper("Katrina-2005") 

hmap$rmap
hmap$wmap
hmap$expos

# Mapping

map_counties(storm = "Katrina-2005", metric= "rainfall", days_included = -1:0) +
  ggtitle("Rain Katrina")

map_counties("Katrina-2005", metric = "wind", wind_var = "vmax_gust")

# Extract time from the data
hurr_tracks$year <- substr(hurr_tracks$date, 1, 4) 
hurr_tracks$month <- substr(hurr_tracks$date, 5, 6) 
hurr_tracks$day <- substr(hurr_tracks$date, 7, 8)
hurr_tracks$time <- substr(hurr_tracks$date, 9, 12)

# Filter the data

Katrina <- hurr_tracks %>% filter(storm_id == "Katrina-2005", day == "25")


