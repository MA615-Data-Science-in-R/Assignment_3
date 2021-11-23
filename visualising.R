# Load relevant packages and data

library(tidyverse)
library(drat)
library(lubridate)
library(hurricaneexposuredata)
library(hurricaneexposure)

addRepo("geanders")

data("hurr_tracks")

data("rain")

# head(hurr_tracks)
# 
# head(rain, 15)
# 
# unique(rain$storm_id)

# Define function to look at rainfall, wind 

hmapper <- function(hurr){
  
  rmap = map_counties(storm = hurr, metric = "rainfall") +
    ggtitle("Rainfall") +
    theme(plot.title = element_text(hjust = 0.5))
  
  expos = map_rain_exposure(storm =hurr, 
                            rain_limit = 175, 
                            dist_limit = 500, 
                            days_included =-5:3) +
    ggtitle("High Rain") +
    theme(plot.title = element_text(hjust = 0.5))
  
  wmap1 = map_counties(storm = hurr, metric = "wind") +
    ggtitle("Max Sustained Winds") +
    theme(plot.title = element_text(hjust = 0.5))
  
  wmap2 = map_counties("Katrina-2005", metric = "wind", wind_var = "vmax_gust") +
    ggtitle("Max Gust Winds") +
    theme(plot.title = element_text(hjust = 0.5))
  
  wmap3 = map_counties(storm = hurr, metric = "wind", wind_var = "sust_dur") +
    ggtitle("Minutes of Sustained Winds of 20 m/s or Higher") +
    theme(plot.title = element_text(hjust = 0.5))
  
  wmap4 = map_counties(storm = hurr, metric = "wind", wind_var = "gust_dur") +
    ggtitle("Minutes of Gust Winds of 20 m/s or Higher") +
    theme(plot.title = element_text(hjust = 0.5))
  
  ml <-  list(rmap, expos, wmap1, wmap2, wmap3, wmap4)
  names(ml) <- c("rmap", "expos", "wmap1", "wmap2", "wmap3", "wmap4")
  
  return(ml)
}


hmap <- hmapper("Katrina-2005") 

# hmap$rmap
# hmap$expos
hmap$wmap1
hmap$wmap2
hmap$wmap3
hmap$wmap4
# 
# # Extract time from the data
# hurr_tracks$year <- substr(hurr_tracks$date, 1, 4) 
# hurr_tracks$month <- substr(hurr_tracks$date, 5, 6) 
# hurr_tracks$day <- substr(hurr_tracks$date, 7, 8)
# hurr_tracks$time <- substr(hurr_tracks$date, 9, 12)
# 
# # Filter the data
# 
# Katrina <- hurr_tracks %>% filter(storm_id == "Katrina-2005", day == "25")


