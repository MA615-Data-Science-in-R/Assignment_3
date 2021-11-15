Sys.setenv("LANGUAGE" = "EN")
Sys.setlocale("LC_ALL", "C")

library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(ggplot2)
library(dplyr)
addRepo("geanders")

data("hurr_tracks")

data("rain")

head(hurr_tracks)

head(rain, 15)

#update the hurrincanexposuredata
library(drat)
addRepo("geanders")
install.packages("hurricaneexposuredata")

#Extract the data of Katrina
Katrina_track <- filter(hurr_tracks, storm_id =="Katrina-2005")

map_counties(storm = "Katrina-2005", metric = "rainfall",days_included = c(0)) +
    ggtitle("Katrina-2005") +
    theme(plot.title = element_text(hjust = 0.5))



map_rain_exposure(storm ="Katrina-2005", 
                  rain_limit = 175, 
                  dist_limit = 500, 
                  days_included =-5:3) +
    ggtitle("Katrina-2005") +
    theme(plot.title = element_text(hjust = 0.5))


###############################################


# map_counties(storm = "Allison-2001", metric= "rainfall", days_included = -1:0) +
#     ggtitle("Rain Allison")
# 
# 
# 
# map_counties(storm = "Allison-2001", metric = "rainfall", days_included = -5:3) +
#     ggtitle("rain Allison 2001")
# 


map_counties(storm = "Katrina-2005", metric = "wind")


map_counties("Katrina-2005", metric = "wind", wind_var = "vmax_gust", days_included = c(0))



map_counties("Katrina-2005", metric = "wind", wind_var = "vmax_gust", wind_source = "ext_tracks")
# 
# 
# map_counties(storm = "Sandy-2012", metric = "distance")
# 
# map_distance_exposure(storm = "Sandy-2012", dist_limit = 75)
# 
# # 
# 
# map_rain_exposure(storm ="Allison-2001", 
#                   rain_limit = 175, 
#                   dist_limit = 500, 
#                   days_included =-5:3)



library(weathermetrics)

map_wind_exposure(storm = "Katrina-2005",
    wind_limit = convert_wind_speed(34, "knots","mps"))


map_event_exposure(storm = "Katrina-2005", event_type = "flood",add_track = TRUE)

map_event_exposure(storm = "Katrina-2005", event_type = "tornado")


map_tracks(storms = "Katrina-2005")



map_tracks(storms = c("Andrew-1992", "Katrina-2005", "Rita-2005"), 
                 alpha = 0.5,
                 plot_points = TRUE,
                 color = "blue")


storms_2018 <- hurr_tracks %>% select(storm_id) %>% 
                                distinct() %>% 
                               mutate(year = str_extract(storm_id, "-[0-9].+")) %>% 
                               filter(year == "-2018")

map_tracks(storms = storms_2018$storm_id)




katrina_map <- map_event_exposure(storm = "Katrina-2005", event_type = "flood")

map_tracks(storms = "Katrina-2005",
    plot_object =floyd_map,
    plot_points =TRUE,
    color ="darkgray"
)

