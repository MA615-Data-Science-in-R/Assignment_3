

library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)

addRepo("geanders")


data("hurr_tracks")

data("rain")

head(hurr_tracks)

head(rain, 15)

unique(rain$storm_id)



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
 
mapps <- hmapper("Dennis-2005") 
mapps <- hmapper("Ike-2008") 


mapps$rmap
mapps$wmap
mapps$expos


## let's use Ike-2008

## now let's look at Ike's landfall

## https://ggle.io/4NAD

## https://www.ndbc.noaa.gov/



###############################################


map_counties(storm = "Allison-2001", metric= "rainfall", days_included = -1:0) +
    ggtitle("Rain Allison")



map_counties(storm = "Allison-2001", metric = "rainfall", days_included = -5:3) +
    ggtitle("rain Allison 2001")



map_counties(storm = "Katrina-2005", metric = "wind")


map_counties("Katrina-2005", metric = "wind", wind_var = "sust_dur")



map_counties("Katrina-2005", metric = "wind", wind_source = "ext_tracks")


map_counties(storm = "Sandy-2012", metric = "distance")

map_distance_exposure(storm = "Sandy-2012", dist_limit = 75)



map_rain_exposure(storm ="Allison-2001", 
                  rain_limit = 175, 
                  dist_limit = 500, 
                  days_included =-5:3)



library(weathermetrics)

map_wind_exposure(storm = "Katrina-2005",
    wind_limit = convert_wind_speed(34, "knots","mps"))


map_event_exposure(storm = "Floyd-1999", event_type = "flood")

map_event_exposure(storm = "Ivan-2004", event_type = "tornado")


map_tracks(storms = "Floyd-1999")



map_tracks(storms = c("Andrew-1992", "Katrina-2005", "Rita-2005"), 
                 alpha = 0.5,
                 plot_points = TRUE,
                 color = "blue")


storms_2018 <- hurr_tracks %>% select(storm_id) %>% 
                                distinct() %>% 
                               mutate(year = str_extract(storm_id, "-[0-9].+")) %>% 
                               filter(year == "-2018")

map_tracks(storms = storms_2018$storm_id)




floyd_map <- map_event_exposure(storm = "Floyd-1999", event_type = "flood")

map_tracks(storms = "Floyd-1999",
    plot_object =floyd_map,
    plot_points =TRUE,
    color ="darkgray"
)

