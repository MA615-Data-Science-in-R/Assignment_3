# MA 615 Assignment 3
Authors: William Zen, Jack Carbaugh, Zara Waheed, Kosuke Sasaki

Due: November 22, 2021

Professor: Haviland Wright, Boston University

# Hurricane Katrina Landfall

- The purpose of this assignment was to better understand the landing of Hurricane Katrina through the use of the hurricaneexposure package, as well as buoy data provided by NOAA's National Data Buoy center. 

- We first explored the hurricaneexposure package, and observed different weather variables along Katrina's track into Louisiana. We decided to mainly visualize rain, wind speed, and gust speed data. Our explorations can be found in the hmap.R and visualising.R files.

- Afterwards, we collected buoy data that was near the hurricane landing site in Louisiana. The buoy data was vast and messy, so it was filtered down into an easier and combined dataframe. Plots were then constructed to see if this information matched what we found the hurricaneexposure package. A variogram was attempted to explain wind speed and gust speed variance, but results were not very fruitful. This work can be found in the buoy import-a.R file

- Our results and findings were written as a combined report with shiny embedded so that some of the maps could be better explored by the viewer. The report can be found by running shiny.Rmd, or be pasting the html link into the following website: https://htmlpreview.github.io/
