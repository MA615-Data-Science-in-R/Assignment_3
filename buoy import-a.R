library(tidyverse)
library(magrittr)

### reading data from buoy 44013  --  outside Boston harbor
### make URLs by splitting the URL into two pieces --
### "before the year" and "after the year"

#LA buoys: bygl1, burl1, 42040, DPIA1, gdil1, taml1, 42007, wavm6, labl1
# 42001, lkpl1, 42003, 42036, pcbf1, pclf1, 42039, shpf1, 42038, 42014, 42047, fgbl1, 42046, capl1, sbpt2, srst2, 42035
#FL buoys: mlrf1

buoy_tags <- c("bygl1", "burl1", "42040", "dpia1", "gdil1", "taml1", "42007", "wavm6", "labl1", "pcbf1", "pclf1", "42039", "shpf1", "42038", "42014", "42047", "fgbl1", "42046", "capl1", "sbpt2", "srst2", "42035", "42001", "lkpl1", "42003", "42036", "mlrf1")

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename="
url2 <- ".txt.gz&dir=data/historical/stdmet/"


years <- c(2005)


urls <- str_c(url1, buoy_tags, "h", years, url2, sep = "")

filenames <- str_c(buoy_tags, sep = "")

###  Read the data from the website

N <- length(urls)

for (i in 1:N){
  suppressMessages(  ###  This stops the annoying messages on your screen.
    file <- read_table(urls[i], col_names = TRUE)
    )
  
  
   if(colnames(file)=="YY"){
     yr = file[1,1]
     yr = paste0("19", yr)
     file %<>% rename(YYYY=YY)
     file$YYYY <- rep(yr, nrow(file))
   }
   file$station <- rep(filenames[i],nrow(file))
   
   assign(filenames[i], file)
    
 # file <- get(filenames[i]) ## get() returns the value of an object
                             ## when the arguement is the name of the 
                             ## object -- as a string.
  
                             ## example:
                             ## a <- c(1,3)
                             ## b <- get(a)  throws and error
                             ##
                             ## b <- get('a')
                             ## b
                             ## [1] 1 3
  
 
  
  
  # put '19' in front of 2 digit years
  # check that all columns are included
  # filter down to only the 1 daily observation that you want
  # etc etc etc
  
  # if(i == 1){
  #   MR <- file
  # }
  # 
  # else{
  #   MR <- rbind.data.frame(MR, file)
  # }
  
  
  
}

# test <- `42040_2005`[1,]
# test <- mutate(test,x = 29.207, y = -88.237)
# 
# library(gstat)
# library(sp)
# library(tidyverse)
# library(sf)
# library("rnaturalearth")
# library("rnaturalearthdata")
# coordinates(test) <- ~ x + y
# #proj4string(test) <- CRS("+init=epsg:28992")
# world <- ne_countries(scale = "medium", returnclass = "sf")
# ret <- class(world)
# world %>% ggplot() + geom_sf() + stat_sf_coordinates()

tighten <- function(df){
  df %<>% filter(MM == "08", DD %in% c("26","27","28","29","30","31"))
}

result <- list(`42001_2005`, `42003_2005`, `42007_2005`, `42014_2005`, `42035_2005`, `42036_2005`, `42038_2005`, `42039_2005`, `42040_2005`, `42046_2005`, `42047_2005`, burl1_2005, bygl1_2005, capl1_2005, dpia1_2005, fgbl1_2005, gdil1_2005, labl1_2005, lkpl1_2005, mlrf1_2005, pcbf1_2005, pclf1_2005, sbpt2_2005,shpf1_2005, srst2_2005, taml1_2005, wavm6_2005) %>%
  lapply(tighten)

fix_nums <- function(df){
  df %<>% mutate(YYYY = as.numeric(YYYY), DD = as.numeric(DD), MM = as.numeric(MM), hh = as.numeric(hh), mm = as.numeric(mm))
}
result2 <- lapply(result, fix_nums)

fulldf <- bind_rows(result2)
