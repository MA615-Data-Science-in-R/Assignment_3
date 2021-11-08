library(tidyverse)
library(magrittr)

### reading data from buoy 44013  --  outside Boston harbor
### make URLs by splitting the URL into two pieces --
### "before the year" and "after the year"

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"


years <- c(1987:2016)


urls <- str_c(url1, years, url2, sep = "")

filenames <- str_c("mr", years, sep = "")

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

