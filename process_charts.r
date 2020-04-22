#Scrape PHE PDFs
#By Michael Goodier @michaelgoodier
#Completely and utterly based on a script by John burn Murdoch @jbmurdoch used to scrape google mobility reports here: https://gist.github.com/johnburnmurdoch/1d23978fc8213ff656d9f629608dd1f5
# Install and load required packages
install.packages("needs")
library(needs)
needs(tidyverse, lubridate, magrittr, animation, pdftools, png, scales, zoo)

# Function that extracts data from PHE line charts
process_chart <- function(chart_name,pdf_filepath,start_date,end_date,y_top,y_bottom,page_number=1,chart_top_px,chart_bottom_px,chart_left_px,chart_right_px,red,green,blue){
  
  # Convert n page of PDF into high-res PNG
  pdf_convert(pdf_filepath, format = "png", pages = page_number, dpi = 300, filenames = "IMG.png")
  
  # Read PDF page one image into R as an array of red, green and blue pixel values
  img <- readPNG("IMG.png")
  
  # Rescale the pixel values from 0-1 into 0-255
  r <- img[,,1] * 255
  g <- img[,,2] * 255
  b <- img[,,3] * 255
  
  # rgb(66,133,244) rgb(0,176,146)is the rgb specification of the green line used in PHE charts
  #chart one 
  # gastro charts from (759) to (2296) left to right, so we loop through those pixels from left to right, and in each case
  # val <- c(x_leftmost_chart_pixel, x_rightmost_pixel)
  val <- c(chart_left_px:chart_right_px) %>%
    map_dbl(~{
      # find the vertical pixels that match our specified rgb() colour, and average them to get the middle pixel of the line on the chart
      mean(which(
        r[chart_top_px:chart_bottom_px,.x]==red & g[chart_top_px:chart_bottom_px,.x]==green & b[chart_top_px:chart_bottom_px,.x]==blue
      ))
    }) %>%
    # rescale this value so instead of being a pixel number, it’s a value from -100 to 80 (the domain of the y-axis)
    scales::rescale(to=c(y_top, y_bottom), from = c(1, chart_bottom_px-chart_top_px))
  # then generate a series of data values equal to the number of horizontal chart pixels, so we can match a date/time to every y-axis value
  date <- seq.Date(as.Date(start_date), as.Date(end_date), length.out = length(val))
  # then join values to dates, summarise by individual day (average value per day)
  chart <- tibble(date, val) %>%
    mutate(date = ymd(date)) %>%
    filter(is.finite(val)) %>%
    group_by(date) %>%
    summarise(value = mean(val, na.rm=T)) %>%
    mutate(chart_name = chart_name)
  
  unlink("IMG.png")
  return(chart)
}

#To run this on multiple charts, make a csv with the following columns, a row for each chart
#chart_name,pdf_filepath,start_date,end_date,y_top,y_bottom,page_number,chart_top_px,chart_bottom_px,chart_left_px,chart_right_px,red,green,blue
#read in a dataframe with all the inputs for our different charts
chart_data <- read_csv("chart_dimensions.csv") %>%
  pmap_dfr(process_chart) %>%
  group_by(chart_name) %>% 
  arrange(date) %>%
  mutate(rolling_average = rollapplyr(value,7,mean,na.rm=T,fill=NA))

write_csv(chart_data,"chart_data.csv")