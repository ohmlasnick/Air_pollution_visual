library(crqa)
library(ggplot2)
library(stringr)
library(dplyr)
library(usmap)
library(maps)
library(mapdata)
library(purrr)
library(knitr)
library(animation)

setwd("/Users/ohml/Documents/Coding_projects/Air_Pollution_Visual")

air_pollution_df <- read.csv("CDC_PM25_2016-2019.csv")

# Recode date into numeric value (YYYYMMDD)
recode_date <- function(date_char){
  date_char <- str_replace_all(date_char, c(JAN="01", FEB="02", MAR="03",
                                            APR="04", MAY="05", JUN="06",
                                            JUL="07", AUG="08", SEP="09",
                                            OCT="10", NOV="11", DEC="12"))
  date_char <- paste(substr(date_char, 5, 8), 
                     substr(date_char, 3, 4),
                     substr(date_char, 1, 2), sep="")
  return(date_char)
}

# Reverse month encoding to get clean string
date_to_str <- function(mo_int){
  return(str_replace_all(mo_int, c("01"="Jan", "02"="Feb", "03"="Mar",
                               "04"="Apr", "05"="May", "06"="Jun",
                               "07"="Jul", "08"="Aug", "09"="Sep",
                               "10"="Oct", "11"="Nov", "12"="Dec")))
}

# Convert date to Date-time format
date_time_conv <- function(date){
  return(paste(substr(date, 1, 4), "-",
               substr(date, 5, 6), "-",
               substr(date, 7, 8), sep=''))
}

# Encode 5-digit county FIPS code from state & county, 
# padding with leading zeros if necessary
encode_fips <- function(state, county){
  if (state < 10){
    state <- paste('0', state, sep='')
  }
  if (county < 10){
    county <- paste('00', county, sep='')
  } else if (county < 100) {
    county <- paste('0', county, sep='')
  }
  return(paste(state, county, sep=''))
}

# Recode dates as numeric type
air_pollution_df$numeric_date <- as.numeric(recode_date(air_pollution_df$date))

# Recode separate state & county FIPS codes into single code
air_pollution_df$fips <- as.character(map2(air_pollution_df$statefips, 
                                           air_pollution_df$countyfips, encode_fips))

# Choose PM 25 concentrations as numeric value to plot
air_pollution_df$values <- as.numeric(air_pollution_df$DS_PM_pred)

# Order by date if you want to visualize as time series data
air_pollution_df <- air_pollution_df %>%
  arrange(
    numeric_date
  )

palette <- c("white", "#FFFFC8", "#F9D882",
             "#F5A200","#D73300","#990316", 
             "#682714", "black")

# Plot one frame of the animation
create_frame <- function(start, end, month=FALSE) {
  
  year <- substr(start, 1, 4)
  if (month == TRUE){
    mo <- date_to_str(substr(start, 5, 6))
  } else {
    mo <- ''
  }
  
  # Restrict date ranges
  air_pollution_timeline <- subset(air_pollution_df, start <= numeric_date & numeric_date < end)
  
  # Create analyzable format for plot_usmap
  air_pollution_reduced <- select(air_pollution_timeline, c('fips', 'values'))
  
  # Average PM25 across all days in timeline for each fips
  average_pm25 <- aggregate(values ~ fips, air_pollution_reduced, mean)
  
  # Create the plot
  plot_usmap(regions = "counties", data = average_pm25) +
    scale_fill_gradientn(limits = range(0, 45), colours = palette) +
    labs(title = paste(mo, year), fill = "PM 2.5\n(\u00b5g/m\u00b3)") + 
    theme(panel.background=element_blank(), 
          legend.position = c(0.7, 1.0),
          legend.direction = "horizontal",
          legend.text = element_text(size=14),
          legend.title = element_text(size=20),
          plot.title = element_text(size=48,face="italic", hjust=0.2))
}

create_frame(20160101, 20191231, FALSE)

oopt <- animation::ani.options(interval = 0.1, ani.height = 1080)

# Create an animation (by year or month)
# If interval is month, must provide a given start & end
# If interval is year, plot all years
animate_map <- function(start=20160101, end=20200101, interval){
  if (interval == "mo"){
    fr <- seq(start, end, by=00000100)
    fr <- head(fr["01" <= substr(fr, 5, 6) & substr(fr, 5, 6) <= "12"], -1)
    print(fr)
    lapply(fr, function(i) {
      print(create_frame(i, i + 00000100, TRUE))
      ani.pause()
    })
  } else if (interval == "yr"){
    fr <- c(20160101, 20170101, 20180101, 20190101)
    lapply(fr, function(i) {
      print(create_frame(i, i + 00010000))
      ani.pause()
    })
  }
}

saveHTML(animate_map(interval = "mo"), autoplay = FALSE, loop = TRUE, 
         verbose = FALSE, outdir = getwd(), navigator = TRUE, htmlfile = "months.html",
         single.opts = "'controls': ['first', 'previous', 'play', 'next', 'speed'], 
         'delayMin': 0")

### County-level values ###
county_series <- subset(air_pollution_df, 
                        statefips == 1 & countyfips == 1)

county_series$Date_format <- as.Date(date_time_conv(county_series$numeric_date), "%Y-%m-%d")

ggplot(data = county_series, mapping = aes(Date_format, DS_PM_pred)) +
  geom_point() + geom_smooth() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
