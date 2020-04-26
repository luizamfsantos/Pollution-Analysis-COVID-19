# import libraries
my_packages <- c("readr","ggplot2","dplyr","tidyverse")
lapply(my_packages,library,character.only = TRUE)

# import data
austin_air_data <- read_csv('data/air_quality/epa_air_data_daily_air_quality_ozone_pm25_austin_roundrock.csv',
                 col_names = TRUE,
                 col_types = cols(
                   'Date' = col_date(format = "%m/%d/%Y"),
                   '2020 AQI Value' = col_double(),
                   'Main Pollutant' = col_character(),
                   'Site Name' = col_skip(),
                   'Site ID' = col_skip(),
                   'Source' = col_skip(),
                   '20-year High (1980-2019)' = col_double(),
                   '20-year Low (1980-2019)' = col_double(),
                   '5-year Median (2015-2019)' = col_double()
                   )
                 )

health <- vector(mode = "list", length = dim(austin_air_data)[1])
for (i in 1:dim(austin_air_data)[1]) {
  if (austin_air_data[i,2] <= 50) {
    health[i] <- 'darkgreen'
  } else  if (austin_air_data[i,2] <= 100) {
    health[i] <-  'yellow'
  }  else  if (austin_air_data[i,2] <= 150) {
    health[i] <- 'orange' 
  } else  if (austin_air_data[i,2] <= 200) {
    health[i] <- 'red' 
  } else  if (austin_air_data[i,2] <= 300) {
    health[i] <- 'purple' 
  } else {
    health[i] <- 'maroon'
  }
}
health_high <- vector(mode = "list", length = dim(austin_air_data)[1])
for (i in 1:dim(austin_air_data)[1]) {
  if (austin_air_data[i,'20-year High (1980-2019)'] <= 50) {
    health_high[i] <- 'darkgreen'
  } else  if (austin_air_data[i,'20-year High (1980-2019)'] <= 100) {
    health_high[i] <-  'yellow'
  }  else  if (austin_air_data[i,'20-year High (1980-2019)'] <= 150) {
    health_high[i] <- 'orange' 
  } else  if (austin_air_data[i,'20-year High (1980-2019)'] <= 200) {
    health_high[i] <- 'red' 
  } else  if (austin_air_data[i,'20-year High (1980-2019)'] <= 300) {
    health_high[i] <- 'purple' 
  } else {
    health_high[i] <- 'maroon'
  }
}
health_low <- vector(mode = "list", length = dim(austin_air_data)[1])
for (i in 1:dim(austin_air_data)[1]) {
  if (austin_air_data[i,'20-year Low (1980-2019)'] <= 50) {
    health_low[i] <- 'darkgreen'
  } else  if (austin_air_data[i,'20-year Low (1980-2019)'] <= 100) {
    health_low[i] <-  'yellow'
  }  else  if (austin_air_data[i,'20-year Low (1980-2019)'] <= 150) {
    health_low[i] <- 'orange' 
  } else  if (austin_air_data[i,'20-year Low (1980-2019)'] <= 200) {
    health_low[i] <- 'red' 
  } else  if (austin_air_data[i,'20-year Low (1980-2019)'] <= 300) {
    health_low[i] <- 'purple' 
  } else {
    health_low[i] <- 'maroon'
  }
}

health_median <- vector(mode = "list", length = dim(austin_air_data)[1])
for (i in 1:dim(austin_air_data)[1]) {
  if (austin_air_data[i,'5-year Median (2015-2019)'] <= 50) {
    health_median[i] <- 'darkgreen'
  } else  if (austin_air_data[i,'5-year Median (2015-2019)'] <= 100) {
    health_median[i] <-  'yellow'
  }  else  if (austin_air_data[i,'5-year Median (2015-2019)'] <= 150) {
    health_median[i] <- 'orange' 
  } else  if (austin_air_data[i,'5-year Median (2015-2019)'] <= 200) {
    health_median[i] <- 'red' 
  } else  if (austin_air_data[i,'5-year Median (2015-2019)'] <= 300) {
    health_median[i] <- 'purple' 
  } else {
    health_median[i] <- 'maroon'
  }
}
austin_air_data %>% tibble() %>% add_column(health)
austin_air_data %>% tibble() %>% add_column(health_high)
austin_air_data %>% tibble() %>% add_column(health_low)
austin_air_data %>% tibble() %>% add_column(health_median)
# visualize data
aqiAust = ggplot(austin_air_data, aes(Date,`2020 AQI Value`)) +xlab('Time')+ ylab('AQI Values')+
  geom_line(aes(Date,`2020 AQI Value`), color = 'blue') +
  geom_point(color = health) + theme(
    panel.background = element_rect(fill = "grey",
                                    colour = "grey",
                                    size = 0.5, linetype = "solid")
  )
aqiAustHigh = ggplot(austin_air_data, aes(Date,`20-year High (1980-2019)`)) +xlab('Time')+ ylab('AQI Values')+
  geom_line(aes(Date,`20-year High (1980-2019)`), color = 'red') +
  geom_point(color = health_high) + theme(
    panel.background = element_rect(fill = "grey",
                                    colour = "grey",
                                    size = 0.5, linetype = "solid")
  )
aqiAustLow = ggplot(austin_air_data, aes(Date,`20-year Low (1980-2019)`)) +xlab('Time')+ ylab('AQI Values')+
  geom_line(aes(Date,`20-year Low (1980-2019)`), color = 'green') +
  geom_point(color = health_low) + theme(
    panel.background = element_rect(fill = "grey",
                                    colour = "grey",
                                    size = 0.5, linetype = "solid")
  )
aqiAustMedian = ggplot(austin_air_data, aes(Date,`5-year Median (2015-2019)`)) +xlab('Time')+ ylab('AQI Values')+
  geom_line(aes(Date,`5-year Median (2015-2019)`), color = 'orange') +
  geom_point(color = health_median) + theme(
    panel.background = element_rect(fill = "grey",
                                    colour = "grey",
                                    size = 0.5, linetype = "solid")
  )
