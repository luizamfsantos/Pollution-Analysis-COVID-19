# import libraries
my_packages <- c("readr","ggplot2","dplyr","tidyverse")
lapply(my_packages,library,character.only = TRUE)

# import data
san_diego_data_aqi_2019 <- read_csv('data/air_quality/san_diego_2019_aqi.csv',
                                    col_types = cols(
                                      'date_local' = col_date(format='%Y-%m-%d'),
                                      'aqi' = col_double()))
health <- vector(mode = "list", length = dim(san_diego_data_aqi_2019)[1])
for (i in 1:dim(san_diego_data_aqi_2019)[1]) {
  if (san_diego_data_aqi_2019[i,2] <= 50) {
    health[i] <- 'darkgreen'
  } else  if (san_diego_data_aqi_2019[i,2] <= 100) {
    health[i] <-  'yellow'
  }  else  if (san_diego_data_aqi_2019[i,2] <= 150) {
    health[i] <- 'orange' 
  } else  if (san_diego_data_aqi_2019[i,2] <= 200) {
    health[i] <- 'red' 
  } else  if (san_diego_data_aqi_2019[i,2] <= 300) {
    health[i] <- 'purple' 
  } else {
    health[i] <- 'maroon'
  }
}

san_diego_data_aqi_2019 %>% tibble() %>% add_column(health)

# visualize data
aqiSanD = ggplot(san_diego_data_aqi_2019,aes(date_local,aqi)) +
  geom_line(aes(date_local,aqi)) + xlab('Time')+ ylab('AQI Values')+
  geom_point(color = health) + theme(
    panel.background = element_rect(fill = "grey",
                                    colour = "grey",
                                    size = 0.5, linetype = "solid")
  )
