# import libraries
my_packages <- c("readr","ggplot2")
lapply(my_packages,library,character.only = TRUE)

# import data
san_diego_data_aqi_2019 <- read_csv('data/air_quality/san_diego_2019_aqi.csv',
                                    col_types = cols(
                                      'date_local' = col_date(format='%Y-%m-%d'),
                                      'aqi' = col_double()))

# visualize data
aqiSanD = ggplot(san_diego_data_aqi_2019, aes(date_local,aqi)) +
  geom_line(aes(date_local,aqi), color = 'grey50') + geom_point()
