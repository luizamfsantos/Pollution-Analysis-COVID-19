# import libraries
my_packages <- c("readr","xts","zoo", "ggplot2")
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


# visualize data
aqiAust = ggplot(data, aes(Date,`2020 AQI Value`)) +
  geom_line(aes(Date,`2020 AQI Value`), color = 'grey50') + geom_point()


