# import libraries
my_packages <- c("readr","xts","zoo", "ggplot2")
lapply(my_packages,library,character.only = TRUE)

# import data
data <- read_csv('data/air_quality/epa_air_data_daily_air_quality_ozone_pm25_austin_roundrock.csv',
                 col_names = TRUE)

# clean data
date <- as.Date(unlist(data[1]), format = "%m/%d/%Y")
air  <- xts(data[,2:ncol(data)],
            order.by = date)
for (i in c(1,6,7,8)){
  air[,i] <- as.numeric(unlist(air[,i]))  
}

# plot data
ggplot(air[,1])
