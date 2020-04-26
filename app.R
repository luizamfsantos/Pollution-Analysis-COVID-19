# EarthXHack 2020
# Contributors: Leidy Buescher, Luiza Santos 


# load needed packages
if(!require(shiny)) install.packages("shiny")
if(!require(shinyWidgets)) install.packages("shinyWidgets")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(dplyr)) install.packages("dplyr")
if(!require(maps)) install.packages("maps")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(reshape2)) install.packages("reshape2")
if(!require(ggiraph)) install.packages("ggiraph")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(leaflet)) install.packages("leaflet")
if(!require(plotly)) install.packages("plotly")
if(!require(geojsonio)) install.packages("geojsonio")
if(!require(rgdal)) install.packages("rgdal")
if(!require(stringr)) install.packages("stringr")

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(leaflet)
library(geojsonio)
library(plotly)
library(ggiraph)
library(maps)
library(rgdal)
library(RCurl)
library(stringr)
library(markdown)
library(readr)

downloader::download(url = "https://github.com/eparker12/nCoV_tracker/blob/master/input_data/50m.geojson", destfile = "data/50m.GeoJSON")

# set mapping colour
covid_col = "#cc4c02"
covid_other_col = "#662506"

# import data for covid map
f1 <- getURL('https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/coronavirus.csv', ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
f2 <- getURL('https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/countries_codes_and_coordinates.csv', ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
f3 <- getURL('https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/50m.geojson', ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
f4 <- getURL('https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/country_geoms.csv', ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
cv_cases = read.csv(textConnection(f1), header = T)
countries = read.csv(textConnection(f2), header = T)
worldcountry = readOGR(dsn = f3,  layer = "OGRGeoJSON")
country_geoms = read.csv(textConnection(f4), header = T)


### MAP FUNCTIONS ###
# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
  plot_df = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative cases") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  plot_df_new = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = date, y = new, fill = region)) + 
    geom_bar(position="stack", stat="identity") + 
    ylab("new cases") + theme_bw() + 
    scale_fill_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new cases by region
country_cases_plot = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death")) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = region, 
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
      xlim(c(cv_min_date,current_date+1)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = new_outcome, fill = region, 
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",new_outcome)))+
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = new_outcome, fill = region, 
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",new_outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g +
    geom_bar(position="stack", stat="identity") + 
    ylab("new") + theme_bw() + 
    scale_fill_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region
country_cases_cumulative = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death")) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(cv_min_date,current_date+1)) + xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_case100,"\n", region, ": ",outcome))) +
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_death10,"\n", region, ": ",outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region on log10 scale
country_cases_cumulative_log = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death"))  {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(cv_min_date,current_date+1)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",outcome))) +
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative (log10)") + theme_bw() +
    scale_y_continuous(trans="log10") +
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}


### DATA PROCESSING: COVID-19 ###

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
  cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$per100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$newper100k = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$activeper100k = as.numeric(format(round(cv_cases$active_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)

# add variable for days since 100th case and 10th death
cv_cases$days_since_case100 = cv_cases$days_since_death10 = 0
for (i in 1:length(unique(cv_cases$country))) {
  country_name = as.character(unique(cv_cases$country))[i]
  country_db = subset(cv_cases, country==country_name)
  country_db$days_since_case100[country_db$cases>=100] = 1:sum(country_db$cases>=100)
  country_db$days_since_death10[country_db$deaths>=10] = 1:sum(country_db$deaths>=10)
  cv_cases$days_since_case100[cv_cases$country==country_name] = country_db$days_since_case100
  cv_cases$days_since_death10[cv_cases$country==country_name] = country_db$days_since_death10
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_China = sum(cv_today$cases[cv_today$country=="Mainland China"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Mainland China"])
current_death_count = sum(cv_today$deaths)

# create subset for countries with at least 100 cases
cv_today_100 = subset(cv_today, cases>=100)


# aggregate at continent level
cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# add variable for days since 100th case and 10th death
cv_cases_continent$days_since_case100 = cv_cases_continent$days_since_death10 = 0
cv_cases_continent$continent = cv_cases_continent$continent_level
for (i in 1:length(unique(cv_cases_continent$continent))) {
  continent_name = as.character(unique(cv_cases_continent$continent))[i]
  continent_db = subset(cv_cases_continent, continent==continent_name)
  continent_db$days_since_case100[continent_db$cases>=100] = 1:sum(continent_db$cases>=100)
  continent_db$days_since_death10[continent_db$deaths>=10] = 1:sum(continent_db$deaths>=10)
  cv_cases_continent$days_since_case100[cv_cases_continent$continent==continent_name] = continent_db$days_since_case100
  cv_cases_continent$days_since_death10[cv_cases_continent$continent==continent_name] = continent_db$days_since_death10
}

# aggregate at global level
cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
cv_cases_global$days_since_case100 = cv_cases_global$days_since_death10 = 1:nrow(cv_cases_global)

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$ADM0_A3)
if (all(cv_large_countries$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,1,10,50,100,500,1000,Inf)
cv_pal <- colorBin("Oranges", domain = cv_large_countries$per100k, bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% cv_large_countries$alpha3, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2019-COVID (active)", "2019-COVID (new)", "2019-COVID (cumulative)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID (new)", "2019-COVID (cumulative)"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-50,~80,80) %>%
  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$per100k,
            title = "<small>Active cases per 100,000</small>") 

# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
names(cv_aggregated) = c("date", "cases")

# add variable for new cases in last 24 hours
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new[i] = 0 }
  if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
cls_names = c(as.character(unique(cv_cases$country)), as.character(unique(cv_cases_continent$continent)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names

library(stringr)


# Import Data

# San Diego Traffic Volume Data
sd_traffic_count <- read.csv("data/traffic/san_diego_traffic_counts.csv")
# create column for year in traffic data by splitting date_count attribute
y <- str_split(as.character(sd_traffic_count$date_count),'/', simplify = TRUE) 
y <- substr(y[,3], 1, 2)
sd_traffic_count$year <- y
date_normal_format <- str_split(as.character(sd_traffic_count$date_count),' ', simplify = TRUE) 
sd_traffic_count$date_count <- date_normal_format[,1]

# San Diego Traffic Collision Data
sd_traffic_collision <- read.csv("data/traffic/collisions_data_san_diego.csv")
# create column for year in traffic data by splitting date_count attribute
y <- str_split(as.character(sd_traffic_collision$date_time),'/', simplify = TRUE) 
y <- substr(y[,3], 1, 2)
sd_traffic_collision$year <- y
date_normal_format <- str_split(as.character(sd_traffic_collision$date_time),' ', simplify = TRUE) 
sd_traffic_collision$date_time <- date_normal_format[,1]
sd_traffic_collision$num <- 1

# sum traffic count by year
tc_aggregated <- aggregate(sd_traffic_count$total_count, by = list(Category = sd_traffic_count$date_count), FUN = sum)
y_agg <- str_split(as.character(tc_aggregated$Category),'/', simplify = TRUE) 
y_agg <- substr(y_agg[,3], 1, 2)
tc_aggregated$year <- y_agg
y_normal_format_agg <- str_split(as.character(tc_aggregated$Category),' ', simplify = TRUE) 
tc_aggregated$Category <- y_normal_format_agg[,1]
names(tc_aggregated) <- c("date", "count", "year")

tc_year_agg <- aggregate(tc_aggregated$count, by = list(Category = tc_aggregated$year), FUN = sum)
names(tc_year_agg) <- c("year", "count")
tc_year_agg <- tc_year_agg[-1,]
tc_aggregated <- tc_aggregated[order(as.Date(tc_aggregated$date, format = "%m/%d/%y")),]
tc_aggregated$date <- as.Date(tc_aggregated$date, format = "%m/%d/%y") 

# sum traffic collisions by year
tcol_aggregated <- aggregate(sd_traffic_collision$num, by = list(Category = sd_traffic_collision$date_time), FUN = sum)
y_agg <- str_split(as.character(tcol_aggregated$Category),'/', simplify = TRUE) 
y_agg <- substr(y_agg[,3], 1, 2)
tcol_aggregated$year <- y_agg
date_normal_format_agg <- str_split(as.character(tcol_aggregated$Category),' ', simplify = TRUE) 
tcol_aggregated$Category <- date_normal_format_agg[,1]
names(tcol_aggregated) <- c("date", "collisions", "year")


#### Plotting functions for traffic count and collisions ####

# function to plot traffic counts (April 2018 - 2020)
# TODO: present graphs better
all_tc_plot = function(tc_aggregated, yr) {
  plot_df = subset(tc_aggregated, year<=yr)
  year18 <- subset(tc_aggregated, year == "18")
  year19 <- subset(tc_aggregated, year == "19")
  year20 <- subset(tc_aggregated, year == "20")
  
  #g1 <- ggplot(tc_year_agg, aes(x = year, y = count)) + geom_bar(stat = "identity") + xlab("April of Year") + theme_bw() + theme(legend.position = "")
  g1 <- ggplot(data = tc_aggregated) + 
    geom_line(data = year18, aes(x = date, y = count, group = 1)) + 
    geom_line(data = year19, aes(x = date, y = count, group = 1)) +
    geom_line(data = year20, aes(x = date, y = count, group = 1)) + 
    scale_x_date(date_labels = "%m/%y")
  
  if(yr == "18") { g1 <- ggplot(data = year18, aes(x = date, y = count)) + geom_line(aes(group = 1)) + 
    ylab("traffic count") + theme_bw() + theme(legend.position = "") + geom_point(size = 1) }
  if(yr == "19") { g1 <- ggplot(data = year19, aes(x = date, y = count)) + geom_line(aes(group = 1)) + 
    ylab("traffic count") + theme_bw() + theme(legend.position = "") + geom_point(size = 1) + ylim(0, 250000) }
  if(yr == "20") { g1 <- ggplot(data = year20, aes(x = date, y = count)) + geom_line(aes(group = 1)) + 
    ylab("traffic count") + theme_bw() + theme(legend.position = "") + geom_point(size = 1) + ylim(0, 200000) }
  #if(yr == "all") { g1 <- ggplot(data = subset(plot_df, year == "20"), aes(x = date, y = count)) + geom_line(aes(group = 1)) + 
   # ylab("traffic count") + theme_bw() + theme(legend.position = "") + geom_point(size = 1) }
  g1
}

tcol_plot = function(tcol_aggregated, yr) {
  plot_df = subset(tcol_aggregated, year<=yr)
  g1 <- ggplot(plot_df, aes(x = date, y = collisions)) + geom_line(aes(group = 1)) + 
    ylab("collision count") + theme_bw() + theme(legend.position = "") + geom_point(size = 1) + ylim(0, 40)
  
  if(yr == "18") { g1 <- ggplot(data = subset(plot_df, year == "18"), aes(x = date, y = collisions)) + geom_line(aes(group = 1)) + 
    ylab("collision count") + theme_bw() + theme(legend.position = "") + geom_point(size = 1) + ylim(0, 40) }
  if(yr == "19") { g1 <- ggplot(data = subset(plot_df, year == "19"), aes(x = date, y = collisions)) + geom_line(aes(group = 1)) + 
    ylab("collision count") + theme_bw() + theme(legend.position = "") + geom_point(size = 1) + ylim(0, 40) }
  if(yr == "20") { g1 <- ggplot(data = subset(plot_df, year == "20"), aes(x = date, y = collisions)) + geom_line(aes(group = 1)) + 
    ylab("collision count") + theme_bw() + theme(legend.position = "") + geom_point(size = 1) + ylim(0, 40) }
  g1
}


# table with Air Quality Index Explanations from https://www.epa.gov/outdoor-air-quality-data/air-data-basic-information
ranges <- c('0-50', '51-100', '101-150', '151-200', '201-300','301-500')
health_concern <- c('Good', 'Moderate', 'Unhealthy for Sensitive Groups', 
'Unhealthy', 'Very Unhealthy', 'Hazardous')
colors <- c('green', 'yellow', 'orange', 'red', 'purple', 'maroon')
infoAQI <- data.frame(ranges,health_concern,colors) 

##############################################

# Define UI for webpage
ui <- navbarPage(theme = shinytheme("sandstone"), collapsible = TRUE, 
                 "IMPACT", id = "nav",
                 
                 header = tagList(
                   useShinydashboard()
                 ),
                 
                 # Home Tab, could include COVID-19 Map
                 tabPanel("Home",
                          div(class = "outer", 
                              tags$head(includeCSS("styles.css")),
                              # TODO: create map
                              leafletOutput("mymap", width = "100%", height = "100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 20, width = 250, fixed = TRUE,
                                            draggable = TRUE, height = "auto",
                                            
                                            h3(textOutput("reactive_case_count"), align = "right"),
                                            h4(textOutput("reactive_death_count"), align = "right"),
                                            span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                            span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                            h6(textOutput("clean_date_reactive"), align = "right"),
                                            h6(textOutput("reactive_country_count"), align = "right"),
                                            tags$i(h6("This data is up to date as of 04/23/2020")),
                                            tags$i(h6("Reported cases are subject to significant variation in testing capacity between countries.")),
                                            plotOutput("epi_curve", height="130px", width="100%"),
                                            plotOutput("cumulative_plot", height="130px", width="100%"),
                                            
                                            sliderInput("plot_date",
                                                        label = h5("Select mapping date"),
                                                        min = as.Date(cv_min_date,"%Y-%m-%d"),
                                                        max = as.Date(current_date,"%Y-%m-%d"),
                                                        value = as.Date(current_date),
                                                        timeFormat = "%d %b", 
                                                       animate=animationOptions(interval = 3000, loop = FALSE))
                              )
                          )
                 ),
                 
                 # Traffic Tab
                 tabPanel("Traffic",
                          
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plot_year", "Date Range: ",
                                          c("January 2020" = "20", "April 2019" = "19", 
                                            "April 2018" = "18", "All" = "all")),
                              "San Diego Data"
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Traffic Volume", plotlyOutput("all_tc_plot"), width = 6),
                                tabPanel("Traffic Collision", plotlyOutput("tcol_plot"), width = 800),
                                tabPanel("Combined")
                              )
                            )
                          )
                 ),
                 
                 # Air Quality Tab
                 tabPanel("Air Quality",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("data_shown", h3("Select comparisons:"),
                                                 c("20 Yeah High" = "20year_high",
                                                   "20 Yeah Low" = "20year_low",
                                                   "5 Year Median" = "5year_med",
                                                   "2020 AQI Value" = "aqi")),
                              textOutput("Notes.")
                            ),
                            
                            mainPanel(
                                      tabsetPanel(
                                        tabPanel('AQI Info', tableOutput('AQIinfo'), width = 800),
                                        tabPanel('Austin', plotlyOutput('AustinAQI'), width = 800),
                                        tabPanel('San Diego', plotlyOutput('SanDiegoAQI'), width = 800)
                                      )
                              )
                          )      
                 ),
                 
                 # Gardening Tab
                 tabPanel("Plants",
                          fluidRow(
                            column(width = 8,
                                   htmlOutput("frame")),
                            column(width = 4,

                                   "Click on a state on the map to learn more about gardening conditions
                                    in that area!", tags$br(), tags$br(),
                                    "You can better the environment by growing your own food, and it is important to
                                    your health to get some sunlight!", tags$br(), tags$br(),
                                    "Select your zone for some planting suggestions.", tags$br(), tags$br(),
                                   pickerInput("zone_select",
                                               choices = c("Zone 1 - 2", "Zone 3 - 4",
                                                           "Zone 5 - 6", "Zone 7 - 8",
                                                           "Zone 9 - 10","Zone 11 - 13"), multiple = FALSE),
                                   textOutput("zone_notes_1_2"), textOutput("zone_notes_3_4"), textOutput("zone_notes_5_6"),
                                   textOutput("zone_notes_7_8"), textOutput("zone_notes_9_10"), textOutput("zone_notes_11_13")
                                  )
                          )
                  ),
                 
                 # Our Goals Tab
                 tabPanel("Goal",
                         h2("Our Goal"),
                         dashboardSidebar(disable = TRUE),
                         dashboardBody(
                           fluidRow(
                             valueBoxOutput("traffic_box", width = 4),
                             valueBoxOutput("aqi_box", width = 4),
                             valueBoxOutput("plant_box", width = 4)
                           ),
                           fluidRow(
                             column(width = 4,
                                    box(
                                      title = "Data Results", width = NULL, status = "warning",
                                      "talk about graphs"
                                    ),
                                    box(
                                      title = "Why does this matter?", width = NULL, solidHeader = TRUE, status = "primary",
                                      "talk about what results mean"
                                    ),
                                    box(
                                      width = NULL, background = "light-blue",
                                      "What should we do next?"
                                    )
                             ),
                             
                             column(width = 4,
                                    box(
                                      status = "warning", width = NULL,
                                      "Describe air quality index"
                                    ),
                                    box(
                                      title = "Data Results", width = NULL, solidHeader = TRUE, status = "primary",
                                      "graph results"
                                    ),
                                    box(
                                      title = "What should we do next?", width = NULL,
                                      "future plans"
                                    )
                             ),
                             
                             column(width = 4,
                                    box(
                                      title = "Do Something!", width = NULL, solidHeader = TRUE,
                                      "talk about how people should make sure to get some sun and interact with their environment"
                                    ),
                                    box(
                                      title = "Reduce, Reuse, Recycle", width = NULL, background = "black",
                                      "talk about how growing your own food reduces waste"
                                    )
                             )
                           )
                         )
                 ),
                 
                 # Data Sources Tab
                 tabPanel("Data",
                          h4("We hope to have encouraged people to think about the world around them. Take a look at the data for yourself! 
                          Each and every one of us has an impact on the environment."), tags$br(),
                          h4("Check out some neat data. A list of our full data sources can be found on our ", 
                          tags$a(href = "https://github.com/luizamfsantos/EarthX/tree/master/data", "github repo.")),  tags$br(),
                          h4("COVID-19 Data"), tags$br(),
                          numericInput("maxrows", "Rows to show", 25),
                          verbatimTextOutput("rawtable"),
                          downloadButton("downloadCsv", "Download as CSV"), tags$br(), tags$br(),
                          "Adapted from timeline data published by ", 
                          tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                                                                             "Johns Hopkins Center for Systems Science and Engineering."), tags$br()
                  )
                 
)

# Server logic
server <- function(input, output, session) {
  # covid tab 
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(input$plot_date),"%d %B %Y")
  })
  
  reactive_db = reactive({
    cv_cases %>% filter(date == input$plot_date)
    # reactive = cv_cases %>% filter(date == "2020-04-07")
  })
  
  reactive_db_last24h = reactive({
    cv_cases %>% filter(date == input$plot_date & new_cases>0)
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    #large_countries = reactive %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
    large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
    large_countries
  })
  
  reactive_db_large_last24h = reactive({
    large_countries = reactive_db_last24h() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$alpha3, ]
  })
  
  reactive_polygons_last24h = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large_last24h()$alpha3, ]
  })
  
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " cases")
  })
  
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$death), big.mark=","), " deaths")
  })
  
  output$reactive_recovered_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$recovered), big.mark=","), " recovered")
  })
  
  output$reactive_active_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$active_cases), big.mark=","), " active cases")
  })
  
  output$reactive_case_count_China <- renderText({
    paste0("Mainland China: ", prettyNum(sum(subset(reactive_db(), country=="Mainland China")$cases), big.mark=",")," (",
           prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Mainland China"))$new, big.mark=",")," new)")
  })
  
  output$reactive_case_count_row <- renderText({
    paste0("Other: ", prettyNum(sum(subset(reactive_db(), country!="Mainland China")$cases), big.mark=",")," (",
           prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Other"))$new, big.mark=",")," new)")
  })
  
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
  })
  
  output$reactive_new_cases_24h <- renderText({
    paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$activeper100k)) %>%
     
      addCircleMarkers(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
                       label = sprintf("<strong>%s (past 24h)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$new_recovered, reactive_db_last24h()$newper100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                       label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered, reactive_db()$per100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/5), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (active)",
                       label = sprintf("<strong>%s (active)</strong><br/>Confirmed cases: %g<br/>Cases per 100,000: %g<br/><i><small>Excludes individuals known to have<br/>recovered (%g) or died (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto"))
    
  })
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot(cv_aggregated, input$plot_date)
  })
  
  output$epi_curve <- renderPlot({
    new_cases_plot(cv_aggregated, input$plot_date)
  })
  
  # update region selections
  observeEvent(input$level_select, {
    if (input$level_select=="Global") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = "Global", selected = "Global")
    }
    
    if (input$level_select=="Continent") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                        selected = c("Africa", "Asia", "Europe", "North America", "South America"))
    }
    
    if (input$level_select=="Country") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(cv_today_100[order(-cv_today_100$cases),]$country), 
                        selected = cv_today_100$country)
    }
  }, ignoreInit = TRUE)
  
  # create dataframe with selected countries
  country_reactive_db = reactive({
    if (input$level_select=="Global") { 
      db = cv_cases_global
      db$region = db$global_level
    }
    if (input$level_select=="Continent") { 
      db = cv_cases_continent 
      db$region = db$continent
    }
    if (input$level_select=="Country") { 
      db = cv_cases
      db$region = db$country
    }
    
    if (input$outcome_select=="Cases") { 
      db$outcome = db$cases
      db$new_outcome = db$new_cases
    }
    
    if (input$outcome_select=="Deaths") { 
      db$outcome = db$deaths 
      db$new_outcome = db$new_deaths 
    }
    
    db %>% filter(region %in% input$region_select)
  })
  
  # country-specific plots
  output$country_plot <- renderPlotly({
    country_cases_plot(country_reactive_db(), start_point=input$start_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative <- renderPlotly({
    country_cases_cumulative(country_reactive_db(), start_point=input$start_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative_log <- renderPlotly({
    country_cases_cumulative_log(country_reactive_db(), start_point=input$start_date)
  })
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                     recovered, new_recovered, active_cases, 
                                     per100k, newper100k, activeper100k)), input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  
  # traffic count plots
  output$all_tc_plot <- renderPlotly({
    all_tc_plot(tc_aggregated, input$plot_year)
  })
  
  # traffic collisions plots
  output$tcol_plot <- renderPlotly({
    tcol_plot(tcol_aggregated, input$plot_year)
  })
  
  # plant gardening zones map
  output$frame <- renderUI({
    test <- tags$iframe(src = 'https://www.gilmour.com/gilmour_map/map.html', 
                        id = 'gilmour-planting-map', width="100%", height=550)
    test
  })
  
  # planting notes
  output$zone_notes_1_2 <- renderText({
    if(input$zone_select=="Zone 1 - 2") { paste0("Note 1 - 2") }
  })
  output$zone_notes_3_4 <- renderText({
    if(input$zone_select=="Zone 3 - 4") { paste0("Note 3 - 4") }
  })
  output$zone_notes_5_6 <- renderText({
    if(input$zone_select=="Zone 5 - 6") { paste0("Note 5 -6") }
  })
  output$zone_notes_7_8 <- renderText({
    if(input$zone_select=="Zone 7 - 8") { paste0("Note 7 - 8") }
  })
  output$zone_notes_9_10 <- renderText({
    if(input$zone_select=="Zone 9 - 10") { paste0("Note 9 - 10") }
  })
  output$zone_notes_11_13 <- renderText({
    if(input$zone_select=="Zone 11 - 13") { paste0("Note 11 - 13") }
  })
  

  #air quality plots
  output$AQIinfo <- renderTable(infoAQI)
  output$AustinAQI <- renderPlotly({
    source("data/air_quality_Austin.R")
    aqiAust
  })
  output$SanDiegoAQI <- renderPlotly({
    source("data/air_quality_San_Diego.R")
    aqiSanD
  })
  
  # data tab
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("COVID_data_", cv_today$date[1], ".csv", sep="")
    },
    content = function(file) {
      write.csv(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                      recovered, new_recovered, active_cases, 
                                      per100k, newper100k, activeper100k)), file)
    }
  )
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                     recovered, new_recovered, active_cases, 
                                     per100k, newper100k, activeper100k)), input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  # goals tab
  output$traffic_box <- renderValueBox({
    valueBox("Traffic", "2019 vs 2020", icon = icon("exclamation-triangle"), color = "yellow")
  })
  output$aqi_box <- renderValueBox({
    valueBox("AQI", "2019 vs 2020", icon = icon("fire"), color = "orange")
  })
  output$plant_box <- renderValueBox({
    valueBox("Plants",  "Gardening", icon = icon("fire"), color = "green")
  })
}

shinyApp(ui, server)