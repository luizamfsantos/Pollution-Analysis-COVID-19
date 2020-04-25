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

# Import Data

# San Diego Traffic Data
sd_traffic_count <- read.csv("/Users/leidyward/Desktop/CS/R/EarthX/data/traffic/san_diego_traffic_counts.csv")
# create column for year in traffic data by splitting date_count attribute
y <- str_split(as.character(sd_traffic_count$date_count),'/', simplify = TRUE) 
y <- substr(y[,3], 1, 2)
sd_traffic_count$year <- y
y_normal_format <- str_split(as.character(sd_traffic_count$date_count),' ', simplify = TRUE) 
sd_traffic_count$date_count <- y_normal_format[,1]

# sum traffic count by year
tc_aggregated <- aggregate(sd_traffic_count$total_count, by = list(Category = sd_traffic_count$date_count), FUN = sum)
y_agg <- str_split(as.character(tc_aggregated$Category),'/', simplify = TRUE) 
y_agg <- substr(y_agg[,3], 1, 2)
tc_aggregated$year <- y_agg
y_normal_format_agg <- str_split(as.character(tc_aggregated$Category),' ', simplify = TRUE) 
tc_aggregated$Category <- y_normal_format_agg[,1]
names(tc_aggregated) <- c("date", "count", "year")


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

#### Plotting functions for traffic count ####
# function to plot traffic count by year (April of 2018, 2019, 2020)


# function to plot all traffic counts (April 2018 - 2020)
# TODO: present graph better, add graph for traffic incidents?
all_tc_plot = function(tc_aggregated, yr) {
  plot_df = subset(tc_aggregated, year<=yr)
  g1 <- ggplot(plot_df, aes(x = date, y = count)) + geom_line() + 
    ylab("traffic count") + theme_bw() + theme(legend.position = "") + geom_point()
  
  if(yr == "18") { g1 <- ggplot(data = subset(plot_df, year == "18"), aes(x = date, y = count)) + geom_line() + 
    ylab("traffic count") + theme_bw() + theme(legend.position = "") + geom_point() }
  if(yr == "19") { g1 <- ggplot(data = subset(plot_df, year == "19"), aes(x = date, y = count)) + geom_line() + 
    ylab("traffic count") + theme_bw() + theme(legend.position = "") + geom_point()}
  if(yr == "20") { g1 <- ggplot(data = subset(plot_df, year == "20"), aes(x = date, y = count)) + geom_line() + 
    ylab("traffic count") + theme_bw() + theme(legend.position = "") + geom_point()}
  g1
}

##############################################

# Define UI for webpage
ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE, 
                 "IMPACT", id = "nav",
                 
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
                                            tags$i(h6("For more regular updates, refer to: ", tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard."))),
                                            tags$i(h6("Reported cases are subject to significant variation in testing capacity between countries.")),
                                            plotOutput("epi_curve", height="130px", width="100%"),
                                            plotOutput("cumulative_plot", height="130px", width="100%"),
                                            
                                            #sliderInput("plot_date",
                                            #            label = h5("Select mapping date"),
                                            #            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            #            max = as.Date(current_date,"%Y-%m-%d"),
                                            #            value = as.Date(current_date),
                                            #            timeFormat = "%d %b", 
                                            #           animate=animationOptions(interval = 3000, loop = FALSE))
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
                                          #selected = c("January 2020"),
                              "Select date."
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Traffic Volume", plotlyOutput("all_tc_plot"), width = 6),
                                tabPanel("Traffic Incident"),
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
                            
                            mainPanel(width = 6)
                          )      
                 ),
                 
                 # Gardening Tab
                 tabPanel("Plants",
                          "hardiness zones. planting suggestions. go outside, better the environment, etc."
                          ),
                 
                 # Our Goals Tab
                 tabPanel("Goal",
                          tags$div(
                            tags$h4("Our Goal"),
                            tags$h6("goals")
                          )
                 ),
                 
                 # Data Sources Tab
                 tabPanel("Data",
                          "our data")
                 
)

# Server logic
server <- function(input, output, session) {
  
  # traffic plots
  output$all_tc_plot <- renderPlotly({
    all_tc_plot(tc_aggregated, input$plot_year)
  })
}

shinyApp(ui, server)