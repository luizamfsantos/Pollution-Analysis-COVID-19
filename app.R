# EarthXHack 2020
# Leidy Buescher, Luiza Santos 


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

# import data


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
                              pickerInput("date_selected", "Date Range: ",
                                          choices = c("April 2020", "March 2020", "Both"),
                                          multiple = FALSE),
                              "Select date."
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("2020 Data"),
                                tabPanel("2019 Data"),
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
                          "hardiness zones. planting suggestions. go outside, better the environment, etc."),
                 
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
  
}

shinyApp(ui, server)