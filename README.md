# Impact

EarthXHack 2020

Our goal in this project is to be able to show some trends of impact the shelther-in-place order has had on various areas across the US. We want to increase awareness of how we all contribute to the environment around us. (An easy way to get involved in helping your environment is growing your own food!)

To run this app, load shiny library in RStudio and run this command: runGitHub("EarthX","luizamfsantos").


Air Quality Data Source

  - https://www.epa.gov/outdoor-air-quality-data/air-data-daily-air-quality-tracker
  - https://www.nasa.gov/feature/goddard/2020/drop-in-air-pollution-over-northeast

Traffic Data Sources

  - https://data.sandiego.gov/datasets/traffic-volumes/
  - https://data.austintexas.gov/Transportation-and-Mobility/Radar-Traffic-Counts/i626-g7ub
  - https://data.austintexas.gov/Transportation-and-Mobility/Real-Time-Traffic-Incident-Reports/dx9v-zd7x
  
Employment Data Sources  

  - https://www.bls.gov/
  [BLS.gov cannot vouch for the data or analyses derived from these data after the data have been retrieved from BLS.gov.]
  
Plant Data Sources

  - https://www.ecoscraps.com/blogs/gardening-farming/87136132-planting-by-zone-a-complete-guide
  - https://www.ufseeds.com/learning/planting-schedules/
  - https://www.epa.gov/facts-and-figures-about-materials-waste-and-recycling/containers-and-packaging-product-specific-data
  
COVID-19 Map Data Source

  - https://github.com/eparker12/nCoV_tracker/tree/master/input_data
  
## Built With
- R
- Jupyter Notebooks
- Python 3.7
- Shiny

R packages:
- shiny
- shinyWidgets
- shinydashboard
- shinythemes
- dplyr
- ggplot2
- reshape2
- RColorBrewer
- leaflet
- geojsonio
- plotly
- ggiraph
- maps
- rgdal
- RCurl
- stringr
- markdown
- readr

Python packages:
- requests
- json
- bs4
- re
- pandas
- numpy
- matplotlib
- seaborn
- sqlite3

## Future Plans:
Some future plans are to add more cities to get a better picture. We were able to write a script that can draw the data for air quality and employment data from multiple cities but due to time constraints we were not able to implement it. We planned to make a Machine Learning model to determine which sector caused the most decrease in pollution and intersect with jobs that can be turned remote. This information can be beneficial for companies and governments in an attempt to be take better care of our environment. 

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

## Authors 
Leidy Buescher
Luiza Santos



