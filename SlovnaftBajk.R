library(leaflet)
library(tidyverse)

# https://rstudio.github.io/leaflet/
# https://rstudio.github.io/leaflet/map_widget.html

data <- read.delim('data.csv',sep= ';')
## neplatne: data <- read.delim("https://opendata.bratislava.sk/dataset/download/411",sep= ';')  # priamo z webu
data <- read.delim("https://opendata.bratislava.sk/dataset/download/slovnaft-bajk-lokalizacia-vypozicnych-stanic-zdielanych-bicyklov/1",sep= ';')  # priamo z webu

head(data)

colnames(data) <- c("ID","nazov", "GPSlat","GPSlon")


mapa <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng = data$GPSlon, lat = data$GPSlat, label = data$nazov, popup = data$nazov)
  #addCircles(lat = data$GPSlat, lng = data$GPSlon)
  #addCircleMarkers(radius = 7, fill = TRUE, lat = data$GPSlat, lng = data$GPSlon)

print(mapa)

# Ciernobiela theme
leaflet() %>% 
  addProviderTiles(providers$Stamen.Toner) %>%
  addCircleMarkers(radius = 7, fill = TRUE, lat = data$GPSlat, lng = data$GPSlon)


