############################################################################################################################
# Code to generate places of interest in University of Chicago data frames
# Updated March 4, 2024
############################################################################################################################

library('leaflet')
library('leaflet.extras')
library('ggplot2')
library('tidyverse')
library('lubridate')

# getting crime data over the last 10 years and the UChicago area(community areas: Hyde Park, Kenwood, Woodlawn)
uchicago_poi<- read.csv('data/hp_poi.csv', header = TRUE)
uchicago_poi <- uchicago_poi %>%
  separate(Latitude, into = c("latitude", "longitude"), sep = ",") %>% 
  rename(id = ID,
         name = Name) %>% 
  select(-c('Longitude'))

uchicago_poi$latitude <- as.numeric(uchicago_poi$latitude)
uchicago_poi$longitude <- as.numeric(uchicago_poi$longitude)

university_poi<- uchicago_poi %>% 
  subset(type == 'University') %>% 
  select(-c('type'))

residence_poi<- uchicago_poi %>% 
  subset(type == 'Residence') %>% 
  select(-c('type'))


#testing
leaflet_map <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    lng = uchicago_poi$longitude, 
    lat = uchicago_poi$latitude, 
    popup = uchicago_poi$name,
    radius = 5, 
    color = "steelblue",
    fillOpacity = 0.8
  )

leaflet_map

#saving dataframes
saveRDS(university_poi, "data/university_poi.rds")
saveRDS(residence_poi, "data/residence_poi.rds")
saveRDS(uchicago_poi, "data/uchicago_poi.rds")
saveRDS(leaflet_map, "data/uchicago_poi_map.rds")
