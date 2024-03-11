############################################################################################################################
# Code to pre-generate heatmaps for geographical hotspots of crime
# Updated March 4, 2024
############################################################################################################################
library(leaflet)
library(leaflet.extras)

crime_lat_long <- readRDS("data/crime_lat_long.rds")

years <- unique(crime_lat_long$year)
crime_lat_long <- crime_lat_long %>% 
  filter(count >=3)

# Pre-generate heatmaps for each year
pre_generated_maps <- lapply(years, function(y) {
  data_year <- crime_lat_long[crime_lat_long$year == y, ]
  m <- leaflet(data_year) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~count, radius = 15, blur = 20, max = 0.05)
  saveRDS(m, paste0("data/heatmap_", y, ".rds"))
})


# Store bounding box coordinates
hp_bb <- c(
  left = -87.608221,
  bottom = 41.783249,
  right = -87.577643,
  top = 41.803038
)

# Filter the data for the year 2023 and within the bounding box
data_2023 <- subset(crime_lat_long, year == 2023) %>% 
  filter(count >5)


# Create the heatmap
leaflet(data_2023) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~longitude, lat = ~latitude,
    intensity = ~count,
    radius = 20, blur = 15, max = 0.05,
    gradient = c("0.4" = "blue", "0.65" = "black", "1" = "red")
  ) 