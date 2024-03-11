############################################################################################################################
# Code to generate routes data for Shiny Application for CTA buses and UChicago shuttles
# Updated March 4, 2024
############################################################################################################################

### Getting the routes data
library(sf)
library(tidyverse)

### Bus Routes

# Set the path to the shapefile
shapefile_path <- "data/cta_buses/CTA_BusRoutes.shp"

# Read the shapefile
raw_bus_routes <- st_read(shapefile_path)

# Convert ROUTE and ROUTE0 to numeric
raw_bus_routes$ROUTE <- as.numeric(as.character(raw_bus_routes$ROUTE))
raw_bus_routes$ROUTE0 <- as.numeric(as.character(raw_bus_routes$ROUTE0))

# Filter rows for ROUTE 171 and 172
bus_routes <- raw_bus_routes %>%
  filter(ROUTE == 171 | ROUTE == 172) %>% 
  rename(route = ROUTE) %>% 
  select(route, geometry)


bus_routes <- bus_routes %>%
  st_transform(crs = 4326) 

saveRDS(bus_routes, "data/bus_routes.rds")

### UChicago Shuttles

#reading in shuttle data 
north <- st_read("data/uchicago_shuttles/north.kml")
east <- st_read("data/uchicago_shuttles/east.kml")
south <- st_read("data/uchicago_shuttles/south.kml")
central <- st_read("data/uchicago_shuttles/central.kml")

north <- north %>% 
  filter(st_geometry_type(geometry) == "LINESTRING" | st_geometry_type(geometry) == "MULTILINESTRING") %>% 
  select(-c('Description'))

east <- east %>% 
  filter(st_geometry_type(geometry) == "LINESTRING" | st_geometry_type(geometry) == "MULTILINESTRING") %>% 
  select(-c('Description'))
south <- south %>% 
  filter(st_geometry_type(geometry) == "LINESTRING" | st_geometry_type(geometry) == "MULTILINESTRING") %>% 
  select(-c('Description'))
central <- central %>% 
  filter(st_geometry_type(geometry) == "LINESTRING" | st_geometry_type(geometry) == "MULTILINESTRING") %>% 
  select(-c('Description'))

uchicago_shuttles<- rbind(north, south, east, central)

uchicago_shuttles <- st_cast(uchicago_shuttles, "MULTILINESTRING")

uchicago_shuttles <- uchicago_shuttles %>%
  st_transform(crs = 4326) 

# Function to convert XYZ to XY for each geometry
convert_xyz_to_xy <- function(geometry) {
  # Extract coordinates and remove the Z dimension
  coords <- st_coordinates(geometry)
  coords <- coords[, 1:2] # Keep only X and Y
  
  # Create a new LineString or MultiLineString without the Z dimension
  if (st_geometry_type(geometry) == "MULTILINESTRING") {
    st_multilinestring(list(coords))
  } else {
    st_linestring(coords)
  }
}

# Apply the function to each geometry
uchicago_shuttles_xy <- st_sfc(lapply(st_geometry(uchicago_shuttles), convert_xyz_to_xy), crs = st_crs(uchicago_shuttles))

# Create a new sf object with the original data but new geometries
uchicago_shuttles <- st_sf(st_drop_geometry(uchicago_shuttles), geometry = uchicago_shuttles_xy)

saveRDS(uchicago_shuttles, "data/uchicago_shuttles.rds")