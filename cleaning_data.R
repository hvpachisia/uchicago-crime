############################################################################################################################
# Code to generate clean data for Shiny Application including clean UChicago crime data, crime counts, crime locations
# Updated March 4, 2024
############################################################################################################################

#packages needed
library('ggplot2')
library('tidyverse')
library('lubridate')

# getting crime data in the UChicago area(community areas: Hyde Park)
uchicago_crime<- read.csv('data/uchicago_crime.csv', header = TRUE)

# getting a date column and subsetting data to only include crimes till end-2023 (removing crimes in Jan 2024)
uchicago_crime$date_of_crime <- as.Date(uchicago_crime$Date, format = "%m/%d/%Y") 
uchicago_crime<- uchicago_crime %>% 
  subset(date_of_crime <= "2023-12-31" & date_of_crime >= '2014-01-01')

# adding columns for time of day, month, and year
uchicago_crime$time_of_day <- hour(strptime(uchicago_crime$Date, "%m/%d/%Y %I:%M:%S %p"))
uchicago_crime$month <- months(strptime(uchicago_crime$Date, "%m/%d/%Y %I:%M:%S %p"))
uchicago_crime$year <- year(strptime(uchicago_crime$Date, "%m/%d/%Y %I:%M:%S %p"))

# Store bounding box coordinates
hp_bb <- c(
  left = -87.608221,
  bottom = 41.783249, #41.783249
  right = -87.577643,
  top = 41.803038
)

uchicago_crime_filtered <- uchicago_crime %>%
  filter(Longitude >= hp_bb["left"], Longitude <= hp_bb["right"],
         Latitude >= hp_bb["bottom"], Latitude <= hp_bb["top"])

# filtering to only Hyde Park as the UChicago area
uchicago_crime_filtered <- uchicago_crime_filtered %>% 
  rename(
    id = ID,
    date = Date,
    primary_type = Primary.Type,
    time_of_day = time_of_day,
    location = Location.Description,
    latitude = Latitude,
    longitude = Longitude
  ) %>% 
  filter(!is.na(time_of_day) & !is.na(primary_type)) %>%
  select(time_of_day, primary_type, location, id, date, month, year, latitude, longitude)

# to get count by year and primary type
crimes_count <- uchicago_crime_filtered %>%
  mutate(primary_type = tolower(primary_type)) %>% 
  group_by(year, primary_type) %>%
  summarise(count = n())

crimes_count_hourly <- uchicago_crime_filtered %>%
  mutate(primary_type = tolower(primary_type)) %>% 
  group_by(year, primary_type, time_of_day) %>%
  summarise(count = n())

crimes_location <- uchicago_crime_filtered %>%
  mutate(primary_type = tolower(primary_type),
         location = tolower(location),
         location = ifelse(location %in% c("apartment", "residence"), "apartment/residence", location)) %>% 
  group_by(primary_type, location) %>%
  summarise(count = n())

# to get year, lat, long, primary_type, count
# filtering to only Hyde Park as the UChicago area
crime_lat_long <- uchicago_crime_filtered %>%
  group_by(latitude, longitude, year, primary_type) %>%
  summarise(count = n(), .groups = "drop")

crime_lat_long$latitude <- as.numeric(crime_lat_long$latitude)
crime_lat_long$longitude <- as.numeric(crime_lat_long$longitude)
crime_lat_long$count <- as.numeric(crime_lat_long$count)

#saving data
saveRDS(crimes_count, "data/crime_counts.rds")
saveRDS(crime_lat_long, "data/crime_lat_long.rds")
saveRDS(crimes_count_hourly, "data/crimes_count_hourly.rds")
saveRDS(crimes_location, "data/crimes_location.rds")