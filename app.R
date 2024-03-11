############################################################################################################################
# Shiny App for UChicago Crime data exploration and analysis
# Updated March 4, 2024
############################################################################################################################

#importing packages
library(viridis) 
library(tidyverse)
library(ggrepel)
library(shinythemes)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(sf)


options(stringsAsFactors=FALSE)

#loading relevant data frames
crime_counts <- readRDS("data/crime_counts.rds")
crime_lat_long <- readRDS("data/crime_lat_long.rds")
crimes_count_hourly <- readRDS("data/crimes_count_hourly.rds")
university_poi <- readRDS("data/university_poi.rds")
residence_poi <- readRDS("data/residence_poi.rds")
crimes_location <- readRDS("data/crimes_location.rds")
bus_routes <- readRDS("data/bus_routes.rds")
uchicago_shuttles <- readRDS("data/uchicago_shuttles.rds")

#ui for the app
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                h1("Rides to Safety: Scrutinizing Crime in Hyde Park and Campus Transits"),
                div(class = "intro-text",
                    p("This interactive application sheds light on the safety dynamics around the University of Chicago and the Hyde Park area. While the university is globally recognized for its academic excellence, it is situated in an urban setting that presents diverse safety challenges. This interactive enables users - whether students, faculty, local residents, or visitors - to delve deep into the patterns and trends of various crimes, offering a detailed and evolving picture of safety in the area.")
                ),
                h4("(data from ", a("City of Chicago", href="https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2/about_data", target="_blank"), ")"),
                h3("Explore Crime in Hyde Park"),

                navlistPanel(
                  "Analysis options",
                  tabPanel("Evolution of crime types",
                           fluidRow(
                             column(12,
                                    selectInput("crime_type", "Select Crime Types:",
                                                choices = unique(crime_counts$primary_type),
                                                selected = c("assault", "battery", "criminal damage"),
                                                multiple = TRUE))
                           ),
                           fluidRow(
                             column(12,
                                    h3('What are temporal patterns in selected crimes?'),
                                    plotOutput("density")
                             )
                           ),
                           fluidRow(
                             column(12,
                                    h3('When do selected crimes take place?'),
                                    plotOutput("hourly_crimes")
                             )
                           ),
                           
                           fluidRow(
                             column(12,
                                    h3('Where do selected crimes take place?'),
                                    plotOutput("location_bar_chart")  # New plot output for the bar chart
                             )
                           )
                           
                           ),
                  
                  tabPanel("Geographical Hotspots of Crime", 
                           sliderInput("year", "Year", 
                                       min = 2014, max = 2023,
                                       value = 2023, step = 1,
                                       sep = ""),
                           checkboxInput("showCampus", "Major Campus Buildings", value = TRUE),
                           checkboxInput("showResidence", "Major Residential Buildings", value = TRUE),
                           checkboxInput("showBusRoutes", "CTA Buses (171 and 172)", value = FALSE),
                           checkboxInput("showShuttleRoutes", "UChicago Shuttles", value = FALSE),
                           leafletOutput("hydeParkMap", width = "100%", height = "500px"))
                  
                  )
)
# server function
server <- function(input, output) {
  
  output$density <- renderPlot({
    # Convert user input to lowercase and split by comma
    specificCrimes <- tolower(input$crime_type)
    
    # Filter the data set based on user input
    d <- filter(crime_counts, primary_type %in% specificCrimes) %>% 
      mutate(label = if_else(year == max(year), as.character(primary_type), NA_character_))
    
    # Check if the dataset is not empty
    if (nrow(d) > 0) {
      ggplot(d, aes(x=year, y=count, color=primary_type)) +
        geom_line(linewidth = 1) +
        labs(title = "Crimes over years",x = "Year", y = "Number of Crimes", color = "Crime Type") +
        scale_color_viridis_d(option = "magma", end = 0.9) +
        theme_bw(15)
    } else {
      ggplot() + 
        annotate("text", x = 1, y = 1, label = "No data available for selected crime types", size = 6, hjust = 0.5, vjust = 0.5)
    }
  })
  
  output$hourly_crimes <- renderPlot({
    specificCrimes <- tolower(input$crime_type)
    
    hourly_data <- crimes_count_hourly %>%
      filter(primary_type %in% specificCrimes) %>%
      group_by(time_of_day, primary_type) %>%
      summarize(count = sum(count), .groups = 'drop') %>% 
      mutate(time_of_day = format(strptime(as.character(time_of_day), format = "%H"), format = "%I %p"),
             time_label = format(time_of_day, format = "%I %p"))

    # Convert time_of_day to a factor with levels in chronological order
    time_levels <- format(strptime(0:23, format = "%H"), format = "%I %p")
    hourly_data$time_of_day <- factor(hourly_data$time_of_day, levels = time_levels, ordered = TRUE)
    
    # Custom breaks every 5 hours
    breaks_hourly <- time_levels[c(TRUE, rep(FALSE, 4))]
    
    if (nrow(hourly_data) > 0) {
      ggplot(hourly_data, aes(x = time_of_day, y = primary_type, fill = count)) +
        geom_tile() +
        scale_fill_gradient(low = "floralwhite", high = "steelblue") +
        labs(title = "Crimes by hour of day", x = "Hour of Day", y = "Crime Type", fill = "Number of Crimes") +
        scale_x_discrete(breaks = breaks_hourly) + 
        theme_bw(15)
    } else {
      ggplot() + 
        annotate("text", x = 1, y = 1, label = "No data available for selected crime types", size = 6, hjust = 0.5, vjust = 0.5)
    }
  })
  
  output$location_bar_chart <- renderPlot({
    specificCrimes <- tolower(input$crime_type)
    
    # Filter and process the data for top 5 locations for the selected crime types
    top_locations_data <- crimes_location %>%
      filter(primary_type %in% specificCrimes) %>%
      group_by(location) %>%
      summarize(count = sum(count)) %>%
      arrange(desc(count)) %>%
      slice_max(order_by = count, n = 5)
    
    # Define indoor and outdoor locations
    outdoor_locations <- c("street", "alley", "sidewalk", "parking lot / garage (non residential)")
    
    # Add a new column indicating indoor or outdoor
    top_locations_data <- top_locations_data %>%
      mutate(location_type = if_else(location %in% outdoor_locations, "Outdoor", "Indoor"))
    
    # Create the bar chart
    if (nrow(top_locations_data) > 0) {
      ggplot(top_locations_data, aes(x = reorder(location, count), y = count, fill = location_type)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("Indoor" = "maroon4", "Outdoor" = "steelblue"))+
        coord_flip() +
        labs(title = "Locations of Crime Types (Indoor/Outdoor)",
             x = "Location of Crime",
             y = "Number of Crimes",
             fill = "Location") +
        theme_bw(15)+
        theme(legend.position = "right")+
        guides(fill = guide_legend(title = "Location Type"))
    } else {
      ggplot() + 
        annotate("text", x = 1, y = 1, label = "No data available for selected crime types", size = 6, hjust = 0.5, vjust = 0.5)
    }
   
  })
  
  # have to generate heatmaps here since cant do spatial stuff outside app.R, doesn't deploy
  years <- unique(crime_lat_long$year)
  crime_lat_long <- crime_lat_long %>% 
    # count of crime greater than or equal to 3
    filter(count >=3)
  

  # Pre-generate heatmaps for each year
  pre_generated_maps <- lapply(years, function(y) {
    data_year <- crime_lat_long[crime_lat_long$year == y, ]
    m <- leaflet(data_year) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addHeatmap(lng = ~longitude, lat = ~latitude, intensity = ~count, radius = 15, blur = 20, max = 0.05
                 )
    saveRDS(m, paste0("data/heatmap_", y, ".rds"))
  })

  output$hydeParkMap <- renderLeaflet({
    year_selected <- input$year
    heatmap_file <- paste0("data/heatmap_", year_selected, ".rds")
    
    map <- if (file.exists(heatmap_file)) {
      readRDS(heatmap_file)
    } else {
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -87.5903, lat = 41.7943, zoom = 14)
    }
    
    # Conditionally add the POI layer based on user input
    if (input$showCampus) {
      map <- map %>%
        addAwesomeMarkers(
          lng = university_poi$longitude, 
          lat = university_poi$latitude, 
          popup = university_poi$name,
          icon = awesomeIcons(
            icon = 'university', 
            iconColor = 'white', 
            library = 'fa', 
            markerColor = 'blue'
          )
        )
        
    }
    # Conditionally add the POI layer based on user input
    if (input$showResidence) {
      map <- map %>%
        addAwesomeMarkers(
          lng = residence_poi$longitude, 
          lat = residence_poi$latitude, 
          popup = residence_poi$name,
          icon = awesomeIcons(
            icon = 'home', 
            iconColor = 'white', 
            library = 'fa', 
            markerColor = 'blue'
          )
        )
      
    }
    # Add bus routes layer conditionally
    if (input$showBusRoutes && !is.null(bus_routes)) {
      for (i in 1:nrow(bus_routes)) {
        map <- map %>%
          addPolylines(data = bus_routes[i, ],
                       color = ifelse(bus_routes$route[i] == 171, 'darkred', 'darkred'),
                       weight = 2,
                       opacity = 1,
                       label = paste("Route", bus_routes$route[i]))
      }
    }
    # Add UChicago Shuttles Layer
    if (input$showShuttleRoutes && !is.null(uchicago_shuttles)) {
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
      for (i in 1:nrow(uchicago_shuttles)) {
        shuttle_name <- as.character(uchicago_shuttles$Name[i])
        
        # Assign color based on shuttle name
        shuttle_color <- ifelse(shuttle_name == "North", "darkblue",
                                ifelse(shuttle_name == "East", "darkblue",
                                       ifelse(shuttle_name == "South", "darkblue", "darkblue")))
        
        map <- map %>%
          addPolylines(data = uchicago_shuttles[i, ],
                       color = shuttle_color,
                       weight = 2,
                       opacity = 1,
                       label = shuttle_name)
      }
    }
    
   
    if (input$showBusRoutes) {
      map <- map %>%
        addLegend("bottomright",
                  colors = "darkred",
                  labels = 'Bus Routes (Hover for name)',
                  title = "CTA Routes")
    }
    
    if (input$showShuttleRoutes) {
      map <- map %>%
        addLegend("bottomright",
                  colors = "darkblue",
                  labels = 'Shuttle Routes (Hover for name)',
                  title = "Shuttle Routes")
    }
    
    color_palette <- colorNumeric(
      palette = c('#0000FF', '#00FFFF', '#00FF00', '#FFFF00', '#FFA500', '#FF0000'),
      domain = crime_lat_long$count
    )
    leafletProxy("hydeParkMap", data = crime_lat_long) %>% 
      addLegend(position = "bottomright",
                title = "Number of Crimes",
                pal = color_palette,
                values = crime_lat_long$count,
                opacity = 1)
    map
  })
  
}

# Run the application 
shinyApp(ui=ui, server=server)