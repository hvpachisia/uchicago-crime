[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/onTza0gm)

# Rides to Safety: Scrutinizing Crime in Hyde Park and Campus Transits
by Harsh Vardhan Pachisia

March 2024

### Project Description

This project aims to analyze and understand the safety concerns and effectiveness of transportation options provided by the University of Chicago in the Hyde Park neighborhood. The neighborhood has a reputation of being unsafe due to prevalent criminal activities. The University of Chicago has taken extensive measures to enhance student safety, including establishing one of America's largest private security forces and initiating safety-oriented transportation services such as the UGO shuttle service and the UChicago Free Lyft Program. Despite these initiatives, the area has seen a worrying rise in crime, including several student fatalities and numerous robberies. This project seeks to investigate the changes in crime rates over time in Hyde Park and critically evaluate the effectiveness of the university's safety transportation services in reducing outdoor crime.

The investigation in this project is twofold. Firstly, it examines the historical trend of crime rates in Hyde Park, providing insights into how criminal activities have evolved over time. This includes a detailed analysis of different types of crimes, identifying temporal trends, daily peaks, and specific locations prone to criminal incidents. Secondly, the project evaluates the university's safety transit options, questioning their effectiveness in enhancing student safety. The analysis is based on the hypothesis that these transit options may not be significantly improving safety. To facilitate a comprehensive and interactive exploration of the data, the project includes an RShiny article and a standalone Shiny application. These outputs allow users to engage with the data, explore geographical hotspots of crime, and possibly unearth further insights into the dynamics of crime and safety in Hyde Park.

Final Article: https://harshpachisia.shinyapps.io/uchicago-crime-notebook/

Shiny Application: https://harshpachisia.shinyapps.io/uchicago-crimes-story/

### Instructions for running the project

#### Packages and Data needed

Install the packages below using the `install.packages()` command and then run the following code below. 
```{r}
library(viridis) 
library(tidyverse)
library(ggrepel)
library(shinythemes)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(sf)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(leaflet.extras)
library(rsconnect)
```

All data sets needed to replicate the analysis are provided in the `data` folder in the repository. For updated data for future analysis of crime, please check out the [Chicago Data Portal for crime.](https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2/about_data)
