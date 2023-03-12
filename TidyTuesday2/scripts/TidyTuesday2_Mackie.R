###### General Info ######-----------------------------------------------------
# This is my 2nd Tidy Tuesday script
# In this script, I will be using the Numbats in Australia TidyTuesday Data
# (Data is from "The Atlas of Living Australia")
# With this data, I will make a scatterplot on top of a map.
#
# created: 2023-03-11
# created by: Marisa Mackie
# edited: 2023-03-11

###### Load Libraries ######---------------------------------------------------
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)

###### Data ######-------------------------------------------------------------
# Load in numbats data
numbats <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')

view(numbats)

###### Analysis ######---------------------------------------------------------

# selects only latitude, longitude, and year columns from data, excluding NA values
numbats <- numbats %>% 
  select(decimalLatitude, decimalLongitude, year) %>% 
  drop_na()

# gets map of world data, specifically Australia
Aus <- map_data("world", region = "Australia")

# Creates plot
numbat_plot <- ggplot()+
  # Creates shape for Australia
  geom_polygon(data = Aus, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               # outlines map with black line & fills with tan color
               color = "black",
               fill = "tan")+
  
  # Plots numbat data onto map
  geom_point(data = numbats,
             aes(x = decimalLongitude,
                 y = decimalLatitude),
             # sets color of numbat data points to red
             color = "red4")+
  
  # Facets plots by year that data was collected
  facet_wrap(~year, ncol = 5)+
  
  # labels axes, title, etc.
  labs(x = "Longitude",
       y = "Latitude",
       title = "Where are Numbats found in Australia? From 1856 - 2023",
       caption = "Plot showing location of numbats (red) in Australia during various years, 
starting from 1856 until 2023. Data from The Atlas of Living Australia https://www.ala.org.au/")+
  
  # Specifies color & size of plot elements
  theme(strip.background = element_rect(fill = "linen"),
        panel.background = element_rect(fill = "skyblue"),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 20),
        plot.caption = element_text(size = 8))

numbat_plot

# saves plot in output folder & specifies dimensions
ggsave(here("TidyTuesday2","output", "NumbatPlot.png"), width = 9, height = 9)

