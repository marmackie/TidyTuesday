#### General Info ####--------------------------------------------------
#
# This script is my Tidy Tuesday 5.
# This will be using the TidyTuesday dataset: The "Neolithic Founder Crops"" in Southwest Asia: Research Compendium
#
# created: 2023-04-23
# created by: Marisa Mackie
# edited: 2023-04-23

#### Load Libraries ####------------------------------------------------
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)

#### Data ####----------------------------------------------------------

# Read in Founder crop data
crops <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-18/founder_crops.csv")
view(crops)
  
#### Analysis ####------------------------------------------------------

### Cleaning -------
crops_clean <- crops %>% 
  
  # selects only for the specified columns
  select(source_id, site_name, age_start, age_end, latitude, longitude, n, founder_crop, edibility) %>% 
  
  # filters only for wheat, with edible seeds/fruit, and with a sample size greater than or equal to 20
  filter(founder_crop == "wheat" & edibility == "Edible seed/fruit" & n >= 20)

view(crops_clean)

### Mapping --------

# Gets map of countries where sites are (mainly middle east)
sites_map <-  map_data("world", regions = c("Syria","Turkey","Iran","Palestine","Cyprus","Lebanon","Jordan","Israel","Iraq"))

### Plotting -------

# Generates plot
CropsPlot <- ggplot()+
  
  # geom polygon for plotting map
  geom_polygon(data = sites_map, 
               aes(x = long, 
                   y = lat, 
                   group = group),
               color = "black", # outlines each country with black lines
               fill = "linen")+ # specifies fill color of countries
  guides(fill = "none")+ # removes legend
  
  # specifies scatterplot
  geom_point(data = crops_clean, # using crops_clean data
             aes(x = longitude,
                 y = latitude,
                 color = site_name, # color of points represents site
                 size = n), # size of points corresponds to sample size
             alpha = 0.5)+ # makes all points slightly transparent
  guides(color = "none")+ # removes legend for color
  
  # specifies colors of plot elements
  theme(panel.background = element_rect(fill = "lightcyan"),
        plot.caption = element_text(size = 7, color = "gray45"))+
  
  # specifies plot labels
  labs(x = "Longitude",
       y = "Latitude",
       size = "Sample size",
       title = "Neolithic Founder Sites for Wheat",
       caption = "Data source: Tidy Tuesday dataset 'The Neolithic Founder Crops' 
in Southwest Asia: Research Compendium")

CropsPlot

# saves plot to output folder
ggsave(here("TidyTuesday5","output","CropsPlot.png"))