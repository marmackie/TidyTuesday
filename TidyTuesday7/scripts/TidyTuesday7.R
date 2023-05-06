#### General Info ####--------------------------------------------------
#
# This script is my Tidy Tuesday 7.
# This will be using the Tidy Tuesday dataset from the Portal Project, 
# a long-term ecological research site studying the dynamics of desert rodents, plants, ants and weather in Arizona.
# From the Weecology research group.
#
# created: 2023-05-05
# created by: Marisa Mackie
# edited: 2023-05-05

#### Load Libraries ####------------------------------------------------
library(tidyverse)
library(here)

#### Data ####----------------------------------------------------------

# Read in the data
plots <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/plots.csv')
species <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/species.csv')
surveys <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/surveys.csv')

view(plots)
view(species)
view(surveys)
  
#### Analysis ####------------------------------------------------------

### Cleaning----
surveys_clean <- surveys %>% 
  # selects for specified columns
  select(year, treatment, species, hfl, wgt) %>% 
  # filters for only data collected in 2022
  filter(year == 2022)

view(surveys_clean)

### Plotting----
SurveysPlot <- ggplot(surveys_clean,
                      aes(x = hfl,
                          y = wgt,
                          color = species))+
  # specifies scatterplot
  geom_point()+
  
  # facets by species & treatment
  facet_wrap(~treatment)+
  
  # color-blind friendly colors
  scale_color_viridis_d()+
  
  # specifies plot labels
  labs(x = "Hindfoot Length",
       y = "Weight",
       color = "Desert Rodent Species",
       title = "Relationship between Hindfoot Length and Weight in Males and Females 
across Different Arizona Desert Rodent Species",
       caption = "Data source: Tidy Tuesday dataset from the Portal Project, a long-term ecological research site 
studying the dynamics of desert rodents, plants, ants and weather in Arizona. From the Weecology research group.")+
  
  # specifies color & size of plot elements
  theme(panel.background = element_rect(fill = "linen"),
        strip.background = element_rect(fill = "tan"),
        axis.title = element_text(color = "saddlebrown", size = 15),
        plot.title = element_text(color = "saddlebrown", size = 15, face = "bold"),
        plot.caption = element_text(color = "gray40", size = 7))

SurveysPlot

ggsave(here("TidyTuesday7","output","SurveysPlot.png"),
       height = 7, width = 9)