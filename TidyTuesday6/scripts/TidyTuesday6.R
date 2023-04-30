#### General Info ####--------------------------------------------------
#
# This script is my Tidy Tuesday 6.
# This will be using the Tidy Tuesday dataset from Nicola Rennie's LondonMarathon R package (scraped from Wikipedia 1 November 2022 on London Marathon winners).
#
# created: 2023-04-29
# created by: Marisa Mackie
# edited: 2023-04-29

#### Load Libraries ####------------------------------------------------
library(tidyverse)
library(here)

#### Data ####----------------------------------------------------------

# Read in the data
winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')
marathon <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv')

view(winners)
view(marathon)

#### Analysis ####------------------------------------------------------

## Cleaning data----

winners_clean <- winners %>% 
  
  # filters for Wheelchair Women category only & from 2000 to present
  filter(Category == "Wheelchair Women" & Year >= 2000)

view(winners_clean)
  
## Plotting----

# generates plot using clean winners data where x is Year and Y is winning time
WinnersPlot <- ggplot(winners_clean,
                      aes(x = Year,
                          y = Time,
                          fill = Nationality))+
  
  # specifies column/bar plot
  geom_col()+
  
  # manually specifies bar fill colors
  scale_fill_manual(values = c("orange","forestgreen","firebrick3","darkorange3","lightseagreen","royalblue3","mediumorchid4"))+
  
  # specifies plot labels
  labs(x = "Year",
       y = "Winning Time",
       title = "London Marathon Winning Times by Year and Nationality 
for Womens Wheelchair Category",
       caption = "Data source: Tidy Tuesday dataset from Nicola Rennie's LondonMarathon R package 
(scraped from Wikipedia 1 November 2022 on London Marathon winners)")+
  
  # specifies color & size of plot elements
  theme(panel.background = element_rect(fill = "seashell"),
        plot.caption = element_text(color = "gray45", size = 7),
        plot.title = element_text(color = "saddlebrown", size = 14),
        axis.title = element_text(color = "navyblue", size = 10),
        legend.text = element_text(color = "navyblue", size = 10),
        legend.background = element_rect(fill = "seashell"))

WinnersPlot

# savges plot to output folder
ggsave(here("TidyTuesday6","output","WinnersPlot.png"),
       height = 5, width = 8)
