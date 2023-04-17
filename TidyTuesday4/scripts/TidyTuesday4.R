#### General Info ####--------------------------------------------------
#
# This script is my Tidy Tuesday 4.
# This will be using the The Humane League's US Egg Production dataset by Samara Mendez.
#
# created: 2023-04-16
# created by: Marisa Mackie
# edited: 2023-04-17

#### Load Libraries ####------------------------------------------------
library(tidyverse)
library(here)
library(magick)

#### Data ####----------------------------------------------------------

# Read in egg production data
eggs  <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')
view(eggs)

#### Analysis ####------------------------------------------------------

### Cleaning ###-----------

# Cleans eggs data
eggs_clean <- eggs %>% 

  # filters for only data specifically noted as "cage-free" production, and only table eggs
  filter(prod_process != "all" & prod_type == "table eggs") %>% 
  
  # removes NA values
  drop_na() %>% 
  
  # selects only for the specified columns
  select(observed_month, prod_type, prod_process, n_eggs)
  
view(eggs_clean)


### Plotting ###-----------
# Generates plot, specifies variables
eggplot <- ggplot(eggs_clean, aes(x = observed_month,
                               y = n_eggs,
                               color = prod_process))+
  geom_line()+ # specifies line plot
  
  # specifies plot labels
  labs(x = "Year",
       y = "Number of eggs produced",
       color = "Production process",
       title = "Number of eggs produced over time for cage-free hens",
       caption = "Data source: Tidy Tuesday 2023 - The Humane League's US Egg Production dataset by Samara Mendez.")+

  #manually specifies colors
  scale_color_manual(values = c("darkorange3","red4"))+

  # specifies color and size of plot elements
  theme(panel.background = element_rect(fill = "papayawhip"),
        plot.title = element_text(color = "violetred4", face = "bold", size = 20),
        axis.title = element_text(color = "violetred4", size = 20),
        plot.caption = element_text(color = "gray45", size = 10))
        
eggplot

# saves plot & specifies dimensions
ggsave(here("TidyTuesday4", "output", "eggplot.png"),
       width = 10, height = 8)

# Get chick image
chick <- image_read("https://www.ecofarmingdaily.com/wp-content/uploads/shutterstock_1350352415.jpg") %>% 
  # resizes image
  image_scale("350")

# Read in image & plot
eggplot <- image_read(here("TidyTuesday4","output","eggplot.png"))
eggplot_img <- image_composite(eggplot, chick, offset = "-2500+30")
eggplot_img

# saves as image in output folder
image_write(eggplot_img, here("TidyTuesday4","output","eggplot_img.png"), format = "png")

print(eggplot_img)