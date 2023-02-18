###### General Info ######----------------------------------
# This is my first Tidy Tuesday script
# In this script, I will be using the Pet Cats UK TidyTuesday Data
# (Data is from "Movebank for Animal Tracking Data" via "Data is Plural")
# With this data, I will make a plot.
#
# created: 2023-02-18
# created by: Marisa Mackie
# edited: 2023-02-18

###### Load Libraries ######--------------------------------
library(tidyverse)
library(here)

###### Data ######------------------------------------------
# Read in data manually
cats_uk <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')
cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')

view(cats_uk)
view(cats_uk_reference)

###### Analysis ######--------------------------------------

#------[Data Cleanup]------
# Creates new dataset called cats_data using cats_uk_reference
cats_data <- cats_uk_reference %>% 
  
  # Removes NA values
  drop_na() %>% 
  
  # Filters only for animals allowed to hunt
  filter(hunt == TRUE) %>%
  
  # Creates new column "sex" to change "f" and "m" to "Female" & "Male"
  mutate(sex = ifelse(animal_sex == "f", "Female", "Male")) %>% 
  
  # selects for the listed cols
  select(sex, age_years, hrs_indoors, prey_p_month)

view(cats_data)

#------[Generate Plot]------
# Creates plot using cats_data
cats_plot <- ggplot(data = cats_data,
                    aes(x = hrs_indoors,
                        y = prey_p_month,
                        color = age_years))+
  
  # Makes scatterplot
  geom_point()+
  
  # Makes points color-blind friendly
  scale_color_viridis_c()+
  
  # Subsets plot by male vs female
  facet_wrap(~sex)+
  
  # Labels plot elements (axes, legend, etc.)
  labs(
    x = "Time spent indoors (hours)",
    y = "Number of prey caught per month",
    color = "Age (yrs)",
    title = "Average Time Spent Indoors vs Prey Caught per month in Male vs Female Cats",
    caption = "cite: Kays R, Dunn RR, Parsons AW, Mcdonald B, Perkins T, Powers S, Shell L, McDonald JL, Cole H, Kikillus H, Woods L, Tindle H, Roetman P (2020) 
The small home ranges and large local ecological impacts of pet cats. Animal Conservation. doi:10.1111/acv.12563"
    )+

  # Adds jitter (so data points do not overlap)
  geom_jitter()+

  # Changes size and color of various plot elements
  theme(
    axis.title = element_text(size = 11, color = "navy"),
    legend.title = element_text(size = 8),
    plot.title = element_text(size = 13, face = "bold"),
    panel.background = element_rect(fill = "linen"),
    strip.background.x = element_rect(fill = "goldenrod"),
    plot.caption = element_text(size = 5, color = "darkgray"))

cats_plot

ggsave(here("TidyTuesday1","output", "TidyTuesday_cats_plot.png"),
       height = 5, width = 7.5)