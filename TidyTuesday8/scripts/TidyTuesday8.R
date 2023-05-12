#### General Info ####--------------------------------------------------
#
# This script is my Tidy Tuesday 8 (last one!).
# This will be using the Tidy Tuesday Childcare dataset from the National Database of Childcare Prices.
#
# created: 2023-05-09
# created by: Marisa Mackie
# edited: 2023-05-11

#### Load Libraries ####------------------------------------------------
library(tidyverse)
library(here)
library(patchwork)

#### Data ####----------------------------------------------------------
childcare_costs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/childcare_costs.csv')
counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv')
view(childcare_costs)
view(counties)

#### Analysis ####------------------------------------------------------


### Wrangling data ----

# get county code for LA county (California)
counties_CA <- counties %>% 
  # filters only for LA county
  filter(county_name == "Los Angeles County")

glimpse(counties_CA) # looks at filtered data. The county code is 6037

# wrangles data for childcare costs
childcare_costs_LA <- childcare_costs %>% 
  # selects for specified columns
  select(county_fips_code, study_year, mfccsa, pr_f) %>% 
  # filters only for LA county
  filter(county_fips_code == 6037)
  
view(childcare_costs_LA)

### Plotting ----

# generates plot using childcare costs data for LA
ChildcarePlot <- ggplot(childcare_costs_LA,
               aes(x = study_year))+
  
  # creates line based on weekly median family care charge data
  geom_line(aes(y = mfccsa), color = "darkviolet")+
  # creates line based on poverty rate data
  geom_line(aes(y = pr_f), color = "darkorange4")+
  
  # specifies x axis
  scale_x_continuous(breaks = c(2008:2018))+
  
  # makes two y-axes
  scale_y_continuous(name = "Weekly full-time median price charged for 
Family Childcare for school-age children", # names first y axis

  # makes second y axis (1/10th as the first axis) and names it
  sec.axis = sec_axis(trans=~./10, name = "Poverty Rate for Families"))+
  
  
  # specifies plot labels
  labs(x = "Year",
       title = "Comparison of Family Poverty Rate vs Weekly median price 
for Family Childcare in LA county from 2008-2018",
       caption = "Data source: Tidy Tuesday Childcare dataset from the National Database of Childcare Prices")+
  
  # specifies color and size of plot elements
  theme(plot.title = element_text(face = "bold", color = "darkgreen"),
        plot.caption = element_text(color = "gray40"),
        axis.title.y.left = element_text(color = "darkviolet"),
        axis.title.y.right = element_text(color = "darkorange4"))

ChildcarePlot

# save plot to output folder
ggsave(here("TidyTuesday8","output","ChildcarePlot.png"),
       width = 7, height = 7)

