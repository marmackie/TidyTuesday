###### General Info ######--------------------------------------------------
#
# This script is my Tidy Tuesday 3.
# This will be using the European Drug Development data from  European Medicines Agency 
# via Miquel Anglada Girotto on GitHub
#
# created: 2023-03-17
# created by: Marisa Mackie
# edited: 2023-03-19

###### Load Libraries ######------------------------------------------------
library(tidyverse)
library(here)
library(data.table)

###### Data ######----------------------------------------------------------
drugs <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv")

view(drugs)

####### Analysis ######-----------------------------------------------------

### Cleaning ###-----------------------------

# Filters drug data for human medicines for Type 2 Diabetes & their active ingredients
drugs <- drugs %>% 
  
  # selects for the following columns of interest
  select(category, medicine_name, therapeutic_area, active_substance) %>%
  
  # filters for only human medicines that treat Type 2 Diabetes
  filter(category == "human" & therapeutic_area == "Diabetes Mellitus, Type 2") %>% 
  
  # separates data with multiple active substances into singles
  separate(col = active_substance,
           into = c("active_1", "active_2"),
           sep = ", ") %>% 
  
  # pivots data longer to re-align medicines with their corresponding active ingredients
  pivot_longer(cols = active_1:active_2,
               names_to = "substance",
               values_to = "active_substances") %>% 
  
  # removes NA values (medicines that only had 1, not 2, active substances)
  drop_na() %>% 
  
  # selects only for active substances & arranges them alphabetically
  select(active_substances) %>% 
  arrange(active_substances)

view(drugs)

# creates table showing how often each active ingredient appears in Diabetes medicines
actives <- table(drugs$active_substances)
view(actives)
  
# converts actives data from table to dataframe so we can use ggplot later
actives_tbl <- data.frame(actives)

### Plot ###-----------------------------

# Creates lollipop plot showing how often each active ingredient is used in medicine for Type 2 Diabetes
DrugsPlot <- ggplot(actives_tbl,
                    mapping = aes(x = Var1, # x variable is active substance
                                  y = Freq))+ # y variable is frequency used
  
  # geometry to specify lollipop plot
  geom_point(size = 5, color = "maroon", 
             fill = alpha("hotpink", 0.5), alpha = 0.7, 
             shape = 21, stroke = 2)+ # customizes lollipop tops
  geom_segment(aes(x = Var1, xend = Var1, y = 0, yend = Freq))+
  
  # labels axes, title, other plot elements
  labs(x = "Active Substance",
       y = "Frequency",
       title = "How often Active Substances are used in Medicines that treat Type 2 Diabetes",
       caption = "European Drug Development data from European Medicines Agency, provided by Miquel Anglada Girotto on GitHub")+
  
  # specifies color, size, rotation of plot elements
  theme(axis.text.x = element_text(angle = 75, vjust = 0, hjust = 0), # rotates x axis text
        plot.caption = element_text(color = "gray68", size = 7),
        plot.title = element_text(color = "maroon4", size = 18),
        axis.title = element_text(size = 15),
        panel.background = element_rect(fill = "lightblue"))
  
DrugsPlot

# saves plot & specifies dimensions
ggsave(here("TidyTuesday3", "output", "DrugsPlot.png"),
       width = 10, height = 7)
