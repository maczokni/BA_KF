# This script takes gender out of the fear of crime polygon data, 
#  cleans and translates it
#  and attaches it to the polygons data

library(sf) # package for spatial data
library(ggplot2) # package for visualisations
library(dplyr) # package for data wrangling
library(tidyr) # package for data wrangling

# read in the participant answers
data <- read.csv("data/andrea_data/answer_20200822.csv") 

# read in the polygon data
geodata <- st_read("data/andrea_data/json_point_20200822.geojson")

# Plot the polygons just to have a look. 
ggplot() + 
  ggspatial::annotation_map_tile(zoom = 11, type = "stamenbw") +
  geom_sf(data = geodata, aes(colour = type), fill = NA) + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank())


# clean the participant data to extract (and translate) gender for respondents: 
data <- data %>% 
  separate(username.date.answer., c("id", "datetime", "data"), ";") %>% 
  filter(data %in% c("Férfi", "Nő")) %>% 
  select(-datetime) %>% 
  mutate(id = gsub("anonymous_", "", id), 
         gender = ifelse(data == "Férfi", "Male", "Female"))

# Join with the polygon data to add gender of each respondent
polys_w_data <- left_join(geodata, data, by = c("id" = "id"))

# sanity check
table(polys_w_data$data)
table(polys_w_data$gender)

# write out new dataframe
st_write(polys_w_data, "data/polys_w_gender.geojson")

