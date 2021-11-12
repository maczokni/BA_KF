# This script is to check whether larger area polygons have more images
#  it's just a sanity check really!

library(sf) # package for spatial data
library(ggplot2) # package for visualisations
library(dplyr) # package for data wrangling
library(tidyr) # package for data wrangling
library(readr) # to read csv data
library(janitor) # to tidy variable names

options(scipen = 9999) # turn off scientific notation

# read in data with all variables combined
all_data <- read_csv("data/polys_linked_w_images.csv") %>% clean_names()


# test correlation
cor.test(all_data$num_images, all_data$poly_area)

# plot correlation
ggplot(all_data, aes(x = num_images, y = as.numeric(poly_area))) + 
  geom_point() + 
  geom_smooth() + 
  scale_x_log10() + 
  scale_y_log10() + 
  xlab("Number of images") + 
  ylab("Area of polygon (square meters)") + 
  theme_minimal()



