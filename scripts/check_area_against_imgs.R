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

# Any difference in the size of polys between safe/worry?

ggplot(all_data, aes(x = type, y = poly_area)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  theme_minimal() + 
  xlab("Polygon type") + 
  ylab("Polygon size (km^2)")

# people draw larger fear polygons and smaller safe polygons. 
# I assume the number of images per each might be different too then

ggplot(all_data, aes(x = type, y = num_images)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  theme_minimal() + 
  xlab("Polygon type") + 
  ylab("Number of images per polygon")


# Makes sense... 

# So possibly might need some normalised number of observations... like count per image in polygon

