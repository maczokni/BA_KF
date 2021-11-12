library(sf) # package for spatial data
library(ggplot2) # package for visualisations
library(dplyr) # package for data wrangling
library(tidyr) # package for data wrangling
library(lwgeom) # package for calculating the area with geographic coords (lat/long)

# Function for cleaning up the confidence rating with each item
#  NOTE: if we want to use these, will need to think of a way to preserve them. 
deletePostSpace <- function(x){
  return( gsub(" .*", "", x))
}

# read in the polygon with the gender attached
geodata <- st_read("data/polys_w_gender.geojson")

# create new variable with linking id to the images data files
geodata$link_id <- 0:(nrow(geodata)-1)

# create new variable with area of polygon
geodata$poly_area <- lwgeom::st_geod_area(geodata)

# get all the images for all the safe areas
# for each file count the number of occurrences of each feature for each image
# then save these all as columns
# and append to a dataframe where each polygon is 1 row

datalist <- list() %>% # create empty list
i <- 1 # iterator

for (imgfilename in list.files("data/simon_data/Labelled polygon data/1 Safe/")){
  
  # read in file
  safe_img_data <- read.delim(paste0("data/simon_data/Labelled polygon data/1 Safe/", imgfilename), sep = ",", header = FALSE)
  
  #clean away the accuracy scores
  safe_img_data <- safe_img_data %>% 
    select(-V1) %>% 
    mutate_all(deletePostSpace)
  
  # append into a one-column data frame
  items_list <- data.frame( 
    items = c(safe_img_data$V2, safe_img_data$V3, safe_img_data$V4, safe_img_data$V5, safe_img_data$V6,
              safe_img_data$V7, safe_img_data$V8, safe_img_data$V9, safe_img_data$V10, safe_img_data$V11)
  )
  
  # make sure it's all one row 
  wide_list <- as.data.frame(table(items_list$items)) %>% 
    pivot_wider(names_from = Var1, values_from = Freq) %>% 
    mutate(link_id = imgfilename, 
           link_id = as.numeric(gsub("Area ", "", strsplit(link_id, "_")[[1]][1])))
  
  # add to df list 
  datalist[[i]] <- wide_list %>% 
    mutate(num_images = nrow(safe_img_data)) # also include variable for number of images in poly. 
  
  i <- i+1 # increase
  
}

# bind all 1row dataframes into a complete one with all safe polygons
safe_imgs_labelled <- do.call(bind_rows, datalist) %>% 
  replace(is.na(.),0)


# get all the images for all the unsafe areas (repeats the steps for safe ones)
datalist <- list()
i <- 1
for (imgfilename in list.files("data/simon_data/Labelled polygon data/0 Unsafe/")){
  
  safe_img_data <- read.delim(paste0("data/simon_data/Labelled polygon data/0 Unsafe/", imgfilename), sep = ",", header = FALSE)
  
  safe_img_data <- safe_img_data %>% 
    select(-V1) %>% 
    mutate_all(deletePostSpace)
  
  items_list <- data.frame( 
    items = c(safe_img_data$V2, safe_img_data$V3, safe_img_data$V4, safe_img_data$V5, safe_img_data$V6,
              safe_img_data$V7, safe_img_data$V8, safe_img_data$V9, safe_img_data$V10, safe_img_data$V11)
  )
  
  wide_list <- as.data.frame(table(items_list$items)) %>% 
    pivot_wider(names_from = Var1, values_from = Freq) %>% 
    mutate(link_id = imgfilename, 
           link_id = as.numeric(gsub("Area ", "", strsplit(link_id, "_")[[1]][1])))
  
  datalist[[i]] <- wide_list %>% mutate(num_images = nrow(safe_img_data))
  
  i <- i+1
  
}

unsafe_imgs_labelled <- do.call(bind_rows, datalist) %>% 
  replace(is.na(.),0)

# join the safe and unsafe polygons into one unified dataframe
all_imgs_labelled <- bind_rows(safe_imgs_labelled, unsafe_imgs_labelled)

# add back the geodata variables joining on link_id
all_data <- left_join(all_imgs_labelled, geodata, by = c("link_id" = "link_id"))

# convert unit to number 
all_data <- mutate(all_data, poly_area = as.numeric(poly_area))

# save as csv
write.csv(all_data %>% select(-geometry), "data/polys_linked_w_images.csv")


# There are two with non polygon areas (they are lines)
# geodata %>% filter(link_id %in% c(54, 67)) %>% View()
# thing <- geodata %>% filter(link_id %in% c(54, 67))
# plot(st_geometry(thing))
# 
# ggplot() + 
#   geom_sf(data = thing %>% filter(link_id == 54), col = "#e66101") + 
#   geom_sf(data = thing %>% filter(link_id == 67), col = "#5e3c99") + 
#   theme_minimal()
