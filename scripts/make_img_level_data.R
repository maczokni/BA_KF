library(sf) # package for spatial data
library(ggplot2) # package for visualisations
library(dplyr) # package for data wrangling
library(tidyr) # package for data wrangling
library(lwgeom) # package for calculating the area with geographic coords (lat/long)

# Function for cleaning up the confidence rating with each item
#  NOTE: if we want to use these, will need to think of a way to preserve them. 
deletePostSpace <- function(x){
  return( substr(x,1,nchar(x)-3))
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

datalist <- list() # create empty list
i <- 1 # iterator

for (imgfilename in list.files("data/simon_data/Labelled polygon data/1 Safe/")){
  
  # read in file
  safe_img_data <- read.delim(paste0("data/simon_data/Labelled polygon data/1 Safe/", imgfilename), sep = ",", header = FALSE)
  
  #clean away the accuracy scores
  safe_img_data <- safe_img_data %>% 
    select(-V1) %>% 
    mutate_all(deletePostSpace)
  
  imgs_list <- list()
  poly_id <- as.numeric(gsub("Area ", "", strsplit(imgfilename, "_")[[1]][1]))
  for (rownum in 1:nrow(safe_img_data)) {
    
    imgs_list[[rownum]] <- data.frame(img_id = paste0(poly_id, "_", rownum), 
                                      vec_of_features = unlist(safe_img_data[rownum, ]))
    
  }
  
  all_imgs <- do.call(bind_rows, imgs_list)
  all_imgs <- aggregate(all_imgs$vec_of_features, by=list(all_imgs$img_id), paste, collapse=";")
  all_imgs$vec_of_features <- strsplit(all_imgs$x, ";")
  
  datalist[[i]] <- all_imgs %>% 
    select(-x) %>% 
    mutate(poly_id = poly_id)
  
  print(paste("we're on poly", i))
  
  i <- i+1 # increase
  
}

# bind all images into a complete df with all safe polygons images
all_safe_imgs <- do.call(bind_rows, datalist) %>% 
  replace(is.na(.),0)





# get all the images for all the unsafe areas (repeats the steps for safe ones)
datalist <- list()
i <- 1
for (imgfilename in list.files("data/simon_data/Labelled polygon data/0 Unsafe/")){
  
  safe_img_data <- read.delim(paste0("data/simon_data/Labelled polygon data/0 Unsafe/", imgfilename), sep = ",", header = FALSE)
  
  #clean away the accuracy scores
  safe_img_data <- safe_img_data %>% 
    select(-V1) %>% 
    mutate_all(deletePostSpace)
  
  imgs_list <- list()
  poly_id <- as.numeric(gsub("Area ", "", strsplit(imgfilename, "_")[[1]][1]))
  for (rownum in 1:nrow(safe_img_data)) {
    
    imgs_list[[rownum]] <- data.frame(img_id = paste0(poly_id, "_", rownum), 
                                      vec_of_features = unlist(safe_img_data[rownum, ]))
    
  }
  
  all_imgs <- do.call(bind_rows, imgs_list)
  all_imgs <- aggregate(all_imgs$vec_of_features, by=list(all_imgs$img_id), paste, collapse=";")
  all_imgs$vec_of_features <- strsplit(all_imgs$x, ";")
  
  datalist[[i]] <- all_imgs %>% 
    select(-x) %>% 
    mutate(poly_id = poly_id)
  
  print(paste("we're on poly", i))
  
  i <- i+1 # increase
  
}

unsafe_imgs_labelled <- do.call(bind_rows, datalist) %>% 
  replace(is.na(.),0)

# join the safe and unsafe images into one unified dataframe
all_imgs_labelled <- bind_rows(all_safe_imgs, unsafe_imgs_labelled)

# add back the geodata variables joining on link_id
all_data <- left_join(all_imgs_labelled, geodata, by = c("poly_id" = "link_id"))

# convert unit to number 
all_data <- mutate(all_data, poly_area = as.numeric(poly_area))

# save as csv
saveRDS(all_data %>% select(-geometry), "data/all_images_w_features_and_poly_data.rds")

# There are two with non polygon areas (they are lines)
# geodata %>% filter(link_id %in% c(54, 67)) %>% View()
# thing <- geodata %>% filter(link_id %in% c(54, 67))
# plot(st_geometry(thing))
# 
# ggplot() + 
#   geom_sf(data = thing %>% filter(link_id == 54), col = "#e66101") + 
#   geom_sf(data = thing %>% filter(link_id == 67), col = "#5e3c99") + 
#   theme_minimal()
