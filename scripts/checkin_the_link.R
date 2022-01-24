library(sf)
library(dplyr)
library(tidyr)

# get original json from Andrea and label from 0 to max
original_geojson <- st_read("data/andrea_data/json_point_20200822.geojson")
original_geojson$link_id <- c(0:(nrow(original_geojson)-1))

# read in all the IDs in the safe folder
safe_imgs <- list.files("data/simon_data/Labelled polygon data/1 Safe/")
safe_imgs <- gsub("Area ", "", safe_imgs)
safe_imgs <- gsub("_Labelled.txt", "", safe_imgs)
link_safe_imgs <- data.frame(link_id = safe_imgs, 
                            type = "safe")

# read in all the IDs in the unsafe folder
unsafe_imgs <- list.files("data/simon_data/Labelled polygon data/0 Unsafe/")
unsafe_imgs <- gsub("Area ", "", unsafe_imgs)
unsafe_imgs <- gsub("_Labelled.txt", "", unsafe_imgs)
link_unsafe_imgs <- data.frame(link_id = unsafe_imgs, 
                             type = "fear")

# link up the safe and unsafe images
link_type <- bind_rows(link_safe_imgs, 
                       link_unsafe_imgs)

# join this with the original data
geodata_check <- left_join(original_geojson %>% mutate(link_id = as.character(link_id)), link_type, by = c ("link_id" = "link_id"))

# clean 'type.x' variable by removing "Polygon" from values
geodata_check <-  geodata_check %>% mutate(type.x = gsub("Polygon", "", type.x))

# check for rows where two types don't match
geodata_check %>% filter(type.x != type.y) %>% View()

