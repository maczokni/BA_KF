library(janitor)

simon_data <- read.csv("data/polys_linked_w_images_simon.csv") %>% clean_names()

reka_data <- read.csv("data/polys_linked_w_images_reka.csv") %>% clean_names() #%>% select(-c(x, num_images, id))

names(reka_data)[!names(reka_data) %in% names(simon_data)]
names(simon_data)[!names(simon_data) %in% names(reka_data)]
