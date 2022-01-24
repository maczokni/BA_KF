library(readr)
library(dplyr)
library(sf)

polys_w_data <- st_read("data/polys_w_gender.geojson")

# create new variable with area of polygon
polys_w_data$poly_area <- lwgeom::st_geod_area(polys_w_data)

under10thousand <- polys_w_data %>% filter(as.numeric(poly_area) < 10000 & 
                                             as.numeric(poly_area) > 0) %>% 
  mutate(x = 1:nrow(.))

# take 5, 14, 28, 35, 39, 57, 65, 85
for_sso <- under10thousand %>% 
  filter(x %in% c(5, 14, 17,28, 39, 41, 54, 85)) %>% 
  select(x, id, type, data, gender, poly_area)

st_write(for_sso, "data/for_sso.geojson")

library(leaflet)
pal <- colorFactor("viridis", for_sso$type)

leaflet(for_sso) %>% 
  addTiles() %>% 
  addPolygons(fillOpacity = 0.7,
              fillColor = ~pal(type), 
              color = ~pal(type), 
              label = ~x) %>% 
  addLegend(pal = pal, values = ~type)



# -----
  
all_data <- read_csv("data/polys_linked_w_images.csv") %>% clean_names()
geodata <- st_read("data/geodata_w_linkid.geojson")

linked <- left_join(for_sso %>% 
                      mutate(poly_area = as.numeric(poly_area)), geodata %>% st_drop_geometry())
  
  

leaflet(linked) %>% 
  addTiles() %>% 
  addPolygons(fillOpacity = 0.7,
              fillColor = ~pal(type), 
              color = ~pal(type), 
              label = ~link_id) %>% 
  addLegend(pal = pal, values = ~type)





