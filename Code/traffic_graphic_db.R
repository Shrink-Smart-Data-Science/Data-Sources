library(sf)
library(tidyverse)

fix_names <- function(x) {
  x %>%
    set_names(make.names(names(x)) %>% str_replace_all("\\.{1,}", ".") %>% 
                str_remove("\\.$"))
}

remove_empty_cols <- function(x) {
  all_same_cols <- apply(x, 2, function(z) length(unique(z)))
  
  x %>% select(which(all_same_cols > 1))
}

crs <- structure(list(epsg = 26915L, proj4string = "+proj=utm +zone=15 +datum=NAD83 +units=mi +no_defs"), class = "crs")

iowa_border <- sf::read_sf("Data/Geography/iowa_border/") %>%
  mutate(geometry = st_transform(geometry, crs))

ia_counties <- sf::read_sf("Data/Geography/county/") %>%
  mutate(geometry = st_transform(geometry, crs))

traffic <- sf::st_read(dsn = "Data/Traffic_data.gdb", layer = "Primary") %>%
  mutate(geometry = st_transform(geometry, crs))

ia_centroids <- read.csv("Data/place_centroids_with_latlong.csv")


ggplot() + 
  geom_sf(aes(geometry = geometry), data = rivers, color = "blue", alpha = .5) + 
  geom_sf(data = ia_counties, color = "grey", fill = "white", alpha = .5) + 
  geom_sf(data = schools, aes(color = type, shape = type)) + 
  scale_shape_discrete(solid = F)
