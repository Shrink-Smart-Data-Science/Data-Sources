library(tidyverse)
library(sf)
library(lubridate)
library(RMySQL)
library(DBI)
library(keyring)
library("ggspatial")

#--- Start with getting the centriods from Ricardo ----
county_centriods <- read.csv("Data/place_centroids_with_latlong.csv") %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs)
  
#---Distance from hospitals ----
hospitals <- read.csv("Data/IowaGov/hospitals.csv") %>%
  select(-X) %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs)

hospitals_sm <- hospitals %>%
  select(City,County,hospital_trauma_level,geometry)

ia_hospitals_dist <- 
  full_join(county_centriods %>% as.data.frame(), 
                hospitals_sm %>% as.data.frame(), by = c("NAME" = "County")) %>% 
  na.omit() %>%
  st_sf(sf_column_name = 'geometry.x') %>%
  st_sf(sf_column_name = 'geometry.y') %>%
  select(City,County = NAME, hospital_trauma_level,center = geometry.x,geometry = geometry.y) %>%
  mutate(dist = st_distance(center, geometry, by_element = T)) %>% ## how to get units
  group_by(City,County,hospital_trauma_level) %>%
  filter(dist == min(dist)) %>%
  ungroup()

#--- Graphical Visualization ----
iowa <- map_data("county")  %>%
  filter(region == 'iowa') 

# %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
#   st_transform(crs = crs) 

iowa2 = st_cast(iowa, "GEOMETRY")

ia_hospitals_dist2 = ia_hospitals_dist %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
  
ggplot() + 
   geom_sf(data = iowa) +
   geom_sf(data = ia_hospitals_dist2, colour = "red", fill = NA) +
   geom_sf(data = ia_firedept_dist2, colour = "blue", fill = NA) 
  


#---Distance from Post Office  ----
post_offices <- read.csv("Data/IowaGov/post_offices.csv") %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs)

post_offices_sm <- post_offices %>%
  select(map_name,County,geometry)

ia_postoffice_dist <- 
  full_join(county_centriods %>% as.data.frame(), 
            post_offices_sm %>% as.data.frame(), by = c("NAME" = "County")) %>% 
  na.omit() %>%
  st_sf(sf_column_name = 'geometry.x') %>%
  st_sf(sf_column_name = 'geometry.y') %>%
  select(map_name, County = NAME, center = geometry.x, geometry = geometry.y) %>%
  mutate(dist = st_distance(center, geometry, by_element = T)) %>% ## how to get units
  group_by(map_name,County) %>%
  filter(dist == min(dist)) %>%
  ungroup()

#--- Distance from Fire Department ----
fire_dept <- read.csv("Data/IowaGov/fire_dept.csv") %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs)

fire_dept_sm <- fire_dept %>%
  select(dept_type,organization_type,primary_agency_for_em,County,geometry)

ia_firedept_dist <- 
  full_join(county_centriods %>% as.data.frame(), 
            fire_dept_sm %>% as.data.frame(), by = c("NAME" = "County")) %>% 
  na.omit() %>%
  st_sf(sf_column_name = 'geometry.x') %>%
  st_sf(sf_column_name = 'geometry.y') %>%
  select(dept_type,organization_type,primary_agency_for_em, County = NAME, center = geometry.x, geometry = geometry.y) %>%
  mutate(dist = st_distance(center, geometry, by_element = T)) %>% ## how to get units
  group_by(dept_type,organization_type,primary_agency_for_em,County) %>%
  filter(dist == min(dist)) %>%
  ungroup()

ia_firedept_dist2 = ia_firedept_dist %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

#--- Clean up -----

save(ia_firedept_dist, ia_hospitals_dist,
     ia_postoffice_dist, file = "Data/distance_Data.Rdata")

rm(fire_dept, hospitals,
   post_offices)