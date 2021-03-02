library(sf)
library(tidyverse)
library(here)
library(readxl)
library(sp)

crs <- structure(list(epsg = 26915L, proj4string = "+proj=utm +zone=15 +datum=NAD83 +units=mi +no_defs"), class = "crs")


census2010_pop_zip <- read_csv(here::here("Data/National_Data/pop-by-zip-code.csv")) %>%
  select(zip5 = zip_code, pop_2010 = `y-2010`) %>%
  mutate(zip5 = as.numeric(zip5))

#--- US Zip Codes ----
us_zip_codes <- sf::read_sf("Data/Geography/cb_2018_us_zcta510_500k/") %>%
  mutate(geometry = st_transform(geometry, crs))
#--- Iowa border, counties and cities ----
iowa_border <- sf::read_sf("Data/Geography/iowa_border/") %>%
  mutate(geometry = st_transform(geometry, crs))
ia_counties <- sf::read_sf("Data/Geography/county/") %>%
  mutate(geometry = st_transform(geometry, crs))
ia_cities <- sf::read_sf("Data/Inc_cities_Twp_2010/") %>% 
  filter(str_detect(CLASSFP10, "C")) %>%
  mutate(geometry = st_transform(geometry, crs))

#--- Place Centers from NAD 1983 to coordinates -----
centroid_points <- read_xls(here::here("Data/place_centroids.xls")) %>%
  select(NAME, `CENTROID_X (NAD_1983_UTM_Zone_15N)`, `CENTROID_Y (NAD_1983_UTM_Zone_15N)`) 
  
coordinates(centroid_points) <- c('CENTROID_X (NAD_1983_UTM_Zone_15N)', 'CENTROID_Y (NAD_1983_UTM_Zone_15N)')
proj4string(centroid_points) = CRS("+init=EPSG:3531")
coordinates_deg <- spTransform(centroid_points, CRS("+init=epsg:4326")) %>% 
  as.data.frame() %>% 
  rename(long = `CENTROID_X..NAD_1983_UTM_Zone_15N.`,
         lat = `CENTROID_Y..NAD_1983_UTM_Zone_15N.`) %>%
  left_join(read_xls(here::here("Data/place_centroids.xls")), by = "NAME")

#write.csv(coordinates_deg,"Data/place_centroids_with_latlong.csv", row.names = FALSE)

#--- Creating the shape files ----
ia_zipcode_sf <- read_rds("Data/IA_zip_sf.rda") %>%
  mutate(geometry = st_transform(geometry, crs)) %>%
  mutate(zip_area = st_area(geometry))

ia_city_zip <- read_rds("Data/IA_City_Zip.rda") %>%
  mutate(zip_city_area = st_area(geometry)) %>%
  mutate(zip5 = as.numeric(zip5)) %>%
  left_join(census2010_pop_zip) %>%
  group_by(City) %>%
  mutate(pop_weight = (zip_city_area/zip_area) * pop_2010,
         weight = pop_weight/sum(pop_weight)) %>%
  ungroup() %>%
  select(-pop_weight, -zip_city_area, -zip_area)

ia_city_county <- read_rds("Data/IA_City_County.rda")

ia_census_place <- sf::read_sf("Data/Geography/cb_2018_us_zcta510_500k/") %>%
  mutate(FIPS = GEOID10 %>% as.numeric()) %>%
  rename(place_geometry = geometry)

suppressWarnings(rm(centroid_points,us_zip_codes, iowa_border, ia_zip_codes, find_ia_zipcode, find_ia_county))


