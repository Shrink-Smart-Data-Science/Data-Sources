library(sf)
library(tidyverse)
library(here)
library(readxl)

crs <- structure(list(epsg = 26915L, proj4string = "+proj=utm +zone=15 +datum=NAD83 +units=mi +no_defs"), class = "crs")


census2010_pop_zip <- read_csv(here::here("data/National_Data/pop-by-zip-code.csv")) %>%
  select(zip5 = zip_code, pop_2010 = `y-2010`) %>%
  mutate(zip5 = as.numeric(zip5))

ia_cities <- sf::read_sf("data/Inc_cities_Twp_2010/") %>% 
  filter(str_detect(CLASSFP10, "C")) %>%
  mutate(geometry = st_transform(geometry, crs))

us_zip_codes <- sf::read_sf("data/Geography/cb_2018_us_zcta510_500k/") %>%
  mutate(geometry = st_transform(geometry, crs))

iowa_border <- sf::read_sf("data/Geography/iowa_border/") %>%
  mutate(geometry = st_transform(geometry, crs))
ia_counties <- sf::read_sf("data/Geography/county/") %>%
  mutate(geometry = st_transform(geometry, crs))

centroid_points <- read_xls(here::here("data/place_centroids.xls"))

# ia_zip_codes <- intersect(us_zip_codes, iowa_border)

# ia_zipcodes <- read_csv("data/Geography/zip_code_database.csv") %>%
#   filter(state == "IA")
# 
# ia_zipcode_sf <- us_zip_codes %>% filter(ZCTA5CE10 %in% ia_zipcodes$zip)
# 
# # fix crs
# ia_zipcode_sf$geometry <- st_transform(ia_zipcode_sf$geometry, st_crs(ia_cities$geometry))
# write_rds(ia_zipcode_sf, "data/IA_zip_sf.rda")
# ia_cities$geometry <- st_transform(ia_cities$geometry, st_crs(ia_zipcode_sf$geometry))
ia_zipcode_sf <- read_rds("data/IA_zip_sf.rda") %>%
  mutate(geometry = st_transform(geometry, crs)) %>%
  mutate(zip_area = st_area(geometry))

# find_ia_zipcode <- partial(st_intersection, y = ia_zipcode_sf)
# 
# ia_city_df <- ia_cities %>%
#   mutate(idx = 1:n()) %>%
#   nest(-idx, .key = "df")
# 
# ia_city_zip <- ia_city_df$df %>%
#   purrr::map_dfr(find_ia_zipcode) %>%
#   rename(City = NAME10, zip5 = ZCTA5CE10) %>%
#   select(City, zip5, zip_area, lat = INTPTLAT10, long = INTPTLON10, geometry)
# 
# write_rds(ia_city_zip, "data/IA_City_Zip.rda")
ia_city_zip <- read_rds("data/IA_City_Zip.rda") %>%
  mutate(zip_city_area = st_area(geometry)) %>%
  mutate(zip5 = as.numeric(zip5)) %>%
  left_join(census2010_pop_zip) %>%
  group_by(City) %>%
  mutate(pop_weight = (zip_city_area/zip_area) * pop_2010,
         weight = pop_weight/sum(pop_weight)) %>%
  ungroup() %>%
  select(-pop_weight, -zip_city_area, -zip_area)

# 
# 
# find_ia_county <- partial(st_intersection, y = ia_counties)
# ia_city_county <- ia_city_df$df %>%
#   purrr::map_dfr(find_ia_county) %>%
#   rename(City = NAME10, County = COUNTY, County_FIPS = CO_FIPS) %>%
#   select(City, County, County_FIPS, geometry)
# 
# write_rds(ia_city_county, "data/IA_City_County.rda")
ia_city_county <- read_rds("data/IA_City_County.rda")

ia_census_place <- sf::read_sf("data/Geography/cb_2018_19_place_500k/") %>%
  mutate(FIPS = paste0(STATEFP, PLACEFP) %>% as.numeric()) %>%
  rename(place_geometry = geometry)

suppressWarnings(rm(us_zip_codes, iowa_border, ia_zip_codes, find_ia_zipcode, find_ia_county))


