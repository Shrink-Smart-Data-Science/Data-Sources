library(tidyverse)
library(sf)
library(lubridate)
library(RMySQL)
library(DBI)
library(keyring)
library(stringr)

conn <- DBI::dbConnect(RMySQL::MySQL(),
                       host = "srvanderplas.com",
                       dbname = "scc",
                       user = "remote",
                       password = keyring::key_get("MY_SECRET"))

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
  select(City,County,hospital_trauma_level,geometry) %>%
  mutate(City = str_to_title(City))

ia_hospitals_dist <- 
  full_join(county_centriods %>% as.data.frame(), 
                hospitals_sm %>% as.data.frame(), by = c("NAME" = "City")) %>% 
  na.omit() %>%
  st_sf(sf_column_name = 'geometry.x') %>%
  st_sf(sf_column_name = 'geometry.y') %>%
  select(City = NAME,County, hospital_trauma_level,center = geometry.x,geometry = geometry.y) %>%
  mutate(dist = st_distance(center, geometry, by_element = T)) %>% ## how to get units
  group_by(City,County,hospital_trauma_level) %>%
  filter(dist == min(dist)) %>%
  ungroup()

write.csv(ia_hospitals_dist,"Data/Distance_Data/Hospital_distances.csv", row.names = FALSE)


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

write.csv(ia_postoffice_dist,"Data/Distance_Data/PostOffice_distances.csv", row.names = FALSE)

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

write.csv(ia_firedept_dist,"Data/Distance_Data/FireDept_distances.csv", row.names = FALSE)






#---Distance from schools ----
school_cat <- function(grade_start, grade_end) {
  gradetbl <- tibble(grade = 0:12, type = rep(c("Elementary", "Middle", "High"), c(6, 3, 4)))
  grades <- seq(grade_start, grade_end, by = 1)
  tmp <- gradetbl %>%
    filter(grade %in% grades) %>%
    group_by(type) %>%
    count()
  if (nrow(tmp) > 1) {
    tmp <- filter(tmp, n > 1)
  }
  
  tmp$type
}

schools <- dbReadTable(conn, "school_building_directory") %>%
  fix_names() %>%
  remove_empty_cols() %>%
  dplyr::rename(county = co_name) %>%
  mutate(district_name = ifelse(is.na(district_name), "Private", district_name)) %>%
  mutate(zip5 = str_extract(mailing_zip_code, "\\d{5}"),
         coords = str_remove(physical_location, " ?POINT ?"),
         lat = str_extract(gsub("^.* ", "", coords), pattern = "-?\\d+(?:\\.\\d+)?") %>% parse_number(),
         long = str_extract(coords, "-?\\d+(?:\\.\\d+)?") %>% parse_number()) %>%
  mutate(grade_start = str_replace(grade_start, "P?K" , "0") %>% parse_number,
         grade_end = str_replace(grade_end, "P?K" , "0") %>% parse_number,
         public = district_name != "Private") %>%
  mutate(grade_start = ifelse(grade_start == "NULL", NA, grade_start) %>% as.numeric,
         grade_end = ifelse(grade_end == "NULL", NA, grade_end) %>% as.numeric) %>%
  filter(!is.na(grade_start) & !is.na(grade_end)) %>%
  mutate(type = purrr::map2(grade_start, grade_end, school_cat)) %>%
  unnest(type) %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs)

school_sm <- schools %>%
  select(mailing_city,county,district_school_id, public, type, geometry)


ia_schools_dist <- 
  full_join(county_centriods %>% as.data.frame(), 
            school_sm %>% as.data.frame(), by = c("NAME" = "mailing_city")) %>% 
  na.omit() %>%
  st_sf(sf_column_name = 'geometry.x') %>%
  st_sf(sf_column_name = 'geometry.y') %>%
  select(City = NAME,county,district_school_id,public,type,center = geometry.x,geometry = geometry.y) %>%
  mutate(dist = st_distance(center, geometry, by_element = T)) %>% ## how to get units
  group_by(City,county,district_school_id,public,type) %>%
  filter(dist == min(dist)) %>%
  ungroup()

write.csv(ia_schools_dist,"Data/Distance_Data/School_distances.csv", row.names = FALSE)

#--- Clean up -----

save(ia_firedept_dist, ia_hospitals_dist,
     ia_postoffice_dist, ia_schools_dist, file = "Data/distance_Data.Rdata")

rm(fire_dept, fire_dept_sm, hospitals, hospitals_sm,
   post_offices, post_offices_sm, schools, school_sm)

