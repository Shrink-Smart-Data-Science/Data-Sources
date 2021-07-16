library(tidyverse)
library(sf)
library(lubridate)
library(stringr)
library(geosphere)
library(sp)
library(rgeos)
library(stringr)
library(magrittr)
library(geodist)
library(readxl)
library(zipcodeR)


crs <- structure(list(epsg = 26915L, proj4string = "+proj=longlat +zone=15 +datum=NAD83 +units=mi +no_defs"), class = "crs")
#--- Start with getting the centriods from Ricardo ----
city_centroids <- read.csv("Data/place_centroids.csv") %>%
  select(City = name, center.long = lon, center.lat = lat)
  # st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  # st_transform(crs = crs)

#--- Nearest Point Function ----
#st_nn(a, b, k = 1, returnDist = T)
  
#---Distance from hospitals ----
hospitals <- read.csv("Data/IowaGov/hospitals.csv") %>%
  select(-X) %>%
  filter(!is.na(lat) & !is.na(long)) 

hospitals_sm <- hospitals %>%
  select(City,County,hospital_trauma_level,hosp.long = long,hosp.lat = lat) %>%
  mutate(City = str_to_title(City))

#Creates the distances from every hospital that available with the min distance.
ia_hospitals_dist <- 
  crossing(City = city_centroids$City, hosp.city = hospitals_sm$City) %>%
  left_join(city_centroids) %>%
  left_join(hospitals_sm, by = c("hosp.city" = "City")) %>%
  #group_by(City,hospital_trauma_level) %>%
  # Calculate distance between city and hospital
  mutate(dist = distGeo(cbind(hosp.long,hosp.lat), cbind(center.long,center.lat)),
  # Converts the distance to a mile calculation
         dist.mi = dist*0.000621371,
         hospital_trauma_level = ifelse(is.na(hospital_trauma_level), "Not rated", hospital_trauma_level)) %>%
  # Group by city
  group_by(City) %>%
  # Take minimum distance hospital
  filter(dist == min(dist)) %>%
  select(City,hosp.city,hosp.county = County,hospital_trauma_level,dist,dist.mi)

# ia_hospitals_dist2 <- ia_hospitals_dist %>%
#   group_by(City) %>%
#   arrange(City, hosp.city,-dist.mi) %>%
#   top_n(n = 2)
# 
#   slice_max(order_by = dist.mi, n = 2)

write.csv(ia_hospitals_dist,"Data/Distance_Data/Hospital_distances.csv", row.names = FALSE)


#---Distance from Post Office - TO DO: GET THE RIGHT POST OFFICE INFO!!  ----
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

post_offices <- tibble(text = read_lines("Data/Iowa_Post_Offices2.txt", skip = 2)) %>%
  tidyr::extract(text, into = c("zip5", "post_office", "established", "discontinued"),
                 regex = "(\\d{5})?\\s{0,}([A-Z ]{1,}(?:.\\d{1,}.)?)\\t([0-9/]{0,10})\\t([0-9/]{0,10})") %>%
  mutate(zip5 = ifelse(is.na(zip5), lag(zip5, 1), zip5) %>% as.numeric(),
         post_office = proper(post_office)) %>%
  filter(is.na(discontinued) | discontinued == "") %>%
  group_by(zip5, post_office) %>%
  summarize(n_post_offices = n()) %>%
  select(post_office, zip5, n_post_offices) %>%
  left_join(ia_city_county_population %>% filter(year == 2019) %>% 
              select(post_office = geo_name,post.lat = lat,post.long = long)) %>% 
  unique()

post_offices_sm <- post_offices %>%
  select(-zip5) %>% 
  rename(City = post_office) %>%
  filter(!is.na(post.lat) & !is.na(post.long))

  #Creates the distances from every post office that available with the min distance.
ia_postoffice_dist <-
  crossing(City = city_centroids$City, post.city = post_offices_sm$City) %>%
  left_join(city_centroids) %>%
  left_join(post_offices_sm, by = c("post.city" = "City")) %>%
  group_by(City) %>%
  # Calculate distance between city and hospital
  mutate(dist = distGeo(cbind(post.long,post.lat), cbind(center.long,center.lat)),
         # Converts the distance to a mile calculation
         dist.mi = dist*0.000621371) %>%
  # Group by city
   group_by(City) %>%
  # Take minimum distance post office
  filter(dist == min(dist)) %>%
  select(City,post.city,dist,dist.mi)

write.csv(ia_postoffice_dist,"Data/Distance_Data/PostOffice_distances.csv", row.names = FALSE)
 
#--- Distance from Fire Department ----
fire_dept <- read.csv("Data/IowaGov/fire_dept.csv") %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  select(-X)
  
city_county_centroids <- city_centroids %>%
  left_join(ia_city_county_population %>% select(City = geo_name, county, year) %>% 
              group_by(City) %>% filter(year == 2019))
  
fire_dept_sm <- fire_dept %>%
  select(dept_type,Firefighters,primary_agency_for_em,
         number_stations,fire.county = County,fire.lat = lat,fire.long = long,zip5) %>%
  mutate(primary_agency_for_em =ifelse(is.na(primary_agency_for_em),"Not App",primary_agency_for_em))

ia_firedept_dist <- 
  crossing(county = city_county_centroids$county, fire.county = fire_dept_sm$fire.county) %>%
  left_join(city_county_centroids) %>%
  left_join(fire_dept_sm) %>%
  group_by(county) %>%
  # Calculate distance between city and hospital
  mutate(dist = distGeo(cbind(fire.long,fire.lat), cbind(center.long,center.lat)),
         # Converts the distance to a mile calculation
         dist.mi = dist*0.000621371) %>%
  # Group by city
  group_by(City) %>%
  # Take minimum distance fire department
  filter(dist == min(dist)) %>%
  #ungroup() %>%
  select(center.city = City,fire.county = county,number_stations,Firefighters,dept_type,dist,dist.mi)

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

schools.test <- read_excel("Data/2020-21PublicSchoolBuildingDirectory9-11-20.xlsx") %>%
  fix_names() %>%
  remove_empty_cols() %>%
  dplyr::rename(county = County.Name) %>%
  mutate(District.Name = ifelse(is.na(District.Name), "Private", District.Name)) %>%
  mutate(grade_start = sub("(^[^-]+)-.*", "\\1", Grades.Served),
         grade_end = sub('.*-','', Grades.Served)) %>%
  mutate(zip5 = str_extract(Zip.Code.12, "\\d{5}")) %>%
  mutate(grade_start = str_replace(grade_start, "P?K" , "0") %>% parse_number,
         grade_end = str_replace(grade_end, "P?K" , "0") %>% parse_number,
         public = District.Name != "Private") %>%
  mutate(grade_start = ifelse(grade_start == "NULL", NA, grade_start) %>% as.numeric,
         grade_end = ifelse(grade_end == "NULL", NA, grade_end) %>% as.numeric) %>%
  unite("Address", Physical.Street:Zip.Code.16, sep= ", ", 
        remove = FALSE) %>%
  filter(!is.na(grade_start) & !is.na(grade_end)) %>%
  mutate(type = purrr::map2(grade_start, grade_end, school_cat)) %>%
  unnest(type) 
  
## Use zipcodeR to get the lat/long from zipcode values
#Example using an IOWA zipcode
ggmap::geocode("ames iowa", output = "latlona")

school.lat.lng <- ggmap::geocode(schools.test$Address, output = "latlona") 

school.lat.lng <- school.lat.lng %>% mutate(Address = proper(address))

#Take the zipcodes and add them to the original data
schools.test <- schools.test %>%
  left_join(school.lat.lng, by = c("Zip.Code.12" = "zipcode")) %>%
  filter(!is.na(lat) & !is.na(lng)) 

school_sm <- schools.test %>%
  select(Mailing.City,county,District, public, type, school.long = lng, school.lat = lat)


ia_schools_dist <- 
  crossing(City = city_centroids$City, school.city = school_sm$Mailing.City) %>%
  left_join(city_centroids) %>%
  left_join(school_sm, by = c("school.city" = "Mailing.City")) %>%
  #group_by(City,hospital_trauma_level) %>%
  # Calculate distance between city and school
  mutate(dist = distGeo(cbind(school.long,school.lat), cbind(center.long,center.lat)),
         # Converts the distance to a mile calculation
         dist.mi = dist*0.000621371) %>%
  # Group by city
  group_by(City, public, type) 

%>%
  # Take minimum distance school
  filter(dist == min(dist)) %>%
  select(City,school.city,school.county = county,public,type,dist,dist.mi)

ia_city_school_dist <- 
  ia_schools_dist %>%
  mutate(key = sprintf("dist_%s_%s", c("private", "public")[public+1], type)) %>%
  ungroup() %>%
  select(-public, -type) %>%
  unique() %>%
  tidyr::spread(key = key, value = dist.mi)
  

write.csv(ia_city_school_dist,"Data/Distance_Data/School_distances.csv", row.names = FALSE)

#--- Clean up -----

save(ia_firedept_dist, ia_hospitals_dist,
     ia_postoffice_dist, ia_city_school_dist, file = "Data/distance_Data.Rdata")

save(school_sm, file = "Data/schools.Rdata")

rm(fire_dept, fire_dept_sm, hospitals, hospitals_sm,
   post_offices, post_offices_sm, schools, school_sm)

