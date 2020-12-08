library(tidyverse)
library(sf)
library(lubridate)
library(RMySQL)
library(DBI)
library(keyring)
setwd("/Users/denisebradford/Documents/ShrinkSmartExploration/")
# --- Initial Setup ------------------------------------------------------------
conn <- DBI::dbConnect(RMySQL::MySQL(), 
                       host = "srvanderplas.com",
                       dbname = "scc",
                       user = "remote",
                       password = keyring::key_get("MY_SECRET"))

national_data_files <- list.files("data/National_Data/", full.names = T)

remove_empty_cols <- function(x) {
  all_same_cols <- apply(x, 2, function(z) length(unique(z)))
  
  x %>% select(which(all_same_cols > 1))
}

fix_names <- function(x) {
  x %>%
    set_names(make.names(names(x)) %>% str_replace_all("\\.{1,}", ".") %>% 
                str_remove("\\.$"))
}

fips_data <- read.csv("/Users/denisebradford/Documents/ShrinkSmartExploration/data/fips_codes.csv", skip = 4, header = TRUE) 
fips_data$county <- gsub("([A-Za-z]+).*", "\\1", fips_data$Area_Name_FIPS)
#write.csv(fips_data, "/Users/denisebradford/Documents/Data-Sources/Data/fips_data.csv")
# --- Individual/fine-grained Data ---------------------------------------------
fire_dept <- dbReadTable(conn, "fire_department_census") %>%
  mutate(zip5 = str_sub(hq_zip, 1, 5) %>% parse_number(),
         zip9 = str_remove_all(hq_zip, "\\D") %>% 
           parse_number()) %>%
  mutate(coords = str_remove(hq_location, " ?POINT ?"),
         lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
         long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>%
  select(-coords) %>%
  mutate(County = str_to_title(county)) %>%
  select(1:2, County, dept_type, organization_type, 
         number_stations:long) %>%
  remove_empty_cols() %>%
  fix_names() %>%
  mutate(Firefighters = active_firefighters_career + 
           active_firefighters_volunteer + active_firefighters_paid_per_call)
#write.csv(fire_dept, "/Users/denisebradford/Documents/Data-Sources/Data/fire_dept.csv")

liquor_stores <- dbReadTable(conn, "liquor_stores") %>%
  fix_names %>%
  remove_empty_cols() %>%
  filter(state == "IA") %>%
  mutate(zip5 = str_sub(zipcode, 1, 5) %>% parse_number(),
         zip9 = str_remove_all(zipcode, "\\D") %>% parse_number() ) %>%
  select(-zipcode) %>%
  mutate(coords = str_remove(store_address, " ?POINT ?"),
         lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
         long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>% 
  select(-store_address, -coords)
#write.csv(liquor_stores, "/Users/denisebradford/Documents/Data-Sources/Data/liquor_stores.csv")

physical_cultural_geographic_features <- dbReadTable(conn, "physical_and_cultural_geographic_features") %>%
  fix_names %>%
  remove_empty_cols() %>%
  rename(County = county_name, State = state_alpha) %>%
  select(-date_created, -date_edited) %>%
  mutate(coords = str_remove(primary_point, " ?POINT ?"),
         lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
         long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>% 
  select(-primary_point, -coords)
#write.csv(physical_cultural_geographic_features, "/Users/denisebradford/Documents/Data-Sources/Data/physical_cultural_geographic_features.csv")

post_offices <- filter(physical_cultural_geographic_features, 
                       feature_class == "Post Office") %>%
  select(-feature_class) %>%
  filter(!is.na(lat) & !is.na(long))
#write.csv(post_offices, "/Users/denisebradford/Documents/Data-Sources/Data/post_offices.csv")

ems <- read_csv(national_data_files[str_detect(national_data_files, "Emergency_Medical")]) %>%
  fix_names() %>%
  remove_empty_cols() %>%
  filter(STATE == "IA") %>%
  select(State = STATE, County = COUNTY, FIPS = FIPS, zip5 = ZIP, NAICSDESCR, long = LONGITUDE, lat = LATITUDE, 
         phone_loc = PHONELOC, level = LEVEL_, specialty = SPECIALTY, license = EMSLICENSE, 
         Name = NAME, Owner = OWNER) %>%
  mutate(County = str_to_title(County))
#write.csv(ems, "/Users/denisebradford/Documents/Data-Sources/Data/ems.csv")

hospitals <- read_csv(national_data_files[str_detect(national_data_files, "Hospitals")]) %>%
  fix_names() %>%
  remove_empty_cols() %>%
  set_names(str_to_title(names(.))) %>%
  filter(State == "IA") %>%
  select(State, City, Zip, Type, Status, County, County_FIPS = Countyfips, Name, Address,
         lat = Latitude, long = Longitude, NAICSDESCR = Naics_desc, Source, 
         Sourcedate, Owner, Beds, Trauma, Helipad) %>%
  mutate(zip5 = parse_number(Zip), 
         County = str_to_title(County)) %>%
  mutate(hospital_beds = ifelse(Beds < 0, NA, Beds),
         hospital_trauma_level = str_extract(Trauma, "LEVEL [IVX]{1,}"))
#write.csv(hospitals, "/Users/denisebradford/Documents/Data-Sources/Data/hospitals.csv")

retirement_homes <- dbReadTable(conn, "registered_retirement_facilities") %>%
  fix_names %>%
  remove_empty_cols() %>%
  mutate(
    zip5 = mailing_zip %>% parse_number(),
    coords = str_remove(physical_location, " ?POINT ?"),
    lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
    long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()) %>%
  filter(str_detect(mailing_state, "IA")) %>%
  filter(!is.na(coords))
#write.csv(retirement_homes, "/Users/denisebradford/Documents/Data-Sources/Data/retirement_homes.csv")

assisted_living <- dbReadTable(conn, "assisted_living") %>%
  mutate(zip = str_extract(location_1_zip, "\\d{5,9}"),
         zip5 = str_sub(zip, 1, 5) %>% parse_number(),
         zip9 = str_sub(zip, 1, 9) %>% parse_number(),
         coords = str_remove(location_1, " ?POINT ?"),
         lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
         long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()) %>%
  select(-zip) 
#write.csv(assisted_living, "/Users/denisebradford/Documents/Data-Sources/Data/assisted_living.csv")


# --- Zip-level Data -----------------------------------------------------------
assisted_living_zip <- assisted_living %>%
  group_by(zip5) %>%
  summarize(occupancy = sum(occupancy, na.rm = T)) %>%
  ungroup() %>%
  remove_empty_cols() %>%
  fix_names %>%
  select(zip5, assisted_living_occupancy = occupancy)

#write.csv(assisted_living_zip, "/Users/denisebradford/Documents/Data-Sources/Data/assisted_living_zip.csv")

fire_dept_zip <- fire_dept %>%
  select(zip5, firefighters = Firefighters) %>%
  group_by(zip5) %>%
  summarize(firefighters = sum(firefighters, na.rm = T))
#write.csv(fire_dept_zip, "/Users/denisebradford/Documents/Data-Sources/Data/fire_dept_zip.csv")

liquor_shops_zip <- liquor_stores %>%
  select(store, zip5, state) %>%
  group_by(zip5, store) %>%
  group_by(zip5) %>%
  summarize(liquor_stores = n()) %>%
  ungroup()
#write.csv(liquor_shops_zip, "/Users/denisebradford/Documents/Data-Sources/Data/liquor_shops_zip.csv")

# saveRDS(liquor_shops_zip, "data/IowaGov/liquor_shops.rda")
#liquor_shops_zip <- readRDS("/Users/denisebradford/Documents/ShrinkSmartExploration/data/IowaGov/liquor_shops.rda")

#Need new data for this information
post_office_zip <- tibble(text = read_lines("/Users/denisebradford/Documents/ShrinkSmartExploration/data/Iowa_Post_Offices.txt", skip = 2)) %>%
  tidyr::extract(text, into = c("zip5", "post_office", "established", "discontinued"),
                 regex = "(\\d{5})?\\s{0,}([A-Z ]{1,}(?:.\\d{1,}.)?)\\t([0-9/]{0,10})\\t([0-9/]{0,10})") %>%
  mutate(zip5 = ifelse(is.na(zip5), lag(zip5, 1), zip5) %>% as.numeric()) %>%
  filter(is.na(discontinued) | discontinued == "") %>%
  group_by(zip5) %>%
  summarize(n_post_offices = n())
#write.csv(post_office_zip, "/Users/denisebradford/Documents/Data-Sources/Data/post_office_zip.csv")

retirement_home_zip <- retirement_homes %>%
  unique() %>%
  select(zip5) %>%
  group_by(zip5) %>% 
  count(name = "retirement_homes") 
#write.csv(retirement_home_zip, "/Users/denisebradford/Documents/Data-Sources/Data/retirement_home_zip.csv")

hospitals_zip <- hospitals %>%
  mutate(hospital_trauma_level = factor(hospital_trauma_level, 
                                        levels = paste("LEVEL",  c("I", "II", "III", "IV")), 
                                        ordered = T) %>% as.numeric(),
         hospital_trauma_level = ifelse(is.na(hospital_trauma_level), 5, hospital_trauma_level)) %>%
  group_by(zip5) %>%
  summarize(
    hospital_beds = sum(hospital_beds, na.rm = T),
    hospital_trauma_level = min(hospital_trauma_level, na.rm = T)
  )
#write.csv(hospitals_zip, "/Users/denisebradford/Documents/Data-Sources/Data/hospitals_zip.csv")

census2010_pop_zip <- read_csv("data/National_Data/pop-by-zip-code.csv") %>%
  select(zip5 = zip_code, pop_2010 = `y-2010`) %>%
  mutate(zip5 = as.numeric(zip5))

# --- City level Data ----------------------------------------------------------
ia_city_county_population <- dbReadTable(conn, "city_population_iowa_by_county_year") %>%
  select(fips, county, geo_name, year, estimate, primary_point) %>%
  mutate(year = ymd(year)) %>%
  mutate(partial = str_detect(geo_name, "\\(pt\\.\\)"),
         geo_name = str_remove(geo_name, " \\(pt\\.\\)") %>%
         str_to_title() %>% str_remove_all("[[:punct:]]") %>%
         str_replace_all(c("Grand Mounds" = "Grand Mound", "St " = "Saint ", 
                             "Lemars" = "Le Mars", "Ottuwma" = "Ottumwa", 
                             "Ft " = "Fort ", "Clearlake" = "Clear Lake", 
                             "Kellogg?" = "Kellogg", "Mt " = "Mount ", 
                             "MelcherDallas" = "Melcher-Dallas"))) %>%
  mutate(coords = str_remove(primary_point, " ?POINT ?"),
         lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
         long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()) %>%
  rename(population = estimate) %>%
  mutate(year = floor_date(year, unit = "year") %>% year()) %>%
  group_by(fips, county, geo_name, year, lat, long) %>%
  summarize(population = median(population, na.rm = T)) %>%
  ungroup()
#write.csv(ia_city_county_population, "/Users/denisebradford/Documents/Data-Sources/Data/ia_city_county_population.csv")


ia_city_population <- ia_city_county_population %>%
  group_by(geo_name, year, fips, lat, long) %>%
  summarize(population = sum(population, na.rm = T)) %>%
  ungroup() %>% 
  mutate(geo_name = str_to_title(geo_name) %>% str_remove_all("[[:punct:]]") %>%
           str_replace_all(c("Grand Mounds" = "Grand Mound", "St " = "Saint ", 
                             "Lemars" = "Le Mars", "Ottuwma" = "Ottumwa", 
                             "Ft " = "Fort ", "Clearlake" = "Clear Lake", 
                             "Kellogg?" = "Kellogg", "Mt " = "Mount ", 
                             "MelcherDallas" = "Melcher-Dallas", 
                             "Maharishi Vedic Cit?y?" = "Maharishi Vedic City",
                             "Dakota City.*" = "Dakota City",
                             "^Decatur$" = "Decatur City",
                             "Jewell$" = "Jewell Junction",
                             "Luverne" = "Lu Verne",
                             "Legrand" = "Le Grand",
                             "^Saint " = "St. ",
                             "Gillette? Grove" = "Gillett Grove")) %>%
           gsub("^Mc([a-z])", "Mc\\U\\1", x = ., perl = T))
#write.csv(ia_city_population, "/Users/denisebradford/Documents/Data-Sources/Data/ia_city_population.csv")


# liquor_shops_city <- liquor_sales %>%
#   mutate(CityFix = str_to_title(City) %>% str_remove_all("[[:punct:]]") %>%
#            str_replace_all(c("Grand Mounds" = "Grand Mound", "St " = "Saint ",
#                              "Lemars" = "Le Mars", "Ottuwma" = "Ottumwa",
#                              "Ft " = "Fort ", "Clearlake" = "Clear Lake",
#                              "Kellogg?" = "Kellogg", "Mt " = "Mount ",
#                              "MelcherDallas" = "Melcher-Dallas"))) %>%
#   select(Store.Number, City = CityFix, year) %>%
#   filter(year != 2019) %>%
#   group_by(City, Store.Number, year) %>%
#   summarize(sales = n()) %>%
#   group_by(City, year) %>%
#   summarize(liquor_stores = n(), sales = sum(sales)) %>%
#   summarize(liquor_stores = median(liquor_stores), liquor_sales = median(sales)) %>%
#   ungroup()
# saveRDS(liquor_shops_city, "data/IowaGov/liquor_shops_city.rda")
 liquor_shops_city <- readRDS("/Users/denisebradford/Documents/ShrinkSmartExploration/data/IowaGov/liquor_shops_city.rda")

# --- County level Data --------------------------------------------------------

child_abuse_county <- dbReadTable(conn, "child_abuse_occurrences") %>%
  mutate(
    County_FIPS = str_sub(fips_county_code, 3, -1),
    State_FIPS = str_sub(fips_county_code, 1, 2),
    coords = str_remove(geocoded_column, " ?POINT ?"),
    lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
    long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>%
  select(-fips_county_code, -geocoded_column) %>%
  select(-coords) %>%
  remove_empty_cols() %>%
  fix_names
 #write.csv(child_abuse_county, "/Users/denisebradford/Documents/Data-Sources/Data/child_abuse_county.csv")
 
child_abuse_county_age_group <- dbReadTable(conn, "child_abuse_victims") %>%
  mutate(
    County_FIPS = str_sub(fips_county_code, 3, -1),
    State_FIPS = str_sub(fips_county_code, 1, 2),
    coords = str_remove(geocoded_column, " ?POINT ?"),
    lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
    long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>%
  select(-coords) %>%
  remove_empty_cols() %>%
  fix_names
#write.csv(child_abuse_county_age_group, "/Users/denisebradford/Documents/Data-Sources/Data/child_abuse_county_age_group.csv")

child_welfare_assessments <- dbReadTable(conn, "child_welfare_assessments") %>%
  mutate(
    County_FIPS = str_sub(fips_county_code, 3, -1),
    State_FIPS = str_sub(fips_county_code, 1, 2),
    coords = str_remove(county_primary_point, " ?POINT ?"),
    lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
    long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>%
  select(-fips_county_code, -county_primary_point, -coords) %>%
  remove_empty_cols() %>%
  fix_names
#write.csv(child_welfare_assessments, "/Users/denisebradford/Documents/Data-Sources/Data/child_welfare_assessments.csv")

tanf_county <- dbReadTable(conn, "family_investment_program") %>%
  left_join(fips_data %>% filter(State_FIPS == "19") %>% 
              select(County_FIPS, county), by = c(county_name = "county")) %>%
  mutate(coords = str_remove(primary_county_coordinates, " ?POINT ?"),
         lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
         long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>%
  select(-starts_with("primary")) %>%
  mutate(month = mdy(month)) %>%
  select(-month, -coords) %>%
  fix_names %>%
  remove_empty_cols() %>%
  group_by(cy, county_name, county, service_area, 
           County_FIPS, lat, long) %>%
  summarize(avg_monthly_cases = median(cases, na.rm = T),
            avg_monthly_recipients = median(recipients, na.rm = T),
            avg_monthly_grants = median(grants, na.rm = T)) %>%
  ungroup()
#write.csv(tanf_county, "/Users/denisebradford/Documents/Data-Sources/Data/tanf_county.csv")

food_stamps_county <-  dbReadTable(conn, "food_assistance_program_statistics")  %>%
  left_join(fips_data %>% filter(State_FIPS == "19") %>% 
              select(County_FIPS, county), by = c(county_name = "county")) %>%
  mutate(coords = str_remove(primary_county_coordinates, " ?POINT ?"),
         lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
         long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>%
  select(-starts_with("primary")) %>%
  mutate(month = ymd(month)) %>%
  select(-month, -coords) %>%
  fix_names %>%
  remove_empty_cols() %>%
  group_by(cy, county_name, County_FIPS, service_area, lat, long) %>%
  summarize(avg_monthly_households = median(households, na.rm = T),
            avg_monthly_recipients = median(recipients, na.rm = T),
            avg_monthly_allotments = median(allotments, na.rm = T)) %>%
  ungroup()
#write.csv(food_stamps_county, "/Users/denisebradford/Documents/Data-Sources/Data/food_stamps_county.csv")

medicaid_payments_county <- dbReadTable(conn, "medicaid_payments_county") %>%
  fix_names() %>%
  remove_empty_cols() %>%
  mutate(county = str_to_title(county)) %>%
  mutate(county_fip = as.character(county_fip),
         county_FIPS = str_sub(county_fip, 3, -1),
         state_FIPS = str_sub(county_fip, 1, 2)) %>%
  select(-county_fip) %>%
  select(-county_primary_location, -matches("^prim")) %>%
  mutate(month = ymd(report_as_of_date)) %>%
  select(-report_as_of_date, -gnis_feature_id) %>%
  gather(medneedy_elig:medneedy_pmt,
         key = "var", value = "value") %>%
  complete(crossing(month, var, nesting(county, county_FIPS, state_FIPS)),
           fill = list(value = 0)) %>%
  spread(key = var, value = value) %>%
  mutate(year = year(month)) %>%
  select(-month) %>%
  group_by(year, county, county_FIPS, state_FIPS) %>%
  summarise_all(median, na.rm = T) %>%
  mutate(avg_reimbursement_per_eligible_person =
           medneedy_pmt/medneedy_elig)
#write.csv(medicaid_payments_county, "/Users/denisebradford/Documents/Data-Sources/Data/medicaid_payments_county.csv")

unemployment <- dbReadTable(conn, "unemployment_insurance_benefit_payments") %>%
  fix_names() %>%
  remove_empty_cols() %>%
  mutate(month = ymd(month_ending) %>% floor_date("day")) %>%
  rename(county_name = "county_name") %>%
  mutate(county_fip = as.character(county_fip),
         County_FIPS = str_sub(county_fip, 3, -1),
         State_FIPS = str_sub(county_fip, 1, 2)) %>%
  select(-county_fip) %>%
  rename(GNIS = gnis_feature_id) %>%
  select(month:GNIS) %>%
  mutate(year = year(month)) %>%
  gather(-c(month:county_name, year, GNIS), key = "var", value = "value") %>%
  group_by(year, county_name, GNIS, var) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  spread(var, value) %>%
  ungroup()
#write.csv(unemployment, "/Users/denisebradford/Documents/Data-Sources/Data/unemployment.csv")

sales_tax <- dbReadTable(conn, "quarterly_retail_sales_tax") %>%
  fix_names() %>%
  remove_empty_cols() %>%
  mutate(fips_county_code = as.character(fips_county_code),
         County_FIPS = str_sub(fips_county_code, 3, -1),
         State_FIPS = str_sub(fips_county_code, 1, 2)) %>%
  select(county_number = county_number, county, city, County_FIPS, State_FIPS, 
         fiscal_year, quarter_ending, 
         number_of_returns:taxable_sales) %>%
  mutate(quarter_ending = ymd(quarter_ending))
#write.csv(sales_tax, "/Users/denisebradford/Documents/Data-Sources/Data/sales_tax.csv")

# --- Tax-district level Data --------------------------------------------------

assessed_property_values <- dbReadTable(conn, "assessed_property_values") %>%
  mutate(county_fip = str_sub(county_fip, 3, -1),
         State_FIPS = str_sub(county_fip, 1, 2),
         Place_FIPS = str_sub(city_fip, 3, -1),
         coords = str_remove(tax_district_location, " ?POINT ?"),
         lat = str_extract(coords, "\\([\\d\\.]{1,}") %>% parse_number(),
         long = str_extract(coords, "-[\\d\\.]{1,}\\)") %>% parse_number()) %>%
  #select(-tax_district_location, -county_fip, -city_fip,
  #       -prim_lat_dec, -prim_long_dec) %>%
  #select(-coords) %>%
  remove_empty_cols() %>%
  fix_names
#write.csv(assessed_property_values, "/Users/denisebradford/Documents/Data-Sources/Data/assessed_property_values.csv")

# --- School-district level data -----------------------------------------------

ia_cities <- sf::read_sf("data/Geography/Inc_cities_Twp_2010/") %>%
  filter(str_detect(CLASSFP10, "C")) %>%
  mutate(geometry = st_transform(geometry, 4326)) %>%
  mutate(center = st_centroid(geometry))

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
  mutate(zip5 = str_extract(physical_location, "IA \\d{5}") %>%
         str_remove("IA ") %>% parse_number(),
         coords = str_extract(physical_location, "\\([\\d\\.]{1,}, -?[\\d\\.]{1,}\\)"),
         lat = str_extract(coords, "\\([\\d\\.]{1,}") %>% parse_number(),
         long = str_extract(coords, "-[\\d\\.]{1,}\\)") %>% parse_number()) %>%
  mutate(grade_start = ifelse(grade_start == "NULL", NA, grade_start) %>% as.numeric,
         grade_end = ifelse(grade_end == "NULL", NA, grade_end) %>% as.numeric) %>%
  mutate(grade_start = str_replace(grade_start, "P?K" , "0") %>% parse_number,
         grade_end = str_replace(grade_end, "P?K" , "0") %>% parse_number,
         public = district_name != "Private") %>%
  filter(!is.na(grade_start) & !is.na(grade_end)) %>%
  mutate(type = purrr::map2(grade_start, grade_end, school_cat)) %>%
  unnest() %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  #st_set_crs(4326) %>% 
  #st_transform(3857)
  st_transform(crs = 4326)

school_sm <- schools %>%
   select(School.ID, public, type, geometry)
# 
# ia_city_schools <- ia_cities %>%
#   select(City = NAME10, center) %>%
#   st_drop_geometry() %>%
#   crossing(school_sm) %>%
#   select(City, center, School.ID, public, type, geometry) %>%
#   mutate(dist = st_distance(center, geometry, by_element = T)) %>% ## how to get units
#   group_by(City, public, type) %>%
#   filter(dist == min(dist)) %>%
#   ungroup()
# 
# ia_city_schools <- ia_city_schools %>%
#   modify_at("dist", units::set_units, "mi")
# 
# saveRDS(ia_city_schools, file = "data/IowaGov/city_school_distance.rda")
# ia_city_schools <- readRDS("data/IowaGov/city_school_distance.rda")
# 
# ia_city_school_dist <- ia_city_schools %>%
#   select(-geometry, -School.ID) %>%
#   mutate(key = sprintf("dist_%s_%s", c("private", "public")[public+1], type)) %>%
#   select(-public, -type) %>%
#   unique() %>%
#   tidyr::spread(key = key, value = dist) %>%
#   select(-center)
# 
# rm(school_sm, school_cat)

school_revenue_year <- dbReadTable(conn, "school_district_revenues") %>%
  fix_names() %>%
  remove_empty_cols() %>%
  filter(amount > 0) %>%
  group_by(fiscalyear, aea, dist, de_district, district_name) %>%
  summarize(amount = sum(amount), revenues_per_pupil = sum(revenues_per_pupil))
#write.csv(school_revenue_year, "/Users/denisebradford/Documents/Data-Sources/Data/school_revenue_year.csv")

# --- Cleaning up --------------------------------------------------------------

save(fire_dept, physical_cultural_geographic_features, 
     post_offices, retirement_homes, 
     file = "data/Individual_Level_Data.Rdata")
rm(fire_dept, physical_cultural_geographic_features, 
   post_offices, retirement_homes)


save(assisted_living, child_care_zip, fire_dept_zip, 
     post_office_zip, retirement_home_zip, 
     file = "data/Zip_Level_Data.Rdata")

