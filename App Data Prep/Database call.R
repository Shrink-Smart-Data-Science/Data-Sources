library(tidyverse)
library(sf)
library(lubridate)
library(RMySQL)
library(DBI)
library(keyring)
library(pool)
# --- Initial Setup ------------------------------------------------------------
pool <- pool::dbPool(
  drv = RMySQL::MySQL(),
  dbname = "scc",
  host = "srvanderplas.com",
  username = "remote",
  password = keyring::key_get("MY_SECRET")
)


national_data_files <- list.files("Data/National_Data/", full.names = T)

remove_empty_cols <- function(x) {
  all_same_cols <- apply(x, 2, function(z) length(unique(z)))
  
  x %>% select(which(all_same_cols > 1))
}

fix_names <- function(x) {
  x %>%
    set_names(make.names(names(x)) %>% str_replace_all("\\.{1,}", ".") %>%
                str_remove("\\.$"))
}

ie <- function(a, b, c) {
  if(a) {
    b
  } else {
    c
  }
}

crs <- structure(list(epsg = 26915L, proj4string = "+proj=longlat +zone=15 +datum=NAD83 +units=mi +no_defs"), class = "crs")

fips_data_clean <- read.csv("Data/fips_codes.csv", skip = 4, header = TRUE) 
fips_data_clean$county <- gsub("([A-Za-z]+).*", "\\1", fips_data_clean$Area_Name_FIPS)

# --- Individual/fine-grained Data ---------------------------------------------
child_care_registered_clean <- read_csv("Data/child_care_data.csv", skip = 1) %>%
  remove_empty_cols() %>%
  fix_names %>%
  set_names(str_to_title(names(.))) %>%
  filter(Is.active.ccaprovider == "Yes") %>%
  mutate(zip5 = Provider.zip.code, 
         Community = str_to_title(Community),
         capacity = Provider.capacity,
         rating = Provider.qrs.rating)

fire_dept_clean <- dbReadTable(pool, "fire_department_census") %>%
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


physical_cultural_geographic_features_clean <- dbReadTable(pool, "physical_and_cultural_geographic_features") %>%
  fix_names %>%
  remove_empty_cols() %>%
  rename(County = county_name, State = state_alpha) %>%
  select(-date_created, -date_edited) %>%
  mutate(coords = str_remove(primary_point, " ?POINT ?"),
         lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
         long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>%
  select(-primary_point, -coords)

ems_clean <- read_csv(national_data_files[str_detect(national_data_files, "Emergency_Medical")]) %>%
  fix_names() %>%
  remove_empty_cols() %>%
  filter(STATE == "IA") %>%
  select(State = STATE, County = COUNTY, FIPS = FIPS, zip5 = ZIP, NAICSDESCR, long = LONGITUDE, lat = LATITUDE,
         phone_loc = PHONELOC, level = LEVEL_, specialty = SPECIALTY, license = EMSLICENSE,
         Name = NAME, Owner = OWNER) %>%
  mutate(County = str_to_title(County))

hospitals_clean <- read_csv(national_data_files[str_detect(national_data_files, "Hospitals")]) %>%
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

retirement_homes_clean <- dbReadTable(pool, "registered_retirement_facilities") %>%
  fix_names %>%
  remove_empty_cols() %>%
  mutate(
    zip5 = mailing_zip %>% parse_number(),
    coords = str_remove(physical_location, " ?POINT ?"),
    lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
    long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()) %>%
  filter(str_detect(mailing_state, "IA")) %>%
  filter(!is.na(coords))

assisted_living_clean <- dbReadTable(pool, "assisted_living") %>%
  mutate(zip = str_extract(location_1_zip, "\\d{5,9}"),
         zip5 = str_sub(zip, 1, 5) %>% parse_number(),
         zip9 = str_sub(zip, 1, 9) %>% parse_number(),
         coords = str_remove(location_1, " ?POINT ?"),
         lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
         long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()) %>%
  select(-zip)

# --- City level Data ----------------------------------------------------------
ia_city_county_population_clean <- dbReadTable(pool, "city_population_iowa_by_county_year") %>%
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


ia_city_population_clean <- ia_city_county_population_clean %>%
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


# --- County level Data --------------------------------------------------------

child_abuse_county_clean <- dbReadTable(pool, "child_abuse_occurrences") %>%
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

child_abuse_county_age_group_clean <- dbReadTable(pool, "child_abuse_victims") %>%
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

child_welfare_assessments_clean <- dbReadTable(pool, "child_welfare_assessments") %>%
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

tanf_county_clean <- dbReadTable(pool, "family_investment_program") %>%
  left_join(fips_data_clean %>% filter(State_FIPS == "19") %>%
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

food_stamps_county_clean <-  dbReadTable(pool, "food_assistance_program_statistics")  %>%
  left_join(fips_data_clean %>% filter(State_FIPS == "19") %>%
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

medicaid_payments_county_clean <- dbReadTable(pool, "medicaid_payments_county") %>%
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

unemployment_insurance_payments_clean <- dbReadTable(pool, "unemployment_insurance_benefit_payments") %>%
  fix_names() %>%
  remove_empty_cols() %>%
  mutate(month = ymd(month_ending) %>% floor_date("day")) %>%
  mutate(county_fip = as.character(county_fip),
         County_FIPS = str_sub(county_fip, 3, -1),
         State_FIPS = str_sub(county_fip, 1, 2)) %>%
  select(-county_fip) %>%
  select(month_ending:gnis_feature_id) %>%
  mutate(year = year(month_ending)) %>%
  gather(-c(month_ending:county_name, year, gnis_feature_id), key = "var", value = "value") %>%
  group_by(year, county_name, gnis_feature_id, var) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  spread(var, value) %>%
  ungroup()

sales_tax_clean <- dbReadTable(pool, "quarterly_retail_sales_tax") %>%
  fix_names() %>%
  remove_empty_cols() %>%
  mutate(fips_county_code = as.character(fips_county_code),
         County_FIPS = str_sub(fips_county_code, 3, -1),
         State_FIPS = str_sub(fips_county_code, 1, 2)) %>%
  select(county_number = county_number, county, city, County_FIPS, State_FIPS,
         fiscal_year, quarter_ending,
         number_of_returns:taxable_sales) %>%
  mutate(quarter_ending = ymd(quarter_ending))

# --- Tax-district level Data --------------------------------------------------

assessed_property_values_clean <- dbReadTable(pool, "assessed_property_values") %>%
  mutate(county_fip = str_sub(county_fip, 3, -1),
         State_FIPS = str_sub(county_fip, 1, 2),
         Place_FIPS = str_sub(city_fip, 3, -1),
         coords = str_remove(tax_district_location, " ?POINT ?"),
         lat = str_extract(coords, "\\([\\d\\.]{1,}") %>% parse_number(),
         long = str_extract(coords, "-[\\d\\.]{1,}\\)") %>% parse_number()) %>%
  remove_empty_cols() %>%
  fix_names

# --- School-district level data -----------------------------------------------

center_points <- read_csv("Data/place_centroids.csv") 

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

schools_clean <- dbReadTable(pool, "school_building_directory") %>%
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

school_sm <- schools_clean %>%
  select(geometry) %>%
  st_cast("MULTIPOINT") %>%
  st_cast("POINT") %>% 
  st_coordinates() %>%
  as.data.frame() %>% 
  mutate(long = X, lat = Y) %>%
  select(long, lat) %>%
  tidyr::crossing(schools_clean) %>% 
  select(district_school_id, public, type, long, lat)

ia_city_schools_clean <- center_points %>%
  select(City = name, lat, lon) %>%
  #st_drop_geometry() %>%
  tidyr::crossing(school_sm) %>%
  select(City, center, district_school_id, public, type, geometry) %>%
  mutate(dist = st_distance(center, geometry, by_element = T)) %>% ## how to get units
  group_by(City, public, type) %>%
  filter(dist == min(dist)) %>%
  ungroup()
#
ia_city_schools <- ia_city_schools %>%
  modify_at("dist", units::set_units, "mi")
#
# saveRDS(ia_city_schools, file = "data/IowaGov/city_school_distance.rda")
# ia_city_schools <- readRDS("data/IowaGov/city_school_distance.rda")
#
ia_city_school_dist <- ia_city_schools %>%
  select(-center, -School.ID) %>%
  mutate(key = sprintf("dist_%s_%s", c("private", "public")[public+1], type)) %>%
  select(-public, -type) %>%
  unique() %>%
  tidyr::spread(key = key, value = dist) %>%
  select(-center)
#
rm(school_sm, school_cat)

school_revenue_year_clean <- dbReadTable(pool, "school_district_revenues") %>%
  fix_names() %>%
  remove_empty_cols() %>%
  filter(amount > 0) %>%
  group_by(fiscalyear, aea, dist, de_district, district_name) %>%
  summarize(amount = sum(amount), revenues_per_pupil = sum(revenues_per_pupil))

# --- Cleaning up --------------------------------------------------------------
# 
# save(fire_dept, physical_cultural_geographic_features,
#      post_offices, retirement_homes,
#      file = "Data/Individual_Level_Data.Rdata")
# rm(fire_dept, physical_cultural_geographic_features,
#    post_offices, retirement_homes)
# 
# 
# save(assisted_living, fire_dept_zip,
#      post_office_zip, retirement_home_zip,
#      file = "Data/Zip_Level_Data.Rdata") #child_care_zip,

clean_data_sources = c()

clean_data_sources <- tibble::tribble(
  ~name, ~data.frame.name,
  "assisted_living_clean", assisted_living_clean,
  "assessed_property_values_clean", assessed_property_values_clean,
  "child_abuse_county_clean", child_abuse_county_clean,
  "child_abuse_county_age_group_clean", child_abuse_county_age_group_clean,
  "child_care_registered_clean", child_care_registered_clean,
  "child_welfare_assessments_clean", child_welfare_assessments_clean,
  "ems_clean", ems_clean,
  "fips_data_clean", fips_data_clean,
  "fire_dept_clean", fire_dept_clean,
  "food_stamps_county_clean", food_stamps_county_clean,
  "hospitals_clean", hospitals_clean,
  "medicaid_payments_county_clean", medicaid_payments_county_clean,
  "physical_and_cultural_geographic_features_clean", physical_cultural_geographic_features_clean,
  "retirement_homes_clean", retirement_homes_clean,
  "sales_tax_clean", sales_tax_clean,
  "school_revenue_year_clean", school_revenue_year_clean,
  "schools_clean", schools_clean,
  "tanf_county_clean", tanf_county_clean,
  "unemployment_insurance_payments_clean", unemployment_insurance_payments_clean
)


for (i in seq_along(clean_data_sources$name)){
  # data_sources <- data_sources %>% mutate(data = purrr::map(url[[i]], read.socrata))
  # data_name = gsub(" ", "_", data_sources$name[[i]])
  
  dbWriteTable(conn = pool,
               name = clean_data_sources$name[[i]],
               value = clean_data_sources$data.frame.name[[i]],
               row.names=FALSE,
               overwrite = TRUE) 
}
