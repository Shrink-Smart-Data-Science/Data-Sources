## Let's combine the datasets based on the topics related to QoL
library(SmartEDA)
library(DBI)
library(RMySQL)
library(tidyverse)
library(keyring)
library(DT)
library(lubridate)

#To-Do for the password call to the database make sure to create a Github permission key with my database password
# pool <- pool::dbPool(
#   drv = RMySQL::MySQL(),
#   dbname = "scc",
#   host = "srvanderplas.com",
#   username = "remote",
#   password = keyring::key_get("MY_SECRET")
# )


conn <- DBI::dbConnect(RMySQL::MySQL(), 
                        host = "srvanderplas.com",
                        dbname = "scc",
                        user = "remote",
                        password = keyring::key_get("MY_SECRET"))

remove_empty_cols <- function(x) {
  all_same_cols <- apply(x, 2, function(z) length(unique(z)))
  
  x %>% select(which(all_same_cols > 1))
}

fix_names <- function(x) {
  x %>%
    set_names(make.names(names(x)) %>% str_replace_all("\\.{1,}", ".") %>%
                str_remove("\\.$"))
}

#### Start with Employment related datasets ####
unemployment.ins.payments <- dbReadTable(conn, "unemployment_insurance_benefit_payments") %>%
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

#Data source has no identifiable information to a county or aa zip
# unemployment.benefits <- dbReadTable(conn, "unemployment_compensation_fund_status_benefits") %>%
#   fix_names() %>%
#   remove_empty_cols() 

#To-Do only taake on the current years in the datasets here.
zbp_clean <- dbReadTable(conn, "zbp_clean") %>% 
  mutate(zip5 = as.numeric(GEOID_Z))

key.place.data <- dbReadTable(conn, "keys_zip_place")

zbp.city.zip <- zbp_clean %>% left_join(key.place.data, by = c("GEOID_Z","year"))

#### Start with Housing related datasets ####
home.values <- dbReadTable(conn, "homeValues_zillow_clean")

home.values.zip <- home.values %>% 
  left_join(key.place.data %>% select(year, GEOID_P,GEOID_Z), by = c("year","GEOID_P"))

acs.data <- dbReadTable(conn, "acs_clean")

acs.data.zip <- acs.data %>% 
  left_join(key.place.data %>% select(year, GEOID_P,GEOID_Z), by = c("year","GEOID_P"))

#### Start with Education related datasets ####
key.schools <- dbReadTable(conn, "keys_school")
area.school.place <- dbReadTable(conn, "area_schoolPlace")
childcare.facilities <- dbReadTable(conn, "childcare_facilities_clean")

#### Start with Envirnoment related datasets ####
geo.features <- dbReadTable(conn, "physical_and_cultural_geographic_features")
geo.features <- geo.features %>% filter(feature_class %in% c("Park", "Trail", "Church","Airport") )
