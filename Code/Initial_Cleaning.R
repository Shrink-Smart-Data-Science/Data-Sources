library(tidyverse)
library(sf)
library(lubridate)
library(RMySQL)
library(DBI)
library(keyring)

# --- Initial Setup ------------------------------------------------------------
conn <- DBI::dbConnect(RMySQL::MySQL(), 
                       host = "srvanderplas.com",
                       dbname = "scc",
                       user = "remote",
                       password = keyring::key_get("MY_SECRET"))

dbListTables(conn) 

remove_empty_cols <- function(x) {
  all_same_cols <- apply(x, 2, function(z) length(unique(z)))
  
  x %>% select(which(all_same_cols > 1))
}

fix_names <- function(x) {
  x %>%
    set_names(make.names(names(x)) %>% str_replace_all("\\.{1,}", ".") %>% 
                str_remove("\\.$"))
}
# --- Individual/fine-grained Data ---------------------------------------------

# campaign_contributions <- read_csv(state_data_files[str_detect(state_data_files, "Campaign_Contributions")]) %>%
#   mutate(
#     zip5 = str_sub(`Zip Code`, 1, 5) %>% parse_number(na = "N/A"),
#     zip9 = str_remove_all(`Zip Code`, "\\D") %>% parse_number(na = ""),
#     year = str_extract(Date, "\\d{4}") %>% parse_number()
#   ) %>%
#   remove_empty_cols() %>%
#   fix_names

# child_care_registered <- readxl::read_xls(state_data_files[str_detect(state_data_files, "child_care")]) %>%
#   remove_empty_cols() %>%
#   fix_names %>%
#   set_names(str_to_title(names(.))) %>%
#   filter(Is.active.ccaprovider == "Yes") %>%
#   mutate(zip5 = Provider.zip.code, 
#          Community = str_to_title(Community),
#          capacity = parse_number(Provider.capacity),
#          rating = Provider.qrs.rating)


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

# liquor_sales <- read_csv(state_data_files[str_detect(state_data_files, "Liquor_Sales")]) %>%
#   fix_names %>%
#   remove_empty_cols() %>%
#   mutate(zip5 = str_sub(Zip.Code, 1, 5) %>% parse_number(),
#          zip9 = str_remove_all(Zip.Code, "\\D") %>% parse_number(),
#          year = str_extract(Date, "\\d{4}") %>% parse_number()) %>%
#   select(-Zip.Code) %>%
#   mutate(coords = str_remove(Store.Location, " ?POINT ?"),
#          lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
#          long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
#   )

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

post_offices <- filter(physical_cultural_geographic_features, 
                       feature_class == "Post Office") %>%
  select(-feature_class) %>%
  filter(!is.na(lat) & !is.na(long))

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


ems <- read_csv(national_data_files[str_detect(national_data_files, "Emergency_Medical")]) %>%
  fix_names() %>%
  remove_empty_cols() %>%
  filter(STATE == "IA") %>%
  select(State = STATE, County = COUNTY, FIPS = FIPS, zip5 = ZIP, NAICSDESCR, long = LONGITUDE, lat = LATITUDE, 
         phone_loc = PHONELOC, level = LEVEL_, specialty = SPECIALTY, license = EMSLICENSE, 
         Name = NAME, Owner = OWNER) %>%
  mutate(County = str_to_title(County))

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

assisted_living <- read_csv(state_data_files[str_detect(state_data_files, "Assisted_Living")]) %>%
  mutate(zip = str_extract(`Location 1`, "\\d{5,9}"),
         zip5 = str_sub(zip, 1, 5) %>% parse_number(),
         zip9 = str_sub(zip, 1, 9) %>% parse_number(),
         coords = str_extract(`Location 1`, "\\([\\d\\.]{1,}, -[\\d\\.]{1,}\\)"),
         lat = str_extract(coords, "\\([\\d\\.]{1,}") %>% parse_number(),
         long = str_extract(coords, "-[\\d\\.]{1,}\\)") %>% parse_number()) %>%
  select(-zip) 



# --- Zip-level Data -----------------------------------------------------------
assisted_living_zip <- assisted_living %>%
  group_by(zip5) %>%
  summarize(Occupancy = sum(Occupancy, na.rm = T)) %>%
  ungroup() %>%
  remove_empty_cols() %>%
  fix_names %>%
  select(zip5, assisted_living_occupancy = Occupancy)


child_care_zip <- child_care_registered %>%
  group_by(zip5) %>%
  summarize(slots = sum(capacity, na.rm = T),
            avg.rating = median(rating[rating != 0], weight = slots[rating != 0])) %>%
  modify_at("avg.rating", ~ifelse(is.nan(.), 0, .)) %>%
  modify_at("zip5", parse_number) %>%
  ungroup() %>%
  select(zip5, childcare_slots = slots, childcare_avg_rating = avg.rating)

fire_dept_zip <- fire_dept %>%
  select(zip5, firefighters = Firefighters) %>%
  group_by(zip5) %>%
  summarize(firefighters = sum(firefighters, na.rm = T))

# liquor_shops_zip <- liquor_sales %>%
#   select(Store.Number, zip5, year) %>%
#   filter(year != 2019) %>%
#   group_by(zip5, Store.Number, year) %>%
#   summarize(sales = n()) %>%
#   group_by(zip5, year) %>%
#   summarize(liquor_stores = n(), sales = sum(sales)) %>%
#   summarize(liquor_stores = median(liquor_stores), liquor_sales_annual = median(sales)) %>%
#   ungroup()
# saveRDS(liquor_shops_zip, "data/IowaGov/liquor_shops.rda")
liquor_shops_zip <- readRDS("data/IowaGov/liquor_shops.rda")


post_office_zip <- tibble(text = read_lines("data/Iowa_Post_Offices.txt", skip = 2)) %>%
  tidyr::extract(text, into = c("zip5", "post_office", "established", "discontinued"),
                 regex = "(\\d{5})?\\s{0,}([A-Z ]{1,}(?:.\\d{1,}.)?)\\t([0-9/]{0,10})\\t([0-9/]{0,10})") %>%
  mutate(zip5 = ifelse(is.na(zip5), lag(zip5, 1), zip5) %>% as.numeric()) %>%
  filter(is.na(discontinued) | discontinued == "") %>%
  group_by(zip5) %>%
  summarize(n_post_offices = n())

retirement_home_zip <- retirement_homes %>%
  unique() %>%
  select(zip5) %>%
  group_by(zip5) %>% 
  count(name = "retirement_homes") 

ems_zip <- ems %>%
  group_by(zip5) %>%
  count(name = "ems_organizations") %>%
  ungroup()

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

census2010_pop_zip <- read_csv("data/National_Data/pop-by-zip-code.csv") %>%
  select(zip5 = zip_code, pop_2010 = `y-2010`) %>%
  mutate(zip5 = as.numeric(zip5))

# --- City level Data ----------------------------------------------------------

ia_city_county_population <- read_csv("data/City_Population_in_Iowa_by_County_and_Year.csv") %>%
  select(FIPS, County, City, Year, Estimate) %>%
  mutate(Year = mdy(Year)) %>%
  mutate(partial = str_detect(City, "\\(pt\\.\\)"),
         City = str_remove(City, " \\(pt\\.\\)") %>%
           str_to_title() %>% str_remove_all("[[:punct:]]") %>%
           str_replace_all(c("Grand Mounds" = "Grand Mound", "St " = "Saint ", 
                             "Lemars" = "Le Mars", "Ottuwma" = "Ottumwa", 
                             "Ft " = "Fort ", "Clearlake" = "Clear Lake", 
                             "Kellogg?" = "Kellogg", "Mt " = "Mount ", 
                             "MelcherDallas" = "Melcher-Dallas"))) %>%
  rename(Population = Estimate) %>%
  mutate(Year = floor_date(Year, unit = "year") %>% year()) %>%
  group_by(FIPS, County, City, Year) %>%
  summarize(Population = median(Population, na.rm = T)) %>%
  ungroup()

ia_city_population <- ia_city_county_population %>%
  group_by(City, Year, FIPS) %>%
  summarize(Population = sum(Population, na.rm = T)) %>%
  ungroup() %>% 
  mutate(City = str_to_title(City) %>% str_remove_all("[[:punct:]]") %>%
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

fiscal_capacity <- readxl::read_excel("data/Public_Finance/City County Fiscal Capacity and Effort Indexes.xlsx") %>%
  mutate(size = ifelse(str_detect(City, "[A-z ]{1,}, >?[\\d,]{1,}"), City, NA), 
         size = size[which(!is.na(size))][cumsum(!is.na(size))]) %>%
  filter(!City == size) %>%
  filter(!City == "City") %>%
  select(City, Population, size, everything()) %>%
  fix_names() %>% 
  mutate_at(4:10, parse_number) %>%
  select(City, Population, size, Fiscal.Capacity, Fiscal.Effort) %>%
  mutate(City = str_to_title(City) %>% str_remove_all("[[:punct:]]") %>%
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
           gsub("^Mc([a-z])", "Mc\\U\\1", x = ., perl = T)) %>%
  filter(!str_detect(City, "Totals?|(Rural Under.*)"))

city_budget_all <- readxl::read_excel("data/Public_Finance/FY2014_AFR_TOTAL.xlsx", skip = 2) 

city_budget_names <- make.names(city_budget_all$REVENUES, unique = T) %>%
  str_replace_all("\\.{1,}", ".") %>% 
  str_remove("\\.$")

cities <- names(city_budget_all)[-c(1:2)] %>%
  str_to_title()
city_budget_all <- city_budget_all %>%
  select(-c(1:2)) %>%
  t() %>%
  as_tibble() %>%
  set_names(city_budget_names) %>%
  mutate(City = cities)

rm(city_budget_names, cities)

city_budget <- city_budget_all %>%
  select(City,
         Total.property.tax, Other.local.option.taxes, Mobile.home.tax, 
         TOTAL.INTERGOVERNMENTAL.Sum.of.lines.33.60.and.70, Road.use.taxes, 
         Water:Gas, Landfill.garbage, Cable.TV:Telephone, Storm.water, 
         Fire.service.charges:Ambulance.charges,
         Library.charges:Animal.control.charges,
         Contributions,
         Proceeds.of.long.term.debt.Excluding.TIF.internal.borrowing,
         Beginning.fund.balance.July.1.2012,
         TOTAL.PUBLIC.WORKS, 
         TOTAL.HEALTH.AND.SOCIAL.SERVICES,
         TOTAL.CULTURE.AND.RECREATION,
         TOTAL.COMMUNITY.AND.ECONOMIC.DEVELOPMENT,
         TOTAL.GENERAL.GOVERNMENT,
         TOTAL.DEBT.SERVICE,
         matches("Current.operation")) %>%
  rename(rev_property_tax = Total.property.tax,
         rev_local_sales_tax = Other.local.option.taxes,
         rev_mobile_home_tax = Mobile.home.tax,
         rev_road_use_tax = Road.use.taxes,
         rev_water = Water,
         rev_sewer = Sewer,
         rev_electric = Electric, 
         rev_gas = Gas, 
         rev_garbage = Landfill.garbage,
         rev_cable = Cable.TV,
         rev_internet = Internet,
         rev_telephone = Telephone,
         rev_stormwater = Storm.water,
         rev_fire = Fire.service.charges,
         rev_ambulance = Ambulance.charges,
         rev_library = Library.charges,
         rev_parkrec = Park.recreation.and.cultural.charges,
         rev_animal_control = Animal.control.charges,
         rev_contributions = Contributions,
         rev_debt = Proceeds.of.long.term.debt.Excluding.TIF.internal.borrowing,
         savings = Beginning.fund.balance.July.1.2012,
         exp_police = Police.department.Crime.prevention.Current.operation,
         exp_emerg = Emergency.management.Current.operation,
         exp_flood = Flood.control.Current.operation,
         exp_fire = Fire.department.Current.operation,
         exp_ambulance = Ambulance.Current.operation,
         exp_build_insp = Building.inspections.Current.operation,
         exp_animal_control = Animal.control.Current.operation,
         exp_roads = Roads.bridges.sidewalks.Current.operation,
         exp_street_lights = Street.lighting.Current.operation,
         exp_snow_removal = Snow.removal.Current.operation,
         exp_garbage = Garbage.if.not.an.enterprise.Current.operation,
         exp_water = Water.Current.operation,
         exp_sewer = Sewer.and.sewage.disposal.Current.operation,
         exp_electric = Electric.Current.operation,
         exp_gas = Gas.Utility.Current.operation,
         exp_garbage2 = Landfill.Garbage.Current.operation,
         exp_telecom = Cable.TV.telephone.Internet.Current.operation,
         exp_stormwater = Storm.water.Current.operation,
         exp_pub_works = TOTAL.PUBLIC.WORKS,
         exp_health_social = TOTAL.HEALTH.AND.SOCIAL.SERVICES,
         exp_parkrec = TOTAL.CULTURE.AND.RECREATION,
         exp_econ_dev = TOTAL.COMMUNITY.AND.ECONOMIC.DEVELOPMENT,
         exp_govt = TOTAL.GENERAL.GOVERNMENT,
         exp_debt = TOTAL.DEBT.SERVICE) %>%
  mutate(rev_intergov_noroad = 
           TOTAL.INTERGOVERNMENTAL.Sum.of.lines.33.60.and.70 - rev_road_use_tax,
         rev_telecom = rev_cable + rev_internet + rev_telephone) %>%
  select(-TOTAL.INTERGOVERNMENTAL.Sum.of.lines.33.60.and.70, -c(rev_cable:rev_telephone)) %>%
  select(City, starts_with("rev"), savings, starts_with("exp")) %>%
  mutate(City = str_to_title(City) %>% str_remove_all("[[:punct:]]") %>%
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
                             "^Saint " = "St. ",
                             "Legrand" = "Le Grand",
                             "Gillette? Grove" = "Gillett Grove")) %>%
           gsub("^Mc([a-z])", "Mc\\U\\1", x = ., perl = T))

liquor_shops_city <- liquor_sales %>%
  mutate(CityFix = str_to_title(City) %>% str_remove_all("[[:punct:]]") %>%
           str_replace_all(c("Grand Mounds" = "Grand Mound", "St " = "Saint ",
                             "Lemars" = "Le Mars", "Ottuwma" = "Ottumwa",
                             "Ft " = "Fort ", "Clearlake" = "Clear Lake",
                             "Kellogg?" = "Kellogg", "Mt " = "Mount ",
                             "MelcherDallas" = "Melcher-Dallas"))) %>%
  select(Store.Number, City = CityFix, year) %>%
  filter(year != 2019) %>%
  group_by(City, Store.Number, year) %>%
  summarize(sales = n()) %>%
  group_by(City, year) %>%
  summarize(liquor_stores = n(), sales = sum(sales)) %>%
  summarize(liquor_stores = median(liquor_stores), liquor_sales = median(sales)) %>%
  ungroup()
saveRDS(liquor_shops_city, "data/IowaGov/liquor_shops_city.rda")
liquor_shops_city <- readRDS("data/IowaGov/liquor_shops_city.rda")


# campaign_contributions_city <- campaign_contributions %>%
#   mutate(City = str_remove_all(City, "[[:punct:][:digit:]]") %>%
#            str_remove_all("[`<\\$]") %>%
#            str_remove_all(" Iowa$") %>%
#            str_remove_all(" [A-Z]{2}$") %>%
#            str_replace("Des Moines.*", "Des Moines") %>%
#            str_replace_all("\\s{1,}", " ") %>%
#            str_trim() %>%
#            str_to_title() %>%
#            str_replace("^Aa", "A"),
#          City = ifelse(str_detect(City, "[A-z]{3,}"), City, NA)) %>%
#   group_by(State, City, year) %>%
#   summarize(campaign_contrib_amt = sum(Contribution.Amount, na.rm = T))
# saveRDS(campaign_contributions_zip_city, "data/IowaGov/Campaign_Contrib_City.rda")
# campaign_contributions_city <- readRDS("data/IowaGov/Campaign_Contrib_City.rda")

# --- County level Data --------------------------------------------------------

child_abuse_county <- read_csv(state_data_files[str_detect(state_data_files, "Type_of_Abuse")]) %>%
  mutate(
    County_FIPS = str_sub(`FIPS County Code`, 3, -1),
    State_FIPS = str_sub(`FIPS County Code`, 1, 2),
    coords = str_remove(`County Primary Point`, " ?POINT ?"),
    lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
    long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>%
  select(-`FIPS County Code`, -`County Primary Point`) %>%
  select(-coords) %>%
  remove_empty_cols() %>%
  fix_names

child_abuse_county_age_group <- read_csv(state_data_files[str_detect(state_data_files, "Age_Group")]) %>%
  mutate(
    County_FIPS = str_sub(`FIPS County Code`, 3, -1),
    State_FIPS = str_sub(`FIPS County Code`, 1, 2),
    coords = str_remove(`County Primary Point`, " ?POINT ?"),
    lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
    long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>%
  select(-coords) %>%
  remove_empty_cols() %>%
  fix_names

child_welfare_assessments <- read_csv(state_data_files[str_detect(state_data_files, "Child_Welfare")]) %>%
  mutate(
    County_FIPS = str_sub(`FIPS County Code`, 3, -1),
    State_FIPS = str_sub(`FIPS County Code`, 1, 2),
    coords = str_remove(`County Primary Point`, " ?POINT ?"),
    lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
    long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>%
  select(-`FIPS County Code`, -`County Primary Point`, -coords) %>%
  remove_empty_cols() %>%
  fix_names

tanf_county <- read_csv(state_data_files[str_detect(state_data_files, "Family_Investment")]) %>%
  left_join(fips_data %>% filter(State_FIPS == "19") %>% 
              select(County_FIPS, County), by = c(`County Name` = "County")) %>%
  mutate(coords = str_remove(`Primary County Coordinates`, " ?POINT ?"),
         lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
         long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>%
  select(-starts_with("Primary")) %>%
  mutate(month = mdy(Month)) %>%
  select(-Month, -coords) %>%
  fix_names %>%
  remove_empty_cols() %>%
  group_by(CY, County.Name, County, Service.Area, Iowa.ZIP.Code.Tabulation.Areas, 
           Iowa.Watershed.Sub.Basins.HUC.08, Iowa.Watersheds.HUC.10, US.Counties, 
           County_FIPS, lat, long) %>%
  summarize(avg_monthly_cases = median(Cases, na.rm = T),
            avg_monthly_recipients = median(Recipients, na.rm = T),
            avg_monthly_grants = median(Grants, na.rm = T)) %>%
  ungroup()

food_stamps_county <-  read_csv(state_data_files[str_detect(state_data_files, "Food_Assistance")]) %>%
  left_join(fips_data %>% filter(State_FIPS == "19") %>% 
              select(County_FIPS, County), by = c(`County Name` = "County")) %>%
  mutate(coords = str_remove(`Primary County Coordinates`, " ?POINT ?"),
         lat = str_extract(coords, "[\\d\\.]{1,}\\)") %>% parse_number(),
         long = str_extract(coords, "\\(-[\\d\\.]{1,}") %>% parse_number()
  ) %>%
  select(-starts_with("Primary")) %>%
  mutate(month = mdy(Month)) %>%
  select(-Month, -coords) %>%
  fix_names %>%
  remove_empty_cols() %>%
  group_by(CY, County.Name, County_FIPS, Service.Area, 
           Iowa.ZIP.Code.Tabulation.Areas, Iowa.Watershed.Sub.Basins.HUC.08, 
           Iowa.Watersheds.HUC.10, US.Counties, lat, long) %>%
  summarize(avg_monthly_households = median(Households, na.rm = T),
            avg_monthly_recipients = median(Recipients, na.rm = T),
            avg_monthly_allotments = median(Allotments, na.rm = T)) %>%
  ungroup()


# medicaid_payments_county <- read_csv(state_data_files[str_detect(state_data_files, "Medicaid_Payments")]) %>%
#   fix_names() %>%
#   remove_empty_cols() %>%
#   mutate(County = str_to_title(County)) %>%
#   mutate(County.FIP = as.character(County.FIP),
#          County_FIPS = str_sub(County.FIP, 3, -1),
#          State_FIPS = str_sub(County.FIP, 1, 2)) %>%
#   select(-County.FIP) %>%
#   select(-County.Primary.Location, -matches("^Primary")) %>%
#   mutate(month = mdy(Report.Date)) %>%
#   select(-Report.Date, -GNIS.Feature.ID, 
#          -c(US.Counties:Iowa.Watersheds.HUC.10)) %>%
#   gather(Medically.Needy.Eligible:Medicaid.Reimbursed, 
#          key = "var", value = "value") %>%
#   complete(crossing(month, var, nesting(County, County_FIPS, State_FIPS)), 
#            fill = list(value = 0)) %>%
#   spread(key = var, value = value) %>%
#   mutate(year = year(month)) %>%
#   select(-month) %>%
#   group_by(year, County, County_FIPS, State_FIPS) %>%
#   summarise_all(median, na.rm = T) %>%
#   mutate(avg_reimbursement_per_eligible_person = 
#            Medicaid.Reimbursed/Medicaid.Eligible)

unemployment <- read_csv(state_data_files[str_detect(state_data_files, "Unemployment_Insurance")]) %>%
  fix_names() %>%
  remove_empty_cols() %>%
  mutate(Month = mdy_hms(Month) %>% floor_date("day")) %>%
  rename(month = Month, County = County.Name) %>%
  mutate(County.FIP = as.character(County.FIP),
         County_FIPS = str_sub(County.FIP, 3, -1),
         State_FIPS = str_sub(County.FIP, 1, 2)) %>%
  select(-County.FIP) %>%
  rename(GNIS = GNIS.Feature.ID) %>%
  select(month:GNIS) %>%
  mutate(year = year(month)) %>%
  gather(-c(month:County, year, GNIS), key = "var", value = "value") %>%
  group_by(year, County, GNIS, var) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  spread(var, value) %>%
  ungroup()

sales_tax <- read_csv(state_data_files[str_detect(state_data_files, "Quarterly_Retail_Sales_Tax")]) %>%
  fix_names() %>%
  remove_empty_cols() %>%
  mutate(FIPS.County.Code = as.character(FIPS.County.Code),
         County_FIPS = str_sub(FIPS.County.Code, 3, -1),
         State_FIPS = str_sub(FIPS.County.Code, 1, 2)) %>%
  select(County.Number = County_Number, County, City, County_FIPS, State_FIPS, 
         Fiscal.Year, Quarter.Ending, 
         Number.of.Returns:Taxable.Sales) %>%
  mutate(Quarter.Ending = mdy(Quarter.Ending))

# --- Tax-district level Data --------------------------------------------------

assessed_property_values <- read_csv(state_data_files[str_detect(state_data_files, "Assessed_Property_Values")]) %>%
  mutate(County_FIPS = str_sub(`County FIP`, 3, -1),
         State_FIPS = str_sub(`County FIP`, 1, 2),
         Place_FIPS = str_sub(`City FIP`, 3, -1),
         coords = `Tax District Location`,
         lat = str_extract(coords, "\\([\\d\\.]{1,}") %>% parse_number(),
         long = str_extract(coords, "-[\\d\\.]{1,}\\)") %>% parse_number()) %>%
  select(-`Tax District Location`, -`County FIP`, -`City FIP`, 
         -PRIM_LAT_DEC, -PRIM_LONG_DEC) %>%
  select(-coords) %>%
  remove_empty_cols() %>%
  fix_names

# --- School-district level data -----------------------------------------------

# ia_cities <- sf::read_sf("data/Geography/Inc_cities_Twp_2010/") %>% 
#   filter(str_detect(CLASSFP10, "C")) %>%
#   mutate(geometry = st_transform(geometry, crs)) %>%
#   mutate(center = st_centroid(geometry))
# 
# school_cat <- function(Grade.Start, Grade.End) {
#   gradetbl <- tibble(grade = 0:12, type = rep(c("Elementary", "Middle", "High"), c(6, 3, 4)))
#   grades <- seq(Grade.Start, Grade.End, by = 1)
#   tmp <- gradetbl %>%
#     filter(grade %in% grades) %>%
#     group_by(type) %>%
#     count()
#   if (nrow(tmp) > 1) {
#     tmp <- filter(tmp, n > 1)
#   }
# 
#   tmp$type
# }
# 
# schools <- read_csv(state_data_files[str_detect(state_data_files, "School_Building_Directory")]) %>%
#   fix_names() %>%
#   remove_empty_cols() %>%
#   dplyr::rename(County = County.Name) %>%
#   mutate(District.Name = ifelse(is.na(District.Name), "Private", District.Name)) %>%
#   mutate(zip5 = str_extract(Physical.Location, "IA \\d{5}") %>% 
#            str_remove("IA ") %>% parse_number(),
#          coords = str_extract(Physical.Location, "\\([\\d\\.]{1,}, -?[\\d\\.]{1,}\\)"),
#          lat = str_extract(coords, "\\([\\d\\.]{1,}") %>% parse_number(),
#          long = str_extract(coords, "-[\\d\\.]{1,}\\)") %>% parse_number())%>%
#   mutate(Grade.Start = str_replace(Grade.Start, "P?K" , "0") %>% parse_number,
#          Grade.End = str_replace(Grade.End, "P?K" , "0") %>% parse_number,
#          public = District.Name != "Private") %>%
#   filter(!is.na(Grade.Start) & !is.na(Grade.End)) %>%
#   mutate(type = purrr::map2(Grade.Start, Grade.End, school_cat)) %>% 
#   unnest() %>%
#   filter(!is.na(lat) & !is.na(long)) %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326L) %>%
#   st_transform(crs = crs)
# 
# school_sm <- schools %>%
#   select(School.ID, public, type, geometry)
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
ia_city_schools <- readRDS("data/IowaGov/city_school_distance.rda")

ia_city_school_dist <- ia_city_schools %>%
  select(-geometry, -School.ID) %>%
  mutate(key = sprintf("dist_%s_%s", c("private", "public")[public+1], type)) %>%
  select(-public, -type) %>%
  unique() %>%
  tidyr::spread(key = key, value = dist) %>%
  select(-center)

rm(school_sm, school_cat)

school_revenue_year <- read_csv(state_data_files[str_detect(state_data_files, "School_District_Revenue")]) %>%
  fix_names() %>%
  remove_empty_cols() %>%
  filter(Amount > 0) %>%
  group_by(FiscalYear, AEA, Dist, DE_District, District.Name) %>%
  summarize(Amount = sum(Amount), Revenues.Per.Pupil = sum(Revenues.Per.Pupil))


  

# --- Cleaning up --------------------------------------------------------------

save(child_care_registered, fire_dept, physical_cultural_geographic_features, 
     post_offices, retirement_homes, ems, hospitals, 
     file = "data/Individual_Level_Data.Rdata")
rm(child_care_registered, fire_dept, physical_cultural_geographic_features, 
   post_offices, retirement_homes, ems, hospitals)


save(assisted_living, child_care_zip, fire_dept_zip, 
     post_office_zip, retirement_home_zip, ems_zip, 
     hospitals_zip, file = "data/Zip_Level_Data.Rdata")

# --- Not Useful anymore -------------------------------------------------------
# 
# city_budget_expenditures <- read_csv(state_data_files[str_detect(state_data_files, "Actual_Expenditures")]) %>%
#   mutate(coords = `Primary City Coordinates`,
#          lat = str_extract(coords, "\\([\\d\\.]{1,}") %>% parse_number(),
#          long = str_extract(coords, "-[\\d\\.]{1,}\\)") %>% parse_number(),
#          State_FIPS = str_sub(`City FIPS`, 1, 2),
#          Place_FIPS = str_sub(`City FIPS`, 3, -1)
#   ) %>%
#   select(-`Primary City Coordinates`, -`City FIPS`) %>%
#   remove_empty_cols() %>%
#   fix_names
# 
# city_budget_revenue <- read_csv(state_data_files[str_detect(state_data_files, "Actual_Revenue")]) %>%
#   mutate(coords = `Primary City Coordinates`,
#          lat = str_extract(coords, "\\([\\d\\.]{1,}") %>% parse_number(),
#          long = str_extract(coords, "-[\\d\\.]{1,}\\)") %>% parse_number(),
#          State_FIPS = str_sub(`City FIPS`, 1, 2),
#          Place_FIPS = str_sub(`City FIPS`, 3, -1)
#   ) %>%
#   select(-`Primary City Coordinates`, -`City FIPS`) %>%
#   remove_empty_cols() %>%
#   fix_names
