# Combining data into aggregate sets by observational unit
library(tidyverse)
source("Code/Initial_Cleaning.R")
source("Code/Spatial_Processing.R")

codebook <- tibble(variable = c("City"), 
                   initial_data = c("city"),
                   initial_aggregation_level = NA)
#----- Zip Level ---------------------------------------------------------------
ia_by_zip <- ia_city_zip %>%
  select(City, zip5, weight) %>% 
  left_join(assisted_living_zip) %>%
  left_join(child_care_zip) %>%
  left_join(ems_zip) %>%
  left_join(fire_dept_zip) %>%
  left_join(hospitals_zip) %>%
  left_join(post_office_zip) %>%
  left_join(retirement_home_zip)

codebook <- bind_rows(codebook, 
                      tibble(variable = names(ia_by_zip)[4:12],
                             initial_data = "individual location",
                             initial_aggregation_level = "5 digit zip"))

#---- City Level ---------------------------------------------------------------

ia_city_geometry <- ia_cities %>%
  select(City = NAME10, geometry)

ia_city_zip_aggregate <- ia_by_zip %>%
  st_drop_geometry() %>%
  group_by(City) %>%
  tidyr::gather(key = "key", value = "value", -zip5, -weight, -City, -n_post_offices, -hospital_trauma_level) %>%
  group_by(City, key) %>%
  summarize(value = mean(value, weight = weight, na.rm = T),
            n_post_offices = sum(n_post_offices, na.rm = T),
            hospital_trauma_level = min(hospital_trauma_level)) %>%
  tidyr::spread(key = key, value = value, fill = NA) %>%
  ungroup()

ia_by_city_finance <- full_join(
  city_budget,
  fiscal_capacity
) %>%
  rename(Population_finance = Population, size_finance = size) %>%
  select(City, Population_finance, size_finance, 
         Fiscal_Capacity = Fiscal.Capacity, Fiscal_Effort = Fiscal.Effort, 
         everything()) %>%
  modify_at("Population_finance", parse_number)

ia_data <- ia_city_zip_aggregate %>%
  left_join(liquor_shops_city) %>%
  full_join(filter(ia_city_population, Year == 2010) %>%
              ungroup() %>%
              filter(!str_detect(City, "Balance of .* County")) %>%
              select(-Year))  %>%
  full_join(ia_city_school_dist) %>%
  full_join(ia_by_city_finance) %>%
  filter(!str_detect(City, "Balance [Oo]f "))
  

codebook <- bind_rows(codebook, 
                      tibble(variable = names(liquor_shops_city)[-1],
                             initial_data = "individual location",
                             initial_aggregation_level = "city")) %>%
  bind_rows(tibble(variable = names(ia_city_population)[-c(1:2)],
                   initial_data = "city",
                   initial_aggregation_level = "city")) %>%
  bind_rows(tibble(variable = names(ia_city_school_dist)[-1],
                   initial_data = "individual location",
                   initial_aggregation_level = "city")) %>%
  bind_rows(tibble(variable = names(ia_by_city_finance)[-1],
                   initial_data = "city",
                   initial_aggregation_level = "city"))

small_town_poll <- read_csv("Data/ISTP_QOL/qol_ISTP2014_x14jun2019_data.csv")

ia_data <- ia_data %>%
  full_join(small_town_poll, by = c("FIPS" = "FIPS_PL")) %>%
  select(City, FIPS, Population, everything()) %>%
  full_join(ia_city_geometry)

codebook <- bind_rows(codebook, 
                      tibble(variable = names(small_town_poll)[-1],
                             initial_data = "city",
                             initial_aggregation_level = "city"))

ia_data_scaled <- ia_data %>%
  mutate_at(vars(matches("slots|beds|firefighters|occupancy|sales|stores|rev|exp")), function(x) x/.$Population_finance)


save(ia_data, ia_data_scaled, codebook, file = "Data/Combined_IA_data.Rdata")

#----- County Level ------------------------------------------------------------



