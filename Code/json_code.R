library(httr)
library(jsonlite)
library(tidyverse)

# See https://www.tidyverse.org/blog/2017/12/workflow-vs-script/ for why this is
# not exactly best practice :)
rm(list = ls())

#Pull in data via JSON
assisted_living_json = GET("https://data.iowa.gov/resource/67aj-bdft.json")
#campaign_contrib_zip_city_json = GET("")
#campaign_contrib_zip_json = GET("")
city_budget_and_actual_expenditures_json = GET("https://data.iowa.gov/resource/jy6h-2e5x.json")
city_budget_and_actual_revenue_json = GET("https://data.iowa.gov/resource/bzed-t5zc.json")
#city_school_distance_json = GET("")
iowa_child_abuse_occurrences_by_year_county_and_type_of_abuse_json = GET("https://data.iowa.gov/resource/mh9d-fias.json")
iowa_child_abuse_victims_by_year_county_and_age_group_json = GET("https://data.iowa.gov/resource/n84y-ufum.json")
#iowa_child_care_json = GET ("")
iowa_child_welfare_assessments_by_disposition_county_and_year_json = GET("https://data.iowa.gov/resource/er5e-kmgq.json")
iowa_family_investment_program_recipients_and_grants_by_month_and_county_json = GET("https://data.iowa.gov/resource/79c3-mzyc.json")
iowa_fire_department_census_json = GET("https://data.iowa.gov/resource/hv43-6ksq.json")
iowa_food_assistance_program_statistics_by_month_and_county_json = GET("https://data.iowa.gov/resource/nqiw-f9td.json")
iowa_medicaid_payments_and_recipients_by_month_and_county_json = GET("https://data.iowa.gov/resource/jmyd-wk9g.json")
iowa_medicaid_payments_and_recipients_by_month_and_vendor_json = GET("https://data.iowa.gov/resource/b3t9-awkp.json")
iowa_physical_and_cultural_geographic_features_json = GET("https://data.iowa.gov/resource/uedc-2fk7.json")
iowa_school_building_directory_json = GET("https://data.iowa.gov/resource/spci-5thi.json")
iowa_school_district_revenues_by_fiscal_year_json = GET("https://data.iowa.gov/resource/pf4i-4nww.json")
iowa_unemployment_compensation_fund_status_benefits_paid_json = GET("https://data.iowa.gov/resource/bbux-m3a4.json")
iowa_unemployment_insurance_benefit_payments_and_recipients_by_county_monthly_json = GET("https://data.iowa.gov/resource/aeyn-twxp.json")
iowa_liquor_stores_json = GET("https://data.iowa.gov/resource/ykb6-ywnd.json")
iowa_liquor_sales_data_json = GET("https://data.iowa.gov/resource/m3tr-qhgy.json")
quarterly_retail_sales_tax_data_by_county_and_city_json = GET("https://data.iowa.gov/resource/55fz-vque.json")
registered_retirement_facilities_in_iowa_json = GET("https://data.iowa.gov/resource/cvnj-m3t8.json")


# convert from JSON into R object
# liquor_sales_data <- fromJSON(rawToChar(liquor_data_json$content))

jsonlist <- list(assisted_living_json,
             city_budget_and_actual_expenditures_json,
             city_budget_and_actual_revenue_json,
             iowa_child_abuse_occurrences_by_year_county_and_type_of_abuse_json,
             iowa_child_abuse_victims_by_year_county_and_age_group_json,
             iowa_child_welfare_assessments_by_disposition_county_and_year_json,
             iowa_family_investment_program_recipients_and_grants_by_month_and_county_json,
             iowa_fire_department_census_json,
             iowa_food_assistance_program_statistics_by_month_and_county_json,
             iowa_medicaid_payments_and_recipients_by_month_and_county_json,
             iowa_medicaid_payments_and_recipients_by_month_and_vendor_json,
             iowa_physical_and_cultural_geographic_features_json,
             iowa_school_building_directory_json,
             iowa_school_district_revenues_by_fiscal_year_json,
             iowa_unemployment_compensation_fund_status_benefits_paid_json,
             iowa_unemployment_insurance_benefit_payments_and_recipients_by_county_monthly_json,
             iowa_liquor_stores_json,
             iowa_liquor_sales_data_json,
             quarterly_retail_sales_tax_data_by_county_and_city_json,
             registered_retirement_facilities_in_iowa_json
             )

namelist <- c("assisted_living_json",
              "city_budget_and_actual_expenditures_json",
              "city_budget_and_actual_revenue_json",
              "iowa_child_abuse_occurrences_by_year_county_and_type_of_abuse_json",
              "iowa_child_abuse_victims_by_year_county_and_age_group_json",
              "iowa_child_welfare_assessments_by_disposition_county_and_year_json",
              "iowa_family_investment_program_recipients_and_grants_by_month_and_county_json",
              "iowa_fire_department_census_json",
              "iowa_food_assistance_program_statistics_by_month_and_county_json",
              "iowa_medicaid_payments_and_recipients_by_month_and_county_json",
              "iowa_medicaid_payments_and_recipients_by_month_and_vendor_json",
              "iowa_physical_and_cultural_geographic_features_json",
              "iowa_school_building_directory_json",
              "iowa_school_district_revenues_by_fiscal_year_json",
              "iowa_unemployment_compensation_fund_status_benefits_paid_json",
              "iowa_unemployment_insurance_benefit_payments_and_recipients_by_county_monthly_json",
              "iowa_liquor_stores_json",
              "iowa_liquor_sales_data_json",
              "quarterly_retail_sales_tax_data_by_county_and_city_json",
              "registered_retirement_facilities_in_iowa_json"
)

jsonlist = as.list(jsonlist)

pull_data <- function (x) {
  data = fromJSON(rawToChar(x$content))
  data
  }

#Not sure what I'm doing wrong here
lapply(jsonlist, pull_data)
# You're working with a single JSON file and trying to pull_data for every entry
# in that JSON file. What you wanted was to pull data for each of the 20 JSON
# files.

for (i in jsonlist) {
  for (j in namelist) {
      name = substr("j",1,nchar("j")-5)
      data = lapply(jsonlist[[i]], pull_data)
      names(data) <- name
      }
}




# ---- Susan's partial attempt to riff off of Denise's code --------------------


library(purrr)
library(tibble)

data_sources <- tibble::tribble(
  ~name, ~url,
  "assisted living", "https://data.iowa.gov/resource/67aj-bdft.json",
  "city budget expenditures", "https://data.iowa.gov/resource/jy6h-2e5x.json",
  "city budget revenue", "https://data.iowa.gov/resource/bzed-t5zc.json",
  "liquor sales", "https://data.iowa.gov/resource/m3tr-qhgy.json"
) %>%
  # Download JSON
  mutate(json = purrr::map(url, GET))

# Get data frames
data_sources <- data_sources %>%
  mutate(data = purrr::map(json, pull_data))


# Right now you're only getting 1000 records for each file, so you'll probably
# need to do something with pull_data to make sure you're getting all records
# instead of the last 1000. But, your code works beautifully!
#
# I changed it to a tidy version because I suspect it will be easier to read and
# maintain. I know that you may not have seen as much of purrr, it's a bit more
# obscure than e.g. dplyr and tidyr. But it is *so* amazingly awesome.
# You can use purrr::map where you'd use lapply, but the difference is that you
# can use a specific map function based on the type you want back out.
