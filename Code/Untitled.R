library(httr)
library(jsonlite)

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
liquor_sales_data <- fromJSON(rawToChar(liquor_data_json$content)) 

jsonlist <- (assisted_living_json, city_budget_and_actual_expenditures_json, city_budget_and_actual_revenue_json, iowa_child_abuse_occurrences_by_year_county_and_type_of_abuse_json,
             iowa_child_abuse_victims_by_year_county_and_age_group_json,  iowa_child_welfare_assessments_by_disposition_county_and_year_json, iowa_family_investment_program_recipients_and_grants_by_month_and_county_json,
             )

for (i in jsonlist) {
  #name = substr("jsonlist",1,nchar("jsonlist")-5)
  data <- fromJSON(rawToChar(jsonlist$content)) 
  #names(data) <- name
}

