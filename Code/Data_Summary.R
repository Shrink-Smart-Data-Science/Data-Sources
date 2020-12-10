library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)
library(tibble)
library(DBI)
library(rstudioapi)
library(RMySQL)
library(keyring)

conn <- DBI::dbConnect(RMySQL::MySQL(), 
                       host = "srvanderplas.com",
                       dbname = "scc",
                       user = "remote",
                       password = keyring::key_get("MY_SECRET"))

tables<-dbListTables(conn)

dfList <- setNames(lapply(tables, function(t) dbReadTable(conn, t)), tables)

# --- Data Summary ------------------------------------------------------------
#Assisted Living
summary(dfList$assisted_living)

#Assessed Property Values
min(dfList$assessed_property_values$assessment_year)
max(dfList$assessed_property_values$assessment_year)

#Child abuse Occurrences
min(dfList$child_abuse_occurrences$year)
max(dfList$child_abuse_occurrences$year)

#Child Abuse Victims
min(dfList$child_abuse_victims$year)
max(dfList$child_abuse_victims$year)

#Child Center Data
min(dfList$child_center_data$Certificate.Expiration..Date)
max(dfList$child_center_data$Certificate.Expiration..Date)

#Child Welfare Assessments
min(dfList$child_welfare_assessments$year)
max(dfList$child_welfare_assessments$year)

#City Budget Expenditures
min(dfList$city_budget_expenditures$fiscal_year)
max(dfList$city_budget_expenditures$fiscal_year)

#City Budget Revenue
min(dfList$city_budget_revenue$fiscal_year)
max(dfList$city_budget_revenue$fiscal_year)

#Family Investment Program
min(dfList$family_investment_program$cy)
max(dfList$family_investment_program$cy)

#Fire Department Census
summary(dfList$fire_department_census)

#Food Assistance Program
min(dfList$food_assistance_program_statistics$cy)
max(dfList$food_assistance_program_statistics$cy)

#Homes Data
min(dfList$homes_data$Certificate..Expiration..Date)
max(dfList$homes_data$Certificate..Expiration..Date)

#Liquor Stores
min(dfList$liquor_stores$date)
max(dfList$liquor_stores$date)

#Medicaid Payments County
min(dfList$medicaid_payments_county$report_as_of_date)
max(dfList$medicaid_payments_county$report_as_of_date)

#Medicaid Payments Vendor
min(dfList$medicaid_payments_vendor$report_as_of_date)
max(dfList$medicaid_payments_vendor$report_as_of_date)

#Physical and cultural Geographic Features
min(dfList$physical_and_cultural_geographic_features$date_created, na.rm = TRUE)
max(dfList$physical_and_cultural_geographic_features$date_created, na.rm = TRUE)

min(dfList$physical_and_cultural_geographic_features$date_edited, na.rm = TRUE)
max(dfList$physical_and_cultural_geographic_features$date_edited, na.rm = TRUE)

#Quarterly Retail Sales Tax
min(dfList$quarterly_retail_sales_tax$fiscal_year)
max(dfList$quarterly_retail_sales_tax$fiscal_year)

#Registered Retirement Facilities
summary(dfList$registered_retirement_facilities)

#School Building Directory
summary(dfList$school_building_directory)

#School District Revenues
min(dfList$school_district_revenues$fiscalyear)
max(dfList$school_district_revenues$fiscalyear)

#Unemployment Compensation Fund Status Benefits
min(dfList$unemployment_compensation_fund_status_benefits$year)
max(dfList$unemployment_compensation_fund_status_benefits$year)

#Unemployment Insurance Benefits Payments
min(dfList$unemployment_insurance_benefit_payments$month_ending)
max(dfList$unemployment_insurance_benefit_payments$month_ending)

#--- Find the Center of county ---------------
#My notes: This will information will need to come from ISU statisticians on how to best do this. 

#Using the Assessed Property Values?  
apv_centroids <- 
  summarize(group_by(assessed_property_values, county_name),
            x = mean(range(prim_long_dec)), y = mean(range(prim_lat_dec)))

names(apv_centroids)[1] <- "county"
head(apv_centroids)

#Using Hospital Center?
hospital_centroids <- 
  summarize(group_by(hospitals, County),
            x = mean(range(long)), y = mean(range(lat)))

names(hospital_centroids)[1] <- "county"
head(hospital_centroids)

#Using Fire Department Center?
fdc_centroids <- 
  summarize(group_by(fire_dept, County),
            x = mean(range(long)), y = mean(range(lat)))

names(fdc_centroids)[1] <- "county"
head(fdc_centroids)

#Using Schools Center?
#---See Initial Cleaning (schools_sm dataset)
