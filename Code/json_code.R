## Check required packages
# package.required <- c("httr", "jsonlite", "tidyverse", "purr", "tibble", "RSocrata", "DBI", "rstudioapi", "RMySQL", "skimr", "keyring")
# 
# for(i in 1:length(package.required)){
#   if(!package.required[i] %in% rownames(installed.packages())){
#     install.packages(package.required[i])
#   }
# }

library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)
library(tibble)
library(RSocrata)
library(DBI)
library(rstudioapi)
library(RMySQL)
library(skimr)
library(keyring)


data_sources = c()

data_sources <- tibble::tribble(
  ~name, ~url,
  #  "assisted living", "https://data.iowa.gov/api/odata/v4/67aj-bdft",
  #   "assessed property values", "https://data.iowa.gov/api/odata/v4/p8sk-8ig7" 
  #  "city budget expenditures", "https://data.iowa.gov/api/odata/v4/jy6h-2e5x",
  #  "city budget revenue", "https://data.iowa.gov/api/odata/v4/bzed-t5zc",
    "city population iowa by county year",  "https://data.iowa.gov/api/odata/v4/y8va-rhk9"
  # "child abuse occurrences", "https://data.iowa.gov/api/odata/v4/mh9d-fias",
  # "child abuse victims","https://data.iowa.gov/api/odata/v4/n84y-ufum",
  # "child welfare assessments", "https://data.iowa.gov/api/odata/v4/er5e-kmgq",
  # "family investment program", "https://data.iowa.gov/api/odata/v4/79c3-mzyc",
  # "fire department census", "https://data.iowa.gov/api/odata/v4/hv43-6ksq",
  # "food assistance program statistics", "https://data.iowa.gov/api/odata/v4/nqiw-f9td",
  # "medicaid payments county", "https://data.iowa.gov/api/odata/v4/jmyd-wk9g",
  # "medicaid payments vendor", "https://data.iowa.gov/api/odata/v4/b3t9-awkp",
  # "physical and cultural geographic features", "https://data.iowa.gov/api/odata/v4/uedc-2fk7",
  # "school building directory", "https://data.iowa.gov/api/odata/v4/spci-5thi",
  # "school district revenues" ,"https://data.iowa.gov/api/odata/v4/pf4i-4nww",
  # "unemployment compensation fund status benefits", "https://data.iowa.gov/api/odata/v4/bbux-m3a4",
  # "unemployment insurance benefit payments", "https://data.iowa.gov/api/odata/v4/aeyn-twxp",
  # "liquor_stores", "https://data.iowa.gov/api/odata/v4/ykb6-ywnd",
  # "liquor sales", "https://data.iowa.gov/api/odata/v4/m3tr-qhgy"
  # "quarterly retail sales tax", "https://data.iowa.gov/api/odata/v4/55fz-vque",
  #  "registered retirement facilities", "https://data.iowa.gov/api/odata/v4/cvnj-m3t8"
)

#Here is the list of data sources that will take on the data directly from the JSON file
#Export Data to MySQL Server 
conn <- DBI::dbConnect(RMySQL::MySQL(), 
                       host = "srvanderplas.com",
                       dbname = "scc",
                       user = "remote",
                       password = keyring::key_get("MY_SECRET"))

#summary(conn) 

for (i in seq_along(data_sources$name)){
  data_sources <- data_sources %>% mutate(data = purrr::map(url[[i]], read.socrata))
  data_name = gsub(" ", "_", data_sources$name[[i]])
  dbWriteTable(conn = conn,
               name = data_name,
               value = data.frame(data_sources$data[[i]]),
               row.names=FALSE,
#               append = TRUE)
#              nrows = 50000000,
               #field.types=list(organization_type='BLOB'),
               overwrite = TRUE) 
}

#Make sure that tables are added with right name 
dbListTables(conn) 

#Delete tables that aren't useful anymore
#dbRemoveTable(conn, "liquor_stores_mysql")

#Check for duplicate columns
skim(dbReadTable(conn, "unemployment_insurance_benefit_payments"))

#disconnet from the server ;)
dbDisconnect(conn)

