library(tidyverse)
library(sf)
library(lubridate)
library(RMySQL)
library(DBI)
library(keyring)
library(geojsonio)
library(rgdal)

# --- Initial Setup ------------------------------------------------------------
conn <- DBI::dbConnect(RMySQL::MySQL(),
                       host = "srvanderplas.com",
                       dbname = "scc",
                       user = "remote",
                       password = keyring::key_get("MY_SECRET"))

dbListTables(conn) 
# --- Download GEOJSON file from Iowa DOT --------------------------------------
json_url <- "https://gis.iowadot.gov/rams/rest/services/lrs/FeatureServer/102/query?where=1%3D1&outFields=*&outSR=4326&f=json"
download.file(json_url, destfile = "Data/traffic.json", mode = "wb")

#----Read in the GEOJSON data from the IowaDOT
json.link <- geojson_sf("Data/traffic.json")

