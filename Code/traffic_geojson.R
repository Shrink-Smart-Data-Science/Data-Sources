library(tidyverse)
library(sf)
library(lubridate)
library(RMySQL)
library(DBI)
library(keyring)
library(geojsonio)
library(geojsonR)
library(rgdal)


# --- Initial Setup ------------------------------------------------------------
conn <- DBI::dbConnect(RMySQL::MySQL(),
                       host = "srvanderplas.com",
                       dbname = "scc",
                       user = "remote",
                       password = keyring::key_get("MY_SECRET"))


# --- Download GEOJSON file from Iowa DOT --------------------------------------
json_url <- "https://gis.iowadot.gov/rams/rest/services/lrs/FeatureServer/102/query?where=1%3D1&outFields=*&outSR=4326&f=json"
download.file(json_url, destfile = "Data/traffic.json", mode = "wb")

#----Read in the GEOJSON data from the IowaDOT
json.link <- geojson_sf("Data/traffic.json")

save.image(json.link, "Data/json.link_denise.RData")

dbListTables(conn) 
dbWriteTable(conn = conn,
             name = "traffic_data",
             value = json.link,
             row.names=FALSE,
             overwrite = TRUE) 

#--- Processing the .gdb file from Iowa DOT ---------

#ogrListLayers("Data/Traffic_data.gdb")
#traffic_data = readOGR("Data/Traffic_data.gdb","Primary")

traffic <- sf::st_read(dsn = "Data/Traffic_data.gdb", layer = "Primary")
head(traffic)
