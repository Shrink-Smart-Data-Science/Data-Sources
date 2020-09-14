library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)
library(tibble)

#TO-DO: figure out the way to get more information for the bigger files. (liquor stores and liquor sales)
#TO-DO: Find the best way to download the child care data that doesn't have a JSON url
pull_data <- function (x) {
  data = fromJSON(rawToChar(x$content))
  data
  }


data_sources <- tibble::tribble(
  ~name, ~url,
  "assisted living", "https://data.iowa.gov/resource/67aj-bdft.json",
  "city budget expenditures", "https://data.iowa.gov/resource/jy6h-2e5x.json",
  "city budget revenue", "https://data.iowa.gov/resource/bzed-t5zc.json",
  "child abuse occurrences", "https://data.iowa.gov/resource/mh9d-fias.json",
  "child abuse victims","https://data.iowa.gov/resource/n84y-ufum.json",
  "child welfare assessments", "https://data.iowa.gov/resource/er5e-kmgq.json", 
  "family investment program", "https://data.iowa.gov/resource/79c3-mzyc.json",
  "fire department census", "https://data.iowa.gov/resource/hv43-6ksq.json",
  "food assistance program statistics", "https://data.iowa.gov/resource/nqiw-f9td.json",
  "medicaid payments county", "https://data.iowa.gov/resource/jmyd-wk9g.json",
  "medicaid payments vendor", "https://data.iowa.gov/resource/b3t9-awkp.json",
  "physical and cultural geographic features", "https://data.iowa.gov/resource/uedc-2fk7.json",
  "school building directory", "https://data.iowa.gov/resource/spci-5thi.json",
  "school district revenues" ,"https://data.iowa.gov/resource/pf4i-4nww.json",
  "unemployment compensation fund status benefits", "https://data.iowa.gov/resource/bbux-m3a4.json",
  "unemployment insurance benefit payments", "https://data.iowa.gov/resource/aeyn-twxp.json",
  "liquor_stores", "https://data.iowa.gov/resource/ykb6-ywnd.json",
  "liquor sales", "https://data.iowa.gov/resource/m3tr-qhgy.json",
  "quarterly retail sales tax", "https://data.iowa.gov/resource/55fz-vque.json",
  "registered retirement facilities", "https://data.iowa.gov/resource/cvnj-m3t8.json"
) %>%
  # Download JSON
  mutate(json = purrr::map(url, GET))

# Get data frames
data_sources <- data_sources %>%
  mutate(data = purrr::map(json, pull_data))



