library(tidyverse)
library(rvest)
library(stringr)
library(pdftools)
library(tabulizer)

## ---- using pdftools package----
download.file("https://www.icip.iastate.edu/sites/default/files/retail/retail_1900190.pdf", "retail_1900190.pdf", mode = "wb")
txt <- pdf_text("retail_1900190.pdf")

# first page text
txt[1]

# second page text
txt[2]

# some tables
txt[23] #There has to be a better way to do this
txt[24]

## ---- using tabulizer package----
f <- "retail_1900190.pdf"
out1 <- extract_tables(f)
str(out1)

out2 <- extract_tables(f, pages = 1, guess = TRUE, output = "data.frame")
str(out2)

## ---- No longer using this ----
# page <- read_html("https://www.icip.iastate.edu/retail/city")
#
# raw_list <- page %>% # takes the page above for which we've read the html
#   html_nodes("a") %>%  # find all links in the page
#   html_attr("href") %>% # get the url for these links
#   str_subset("\\.pdf") %>% # find those that end in pdf only
#   str_c("https://www.icip.iastate.edu", .) %>% # prepend the website to the url
#   map(read_html) %>% # take previously generated list of urls and read them
#   map(html_node, "#raw-url") %>% # parse out the 'raw' url - the link for the download button
#   map(html_attr, "href") %>% # return the set of raw urls for the download buttons
#   str_c("https://www.icip.iastate.edu", .) %>% # prepend the website again to get a full url
#   walk2(., basename(.), download.file, mode = "wb") # use purrr to download the pdf associated with each url to the current working directory
