library(rvest)
library(RSelenium)
library(htmltab)
library(splashr)
library(XML)
library(dplyr)
library(purrr)
library(DBI)
library(rstudioapi)
library(RMySQL)

#Using Splash
#install_splash()    # run this once to install the docker image
#sp <- start_splash()
#pg <- render_html(url = 'https://dhs.iowa.gov/iqrs/providers/homes') #https://dhs.iowa.gov/iqrs/providers/centers
#stop_splash(sp)

#Using Selenium
#For the Homes Data
rd <- rsDriver(browser=c("chrome"), chromever="85.0.4183.83")
rd$client$navigate('https://dhs.iowa.gov/iqrs/providers/homes')
h <- rd$client$getPageSource()

rd$client$close()
rd$server$stop()
rm(rd)

nodes <- h[[1]] %>%
     read_html() %>%
     html_nodes(xpath = "/html/body/div[4]/div[2]/div[2]/div[2]/div/div/div/div/div[1]/div/div/div/table[2]")

homes_data <- html_table(nodes, fill = TRUE)[[1]]
str(homes_data)

names(homes_data) <- homes_data[2, ]
homes_data <- homes_data[-c(1:2), c(1:6)]

#For the Child Center Tables
rd <- rsDriver(browser=c("chrome"), chromever="85.0.4183.83")
rd$client$navigate('https://dhs.iowa.gov/iqrs/providers/centers')
h <- rd$client$getPageSource()

nodes <- h[[1]] %>%
  read_html() %>%
  html_nodes(xpath = "/html/body/div[4]/div[2]/div[2]/div[2]/div/div/div/div/div[1]/div/div/div/table")

child_center_data <- html_table(nodes, fill = TRUE)[[1]]
str(child_center_data)

names(child_center_data) <- child_center_data[1, ]
child_center_data <- child_center_data[-1,]


#Write data to the database

conn <- DBI::dbConnect(RMySQL::MySQL(),
                       host = "srvanderplas.com",
                       user = "remote",
                       password = rstudioapi::askForPassword("Database password"))
                       #password = "awesome-remote-mysql-server-password")
                       #
# DBI::dbSendQuery(conn, "CREATE TABLE scc.homes;")
# the database is scc; the tables would be created inside that database.
#
# Error in .local(conn, statement, ...) : could not run statement: Access denied for user 'remote'@'%' to database 'homes'

summary(conn) #We need a Dbname to get data on to the server??

#db_info <- capture.output(mysqlDescribeConnection(conn, verbose = T))

dbWriteTable(conn = conn,
             dbname = "scc",
             name = "homes",
             value = homes_data)

dbWriteTable(conn = conn,
             dbname = "scc",
             name = "child_center",
             value = child_center_data)
