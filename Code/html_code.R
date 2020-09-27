library(rvest)
library(RSelenium)
library(htmltab)
library(splashr)

#Using Splash
install_splash()    # run this once to install the docker image
sp <- start_splash()
pg <- render_html(url = 'https://dhs.iowa.gov/iqrs/providers/homes')
stop_splash(sp)

#Using Selenium
rd <- rsDriver(browser = "chrome")
rd$client$navigate('https://dhs.iowa.gov/iqrs/providers/homes')
h <- rd$client$getPageSource()
h <- h[[1]] %>% read_html()

rd$client$close()
rd$server$stop()
rm(rd)

#Using rves
url <- "https://dhs.iowa.gov/iqrs/providers/homes"
    
homes <- url %>%
      read_html() %>%  
      html_table()

URL<-read_html("https://dhs.iowa.gov/iqrs/providers/homes")
#find the table rows in the page
table<-html_nodes(URL, "table")
  