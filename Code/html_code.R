library(rvest)
library(RSelenium)
library(htmltab)

library(splashr)

install_splash()    # run this once to install the docker image
sp <- start_splash()

pg <- render_html(url = 'https://dhs.iowa.gov/iqrs/providers/homes')

stop_splash(sp)



rd <- rsDriver()
rd$client$navigate('https://dhs.iowa.gov/iqrs/providers/homes')
h <- rd$client$getPageSource()
h <- h[[1]] %>% read_html()

rd$client$close()
rd$server$stop()
rm(rd)

  url <- "https://dhs.iowa.gov/iqrs/providers/homes"
    
  homes <- url %>%
      read_html() %>%  
      html_nodes(xpath='/html/body/div[4]/div[2]/div[2]/div[2]/div/div/div/div/div[1]/div/div/div/table[2]') %>%  
      html_table()

  
  URL<-read_html("https://dhs.iowa.gov/iqrs/providers/homes")
  #find the table rows in the page
  table<-html_nodes(URL, "table")
  
  #pull info from the table rows
  num<-html_text(html_nodes(table, "td.u-align-right"))
  provider<-html_text(html_nodes(table, "td.cell-provider-name"))
  
  #final data.frame with a table of the results
  df<-data.frame(provider, matrix(num, ncol=3, byrow=TRUE))    