library(usethis)
library(rvest)
library(purrr)
library(tidyverse)
library(data.table)
library(openxlsx)

# usethis::use_course("https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki")


scraping_wiki_pg <- read_html("https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki")

scraping_wiki_pg %>%
  html_nodes("ul") %>%
  html_text() %>%
  length()


data_sources_codebook <- tibble::tribble(
  ~name, ~url,
    "assisted living facilities", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Assisted-Living-Facilities",
    "assessed property values", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Assessed-Property-Values"
#     "Child Abuse Occurrences", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Child-Abuse-Occurrences",
#     "Child Abuse Victims", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Child-Abuse-Victims",
#     "Child Welfare Assessments", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Child-Welfare-Assessments",
#     "City Budget Expenditures","https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/City-Budget-Expenditures",
#     "City Budget Revenue", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/City-Budget-Revenue",
#     "Family Investment Program", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Family-Investment-Program",
#     "Fire Department Census", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Fire-Department-Census",
#     "Food Assistance Program Statistics", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Food-Assistance-Program-Statistics",
#     "Liquor Stores", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Liquor-Stores",
#     "Medicaid Payments County", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Medicaid-Payments-County",
#     "Medicaid Payments Vendor", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Medicaid-Payments-Vendor",
#     "Physical and Cultural Geographic Features", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Physical-and-Cultural-Geographic-Features",
#     "Post Offices", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Post-Offices",
#     "Quarterly Retail Sales Tax", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Quarterly-Retail-Sales-Tax",
#     "Registered Retirement Facilities", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Registered-Retirement-Facilities",
#     "School Building Directory", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/School-Building-Directory",
#     "School District Revenues", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/School-District-Revenues",
#     "Unemployment Compensation Fund Status Benefits", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Unemployment-Compensation-Fund-Status-Benefits",
#     "Unemployment Insurance Benefit Payments", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Unemployment-Insurance-Benefit-Payments"
)

data_sources_text = c()

for (i in seq_along(data_sources_codebook$name)){
  data_sources_codebook <- data_sources_codebook %>% mutate(data = purrr::map(url[[i]], read_html))

  data_sources_text <-
    data_sources_codebook$data[[i]] %>%
    html_nodes("li") %>%
    html_text() %>%
    stringr::str_trim()

  source <- data_sources_codebook$data[[i]] %>%
    # Get body text, in a bulleted list, with a link
    html_node(css = ".markdown-body ul li a") %>%
    # convert to text
    html_text()

  temporal <- data_sources_codebook$data[[i]] %>%
    # Get body text, in a bulleted list, and get the 3rd bullet from that list
    html_node(css = ".markdown-body > ul > li:nth-of-type(3)") %>%
    html_text() %>%
    str_trim() %>%
    str_split("\\n")

  variables <- data_sources_codebook$data[[i]] %>%
    # Get body text, bulleted list, 2nd bullet, with a nested bulleted list, and get the nested items
    html_nodes(css = ".markdown-body > ul > li:nth-of-type(2) > ul > li") %>%
    html_text()
  
  var_types <- variables %>%
    # Pull out the (parenthetical)
    str_extract("\\((.*)\\)") %>%
    # remove parentheses
    str_remove_all("[[:punct:]]")
  
  variable_names <- variables %>%
    # Remove parenthetical information at the end of the string ($)
    str_remove_all(" \\(.*\\)$")

  mysql_name <- data_sources_codebook$data[[i]] %>%
    # bulleted list with a code tag
    html_nodes(css = ".markdown-body > ul > li > code") %>%
    html_text()

  last_update <- data_sources_codebook$data[[i]] %>%
    # bulleted list, get the last bullet
    html_nodes(css = ".markdown-body > ul > li:last-of-type") %>%
    html_text()

df = c()
df$CONDITION = data_sources_text %like% "^saved" %>% as.logical()
#Pulls the data in text before saved date in the wiki page
df = df %>% as.data.frame()

bottom = df %>%
  #The following code will count the true/false statements until true value
  mutate(group_break = case_when(
    row_number() == 1 ~ 1,
    CONDITION & !stats::lag(CONDITION, 1) ~ 1,
    !CONDITION & !stats::lag(CONDITION, 1) ~ 1,
    FALSE ~ 0),
    group_ind = cumsum(group_break)
  ) %>%
  filter(!is.na(group_ind)) %>%
  summarize(count = n() + 1) %>% as.numeric()

top = as.numeric(which(data_sources_text %like% "^Columns", arr.ind = TRUE)) + 1
#This statement will find the first variable column in the wiki page

data.sources.test = as.data.frame(data_sources_text[top:bottom])
#Will create the data frame with only those columns

data.sources.test <- data.sources.test %>%
   mutate(`Data Type` = str_extract(.[[1]], " \\(.*\\)") %>% str_replace(" \\((.*)\\)$", "\\1"))

  data.sources.test = cbind.data.frame(variable_names,var_types)
  data.sources.test = rbind.data.frame(data.sources.test,temporal,mysql_name,last_update)
  
  data_name = data_sources_codebook$name[[i]] #gsub(" ", "_", data_sources_codebook$name[[i]])
  colnames(data.sources.test) <- data_name

  openxlsx::write.xlsx(data.sources.test, file = "Data Source Codebook.xlsx",
           sheetName = data_name, append = T, row.names = FALSE)
}

