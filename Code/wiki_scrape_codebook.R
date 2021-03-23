library(usethis)
library(rvest)
library(purrr)
library(tidyverse)
library(data.table)
library(xlsx)

# usethis::use_course("https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki")


scraping_wiki_pg <- read_html("https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki")

scraping_wiki_pg %>%
  html_nodes("ul") %>%
  html_text() %>%
  length()


data_sources_codebook <- tibble::tribble(
  ~name, ~url,
    "assisted living facilities", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Assisted-Living-Facilities",
    "assessed property values", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Assessed-Property-Values",
    "Child Abuse Occurrences", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Child-Abuse-Occurrences",
    "Child Abuse Victims", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Child-Abuse-Victims",
    "Child Welfare Assessments", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Child-Welfare-Assessments",
    "City Budget Expenditures","https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/City-Budget-Expenditures",
    "City Budget Revenue", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/City-Budget-Revenue",
    "Family Investment Program", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Family-Investment-Program",
    "Fire Department Census", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Fire-Department-Census",
    "Food Assistance Program Statistics", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Food-Assistance-Program-Statistics",
    "Liquor Stores", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Liquor-Stores",
    "Medicaid Payments County", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Medicaid-Payments-County",
    "Medicaid Payments Vendor", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Medicaid-Payments-Vendor",
    "Physical and Cultural Geographic Features", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Physical-and-Cultural-Geographic-Features",
    "Post Offices", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Post-Offices",
    "Quarterly Retail Sales Tax", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Quarterly-Retail-Sales-Tax",
    "Registered Retirement Facilities", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Registered-Retirement-Facilities",
    "School Building Directory", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/School-Building-Directory",
    "School District Revenues", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/School-District-Revenues",
    "Unemployment Compensation Fund Status Benefits", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Unemployment-Compensation-Fund-Status-Benefits",
    "Unemployment Insurance Benefit Payments", "https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Unemployment-Insurance-Benefit-Payments"
)

data_sources_text = c()

for (i in seq_along(data_sources_codebook$name)){
  data_sources_codebook <- data_sources_codebook %>% mutate(data = purrr::map(url[[i]], read_html))

  data_sources_text <-
    data_sources_codebook$data[[i]] %>%
    html_nodes("li") %>%
    html_text() %>%
    stringr::str_trim()

  df = c()
  df$CONDITION = data_sources_text %like% "^saved" %>% as.logical()
  df = df %>% as.data.frame()

  bottom = df %>%
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

  data.sources.test = as.data.frame(data_sources_text[top:bottom])

  data.sources.test <- data.sources.test %>%
    mutate(`Data Type` = str_extract(.[[1]], " \\(.*\\)") %>% str_replace(" \\((.*)\\)$", "\\1"))

  data_name = gsub(" ", "_", data_sources_codebook$name[[i]])
  colnames(data.sources.test) <- data_name

  write.xlsx(data.sources.test, file = "Data Source Codebook.xlsx",
           sheetName = data_name, append = F, row.names = FALSE)
}


# wiki_pg <- read_html("https://github.com/Shrink-Smart-Data-Science/Data-Sources/wiki/Assessed-Property-Values")
#
# li_text <-
#   wiki_pg %>%
#   html_nodes("li") %>%
#   html_text()
#
# length(li_text)
#
# df = c()
# df$CONDITION = li_text %like% "^saved" %>% as.logical()
# df = df %>% as.data.frame()
#
# df %>%
#   mutate(group_break = case_when(
#   row_number() == 1 ~ 1,
#   CONDITION & !stats::lag(CONDITION, 1) ~ 1,
#   !CONDITION & !stats::lag(CONDITION, 1) ~ 1,
#   FALSE ~ 0),
#   group_ind = cumsum(group_break)
#   ) %>%
#   filter(!is.na(group_ind)) %>%
#   summarize(count = n() + 1)
#
# assessed_living_values <- as.data.frame(li_text[60:87])
#
# colnames(assessed_living_values) <- c("variables")
