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
data_sources_codebook <- data_sources_codebook %>% mutate(data = purrr::map(url, read_html))

# Fix names that are too long
data_sources_codebook <- data_sources_codebook %>%
  mutate(name = str_replace_all(name, c("Assistance" = "Assist", "Program" = "Prog", "Statistics" = "Stats",
                                        "Physical and Cultural" = "Phys & Cultural", "Geographic" = "Geo",
                                        "Unemployment" = "Unempl", "Compensation" = "Comp",
                                        "Registered" = "Reg", "Insurance" = "Ins", "Fund Status" = "Fund")) %>%
           str_to_title())

# file.copy("Data Source Codebook.xlsx", "Data Source Codebook_bkup.xlsx", overwrite = T)
# file.remove("Data Source Codebook.xlsx")
wb <- openxlsx::createWorkbook(creator = "S&CC Data Science Team",
                               title = "Web Data Codebook")

for (i in seq_along(data_sources_codebook$name)) {

  data_sources_text <-
    data_sources_codebook$data[[i]] %>%
    html_nodes("li") %>%
    html_text() %>%
    stringr::str_trim()

  source <- data_sources_codebook$data[[i]] %>%
    # Get body text, in a bulleted list, with a link
    html_nodes(css = "li") %>%
    # This looks for text that says Source: instead of presuming a natural ordering
    `[[`(which(str_detect(data_sources_text, "Source:"))) %>%
    html_children() %>%
    # convert to text
    html_text()  %>%
    tibble(V1 = "Data Source", V2 = .)


  # Summary doesn't always exist...
  summary_idx <- which(str_detect(data_sources_text, "Summarized data"))
  if (length(summary_idx) > 0) {
    summary <- data_sources_codebook$data[[i]] %>%
      # Get body text, in a bulleted list, and get the 3rd bullet from that list
      # html_node(css = ".markdown-body > ul > li:nth-of-type(3)") %>%
      html_nodes(css = "li") %>%
      # This looks for text that says Summarized data instead of presuming a natural ordering
      `[[`(summary_idx) %>%
      html_children() %>%
      html_attr("href") %>%
      tibble(V1 = "Summary Data URL", V2 = .)
  } else {
    summary <- tibble(V1 = "Summary Data URL", V2 = "")
  }


  mysql_name <- data_sources_codebook$data[[i]] %>%
    # bulleted list with a code tag
    # html_nodes(css = ".markdown-body > ul > li > code") %>%
    html_nodes(css = "li") %>%
    # This looks for text that says Temporal information instead of presuming a natural ordering
    `[[`(which(str_detect(data_sources_text, "MySQL"))) %>%
    html_text() %>%
    str_trim() %>%
    str_remove("saved in MySQL db as ") %>%
    tibble(V1 = "MySQL table name", V2 = .)

  update_idx <- which(str_detect(data_sources_text, "last updated"))
  if (length(update_idx) > 0) {
    last_update <- data_sources_codebook$data[[i]] %>%
      # bulleted list, get the last bullet
      # html_nodes(css = ".markdown-body > ul > li:last-of-type") %>%
      html_nodes(css = "li") %>%
      # This looks for text that says Temporal information instead of presuming a natural ordering
      `[[`(update_idx) %>%
      html_text() %>%
      str_remove("Data last updated on ") %>%
      lubridate::mdy() %>%
      tibble(V1 = "Last updated on", V2 = as.character(.)) %>%
      select(-.)
  } else {
    last_update <- tibble(V1 = "Last updated", V2 = "Unknown")
  }


  temporal_idx <- which(str_detect(data_sources_text, "Temporal information"))
  if (length(temporal_idx) > 0) {
    temporal <- data_sources_codebook$data[[i]] %>%
      # Get body text, in a bulleted list, and get the 3rd bullet from that list
      # html_node(css = ".markdown-body > ul > li:nth-of-type(3)") %>%
      html_nodes(css = "li") %>%
      # This looks for text that says Temporal information instead of presuming a natural ordering
      `[[`(temporal_idx) %>%
      # convert to text
      html_text() %>%
      str_trim() %>%
      str_split("\\n", simplify = T) %>%
      as.character() %>%
      str_trim() %>%
      str_split(":") %>%
      purrr::map_df(., ~set_names(., c("V1", "V2"))) %>%
      bind_rows(tibble(V1 = "", V2 = ""), .)
  } else {
    temporal <- tibble(V1 = "Temporal information", V2 = "NA")
  }


  table_info <- bind_rows(source, summary, mysql_name, last_update, temporal)

  variables <- data_sources_codebook$data[[i]] %>%
    # Get body text, bulleted list, 2nd bullet, with a nested bulleted list, and get the nested items
    # html_nodes(css = ".markdown-body > ul > li:nth-of-type(2) > ul > li") %>%
    html_nodes(css = "li") %>%
    # This looks for text that says Temporal information instead of presuming a natural ordering
    `[[`(which(str_detect(data_sources_text, "Columns"))) %>%
    # Get unordered list
    html_children() %>%
    # Get bullets from the list
    html_children() %>%
    # Convert to text
    html_text() %>%
    tibble(V1 = .) %>%
    extract(col = "V1", into = c("V1", "V2"), regex = "(.*) \\((.*)\\)")

  table_info <- bind_rows(table_info,
                          tibble(V1 = "", V2 = ""),
                          tibble(V1 = "Column Name", V2 = "Column Type"),
                          variables)
  # df = c()
  # df$CONDITION = data_sources_text %like% "^saved" %>% as.logical()
  # #Pulls the data in text before saved date in the wiki page
  # df = df %>% as.data.frame()
  #
  # bottom = df %>%
  #   #The following code will count the true/false statements until true value
  #   mutate(group_break = case_when(
  #     row_number() == 1 ~ 1,
  #     CONDITION & !stats::lag(CONDITION, 1) ~ 1,
  #     !CONDITION & !stats::lag(CONDITION, 1) ~ 1,
  #     FALSE ~ 0),
  #     group_ind = cumsum(group_break)
  #   ) %>%
  #   filter(!is.na(group_ind)) %>%
  #   summarize(count = n() + 1) %>% as.numeric()
  #
  # top = as.numeric(which(data_sources_text %like% "^Columns", arr.ind = TRUE)) + 1
  # #This statement will find the first variable column in the wiki page
  #
  # data.sources.test = as.data.frame(data_sources_text[top:bottom])
  # #Will create the data frame with only those columns
  #
 # data.sources.test <- data.sources.test %>%
#    mutate(`Data Type` = str_extract(.[[1]], " \\(.*\\)") %>% str_replace(" \\((.*)\\)$", "\\1"))

  # data.sources.test = cbind.data.frame(variable_names,var_types)
  # data.sources.test = rbind.data.frame(data.sources.test,mysql_name,last_update)
#
  data_name <- data_sources_codebook$name[[i]] #gsub(" ", "_", data_sources_codebook$name[[i]])
  pretty_data_name <- str_replace_all(data_name, "_", " ") %>% str_to_title()
  # colnames(data.sources.test) <- data_name

  # openxlsx::write.xlsx(table_info, file = "Data Source Codebook.xlsx",
  #                      sheetName = pretty_data_name, append = T, row.names = FALSE, col.names = FALSE)

  # May want to bold things using addStyle() to indicate headers?

  openxlsx::addWorksheet(wb, sheetName = pretty_data_name)
  openxlsx::writeData(wb, sheet = i, x = table_info, colNames = F, rowNames = F)
  openxlsx::setColWidths(wb, sheet = i, cols = c(1, 2), widths = "auto")

}

openxlsx::saveWorkbook(wb, "Data Source Codebook.xlsx", overwrite = TRUE)

