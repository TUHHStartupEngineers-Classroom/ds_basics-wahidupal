
# Importing Libraries

library(tidyverse)
library(data.table)
library(vroom)
library(tictoc)

# Data wrangling part 1

col_types_assignee <- list(
  id = col_character(),
  type = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "C:\\Users\\upal1\\Desktop\\DS basic project\\ds_basics-wahidupal\\assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)

# data table conversion 
assignee_data_frame <- as.data.table(assignee_tbl %>% rename(assignee_id = id))

assignee_data_frame %>% glimpse()

# Load patent assignee data

col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character()
  
)

patent_assignee_tbl <- vroom(
  file       = "C:\\Users\\upal1\\Desktop\\DS basic project\\ds_basics-wahidupal\\patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)

# data table conversion

patent_assignee_data_frame <- as.data.table(patent_assignee_tbl)

patent_assignee_data_frame %>% glimpse()

# merging data( assignee and patent assignee)

tic()
combined_data <- merge(x = assignee_data_frame, y = patent_assignee_data_frame, 
                       by    = "assignee_id", 
                       all.x = TRUE, 
                       all.y = FALSE)
toc()

combined_data %>% glimpse()

# Patent dominance
Pat_Dom <- combined_data %>%
  
  filter(!is.na(type) & type == 2) %>%
  group_by(organization, type) %>%
  tally(sort = T) %>%
  ungroup() %>%
  arrange(desc(n))
# Patent Dominance: What US company / corporation has the most patents? 
Pat_Dom

Pat_Dom_top10 <- head(Pat_Dom,10)

# Data wrangling part 2

# Loading the reduced patent data

col_types_patent <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
  
)

patent_tbl <- vroom(
  file       = "C:\\Users\\upal1\\Desktop\\DS basic project\\ds_basics-wahidupal\\patent.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent,
  na         = c("", "NA", "NULL")
)

patent_tbl

# converting to data frame

patent_data_frame <- as.data.table(patent_tbl %>% rename(patent_id = id)) 


patent_data_frame %>% glimpse()

# Merging data( assignee and patent assignee and patent)

tic()
combined_new_data <- merge(x = combined_data, y = patent_data_frame, 
                           by    = "patent_id", 
                           all.x = TRUE, 
                           all.y = FALSE)
toc()

combined_new_data %>% glimpse()

#Manipulating data

merged_data <- combined_new_data %>%
  
  select(organization, date, type) %>%
  mutate(year = year(date)) %>%
  filter(year == 2014)

merged_data %>% glimpse()

# Recent patent activity
# What US company had the most patents granted in August 2014? 
Aug_patents_2014 <- merged_data %>%
  
  filter(!is.na(type) & type == 2) %>%
  group_by(organization, type, year) %>%
  tally(sort = T) %>%
  ungroup() %>%
  arrange(desc(n))

Aug_patents_2014

Aug_patents_2014_top10 <- head(Aug_patents_2014,10)
# Data wrangling part 3
# Loading uspc data

col_types_uspc <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_character()
)

uspc_tbl <- vroom(
  file       = "C:\\Users\\upal1\\Desktop\\DS basic project\\ds_basics-wahidupal\\uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)

# data table conversion 
uspc_data_frame <- as.data.table(uspc_tbl)

uspc_data_frame %>% glimpse()

# merging data( assignee and patent assignee and uspc)

tic()
combined_newest_data <- merge(x = combined_data, y = uspc_data_frame, 
                              by    = "patent_id", 
                              all.x = TRUE, 
                              all.y = FALSE)
toc()

combined_newest_data %>% glimpse()

# For the top 10 companies (worldwide) with the most patents, what are the top 5 USPTO tech main classes?

top_10_uspto <- combined_newest_data %>%
  
  select(organization, type, mainclass_id, sequence) %>%
  filter(sequence == 0) %>%
  group_by( mainclass_id) %>%
  tally(sort = T) %>%
  ungroup() %>%
  arrange(desc(n))


top_10_uspto

top_10_uspto_top10 <- head(top_10_uspto,10)

data_wrangling_1 <- Aug_patents_2014_top10

write_rds(data_wrangling_1, "C:\\Users\\upal1\\Desktop\\DS basic project\\ds_basics-wahidupal\\data_wrangling_1.rds")


data_wrangling_2 <- top_10_uspto_top10

write_rds(data_wrangling_2, "C:\\Users\\upal1\\Desktop\\DS basic project\\ds_basics-wahidupal\\data_wrangling_2.rds")