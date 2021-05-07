library(httr)
library(glue)
library(tibble)
library(jsonlite)
library(tidyverse)
library(purrr)
library(stringr)
library(xml2)
wp_url <- "https://apify.com/covid-19" # Data Scraping 
wp <- xml2::read_html(wp_url)
data_frame <- rvest::html_table(wp)[[1]] %>% 
  tibble::as_tibble(.name_repair = "unique") # Columns Repairing
data_frame %>% dplyr::glimpse(45)
data_frame


#write.csv(x=data_frame, file="C:\\Users\\upal1\\Desktop\\DS basic project\\ds_basics-wahidupal\\data_accuisition1.csv")
#write_rds(data_frame, "C:\\Users\\upal1\\Desktop\\DS basic project\\ds_basics-wahidupal\\data_accuisition1.rds")
