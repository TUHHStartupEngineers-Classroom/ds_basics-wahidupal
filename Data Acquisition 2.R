library(tidyverse) 
library(rvest)     
library(xopen)     
library(jsonlite)  
library(glue)      
library(stringi)   

url_home          <- "https://www.rosebikes.de/fahrr%C3%A4der/mtb"
# Open links directly from RStudio to inspect them

html_home         <- read_html(url_home) # Read in the HTML for the entire web page

# data scraping from the web page for  the bike models 
bike_model <- html_home %>% 
  
  html_nodes(css = ".catalog-category-bikes__title-text") %>% 
  html_text() %>%
  
  str_remove_all("\n") 

bike_model

# data scraping from the web page for  the bike prices

bike_price <- html_home %>%
  
  html_nodes(css = ".catalog-category-bikes__price-title") %>%
  html_text() %>%
  
  str_remove_all("\\.") %>%
  stringr::str_replace_all(pattern = "\nab ", replacement = "") %>%
  stringr::str_replace_all(pattern = "\n", replacement = "") 

bike_price

# merging the two tables into one

da2 <- tibble(bike_model, bike_price)

da2 <- da2 %>% mutate(bike_price = as.character(gsub("€", "", bike_price)))
da2$bike_price <- as.character(gsub(",","",da2$bike_price))
da2$bike_price <- as.character(gsub("ab","",da2$bike_price))
#d<-da2
da2


write_rds(da2, "C:\\Users\\upal1\\Desktop\\DS basic project\\ds_basics-wahidupal\\Data Acquisition 2.R")

write.csv(x=da2, file="C:\\Users\\upal1\\Desktop\\DS basic project\\ds_basics-wahidupal\\data_accuisition2.csv")