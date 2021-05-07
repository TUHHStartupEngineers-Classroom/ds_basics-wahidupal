library(tidyverse)
library(readxl)
library(lubridate)

bikes_tbl      <- read_excel("C:\\Users\\upal1\\Desktop\\DS basic project\\ds_data\\01_bike_sales\\01_raw_data\\bikes.xlsx")
orderlines_tbl <- read_excel("C:\\Users\\upal1\\Desktop\\DS basic project\\ds_data\\01_bike_sales\\01_raw_data\\orderlines.xlsx")
# Not necessary for this analysis, but for the sake of completeness
bikeshops_tbl  <- read_excel("C:\\Users\\upal1\\Desktop\\DS basic project\\ds_data\\01_bike_sales\\01_raw_data\\bikeshops.xlsx")
# 3.0 Examining Data ----
# Method 1: Print it to the console
orderlines_tbl
# Method 2: glimpse
glimpse(orderlines_tbl)
# 4.0 Joining Data ----
left_join(orderlines_tbl, bikes_tbl, by = c("product.id" = "bike.id"))
# Chaining commands with the pipe and assigning it to order_items_joined_tbl
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
# Examine the results with glimpse()
bike_orderlines_joined_tbl %>% glimpse()
# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  
  
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ",") %>%
  
  
  mutate(total.price = price * quantity) %>%
 
  select(-...1, -gender) %>%
 
  select(-ends_with(".id")) %>%
  
 
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  

  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))
# 6.1 Sales by Year ----

# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns
  select(order_date, total_price) %>%
  
  # Add year column
  mutate(year = year(order_date)) %>%
  
  # Grouping by year and summarizing sales
  group_by(year) %>% 
  summarize(sales = sum(total_price)) %>%
  
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
sales_by_year_tbl
# Step 2 - Visualization
sales_by_year_tbl %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = year, y = sales)) +
  
  # Geometries
  geom_col(fill = "springgreen2") + 
  geom_label(aes(label = sales_text)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  

  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )
# 6.1 Sales by Year state ----
# Step 1 - Manipulate
sales_by_year_cat_1_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(order_date, total_price, state) %>%
  mutate(year = year(order_date)) %>%
  

  group_by(year, state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%

  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_cat_1_tbl 
# Step 2 - Visualize
sales_by_year_cat_1_tbl %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
# Geometries
geom_col() + 
geom_smooth(method = "lm", se = FALSE) +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
facet_wrap(~ state) +
scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                  decimal.mark = ",", 
                                                  prefix = "", 
                                                  suffix = " €")) +
labs(
  title = "Revenue by Year and State",
  subtitle = "Drop in sales are fairly low",
  fill = "State" # Changes the legend name
) +
  
  theme_light() +
  theme(title = element_text(face = "bold", color = "brown4"),
        legend.position  = "right",
        axis.text.x = element_text(angle = 45))
