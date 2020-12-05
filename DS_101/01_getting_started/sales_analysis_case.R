# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS CHALLENGE LOCATION----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)

# 2.0 Importing Files ----
bikes_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----
glimpse(orderlines_tbl)
glimpse(bikes_tbl)
glimpse(bikeshops_tbl)

# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

glimpse(bike_orderlines_joined_tbl)

# 5.0 Wrangling Data ----
bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # separate category name
  separate(col    = category,
           into   = c("category.1", "category.2", "category.3"),
           sep    = " - ") %>%
  # add total price
  mutate(total.price = price * quantity) %>%
  
  # 5.3 Optional: Reorganize. Using select to grab or remove unnecessary columns
  # 5.3.1 by exact column name
  select(-...1, -gender) %>%
  
  # 5.3.2 by a pattern
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
  select(-ends_with(".id")) %>%
  
  # 5.3.3 Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  # 5.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total.price,
         everything()) %>%
  
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Sales by Location
sales_by_location_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # select columns
  select(location, total_price) %>%
  
  # separate location column
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%
  
  group_by(state) %>%
  summarize(sales = sum(total_price)) %>%
  
  # Optional: Add a column that turns the numbers into a currency format 
  # (makes it in the plot optically more appealing)
  # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " \u20AC"))
  #arrange states by revenue
 sales_by_location_tbl <- arrange(sales_by_location_tbl, desc(sales))

# Step 2 - Visualize
 sales_by_location_tbl %>%
   
   # Setup canvas with the columns state (x-axis) and sales (y-axis)
   ggplot(aes(x = reorder(state, -sales), y = sales)) +
   
   # Geometries
   geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
   geom_label(aes(label = sales_text)) + # Adding labels to the bars
   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
   
   # Formatting
   # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
   # Again, we have to adjust it for euro values
   scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                     decimal.mark = ",", 
                                                     prefix = "", 
                                                     suffix = " \u20AC")) +
   labs(
     title    = "Revenue by location",
     subtitle = "North Rhine Westphalia has the hightest Share",
     x = "", # Override defaults for x and y
     y = "Revenue"
   )
ggsave(
  "revenue_by_location.pdf",
  path = "00_data/01_bike_sales/03_images",
  height = 7, 
  width = 14
)

# 6.2 Sales by Location and Year ----

# Step 1 - Manipulate
sales_by_loc_year_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(location, total_price, order_date) %>%
  mutate(year = year(order_date))  %>%
  
  # Group by and summarize year and main category
  group_by(year, location) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " \u20AC")) %>%
  # separate location column
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ")

sales_by_loc_year_tbl  
# Step 2 - Visualize
sales_by_loc_year_tbl %>%

  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = str_wrap(state, 10))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +

  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot

  # Facet
  facet_wrap(~ str_wrap(state, 10)) +

  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " \u20AC")) +
  labs(
    title = "Revenue by location and year",
    fill = "Location" # Changes the legend name
  )
ggsave(
  "revenue_by_location_year.pdf",
  path = "00_data/01_bike_sales/03_images"
)