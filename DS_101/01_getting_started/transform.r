library(ggplot2) # To load the diamonds dataset
library(dplyr)

# filter() picks cases based on their values ----
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  head(5)

# slice() selects entries  ----
diamonds %>% 
  filter(cut == 'Ideal' | cut == 'Premium', carat >= 0.23) %>% 
  slice(3:4)

# arrange() changes ordering of rows ----
diamonds %>% 
  arrange(cut, carat, desc(price))

# select() picks variables based on their names ----
diamonds %>% 
  select(color, clarity, x:z) %>% 
  head(n = 5)

# Exclusive select()
diamonds %>% 
  select(-(x:z)) %>% 
  head(n = 5)

# starts_with() / end_with() helper that selects every column starting with a prefix or ends with a suffix
# contains() helper that selects any column containing a string of text
# everything() a select helper that selects every column that has not aöready been selected. Good for reordering

# rename() changes name of column ----
diamonds %>% 
  rename(var_x = x) %>% 
  head(n = 5)

# mutate() adds new variables that are functions of existing variables and preserves existing ones
diamonds %>% 
  mutate(p = x + z, q = p + y) %>% 
  select(-(depth:price)) %>% 
  head(n = 5)

# transmute() adds new variables and drops existing ones
diamonds %>% 
  transmute(carat, cut, sum = x + y + z) %>% 
  head(n = 5)

# bind_cols() and bind_rows() binds two tibbles column- or row-wise ---- 
# group_by() and summarize() reduces multiple values down to a single summary
diamonds %>% 
  group_by(cut) %>% 
  summarize(max_price  = max(price),
            mean_price = mean(price),
            min_price  = min(price))

# glimpse() used to display columns of dataset & some portion of the data ----
glimpse(diamonds)

# typeof() gives you data-type of an R object ---- 

# Lubridate parsing of dates ----
# ymd(), ymd_hms(), dmy(), dmy_hms, mdy()
library(lubridate)
ymd(20201204)

# get components of date: year(), month(), mday(), hour(), minute(), second()