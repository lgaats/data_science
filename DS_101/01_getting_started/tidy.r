library(tidyverse)
# Part 1: Diamonds 2 pivot_longer() columns are variables-------
diamonds2 <- readRDS("DS_101/01_getting_started/data/diamonds2.rds")

diamonds2 %>% head(n = 5)
diamonds2 %>% 
  pivot_longer(cols = c("2008", "2009"),
               names_to = "year",
               values_to = "price") %>%
  head(n = 5)

# model <- lm(price ~ ., data = diamonds2_long) # Comment out line: strg+shift+c
# model

# Part 2: Diamonds 3 pivot_wider() observations scattered across rows--------
diamonds3 <- readRDS("DS_101/01_getting_started/data/diamonds3.rds")
diamonds3 %>% head(n = 5)
diamonds3 %>%
  pivot_wider(names_from = "dimension",
              values_from = "measurement") %>%
  head(n = 5)

# Part 3: Diamonds 4 separate() columns contain more than one value -----
diamonds4 <- readRDS("DS_101/01_getting_started/data/diamonds4.rds")
diamonds4 %>%
  separate(col = dim,
           into = c("x", "y", "z"),
           sep = "/",
           convert = TRUE)

# Part 4: unite () Diamonds 5 unite paste together multiple columns ----
diamonds5 <- readRDS("DS_101/01_getting_started/data/diamonds5.rds")
diamonds5 %>% unite(clarity,
                    clarity_prefix,
                    clarity_suffix,
                    sep = "")