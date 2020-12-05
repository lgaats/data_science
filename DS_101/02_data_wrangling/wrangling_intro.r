# Connect to Database ----
library(RSQLite)
library(dplyr)
con <- RSQLite::dbConnect(drv    = SQLite(), 
                          dbname = "00_data/02_chinook/Chinook_Sqlite.sqlite")

#check available tables
dbListTables(con)

# examine table
tbl(con, "Album")

# collect data
album_tbl <- tbl(con, "Album") %>% collect()

# once collecting is done: disconnect from DB
dbDisconnect(con)
con

# Connect to API ----
library(httr)
GET("https://swapi.dev/api/people")
# 200: The request has succeeded.
# 403: The client does not have access rights to the content.
# 404: Not found. The server can not find the requested resource.

# Glue | adressing an url of an API ----
library(glue)
name <- "Fred"
glue('My name is {name}.')

# function to flexibly adress an API url
resp <- GET("https://swapi.dev/api/people/1/")

# Wrapped into a function
sw_api <- function(path) {
  url <- modify_url(url = "https://swapi.dev", path = glue("/api{path}"))
  resp <- GET(url)
  stop_for_status(resp) # automatically throws an error if a request did not succeed
}

resp <- sw_api("/people/1")
resp

# convert API into data (character vector resembling JSON)
starwars <- rawToChar(resp$content)

# convert character vector to list
library(jsonlite)
fromJSON(starwars)
# convert R object to JSON
toJSON(starwars)

# R Lists ----
library(jsonlite)
resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()

# Alternative access content()
content(resp, as = "text")
content(resp, as = "parsed")  # as a list
content(resp) # default, selection in this case: list

# Example: Wirecard | Alphavantage API
GET("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=WI.DE&interval=5min&outputsize=full&apikey=demo")

# Token Response
token    <- "my_individual_token"
response <- GET(glue("https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=WDI.DE&apikey={token}"))
responseg