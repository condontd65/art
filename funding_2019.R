library(googlesheets)
library(tidyverse)
library(stringr)
library(dplyr)
library(qdap)
library(data.table)
library(DescTools)
library(zoo)
library(xlsx)

# Authenticate google
gs_auth(new_user = TRUE)

# Check available sheets
gs_ls()
available <- gs_ls()

# Read in the art extracted spreadsheet
data <- gs_title("BCC - FY19 Funding Recommendations")
art.state <- gs_read(data, ws = 1)
art.city <- gs_read(data, ws = 2)

# State first
art.state <- art.state[1:67,]
art.state$geoaddress <- NA
art.state <- unite(art.state, "geoaddress",
                   c("Address","City"),
                   sep = ' ',
                   remove = FALSE)

write.csv(art.state, 'funding/2019/art_state_geordy.csv', row.names = FALSE)


# City
art.city <- art.city[1:154,]
art.city$geoaddress <- NA
art.city <- unite(art.city, "geoaddress",
                  c("Address", "City", "State"),
                  sep = " ",
                  remove = FALSE)

write.csv(art.city, 'funding/2019/art_city_geordy.csv', row.names = FALSE)



