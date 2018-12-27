library(googlesheets)
library(tidyverse)
library(stringr)
library(dplyr)
library(qdap)
library(data.table)
library(DescTools)
library(zoo)

## This is junk now :(   James was speedy with it   Thank James for James 

# Authenticate google
gs_auth(new_user = TRUE)

# Check available sheets
gs_ls()
available <- gs_ls()

# Read in the art extracted spreadsheet
art <- gs_title("art_extract")
art <- gs_read(art)

# Separate data column based on commas, out to 28 as that's the max number of fields
art.sep <- separate(art, data, into = as.character(c(1:28)), 
                    sep = ",", remove = FALSE)

# Pull new columns into different dataframe
data <- art.sep[, 11:38]

# remove '{' and '}' and '"' from data frame
data <- data.frame(lapply(data, function(x) {
  gsub("\\{", " ", x)
}))
data <- data.frame(lapply(data, function(x) {
  gsub("\\}", "", x)
}))
data <- data.frame(lapply(data, function(x) {
  gsub("\"", "", x)
}))
#data <- data.frame(lapply(data, function(x) {
#  gsub("  \"", "", x)
#}))

# Remove copied strings

# Write function to eliminate copied columns
distinct.col <- function(df) {
  d <- df %>%
    t() %>%
    data.table() %>%
    distinct() %>%
    t() %>%
    data.table()
  df <- d
  return(df)
}
# Run function
data.dist <- distinct.col(data)

# There are still duplicates for some resaon, we'll have to look at individual rows to make this work

### Specificially run on individual rows acting strange)


## Loop through rows individually to create a clean dataset without duplicates
# Create list of rows needed

rows <- 1:158
for (row in 1:nrow(data.dist)) {
  correct <- data.dist[row,] %>%
    t() %>%
    as.matrix() %>%
    unique() %>%
    t() %>%
    as.matrix()
  length(correct) <- length(data.dist[row,])
  correct <- as.matrix(t(as.matrix(correct)))
  correct <- data.table(correct)
  data.dist[row,] <- correct
  #data <- data.dist
}
data <- data.dist[,1:14]

#for (row in 1:nrow(data.dist)) {
#  print(data.dist[row,])
#}

###

# Make column to fill with data
data$collection <- NA
data$medium <- NA
data$medium2 <- NA
data$medium3 <- NA
data$medium4 <- NA
data$neighborhood <- NA
data$type <- NA
data$funders <- NA
data$artists <- NA
data$year <- NA

### Try a bunch of different ways to get the data into several columns


data <- mutate(data, collection = ifelse(V1 %like% "%Collection%", V1, collection))
data <- mutate(data, medium = ifelse(V1 %like% "%Mediu%", V1, medium))
data <- mutate(data, medium = ifelse(V2 %like% "%Mediu%", V2, medium))
data <- mutate(data, medium2 = ifelse(V3 %like% "%Mediu%", V3, medium2))
data <- mutate(data, medium3 = ifelse(V4 %like% "%Mediu%", V4, medium3))
data <- mutate(data, medium4 = ifelse(V4 %like% "%Mediu%", V5, medium4))
















