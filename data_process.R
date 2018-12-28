library(googlesheets)
library(tidyverse)
library(stringr)
library(dplyr)
library(qdap)
library(data.table)
library(DescTools)
library(zoo)

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


### Start here if you don't need to pull in the google sheet again

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
write.csv(data, 'data_nodupes.csv')

###

# Make columns to fill with data 
data$collection <- NA

data$medium <- NA
data$medium2 <- NA
data$medium3 <- NA
data$medium4 <- NA
data$medium5 <- NA

data$neighborhood <- NA
data$neighborhood2 <- NA
data$neighborhood3 <- NA
data$neighborhood4 <- NA

data$type <- NA
data$type2 <- NA
data$type3 <- NA
data$type4 <- NA
data$type5 <- NA

data$funders <- NA
data$funders2 <- NA
data$funders3 <- NA
data$funders4 <- NA
data$funders5 <- NA


data$artists <- NA
data$artists2 <- NA
data$artists3 <- NA
data$artists4 <- NA
data$artists5 <- NA
data$artists6 <- NA
data$artists7 <- NA
data$artists8 <- NA
data$artists9 <- NA
data$artists10 <- NA
data$artists11 <- NA
data$artists12 <- NA

data$year <- NA
data$year2 <- NA
data$year3 <- NA
data$year4 <- NA
data$year5 <- NA
data$year6 <- NA
data$year7 <- NA


### Try a bunch of different ways to get the data into several columns


data <- mutate(data, collection = ifelse(V1 %like% "%Collection%", V1, collection))

data <- mutate(data, medium = ifelse(V1 %like% "%Mediu%", V1, medium))
data <- mutate(data, medium2 = ifelse(V2 %like% "%Mediu%", V2, medium2))
data <- mutate(data, medium3 = ifelse(V3 %like% "%Mediu%", V3, medium3))
data <- mutate(data, medium4 = ifelse(V4 %like% "%Mediu%", V4, medium4))
data <- mutate(data, medium5 = ifelse(V4 %like% "%Mediu%", V5, medium5))

data <- mutate(data, neighborhood = ifelse(V1 %like% "%Neighbor%", V1, neighborhood))
data <- mutate(data, neighborhood2 = ifelse(V2 %like% "%Neighbor%", V2, neighborhood2))
data <- mutate(data, neighborhood3 = ifelse(V3 %like% "%Neighbor%", V3, neighborhood3))
data <- mutate(data, neighborhood4 = ifelse(V4 %like% "%Neighbor%", V4, neighborhood4))

data <- mutate(data, type = ifelse(V3 %like% "%Type%", V3, type))
data <- mutate(data, type2 = ifelse(V4 %like% "%Type%", V4, type2))
data <- mutate(data, type3 = ifelse(V5 %like% "%Type%", V5, type3))
data <- mutate(data, type4 = ifelse(V6 %like% "%Type%", V6, type4))
data <- mutate(data, type5 = ifelse(V7 %like% "%Type%", V7, type5))

data <- mutate(data, funders = ifelse(V4 %like% "%Funder%", V4, funders))
data <- mutate(data, funders2 = ifelse(V5 %like% "%Funder%", V5, funders2))
data <- mutate(data, funders3 = ifelse(V6 %like% "%Funder%", V6, funders3))
data <- mutate(data, funders4 = ifelse(V7 %like% "%Funder%", V7, funders4))
data <- mutate(data, funders5 = ifelse(V8 %like% "%Funder%", V8, funders5))

data <- mutate(data, artists = ifelse(V2 %like% "%Artis%", V2, artists))
data <- mutate(data, artists2 = ifelse(V3 %like% "%Artis%", V3, artists2))
data <- mutate(data, artists3 = ifelse(V4 %like% "%Artis%", V4, artists3))
data <- mutate(data, artists4 = ifelse(V5 %like% "%Artis%", V5, artists4))
data <- mutate(data, artists5 = ifelse(V6 %like% "%Artis%", V6, artists5))
data <- mutate(data, artists6 = ifelse(V7 %like% "%Artis%", V7, artists6))
data <- mutate(data, artists7 = ifelse(V8 %like% "%Artis%", V8, artists7))
data <- mutate(data, artists8 = ifelse(V9 %like% "%Artis%", V9, artists8))
data <- mutate(data, artists9 = ifelse(V10 %like% "%Artis%", V10, artists9))
data <- mutate(data, artists10 = ifelse(V11 %like% "%Artis%", V11, artists10))
data <- mutate(data, artists11 = ifelse(V12 %like% "%Artis%", V12, artists11))
data <- mutate(data, artists12 = ifelse(V13 %like% "%Artis%", V13, artists12))

data <- mutate(data, year = ifelse(V6 %like% "%Year%", V6, year))
data <- mutate(data, year2 = ifelse(V7 %like% "%Year%", V7, year2))
data <- mutate(data, year3 = ifelse(V8 %like% "%Year%", V8, year3))
data <- mutate(data, year4 = ifelse(V9 %like% "%Year%", V9, year4))
data <- mutate(data, year5 = ifelse(V10 %like% "%Year%", V10, year5))
data <- mutate(data, year6 = ifelse(V11 %like% "%Year%", V11, year6))
data <- mutate(data, year7 = ifelse(V14 %like% "%Year%", V14, year7))

## Separate out the new columns from the old
data.cols <- data[,15:53]

# Remove the qualifiers inside of the columns
# This can be done because the column names now serve as the qualifiers
data.cols <- data.frame(lapply(data.cols, function(x) {
  gsub("Collection: ", "", x)
}))

data.cols <- data.frame(lapply(data.cols, function(x) {
  gsub("Medium: ", "", x)
}))

data.cols <- data.frame(lapply(data.cols, function(x) {
  gsub("Neighborhood: ", "", x)
}))

data.cols <- data.frame(lapply(data.cols, function(x) {
  gsub("Type: ", "", x)
}))

data.cols <- data.frame(lapply(data.cols, function(x) {
  gsub("Funders: ", "", x)
}))

data.cols <- data.frame(lapply(data.cols, function(x) {
  gsub("Artist: ", "", x)
}))

data.cols <- data.frame(lapply(data.cols, function(x) {
  gsub("Year: ", "", x)
}))


# Merge columns with like data with comma seperator
data.cols$medium.new <- paste(data.cols$medium, data.cols$medium2, data.cols$medium3,
                              data.cols$medium4, data.cols$medium5, sep = ", ")

data.cols$neighborhood.new <- paste(data.cols$neighborhood, data.cols$neighborhood2, data.cols$neighborhood3,
                                    data.cols$neighborhood4, sep = ", ")

data.cols$type.new <- paste(data.cols$type, data.cols$type2, data.cols$type3,
                            data.cols$type4, data.cols$type5, sep = ", ")

data.cols$funders.new <- paste(data.cols$funders, data.cols$funders2, data.cols$funders3,
                               data.cols$funders4, data.cols$funders5, sep = ", ")

data.cols$artists.new <- paste(data.cols$artists, data.cols$artists2, data.cols$artists3,
                               data.cols$artists4, data.cols$artists5, data.cols$artists6,
                               data.cols$artists7, data.cols$artists8, data.cols$artists9,
                               data.cols$artists10, data.cols$artists11, data.cols$artists12, 
                               sep = ", ")

data.cols$year.new <- paste(data.cols$year, data.cols$year2, data.cols$year3,
                            data.cols$year4, data.cols$year5, data.cols$year6,
                            data.cols$year7, sep = ", ")

# Remove NA strings in new columns
data.cols <- data.frame(lapply(data.cols, function(x) {
  gsub("NA, ", "", x)
}))
data.cols <- data.frame(lapply(data.cols, function(x) {
  gsub(", NA", "", x)
}))
data.cols <- data.frame(lapply(data.cols, function(x) {
  gsub("NA", NA, x)
}))

data.rdy <- data.table(data.cols$collection, data.cols$medium.new, data.cols$neighborhood.new, 
                       data.cols$type.new, data.cols$funders.new, data.cols$artists.new,
                       data.cols$year.new)

colnames(data.rdy) <- c("Collection", "Medium", "Neighborhood", "Type", "Funder", "Artists",
                        "Year")

# Create final art dataframe to be exported for review
art.rdy <- data.table(art$type, art$nid, art$title, art$body, art$name, art$latitude, art$longitude,
                      art$images, data.rdy$Collection, data.rdy$Medium, data.rdy$Neighborhood,
                      data.rdy$Type, data.rdy$Funder, data.rdy$Artists, data.rdy$Year)

# create colnames for art.rdy
colnames(art.rdy) <- c("type", "id", "title", "summary", "location", "latitude", "longitude",
                       "images", "collection", "medium", "neighborhood", "subtype", "funder", "artist",
                       "year")
# Remove strange strings
art.rdy <- data.frame(lapply(art.rdy, function(x) {
  gsub("<p>", "", x)
}))
art.rdy <- data.frame(lapply(art.rdy, function(x) {
  gsub("</p>", "", x)
}))

write.csv(art.rdy, 'art_data.csv', row.names = FALSE)














