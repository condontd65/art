library(googlesheets)
library(tidyverse)
library(stringr)
library(dplyr)
#library(qdap)
library(data.table)
#library(DescTools)
#library(zoo)

art.extract <- read.csv("art_extract_Parsed.csv")

# Authenticate google
gs_auth(new_user = TRUE)

# Check available sheets
gs_ls()
available <- gs_ls()

# Read in the art extracted spreadsheet
art <- gs_title("art_extract")
art <- gs_read(art)

art.extract[]






