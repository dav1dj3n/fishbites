

#install.packages("googlesheets4")
library(googlesheets4)
# Using the full URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1hRSyADWF6YhFdlJ-JwN13CSUPK8jjSJmHHYD4PuBdSE/edit?gid=0#gid=0"

# Read the first sheet (or specify by name or position)
benthic_data <- read_sheet(sheet_url)
head(benthic_data)
