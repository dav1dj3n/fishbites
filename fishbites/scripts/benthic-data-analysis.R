

#install.packages("googlesheets4")
library(googlesheets4)
library(tidyverse)
library(ggplot2)
library(dplyr)

# Using the full URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1hRSyADWF6YhFdlJ-JwN13CSUPK8jjSJmHHYD4PuBdSE/edit?gid=0#gid=0"

# Read the first sheet (or specify by name or position)
benthic_data <- read_sheet(sheet_url)
head(benthic_data)

filter_benthic_data <- filter_benthic_data %>%
  filter(!is.na(Sarg))

filter_benthic_data<-subset(filter_benthic_data, Sum==25)
filter_benthic_data$percent_turf<-(filter_benthic_data$Turf+filter_benthic_data$CCA)/filter_benthic_data$Sum*100
filter_benthic_data$percent_turb<-(filter_benthic_data$Turb+filter_benthic_data$Sarg)/filter_benthic_data$Sum*100
head(filter_benthic_data)
#fix some of david's zeroes
filter_benthic_data <- filter_benthic_data %>%
  mutate(
    percent_turf = if_else(is.na(percent_turf), 0, percent_turf),
    percent_turb = if_else(is.na(percent_turb), 0, percent_turb)
  )

# Histogram of percent_turf
p1 <- ggplot(filter_benthic_data, aes(x = percent_turf)) +
  geom_histogram(binwidth = 5, fill = "#E16A86", color = "black") +
  labs(title = "Distribution of Turf Cover (%)",
       x = "Percent Turf",
       y = "Count") +
  theme_classic()

# Histogram of percent_turb
p2 <- ggplot(filter_benthic_data, aes(x = percent_turb)) +
  geom_histogram(binwidth = 5, fill = "#69B3A2", color = "black") +
  labs(title = "Distribution of Turbinaria Cover (%)",
       x = "Percent Turbidity",
       y = "Count") +
  theme_classic()

# Scatter plot of percent_turf vs percent_turb
p3 <- ggplot(filter_benthic_data, aes(x = percent_turf, y = percent_turb)) +
  geom_density_2d_filled(contour_var = "density") +
  labs(title = "Turf vs. Turbinaria Density",
       x = "Percent Turf",
       y = "Percent Turbinaria") +
  theme_classic()

ggplot(filter_benthic_data, aes(x = percent_turf, y = percent_turb)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Turf vs. Turbinaria Cover",
       x = "Percent Turf",
       y = "Percent Turbinaria") +
  theme_classic()


p1
p2
p3
