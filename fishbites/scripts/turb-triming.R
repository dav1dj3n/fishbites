#install.packages("googlesheets4")
library(googlesheets4)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggridges)
library(lubridate)

# Using the full URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1J29FZPk6blnLTn8GVwNePCZDFBF4De7aQOIMoD71rdw/edit?gid=0#gid=0"

# Read the first sheet (or specify by name or position)
data <- read_sheet(sheet_url)
head(data)
data_bites<-data  %>% filter (Bites>0)


# Example start times for each replicate (just time, no date)
video_starts <- tibble(
  Replicate = 1:4,
  start_time = hms::as_hms(c("09:11:00", "09:40:00", "09:38:00", "09:26:00"))
)



# Step 1: Convert timevi to POSIXct or numeric
data_bites <- data_bites %>%
  mutate(timevi_hms = hms::as_hms(timevi))

# Join start times by replicate
data_bites <- data_bites %>%
  left_join(video_starts, by = "Replicate")

# Calculate elapsed time in minutes (timevi - start_time)
data_bites <- data_bites %>%
  mutate(elapsed_time_min = as.numeric(difftime(timevi_hms, start_time, units = "mins")))

# Aggregate bites per Replicate, Species, elapsed_time_min
agg_data <- data_bites %>%
  group_by(Replicate, Species, elapsed_time_min) %>%
  summarise(total_bites = sum(Bites, na.rm = TRUE), .groups = "drop")

library(forcats) # for fct_reorder

agg_data <- agg_data %>%
  mutate(Species = fct_reorder(Species, total_bites, .desc = TRUE))

ggplot(agg_data, aes(x = elapsed_time_min, y = Species, height = total_bites, group = Species, fill = Species)) +
  geom_density_ridges(stat = "identity", alpha = 0.7, scale = 5) +
  scale_x_continuous(name = "Elapsed time (minutes since video start)") +
  labs(y = "Species", title = "Bites Over Time by Species (Elapsed Time)") +
  #facet_wrap(~Replicate, scales="free_y")+
  theme_classic() +
  theme(legend.position = "none")

start_times <- tibble(
  Replicate = c(1, 2, 3, 4),
  video_start = hms::as_hms(c("09:11:00", "09:40:00", "09:38:00", "09:26:00"))
)

# 1. Use raw visit times, not aggregated per minute
visits_raw <- data %>%
  mutate(timevi_hms = hms::as_hms(timevi)) %>%
  left_join(start_times, by = "Replicate") %>%
  mutate(elapsed_time_min = as.numeric(difftime(timevi_hms, video_start, units = "mins"))) %>%
  filter(!is.na(elapsed_time_min))

# 2. Ridge plot with smooth density
ggplot(visits_raw, aes(x = elapsed_time_min, y = Species, fill = Species)) +
  geom_density_ridges(
    alpha = 0.7,
    scale = 3,
    from = 0  # <- start smoothing only from 0
  ) +
  #facet_wrap(~ Replicate, scales = "free_y") +
  scale_x_continuous(name = "Elapsed Time (minutes since video start)", expand = expansion(mult = c(0, 0.05))) +
  labs(y = "Species", title = "Smoothed Visitation Over Time by Species") +
  theme_classic() +
  theme(legend.position = "none")

