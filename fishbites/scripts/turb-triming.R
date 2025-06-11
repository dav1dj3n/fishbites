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
data <- data %>%
  filter(!is.na(Bites) & Bites > 0)


table(data$trim, data$Replicate)
# Example start times for each replicate (just time, no date)
video_starts <- tibble(
  Replicate = 1:8,
  start_time = hms::as_hms(c("09:13:00", "09:28:00", "09:34:00", "09:36:00", 
                             "09:39:00", "09:39:00", "09:39:00", "09:41:00")))




# Step 1: Convert timevi to POSIXct or numeric
data_bites <- data %>%
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



# ---- 2. try something new ----

# Load required libraries
library(dplyr)
library(ggplot2)
library(lme4)

# Step 1: Create 10-minute time bins for elapsed_time_min
visits_binned <- visits_raw %>%
  mutate(
    time_bin = cut(
      elapsed_time_min,
      breaks = seq(0, max(elapsed_time_min, na.rm = TRUE) + 10, by = 10),
      right = FALSE,          # Intervals like [0,10), [10,20), ...
      labels = FALSE          # Label bins as integers 1, 2, 3, ...
    )
  )

# Step 2: Calculate average Bites by Species and time_bin
avg_bites <- visits_binned %>%
  group_by(Species, time_bin) %>%
  summarize(
    mean_bites = mean(Bites, na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Plot heatmap of mean bites over time bins for each species
ggplot(avg_bites, aes(x = time_bin, y = Species, fill = mean_bites)) +
  geom_tile(color = "gray80") +                       # Tiles colored by mean bites
  scale_fill_viridis_c(option = "magma", na.value = "white") + # Nice color scale
  labs(
    x = "Elapsed Time (10-minute bins)",
    y = "Species",
    fill = "Mean Bites"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))       # Rotate x-axis labels

# Step 4: Fit mixed-effects regression model to examine effect of elapsed time on Bites
# Make sure 'Replicate' is a factor for random effects
visits_raw <- visits_raw %>%
  mutate(Replicate = factor(Replicate))



species_counts <- visits_raw %>%
  group_by(Species) %>%
  summarise(n_obs = n()) %>%
  filter(n_obs >= 10)  # choose threshold

filtered_data <- visits_raw %>%
  filter(Species %in% species_counts$Species)

library(lmerTest)

model_no_time <- lmer(Bites ~ Species + (1 | Replicate), data = filtered_data, REML = FALSE)
model_with_time <- lmer(Bites ~ elapsed_time_min + Species + (1 | Replicate), data = filtered_data, REML = FALSE)

anova(model_no_time, model_with_time)


anova(model_reduced, model_full)



# Optional: Model with species-specific time slopes
model_species_slope <- lmer(Bites ~ elapsed_time_min + (elapsed_time_min | Species) + (1 | Replicate), data = visits_raw)
summary(model_species_slope)




