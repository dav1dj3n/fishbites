#LTERBENTHICDATA
library(tidyverse)
library(ggplot2)
library(stringr)

lter <- read.csv("mcrlterbenthicdata.csv")

lter2 <- lter %>%
  filter(site == "LTER_2") %>%
  filter(year == "2024")
  
head(lter2)



filter_lter2 <- lter2 %>%
  mutate(
    macroalgae = if_else(is.na(macroalgae), "0", as.character(macroalgae)),
    ctb = if_else(is.na(ctb), "0", as.character(ctb))
  ) %>%
  filter(
    !str_detect(tolower(macroalgae), "bw"),
    !str_detect(tolower(ctb), "bw")
  )%>%
  mutate(macroalgae = as.numeric(macroalgae), ctb = as.numeric(ctb))
head(filter_lter2)

b1 <- ggplot(filter_lter2, aes(x = macroalgae)) +
  geom_histogram(binwidth = 5, fill = "#E16A86", color = "black") +
  labs(title = "Distribution of macroalgal Cover (%)",
       x = "Percent macroalgae",
       y = "Count") +
  theme_classic()


b2 <- ggplot(filter_lter2, aes(x = ctb)) +
  geom_histogram(binwidth = 5, fill = "#69B3A2", color = "black") +
  labs(title = "Distribution of Turf Cover (%)",
       x = "Percent Turf",
       y = "Count") +
  theme_classic()
b2


b3 <- ggplot(filter_lter2, aes(x = ctb, y = macroalgae)) +
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
b3
