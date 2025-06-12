#LTERBENTHICDATA
#install.packages("patchwork")

library(tidyverse)
library(ggplot2)
library(stringr)
library(patchwork)


lter <- read.csv("mcrlterbenthicdata.csv")

lter2 <- lter %>%
  filter(Site == "LTER_2") %>%
  filter(Habitat == "Backreef") %>%
  filter(Year!= 2020)
 
 lter2wide <- pivot_wider(data = lter2 , 
            names_from = Taxonomy_Substrate_Functional_Group,
             values_from = Percent_Cover) 
 
 
head(lter2wide)

filter_lter2 <- lter2wide %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  mutate(
    percent_turb = `Turbinaria ornata` + `Sargassum pacificum`,
    percent_turf = `Algal Turf` + `Crustose Corallines`
  ) %>%
  group_by(Year) %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~mean(.),
        se = ~sd(.) / sqrt(n())
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

filter_lter2 <- filter_lter2%>%
  rename_with(~ str_replace(., "_mean$", ""))

head(filter_lter2)

#insert 2020 back with na
all_years <- tibble(Year = seq(min(filter_lter2$Year),
                               max(filter_lter2$Year)))

# Left join to insert NA for missing years
filter_lter2_complete <- all_years %>%
  left_join(filter_lter2, by = "Year")


#in our benthic format combining cca + turf/ sarg+turb

b3 <- ggplot(filter_lter2_complete, aes(x = percent_turf, y = percent_turb),) +
  geom_density_2d_filled(contour_var = "density") +
  labs(title = "Turf vs. Turbinaria Density",
       x = "Percent Turf",
       y = "Percent Turbinaria") +
  theme_classic()
b3

ggplot(filter_lter2_complete, aes(x = percent_turf, y = percent_turb)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Turf vs. Turbinaria Cover",
       x = "Percent Turf",
       y = "Percent Turbinaria") +
  theme_classic()

ggplot(filter_lter2_complete, aes(x = Year, y = percent_turb)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Turbinaria Cover over time",
       x = "year",
       y = "Percent Turbinaria") +
  theme_classic() 

ggplot(filter_lter2_complete, aes(x = Year, y = percent_turf)) +
  geom_point(alpha = 0.6, size = 2) +
  labs(title = "Turf cover over time",
       x = "year",
       y = "Percent Turbinaria") +
  theme_classic()



m5<- ggplot(filter_lter2_complete, aes(x = Year, y = percent_turb)) +
  geom_line(color = "#2c7fb8") +  
  geom_point(size = 2, color = "#2c7fb8") +
  geom_errorbar(aes(ymin = percent_turb - percent_turb_se,
                    ymax = percent_turb + percent_turb_se),
                width = 0.2) +
  labs(title = "Turbinaria Cover",
       x = "Year",
       y = "Percent Turbinaria (± SE)") +
  theme_classic()

m6<- ggplot(filter_lter2_complete, aes(x = Year, y = percent_turf)) +
  geom_line(color = "#2c7fb8") +  
  geom_point(size = 2, color = "#2c7fb8") +
  geom_errorbar(aes(ymin = percent_turf - percent_turf_se,
                    ymax = percent_turf + percent_turf_se),
                width = 0.2) +
  labs(title = "Turf Cover",
       x = "Year",
       y = "Percent Turf (± SE)") +
  theme_classic()

combined_plot2 <- (m5 | m6)
combined_plot2

#individually
#turb
m1 <- ggplot(filter_lter2_complete, aes(x = Year, y = `Turbinaria ornata`)) +
  geom_line(color = "#2c7fb8") +  
  geom_point(size = 2, color = "#2c7fb8") +
  geom_errorbar(aes(ymin = `Turbinaria ornata` - `Turbinaria ornata_se`,
                    ymax = `Turbinaria ornata` + `Turbinaria ornata_se`),
                width = 0.2) +
  labs(title = "Turbinaria Cover",
       x = "Year",
       y = "Percent Turbinaria (± SE)") +
  theme_classic()
#sarg
m2 <- ggplot(filter_lter2_complete, aes(x = Year, y = `Sargassum pacificum`)) +
  geom_line(color = "#2c7fb8") +  
  geom_point(size = 2, color = "#2c7fb8") +
  geom_errorbar(aes(ymin = `Sargassum pacificum` - `Sargassum pacificum_se`,
                    ymax = `Sargassum pacificum` + `Sargassum pacificum_se`),
                width = 0.2) +
  labs(title = "Sargassum Cover",
       x = "Year",
       y = "Percent Sargassum (± SE)") +
  theme_classic()
#turf
m3 <- ggplot(filter_lter2_complete, aes(x = Year, y = `Algal Turf`)) +
  geom_line(color = "#2c7fb8") +  
  geom_point(size = 2, color = "#2c7fb8") +
  geom_errorbar(aes(ymin = `Algal Turf` - `Algal Turf_se`,
                    ymax = `Algal Turf` + `Algal Turf_se`),
                width = 0.2) +
  labs(title = "Algal Turf Cover",
       x = "Year",
       y = "Percent Algal Turf (± SE)") +
  theme_classic()

#cca
m4<- ggplot(filter_lter2_complete, aes(x = Year, y = `Crustose Corallines`)) +
  geom_line(color = "#2c7fb8") +  
  geom_point(size = 2, color = "#2c7fb8") +
  geom_errorbar(aes(ymin = `Crustose Corallines` - `Crustose Corallines_se`,
                    ymax = `Crustose Corallines` + `Crustose Corallines_se`),
                width = 0.2) +
  labs(title = "Crustose Corallines Cover",
       x = "Year",
       y = "Percent Crustose Corallines (± SE)") +
  theme_classic()

combined_plot <- (m1 | m2) / (m3 | m4)
combined_plot
