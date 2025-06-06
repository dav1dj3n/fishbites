#install.packages("googlesheets4")
library(googlesheets4)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(vegan)

# 1. Read and inspect data

bites <- read.csv("/Users/cat/Dropbox/0 - NSF Timing Project 2025-27/david-and-anthony/fishbites/fishbites/fishbitedata.csv")
bites <- bites %>%
  filter(!is.na(Species), Species != "", totaltimehr > 0)

# 2. Standardize species names
corrections <- c(
  "Juvenile parrotfish"      = "Juvenile Parrotfish",
  "Juvenile wrasse"          = "Juvenile Wrasse",
  "mystery wrasse"           = "Mystery Wrasse",
  "Stethojulis bandanesis"   = "Stethojulis bandanensis",
  "Stethojulius bandanensis" = "Stethojulis bandanensis",
  "Ostracion melagris"       = "Ostracion meleagris",
  "Juvenile thalassoma"      = "Thalassoma hardwicke"
)

bites_clean <- bites %>%
  mutate(Species = recode(Species, !!!corrections)) %>%
  mutate(Species = case_when(
    Species %in% c("Juvenile Wrasse", "Mystery Wrasse", "Unknown wrasse") ~ "Other wrasse",
    TRUE ~ Species
  ))

# 3. Summarize total bites per species per replicate
bites_summary <- bites_clean %>%
  group_by(Replicate, Treatment, Species) %>%
  summarise(total_bites = sum(Bites, na.rm = TRUE), .groups = "drop")

# 4. Get unique replicate durations
unique_videos <- bites_clean %>%
  select(Replicate, Treatment, totaltimehr) %>%
  distinct()

# 5. Join duration with bite data and calculate bites per hour
bites_with_time <- bites_summary %>%
  left_join(unique_videos, by = c("Replicate", "Treatment")) %>%
  mutate(bites_per_hour = total_bites / totaltimehr)

# 6. Visualization: total bites per species by treatment
ggplot(bites_with_time, aes(x = Species, y = total_bites, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              alpha = 0.5, size = 1) +
  labs(x = "Species", y = "Total Bites", title = "Bite Distribution by Species and Treatment") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Pivot to wide format for multivariate analysis
bites_wide <- bites_with_time %>%
  select(Replicate, Treatment, Species, bites_per_hour) %>%
  pivot_wider(
    names_from = Species,
    values_from = bites_per_hour,
    values_fn = sum,
    values_fill = 0
  )
community_data <- bites_wide[, 3:33]

adonis2(community_data~Treatment, data=bites_wide, permutations=999) #highly significant.

################


# Step 4: Separate metadata (Replicate and Treatment) from species data
meta <- bites_wide %>%
  select(Replicate, Treatment)


# Step 5: Perform PCA on standardized species data
pca <- prcomp(community_data, scale. = TRUE)

# Step 6: Create dataframe with PCA results and metadata
pca_df <- as.data.frame(pca$x) %>%
  bind_cols(meta)

loadings <- as.data.frame(pca$rotation) %>%
  rownames_to_column(var = "Species")

arrow_multiplier <- 10
loadings <- loadings %>%
  mutate(PC1 = PC1 * arrow_multiplier,
         PC2 = PC2 * arrow_multiplier)

# Step 7: Plot PCA with confidence ellipses by Treatment
ggplot(pca_df, aes(x = PC1, y = PC2, color = Treatment)) +
  geom_point(size = 3) +
  stat_ellipse(type = "norm", linetype = "dashed") +
  #geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2),
   #            arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  #geom_text(data = loadings, aes(x = PC1, y = PC2, label = Species),
   #         color = "black", size = 3, hjust = 0.5, vjust = -0.5) +
  theme_classic() +
  labs(
    title = "PCA of Fish Community Composition with Loadings",
    x = paste0("PC1 (", round(summary(pca)$importance[2, 1] * 100, 1), "% variance)"),
    y = paste0("PC2 (", round(summary(pca)$importance[2, 2] * 100, 1), "% variance)")
  )
