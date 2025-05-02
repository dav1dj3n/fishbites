#install.packages("googlesheets4")
library(googlesheets4)
library(tidyverse)
library(ggplot2)
library(dplyr)

# Using the full URL
sheet_url <- "https://docs.google.com/spreadsheets/d/1uQtkEVd-T6L_D8-luRIB81lh5zaAWMv2t1TvC2N8KGI/edit?gid=0#gid=0"

# Read the first sheet (or specify by name or position)
bites <- read_sheet(sheet_url)
head(bites)

unique(bites$Species)

bites <- bites %>%
  filter(!is.na(Species))

bites_summary <- bites %>%
  group_by(Replicate, Treatment, Species) %>%
  summarise(total_bites = sum(Bites, na.rm = TRUE), .groups = "drop")

print(bites_summary)


ggplot(bites_summary, aes(x = Species, y = total_bites, fill = Treatment)) +
  geom_col(position = "dodge") +
  labs(x = "Species", y = "Total Bites", title = "Bites by Species and Treatment") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(bites_summary, aes(x = Species, y = total_bites, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
              alpha = 0.5, size = 1) +
  labs(x = "Species", y = "Total Bites", title = "Bite Distribution by Species and Treatment") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



################

community_matrix <- bites_summary %>%
  pivot_wider(names_from = Species, values_from = total_bites, values_fill = 0)

# Step 4: Separate metadata (Replicate and Treatment) from species data
meta <- community_matrix %>%
  select(Replicate, Treatment)

species_data <- community_matrix %>%
  select(-Replicate, -Treatment)

# Step 5: Perform PCA on standardized species data
pca <- prcomp(species_data, scale. = TRUE)

# Step 6: Create dataframe with PCA results and metadata
pca_df <- as.data.frame(pca$x) %>%
  bind_cols(meta)

# Step 7: Plot PCA with confidence ellipses by Treatment
ggplot(pca_df, aes(x = PC1, y = PC2, color = Treatment)) +
  geom_point(size = 3) +
  stat_ellipse(type = "norm", linetype = "dashed") +
  theme_classic() +
  labs(
    title = "PCA of Fish Community Composition",
    x = paste0("PC1 (", round(summary(pca)$importance[2, 1] * 100, 1), "% variance)"),
    y = paste0("PC2 (", round(summary(pca)$importance[2, 2] * 100, 1), "% variance)")
  )
