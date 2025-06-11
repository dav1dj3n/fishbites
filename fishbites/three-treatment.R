#install.packages("googlesheets4")
library(googlesheets4)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(vegan)

# 1. Read and inspect data

bites <- read.csv("/Users/cat/Dropbox/0 - NSF Timing Project 2025-27/david-and-anthony/fishbites/fishbites/turfturbremovaldata.csv")
View(bites)
unique(bites$Species)

table(bites$Replicate, bites$Treatment)
bites <- bites %>%
  filter(!is.na(Species), Species != "", Bites > 0)

glimpse(bites)

# 3. Summarize total bites per hour per species per replicate


bites_summary <- bites %>%
  group_by(Replicate, Treatment, Species) %>%
  summarise(
    total_bites   = sum(Bites, na.rm = TRUE),
    totaltime_hr  = unique(totaltimehr),  # or: first(totaltimehr)
    bite_rate     = total_bites / totaltime_hr,
    .groups = "drop"
  )

glimpse(bites_summary)



species_counts <- bites_summary %>%
  group_by(Species) %>%
  summarise(
    n_replicates = n_distinct(Replicate),
    total_bites = sum(total_bites)
  ) %>%
  arrange(n_replicates)

print(species_counts)

# 6. Visualization: total bites per species by treatment
ggplot(bites_summary, aes(x = Species, y = bite_rate, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              alpha = 0.5, size = 1) +
  labs(x = "Species", y = "Total Bites", title = "Bite Distribution by Species and Treatment") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 7. Pivot to wide format for multivariate analysis
bites_wide <- bites_summary %>%
  select(Replicate, Treatment, Species, bite_rate) %>%
  pivot_wider(
    names_from = Species,
    values_from = bite_rate,
    values_fn = sum,
    values_fill = 0
  )
community_data <- bites_wide[, 3:36]

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
 # geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2),
  #            arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  #geom_text(data = loadings, aes(x = PC1, y = PC2, label = Species),
  #         color = "black", size = 3, hjust = 0.5, vjust = -0.5) +
  theme_classic() +
  labs(
    title = "PCA of Fish Community Composition with Loadings",
    x = paste0("PC1 (", round(summary(pca)$importance[2, 1] * 100, 1), "% variance)"),
    y = paste0("PC2 (", round(summary(pca)$importance[2, 2] * 100, 1), "% variance)")
  )



#------------Filtered---------

# STEP 1: Filter species seen in at least 3 replicates
species_counts <- bites_summary %>%
  group_by(Species) %>%
  summarise(
    n_replicates = n_distinct(Replicate),
    total_bites = sum(total_bites)
  )

common_species <- species_counts %>%
  filter(n_replicates >= 3) %>%
  pull(Species)

bites_summary_filtered <- bites_summary %>%
  filter(Species %in% common_species)

# STEP 2: Pivot to wide format
bites_wide_filtered <- bites_summary_filtered %>%
  select(Replicate, Treatment, Species, bite_rate) %>%
  pivot_wider(
    names_from = Species,
    values_from = bite_rate,
    values_fn = sum,
    values_fill = 0
  )

# STEP 3: Separate metadata and community matrix
meta_filtered <- bites_wide_filtered %>% select(Replicate, Treatment)
community_data_filtered <- bites_wide_filtered %>% select(-Replicate, -Treatment)

# STEP 4: PCA
pca_filtered <- prcomp(community_data_filtered, scale. = TRUE)

# STEP 5: Combine PCA results with metadata
pca_df_filtered <- as.data.frame(pca_filtered$x) %>%
  bind_cols(meta_filtered)

# STEP 6: Loadings for species arrows (optional)
loadings_filtered <- as.data.frame(pca_filtered$rotation) %>%
  rownames_to_column(var = "Species") %>%
  mutate(
    PC1 = PC1 * 10,
    PC2 = PC2 * 10
  )

# STEP 7: Plot PCA
ggplot(pca_df_filtered, aes(x = PC1, y = PC2, color = Treatment)) +
  geom_point(size = 3) +
  stat_ellipse(type = "norm", linetype = "dashed") +
  # Uncomment to add loadings:
  # geom_segment(data = loadings_filtered, aes(x = 0, y = 0, xend = PC1, yend = PC2),
  #              arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  # geom_text(data = loadings_filtered, aes(x = PC1, y = PC2, label = Species),
  #           color = "black", size = 3, hjust = 0.5, vjust = -0.5) +
  theme_classic() +
  labs(
    title = "PCA of Fish Community Composition (Rare Species Removed)",
    x = paste0("PC1 (", round(summary(pca_filtered)$importance[2, 1] * 100, 1), "% variance)"),
    y = paste0("PC2 (", round(summary(pca_filtered)$importance[2, 2] * 100, 1), "% variance)")
  )


# Boxplot of bite rates for common species only

ggplot(bites_summary_filtered, aes(x = Species, y = bite_rate, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8)) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
              alpha = 0.5, size = 1) +
  labs(
    x = "Species",
    y = "Bite Rate (bites per hour)",
    title = "Bite Rates by Species and Treatment (Rare Species Removed)"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


