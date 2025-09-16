
# Description -------------------------------------------------------------

# Conduct a cluster analysis


# Load packages -----------------------------------------------------------

library(here)
library(vegan)
library(ggplot2)
library(ggrepel)


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Create community matrix -------------------------------------------------

AHC_comm_matrix <- mocness_major_taxa_2018_2019 %>%
  filter(!is.na(individuals_in_tow)) %>%
  group_by(transect_station_rep, taxon) %>%
  summarize(individuals_in_tow = sum(individuals_in_tow, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_in_tow, values_fill = 0)

transform_taxa_abundances <- AHC_comm_matrix[, 2:22] %>%
  sqrt()

AHC_comm_matrix_transformed <- AHC_comm_matrix[,1] %>%
  bind_cols(.,transform_taxa_abundances)


# Calculate dissimilarity matrix ------------------------------------------

dissim_matrix <- vegdist(transform_taxa_abundances, method = "bray")


# Perform agglomerative hierarchical clustering ---------------------------

AHC_result <- hclust(dissim_matrix, method = "average")


# Plot the dendrograms -----------------------------------------------------

## plot 2 clusters/rectangles
plot(AHC_result, labels = AHC_comm_matrix_transformed$transect_station_rep, main = "average linkage AHC of sampling events by LFC")
rect.hclust(AHC_result, k = 2, border = c(2, 4))

##plot 3 clusters/rectangles
plot(AHC_result, labels = AHC_comm_matrix_transformed$transect_station_rep, main = "average linkage AHC of sampling events by LFC")
rect.hclust(AHC_result, k = 3, border = c(2, 3, 4))


# Plot NMDS ordinations ---------------------------------------------------

NMDS_result <- metaMDS(dissim_matrix, distance = "bray", k = 2, try = 20, trymax = 20, engine = "monoMDS")
NMDS_result$stress  ##check stress

stressplot(NMDS_result)   ##Shepard diagram

site_scores <- as.data.frame(scores(NMDS_result, display = "sites"))
cluster_groups <- cutree(AHC_result, k = 2)
station_scores <- mutate(site_scores, transect_station_rep = AHC_comm_matrix_transformed$transect_station_rep)
stations_clustered <- mutate(station_scores, cluster = cluster_groups)
stations_clustered$cluster <- as.numeric(as.character(stations_clustered$cluster))
stations_clustered$cluster <- factor(stations_clustered$cluster, levels = c(1,2), labels = c("Cluster 1", "Cluster 2"))

ggplot(stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  scale_color_manual(values = c("red", "blue")) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = transect_station_rep), size = 3, max.overlaps = 10) +
  theme_classic() +
  labs(title = "NMDS Ordination of sampling events by LFC", x = "NMDS1", y = "NMDS2") 
