
# Description -------------------------------------------------------------

# Conduct a cluster analysis


# Load packages -----------------------------------------------------------

library(here)
library(vegan)


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Create community matrix -------------------------------------------------

AHC_comm_matrix <- mocness_major_taxa %>%
  filter(!is.na(individuals_in_tow)) %>%
  group_by(transect_station_rep, taxon) %>%
  summarize(individuals_in_tow = sum(individuals_in_tow, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_in_tow, values_fill = 0)

transform_taxa_abundances <- AHC_comm_matrix[, 2:26] %>%
  sqrt()

AHC_comm_matrix_transformed <- AHC_comm_matrix[,1] %>%
  bind_cols(.,transform_taxa_abundances)


# Calculate dissimilarity matrix ------------------------------------------

dissim_matrix <- vegdist(transform_taxa_abundances, method = "bray")


# Perform agglomerative hierarchical clustering ---------------------------

AHC_result <- hclust(dissim_matrix, method = "average")


# Plot the dendrogram -----------------------------------------------------
## plot 2 clusters/rectanges
plot(AHC_result, labels = AHC_comm_matrix_transformed$transect_station_rep, main = "average linkage AHC of sampling events by LFC")
rect.hclust(AHC_result, k = 2, border = c(2, 4))

##plot 3 clusters/rectanges
plot(AHC_result, labels = AHC_comm_matrix_transformed$transect_station_rep, main = "average linkage AHC of sampling events by LFC")
rect.hclust(AHC_result, k = 3, border = c(2, 3, 4))

