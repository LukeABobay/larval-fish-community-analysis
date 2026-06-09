# Description -------------------------------------------------------------

#Run objective 2 analyses and plots with only 4 main clusters

# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)


# Source code -------------------------------------------------------------

source(here("scripts/03_run_cluster_and_NMDS.R"))


# Filter data frames to main clusters only --------------------------------------------

main_clust_samples <- clusters %>% filter(cluster %in% main_clusters)

main_clust_wide_major_taxa_nets <- wide_major_taxa_nets %>% 
  semi_join(main_clust_samples, by = "transect_station_rep_year_net")

main_clust_env_wide <- main_clust_wide_major_taxa_nets %>%
  mutate(time_of_day = substr(replicate, 3, 3),
         time_of_day = recode(time_of_day, "D" = "Day", "N" = "Night", .default = NA_character_)) %>%
  group_by(transect_station_rep_year_net) %>%   # or collection_date, or station, etc.
  mutate(
    # compute sunrise/sunset at that station/date
    sunrise = getSunlightTimes(date = as.Date(collection_date),
                               lat  = first(start_latitude_dd),
                               lon  = first(start_longitude_dd),
                               keep = c("sunrise", "sunset"))$sunrise,
    sunset  = getSunlightTimes(date = as.Date(collection_date),
                               lat  = first(start_latitude_dd),
                               lon  = first(start_longitude_dd),
                               keep = c("sunrise", "sunset"))$sunset,
    time_of_day = case_when(!is.na(time_of_day) ~ time_of_day,
                            start_time_pt >= sunrise & start_time_pt < sunset ~ "Day",
                            TRUE                                              ~ "Night"),
    time_of_day = factor(time_of_day, levels = c("Day", "Night"))) %>%
  ungroup() %>%
  select(-sunrise, -sunset)


# Recompute matrices ------------------------------------------------------

main_clust_AHC_comm_matrix <- main_clust_wide_major_taxa_nets %>%
  select(transect_station_rep_year_net, chrono_sample_ID, depth_mean_m, 29:50)

main_clust_taxa_cols <- names(main_clust_AHC_comm_matrix)[4:ncol(main_clust_AHC_comm_matrix)]

main_clust_transform_taxa_concentrations <- main_clust_AHC_comm_matrix[, taxa_cols] %>% sqrt()

# Add rownames
row.names(main_clust_transform_taxa_concentrations) <- main_clust_AHC_comm_matrix$transect_station_rep_year_net

main_clust_AHC_comm_matrix_transformed <- main_clust_AHC_comm_matrix[1:2] %>%
  bind_cols(.,main_clust_transform_taxa_concentrations)

main_clust_dissim_matrix <- vegdist(main_clust_transform_taxa_concentrations, method = "bray")
#RM: did not recompute count matrix for Dexter et al. (2018) NMDS stress null model, will come back to this


# Perform agglomerative hierarchical clustering ---------------------------

main_clust_AHC_result <- hclust(main_clust_dissim_matrix, method = "average")

main_clust_cluster_colors <- c("1" = "#1F77B4", "2" = "#FF7F0E", "3" = "#8C564B", "4" = "#D62728")
main_clust_cluster_levels <- as.character(seq_len(4))
main_clust_dendrogram_clusters <- cutree(main_clust_AHC_result, k = 4)
main_clust_dendrogram_cluster_order <- unique(main_clust_dendrogram_clusters[main_clust_AHC_result$order])
main_clust_dendrogram_cluster_colors <- main_clust_cluster_colors[as.character(main_clust_dendrogram_cluster_order)]

#Plot dendrogram
png(filename = here("output/main_clusters_AHC_sampling_events_dendrogram.png"),
    width = 12,
    height = 6,
    units = "in",
    res = 300)
plot(main_clust_AHC_result, labels = main_clust_AHC_comm_matrix_transformed$chrono_sample_ID,
     xlab = "Net tows", main = "Clusters of Net Tows Within Main 4 Clusters", cex = 0.4)
rect.hclust(main_clust_AHC_result, k = 4, border = main_clust_dendrogram_cluster_colors)
dev.off()

# Extract list of sampling events belonging to each cluster
main_clust_new_clusters <- data.frame(transect_station_rep_year_net = names(main_clust_dendrogram_clusters),
                                          cluster = main_clust_dendrogram_clusters)


# Indicator Species Analysis ----------------------------------------------

main_clust_comm_for_isa <- main_clust_AHC_comm_matrix_transformed %>%
  select(3:24) %>% as.data.frame()

main_clust_new_clusters_for_isa <- as.factor(main_clust_new_clusters$cluster)

main_clust_new_isa_result <- multipatt(main_clust_comm_for_isa, main_clust_new_clusters_for_isa, func = "IndVal.g", max.order = 2)
summary(main_clust_new_isa_result)


# Map points in space by cluster and net ----------------------------------




# Plot abundance of each taxon, grouped by cluster ------------------------




# Plot NMDS ordination ---------------------------------------------------





# overlays for NMDS plots -------------------------------------------------