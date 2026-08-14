# Description -------------------------------------------------------------

# Conduct cluster analysis of mean depths (rather than of events) by LFC. Plot
#clusters in stacked barplot and NMDS ordination with environmental variables
#overlayed as vectors


# Load packages -----------------------------------------------------------

library(here)
library(vegan)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(dplyr)
library(purrr)


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Create wide environmental dataframe ---------------------------------------------------

nets_major_taxa_wide <- mocness_major_taxa_nets %>%
  # Removing NAs for now, but there shouldn't be any to begin with
  filter(!is.na(individuals_in_tow)) %>%
  filter(!is.na(individuals_per_m3)) %>%
  select(project, year, cruise, collection_date, solar_dayness,
         transect, replicate, station, net,
         transect_station_rep_year_net, transect_station_rep_year, start_time_pt,
         start_longitude_dd, start_latitude_dd, maximum_depth_m, minimum_depth_m, 
         depth_mean_m, depth_diff_m, volume_best_m3_both_sides,
         mean_temperature_c, mean_salinity_psu, mean_density_kgm3, seafloor_depth_m,
         distance_to_shore_km, shelf_position, prey_zooplankton_abundance_ind_m3,
         dissolved_oxygen_ml_l, mean_chl_0_100_m_mgm3, any_of("mlotst"),
         taxon, individuals_per_m3) %>%
  # For some reason, MOC 1 and MOC 4 have different values of mean_temperature_c, mean_salinity_psu, and mean_density_kgm3 in 6 cases. To eliminate differences, calculate mean
  group_by(transect_station_rep_year_net) %>%
  mutate(mean_temperature_c = mean(mean_temperature_c),
         mean_salinity_psu = mean(mean_salinity_psu),
         mean_density_kgm3 = mean(mean_density_kgm3)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_per_m3, values_fill = 0)

nets_env_wide <- nets_major_taxa_wide %>%
  select(project, collection_date, year, transect_station_rep_year_net, solar_dayness, start_time_pt,
         start_latitude_dd, shelf_position, seafloor_depth_m, dissolved_oxygen_ml_l, 
         mean_temperature_c, mean_salinity_psu, mean_chl_0_100_m_mgm3, depth_mean_m, volume_best_m3_both_sides)
#removed mlotst and prey abundance for right now because both have NAs at the moment and I don't want this to cause errors down the line
#also excluded redundant information like transect, transect_station, transect_station_rep, and so on. Included only variables of interest
# removed chlorophyll until we have values for fluorescence


# Perform cluster analysis ------------------------------------------------

nets_metadata_cols <- c(
  "project", "year", "cruise", "collection_date", "solar_dayness",
  "transect", "replicate", "station", "net",
  "transect_station_rep_year_net", "transect_station_rep_year",
  "start_time_pt", "start_longitude_dd", "start_latitude_dd",
  "maximum_depth_m", "minimum_depth_m", "depth_mean_m", "depth_diff_m",
  "volume_best_m3_both_sides",
  "mean_temperature_c", "mean_salinity_psu", "mean_density_kgm3",
  "seafloor_depth_m", "distance_to_shore_km", "shelf_position",
  "prey_zooplankton_abundance_ind_m3", "dissolved_oxygen_ml_l",
  "mean_chl_0_100_m_mgm3", "mlotst"
)

nets_taxa_cols <- setdiff(names(nets_major_taxa_wide), nets_metadata_cols)

nets_AHC_comm_matrix <- nets_major_taxa_wide %>%
  select(transect_station_rep_year_net, depth_mean_m, all_of(nets_taxa_cols))

nets_transform_taxa_concentrations <- nets_AHC_comm_matrix[, nets_taxa_cols] %>%
  sqrt()

nets_empty_comm_rows <- rowSums(nets_AHC_comm_matrix[, nets_taxa_cols], na.rm = TRUE) == 0
if (any(nets_empty_comm_rows)) {
  stop(
    "Net community matrix has zero-abundance rows after taxon selection: ",
    paste(nets_AHC_comm_matrix$transect_station_rep_year_net[nets_empty_comm_rows], collapse = ", ")
  )
}

# Add rownames
row.names(nets_transform_taxa_concentrations) <- nets_AHC_comm_matrix$transect_station_rep_year_net

nets_AHC_comm_matrix_transformed <- nets_AHC_comm_matrix[,1:2] %>%
  bind_cols(.,nets_transform_taxa_concentrations)

# Calculate dissimilarity matrix
nets_dissim_matrix <- vegdist(nets_transform_taxa_concentrations, method = "bray", na.rm = TRUE)

# Perform agglomerative hierarchical clustering
nets_AHC_result <- hclust(nets_dissim_matrix, method = "average")

# Plot the dendrograms
plot(nets_AHC_result, labels = nets_AHC_comm_matrix_transformed$transect_station_rep_year_net, main = "average linkage AHC of net tows by LFC")
rect.hclust(nets_AHC_result, k = 10, border = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

# Extract list of sampling events belonging to each cluster
sample_size_df <- mocness_major_taxa_nets %>%
  group_by(transect_station_rep_year_net) %>%
  summarise(sample_size = sum(individuals_in_tow), .groups = "drop")

nets_clusters <- data.frame(
  transect_station_rep_year_net = names(cutree(nets_AHC_result, k = 10)),
  cluster = cutree(nets_AHC_result, k = 7)) %>%
  left_join(sample_size_df, by = "transect_station_rep_year_net")

# Plot abundance of each taxon, grouped by cluster ------------------------
  
# Add cluster identities to long version of AHC_comm_matrix_transformed
nets_AHC_comm_matrix_transformed_long <- nets_AHC_comm_matrix_transformed %>%
  pivot_longer(cols = all_of(nets_taxa_cols), names_to = "taxon", values_to = "concentration_transformed") %>%
  merge(., nets_clusters, by = "transect_station_rep_year_net")

# Plot by transect_station_rep_year, sorted by cluster
ggplot(nets_AHC_comm_matrix_transformed_long, aes(x = transect_station_rep_year_net, y = concentration_transformed, fill = factor(taxon, levels = ordered_taxa))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors, breaks = ordered_taxa) +
  facet_grid(rows = vars(cluster)) +
  labs(x = "Depth sampled (m)", y = "individuals/m3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot NMDS ordinations ---------------------------------------------------

nets_NMDS_result <- metaMDS(nets_dissim_matrix, distance = "bray", k = 2, try = 20, trymax = 20, engine = "monoMDS")
nets_NMDS_result$stress

stressplot(nets_NMDS_result)   ##Shepard diagram

nets_site_scores <- as.data.frame(scores(nets_NMDS_result, display = "sites"))
nets_cluster_groups <- cutree(nets_AHC_result, k = 10)
nets_station_scores <- mutate(nets_site_scores, transect_station_rep_year_net = nets_AHC_comm_matrix_transformed$transect_station_rep_year_net)
nets_stations_clustered <- mutate(nets_station_scores, cluster = nets_cluster_groups)
nets_stations_clustered$cluster <- as.numeric(as.character(nets_stations_clustered$cluster))
nets_stations_clustered$cluster <- factor(nets_stations_clustered$cluster, levels = c(1,2,3,4,5,6,7,8,9,10), 
                                          labels = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9", "Cluster 10"))

ggplot(filter(nets_stations_clustered, transect_station_rep_year_net != "TR_4_MaN_2019_2"), aes(x = NMDS1, y = NMDS2, color = cluster)) +
  scale_color_manual(values = c("indianred", "lightsalmon", "lightblue", "palegreen", "khaki", "plum", "turquoise", "pink", "tan3", "darkolivegreen4")) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = transect_station_rep_year_net), size = 3, max.overlaps = 10) +
  theme_classic() +
  labs(title = "NMDS Ordination of sampling events by LFC", x = "NMDS1", y = "NMDS2")   ##NMDS plot


# overlays for NMDS plots -------------------------------------------------

#Vectors for environmental variables
nets_env_wide_aligned <- nets_env_wide[match(rownames(scores(nets_NMDS_result, display = "sites")),
                                   nets_env_wide$transect_station_rep_year_net), ]
nets_env_numeric <- nets_env_wide_aligned[, sapply(nets_env_wide_aligned, is.numeric)]
nets_fit_vectors<- envfit(nets_NMDS_result, nets_env_numeric, permutations = 1000, na.rm = TRUE)

##Extract vector scores for plotting
nets_vector_scores <- scores(nets_fit_vectors, display = "vectors")
nets_vector_df <- as.data.frame(nets_vector_scores)
nets_vector_df$variable <- rownames(nets_vector_df)

##Plot NMDS with vector overlays
ggplot(nets_stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  scale_color_manual(values = c("indianred", "lightsalmon", "lightblue", "palegreen", "khaki", "plum", "turquoise", "pink", "tan3", "darkolivegreen4")) +
  geom_point(size = 3) +
  #geom_text_repel(aes(label = transect_station_rep_year_net), size = 3, max.overlaps = 10) +
  theme_classic() +
  labs(title = "NMDS Ordination of depth-stratified samples by LFC", x = "NMDS1", y = "NMDS2") +
  geom_segment(data = nets_vector_df,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", linewidth = 1) +
  geom_text(data = nets_vector_df,
            aes(x = NMDS1, y = NMDS2, label = variable),
            color = "black", size = 3, vjust = -0.5)

#Ellipses for categorical variables

##shelf_position
###fit ellipses
nets_ell_shelf <- ordiellipse(nets_NMDS_result, nets_env_wide_aligned$shelf_position,
                         kind = "sd", conf = 0.95, draw = "none") 

###convert ellipse output to data frames
nets_ell_shelf_df <- purrr::map_dfr(names(nets_ell_shelf), ~ {
  e     <- nets_ell_shelf[[.x]]
  theta <- seq(0, 2 * pi, length.out = 200)
  circle <- cbind(cos(theta), sin(theta))
  
  # one ellipse per group: center + scale * chol(cov) %*% circle
  xy <- circle %*% chol(e$cov)
  xy <- sweep(xy * e$scale, 2, e$center, "+")
  
  dplyr::tibble(
    NMDS1 = xy[, 1],
    NMDS2 = xy[, 2],
    group = .x
  )
})

###overlay ellipses on NMDS plot
ggplot(nets_stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  geom_point(size = 3) +
  geom_path(data = nets_ell_shelf_df, aes(x = NMDS1, y = NMDS2, color = group),
            linewidth = 1) +
  scale_color_manual(values = c("indianred", "lightsalmon", "lightblue", "palegreen", "khaki", "plum", "turquoise", "pink", "tan3", "darkolivegreen4", "royalblue4", "orangered4")) +
  theme_classic() +
  labs(title = "NMDS Ordination with Clustered Points and Shelf Position Ellipses",
       x = "NMDS1", y = "NMDS2")

# Fit db-RDA models and evaluate support for env vars ---------------------


