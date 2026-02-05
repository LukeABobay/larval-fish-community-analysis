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
volume_sampled_by_both_sides <- mocness_major_taxa_stations %>%
  distinct(transect_station_rep_year, mocness_side, net, .keep_all = TRUE) %>%
  group_by(transect_station_rep_year, net) %>%
  mutate(combined_volume_m3_best = sum(volume_m3_best)) %>%
  ungroup() %>%
  distinct(transect_station_rep_year, net, combined_volume_m3_best)

nets_major_taxa_wide <- mocness_major_taxa_stations %>%
  # Removing NAs for now, but there shouldn't be any to begin with
  filter(!is.na(individuals_in_tow)) %>%
  filter(!is.na(individuals_per_m3)) %>%
  group_by(transect, station, replicate, year, depth_mean_m, taxon) %>%
  mutate(sum_individuals = sum(individuals_in_tow)) %>%
  merge(., volume_sampled_by_both_sides, by = c("transect_station_rep_year", "net"), all.x = TRUE) %>%
  ungroup() %>%
  distinct(transect_station_rep_year_net, taxon, .keep_all = TRUE) %>%
  mutate(avg_taxa_conc = sum_individuals/combined_volume_m3_best) %>%
  select(project, cruise, year, collection_date, transect, replicate, station, net, 
         transect_station_rep_year, transect_station_rep_year_net, start_time_utc, start_time_pt, end_time_utc, end_time_pt, 
         start_longitude_dd, start_latitude_dd, end_longitude_dd, end_latitude_dd,
         maximum_depth_m, minimum_depth_m, depth_mean_m, depth_diff_m,
         mean_temperature_c, mean_salinity_psu, mean_density_kgm3, seafloor_depth_m,
         distance_to_shore_km, shelf_position, prey_zooplankton_abundance_ind_m3,
         dissolved_oxygen_ml_l, chlorophyll_ug_l, mlotst, taxon,
         avg_taxa_conc, combined_volume_m3_best, seawater_density_1000_kg_m3) %>%
  # For some reason, MOC 1 and MOC 4 have different values of mean_temperature_c, mean_salinity_psu, and mean_density_kgm3 in 6 cases. To eliminate differences, calculate mean
  group_by(transect_station_rep_year_net) %>%
  mutate(mean_temperature_c = mean(mean_temperature_c),
         mean_salinity_psu = mean(mean_salinity_psu),
         mean_density_kgm3 = mean(mean_density_kgm3),
         end_longitude_dd = mean(end_longitude_dd),
         end_latitude_dd = mean(end_latitude_dd)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = avg_taxa_conc, values_fill = 0)

nets_env_wide <- nets_major_taxa_wide %>%
  select(project, cruise, collection_date, year, replicate, transect_station_rep_year_net, start_time_pt,
         end_time_pt, start_latitude_dd, start_longitude_dd, end_longitude_dd, end_latitude_dd, 
         maximum_depth_m, minimum_depth_m, depth_mean_m, depth_diff_m, shelf_position,
         seafloor_depth_m, dissolved_oxygen_ml_l, distance_to_shore_km, seawater_density_1000_kg_m3, 
         chlorophyll_ug_l, mean_temperature_c, mean_salinity_psu, combined_volume_m3_best) %>%
  mutate(time_of_day = substr(replicate, 3, 3)) %>%
  mutate(time_of_day = recode(time_of_day, "D" = "Day", "N" = "Night")) %>%
  mutate(time_of_day = factor(time_of_day, levels = c("Day", "Night"))) %>%
  select(project, collection_date, year, transect_station_rep_year_net, time_of_day, start_time_pt,
         start_latitude_dd, shelf_position, seafloor_depth_m, dissolved_oxygen_ml_l, 
         mean_temperature_c, mean_salinity_psu, depth_mean_m, combined_volume_m3_best)
#removed mlotst and prey abundance for right now because both have NAs at the moment and I don't want this to cause errors down the line
#also excluded redundant information like transect, transect_station, transect_station_rep, and so on. Included only variables of interest
# removed chlorophyll until we have values for fluorescence


# Perform cluster analysis ------------------------------------------------

nets_AHC_comm_matrix <- nets_major_taxa_wide %>%
  select(transect_station_rep_year_net, depth_mean_m, Osmeridae, Sebastes, Liparis, Pleuronectidae_other, Parophrys_vetulus, Ammodytidae,
         Cottidae, Gadidae, Agonidae, Stichaeidae, Hexagrammidae, Myctophidae, Anarrhichthys_ocellatus, Lipolagus_ochotensis, Anoplopomatidae,
         Paralichthyidae, Sebastolobus, Bathylagidae, Ptilichthys_goodei, Pholidae, Chauliodus_macouni, Nansenia_candida, Trachipterus_altivelis, 
         Paralepididae, Merluccius_productus, Macrouridae, Artedius, Sardinops_sagax, Engraulis_mordax, Gobiidae, Ophidiidae)

nets_transform_taxa_concentrations <- nets_AHC_comm_matrix[, 3:33] %>%
  sqrt()

# Add rownames
row.names(nets_transform_taxa_concentrations) <- nets_AHC_comm_matrix$transect_station_rep_year_net

nets_AHC_comm_matrix_transformed <- nets_AHC_comm_matrix[,1:2] %>%
  bind_cols(.,nets_transform_taxa_concentrations)

# Calculate dissimilarity matrix
nets_dissim_matrix <- vegdist(nets_transform_taxa_concentrations, method = "bray", na.rm = TRUE)

# Perform agglomerative hierarchical clustering
nets_AHC_result <- hclust(nets_dissim_matrix, method = "average")

# Plot the dendrograms
windows()
plot(nets_AHC_result, labels = nets_AHC_comm_matrix_transformed$transect_station_rep_year_net, main = "average linkage AHC of net tows by LFC")
rect.hclust(nets_AHC_result, k = 10, border = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

# Extract list of sampling events belonging to each cluster
nets_clusters <- data.frame(transect_station_rep_year_net = names(cutree(nets_AHC_result, k = 10)),
                       cluster = cutree(nets_AHC_result, k = 10)) %>%
  mutate(approx_sample_size = mocness_major_taxa_stations %>%
           group_by(transect_station_rep_year_net) %>%
           summarise(sum(individuals_in_tow)) %>%
           ungroup())


# Plot abundance of each taxon, grouped by cluster ------------------------
  
# Add cluster identities to long version of AHC_comm_matrix_transformed
nets_AHC_comm_matrix_transformed_long <- nets_AHC_comm_matrix_transformed %>%
  pivot_longer(cols = 3:33, names_to = "taxon", values_to = "sqrt_concentration") %>%
  merge(., nets_clusters, by = "transect_station_rep_year_net")

# Categorize taxa by habitat affinity
coastal_species <- c("Agonidae", "Artedius", "Cottidae", "Hexagrammidae", "Liparis", "Paralichthyidae", "Parophrys_vetulus", "Pholidae", "Sebastes", 
                     "Stichaeidae", "Ammodytidae", "Gadidae", "Osmeridae", "Pleuronectidae_other", "Anoplopomatidae", "Anarrhichthys_ocellatus", "Sebastolobus",
                     "Ptilichthys_goodei", "Gobiidae")
coastal_colors <- colorRampPalette(brewer.pal(9, "Greens")[2:9])(length(coastal_species))

coastal_oceanic_species <- c("Engraulis_mordax", "Sardinops_sagax", "Nansenia_candida")
coastal_oceanic_colors <- colorRampPalette(brewer.pal(3, "Blues")[2:3])(length(coastal_oceanic_species))

oceanic_species <- c("Bathylagidae", "Chauliodus_macouni", "Lipolagus_ochotensis", "Macrouridae", "Myctophidae", "Paralepididae",
                     "Trachipterus_altivelis", "Merluccius_productus", "Ophidiidae")
oceanic_colors <- colorRampPalette(brewer.pal(9, "Purples")[2:9])(length(oceanic_species))

# Named species color vector
species_colors <- c(setNames(coastal_colors, coastal_species),
                    setNames(coastal_oceanic_colors, coastal_oceanic_species),
                    setNames(oceanic_colors, oceanic_species))

# Vector of taxa ordered alphabetically within categories to order bars and figure legends
ordered_taxa <- c(coastal_species, coastal_oceanic_species, oceanic_species)

# Plot by transect_station_rep_year, sorted by cluster
windows()
ggplot(nets_AHC_comm_matrix_transformed_long, aes(x = transect_station_rep_year_net, y = sqrt_concentration, fill = factor(taxon, levels = ordered_taxa))) +
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

ggplot(nets_stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
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

##time_of_day
###fit ellipses
nets_time_groups <- nets_env_wide_aligned$time_of_day

nets_ell_time <- ordiellipse(
  nets_NMDS_result,
  nets_time_groups,
  kind = "sd",
  conf = 0.95, 
  draw = "none"
)

### convert ellipse output to data frame
nets_ell_time_df <- purrr::map_dfr(names(nets_ell_time), ~ {
  e     <- nets_ell_time[[.x]]
  theta <- seq(0, 2 * pi, length.out = 200)
  circle <- cbind(cos(theta), sin(theta))
  
  xy <- circle %*% chol(e$cov)
  xy <- sweep(xy * e$scale, 2, e$center, "+")
  
  tibble(
    NMDS1 = xy[, 1],
    NMDS2 = xy[, 2],
    group = .x
  )
})

###overlay ellipses on NMDS plot
ggplot(nets_stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  geom_point(size = 3) +
  geom_path(data = nets_ell_time_df, aes(x = NMDS1, y = NMDS2, color = group),
            size = 1, linetype = 2) +
  scale_color_manual(values = c("indianred", "lightsalmon", "lightblue", "palegreen", "khaki", "plum", "turquoise", "pink", "tan3", "darkolivegreen4", "royalblue4", "orangered4")) +
  theme_classic() +
  labs(title = "NMDS Ordination with Clustered Points and Time of Day Ellipses",
       x = "NMDS1", y = "NMDS2")