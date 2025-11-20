
# Description -------------------------------------------------------------

# Conduct a cluster analysis of sampling events by LFC, plot clusters of 
#sampling events in dendrograms, run an NMDS, and plot the NMDS ordination


# Load packages -----------------------------------------------------------

library(here)
library(vegan)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Create wide environmental dataframe ---------------------------------------------------

wide_major_taxa_stations <- mocness_major_taxa_stations %>%
  # Removing NAs for now, but there shouldn't be any to begin with
  filter(!is.na(individuals_in_tow)) %>%
  filter(!is.na(individuals_per_m3)) %>%
  group_by(project, collection_date, start_time_pt, replicate, depth_range, 
           transect_station, transect, station, start_latitude_dd, start_longitude_dd, 
           taxon, transect_station_rep, transect_station_rep_year, seafloor_depth_m, 
           shelf_position,prey_zooplankton_abundance_ind_m3, dissolved_oxygen_ml_l, 
           seawater_density_1000_kg_m3, chlorophyll_ug_l, mlotst, 
           mean_temperature_c, mean_salinity_psu) %>%
  summarize(individuals_per_m3 = sum(individuals_per_m3)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_per_m3, values_fill = 0)

env_wide <- wide_major_taxa_stations %>%
  select(project, collection_date, transect_station_rep_year, start_time_pt,
         start_latitude_dd, start_longitude_dd, depth_range, shelf_position,
         seafloor_depth_m, prey_zooplankton_abundance_ind_m3, dissolved_oxygen_ml_l,
         seawater_density_1000_kg_m3, chlorophyll_ug_l, mean_temperature_c, mean_salinity_psu)
#removed mlotst for right now because all are NAs at the moment and I don't want this to cause errors down the line
#also excluded redundant information like transect, transect_station, replicat, transect_station_rep, and so on

# Create community matrix -------------------------------------------------

AHC_comm_matrix <- mocness_major_taxa_stations %>%
  filter(!is.na(individuals_in_tow)) %>%
  filter(!is.na(individuals_per_m3)) %>%
  group_by(transect_station_rep_year, taxon) %>%
  summarize(individuals_per_m3 = sum(individuals_per_m3, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_per_m3, values_fill = 0)

transform_taxa_concentrations <- AHC_comm_matrix[, 2:24] %>%
  sqrt()

# Add rownames
row.names(transform_taxa_concentrations) <- AHC_comm_matrix$transect_station_rep_year

AHC_comm_matrix_transformed <- AHC_comm_matrix[,1] %>%
  bind_cols(.,transform_taxa_concentrations)


# Calculate dissimilarity matrix ------------------------------------------

dissim_matrix <- vegdist(transform_taxa_concentrations, method = "bray")


# Perform agglomerative hierarchical clustering ---------------------------

AHC_result <- hclust(dissim_matrix, method = "average")


# Plot the dendrograms -----------------------------------------------------

## plot 2 clusters/rectangles
plot(AHC_result, labels = AHC_comm_matrix_transformed$transect_station_rep_year, main = "average linkage AHC of sampling events by LFC")
rect.hclust(AHC_result, k = 2, border = c(2, 4))

##plot 3 clusters/rectangles
plot(AHC_result, labels = AHC_comm_matrix_transformed$transect_station_rep_year, main = "average linkage AHC of sampling events by LFC")
rect.hclust(AHC_result, k = 5, border = c(2, 3, 4, 5, 6))

# Extract list of sampling events belonging to each cluster
clusters <- data.frame(transect_station_rep_year = names(cutree(AHC_result, k = 5)),
                       cluster = cutree(AHC_result, k = 5))


# Plot abundance of each taxon, grouped by cluster ------------------------

# Add cluster identities to long version of AHC_comm_matrix_transformed
AHC_comm_matrix_transformed_long <- AHC_comm_matrix_transformed %>%
  pivot_longer(cols = 2:24, names_to = "taxon", values_to = "sqrt_concentration") %>%
  merge(., clusters, by = "transect_station_rep_year")

# Categories of taxa in AHC_comm_matrix_transformed
coastal_species <- c("Agonidae", "Artedius", "Cottidae", "Hexagrammidae", "Liparis", "Paralichthyidae", "Parophrys vetulus", "Pholidae", "Pleuronectidae", "Sebastes", "Stichaeidae", "Ammodytes", "Microgadus proximus", "Osmeridae")
coastal_colors <- colorRampPalette(brewer.pal(9, "Greens")[2:9])(length(coastal_species))

coastal_oceanic_species <- c("Engraulis mordax", "Sardinops sagax")
coastal_oceanic_colors <- colorRampPalette(brewer.pal(3, "Blues")[2:3])(length(coastal_oceanic_species))

oceanic_species <- c("Bathylagus pacificus", "Chauliodus macouni", "Lestidiops ringens", "Lipolagus ochotensis", "Macrouridae", "Myctophidae", "Paralepididae")
oceanic_colors <- colorRampPalette(brewer.pal(9, "Purples")[2:9])(length(oceanic_species))

# Named species color vector
species_colors <- c(setNames(coastal_colors, coastal_species),
                    setNames(coastal_oceanic_colors, coastal_oceanic_species),
                    setNames(oceanic_colors, oceanic_species))

# Vector of taxa ordered alphabetically within categories to order bars and figure legends
ordered_taxa <- c(coastal_species, coastal_oceanic_species, oceanic_species)

# Plot by transect_station_rep_year, sorted by cluster
windows()
ggplot(AHC_comm_matrix_transformed_long, aes(x = transect_station_rep_year, y = sqrt_concentration, fill = factor(taxon, levels = ordered_taxa))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors, breaks = ordered_taxa) +
  facet_grid(rows = vars(cluster)) +
  labs(x = "Depth sampled (m)", y = "individuals/m3") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##not sure if my adjustments here to account for standardizing counts by volume were correct and/or needed


# Plot NMDS ordinations ---------------------------------------------------

NMDS_result <- metaMDS(dissim_matrix, distance = "bray", k = 2, try = 20, trymax = 20, engine = "monoMDS")
NMDS_result$stress  ##check stress

stressplot(NMDS_result)   ##Shepard diagram

site_scores <- as.data.frame(scores(NMDS_result, display = "sites"))
cluster_groups <- cutree(AHC_result, k = 3)
station_scores <- mutate(site_scores, transect_station_rep_year = AHC_comm_matrix_transformed$transect_station_rep_year)
stations_clustered <- mutate(station_scores, cluster = cluster_groups)
stations_clustered$cluster <- as.numeric(as.character(stations_clustered$cluster))
stations_clustered$cluster <- factor(stations_clustered$cluster, levels = c(1,2,3), labels = c("Cluster 1", "Cluster 2", "Cluster 3"))

ggplot(stations_clustered, aes(x = NMDS1, y = NMDS2, color = cluster)) +
  scale_color_manual(values = c("red", "blue", "black")) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = transect_station_rep_year), size = 3, max.overlaps = 10) +
  theme_classic() +
  labs(title = "NMDS Ordination of sampling events by LFC", x = "NMDS1", y = "NMDS2")   ##NMDS plot


# overlays for NMDS plots -------------------------------------------------

##vectors for environmental variables
env_wide_aligned <- env_wide[match(rownames(scores(NMDS_result, display = "sites")),
                                   rownames(env_wide)), ]
env_numeric <- env_wide_aligned[, sapply(env_wide_aligned, is.numeric)]

fit_vectors<- envfit(NMDS_result, env_numeric, permutations = 1000, na.rm = TRUE)
