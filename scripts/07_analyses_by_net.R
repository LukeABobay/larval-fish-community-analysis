# Description -------------------------------------------------------------

# Conduct cluster analysis of mean depths (rather than of events) by LFC. Plot
#clusters in stacked barplot and NMDS ordination with environmental variables
#overlayed as vectors

#Note to self (steps):
#cluster analysis
#plot NMDS by net and overlay vectors and ellipses


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
  select(project, cruise, collection_date, replicate, transect_station_rep_year_net, start_time_pt,
         end_time_pt, start_latitude_dd, start_longitude_dd, end_longitude_dd, end_latitude_dd, 
         maximum_depth_m, minimum_depth_m, depth_mean_m, depth_diff_m, shelf_position,
         seafloor_depth_m, dissolved_oxygen_ml_l, distance_to_shore_km, seawater_density_1000_kg_m3, 
         chlorophyll_ug_l, mean_temperature_c, mean_salinity_psu, combined_volume_m3_best) %>%
  mutate(time_of_day = substr(replicate, 3, 3)) %>%
  mutate(time_of_day = recode(time_of_day, "D" = "Day", "N" = "Night")) %>%
  mutate(time_of_day = factor(time_of_day, levels = c("Day", "Night"))) %>%
  select(project, collection_date, transect_station_rep_year_net, time_of_day, start_time_pt,
         start_latitude_dd, start_longitude_dd, shelf_position,
         seafloor_depth_m, dissolved_oxygen_ml_l, seawater_density_1000_kg_m3, chlorophyll_ug_l, 
         mean_temperature_c, mean_salinity_psu, depth_mean_m, depth_diff_m, combined_volume_m3_best)
#removed mlotst and prey abundance for right now because both have NAs at the moment and I don't want this to cause errors down the line
#also excluded redundant information like transect, transect_station, transect_station_rep, and so on

# Categorize taxa by habitat affinity -------------------------------------------------

coastal_species <- c("Agonidae", "Artedius", "Cottidae", "Hexagrammidae", "Liparis", "Paralichthyidae", "Parophrys_vetulus", "Pholidae", "Pleuronectidae", "Sebastes", "Stichaeidae", "Ammodytidae", "Gadidae", "Osmeridae", "Pleuronectidae_other")
coastal_colors <- colorRampPalette(brewer.pal(9, "Greens")[2:9])(length(coastal_species))

coastal_oceanic_species <- c("Engraulis_mordax", "Sardinops_sagax")
coastal_oceanic_colors <- colorRampPalette(brewer.pal(3, "Blues")[2:3])(length(coastal_oceanic_species))

oceanic_species <- c("Bathylagidae", "Chauliodus_macouni", "Lestidiops_ringens", "Lipolagus_ochotensis", "Macrouridae", "Myctophidae", "Paralepididae")
oceanic_colors <- colorRampPalette(brewer.pal(9, "Purples")[2:9])(length(oceanic_species))

# Named species color vector
species_colors <- c(setNames(coastal_colors, coastal_species),
                    setNames(coastal_oceanic_colors, coastal_oceanic_species),
                    setNames(oceanic_colors, oceanic_species))

# Vector of taxa ordered alphabetically within categories to order bars and figure legends
ordered_taxa <- c(coastal_species, coastal_oceanic_species, oceanic_species)


# Perform cluster analysis ------------------------------------------------

nets_AHC_comm_matrix <- nets_major_taxa_wide %>%
  select(transect_station_rep_year_net, depth_mean_m, Sebastes, Liparis, Cottidae, Osmeridae, Ammodytidae, Gadidae, Pleuronectidae_other, 
         Parophrys_vetulus, Pholidae, Agonidae, Stichaeidae, Hexagrammidae, Myctophidae, Lipolagus_ochotensis, Anarrhichthys_ocellatus,
         Anoplopomatidae, Sebastolobus, Paralichthyidae, Bathylagidae, Ptilichthys_goodei, Ophidiidae, Chauliodus_macouni, Nansenia_candida, 
         Paralepididae, Trachipterus_altivelis, Merluccius_productus, Macrouridae, Artedius, Sardinops_sagax, Engraulis_mordax, Gobiidae,
         Ronquilus_jordani, Pleuronectidae, Chauliodontidae, Cryptacanthodes)

nets_transform_taxa_concentrations <- nets_AHC_comm_matrix[, 3:37] %>%
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
