# Description -------------------------------------------------------------

# Prepare shared community data, dissimilarity matrix, and cluster
# assignments for assemblage analyses.


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(vegan)


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Create wide environmental dataframe -------------------------------------

wide_major_taxa_nets <- mocness_major_taxa_nets %>%
  # Removing NAs for now, but there shouldn't be any to begin with
  filter(!is.na(individuals_in_tow)) %>%
  filter(!is.na(individuals_per_m3)) %>%
  select(project, year, cruise, collection_date, start_time_pt, solar_dayness,
         transect, replicate, station, net,
         transect_station_rep_year_net, transect_station_rep_year,
         start_longitude_dd, start_latitude_dd, maximum_depth_m, minimum_depth_m,
         depth_mean_m, depth_diff_m, volume_best_m3_both_sides,
         mean_temperature_c, mean_salinity_psu, mean_density_kgm3, seafloor_depth_m,
         distance_to_shore_km, shelf_position, prey_zooplankton_abundance_ind_m3,
         dissolved_oxygen_ml_l, mean_chl_0_100_m_mgm3,
         taxon, individuals_per_m3) %>%
  # For some reason, MOC 1 and MOC 4 have different values of mean_temperature_c,
  # mean_salinity_psu, and mean_density_kgm3 in 6 cases. To eliminate differences,
  # calculate mean.
  group_by(transect_station_rep_year_net) %>%
  mutate(mean_temperature_c = mean(mean_temperature_c),
         mean_salinity_psu = mean(mean_salinity_psu),
         mean_density_kgm3 = mean(mean_density_kgm3)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_per_m3, values_fill = 0) %>%
  # Assign net tows unique sample IDs chronologically
  arrange(start_time_pt) %>%
  mutate(chrono_sample_ID = row_number())

env_wide <- wide_major_taxa_nets


# Create community matrix -------------------------------------------------

AHC_metadata_cols <- c(
  "project", "year", "cruise", "collection_date", "start_time_pt", "solar_dayness",
  "transect", "replicate", "station", "net",
  "transect_station_rep_year_net", "transect_station_rep_year",
  "start_longitude_dd", "start_latitude_dd",
  "maximum_depth_m", "minimum_depth_m", "depth_mean_m", "depth_diff_m",
  "volume_best_m3_both_sides",
  "mean_temperature_c", "mean_salinity_psu", "mean_density_kgm3",
  "seafloor_depth_m", "distance_to_shore_km", "shelf_position",
  "prey_zooplankton_abundance_ind_m3", "dissolved_oxygen_ml_l",
  "mean_chl_0_100_m_mgm3", "chrono_sample_ID"
)

taxa_cols <- setdiff(names(wide_major_taxa_nets), AHC_metadata_cols)

AHC_comm_matrix <- wide_major_taxa_nets %>%
  select(transect_station_rep_year_net, chrono_sample_ID, depth_mean_m, all_of(taxa_cols))

transform_taxa_concentrations <- AHC_comm_matrix[, taxa_cols] %>%
  sqrt()

empty_comm_rows <- rowSums(AHC_comm_matrix[, taxa_cols], na.rm = TRUE) == 0
if (any(empty_comm_rows)) {
  stop(
    "Community matrix has zero-abundance rows after taxon selection: ",
    paste(AHC_comm_matrix$transect_station_rep_year_net[empty_comm_rows], collapse = ", ")
  )
}

row.names(transform_taxa_concentrations) <- AHC_comm_matrix$transect_station_rep_year_net

AHC_comm_matrix_transformed <- AHC_comm_matrix[1:2] %>%
  bind_cols(., transform_taxa_concentrations)


# Count matrix for Dexter et al. (2018) NMDS stress null model ------------

# The quasiswap_count null model preserves row totals, taxon totals, and zeros
wide_major_taxa_counts_nets <- mocness_major_taxa_nets %>%
  filter(!is.na(individuals_in_tow)) %>%
  mutate(individuals_in_tow = as.integer(round(individuals_in_tow))) %>%
  select(transect_station_rep_year_net, taxon, individuals_in_tow) %>%
  pivot_wider(names_from = taxon,
              values_from = individuals_in_tow,
              values_fill = 0,
              values_fn = sum)

AHC_count_abundances <- wide_major_taxa_counts_nets %>%
  select(transect_station_rep_year_net, all_of(taxa_cols))

AHC_count_abundances <- AHC_count_abundances[match(AHC_comm_matrix$transect_station_rep_year_net,
                                                   AHC_count_abundances$transect_station_rep_year_net), ]

stopifnot(all(AHC_count_abundances$transect_station_rep_year_net ==
                AHC_comm_matrix$transect_station_rep_year_net))

AHC_sample_volumes <- wide_major_taxa_nets %>%
  select(transect_station_rep_year_net, volume_best_m3_both_sides) %>%
  distinct()

AHC_sample_volumes <- AHC_sample_volumes[match(AHC_comm_matrix$transect_station_rep_year_net,
                                               AHC_sample_volumes$transect_station_rep_year_net), ]

stopifnot(all(AHC_sample_volumes$transect_station_rep_year_net ==
                AHC_comm_matrix$transect_station_rep_year_net))
stopifnot(all(!is.na(AHC_sample_volumes$volume_best_m3_both_sides)))
stopifnot(all(AHC_sample_volumes$volume_best_m3_both_sides > 0))

AHC_sample_volumes <- AHC_sample_volumes$volume_best_m3_both_sides

AHC_count_abundances <- AHC_count_abundances %>%
  select(all_of(taxa_cols)) %>%
  as.matrix()

rownames(AHC_count_abundances) <- AHC_comm_matrix$transect_station_rep_year_net
storage.mode(AHC_count_abundances) <- "integer"


# Calculate dissimilarity matrix ------------------------------------------

dissim_matrix <- vegdist(transform_taxa_concentrations, method = "bray")


# Perform agglomerative hierarchical clustering ---------------------------

AHC_result <- hclust(dissim_matrix, method = "average")

cluster_colors <- c("1" = "#e6ab02", "2" = "#1b9e77", "3" = "#e7298a",
                    "4" = "#d95f02", "5" = "#66a61e", "6" = "#a6761d",
                    "7" = "#7570b3")
cluster_levels <- as.character(seq_len(7))

dendrogram_clusters <- cutree(AHC_result, k = 7)
dendrogram_cluster_order <- unique(dendrogram_clusters[AHC_result$order])
dendrogram_cluster_colors <- cluster_colors[as.character(dendrogram_cluster_order)]

clusters <- data.frame(
  transect_station_rep_year_net = names(dendrogram_clusters),
  cluster = dendrogram_clusters
)

main_clusters <- clusters %>%
  count(cluster, name = "n_net_tows") %>%
  arrange(desc(n_net_tows), cluster) %>%
  slice_head(n = 4) %>%
  pull(cluster)
