# Description -------------------------------------------------------------

# Conduct cluster analysis of sample nets (rather than of events) by LFC. Plot
#clusters in stacked barplot and NMDS ordination with environmental variables
#overlayed as vectors

#Note to self (steps):
#1) stacked barplots by net. look at how noisy data are by net vs by station
#2) if data has consitent trends, cluster analysis
#3) regardless, plot NMDS by net and overlay vectors and ellipses


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
  group_by(transect_station_rep_year, net, taxon) %>%
  mutate(sum_individuals = sum(individuals_in_tow)) %>%
  merge(., volume_sampled_by_both_sides, by = c("transect_station_rep_year", "net"), all.x = TRUE) %>%
  ungroup() %>%
  distinct(transect_station_rep_year, net, taxon, .keep_all = TRUE) %>%
  mutate(avg_taxa_conc = sum_individuals/combined_volume_m3_best) %>%
  select(project, cruise, year, collection_date, transect, replicate, station, net, 
         transect_station_rep_year, start_time_utc, start_time_pt, end_time_utc, end_time_pt, 
         start_longitude_dd, start_latitude_dd, end_longitude_dd, end_latitude_dd,
         maximum_depth_m, minimum_depth_m, depth_mean_m, depth_diff_m,
         mean_temperature_c, mean_salinity_psu, mean_density_kgm3, seafloor_depth_m,
         distance_to_shore_km, shelf_position, prey_zooplankton_abundance_ind_m3,
         dissolved_oxygen_ml_l, chlorophyll_ug_l, mlotst, taxon,
         avg_taxa_conc, combined_volume_m3_best) %>%
  pivot_wider(names_from = taxon, values_from = avg_taxa_conc, values_fill = 0)
