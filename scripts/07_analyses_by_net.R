# Description -------------------------------------------------------------

# Conduct cluster analysis of sample nets (rather than of events) by LFC. Plot
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
  group_by(transect_station_rep_year, net, taxon) %>%
  mutate(sum_individuals = sum(individuals_in_tow)) %>%
  merge(., volume_sampled_by_both_sides, by = c("transect_station_rep_year", "net"), all.x = TRUE) %>%
  ungroup() %>%
  distinct(transect_station_rep_year, net, taxon, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(avg_taxa_conc = sum_individuals/combined_volume_m3_best) %>%
  select(-individuals_in_tow, -volume_from_flow_meter_m3, -volume_from_ships_stw_m3, -volume_m3_best,
         -individuals_per_m3, -individuals_per_station) %>%
  pivot_wider(names_from = taxon, values_from = avg_taxa_conc, values_fill = 0)
