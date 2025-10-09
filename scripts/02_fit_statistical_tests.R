
# Description -------------------------------------------------------------

# Run a preliminary PERMANOVA with larval fish assemblage data


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(vegan)


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Preliminary PERMANOVA ---------------------------------------------------

# Will want to sum volume_sampled_m3 within each haul (across MOC 1 and 4)
mocness_major_taxa_wide <- mocness_major_taxa %>%
  # Removing NAs for now, but there shouldn't be any to begin with
  filter(!is.na(individuals_in_tow)) %>%
  group_by(project, collection_date, time, replicate, depth_range, 
           transect_station, transect, station, latitude_dd, longitude_dd, 
           taxon, transect_station_rep, seafloor_depth_m, shelf_position, 
           prey_zooplankton_abundance_ind_m3, dissolved_oxygen_ml_l, 
           seawater_density_1000_kg_m3, chlorophyll_ug_l, mlotst) %>%
  summarize(individuals_in_tow = sum(individuals_in_tow)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_in_tow, values_fill = 0)

# Create separate community matrix and apply sqrt tranformation
abundance_by_taxon <- mocness_major_taxa_wide[, 19:41] %>%
  sqrt()

# Add tranformed abundances back into main data frame
mocness_major_taxa_wide_tranformed <- mocness_major_taxa_wide[, 1:18] %>%
  bind_cols(., abundance_by_taxon)

# Try a PERMANOVA
permanova <- adonis2(abundance_by_taxon ~ seafloor_depth_m + shelf_position, 
                     data = mocness_major_taxa_wide_tranformed)
summary(permanova)

##create a data frame to exclude rows that're missing data for covariates for the time being
filt_mocness_major_taxa_wide_tranformed <- filter(mocness_major_taxa_wide_tranformed, 
                                                   prey_zooplankton_abundance_ind_m3 != "" & 
                                                     dissolved_oxygen_ml_l != "" & 
                                                     seawater_density_1000_kg_m3 != "" &
                                                     chlorophyll_ug_l != "" & mlotst != "")

filt_mocness_major_taxa_wide_tranformed <- filter(mocness_major_taxa_wide_tranformed, 
                                                  !is.na(prey_zooplankton_abundance_ind_m3) & 
                                                    !is.na(dissolved_oxygen_ml_l) & 
                                                    !is.na(seawater_density_1000_kg_m3) &
                                                    !is.na(chlorophyll_ug_l != "" & mlotst))

mult_permanova <- adonis2(filt_mocness_major_taxa_wide_transformed[, 19:41] ~ seafloor_depth_m + shelf_position + depth_range + 
                            prey_zooplankton_abundance_ind_m3 + dissolved_oxygen_ml_l + 
                            seawater_density_1000_kg_m3 + chlorophyll_ug_l + mlotst,
                              data = filt_mocness_major_taxa_wide_tranformed, method = "bray", by = "margin")
