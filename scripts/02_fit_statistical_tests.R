
# Description -------------------------------------------------------------

# Run a preliminary PERMANOVA with larval fish assemblage data


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(vegan)


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Preliminary PERMANOVA ---------------------------------------------------

# Will want to sum volume_sampled_m3 within each haul (across MOC 1 and 4) (note 1/30: this is done now, right?)
mocness_major_taxa_wide <- mocness_major_taxa %>%
  # Removing NAs for now, but there shouldn't be any to begin with
  filter(!is.na(individuals_in_tow)) %>%
  filter(!is.na(individuals_per_m3)) %>%
  group_by(year, project, collection_date, start_time_pt, solar_dayness, replicate, depth_range, maximum_depth_m,
           minimum_depth_m, depth_mean_m, depth_diff_m,transect_station, transect, station, 
           start_latitude_dd, start_longitude_dd, taxon, transect_station_rep, 
           transect_station_rep_year, seafloor_depth_m, shelf_position,
           prey_zooplankton_abundance_ind_m3, dissolved_oxygen_ml_l, 
           seawater_density_1000_kg_m3, chlorophyll_ug_l, mlotst, 
           mean_temperature_c, mean_salinity_psu) %>%
  summarize(individuals_per_m3 = sum(individuals_per_m3)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_per_m3, values_fill = 0)

permanova_metadata_cols <- c("year", "project", "collection_date", "start_time_pt", "solar_dayness",
                             "replicate", "depth_range", "maximum_depth_m", "minimum_depth_m",
                             "depth_mean_m", "depth_diff_m", "transect_station", "transect", "station",
                             "start_latitude_dd", "start_longitude_dd", "transect_station_rep",
                             "transect_station_rep_year", "seafloor_depth_m", "shelf_position",
                             "prey_zooplankton_abundance_ind_m3", "dissolved_oxygen_ml_l",
                             "seawater_density_1000_kg_m3", "chlorophyll_ug_l", "mlotst",
                             "mean_temperature_c", "mean_salinity_psu")
permanova_taxa_cols <- setdiff(names(mocness_major_taxa_wide), permanova_metadata_cols)

# Create separate community matrix and apply sqrt tranformation
concentration_by_taxon <- mocness_major_taxa_wide %>%
  select(all_of(permanova_taxa_cols)) %>%
  sqrt()

# Add tranformed abundances back into main data frame
mocness_major_taxa_wide_transformed <- mocness_major_taxa_wide %>%
  select(all_of(permanova_metadata_cols)) %>%
  bind_cols(., concentration_by_taxon)

# Try a PERMANOVA
permanova <- adonis2(concentration_by_taxon ~ mean_temperature_c + mean_salinity_psu, data = mocness_major_taxa_wide_transformed)
summary(permanova)

##create a data frame to exclude rows that're missing data for covariates for the time being
##right now this filters out all rows?? so I'm taking out the mlotst line for now and not including it in multiple permanova
filt_mocness_major_taxa_wide_transformed <- filter(mocness_major_taxa_wide_transformed, 
                                                  !is.na(prey_zooplankton_abundance_ind_m3))
                                                    #& !is.na(mlotst))

mult_permanova <- adonis2(filt_mocness_major_taxa_wide_transformed[, permanova_taxa_cols] ~ mean_temperature_c * mean_salinity_psu +
                            year + dissolved_oxygen_ml_l + solar_dayness + start_latitude_dd + depth_mean_m + 
                            seafloor_depth_m,
                          data = filt_mocness_major_taxa_wide_transformed, method = "bray", by = "margin")
#not currently including mlotst and chlorophyll/flurosence till filled in
view(mult_permanova)
