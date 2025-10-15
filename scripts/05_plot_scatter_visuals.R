# Description -------------------------------------------------------------

##Create scatterplots for visualization of correation among covariates and to
##look at how specific taxa's abundances vary with different covariates

# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library()



# Source code -------------------------------------------------------------

source(here("scripts/02_fit_statistical_tests.R"))


# Plot pairwise scatterplots of continuous covariates ---------------------

#make data frame of environmental covariates from each sampling event

env_covariates_wide <- mocness_major_taxa_wide %>%
  select(collection_date, time, depth_range, latitude_dd, longitude_dd,
         transect_station_rep_year, seafloor_depth_m, shelf_position, 
         prey_zooplankton_abundance_ind_m3, dissolved_oxygen_ml_l, 
         seawater_density_1000_kg_m3, chlorophyll_ug_l, mlotst, temperature_c,
         salinity)