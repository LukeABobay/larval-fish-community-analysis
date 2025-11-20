# Description -------------------------------------------------------------

#explore assemplage structures and abundances with respect to environmental
#covariates by plotting relationships and running PERMANOVAs.
#currently missing some mixed layer depth values.

# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(tidyr)
library(dplyr)
library(gtools)
library(vegan)
library(ggplot2)
library(ggrepel) 
##I'm not sure if all of these packages are actually necessary
##but I included all that have been used so far and others that came up


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Prepare data frames -----------------------------------------------------

##make wide data frame (copied script from 02_fit_statistical_tests.R and slightly modified)
mocness_major_taxa_wide <- mocness_major_taxa %>%
  # Removing NAs for now, but there shouldn't be any to begin with
  filter(!is.na(individuals_in_tow)) %>%
  filter(!is.na(individuals_per_m3)) %>%
  group_by(project, collection_date, start_time_pt, depth_range, transect_station_rep_year,
           start_latitude_dd, start_longitude_dd, taxon, seafloor_depth_m, 
           distance_to_shore_km, shelf_position,prey_zooplankton_abundance_ind_m3, dissolved_oxygen_ml_l, 
           seawater_density_1000_kg_m3, chlorophyll_ug_l, mlotst,mean_temperature_c, mean_salinity_psu) %>%
  summarize(individuals_per_m3 = sum(individuals_per_m3)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_per_m3, values_fill = 0)

##split community matrix and environmental metadata into separate data frames
comm_matrix <- mocness_major_taxa_wide %>%
  select(-project, -collection_date, -start_time_pt, -depth_range,
         -start_latitude_dd, -start_longitude_dd, -seafloor_depth_m, -distance_to_shore_km,
         -shelf_position, -prey_zooplankton_abundance_ind_m3, -dissolved_oxygen_ml_l, 
         -seawater_density_1000_kg_m3, -chlorophyll_ug_l, -mlotst, -mean_temperature_c, -mean_salinity_psu)
env_meta <- mocness_major_taxa_wide %>%
  select(project, collection_date, start_time_pt, depth_range, transect_station_rep_year,
         start_latitude_dd, start_longitude_dd, seafloor_depth_m, distance_to_shore_km,
         shelf_position, prey_zooplankton_abundance_ind_m3, dissolved_oxygen_ml_l, 
         seawater_density_1000_kg_m3, chlorophyll_ug_l, mlotst, mean_temperature_c, mean_salinity_psu)

##transform standardized taxa counts and run bray-curtis on community matrix
transform_concentrations <- comm_matrix[, 2:24] %>%
  sqrt()
comm_matrix_transformed <- comm_matrix[,1] %>%
  bind_cols(.,transform_concentrations)
dissim_matrix <- vegdist(comm_matrix_transformed[,2:24], method = "bray")

##bin environmental data for plotting
env_meta_binned <- env_meta %>%
  mutate(
    seafloor_bin = cut(seafloor_depth_m, breaks = c(0, -100, -500, -1000), labels = c("0-100", "100-500", "500-1000")),
    prey_abundance_bin = cut(prey_zooplankton_abundance_ind_m3, breaks = quantile(prey_zooplankton_abundance_ind_m3, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("1", "2", "3", "4"), include.lowest = TRUE),
    DO_bin = cut(dissolved_oxygen_ml_l, breaks = quantile(dissolved_oxygen_ml_l, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("1", "2", "3", "4"), include.lowest = TRUE),
    density_bin = cut(seawater_density_1000_kg_m3, breaks = quantile(seawater_density_1000_kg_m3, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("1", "2", "3", "4"), include.lowest = TRUE),
    chlorophyll_bin = cut(chlorophyll_ug_l, breaks = quantile(chlorophyll_ug_l, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("1", "2", "3", "4"), include.lowest = TRUE),
    time_parsed = as.POSIXct(start_time_pt, format =  "%H:%M:%S", tz = "America/Los_Angeles"),
    hour = as.numeric(format(time_parsed, "%H")),
    day_night = case_when(hour>=6 & hour<18 ~ "day", TRUE ~ "night"))

#all mlotst are currently NA so no data frame can be made filtering by this so I'm hiding these parts for now
##create separate 2018-2019 community matrix filtered by the presence of a mlotst value
# ###make wide data frame of 2018-2019 MLD data
# mocness_2018_2019_MLD_wide <- mocness_major_taxa_2018_2019_MLD %>%
#   filter(!is.na(individuals_in_tow)) %>%
#   group_by(taxon, transect_station_rep, mlotst) %>%
#   summarize(individuals_in_tow = sum(individuals_in_tow)) %>%
#   ungroup() %>%
#   pivot_wider(names_from = taxon, values_from = individuals_in_tow, values_fill = 0)
# 
# ###split MLD community matrix and environmental metadata into separate data frames
# comm_matrix_2018_2019_MLD <- mocness_2018_2019_MLD_wide %>%
#   select(-mlotst)
# MLD_2018_2019 <- mocness_2018_2019_MLD_wide %>%
#   select(transect_station_rep, mlotst)
# 
# ##transform MLD taxa counts and run bray-curtis on community matrix
# transform_abundances_2018_2019_MLD <- comm_matrix_2018_2019_MLD[, 2:20] %>%
#   sqrt()
# comm_matrix_2018_2019_MLD_transformed <- comm_matrix_2018_2019_MLD[,1] %>%
#   bind_cols(.,transform_abundances_2018_2019_MLD)
# dissim_matrix_2018_2019_MLD <- vegdist(comm_matrix_2018_2019_MLD_transformed[,2:20], method = "bray")
# 
# ##bin MLD data for plotting
# MLD_binned_2018_2019 <- MLD_2018_2019 %>%
#   mutate(MLD_bin = cut(mlotst, breaks = quantile(mlotst, probs = seq(0, 1, 0.25), na.rm = TRUE), labels = c("1", "2", "3", "4"), include.lowest = TRUE))


# Explore covariates and LFC abundances -----------------------------------
##box plot of bray-curtis dissimiliarity by shelf position
bd_shelf_position <- betadisper(dissim_matrix, group = env_meta_binned$shelf_position)
boxplot(bd_shelf_position$distances ~ bd_shelf_position$group,
        xlab = "Shelf Position",
        ylab = "Distance to Group Centroid (Bray-Curtis)",
        main = "Larval Fish Community Dispersion by Shelf Position")

##box plot of bray-curtis dissimilarity by time (day vs night)
bd_time <- betadisper(dissim_matrix, group = env_meta_binned$day_night)
boxplot(bd_time$distances ~ bd_time$group,
        xlab = "Time of Sample",
        ylab = "Distance to Group Centroid (Bray-Curtis)",
        main = "Larval Fish Community Dispersion by Sample Time")

##box plot of bray-curtis dissimilarity by seafloor depth
bd_seafloor_depth <- betadisper(dissim_matrix, group = env_meta_binned$seafloor_bin)
boxplot(bd_seafloor_depth$distances ~ bd_seafloor_depth$group,
        xlab = "Seafloor Depth",
        ylab = "Distance to Group Centroid (Bray-Curtis)",
        main = "Larval Fish Community Dispersion by Seafloor Depth at Station")

##box plot of bray-curtis dissimilarity by prey abundance
bd_prey_abundance <- betadisper(dissim_matrix, group = env_meta_binned$prey_abundance_bin)
boxplot(bd_prey_abundance$distances ~ bd_prey_abundance$group,
        xlab = "Prey Abundance",
        ylab = "Distance to Group Centroid (Bray-Curtis)",
        main = "Larval Fish Community Dispersion by Zooplankton/Prey Abundance")

##box plot of bray-curtis dissimilarity by DO
bd_DO <- betadisper(dissim_matrix, group = env_meta_binned$DO_bin)
boxplot(bd_DO$distances ~ bd_DO$group,
        xlab = "[DO]",
        ylab = "Distance to Group Centroid (Bray-Curtis)",
        main = "Larval Fish Community Dispersion by Dissolved Oxygen Concentration")

##box plot of bray-curtis dissimilarity by seawater density
bd_density <- betadisper(dissim_matrix, group = env_meta_binned$density_bin)
boxplot(bd_density$distances ~ bd_density$group,
        xlab = "Seawater Density",
        ylab = "Distance to Group Centroid (Bray-Curtis)",
        main = "Larval Fish Community Dispersion by Seawater Density")

##box plot of bray-curtis dissimilarity by chlorophyll concentration
bd_chlorophyll <- betadisper(dissim_matrix, group = env_meta_binned$chlorophyll_bin)
boxplot(bd_chlorophyll$distances ~ bd_chlorophyll$group,
        xlab = "Chlorophyll Concentration",
        ylab = "Distance to Group Centroid (Bray-Curtis)",
        main = "Larval Fish Community Dispersion by Phytoplankton Abundance [Chlorophyll]")

##box plot of bray-curtis dissimilarity by depth sampled
bd_samp_depth <- betadisper(dissim_matrix, group = env_meta_binned$depth_range)
boxplot(bd_samp_depth$distances ~ bd_samp_depth$group,
        xlab = "Depth Range",
        ylab = "Distance to Group Centroid (Bray-Curtis)",
        main = "Larval Fish Community Dispersion by Depth Layer Sampled")

# ##box plot of bray-curtis dissimilarity by mixed layer depth
# bd_MLD <- betadisper(dissim_matrix_2018_2019_MLD, group = MLD_binned_2018_2019$MLD_bin)
# boxplot(bd_MLD$distances ~ bd_MLD$group,
#         xlab = "MLD",
#         ylab = "Distance to Group Centroid (Bray-Curtis)",
#         main = "Larval Fish Community Dispersion by Mixed Layer Depth at Station")