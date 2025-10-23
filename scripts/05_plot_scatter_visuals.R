# Description -------------------------------------------------------------

##Create scatterplots for visualization of correation among covariates and to
##look at how specific taxa's abundances vary with different covariates

# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(GGally)


# Source code -------------------------------------------------------------

source(here("scripts/02_fit_statistical_tests.R"))


# Plot pairwise scatterplots of covariates ---------------------

#make data frame of environmental covariates from each sampling event

env_covariates_wide <- mocness_major_taxa_wide %>%
  select(collection_date, time, latitude_dd, longitude_dd, transect_station_rep_year, 
         depth_range, shelf_position, seafloor_depth_m, prey_zooplankton_abundance_ind_m3, 
         dissolved_oxygen_ml_l, seawater_density_1000_kg_m3, chlorophyll_ug_l, mlotst, 
         temperature_c, salinity)
env_covariates_wide$depth_range <- as.factor(env_covariates_wide$depth_range)
env_covariates_wide$shelf_position <- as.factor(env_covariates_wide$shelf_position)

#create scatter plot matrix of covariates
##base R method
pairs(env_covariates_wide[,6:15], lower.panel = NULL, 
      main = "Scatterplots of MOCNESS environmental covariates",
      labels = c("depth range sampled", "shelf position", "seafloor depth", "prey abundance", 
                 "dissolved oxygen", "seawater density", "[chlorophyll]", "mixed layer depth", 
                 "temperature", "salinity"))
#view correlation coefficients for continuous variables
correlations <- cor(env_covariates_wide[,8:15])

##GGPlot method using GGally
##ggpairs scatterplot matrices
ggpairs(env_covariates_wide, columns = 6:15)
##remove density plots in diagonal
ggpairs(env_covariates_wide, columns = 6:15, diag = list(continuous = "blankDiag"))
##remove categorical variables and add one (shelf position) represented by colors
ggpairs(env_covariates_wide, columns = 8:15, aes(color = shelf_position), 
        upper = list(continuous = wrap("cor", size = 2.5)))
##remove density plots in diagonal
ggpairs(env_covariates_wide, columns = 8:15, diag = list(continuous = "blankDiag"), 
        aes(color = shelf_position), upper = list(continuous = wrap("cor", size = 2.5)))

