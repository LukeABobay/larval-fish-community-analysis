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
###ggpairs scatterplot matrices
ggpairs(env_covariates_wide, columns = 6:15)
###remove density plots in diagonal
ggpairs(env_covariates_wide, columns = 6:15, diag = list(continuous = "blankDiag"))
###remove categorical variables and add one (shelf position) represented by colors
ggpairs(env_covariates_wide, columns = 8:15, aes(color = shelf_position), 
        upper = list(continuous = wrap("cor", size = 2.5)))
###remove density plots in diagonal
ggpairs(env_covariates_wide, columns = 8:15, diag = list(continuous = "blankDiag"), 
        aes(color = shelf_position), upper = list(continuous = wrap("cor", size = 2.5)))


# Plot specific major taxa against environmental covariates ---------------

#myctophidae vs...
##seafloor depth at station
myc_seafloor <- lm(Myctophidae ~ seafloor_depth_m, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = seafloor_depth_m, y = Myctophidae)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  annotate("text", x = min(mocness_major_taxa_wide$seafloor_depth_m), y = max(mocness_major_taxa_wide$Myctophidae),
           label = paste("R² =", round(summary(myc_seafloor)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Myctophid abundance by seafloor depth",
       x = "Seafloor depth at station (m)",
       y = "Myctophid abundance (individuals)")
##prey abundance (for some reason the R-squared isn't working on this one. will troubleshoot later)
myc_prey <- lm(Myctophidae ~ prey_zooplankton_abundance_ind_m3, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = prey_zooplankton_abundance_ind_m3, y = Myctophidae)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  annotate("text", x = min(mocness_major_taxa_wide$prey_zooplankton_abundance_ind_m3), y = max(mocness_major_taxa_wide$Myctophidae),
           label = paste("R² =", round(summary(myc_prey)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Myctophid abundance by prey abundance",
       x = "Prey abundance (individuals/m3)",
       y = "Myctophid abundance (individuals)")
##dissolved oxygen
myc_DO <- lm(Myctophidae ~ dissolved_oxygen_ml_l, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = dissolved_oxygen_ml_l, y = Myctophidae)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  annotate("text", x = min(mocness_major_taxa_wide$dissolved_oxygen_ml_l), y = max(mocness_major_taxa_wide$Myctophidae),
           label = paste("R² =", round(summary(myc_DO)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Myctophid abundance by dissolved oxygen concentration",
       x = "[DO]",
       y = "Myctophid abundance (individuals)")
##mixed layer depth (for some reason the R-squared isn't working on this one. will troubleshoot later)
myc_MLD <- lm(Myctophidae ~ mlotst, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = mlotst, y = Myctophidae)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  annotate("text", x = min(mocness_major_taxa_wide$mlotst), y = max(mocness_major_taxa_wide$Myctophidae),
           label = paste("R² =", round(summary(myc_MLD)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Myctophid abundance by mixed layer depth",
       x = "Mixed layer depth",
       y = "Myctophid abundance (individuals)")
##Temperature
myc_temp <- lm(Myctophidae ~ temperature_c, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = temperature_c, y = Myctophidae)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  annotate("text", x = min(mocness_major_taxa_wide$temperature_c), y = max(mocness_major_taxa_wide$Myctophidae),
           label = paste("R² =", round(summary(myc_temp)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Myctophid abundance by temperature",
       x = "Temperature (degrees C)",
       y = "Myctophid abundance (individuals)")
##Salinity
myc_sal <- lm(Myctophidae ~ salinity, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = salinity, y = Myctophidae)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  annotate("text", x = min(mocness_major_taxa_wide$salinity), y = max(mocness_major_taxa_wide$Myctophidae),
           label = paste("R² =", round(summary(myc_sal)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Myctophid abundance by salinity",
       x = "Salinity",
       y = "Myctophid abundance (individuals)")


