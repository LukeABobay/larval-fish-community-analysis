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
  select(collection_date, start_time_pt, start_latitude_dd, start_longitude_dd, transect_station_rep_year, 
         depth_range, shelf_position, seafloor_depth_m, prey_zooplankton_abundance_ind_m3, 
         dissolved_oxygen_ml_l, seawater_density_1000_kg_m3, chlorophyll_ug_l, mlotst, 
         mean_temperature_c, mean_salinity_psu)
env_covariates_wide$depth_range <- as.numeric(env_covariates_wide$depth_range)
env_covariates_wide$shelf_position <- as.factor(env_covariates_wide$shelf_position)

##getting an error here now that new meta data is added so will need to come back to this 
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
###Not sure why the R-squared isn't showing up on the prey abundance or mixed layer depth plots
###Tried troubleshooting it but couldn't find a solution so I might need help with fixing those

#Myctophidae vs... 
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
##prey abundance
myc_prey <- lm(Myctophidae ~ prey_zooplankton_abundance_ind_m3, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = prey_zooplankton_abundance_ind_m3, y = Myctophidae)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  annotate("text", x = max(mocness_major_taxa_wide$prey_zooplankton_abundance_ind_m3), y = max(mocness_major_taxa_wide$Myctophidae)*0.95,
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
##mixed layer depth 
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
##temperature
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
##salinity
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
summary(myc_prey)$r.squared
summary(myc_MLD)$r.squared

#P.vetulus vs... 
##seafloor depth at station
pv_seafloor <- lm(`Parophrys vetulus` ~ seafloor_depth_m, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = seafloor_depth_m, y = `Parophrys vetulus`)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  annotate("text", x = min(mocness_major_taxa_wide$seafloor_depth_m), y = max(mocness_major_taxa_wide$`Parophrys vetulus`),
           label = paste("R² =", round(summary(pv_seafloor)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Parophrys vetulus abundance by seafloor depth",
       x = "Seafloor depth at station (m)",
       y = "P. vetulus abundance (individuals)")
##prey abundance
pv_prey <- lm(`Parophrys vetulus` ~ prey_zooplankton_abundance_ind_m3, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = prey_zooplankton_abundance_ind_m3, y = `Parophrys vetulus`)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  annotate("text", x = max(mocness_major_taxa_wide$prey_zooplankton_abundance_ind_m3), y = max(mocness_major_taxa_wide$`Parophrys vetulus`)*0.95,
           label = paste("R² =", round(summary(pv_prey)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Parophrys vetulus abundance by prey abundance",
       x = "Prey abundance (individuals/m3)",
       y = "P. vetulus abundance (individuals)")
##dissolved oxygen
pv_DO <- lm(`Parophrys vetulus` ~ dissolved_oxygen_ml_l, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = dissolved_oxygen_ml_l, y = `Parophrys vetulus`)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  annotate("text", x = min(mocness_major_taxa_wide$dissolved_oxygen_ml_l), y = max(mocness_major_taxa_wide$`Parophrys vetulus`),
           label = paste("R² =", round(summary(pv_DO)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Parophrys vetulus abundance by dissolved oxygen concentration",
       x = "[DO]",
       y = "P. vetulus abundance (individuals)")
##mixed layer depth 
pv_MLD <- lm(`Parophrys vetulus` ~ mlotst, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = mlotst, y = `Parophrys vetulus`)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  annotate("text", x = min(mocness_major_taxa_wide$mlotst), y = max(mocness_major_taxa_wide$`Parophrys vetulus`),
           label = paste("R² =", round(summary(pv_MLD)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Parophrys vetulus abundance by mixed layer depth",
       x = "Mixed layer depth",
       y = "P. vetulus abundance (individuals)")
##temperature
pv_temp <- lm(`Parophrys vetulus` ~ temperature_c, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = temperature_c, y = `Parophrys vetulus`)) +
  geom_point(color = "dark orange") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  annotate("text", x = min(mocness_major_taxa_wide$temperature_c), y = max(mocness_major_taxa_wide$`Parophrys vetulus`),
           label = paste("R² =", round(summary(pv_temp)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Parophrys vetulus abundance by temperature",
       x = "Temperature (degrees C)",
       y = "P. vetulus abundance (individuals)")
##salinity
pv_sal <- lm(`Parophrys vetulus` ~ salinity, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = salinity, y = `Parophrys vetulus`)) +
  geom_point(color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  annotate("text", x = min(mocness_major_taxa_wide$salinity), y = max(mocness_major_taxa_wide$`Parophrys vetulus`),
           label = paste("R² =", round(summary(pv_sal)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Parophrys vetulus abundance by salinity",
       x = "Salinity",
       y = "P. vetulus abundance (individuals)")
summary(pv_prey)$r.squared
summary(pv_MLD)$r.squared

#Sebastes vs... 
##seafloor depth at station
seb_seafloor <- lm(Sebastes ~ seafloor_depth_m, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = seafloor_depth_m, y = Sebastes)) +
  geom_point(color = "yellow3") +
  geom_smooth(method = "lm", se = FALSE, color = "purple3") +
  annotate("text", x = min(mocness_major_taxa_wide$seafloor_depth_m), y = max(mocness_major_taxa_wide$Sebastes),
           label = paste("R² =", round(summary(seb_seafloor)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Sebastes abundance by seafloor depth",
       x = "Seafloor depth at station (m)",
       y = "Sebastes abundance (individuals)")
##prey abundance
seb_prey <- lm(Sebastes ~ prey_zooplankton_abundance_ind_m3, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = prey_zooplankton_abundance_ind_m3, y = Sebastes)) +
  geom_point(color = "yellow3") +
  geom_smooth(method = "lm", se = FALSE, color = "purple3") +
  annotate("text", x = max(mocness_major_taxa_wide$prey_zooplankton_abundance_ind_m3), y = max(mocness_major_taxa_wide$Sebastes)*0.95,
           label = paste("R² =", round(summary(seb_prey)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Sebastes abundance by prey abundance",
       x = "Prey abundance (individuals/m3)",
       y = "Sebastes abundance (individuals)")
##dissolved oxygen
seb_DO <- lm(Sebastes ~ dissolved_oxygen_ml_l, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = dissolved_oxygen_ml_l, y = Sebastes)) +
  geom_point(color = "yellow3") +
  geom_smooth(method = "lm", se = FALSE, color = "purple3") +
  annotate("text", x = min(mocness_major_taxa_wide$dissolved_oxygen_ml_l), y = max(mocness_major_taxa_wide$Sebastes),
           label = paste("R² =", round(summary(seb_DO)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Sebastes abundance by dissolved oxygen concentration",
       x = "[DO]",
       y = "Sebastes abundance (individuals)")
##mixed layer depth 
seb_MLD <- lm(Sebastes ~ mlotst, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = mlotst, y = Sebastes)) +
  geom_point(color = "yellow3") +
  geom_smooth(method = "lm", se = FALSE, color = "purple3") +
  annotate("text", x = min(mocness_major_taxa_wide$mlotst), y = max(mocness_major_taxa_wide$Sebastes),
           label = paste("R² =", round(summary(seb_MLD)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Sebastes abundance by mixed layer depth",
       x = "Mixed layer depth",
       y = "Sebastes abundance (individuals)")
##temperature
seb_temp <- lm(Sebastes ~ temperature_c, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = temperature_c, y = Sebastes)) +
  geom_point(color = "yellow3") +
  geom_smooth(method = "lm", se = FALSE, color = "purple3") +
  annotate("text", x = min(mocness_major_taxa_wide$temperature_c), y = max(mocness_major_taxa_wide$Sebastes),
           label = paste("R² =", round(summary(seb_temp)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Sebastes abundance by temperature",
       x = "Temperature (degrees C)",
       y = "Sebastes abundance (individuals)")
##salinity
seb_sal <- lm(Sebastes ~ salinity, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = salinity, y = Sebastes)) +
  geom_point(color = "yellow3") +
  geom_smooth(method = "lm", se = FALSE, color = "purple3") +
  annotate("text", x = min(mocness_major_taxa_wide$salinity), y = max(mocness_major_taxa_wide$Sebastes),
           label = paste("R² =", round(summary(seb_sal)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Sebastes abundance by salinity",
       x = "Salinity",
       y = "Sebastes abundance (individuals)")
summary(seb_prey)$r.squared
summary(seb_MLD)$r.squared

#Pleuronectidae vs... 
##seafloor depth at station
ple_seafloor <- lm(Pleuronectidae ~ seafloor_depth_m, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = seafloor_depth_m, y = Pleuronectidae)) +
  geom_point(color = "slategray") +
  geom_smooth(method = "lm", se = FALSE, color = "violetred") +
  annotate("text", x = min(mocness_major_taxa_wide$seafloor_depth_m), y = max(mocness_major_taxa_wide$Pleuronectidae),
           label = paste("R² =", round(summary(ple_seafloor)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Pleuronectid abundance by seafloor depth",
       x = "Seafloor depth at station (m)",
       y = "Pleuronectidae abundance (individuals)")
##prey abundance
ple_prey <- lm(Pleuronectidae ~ prey_zooplankton_abundance_ind_m3, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = prey_zooplankton_abundance_ind_m3, y = Pleuronectidae)) +
  geom_point(color = "slategray") +
  geom_smooth(method = "lm", se = FALSE, color = "violetred") +
  annotate("text", x = max(mocness_major_taxa_wide$prey_zooplankton_abundance_ind_m3), y = max(mocness_major_taxa_wide$Pleuronectidae)*0.95,
           label = paste("R² =", round(summary(ple_prey)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Pleuronectid abundance by prey abundance",
       x = "Prey abundance (individuals/m3)",
       y = "Pleuronectid abundance (individuals)")
##dissolved oxygen
ple_DO <- lm(Pleuronectidae ~ dissolved_oxygen_ml_l, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = dissolved_oxygen_ml_l, y = Pleuronectidae)) +
  geom_point(color = "slategray") +
  geom_smooth(method = "lm", se = FALSE, color = "violetred") +
  annotate("text", x = min(mocness_major_taxa_wide$dissolved_oxygen_ml_l), y = max(mocness_major_taxa_wide$Pleuronectidae),
           label = paste("R² =", round(summary(ple_DO)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Pleuronectid abundance by dissolved oxygen concentration",
       x = "[DO]",
       y = "Pleuronectid abundance (individuals)")
##mixed layer depth 
ple_MLD <- lm(Pleuronectidae ~ mlotst, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = mlotst, y = Pleuronectidae)) +
  geom_point(color = "slategray") +
  geom_smooth(method = "lm", se = FALSE, color = "violetred") +
  annotate("text", x = min(mocness_major_taxa_wide$mlotst), y = max(mocness_major_taxa_wide$Pleuronectidae),
           label = paste("R² =", round(summary(ple_MLD)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Pleuronectid abundance by mixed layer depth",
       x = "Mixed layer depth",
       y = "Pleuronectid abundance (individuals)")
##temperature
ple_temp <- lm(Pleuronectidae ~ temperature_c, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = temperature_c, y = Pleuronectidae)) +
  geom_point(color = "slategray") +
  geom_smooth(method = "lm", se = FALSE, color = "violetred") +
  annotate("text", x = min(mocness_major_taxa_wide$temperature_c), y = max(mocness_major_taxa_wide$Pleuronectidae),
           label = paste("R² =", round(summary(ple_temp)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Pleuronectid abundance by temperature",
       x = "Temperature (degrees C)",
       y = "Pleuronectid abundance (individuals)")
##salinity
ple_sal <- lm(Pleuronectidae ~ salinity, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = salinity, y = Pleuronectidae)) +
  geom_point(color = "slategray") +
  geom_smooth(method = "lm", se = FALSE, color = "violetred") +
  annotate("text", x = min(mocness_major_taxa_wide$salinity), y = max(mocness_major_taxa_wide$Pleuronectidae),
           label = paste("R² =", round(summary(ple_sal)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Pleuronectid abundance by salinity",
       x = "Salinity",
       y = "Pleuronectid abundance (individuals)")
summary(ple_prey)$r.squared
summary(ple_MLD)$r.squared

#L. ochotensis vs... 
##seafloor depth at station
lo_seafloor <- lm(`Lipolagus ochotensis` ~ seafloor_depth_m, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = seafloor_depth_m, y = `Lipolagus ochotensis`)) +
  geom_point(color = "cyan4") +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange3") +
  annotate("text", x = min(mocness_major_taxa_wide$seafloor_depth_m), y = max(mocness_major_taxa_wide$`Lipolagus ochotensis`),
           label = paste("R² =", round(summary(lo_seafloor)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Lipolagus ochotensis abundance by seafloor depth",
       x = "Seafloor depth at station (m)",
       y = "L. ochotensis abundance (individuals)")
##prey abundance
lo_prey <- lm(`Lipolagus ochotensis` ~ prey_zooplankton_abundance_ind_m3, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = prey_zooplankton_abundance_ind_m3, y = `Lipolagus ochotensis`)) +
  geom_point(color = "cyan4") +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange3") +
  annotate("text", x = max(mocness_major_taxa_wide$prey_zooplankton_abundance_ind_m3), y = max(mocness_major_taxa_wide$`Lipolagus ochotensis`)*0.95,
           label = paste("R² =", round(summary(lo_prey)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Lipolagus ochotensis abundance by prey abundance",
       x = "Prey abundance (individuals/m3)",
       y = "L. ochotensis abundance (individuals)")
##dissolved oxygen
lo_DO <- lm(`Lipolagus ochotensis` ~ dissolved_oxygen_ml_l, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = dissolved_oxygen_ml_l, y = `Lipolagus ochotensis`)) +
  geom_point(color = "cyan4") +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange3") +
  annotate("text", x = min(mocness_major_taxa_wide$dissolved_oxygen_ml_l), y = max(mocness_major_taxa_wide$`Lipolagus ochotensis`),
           label = paste("R² =", round(summary(lo_DO)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Lipolagus ochotensis abundance by dissolved oxygen concentration",
       x = "[DO]",
       y = "L. ochotensis abundance (individuals)")
##mixed layer depth 
lo_MLD <- lm(`Lipolagus ochotensis` ~ mlotst, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = mlotst, y = `Lipolagus ochotensis`)) +
  geom_point(color = "cyan4") +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange3") +
  annotate("text", x = min(mocness_major_taxa_wide$mlotst), y = max(mocness_major_taxa_wide$`Lipolagus ochotensis`),
           label = paste("R² =", round(summary(lo_MLD)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Lipolagus ochotensis abundance by mixed layer depth",
       x = "Mixed layer depth",
       y = "L. ochotensis abundance (individuals)")
##temperature
lo_temp <- lm(`Lipolagus ochotensis` ~ temperature_c, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = temperature_c, y = `Lipolagus ochotensis`)) +
  geom_point(color = "cyan4") +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange3") +
  annotate("text", x = min(mocness_major_taxa_wide$temperature_c), y = max(mocness_major_taxa_wide$`Lipolagus ochotensis`),
           label = paste("R² =", round(summary(lo_temp)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Lipolagus ochotensis abundance by temperature",
       x = "Temperature (degrees C)",
       y = "L. ochotensis abundance (individuals)")
##salinity
lo_sal <- lm(`Lipolagus ochotensis` ~ salinity, data = mocness_major_taxa_wide)
ggplot(mocness_major_taxa_wide, aes(x = salinity, y = `Lipolagus ochotensis`)) +
  geom_point(color = "cyan4") +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange3") +
  annotate("text", x = min(mocness_major_taxa_wide$salinity), y = max(mocness_major_taxa_wide$`Lipolagus ochotensis`),
           label = paste("R² =", round(summary(lo_sal)$r.squared, 3)),
           hjust = 0, vjust = 1.5, size = 5, color = "black") +
  theme_classic() +
  labs(title = "Lipolagus ochotensis abundance by salinity",
       x = "Salinity",
       y = "L. ochotensis abundance (individuals)")
summary(lo_prey)$r.squared
summary(lo_MLD)$r.squared