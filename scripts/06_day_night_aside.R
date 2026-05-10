# Description -------------------------------------------------------------

#Aside analysis of depth stratified total abundances across replicates for
# each taxa against day/night and depth in 2019


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(visreg)


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Prepare data ------------------------------------------------------------

#Filter to keep only 2019 rows
mocness_major_taxa_19 <- filter(mocness_major_taxa, collection_date > "2019-01-01" & collection_date < "2019-12-31") %>%
  #add time of day column
  mutate(time_of_day = substr(replicate, 3, 3)) %>%
  mutate(time_of_day = recode(time_of_day, "D" = "Day", "N" = "Night"))


# Classify taxa by habitat affinity and create color vectors ---------------
# Categorize taxa by habitat affinity
nearshore_species <- c("Ammodytidae", "Cottidae", "Gadidae", "Glyptocephalus_zachirus", "Hemilepidotus_spp", 
                       "Hexagrammidae", "Psychrolutidae", "Scorpaenichthys_marmoratus")
nearshore_colors <- colorRampPalette(brewer.pal(9, "Greens")[2:9])(length(nearshore_species))

coastal_species <- c("Agonidae", "Cyclopsettidae", "Isopsetta_isolepis", "Liparis_spp", "Lyopsetta_exilis", "Osmeridae", 
                     "Parophrys_vetulus", "Psettichthys_melanostictus", "Sebastes_spp")
coastal_colors <- colorRampPalette(brewer.pal(10, "Blues")[1:9])(length(coastal_species))

oceanic_species <- c("Bathylagus_ochotensis", "Lestidiops_ringens", "Protomyctophum_spp", "Stenobrachius_leucopsarus", 
                     "Tarletonbeania_crenularis")
oceanic_colors <- colorRampPalette(brewer.pal(5, "Purples")[2:6])(length(oceanic_species))

# Named species color vector
species_colors <- c(setNames(nearshore_colors, nearshore_species),
                    setNames(coastal_colors, coastal_species),
                    setNames(oceanic_colors, oceanic_species))

# Vector of taxa ordered alphabetically within categories to order bars and figure legends
ordered_taxa_19 <- c(nearshore_species, coastal_species, oceanic_species)

mocness_major_taxa_19 <- mocness_major_taxa_19 %>%
  #Reorder taxa
  mutate(taxon = factor(taxon, levels = ordered_taxa_19)) %>%
  #Reorder stations
  mutate(station = factor(station, levels = rev(sort(unique(station))))) %>%
  mutate(adult_habitat_affinity = case_when(taxon %in% nearshore_species ~ "Nearshore",
                                            taxon %in% coastal_species ~ "Coastal",
                                            taxon %in% oceanic_species ~ "Oceanic",
                                            TRUE ~ "Other"))


# Avg taxa concentrations across replicates ------------------------------------

avgd_mocness_major_taxa_19 <- mocness_major_taxa_19 %>%
  group_by(taxon, time_of_day, depth_range, depth_mean_m, depth_diff_m) %>%
  summarise(avg_taxa_concentration = mean(individuals_per_m3, na.rm=TRUE)) %>%
  ungroup()


# Plot taxa concentrations by depths and day/night ------------------------
#Barplot
ggplot(avgd_mocness_major_taxa_19, aes(x = depth_range, y = avg_taxa_concentration, fill = taxon)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  facet_wrap(~ time_of_day, nrow = 2) +
  labs(title = "Day-night comparison of taxa concentrations at depth ranges",
       x = "Depth sampled (m)", y = "average individuals per m3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Scatterplot
ggplot(avgd_mocness_major_taxa_19, aes(x = depth_mean_m, y = log(avg_taxa_concentration), color = taxon)) +
  geom_point() +
  geom_smooth(method = "lm",se = FALSE) +
  facet_wrap(~ time_of_day, nrow = 2) +
  labs(title = "Day-night comparison of taxa concentrations by mean depths",
       x = "Mean depth (m)", y = "log(average individuals per m3)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Fit linear model(s) of taxa concentrations against depth and time of day -----
day_night_depth_model <- lm(avg_taxa_concentration ~ taxon*time_of_day + taxon*depth_range, data = avgd_mocness_major_taxa_19)
summary(day_night_depth_model)

day_night_mean_depth_model <- lm(avg_taxa_concentration ~ taxon*time_of_day + taxon*depth_mean_m, data = avgd_mocness_major_taxa_19)
summary(day_night_mean_depth_model)

#Scatterplot of only 4 species of interest
ggplot(avgd_mocness_major_taxa_19 %>%
         filter(taxon %in% c("Sebastes_spp", "Parophrys_vetulus", "Stenobrachius_leucopsarus", "Isopsetta_isolepis")),
       aes(x = depth_mean_m, y = log(avg_taxa_concentration), color = taxon)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ time_of_day, nrow = 2) +
  labs(title = "Day-night comparison of taxa concentrations by mean depths",
       x = "Mean depth (m)", y = "log(average individuals per m3)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Linear regression on specific taxa --------------------------------------

#Cluster 1: Sebastes
seb_df <- mocness_major_taxa_19 %>%
  filter(taxon == "Sebastes_spp")
seb_lm <- lm(log(individuals_per_m3) ~ depth_mean_m + time_of_day + depth_mean_m:time_of_day, 
             data = seb_df)
summary(seb_lm)
visreg(seb_lm, "depth_mean_m", by = "time_of_day", ylab = "log(Sebastes individuals per m3)", xlab = "mean depth (m)")

#Cluster 2: P. vetulus
p_vetulus_df <- mocness_major_taxa_19 %>%
  filter(taxon == "Parophrys_vetulus")
p_vetulus_lm <- lm(log(individuals_per_m3) ~ depth_mean_m + time_of_day + depth_mean_m:time_of_day, 
             data = p_vetulus_df)
summary(p_vetulus_lm)
visreg(p_vetulus_lm, "depth_mean_m", by = "time_of_day", ylab = "log(P. vetulus individuals per m3)", xlab = "mean depth (m)")

#Cluster 3: S. leucopsarus 
s_leucopsarus_df <- mocness_major_taxa_19 %>%
  filter(taxon == "Stenobrachius_leucopsarus")
s_leucopsarus_lm <- lm(log(individuals_per_m3) ~ depth_mean_m + time_of_day + depth_mean_m:time_of_day, 
                   data = s_leucopsarus_df)
summary(s_leucopsarus_lm)
visreg(s_leucopsarus_lm, "depth_mean_m", by = "time_of_day", ylab = "log(S. leucopsarus individuals per m3)", xlab = "mean depth (m)")

#Cluster 5: I. isolepis
i_isolepis_df <- mocness_major_taxa_19 %>%
  filter(taxon == "Isopsetta_isolepis")
i_isolepis_lm <- lm(log(individuals_per_m3) ~ depth_mean_m + time_of_day + depth_mean_m:time_of_day, 
                       data = i_isolepis_df)
summary(i_isolepis_lm)
visreg(i_isolepis_lm, "depth_mean_m", by = "time_of_day", ylab = "log(I. isolepis individuals per m3)", xlab = "mean depth (m)")