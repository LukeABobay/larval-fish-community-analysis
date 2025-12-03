# Description -------------------------------------------------------------

#Aside analysis of depth stratified total abundances across replicates for
# each taxa against day/night and depth in 2019


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Prepare data ------------------------------------------------------------

#Filter to keep only 2019 rows
mocness_major_taxa_19 <- filter(mocness_major_taxa, collection_date > "2019-01-01" & collection_date < "2019-12-31") %>%
  #add time of day column
  mutate(time_of_day = substr(replicate, 3, 3)) %>%
  mutate(time_of_day = recode(time_of_day, "D" = "Day", "N" = "Night"))


# Classify taxa by habitat affinity and create color vectors ---------------
#Categories of species with >15 individuals from MEZCAL
coastal_species <- c("Agonidae", "Ammodytes", "Anarrhichthys ocellatus", "Artedius", "Chilara taylori", "Cottidae", "Cryptacanthodes",
                     "Gadidae", "Gobiidae", "Hexagrammidae", "Liparis", "Osmeridae", "Paralichthyidae", "Parophrys vetulus", 
                     "Pholidae", "Pleuronectidae", "Ptilichthys goodei", "Ronquilus jordani", "Sebastes", "Stichaeidae")
coastal_colors <- colorRampPalette(brewer.pal(9, "Greens")[2:9])(length(coastal_species))

coastal_oceanic_species <- c("Anoplopomatidae", "Engraulis mordax", "Lestidiops ringens", "Sardinops sagax", 
                             "Sebastolobus")
coastal_oceanic_colors <- colorRampPalette(brewer.pal(9, "Blues")[2:9])(length(coastal_oceanic_species))

oceanic_species <- c("Bathylagus pacificus", "Chauliodontidae", "Chauliodus macouni", "Lipolagus ochotensis", "Macrouridae", 
                     "Merluccius productus", "Myctophidae", "Nansenia candida", "Trachipterus altivelis")
oceanic_colors <- colorRampPalette(brewer.pal(9, "Purples")[2:9])(length(oceanic_species))

#Named species color vector
species_colors <- c(setNames(coastal_colors, coastal_species),
                    setNames(coastal_oceanic_colors, coastal_oceanic_species),
                    setNames(oceanic_colors, oceanic_species))

#Vector of taxa ordered alphabetically within categories to order bars and figure legends
ordered_taxa <- c(coastal_species, coastal_oceanic_species, oceanic_species)

mocness_major_taxa_19 <- mocness_major_taxa_19 %>%
  #Reorder taxa
  mutate(taxon = factor(taxon, levels = ordered_taxa)) %>%
  #Reorder stations
  mutate(station = factor(station, levels = rev(sort(unique(station))))) %>%
  mutate(adult_habitat_affinity = case_when(taxon %in% coastal_species ~ "Coastal",
                                            taxon %in% coastal_oceanic_species ~ "Coastal-oceanic",
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
