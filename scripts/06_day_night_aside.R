# Description -------------------------------------------------------------

#Aside analysis of depth stratified total abundances across replicates for
# each taxa against day/night and depth in 2019


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)


# Source code -------------------------------------------------------------

source(here("scripts/01_data_wrangling.R"))


# Prepare data ------------------------------------------------------------

#Filter to keep only 2019 rows
mocness_major_taxa_19 <- filter(mocness_major_taxa, collection_date > "2019-01-01" & collection_date < "2019-12-31")


# Classify taxa by habitat affinity and create color vectors ---------------
# Categories of species with >15 individuals from MEZCAL
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

# Named species color vector
species_colors <- c(setNames(coastal_colors, coastal_species),
                    setNames(coastal_oceanic_colors, coastal_oceanic_species),
                    setNames(oceanic_colors, oceanic_species))

# Vector of taxa ordered alphabetically within categories to order bars and figure legends
ordered_taxa <- c(coastal_species, coastal_oceanic_species, oceanic_species)

mocness_major_taxa_19 <- mocness_major_taxa_19 %>%
  # Reorder taxa
  mutate(taxon = factor(taxon, levels = ordered_taxa)) %>%
  # Reorder stations
  mutate(station = factor(station, levels = rev(sort(unique(station))))) %>%
  mutate(adult_habitat_affinity = case_when(taxon %in% coastal_species ~ "Coastal",
                                            taxon %in% coastal_oceanic_species ~ "Coastal-oceanic",
                                            taxon %in% oceanic_species ~ "Oceanic",
                                            TRUE ~ "Other"))

#sum counts across replicates