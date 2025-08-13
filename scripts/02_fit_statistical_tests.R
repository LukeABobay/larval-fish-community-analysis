
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
           taxon, transect_replicate) %>%
  summarize(individuals_in_tow = sum(individuals_in_tow)) %>%
  ungroup() %>%
  pivot_wider(names_from = taxon, values_from = individuals_in_tow, values_fill = 0)

# Create separate community matrix and apply sqrt tranformation
abundance_by_taxon <- mocness_major_taxa_wide[, 12:45] %>%
  sqrt()

# Add tranformed abundances back into main data frame
mocness_major_taxa_wide_tranformed <- mocness_major_taxa_wide[, 1:11] %>%
  bind_cols(., abundance_by_taxon)

# Try a PERMANOVA
permanova <- adonis2(abundance_by_taxon ~ transect * station, 
                     data = mocness_major_taxa_wide_tranformed)
summary(permanova)
