# Description -------------------------------------------------------------

#Run objective 3 analyses and plots with only 4 main clusters

# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(visreg)
library(DHARMa)
library(glmmTMB)


# Source code -------------------------------------------------------------

source(here("scripts/06_day_night_aside.R"))
#used some data frames from scripts 03 and 09. Should we also source 09 and/or 03?

# Prepare data ------------------------------------------------------------

#Filter mocness_major_taxa for only main 4 clusters
main_clust_samples_mocness_major_taxa <- mocness_major_taxa_nets %>% 
  semi_join(main_clust_samples, by = "transect_station_rep_year_net")

#Filter to keep only 2019 rows
main_clust_mocness_major_taxa_19 <- filter(main_clust_samples_mocness_major_taxa, 
                                           collection_date > "2019-01-01" & collection_date < "2019-12-31") %>%
  #add time of day column
  mutate(time_of_day = substr(replicate, 3, 3)) %>%
  mutate(time_of_day = recode(time_of_day, "D" = "Day", "N" = "Night"))

