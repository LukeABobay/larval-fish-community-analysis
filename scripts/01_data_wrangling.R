
# Description -------------------------------------------------------------

# Change winter MEZCAL and SPECTRA larval fish abundance data into a usable
# form. As of 7/30/2025, these data are incomplete.


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(gtools)


# Load data ---------------------------------------------------------------

# MOCNESS larval fish abundance
mocness_2018_02_fish_abundance <- read.csv(here("data/18_w_mezcal_fish_ids.csv"))
mocness_2019_03_fish_abundance <- read.csv(here("data/19_w_mezcal_fish_ids.csv"))
mocness_2022_03_fish_abundance <- read.csv(here("data/22_w_SPECTRA_fish_inventory.csv"))
mocness_2023_02_fish_abundance <- read.csv(here("data/23_w_SPECTRA_fish_inventory.csv"))

# MEZCAL metadata
mocness_2018_2019_metadata <- read.csv(here("data/mezcal_envr.csv"))


# Data wrangling ----------------------------------------------------------

# Merge winter 2018 and 2019 MOCNESS fish data
mocness_winter_2018_2019_fish_abundance <- smartbind(mocness_2018_02_fish_abundance,
                                                     mocness_2019_03_fish_abundance) %>%
  # Remove rows where every column is either empty or NA
  filter(rowSums(is.na(.) | . == "") != ncol(.)) %>%
  # Add clean Haul.no without "oblique," "*", etc. for purpose of merging with metadata
  mutate(haul_number = Haul.no) %>%
  # Replace "oblique" in haul_number with "0" to get closest time to reality from metadata for net 0
  mutate(haul_number = gsub("\\*$", "", haul_number),
         haul_number = gsub("oblique$", "0", haul_number),
         # Replace missing haul_number values with NA
         haul_number = ifelse(haul_number == "", NA, haul_number)) %>%
  # Expand haul_number to all rows from a net
  fill(haul_number, .direction = "down") %>%
  # Remove non-quantitative tows (asterisk in Net.no)
  filter(!grepl("\\*$", Net.no)) %>%
  # Add minimum and maximum depth for each tow based on net number from clean haul_number
  mutate(maximum_depth_m = as.numeric(case_match(substr(haul_number, nchar(haul_number), nchar(haul_number)), 
                                                 "0" ~ "100",
                                                 "1" ~ "100",
                                                 "2" ~ "75",
                                                 "3" ~ "50",
                                                 "4" ~ "25"))) %>%
  mutate(minimum_depth_m = as.numeric(case_match(substr(haul_number, nchar(haul_number), nchar(haul_number)),
                                                 "0" ~ "0",
                                                 "1" ~ "75",
                                                 "2" ~ "50",
                                                 "3" ~ "25",
                                                 "4" ~ "0"))) %>%
  # Keep only taxon, haul_number, maximum_depth_m, minimum_depth_m, and no.individuals
  select(haul_number, family, species, maximum_depth_m, minimum_depth_m, individuals_in_tow = no.individuals)

# Change date and start time in 'mocness_2018_2019_metadata' to PT
mocness_2018_2019_metadata_reformat_date <- mocness_2018_2019_metadata %>%
  # Convert to datetime in GMT
  mutate(date_time_gmt = as.POSIXct(paste0("20", paste(Date.GMT, Time.start.GMT, " ")), format = "%Y%m%d %H:%M:%S", tz = "GMT"),
         # Convert datetime from GMT to PT
         date_time_pt = lubridate::with_tz(date_time_gmt, "America/Los_Angeles")) %>%
  # Separate date and time into two columns
  mutate(date = as.Date(substr(date_time_pt, 1, 10)),
         time = substr(date_time_pt, 12, 19))

# Merge fish abundance data with metadata by haul_number
mocness_2018_2019_abundance <- merge(mocness_winter_2018_2019_fish_abundance, 
                           mocness_2018_2019_metadata_reformat_date, 
                           by.x = "haul_number", by.y = "Haul.no", all.x = TRUE) %>%
  # Keep only date, time_gmt, haul_number, maximum_depth_m, minimum_depth_m, latitude_dd,
  # longitude_dd, family, species, and concentration_ind_1000m3
  select(date, time, haul_number, maximum_depth_m, minimum_depth_m, 
         latitude_dd = Station.lat, longitude_dd = Station.lon, family, species, 
         volume_filtered_m3 = Volume.filtered, individuals_in_tow)

