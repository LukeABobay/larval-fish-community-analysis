
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
mocness_2022_03_fish_abundance_raw <- read.csv(here("data/22_w_SPECTRA_fish_inventory.csv"))
mocness_2023_02_fish_abundance <- read.csv(here("data/23_w_SPECTRA_fish_inventory.csv"))

# MEZCAL metadata
mocness_2018_2019_metadata <- read.csv(here("data/mezcal_envr.csv"))


# Data wrangling ----------------------------------------------------------

# Merge winter 2018 and 2019 MOCNESS fish data
mocness_winter_2018_2019_fish_abundance <- smartbind(mocness_2018_02_fish_abundance,
                                                     mocness_2019_03_fish_abundance) %>%
  rename(haul_number = Haul.no, transect = Location, station = Station,
         replicate = Transect, volume_filtered_m3 = volume.filtered.m3, 
         net_number = Net.no, individuals_in_tow = no.individuals) %>%
  select(haul_number, transect, station, replicate, net_number, 
         volume_filtered_m3, family, species, individuals_in_tow) %>%
  # Add project column, which will be necessary for merging with metadata, since this data frame has no date
  mutate(project = "MEZCAL")

# Calculate valid values of 'number.of.individuals.adjusted' for rows in 2022 data set
# that have a question mark following values in 'number.of.individuals.raw'
questionable_values <- mocness_2022_03_fish_abundance_raw %>%
  filter(grepl("\\?$", number.of.individuals.raw)) %>%
  mutate(number.of.individuals.adjusted = as.numeric(gsub("\\?", "", number.of.individuals.raw)) * split.multiplier)

# Bind re-calculated values with winter 2022 MOCNESS data
mocness_2022_03_fish_abundance <- mocness_2022_03_fish_abundance_raw %>%
  filter(!grepl("\\?$", number.of.individuals.raw)) %>%
  rbind(., questionable_values)

#Merge winter 2022 and 2023 MOCNESS fish data
mocness_winter_2022_2023_fish_abundance <- smartbind(mocness_2022_03_fish_abundance,
                                                     mocness_2023_02_fish_abundance) %>%
  rename(collection_date = date, haul_number = haul.number, net_number = net.number, 
         volume_filtered_m3 = volume.filtered.m3, 
         individuals_in_tow = number.of.individuals.adjusted) %>%
  select(collection_date, haul_number, transect, station, net_number, 
         volume_filtered_m3, family, species, individuals_in_tow) %>%
  mutate(project = "SPECTRA")

mocness_winter_fish_abundance <- smartbind(mocness_winter_2018_2019_fish_abundance,
                                           mocness_winter_2022_2023_fish_abundance) %>%
  # Keep rows where at least one of the non-excluded columns is not NA and not an empty string
  filter(if_any(-c(collection_date, individuals_in_tow),
                ~ !is.na(.) & . != "")) %>%
  # Add clean Haul.no without "oblique," "*", etc. for purpose of merging with metadata
  mutate(# Replace "oblique" in haul_number with "0" to get closest time to reality from metadata for net 0
         haul_number = gsub("oblique$", "0", haul_number),
         # Replace missing haul_number values with NA
         haul_number = ifelse(haul_number == "", NA, haul_number),
         # Remove leading zeros from haul numbers
         haul_number = sub("^0+", "", haul_number)) %>%
  # Expand haul_number to all rows from a net
  fill(haul_number, .direction = "down") %>%
  # Remove non-quantitative tows (asterisk in net_number or p or asterisk in haul_number)
  filter(!grepl("\\*$", net_number) & !grepl("\\*$", haul_number) & !grepl("\\p$", haul_number)) %>%
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
  # Make a new column for lowest taxonomic identity available
  mutate(taxon = ifelse(species %in% c("Unknown", ""), family, species)) %>%
  # Format collection_date (only has values for SPECTRA) as date
  mutate(collection_date = as.Date(collection_date, format = "%Y/%m/%d")) %>%
  select(project, collection_date, haul_number, replicate, transect, 
         station, maximum_depth_m, minimum_depth_m, 
         volume_filtered_m3, taxon, individuals_in_tow)

# Change date and start time in 'mocness_2018_2019_metadata' to PT
mocness_2018_2019_metadata_reformat_date <- mocness_2018_2019_metadata %>%
  # Convert to datetime in GMT
  mutate(date_time_gmt = as.POSIXct(paste0("20", paste(Date.GMT, Time.start.GMT, " ")), format = "%Y%m%d %H:%M:%S", tz = "GMT"),
         # Convert datetime from GMT to PT
         date_time_pt = lubridate::with_tz(date_time_gmt, "America/Los_Angeles")) %>%
  # Separate date and time into two columns
  mutate(date = as.Date(substr(date_time_pt, 1, 10)),
         time = substr(date_time_pt, 12, 19)) %>%
  # Add column indicating that these metadata are for the MEZCAL project
  mutate(project = "MEZCAL") %>%
  # Rename columns to be consistent with fish data frame
  rename(haul_number = Haul.no)

# Merge fish abundance data with metadata by haul_number
mocness_full <- merge(mocness_winter_fish_abundance, 
                           mocness_2018_2019_metadata_reformat_date, by = c("project", "haul_number"),
                      all.x = TRUE) %>%
  # Take 'collection_date' from 'date' column when NA
  mutate(collection_date = as.Date(ifelse(is.na(collection_date), date, collection_date))) %>%
  # Keep only date, time_gmt, haul_number, maximum_depth_m, minimum_depth_m, latitude_dd,
  # longitude_dd, family, species, and concentration_ind_1000m3
  select(project, collection_date, time, haul_number, replicate, maximum_depth_m, minimum_depth_m, 
         transect, station, latitude_dd = Station.lat, longitude_dd = Station.lon, taxon, 
         volume_filtered_m3 = Volume.filtered, individuals_in_tow)

