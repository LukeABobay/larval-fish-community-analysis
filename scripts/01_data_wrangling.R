
# Description -------------------------------------------------------------

# Change winter MEZCAL and SPECTRA larval fish abundance data into a usable
# form. As of 7/30/2025, these data are incomplete.


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(purrr)
library(gtools)
library(marmap)
library(lubridate)


# Load data ---------------------------------------------------------------

# MOCNESS larval fish abundance
mocness_2018_02_fish_abundance <- read.csv(here("data/18_w_mezcal_fish_ids.csv"))
mocness_2019_03_fish_abundance <- read.csv(here("data/19_w_mezcal_fish_ids.csv"))
mocness_2022_03_fish_abundance_raw <- read.csv(here("data/22_w_SPECTRA_fish_inventory.csv"))
mocness_2023_02_fish_abundance <- read.csv(here("data/23_w_SPECTRA_fish_inventory.csv"))

# MEZCAL metadata
mocness_2018_2019_metadata <- read.csv(here("data/mezcal_envr.csv"))

# MEZCAL MOCNESS environmental data
mocness_2018_2019_environmental <- read.csv(here("data/mocness_metadata.csv"))

# ISIIS environmental data
files <- c("MEZCAL_101_NH_IaN_binned_conc_w_dist.Rdata",
           "MEZCAL_105_NH_IaD_binned_conc_w_dist.Rdata",
           "MEZCAL_109_NH_IaD_binned_conc_w_dist.Rdata",
           "MEZCAL_113_Tr_IaD_binned_conc_w_dist.Rdata",
           "MEZCAL_301_NH_IaN_binned_conc_w_dist.Rdata",
           "MEZCAL_305_NH_IaD_binned_conc_w_dist.Rdata",
           "MEZCAL_309_NH_IbN_binned_conc_w_dist.Rdata",
           "MEZCAL_313_NH_IbD_binned_conc_w_dist.Rdata",
           "MEZCAL_317_TR_IaD_binned_conc_w_dist.Rdata",
           "MEZCAL_321_TR_IaN_binned_conc_w_dist.Rdata",
           "MEZCAL_325_TR_IbD_binned_conc_w_dist.Rdata",
           "MEZCAL_329_TR_IbN_binned_conc_w_dist.Rdata")

# Write function to load all files in "files"
load_one <- function(f) {
  env <- new.env()
  load(file.path(here("data"), f), envir = env)
  env[[ls(env)]]  # grab the object (biophys.9)
}

# Load the files
isiis_list <- map(files, load_one)
names(isiis_list) <- tools::file_path_sans_ext(files)

# Load anchovy observational data that have been matched with GLORYS mixed layer depth
glorys_covariates <- read.csv(here("data/glorys_covariates_all.csv"))


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


# Geographic data ---------------------------------------------------------

# # Download bathymetry data for OR and CA at 1-minute spatial grid
# bathy <- getNOAA.bathy(lon1 = -127, lon2 = -122,
#                        lat1 = 40, lat2 = 46,
#                        resolution = 1)
# 
# # Save bathymetry data locally for occasions when server is down
# saveRDS(bathy, file = here("data/marmap_bathymetry.rds"))

bathy <- readRDS(here("data/marmap_bathymetry.rds"))

# Get list of sampling stations from whatever data frame contains the working version of the data set
# Currently only returns MEZCAL stations because SPECTRA lat/lon haven't been added in yet
sampling_stations_geographic <- mocness_full %>%
  distinct(latitude_dd, longitude_dd) %>%
  filter(!is.na(latitude_dd) & !is.na(longitude_dd)) %>%
  # Get depth of each sampling station
  mutate(seafloor_depth_m = get.depth(bathy, x = longitude_dd, y = latitude_dd, locator = FALSE)$depth) %>%
  # Get distance to shore from each sampling station
  mutate(distance_to_shore_km = dist2isobath(bathy, x = longitude_dd, y = latitude_dd, isobath = 0, locator = FALSE)$distance 
         # Convert distance from m to km
         / 1000) %>%
  # Evaluate position relative to 200-m isobath
  mutate(shelf_position = ifelse(seafloor_depth_m > -200, "shelf", "offshore"))

mocness_full_geographic <- merge(mocness_full, sampling_stations_geographic, all.x = TRUE, by = c("latitude_dd", "longitude_dd"))


# ISIIS environmental data ------------------------------------------------

# Combine ISIIS data into one data frame
isiis_all <- do.call(smartbind, isiis_list) %>%
  mutate(time_Pacific = ymd_hms(time_Pacific, quiet = TRUE),
         grp_month = floor_date(time_Pacific, "month"))  # month key

# Get unique MOCNESS sampling events from mocness_full
sampling_events <- mocness_full %>%
  distinct(transect, station, collection_date, time, latitude_dd, longitude_dd, maximum_depth_m, minimum_depth_m) %>%
  mutate(event_datetime = ymd_hms(paste(collection_date, time))) %>%
  # 5 km is roughly 0.045 deg latitude near Oregon
  mutate(latitude_max_dd = latitude_dd + 0.045,
         latitude_min_dd = latitude_dd - 0.045,
         # 5 km is roughly 0.063 deg longitude near Oregon
         longitude_max_dd = longitude_dd + 0.063,
         longitude_min_dd = longitude_dd - 0.063)

# --- 1) For each sampling event, find the nearest (Transect_ID, month) in time among ISIIS points inside the 5-km box
event_to_group <- sampling_events %>%
  rowwise() %>%
  mutate(nearest = list({
    # Spatial subset for THIS event
    sub <- isiis_all %>%
      filter(Lat  >= latitude_min_dd,
             Lat  <= latitude_max_dd,
             Long >= longitude_min_dd,
             Long <= longitude_max_dd,
             Depth >= minimum_depth_m,
             Depth <= maximum_depth_m)
    
    if (nrow(sub) == 0) {
      tibble()  # no nearby ISIIS points
    } else {
      # Representative time per (Transect_ID, month)
      sub %>%
        group_by(Transect_ID, grp_month) %>%
        summarise(
          mid_time = as_datetime(median(as.numeric(time_Pacific), na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        filter(!is.na(mid_time)) %>%
        mutate(time_diff = abs(difftime(mid_time, event_datetime, units = "secs"))) %>%
        arrange(time_diff, mid_time) %>%   # deterministic tie-break
        slice(1L) %>%                      # pick the closest group
        select(Transect_ID, grp_month, mid_time, time_diff)
    }
  })) %>%
  ungroup() %>%
  unnest(nearest)

# If some events had no ISIIS in the box, they'll be dropped here. Optional check:
# anti_join(sampling_events, event_to_transect, by = c("transect","station","collection_date","time","latitude_dd","longitude_dd","event_datetime","latitude_max_dd","latitude_min_dd","longitude_max_dd","longitude_min_dd"))

# --- 2) Attach ALL ISIIS rows from that (Transect_ID, month) and re-apply the event’s box
isiis_matched <- event_to_group %>%
  left_join(isiis_all, by = c("Transect_ID", "grp_month")) %>%
  filter(Lat  >= latitude_min_dd,
         Lat  <= latitude_max_dd,
         Long >= longitude_min_dd,
         Long <= longitude_max_dd,
         Depth >= minimum_depth_m,
         Depth <= maximum_depth_m)

isiis_means_by_mocness_tow <- isiis_matched %>%
  group_by(transect, station, event_datetime, maximum_depth_m, minimum_depth_m) %>%
  mutate(prey_zooplankton_abundance_ind_m3 = 
           sum(appendicularian, copepod_calanoid_calanus, copepod_calanoid_diaptomoidea,
               copepod_calanoid_mesocalanus, copepod_calanoid_metridia, copepod_calanoid_other,
               copepod_calanoid_paracalanidae, copepod_calanoid_paraeuchaeta, copepod_calanoid_pseudocalanus_mean_minor,
               copepod_cyclopoid_oithona, copepod_cyclopoid_oithona_eggs, copepod_eucalaniid,
               copepod_other, copepod_poecilostomatoid, crustacean_ostracod,
               crustacean_zoea, echinoderm_brachiolaria, echinoderm_pluteus,
               polychaete_larvae, na.rm = TRUE),
         dissolved_oxygen_ml_l = mean(Oxygen, na.rm = TRUE),
         seawater_density_1000_kg_m3 = mean(sw.density, na.rm = TRUE),
         chlorophyll_ug_l = mean(chl.ug.l, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(transect, station, event_datetime, maximum_depth_m, minimum_depth_m,
           prey_zooplankton_abundance_ind_m3, dissolved_oxygen_ml_l, seawater_density_1000_kg_m3,
           chlorophyll_ug_l) %>%
  mutate(date = as_date(event_datetime)) %>%
  select(-event_datetime)

mocness_full_geographic_isiis <- merge(mocness_full_geographic, isiis_means_by_mocness_tow,
                                       all.x = TRUE, 
                                       by.x = c("collection_date", "transect", "station", "maximum_depth_m", "minimum_depth_m"),
                                       by.y = c("date", "transect", "station", "maximum_depth_m", "minimum_depth_m"))


# GLORYS mixed layer depth ---------------------------------------------------

# Get unique MOCNESS sampling events from mocness_full
mixed_layer_depth <- glorys_covariates %>%
  distinct(date, latitude_dd, longitude_dd, mlotst)

mocness_full_geographic_isiis_mixing <- merge(mocness_full_geographic_isiis, mixed_layer_depth,
                                              all.x = TRUE,
                                              by.x = c("collection_date", "latitude_dd", "longitude_dd"),
                                              by.y = c("date", "latitude_dd", "longitude_dd"))


# MOCNESS data cleanup ----------------------------------------------------

# Add columns with combined location/station and min/max depth
mocness_clean <- mocness_full_geographic_isiis_mixing %>%
  unite(col = "transect_station", transect, station, sep="_", remove = FALSE) %>%
  unite(col = "transect_station_rep", transect_station, replicate, sep="_", remove=FALSE) %>%
  mutate(year = year(collection_date)) %>%
  unite(col = "transect_station_rep_year", transect_station_rep, year, sep = "_", remove = FALSE) %>%
  unite(col = "depth_range", minimum_depth_m, maximum_depth_m, sep="-", remove = TRUE) %>%
  mutate(taxon = case_match(taxon, 
                            c("Xeneretmus spp.", "Xeneretmus latifrons", "Agonidae spp.") ~ "Agonidae",
                            "Ammodytes spp." ~ "Ammodytes",
                            "Anarrhichthys ocellatus" ~ "Anarrhichthys ocellatus",
                            c("Anoplopomatidae spp.", "Anoploploma fimbria", "Anaploploma fimbria") ~ "Anoplopomatidae",
                            "Lipolagus ochotensis" ~ "Lipolagus ochotensis",
                            "Bathylagus pacificus" ~ "Bathylagus pacificus",
                            "Ronquilus jordani" ~ "Ronquilus jordani",
                            "Chauliodus macouni" ~ "Chauliodus macouni",
                            c("Sardinops sagax", "Sardinops sargax") ~ "Sardinops sagax",
                            c("Artedius spp.", "Artedius harringtoni", "Artedius fenestralis") ~ "Artedius",
                            c("Cottidae spp.", "Cottid spp.", "Cottidae", "Scorpaenichthys marmoratus", "Nautichthys spp.", 
                              "Leptocottus armatus", "Hemilepidotus spp.", "Hemilepidotus spinosus", "Radulinus spp.", 
                              "Radulinus asprellus", "Radulina asprellus") ~ "Cottidae",
                            c("Cryptacanthodes spp.”, “Cryptacanthodes aleutensis") ~ "Cryptacanthodes",
                            c("Liparis spp.", "Liparis fucensis") ~ "Liparis",
                            "Engraulis mordax" ~ "Engraulis mordax",
                            c("Gadid spp.”, “Microgadus proximus") ~ "Gadidae",
                            "Gobiidae spp." ~ "Gobiidae",
                            c("Hexagrammidae spp.", "Hexagrammos octogrammus", "Hexagrammos decagrammus", "Ophiodon elongatus", 
                              "Hexagrammos lagocephalus", "Hexagrammos lagocephalus") ~ "Hexagrammidae",
                            c("Macrourid spp.", "Coryphaenoids acrolepis", "Coryphaenoides acrolepis", 
                              "Albatrossia pectoralis", "Macrouridae") ~ "Macrouridae",
                            "Merluccius productus" ~ "Merluccius productus",
                            "Nansenia candida" ~ "Nansenia candida",
                            c("Myctophid spp.", "Nannobrachium regalis", "Protomyctophum crockeri", "Protomyctophum thompsoni", 
                              "Stenobrachius leucopsarus", "Tarletonbeania crenularis", "Diaphus theta", 
                              "Nannobrachium spp.") ~ "Myctophidae",
                            "Chilara taylori" ~ "Chilara taylori",
                            c("Osmerid spp", "Osmerid spp.") ~ "Osmeridae",
                            "Lestidiops ringens" ~"Lestidiops ringens",
                            c("Citharichthys spp.", "Citharichthys sordidus", "Citharichthys stigmaeus") ~ "Paralichthyidae",
                            c("Pholidae spp.", "Apodichthus flavidus", "Apodichthys flavidus") ~ "Pholidae",
                            "Parophrys vetulus" ~ "Parophrys vetulus",
                            c("Atheresthes stomias", "Glyptocephalus zachirus", "Psettichthys melanostictus", "Lyopsetta exilis", 
                              "Isopsetta isolepis", "Microstomus pacificus", "Lepidopsetta bilineata", "Embassichthys bathybius", 
                              "Eopsetta jordani", "Pleuronichthys decurrens") ~ "Pleuronectidae",
                            "Ptilichthys goodei" ~ "Ptilichthys goodei",
                            "Sebastolobus spp." ~ "Sebastolobus",
                            "Sebastes spp." ~ "Sebastes",
                            c("Chirolophis spp.", "Xiphister atrophurpureus", "Xiphister atrophurpureus", 
                              "Plectobranchus evides") ~ "Stichaeidae",
                            "Chauliodus spp." ~ "Chauliodontidae",
                            "Trachipterus altivelis" ~ "Trachipterus altivelis",
                            .default = taxon)) %>%
  mutate(individuals_in_tow = as.numeric(individuals_in_tow))


# filter out rare taxa (present in <5% of samples) -----------------------

taxa_w_gt_5pct <- mocness_clean %>%
  filter(individuals_in_tow != "") %>%
  mutate(individuals_in_tow = as.numeric(individuals_in_tow)) %>%
  filter(individuals_in_tow > 0) %>%
  group_by(taxon) %>%
  summarize(freq = n_distinct(transect_station_rep)) %>%
  ungroup() %>%
  filter(freq >= 0.05 * n_distinct(mocness_clean$transect_station_rep))

mocness_major_taxa <- mocness_clean %>%
  filter(taxon %in% taxa_w_gt_5pct$taxon & taxon != "Unknown" & !is.na(taxon) & taxon != "Damaged" & taxon != "" & taxon != "Fish eggs")

# Get list of date/station/replicate with > 20 individuals of any "major" taxa
stations_w_gt_20ind <- mocness_major_taxa %>%
  group_by(collection_date, transect, station, replicate) %>%
  summarize(individuals_per_station = sum(individuals_in_tow), .groups = "drop") %>%
  filter(individuals_per_station >= 26)

# Filter out stations with few fish larvae, which will be excluded from cluster analysis
mocness_major_taxa_stations <- inner_join(mocness_major_taxa, stations_w_gt_20ind, by = c("collection_date", "transect", "station", "replicate"))

##filter to keep only 2018-2019 data and those with values for mixed layer depth for the time being

mocness_major_taxa_2018_2019 <- filter(mocness_major_taxa_stations, collection_date < "2020-01-01")
mocness_major_taxa_2018_2019_MLD <- filter(mocness_major_taxa_2018_2019, !is.na(mlotst))

