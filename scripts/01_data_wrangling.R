
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
library(sf)
library(RColorBrewer)


# Load data ---------------------------------------------------------------

# MOCNESS larval fish abundance
mocness_2018_02_fish_abundance <- read.csv(here("data/18_w_mezcal_fish_ids.csv"))
mocness_2019_03_fish_abundance <- read.csv(here("data/19_w_mezcal_fish_ids.csv"))
mocness_2022_03_fish_abundance_raw <- read.csv(here("data/22_w_SPECTRA_fish_inventory.csv"))
mocness_2023_02_fish_abundance <- read.csv(here("data/23_w_SPECTRA_fish_inventory.csv"))

# MEZCAL and SPECTRA MOCNESS metadata
mocness_metadata <- read.csv(here("data/mocness_metadata.csv"))

# MEZCAL metadata
mocness_2018_2019_metadata <- read.csv(here("data/mezcal_envr.csv"))

# MEZCAL MOCNESS environmental data
mocness_2018_2019_environmental <- read.csv(here("data/mocness_metadata_old.csv"))

# SPECTRA MOCNESS sampling station coordinates
spectra_sampling_stations <- read.csv(here("data/spectra_mocness_coordinates.csv"))

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

# Load ISIIS environmental data for W22 and W23
isiis_w22_env <- read.csv(here("data/W22_ISIIS3_enviro.csv"))
isiis_w23_env <- read.csv(here("data/W23_ISIIS3_enviro.csv"))

# Load anchovy observational data that have been matched with GLORYS mixed layer depth
glorys_covariates <- read.csv(here("data/glorys_covariates_all_derived.csv"))

# CTD fluorescence data
ctd_fluorescence <- read.csv(here("data/ctd_fluorescence_binned.csv"))


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
  mutate(collection_date = as.Date(collection_date, format = "%m/%d/%Y")) %>%
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

# Get separate columns for transect and station in spectra_sampling_stations
spectra_sampling_stations_clean <- spectra_sampling_stations %>%
  rename(latitude_dd = Latitude,
         longitude_dd = Longitude) %>%
  mutate(transect = substr(Name, 1, 2),
         station = substr(Name, 3, n()),
         project = "SPECTRA") %>%
  select(-Station.Type, -Name)

# Get separate column for time of SPECTRA MOCNESS sampling events
spectra_sampling_times <- mocness_2018_2019_environmental %>%
  mutate(time = substr(cast_start_date_time_pt, 12, 19)) %>%
  distinct(transect, station, date_pt, time) %>%
  filter(date_pt > "2022-01-01")

# Merge fish abundance data with metadata by haul_number
mocness_full_old_metadata <- merge(mocness_winter_fish_abundance, 
                           mocness_2018_2019_metadata_reformat_date, by = c("project", "haul_number"),
                      all.x = TRUE) %>%
  # Take 'collection_date' from 'date' column when NA
  mutate(collection_date = as.Date(ifelse(is.na(collection_date), date, collection_date)),
         mocness_side = str_split(haul_number, "-", simplify = TRUE)[, 2],
         net = str_split(haul_number, "-", simplify = TRUE)[, 3],
         net = case_when(collection_date == "2023-02-17" & transect == "GH" & station == 6 & mocness_side == 4 ~ "0",
                         .default = net)) %>%
  # Keep only date, time_gmt, haul_number, maximum_depth_m, minimum_depth_m, latitude_dd,
  # longitude_dd, family, species, and concentration_ind_1000m3
  select(project, collection_date, time, haul_number, replicate, maximum_depth_m, minimum_depth_m, 
         transect, station, mocness_side, net, latitude_dd = Station.lat, longitude_dd = Station.lon, taxon, 
         volume_filtered_m3 = Volume.filtered, individuals_in_tow) %>%
  # Add in lat/lon for SPECTRA sampling stations
  merge(., spectra_sampling_stations_clean, by = c("project", "transect", "station"), all.x = TRUE) %>%
  # Make latitude_dd column with values of latitude_dd.x if available, or values of latitude_dd.y if not
  mutate(latitude_dd = ifelse(!is.na(latitude_dd.x), latitude_dd.x, latitude_dd.y)) %>%
  # Make longitude_dd column with values of longitude_dd.x if available, or values of longitude_dd.y if not
  mutate(longitude_dd = ifelse(!is.na(longitude_dd.x), longitude_dd.x, longitude_dd.y)) %>%
  select(-latitude_dd.x, -latitude_dd.y, -longitude_dd.x, -longitude_dd.y) %>%
  # Add in sampling time for SPECTRA MOCNESS deployments
  merge(., spectra_sampling_times, by.x = c("transect", "station", "collection_date"), by.y = c("transect", "station", "date_pt"), all.x = TRUE) %>%
  # Make time column with values of time.x if available, or values of time.y if not
  mutate(time = ifelse(!is.na(time.x), time.x, time.y)) %>%
  select(-time.x, -time.y) %>%
  # add temp and salinity from environmental df
  merge(., mocness_2018_2019_environmental, 
        by.x = c("transect", "station", "replicate", "collection_date", "net"), 
        by.y = c("transect", "station", "replicate", "date_pt", "net")) %>%
  select(-cast_start_date_time_utc, -cast_start_date_time_pt, -date_time_closed_utc, 
         -date_time_closed_pt, -depth_closed_m, -volume_filtered_m3.x)

# Add convenient "collection_date" to mocness_metadata
mocness_metadata_collection_date <- mocness_metadata %>%
  mutate(start_time_pt = as.POSIXct(start_time_pt, tz = "America/Los_Angeles")) %>%
  mutate(collection_date = as.Date(start_time_pt, tz = "America/Los_Angeles")) %>%
  # Calculate volume of water sampled by both MOCNESS sides during each tow
  group_by(cruise, transect, station, replicate, net) %>%
  mutate(volume_best_m3_both_sides = sum(volume_m3_best)) %>%
  ungroup()

# Swap new metadata in for old
mocness_full <- mocness_full_old_metadata %>%
  select(project, transect, station, replicate, collection_date, mocness_side,
         net, taxon, individuals_in_tow) %>% 
  # Add in new metadata
  merge(., mocness_metadata_collection_date, by = c("transect", "station", "replicate", "collection_date", "mocness_side", "net"), all.x = TRUE)


# Geographic data ---------------------------------------------------------

# # Download bathymetry data for OR and CA at 1-minute spatial grid
# bathy <- getNOAA.bathy(lon1 = -127, lon2 = -122,
#                        lat1 = 40, lat2 = 48,
#                        resolution = 1)
# 
# # Save bathymetry data locally for occasions when server is down
# saveRDS(bathy, file = here("data/marmap_bathymetry.rds"))

bathy <- readRDS(here("data/marmap_bathymetry.rds"))

# Get list of sampling stations from whatever data frame contains the working version of the data set
# Currently only returns MEZCAL stations because SPECTRA lat/lon haven't been added in yet
sampling_stations_geographic <- mocness_full %>%
  distinct(start_latitude_dd, start_longitude_dd) %>%
  filter(!is.na(start_latitude_dd) & !is.na(start_longitude_dd)) %>%
  # Get depth of each sampling station
  mutate(seafloor_depth_m = get.depth(bathy, x = start_longitude_dd, y = start_latitude_dd, locator = FALSE)$depth) %>%
  # Get distance to shore from each sampling station
  mutate(distance_to_shore_km = dist2isobath(bathy, x = start_longitude_dd, y = start_latitude_dd, isobath = 0, locator = FALSE)$distance 
         # Convert distance from m to km
         / 1000) %>%
  # Evaluate position relative to 200-m isobath
  mutate(shelf_position = ifelse(seafloor_depth_m > -200, "shelf", "offshore"))

mocness_full_geographic <- merge(mocness_full, sampling_stations_geographic, all.x = TRUE, by = c("start_latitude_dd", "start_longitude_dd"))


# ISIIS environmental data ------------------------------------------------

# Keep only needed columns from winter 2022 and winter 2023 ISIIS environmental data sets
isiis_w22_w23_env <- rbind(isiis_w22_env, isiis_w23_env) %>%
  mutate(sw.density = oce::swRho(Salinity, Temperature, Pressure, eos = "unesco")) %>%
  rename(time_Pacific = Time_PT, Lat = Latitude, Long = Longitude, chl.ug.l = chl_a_ul) %>%
  select(time_Pacific, Depth, Lat, Long, Oxygen, sw.density, chl.ug.l) %>%
  mutate(Transect_ID = case_when(Lat > 47 ~ "GH",
                                 Lat > 46 & Lat < 46.5 ~ "CR",
                                 Lat > 45 & Lat < 46 ~ "CM",
                                 Lat > 44.5 & Lat < 45 ~ "NH",
                                 Lat > 43 & Lat < 44.5 ~ "HH",
                                 Lat < 43 ~ "RR"))

# Combine ISIIS data into one data frame
isiis_all <- do.call(smartbind, isiis_list) %>%
  smartbind(., isiis_w22_w23_env) %>%
  mutate(time_Pacific = ymd_hms(time_Pacific, quiet = TRUE),
         grp_month = floor_date(time_Pacific, "month"))

# Get unique MOCNESS sampling events from mocness_full
sampling_events <- mocness_full %>%
  distinct(transect, station, collection_date, start_time_pt, start_latitude_dd, start_longitude_dd, maximum_depth_m, minimum_depth_m) %>%
  # 5 km is roughly 0.045 deg latitude near Oregon
  mutate(latitude_max_dd = start_latitude_dd + 0.045,
         latitude_min_dd = start_latitude_dd - 0.045,
         # 5 km is roughly 0.063 deg longitude near Oregon
         longitude_max_dd = start_longitude_dd + 0.063,
         longitude_min_dd = start_longitude_dd - 0.063,
         event_id = row_number())

# Rectangles (polygons) for event boxes
events_sf <- sampling_events %>%
  mutate(
    geometry = pmap(
      list(longitude_min_dd, latitude_min_dd, longitude_max_dd, latitude_max_dd),
      \(xmin, ymin, xmax, ymax) {
        if (anyNA(c(xmin, ymin, xmax, ymax))) return(st_geometrycollection())
        st_polygon(list(matrix(
          c(xmin, ymin,
            xmax, ymin,
            xmax, ymax,
            xmin, ymax,
            xmin, ymin),
          ncol = 2, byrow = TRUE)))
      }
    )
  ) %>%
  st_as_sf(crs = 4326)

# Build ISIIS points
isiis_needed <- c(
  "Transect_ID", "grp_month", "Long", "Lat", "Depth",
  "time_Pacific", "Oxygen", "sw.density", "chl.ug.l",
  # prey columns used later:
  "appendicularian",
  "copepod_calanoid_calanus", "copepod_calanoid_diaptomoidea",
  "copepod_calanoid_mesocalanus", "copepod_calanoid_metridia",
  "copepod_calanoid_other", "copepod_calanoid_paracalanidae",
  "copepod_calanoid_paraeuchaeta", "copepod_calanoid_pseudocalanus_mean_minor",
  "copepod_cyclopoid_oithona", "copepod_cyclopoid_oithona_eggs",
  "copepod_eucalaniid", "copepod_other", "copepod_poecilostomatoid",
  "crustacean_ostracod", "crustacean_zoea",
  "echinoderm_brachiolaria", "echinoderm_pluteus",
  "polychaete_larvae"
)

isiis_pts <- isiis_all %>%
  select(any_of(isiis_needed)) %>%
  mutate(
    # Harmonize timezone for safety; comment out if already PT
    time_Pacific = with_tz(time_Pacific, "America/Los_Angeles")
  ) %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

# Candidate pairs via spatial join with depth filter
cand_pts <- st_join(
  isiis_pts,
  events_sf %>% select(event_id, start_time_pt, minimum_depth_m, maximum_depth_m),
  join = st_within,
  left = FALSE
)

# Depth filter using the event-specific bounds:
cand_pts <- cand_pts %>%
  filter(Depth >= minimum_depth_m, Depth <= maximum_depth_m)

# Compute representative mid_time PER (event_id, Transect_ID, grp_month) **from the points inside the event box**
cand_grp <- cand_pts %>%
  st_drop_geometry() %>%
  group_by(event_id, Transect_ID, grp_month, start_time_pt) %>%
  summarise(
    mid_time = as_datetime(median(as.numeric(time_Pacific), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  filter(!is.na(mid_time)) %>%
  mutate(time_diff = abs(difftime(mid_time, start_time_pt, units = "secs")))

# Pick the nearest (Transect_ID, grp_month) per event
event_to_group <- cand_grp %>%
  arrange(event_id, time_diff, mid_time) %>%
  group_by(event_id) %>%
  slice(1L) %>%
  ungroup()

# Bring back event attributes needed later (transect, station, bounds, etc.)
event_to_group <- event_to_group %>%
  inner_join(
    sampling_events %>%
      select(event_id, transect, station, collection_date,
             maximum_depth_m, minimum_depth_m,
             latitude_min_dd, latitude_max_dd,
             longitude_min_dd, longitude_max_dd),
    by = "event_id"
  )

# Attach all ISIIS rows for the chosen Transect_ID/grp_month and re-apply event box + depth
# Keep only chosen pairs
chosen_pairs <- event_to_group %>% distinct(event_id, Transect_ID, grp_month)

# Subset ISIIS table to those pairs (keep all rows for the pair)
isiis_all_chosen <- isiis_all %>%
  semi_join(chosen_pairs, by = c("Transect_ID", "grp_month")) %>%
  select(any_of(isiis_needed))

# Convert those rows to sf points (for spatial filtering into each event polygon)
isiis_pts_chosen <- isiis_all_chosen %>%
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

# Attach an event_id to each point by spatial containment, but only for the events we actually matched
events_matched_sf <- events_sf %>%
  semi_join(chosen_pairs, by = "event_id")

pts_in_events <- st_join(
  isiis_pts_chosen,
  events_matched_sf %>%
    select(event_id, maximum_depth_m, minimum_depth_m),
  join = st_within,
  left = FALSE
)

# Now restrict to the specific (event_id, Transect_ID, grp_month) *pair* chosen for that event
pts_in_events <- pts_in_events %>%
  st_drop_geometry() %>%
  inner_join(chosen_pairs, by = c("event_id", "Transect_ID", "grp_month")) %>%
  # event-specific depth bounds:
  filter(Depth >= minimum_depth_m, Depth <= maximum_depth_m)

# Summarize to event-level means/sums
prey_cols <- c(
  "appendicularian",
  "copepod_calanoid_calanus", "copepod_calanoid_diaptomoidea",
  "copepod_calanoid_mesocalanus", "copepod_calanoid_metridia",
  "copepod_calanoid_other", "copepod_calanoid_paracalanidae",
  "copepod_calanoid_paraeuchaeta", "copepod_calanoid_pseudocalanus_mean_minor",
  "copepod_cyclopoid_oithona", "copepod_cyclopoid_oithona_eggs",
  "copepod_eucalaniid", "copepod_other", "copepod_poecilostomatoid",
  "crustacean_ostracod", "crustacean_zoea",
  "echinoderm_brachiolaria", "echinoderm_pluteus",
  "polychaete_larvae"
)

isiis_means_by_mocness_tow <- pts_in_events %>%
  # mark rows where *all* prey columns are NA
  mutate(all_prey_na = if_all(all_of(prey_cols), ~ is.na(.x))) %>%
  # row-sum, but keep NA if all prey are NA
  mutate(
    prey_row = if_else(
      all_prey_na,
      NA_real_,
      rowSums(across(all_of(prey_cols)), na.rm = TRUE)
    )
  ) %>%
  # bring in event keys
  inner_join(
    event_to_group %>% select(event_id, transect, station, start_time_pt),
    by = "event_id"
  ) %>%
  group_by(transect, station, start_time_pt, maximum_depth_m, minimum_depth_m) %>%
  summarise(
    # keep NA if every prey_row in the group is NA
    prey_zooplankton_abundance_ind_m3 =
      if (all(is.na(prey_row))) NA_real_ else sum(prey_row, na.rm = TRUE),
    
    # optional: do the same “all NA -> NA” guard for means
    dissolved_oxygen_ml_l =
      if (all(is.na(Oxygen))) NA_real_ else mean(Oxygen, na.rm = TRUE),
    seawater_density_1000_kg_m3 =
      if (all(is.na(sw.density))) NA_real_ else mean(sw.density, na.rm = TRUE),
    chlorophyll_ug_l =
      if (all(is.na(chl.ug.l))) NA_real_ else mean(chl.ug.l, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(date = as_date(start_time_pt)) %>%
  select(-start_time_pt)

# Merge with MOCNESS data
mocness_full_geographic_isiis <- merge(
  mocness_full_geographic,
  isiis_means_by_mocness_tow,
  all.x = TRUE,
  by.x = c("collection_date", "transect", "station", "maximum_depth_m", "minimum_depth_m"),
  by.y = c("date",            "transect", "station", "maximum_depth_m", "minimum_depth_m")
)


# GLORYS mixed layer depth ---------------------------------------------------

# Get unique MOCNESS sampling events from mocness_full
mixed_layer_depth <- glorys_covariates %>%
  distinct(date, latitude_dd, longitude_dd, mlotst) %>%
  mutate(date = as.Date(date))

mocness_full_geographic_isiis_mixing <- left_join(mocness_full_geographic_isiis, mixed_layer_depth,
                                              by = c("collection_date" = "date",
                                                     "start_latitude_dd" = "latitude_dd",
                                                     "start_longitude_dd" = "longitude_dd"))


# CTD fluorescence --------------------------------------------------------

# Get coordinates for each MOCNESS net tow (start_time_pt, transect, replicate, station, net)
mocness_tow_coordinates <- mocness_metadata %>%
  distinct(start_time_pt, transect, replicate, station, net, .keep_all = TRUE) %>%
  select(cruise, transect, replicate, station, net, start_time_pt,
         start_longitude_dd, start_latitude_dd) %>%
  mutate(moc_id = row_number(),
         start_time_pt = as.POSIXct(start_time_pt, tz = "America/Los_Angeles")) %>%
  # Add point geometries
  st_as_sf(coords = c("start_longitude_dd", "start_latitude_dd"), crs = 4326, remove = FALSE)

# Calculate average fluorescence in top 100 m of water column
ctd_fluorescence_0_100_m <- ctd_fluorescence %>%
  filter(depth_bin_mid_m < 100) %>%
  group_by(start_time_pt, start_longitude_dd, start_latitude_dd) %>%
  summarize(mean_chl_0_100_m_mgm3 = mean(mean_fluor), .groups = "drop")

# Get coordinates for each CTD cast
ctd_coordinates <- ctd_fluorescence_0_100_m %>%
  st_as_sf(coords = c("start_longitude_dd", "start_latitude_dd"), crs = 4326, remove = FALSE) %>%
  mutate(ctd_id = row_number()) %>%
  select(ctd_id, ctd_time_pt = start_time_pt, mean_chl_0_100_m_mgm3)

# Get all candidate pairs within 1 km
pairs <- st_join(mocness_tow_coordinates, ctd_coordinates,
                 join = st_is_within_distance,
                 dist = 5000,
                 left = TRUE)

# Compute time diff and spatial distance for each candidate
pairs <- pairs %>%
  mutate(time_diff_min = if_else(is.na(ctd_time_pt), NA_real_,
                                 abs(as.numeric(difftime(ctd_time_pt, start_time_pt, units = "mins")))),
         dist_m = if_else(is.na(ctd_id), NA_real_,
                          as.numeric(st_distance(geometry, st_geometry(ctd_coordinates)[ctd_id], by_element = TRUE))))

# Pick best CTD per MOC: closest in time, tie-break by distance
ctd_with_mocness <- pairs %>%
  group_by(moc_id) %>%
  slice_min(order_by = time_diff_min, n = 1, with_ties = TRUE) %>%
  slice_min(order_by = dist_m, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(start_time_pt, mean_chl_0_100_m_mgm3) %>%
  st_drop_geometry(geometry)

# Merge with full data frame
mocness_full_geographic_isiis_mixing_fluor <- left_join(mocness_full_geographic_isiis_mixing, ctd_with_mocness,
                                                        by = c("start_time_pt")) %>%
  # Remove ISIIS chlorophyll values
  select(-chlorophyll_ug_l)


# MOCNESS data cleanup ----------------------------------------------------

# Add columns with combined location/station and min/max depth
mocness_clean <- mocness_full_geographic_isiis_mixing_fluor %>%
  unite(col = "transect_station", transect, station, sep="_", remove = FALSE) %>%
  unite(col = "transect_station_rep", transect_station, replicate, sep="_", remove=FALSE) %>%
  mutate(year = year(collection_date)) %>%
  unite(col = "transect_station_rep_year", transect_station_rep, year, sep = "_", remove = FALSE) %>%
  unite(col = "transect_station_rep_year_net", transect_station_rep_year, net, sep = "_", remove = FALSE) %>%
  unite(col = "depth_range", minimum_depth_m, maximum_depth_m, sep="-", remove = FALSE) %>%
  # Eliminate spaces to avoid inconvenient behavior later when each taxon is made into its own column
  mutate(taxon = case_match(taxon, 
                            c("Xeneretmus spp.", "Xeneretmus latifrons", "Agonidae spp.") ~ "Agonidae",
                            "Alepocephalidae" ~ "Alepocephalidae",
                            c("Ammodytes spp.", "Ammodytidae") ~ "Ammodytidae",
                            "Anarrhichthys ocellatus" ~ "Anarrhichthys_ocellatus", # Our only species in Anarhichadidae
                            c("Anoplopomatidae spp.", "Anoploploma fimbria", "Anaploploma fimbria", "Anoplopoma fimbria") ~ "Anoplopomatidae",
                            c("Atheresthes stomias", "Atherestes stomias") ~ "Atheresthes_stomias", # Listed in Eschmeyer's Catalog of Fishes as Platysomatichthys stomia
                            "Lipolagus ochotensis" ~ "Bathylagus_ochotensis", # Listed in Eschmeyer's Catalog of Fishes as Bathylagus ochotensis
                            "Bathylagus pacificus" ~ "Bathylagus_pacificus", # Keeping Bathylagus pacificus and Bathylagus ochotensis separate because of differences in adult habitat affinity
                            c("Ronquilus jordani", "Bathymasterid spp.") ~ "Bathymasteridae",
                            "Clupeidae" ~ "Clupeidae_unidentified",
                            c("Cottidae spp.", "Cottid spp.", "Cottidae", "Cottid spp. ", "Leptocottus armatus") ~ "Cottidae",
                            c("Citharichthys spp.", "Citharichthys sordidus", "Citharichthys stigmaeus") ~ "Cyclopsettidae", # Citharichthys has now been separated into multiple genera, according to Eschmeyer's Catalog of Fishes, and Ross originally had these grouped at the family level
                            "Diaphus theta" ~ "Diaphus_theta",
                            "Engraulis mordax" ~ "Engraulis_mordax",
                            "Eopsetta jordani" ~ "Eopsetta_jordani", # Listed in Eschmeyer's Catalog of Fishes as Hippoglossoides jordani
                            c("Gadid spp.", "Microgadus proximus") ~ "Gadidae",
                            "Glyptocephalus zachirus" ~ "Glyptocephalus_zachirus",
                            "Gobiidae spp." ~ "Gobiidae",
                            c("Hemilepidotus spinosus", "Hemilepodotus spinosus", "Hemilepidotus spp.", "Hemilepodotus spp.") ~ "Hemilepidotus_spp",
                            c("Hexagrammidae spp.", "Hexagrammos octogrammus", "Hexagrammos decagrammus", "Ophiodon elongatus", "Hexagrammos lagocephalus") ~ "Hexagrammidae",
                            "Isopsetta isolepis" ~ "Isopsetta_isolepis", # Listed in Eschmeyer's Catalog of Fishes as Lepidopsetta isolepis
                            "Lepidopsetta bilineata" ~ "Lepidopsetta_bilineata", # Listed in Eschmeyer's Catalog of Fishes as Platessa bilineata
                            "Lestidiops ringens" ~ "Lestidiops_ringens", # Listed in Eschmeyer's Catalog of Fishes as Lestidium elongatum
                            c("Liparis spp.", "Liparis fucensis", "Liparis pulchellus") ~ "Liparis_spp",
                            c("Lyconectes aleutensis") ~ "Lyconectes_aleutensis", # Our only species in Cryptacanthodidae
                            "Lyopsetta exilis" ~ "Lyopsetta_exilis", # Listed in Eschmeyer's Catalog of Fishes as Hippoglossoides exilis
                            c("Macrourid spp.", "Albatrossia pectoralis", "Macrouridae") ~ "Macrouridae",
                            "Merluccius productus" ~ "Merluccius_productus", # Listed in Eschmeyer's Catalog of Fishes as Merlangus productus
                            "Microstomus pacificus" ~ "Microstomus_pacificus", # Listed in Eschmeyer's Catalog of Fishes as Glyptocephalus pacificus
                            c("Myctophid spp.", "Myctophidae") ~ "Myctophidae_unidentified",
                            c("Nannobrachium regalis", "Nannobrachium spp.") ~ "Nannobrachium_spp", # Nannobrachium regale is listed as a synonym of Lampanyctus micropunctatus in Eschmeyer's Catalog of Fishes, but was probably recently separated from Myctophum regale
                            "Nansenia candida" ~ "Nansenia_candida", # Our only species in Microstomatidae
                            c("Nautichthys spp.", "Nautichthys spp. ") ~ "Nautichthys_spp", # Our only genus in Nautichthyidae
                            c("Osmerid spp.", "Osmeridae") ~ "Osmeridae",
                            "Parophrys vetulus" ~ "Parophrys_vetulus",
                            c("Pholidae spp.", "Apodichthus flavidus", "Apodichthys flavidus") ~ "Pholidae",
                            "Plectobranchus evides" ~ "Plectobranchus_evides", # Our only species in Opisthocentridae
                            "Pleuronectidae" ~ "Pleuronectidae_unidentified",
                            "Pleuronichthys decurrens" ~ "Pleuronichthys_decurrens", 
                            c("Protomyctophum crockeri", "Protomyctophum thompsoni") ~ "Protomyctophum_spp", # Listed in Eschmeyer's Catalog of Fishes as Electrona
                            "Psettichthys melanostictus" ~ "Psettichthys_melanostictus",
                            c("Artedius spp.", "Artedius fenestralis", "Radulinus spp.", "Radulinus asprellus", "Radulina asprellus", "Enophrys bison") ~ "Psychrolutidae", # According to Eschmeyer's Catalog of Fishes, not within Cottidae
                            "Ptilichthys goodei" ~ "Ptilichthys_goodei", # Our only species in Ptilichthyidae
                            c("Sardinops sagax", "Sardinops sargax") ~ "Sardinops_sagax",
                            "Scorpaenichthys marmoratus" ~ "Scorpaenichthys_marmoratus",
                            "Sebastes spp." ~ "Sebastes_spp", # It is possible that Sebastes has been broken up recently into multiple genera
                            "Sebastolobus spp." ~ "Sebastolobus_spp",
                            "Stenobrachius leucopsarus" ~ "Stenobrachius_leucopsarus", # Listed in Eschmeyer's Catalog of Fishes as Myctophum Leucopsarum
                            c("Chirolophis spp.", "Xiphister atrophurpureus", "Stichaeidae spp.") ~ "Stichaeidae",
                            c("Chauliodus macouni", "stomiidae") ~ "Stomiidae",
                            "Tarletonbeania crenularis" ~ "Tarletonbeania_crenularis", # Listed in Eschmeyer's Catalog of Fishes as Myctophum crenulare
                            "Trachipterus altivelis" ~ "Trachipterus_altivelis", # Our only species in Trachipteridae
                            .default = taxon)) %>%
  # Not grouping by mocness_side here because both sides will be aggregated for the entire analysis
  group_by(transect_station_rep_year_net, taxon) %>%
  mutate(individuals_in_tow = as.numeric(individuals_in_tow),
         # Sum number of individuals within each net/taxon and return one row per net/taxon
         individuals_in_tow = sum(individuals_in_tow)) %>%
  ungroup() %>%
  # Not grouping by mocness_side here because both sides will be aggregated for the entire analysis
  distinct(transect_station_rep_year, net, taxon, .keep_all = TRUE) %>%
  mutate(individuals_per_m3 = individuals_in_tow/volume_best_m3_both_sides) %>%
  mutate(depth_mean_m = (maximum_depth_m + minimum_depth_m)/2) %>%
  mutate(depth_diff_m = maximum_depth_m - minimum_depth_m) %>%
  select(project, year, cruise, collection_date, start_time_pt, start_latitude_dd, start_longitude_dd, 
         transect_station_rep_year_net, transect_station_rep_year, transect_station_rep,
         transect_station, transect, station, replicate, mocness_side, net, volume_best_m3_both_sides,
         depth_range, maximum_depth_m, minimum_depth_m, depth_mean_m, depth_diff_m, taxon, individuals_in_tow,
         individuals_per_m3, mean_temperature_c, mean_salinity_psu, mean_density_kgm3,
         seafloor_depth_m, distance_to_shore_km, shelf_position, prey_zooplankton_abundance_ind_m3,
         dissolved_oxygen_ml_l, mlotst, mean_chl_0_100_m_mgm3)


# filter out rare taxa (present in <5% of samples) -----------------------

##change Jan 8 2026: removed lower thresholds for taxa frequency and individuals per station counts. Will reconsider these later on.

# # LB; 3/7/26: Exploring taxonomic groupings and sample size thresholds by taxon and net
# sample_sizes_original_taxa <- mocness_full_geographic_isiis_mixing_fluor %>%
#   group_by(taxon) %>%
#   mutate(individuals_in_tow = as.integer(individuals_in_tow)) %>%
#   summarize(n = sum(individuals_in_tow, na.rm = TRUE))
# 
# sample_sizes_grouped_taxa <- mocness_clean %>%
#   group_by(taxon) %>%
#   mutate(individuals_in_tow = as.integer(individuals_in_tow)) %>%
#   summarize(n = sum(individuals_in_tow, na.rm = TRUE))

taxa_w_gt_15pct <- mocness_clean %>%
  filter(individuals_in_tow != "") %>%
  mutate(individuals_in_tow = as.numeric(individuals_in_tow)) %>%
  # LB; Here, individuals_in_tow is by taxon, so filtering like this only keeps observations with > 15 individuals
  # per taxon per tow, which seems very high. I think we want to filter by the total number of individuals per tow,
  # not individuals per tow per taxon, right?
  # filter(individuals_in_tow > 15) %>%
  group_by(taxon) %>%
  summarize(freq = n_distinct(transect_station_rep_year_net)) %>%
  ungroup() %>%
  filter(freq >= 0.05 * n_distinct(mocness_clean$transect_station_rep_year_net))
# RM; I decided to filter the taxa with a mix of strategies like Rodriguez did. I first thresholded by overall taxa count >15 
 # individuals, a value between those used by Huebert and Richardson (30 and 5, respectively). I then filtered by frequency of
 # occurrence like Doyle and Roussel with a threshold of those present in >= 5% of samples

mocness_major_taxa <- mocness_clean %>%
  filter(taxon %in% taxa_w_gt_15pct$taxon & taxon != "Unknown" & !is.na(taxon) & taxon != "Damaged" & 
           taxon != "" & taxon != "Fish eggs" & taxon != "Unknown spotted" & 
           taxon != "No fish" & taxon != "no fish" & !is.na(taxon) & taxon != "unknown" & taxon != "fish egg(s)")

# Get list of date/station/replicate with > 0 individuals of any "major" taxa
mocness_major_taxa_nets <- mocness_major_taxa %>%
  group_by(collection_date, start_time_pt, transect, station, replicate, net) %>%
  mutate(n = sum(individuals_in_tow, na.rm = TRUE)) %>%
  ungroup() %>%
  # The value to filter by here can be adjusted to threshold on the total number of individuals per tow 
  filter(n >= 15)
# RM; I was just thinking, do we want to use the net 0s since these aren't depth-stratified? If not, when should we remove them? Before
 # or after filtering?

sample_sizes_major_taxa_nets <- mocness_major_taxa_nets %>%
  group_by(taxon) %>%
  mutate(individuals_in_tow = as.integer(individuals_in_tow)) %>%
  summarize(n = sum(individuals_in_tow, na.rm = TRUE),
            proportion_tows_present = n_distinct(transect_station_rep_year_net) /
              n_distinct(mocness_major_taxa$transect_station_rep_year_net))

mocness_major_taxa_nets %>%
  distinct(collection_date, transect, station, replicate, net, .keep_all = TRUE) %>%
  pull(n) %>%
  hist(breaks = 253)
# RM ; what is this histogram for? how did you decide on the break level?
# LB; I wanted to be able to see how this histogram changes when different 
# thresholds for individuals per net are applied. I just chose a relatively
# arbitrary number of breaks that was high enough to see what's being 
# excluded when a threshold of 15 is used.


# Taxon colors for plots --------------------------------------------------
# # Categorize taxa by habitat affinity
# nearshore_species <- c()
# nearshore_colors <- colorRampPalette(brewer.pal(9, "Greens")[2:9])(length(nearshore_species))
# coastal_species <- c()
# coastal_colors <- colorRampPalette(brewer.pal(10, "Blues")[1:9])(length(coastal_species))
# oceanic_species <- c()
# oceanic_colors <- colorRampPalette(brewer.pal(5, "Purples")[2:6])(length(oceanic_species))
# # Named species color vector
# species_colors <- c(setNames(nearshore_colors, nearshore_species),
#                     setNames(coastal_colors, coastal_species),
#                     setNames(oceanic_colors, oceanic_species))
# # Vector of taxa ordered alphabetically within categories to order bars and figure legends
# ordered_taxa <- c(nearshore_species, coastal_species, oceanic_species)

# New taxa color scheme: mesopelagics, flatfishes, sculpin relatives, and others/junkdrawer
mesopelagic_species <- c("Bathylagus_ochotensis", "Lestidiops_ringens", "Protomyctophum_spp", 
                         "Stenobrachius_leucopsarus", "Tarletonbeania_crenularis")
mesopelagic_colors <- brewer.pal(length(mesopelagic_species) + 1, "Purples")[-1]

flatfish_species <- c("Glyptocephalus_zachirus", "Cyclopsettidae", "Isopsetta_isolepis", 
                      "Lyopsetta_exilis", "Parophrys_vetulus", "Psettichthys_melanostictus")
flatfish_colors <- brewer.pal(length(flatfish_species) + 1, "Oranges")[-1]

sculpin_relatives_species <- c("Cottidae", "Hemilepidotus_spp", "Psychrolutidae", 
                               "Scorpaenichthys_marmoratus", "Agonidae", "Liparis_spp",
                               "Hexagrammidae", "Sebastes_spp")
sculpin_relatives_colors <- brewer.pal(length(sculpin_relatives_species) + 1, "Blues")[-1]

other_species <- c("Ammodytidae", "Gadidae", "Osmeridae")
other_colors <- brewer.pal(length(other_species) + 1, "Greens")[-1]

species_colors <- c(setNames(mesopelagic_colors, mesopelagic_species),
                    setNames(flatfish_colors, flatfish_species),
                    setNames(sculpin_relatives_colors, sculpin_relatives_species),
                    setNames(other_colors, other_species))

ordered_taxa <- c(mesopelagic_species, flatfish_species, sculpin_relatives_species, other_species)

taxon_labels <- c(
  "Ammodytidae" = "Ammodytidae",
  "Cottidae" = "Cottidae",
  "Gadidae" = "Gadidae",
  "Glyptocephalus_zachirus" = "italic('Glyptocephalus zachirus')",
  "Hemilepidotus" = "italic('Hemilepidotus')~'spp.'",
  "Hexagrammidae" = "Hexagrammidae",
  "Psychrolutidae" = "Psychrolutidae",
  "Scorpaenichthys_marmoratus" = "italic('Scorpaenichthys marmoratus')",
  "Agonidae" = "Agonidae",
  "Cyclopsettidae" = "Cyclopsettidae",
  "Isopsetta_isolepis" = "italic('Isopsetta isolepis')",
  "Liparis_spp" =  "italic('Liparis')~'spp.'",
  "Lyopsetta_exilis" = "italic('Lyopsetta exilis')",
  "Osmeridae" ="Osmeridae",
  "Parophrys_vetulus" = "italic('Parophrys vetulus')",
  "Psettichthys_melanostictus" = "italic('Psettichthys melanostictus')",
  "Sebastes_spp" = "italic('Sebastes')~'spp.'",
  "Bathylagus_ochotensis" = "italic('Bathylagus ochotensis')",
  "Lestidiops_ringens" = "italic('Lestidiops ringens')",
  "Protomyctophum_spp" = "italic('Protomyctophum')~'spp.'",
  "Stenobrachius_leucopsarus" = "italic('Stenobrachius leucopsarus')",
  "Tarletonbeania_crenularis" = "italic('Tarletonbeania crenularis')")


# Create Data frame of excluded net tows ----------------------------------
excluded_tows <- mocness_major_taxa %>%
  group_by(collection_date, start_time_pt, transect, station, replicate, net) %>%
  mutate(n = sum(individuals_in_tow, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(n < 15) %>%
  distinct(transect_station_rep_year_net, start_longitude_dd, start_latitude_dd, transect, station, replicate, net, collection_date, start_time_pt)

