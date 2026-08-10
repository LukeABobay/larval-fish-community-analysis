# Description -------------------------------------------------------------

# Calculate NEP10k mixed layer depth and isothermal layer depth at MOCNESS
# larval fish collection dates and locations.
#
# This script is meant to be run on Novus from the project root. It reads the
# monthly NEP10k files in /home/bobayl/dissertation/dissertation-data/mom6_data,
# extracts the nearest NEP10k grid-cell temperature and salinity profiles for
# each winter MOCNESS tow, and writes a tow-level csv to data/.


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ncdf4)


# Define variables --------------------------------------------------------

nep10k_dir <- "/home/bobayl/dissertation/dissertation-data/mom6_data"
metadata_csv <- file.path("data", "mocness_metadata.csv")
output_csv <- file.path("data", "nep10k_mld_ild_covariates.csv")

temperature_var_candidates <- c("thetao", "temp", "tob", "temperature")
salinity_var_candidates <- c("so", "sal", "salt", "salinity")
time_var_candidates <- c("time", "ocean_time", "time_counter")

lon_var <- "lon"
lat_var <- "lat"

reference_depth_m <- 5
density_threshold_kgm3 <- 0.03
temperature_threshold_c <- 0.2
earth_radius_m <- 6371000


# Calculate density from temperature and salinity -------------------------

calculate_density_unesco_0db <- function(salinity, temperature) {
  rho_w <- 999.842594 +
    6.793952e-2 * temperature -
    9.095290e-3 * temperature^2 +
    1.001685e-4 * temperature^3 -
    1.120083e-6 * temperature^4 +
    6.536332e-9 * temperature^5

  a <- 0.824493 -
    4.0899e-3 * temperature +
    7.6438e-5 * temperature^2 -
    8.2467e-7 * temperature^3 +
    5.3875e-9 * temperature^4

  b <- -5.72466e-3 +
    1.0227e-4 * temperature -
    1.6546e-6 * temperature^2

  c <- 4.8314e-4

  rho_w + a * salinity + b * salinity^1.5 + c * salinity^2
}


# Get larval fish collection locations -----------------------------------

if (!file.exists(metadata_csv)) {
  stop("Could not find ", metadata_csv, ". Run this script from the project root.")
}

mocness_tows <- read_csv(metadata_csv, show_col_types = FALSE) %>%
  mutate(
    start_time_pt = ymd_hms(as.character(start_time_pt), tz = "America/Los_Angeles"),
    collection_date = as_date(start_time_pt),
    year = year(collection_date),
    month = month(collection_date)
  ) %>%
  filter(
    cruise %in% c("W18", "W19", "W22", "W23"),
    is.finite(start_longitude_dd),
    is.finite(start_latitude_dd)
  ) %>%
  distinct(
    cruise, transect, replicate, station, mocness_side, net,
    collection_date, start_time_pt, start_longitude_dd, start_latitude_dd,
    minimum_depth_m, maximum_depth_m, year, month
  ) %>%
  arrange(collection_date, start_time_pt, transect, station, replicate, net, mocness_side)

if (nrow(mocness_tows) == 0) {
  stop("No winter MOCNESS tows were found in ", metadata_csv)
}

message("Calculating NEP10k MLD/ILD for ", nrow(mocness_tows), " MOCNESS tow rows.")


# Extract NEP10k profiles and calculate covariates ------------------------

monthly_tasks <- mocness_tows %>%
  distinct(year, month) %>%
  arrange(year, month)

output_list <- list()
output_idx <- 1L

for (task_i in seq_len(nrow(monthly_tasks))) {
  target_year <- monthly_tasks$year[[task_i]]
  target_month <- monthly_tasks$month[[task_i]]
  year_month_label <- sprintf("%04d-%02d", target_year, target_month)

  message("Reading NEP10k files for ", year_month_label)

  month_file_pattern <- sprintf("NEP10k_raw_.*_%04d\\.%02d\\.nc$", target_year, target_month)
  candidate_files <- list.files(
    nep10k_dir,
    pattern = month_file_pattern,
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(candidate_files) == 0) {
    stop("No NEP10k files were found for ", year_month_label)
  }

  temperature_file <- NA_character_
  temperature_var <- NA_character_

  for (var_candidate in temperature_var_candidates) {
    for (file_path in candidate_files) {
      nc_test <- tryCatch(ncdf4::nc_open(file_path), error = function(e) NULL)
      if (is.null(nc_test)) next

      has_var <- var_candidate %in% names(nc_test$var)
      ncdf4::nc_close(nc_test)

      if (has_var) {
        temperature_file <- file_path
        temperature_var <- var_candidate
        break
      }
    }

    if (!is.na(temperature_file)) break
  }

  salinity_file <- NA_character_
  salinity_var <- NA_character_

  for (var_candidate in salinity_var_candidates) {
    for (file_path in candidate_files) {
      nc_test <- tryCatch(ncdf4::nc_open(file_path), error = function(e) NULL)
      if (is.null(nc_test)) next

      has_var <- var_candidate %in% names(nc_test$var)
      ncdf4::nc_close(nc_test)

      if (has_var) {
        salinity_file <- file_path
        salinity_var <- var_candidate
        break
      }
    }

    if (!is.na(salinity_file)) break
  }

  if (is.na(temperature_file)) {
    stop("No NEP10k temperature file was found for ", year_month_label)
  }

  if (is.na(salinity_file)) {
    stop("No NEP10k salinity file was found for ", year_month_label)
  }

  nc_temperature <- ncdf4::nc_open(temperature_file)
  nc_salinity <- ncdf4::nc_open(salinity_file)

  tryCatch(
    {
      temperature_time_name <- time_var_candidates[
        time_var_candidates %in% c(names(nc_temperature$var), names(nc_temperature$dim))
      ][1]
      salinity_time_name <- time_var_candidates[
        time_var_candidates %in% c(names(nc_salinity$var), names(nc_salinity$dim))
      ][1]

      if (is.na(temperature_time_name) || is.na(salinity_time_name)) {
        stop("Could not find a time variable in the NEP10k files for ", year_month_label)
      }

      if (temperature_time_name %in% names(nc_temperature$var)) {
        temperature_time_vals <- ncdf4::ncvar_get(nc_temperature, temperature_time_name)
        temperature_time_units <- tryCatch(
          ncdf4::ncatt_get(nc_temperature, temperature_time_name, "units")$value,
          error = function(e) ""
        )
      } else {
        temperature_time_vals <- nc_temperature$dim[[temperature_time_name]]$vals
        temperature_time_units <- nc_temperature$dim[[temperature_time_name]]$units
      }

      if (salinity_time_name %in% names(nc_salinity$var)) {
        salinity_time_vals <- ncdf4::ncvar_get(nc_salinity, salinity_time_name)
        salinity_time_units <- tryCatch(
          ncdf4::ncatt_get(nc_salinity, salinity_time_name, "units")$value,
          error = function(e) ""
        )
      } else {
        salinity_time_vals <- nc_salinity$dim[[salinity_time_name]]$vals
        salinity_time_units <- nc_salinity$dim[[salinity_time_name]]$units
      }

      if (is.null(temperature_time_units) || is.na(temperature_time_units)) {
        temperature_time_units <- ""
      }

      if (is.null(salinity_time_units) || is.na(salinity_time_units)) {
        salinity_time_units <- ""
      }

      if (grepl("since", temperature_time_units, ignore.case = TRUE)) {
        temperature_time_origin <- as.POSIXct(sub("^.*since\\s*", "", temperature_time_units), tz = "UTC")
        temperature_time_multiplier <- case_when(
          grepl("day", temperature_time_units, ignore.case = TRUE) ~ 86400,
          grepl("hour", temperature_time_units, ignore.case = TRUE) ~ 3600,
          TRUE ~ 1
        )
        temperature_dates <- as.Date(temperature_time_origin + temperature_time_vals * temperature_time_multiplier)
      } else {
        temperature_dates <- as.Date(temperature_time_vals, origin = "1970-01-01")
      }

      if (grepl("since", salinity_time_units, ignore.case = TRUE)) {
        salinity_time_origin <- as.POSIXct(sub("^.*since\\s*", "", salinity_time_units), tz = "UTC")
        salinity_time_multiplier <- case_when(
          grepl("day", salinity_time_units, ignore.case = TRUE) ~ 86400,
          grepl("hour", salinity_time_units, ignore.case = TRUE) ~ 3600,
          TRUE ~ 1
        )
        salinity_dates <- as.Date(salinity_time_origin + salinity_time_vals * salinity_time_multiplier)
      } else {
        salinity_dates <- as.Date(salinity_time_vals, origin = "1970-01-01")
      }

      lon_mat <- t(ncdf4::ncvar_get(nc_salinity, lon_var))
      lat_mat <- t(ncdf4::ncvar_get(nc_salinity, lat_var))

      if (max(lon_mat, na.rm = TRUE) > 180) {
        lon_mat <- ifelse(lon_mat > 180, lon_mat - 360, lon_mat)
      }

      if (!all(dim(lon_mat) == dim(lat_mat))) {
        stop("NEP10k lon/lat dimensions do not match for ", year_month_label)
      }

      temperature_dims <- nc_temperature$var[[temperature_var]]$dim
      salinity_dims <- nc_salinity$var[[salinity_var]]$dim

      temperature_dim_names <- vapply(temperature_dims, function(d) d$name, "")
      salinity_dim_names <- vapply(salinity_dims, function(d) d$name, "")

      temperature_dim_lengths <- vapply(temperature_dims, function(d) d$len, integer(1))
      salinity_dim_lengths <- vapply(salinity_dims, function(d) d$len, integer(1))

      temperature_time_pos <- grep("time", temperature_dim_names, ignore.case = TRUE)[1]
      temperature_j_pos <- grep("^jh$", temperature_dim_names, ignore.case = TRUE)[1]
      temperature_i_pos <- grep("^ih$", temperature_dim_names, ignore.case = TRUE)[1]
      temperature_z_pos <- grep("depth|lev|z_l|zl|z|s_rho|s_w", temperature_dim_names, ignore.case = TRUE)[1]

      salinity_time_pos <- grep("time", salinity_dim_names, ignore.case = TRUE)[1]
      salinity_j_pos <- grep("^jh$", salinity_dim_names, ignore.case = TRUE)[1]
      salinity_i_pos <- grep("^ih$", salinity_dim_names, ignore.case = TRUE)[1]
      salinity_z_pos <- grep("depth|lev|z_l|zl|z|s_rho|s_w", salinity_dim_names, ignore.case = TRUE)[1]

      if (any(is.na(c(
        temperature_time_pos, temperature_j_pos, temperature_i_pos, temperature_z_pos,
        salinity_time_pos, salinity_j_pos, salinity_i_pos, salinity_z_pos
      )))) {
        stop("Could not identify all time/depth/jh/ih dimensions for ", year_month_label)
      }

      temperature_depth_dim <- temperature_dims[[temperature_z_pos]]
      salinity_depth_dim <- salinity_dims[[salinity_z_pos]]

      if (length(temperature_depth_dim$vals) == temperature_depth_dim$len &&
          any(is.finite(temperature_depth_dim$vals))) {
        temperature_depths <- abs(as.numeric(temperature_depth_dim$vals))
      } else {
        temperature_depths <- abs(as.numeric(ncdf4::ncvar_get(nc_temperature, temperature_depth_dim$name)))
      }

      if (length(salinity_depth_dim$vals) == salinity_depth_dim$len &&
          any(is.finite(salinity_depth_dim$vals))) {
        salinity_depths <- abs(as.numeric(salinity_depth_dim$vals))
      } else {
        salinity_depths <- abs(as.numeric(ncdf4::ncvar_get(nc_salinity, salinity_depth_dim$name)))
      }

      if (length(temperature_depths) != length(salinity_depths) ||
          any(abs(temperature_depths - salinity_depths) > 1e-8)) {
        stop("Temperature and salinity depth grids differ for ", year_month_label)
      }

      month_tows <- mocness_tows %>%
        filter(year == target_year, month == target_month)

      nearest_cells <- map2_dfr(
        month_tows$start_longitude_dd,
        month_tows$start_latitude_dd,
        function(longitude_dd, latitude_dd) {
          rad <- pi / 180
          dlon <- (lon_mat - longitude_dd) * rad
          dlat <- (lat_mat - latitude_dd) * rad
          a <- sin(dlat / 2)^2 +
            cos(latitude_dd * rad) * cos(lat_mat * rad) * sin(dlon / 2)^2
          a[a < 0] <- 0
          distance_m <- 2 * earth_radius_m * asin(pmin(1, sqrt(a)))
          nearest_idx <- which.min(distance_m)

          tibble(
            nep10k_j_idx = arrayInd(nearest_idx, dim(lon_mat))[, 1],
            nep10k_i_idx = arrayInd(nearest_idx, dim(lon_mat))[, 2],
            nep10k_longitude_dd = lon_mat[nearest_idx],
            nep10k_latitude_dd = lat_mat[nearest_idx],
            nep10k_distance_km = distance_m[nearest_idx] / 1000
          )
        }
      )

      month_tows <- bind_cols(month_tows, nearest_cells)

      for (tow_i in seq_len(nrow(month_tows))) {
        this_date <- month_tows$collection_date[[tow_i]]
        temperature_time_idx <- match(this_date, temperature_dates)
        salinity_time_idx <- match(this_date, salinity_dates)

        if (is.na(temperature_time_idx) || is.na(salinity_time_idx)) {
          stop("No NEP10k time index found for ", this_date)
        }

        temperature_start <- rep(1L, length(temperature_dim_lengths))
        temperature_count <- temperature_dim_lengths
        temperature_start[temperature_time_pos] <- as.integer(temperature_time_idx)
        temperature_count[temperature_time_pos] <- 1L
        temperature_start[temperature_j_pos] <- as.integer(month_tows$nep10k_j_idx[[tow_i]])
        temperature_count[temperature_j_pos] <- 1L
        temperature_start[temperature_i_pos] <- as.integer(month_tows$nep10k_i_idx[[tow_i]])
        temperature_count[temperature_i_pos] <- 1L

        salinity_start <- rep(1L, length(salinity_dim_lengths))
        salinity_count <- salinity_dim_lengths
        salinity_start[salinity_time_pos] <- as.integer(salinity_time_idx)
        salinity_count[salinity_time_pos] <- 1L
        salinity_start[salinity_j_pos] <- as.integer(month_tows$nep10k_j_idx[[tow_i]])
        salinity_count[salinity_j_pos] <- 1L
        salinity_start[salinity_i_pos] <- as.integer(month_tows$nep10k_i_idx[[tow_i]])
        salinity_count[salinity_i_pos] <- 1L

        temperature_profile <- as.numeric(ncdf4::ncvar_get(
          nc_temperature,
          temperature_var,
          start = temperature_start,
          count = temperature_count
        ))

        salinity_profile <- as.numeric(ncdf4::ncvar_get(
          nc_salinity,
          salinity_var,
          start = salinity_start,
          count = salinity_count
        ))

        profile <- tibble(
          depth_m = temperature_depths,
          temperature_c = temperature_profile,
          salinity_psu = salinity_profile
        ) %>%
          mutate(density_kgm3 = calculate_density_unesco_0db(salinity_psu, temperature_c)) %>%
          filter(
            is.finite(depth_m),
            is.finite(temperature_c),
            is.finite(salinity_psu),
            is.finite(density_kgm3)
          ) %>%
          arrange(depth_m)

        mld_density_m <- NA_real_
        mld_density_censor_flag <- 2
        ild_temperature_m <- NA_real_
        ild_temperature_censor_flag <- 2
        reference_density_kgm3 <- NA_real_
        reference_temperature_c <- NA_real_

        if (nrow(profile) >= 2) {
          reference_profile <- profile %>%
            filter(depth_m <= reference_depth_m)

          if (nrow(reference_profile) > 0) {
            reference_density_kgm3 <- median(reference_profile$density_kgm3)
            reference_temperature_c <- median(reference_profile$temperature_c)

            density_difference <- profile$density_kgm3 - reference_density_kgm3
            density_crossing_idx <- which(density_difference >= density_threshold_kgm3)[1]

            if (is.na(density_crossing_idx)) {
              mld_density_m <- max(profile$depth_m)
              mld_density_censor_flag <- 1
            } else if (density_crossing_idx == 1) {
              mld_density_m <- profile$depth_m[[density_crossing_idx]]
              mld_density_censor_flag <- 0
            } else {
              previous_difference <- density_difference[[density_crossing_idx - 1]]
              current_difference <- density_difference[[density_crossing_idx]]
              previous_depth <- profile$depth_m[[density_crossing_idx - 1]]
              current_depth <- profile$depth_m[[density_crossing_idx]]
              interpolation_fraction <- (density_threshold_kgm3 - previous_difference) /
                (current_difference - previous_difference)
              interpolation_fraction <- pmin(1, pmax(0, interpolation_fraction))
              mld_density_m <- previous_depth + interpolation_fraction * (current_depth - previous_depth)
              mld_density_censor_flag <- 0
            }

            temperature_difference <- abs(profile$temperature_c - reference_temperature_c)
            temperature_crossing_idx <- which(temperature_difference >= temperature_threshold_c)[1]

            if (is.na(temperature_crossing_idx)) {
              ild_temperature_m <- max(profile$depth_m)
              ild_temperature_censor_flag <- 1
            } else if (temperature_crossing_idx == 1) {
              ild_temperature_m <- profile$depth_m[[temperature_crossing_idx]]
              ild_temperature_censor_flag <- 0
            } else {
              previous_difference <- temperature_difference[[temperature_crossing_idx - 1]]
              current_difference <- temperature_difference[[temperature_crossing_idx]]
              previous_depth <- profile$depth_m[[temperature_crossing_idx - 1]]
              current_depth <- profile$depth_m[[temperature_crossing_idx]]
              interpolation_fraction <- (temperature_threshold_c - previous_difference) /
                (current_difference - previous_difference)
              interpolation_fraction <- pmin(1, pmax(0, interpolation_fraction))
              ild_temperature_m <- previous_depth + interpolation_fraction * (current_depth - previous_depth)
              ild_temperature_censor_flag <- 0
            }
          }
        }

        if (is.finite(mld_density_m) && is.finite(ild_temperature_m)) {
          barrier_layer_thickness_m <- pmax(0, ild_temperature_m - mld_density_m)
        } else {
          barrier_layer_thickness_m <- NA_real_
        }

        if (nrow(profile) > 0) {
          surface_temperature_nep10k_c <- profile$temperature_c[[1]]
          surface_salinity_nep10k_psu <- profile$salinity_psu[[1]]
          surface_density_nep10k_kgm3 <- profile$density_kgm3[[1]]
          n_profile_layers <- nrow(profile)
          deepest_profile_depth_m <- max(profile$depth_m)
        } else {
          surface_temperature_nep10k_c <- NA_real_
          surface_salinity_nep10k_psu <- NA_real_
          surface_density_nep10k_kgm3 <- NA_real_
          n_profile_layers <- 0L
          deepest_profile_depth_m <- NA_real_
        }

        output_list[[output_idx]] <- month_tows[tow_i, ] %>%
          mutate(
            mld_density_0_03_m = mld_density_m,
            mld_density_censor_flag = mld_density_censor_flag,
            ild_temperature_0_2c_m = ild_temperature_m,
            ild_temperature_censor_flag = ild_temperature_censor_flag,
            barrier_layer_thickness_m = barrier_layer_thickness_m,
            reference_depth_m = reference_depth_m,
            reference_density_kgm3 = reference_density_kgm3,
            reference_temperature_c = reference_temperature_c,
            surface_temperature_nep10k_c = surface_temperature_nep10k_c,
            surface_salinity_nep10k_psu = surface_salinity_nep10k_psu,
            surface_density_nep10k_kgm3 = surface_density_nep10k_kgm3,
            n_profile_layers = n_profile_layers,
            deepest_profile_depth_m = deepest_profile_depth_m,
            temperature_nep10k_file = basename(temperature_file),
            salinity_nep10k_file = basename(salinity_file)
          )

        output_idx <- output_idx + 1L
      }
    },
    finally = {
      ncdf4::nc_close(nc_temperature)
      ncdf4::nc_close(nc_salinity)
    }
  )
}

nep10k_mld_ild <- bind_rows(output_list) %>%
  arrange(collection_date, start_time_pt, transect, station, replicate, net, mocness_side)

write_csv(nep10k_mld_ild, output_csv)

message("Wrote NEP10k MLD/ILD covariates to ", output_csv)
