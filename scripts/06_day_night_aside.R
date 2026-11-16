# Description -------------------------------------------------------------

# Aside analysis of depth stratified total abundances across replicates for
# each taxa against solar dayness and depth


# Load packages -----------------------------------------------------------

library(here)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(glmmTMB)
library(DHARMa)


# Source code -------------------------------------------------------------

source(here("scripts/02_prepare_community_data.R"))


# Inventory matched day and night tows -------------------------------------

# Use the quantitative fish inventory before the community abundance cutoff.
# Exclude samples with a recorded spill, including partially lost samples.
lost_mocness_samples <- bind_rows(
  transmute(mocness_2018_02_fish_abundance, year = 2018, haul_number = Haul.no, notes),
  transmute(mocness_2019_03_fish_abundance, year = 2019, haul_number = Haul.no, notes)
) %>%
  filter(str_detect(str_to_lower(notes), "lost.*spill")) %>%
  distinct(year, haul_number)

day_night_inventory <- mocness_clean_by_side %>%
  filter(net %in% 1:4, mocness_side %in% c("1", "4"),
         !is.na(day_night), is.finite(volume_m3_best), volume_m3_best > 0,
         is.finite(minimum_depth_m), is.finite(maximum_depth_m),
         minimum_depth_m >= 0, maximum_depth_m > minimum_depth_m) %>%
  anti_join(lost_mocness_samples, by = c("year", "haul_number")) %>%
  # A recorded empty sample is a zero. A missing count otherwise remains missing.
  mutate(individuals_in_tow = if_else(str_to_lower(str_trim(taxon)) == "no fish",
                                      0, individuals_in_tow,
                                      missing = individuals_in_tow)) %>%
  group_by(year, haul_number) %>%
  filter(all(is.finite(individuals_in_tow) & individuals_in_tow >= 0)) %>%
  ungroup()

# Pool the available sides of each net tow and use their corresponding volumes.
# Assign 25 m strata from observed mean depth because actual fishing depths
# can differ from the depth range intended for a given net number.
day_night_valid_tows <- day_night_inventory %>%
  filter(year %in% c(2018, 2019)) %>%
  distinct(year, transect, station, replicate, net, collection_date,
           transect_station_rep_year_net, mocness_side, start_time_pt, day_night, solar_dayness,
           volume_m3_best, minimum_depth_m, maximum_depth_m) %>%
  group_by(year, transect, station, replicate, net, collection_date,
           transect_station_rep_year_net) %>%
  filter(n_distinct(day_night) == 1) %>%
  summarize(day_night = first(day_night),
            solar_dayness = mean(solar_dayness),
            start_time_pt = min(start_time_pt),
            mocness_sides = paste(sort(unique(mocness_side)), collapse = ", "),
            n_sides = n_distinct(mocness_side),
            volume_filtered_m3 = sum(volume_m3_best),
            depth_mean_m = mean((minimum_depth_m + maximum_depth_m) / 2),
            minimum_depth_m = min(minimum_depth_m),
            maximum_depth_m = max(maximum_depth_m),
            .groups = "drop") %>%
  mutate(depth_stratum = cut(depth_mean_m, breaks = seq(0, 100, 25), include.lowest = TRUE,
                             labels = c("0-25 m", "25-50 m", "50-75 m", "75-100 m"))) %>%
  drop_na(depth_stratum)

# Keep all tows in a block if the same station, year, and stratum has both periods.
matched_day_night_blocks <- day_night_valid_tows %>%
  group_by(year, transect, station, depth_stratum) %>%
  summarize(day_tows = sum(day_night == "Day"),
            night_tows = sum(day_night == "Night"), .groups = "drop") %>%
  filter(day_tows > 0, night_tows > 0)

matched_day_night_tows <- day_night_valid_tows %>%
  semi_join(matched_day_night_blocks, by = c("year", "transect", "station", "depth_stratum")) %>%
  arrange(year, transect, station, depth_stratum, start_time_pt)

# Summarize positive catches for the taxa in the community analysis. Complete
# taxon counts only within valid matched tows, where an unrecorded taxon is absent.
matched_day_night_counts <- day_night_inventory %>%
  semi_join(matched_day_night_tows, by = c("transect_station_rep_year_net", "collection_date")) %>%
  filter(taxon %in% mocness_major_taxa$taxon) %>%
  group_by(transect_station_rep_year_net, collection_date, taxon) %>%
  summarize(individuals_in_tow = sum(individuals_in_tow), .groups = "drop") %>%
  right_join(
    crossing(matched_day_night_tows, taxon = sort(unique(mocness_major_taxa$taxon))),
    by = c("transect_station_rep_year_net", "collection_date", "taxon")
  ) %>%
  replace_na(list(individuals_in_tow = 0)) %>%
  mutate(individuals_per_m3 = individuals_in_tow / volume_filtered_m3)

matched_day_night_taxon_summary <- matched_day_night_counts %>%
  group_by(taxon, day_night) %>%
  summarize(n_tows = n(),
            positive_tows = sum(individuals_in_tow > 0),
            total_individuals = sum(individuals_in_tow), .groups = "drop") %>%
  pivot_wider(names_from = day_night, values_from = c(n_tows, positive_tows, total_individuals))

write_csv(matched_day_night_blocks, here("output", "matched_day_night_blocks.csv"))
write_csv(matched_day_night_tows, here("output", "matched_day_night_tows.csv"))
write_csv(matched_day_night_taxon_summary, here("output", "matched_day_night_taxon_summary.csv"))

# Use the same taxa in the matched and full models, based on positive catches in matched 2019 tows.
glmm_retained_taxa <- matched_day_night_counts %>%
  filter(year == 2019, individuals_in_tow > 0) %>%
  distinct(taxon, transect_station_rep_year_net, collection_date) %>%
  count(taxon) %>%
  filter(n > 25) %>%
  pull(taxon)

# Prepare the matched model input before building the day/night comparisons.
# Center and scale predictors using the matched tows, once per tow.
glmm_solar_dayness_center <- mean(matched_day_night_tows$solar_dayness)
glmm_solar_dayness_scale <- sd(matched_day_night_tows$solar_dayness)
glmm_depth_mean_center <- mean(matched_day_night_tows$depth_mean_m)
glmm_depth_mean_scale <- sd(matched_day_night_tows$depth_mean_m)

glmm_model_data <- matched_day_night_counts %>%
  filter(year == 2019, taxon %in% glmm_retained_taxa) %>%
  mutate(taxon = factor(taxon),
         transect_station_rep_year_net = factor(transect_station_rep_year_net),
         solar_dayness_scaled = (solar_dayness - glmm_solar_dayness_center) / glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - glmm_depth_mean_center) / glmm_depth_mean_scale)

# Export the number of matched tows with at least one larva of each taxon.
matched_day_night_taxon_summary %>%
  select(taxon, positive_tows_Day, positive_tows_Night) %>%
  write_csv(here("output", "matched_day_night_positive_tows_by_taxon.csv"))


# Compare total larval abundance between day and night ---------------------

# Use the same matched 2019 tows, taxa, and time of day as the models.
comparison_tows <- glmm_model_data %>%
  distinct(transect_station_rep_year_net, collection_date, day_night, solar_dayness)

# Use the catches and volumes that went into the pooled model input, keeping
# the sides separate for the panels. Sums over absent taxa give zero catches.
day_night_tow_abundance <- day_night_inventory %>%
  select(-day_night, -solar_dayness) %>%
  inner_join(comparison_tows, by = c("transect_station_rep_year_net", "collection_date"),
             relationship = "many-to-one") %>%
  group_by(transect_station_rep_year_net, collection_date, mocness_side, day_night, solar_dayness) %>%
  summarize(volume_m3_best = first(volume_m3_best),
            total_individuals = sum(individuals_in_tow[taxon %in% glmm_retained_taxa]),
            .groups = "drop") %>%
  # Divide catches by the volume filtered on the corresponding MOCNESS side.
  mutate(total_concentration = total_individuals / volume_m3_best,
         day_night = factor(day_night, levels = c("Night", "Day")),
         mocness_side = factor(mocness_side, levels = c("1", "4")))

# Draw each tow once, including the observations beyond the boxplot whiskers.
# Square-root transform concentrations while keeping zero catches at zero.
day_night_tow_plot <- ggplot(day_night_tow_abundance, aes(x = day_night, y = sqrt(total_concentration))) +
  geom_boxplot(width = 0.5, outlier.shape = NA, fill = "grey95") +
  geom_point(position = position_jitter(width = 0.15, height = 0, seed = 1),
             size = 1.2, alpha = 0.45) +
  facet_wrap(~ mocness_side, nrow = 1, drop = FALSE,
             labeller = labeller(mocness_side = as_labeller(
               c("1" = "'1-'*m^2~mouth", "4" = "'4-'*m^2~mouth"), default = label_parsed))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = NULL, y = expression(sqrt("Concentration (ind. m"^{-3}*")"))) +
  theme_classic(base_size = 12)

ggsave("day_night_total_larval_abundance_by_mocness_side.png", plot = day_night_tow_plot,
       path = here("output"), width = 7, height = 4.5, units = "in", dpi = 300)

# Fit a spline to the conditional median at each solar dayness within each side.
day_night_tow_scatter <- ggplot(day_night_tow_abundance,
                               aes(x = solar_dayness, y = sqrt(total_concentration))) +
  geom_point(size = 1.2, alpha = 0.45) +
  geom_quantile(quantiles = 0.5, formula = y ~ splines::ns(x, df = 3),
                color = "black") +
  facet_wrap(~ mocness_side, nrow = 1, drop = FALSE,
             labeller = labeller(mocness_side = as_labeller(
               c("1" = "'1-'*m^2~mouth", "4" = "'4-'*m^2~mouth"), default = label_parsed))) +
  labs(x = "Time of day", y = expression(sqrt("Concentration (ind. m"^{-3}*")"))) +
  theme_classic(base_size = 12)

ggsave("day_night_total_larval_abundance_scatter_by_mocness_side.png", plot = day_night_tow_scatter,
       path = here("output"), width = 7, height = 4.5, units = "in", dpi = 300)


# Prepare data ------------------------------------------------------------

# Pool the usable sides of each depth-stratified tow before the 10-larva cutoff.
# Calculate tow metadata once per side so counts do not weight the covariate means.
full_tow_covariates <- c("start_latitude_dd", "seafloor_depth_m", "mean_temperature_c",
                         "mean_salinity_psu", "dissolved_oxygen_ml_l", "mean_chl_0_100_m_mgm3")
full_glmm_tows <- day_night_inventory %>%
  distinct(transect_station_rep_year_net, collection_date, year, mocness_side,
           solar_dayness, volume_m3_best, minimum_depth_m, maximum_depth_m,
           across(all_of(full_tow_covariates))) %>%
  group_by(transect_station_rep_year_net, collection_date, year) %>%
  summarize(solar_dayness = mean(solar_dayness),
            volume_filtered_m3 = sum(volume_m3_best),
            depth_mean_m = mean((minimum_depth_m + maximum_depth_m) / 2),
            minimum_depth_m = min(minimum_depth_m),
            maximum_depth_m = max(maximum_depth_m),
            across(all_of(full_tow_covariates), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(depth_diff_m = maximum_depth_m - minimum_depth_m,
         across(c(solar_dayness, depth_mean_m, all_of(full_tow_covariates)),
                ~ as.numeric(scale(.x)), .names = "{.col}_scaled")) %>%
  unite("depth_range", minimum_depth_m, maximum_depth_m, sep = "-", remove = FALSE)

# Complete the taxon counts from the tow inventory, including tows where
# none of the retained taxa was caught. Use the same sides for counts and volume.
full_glmm_tow_counts <- day_night_inventory %>%
  filter(taxon %in% glmm_retained_taxa) %>%
  group_by(transect_station_rep_year_net, collection_date, taxon) %>%
  summarize(individuals_in_tow = sum(individuals_in_tow), .groups = "drop")

ordered_taxa_dvm <- intersect(ordered_taxa, glmm_retained_taxa)
model_data <- crossing(full_glmm_tows, taxon = ordered_taxa_dvm) %>%
  left_join(full_glmm_tow_counts, by = c("transect_station_rep_year_net", "collection_date", "taxon"),
            relationship = "many-to-one") %>%
  replace_na(list(individuals_in_tow = 0)) %>%
  mutate(year = factor(year),
         taxon = factor(taxon, levels = ordered_taxa_dvm),
         individuals_per_m3 = individuals_in_tow / volume_filtered_m3)


# Avg taxa concentrations across replicates ------------------------------------

avgd_mocness_major_taxa_dvm <- model_data %>%
  group_by(taxon, solar_dayness, depth_range, depth_mean_m, depth_diff_m) %>%
  summarise(avg_taxa_concentration = mean(individuals_per_m3, na.rm=TRUE)) %>%
  ungroup()


# Plot taxa concentrations by depths and solar dayness --------------------
# Barplot
ggplot(avgd_mocness_major_taxa_dvm, aes(x = depth_range, y = avg_taxa_concentration, fill = taxon)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = species_colors) +
  labs(title = "Taxa concentrations at depth ranges",
       x = "Depth sampled (m)", y = "average individuals per m3") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot positive catches on the log scale.
d_n_scatter_all <- ggplot(filter(model_data, individuals_in_tow > 0), aes(x = depth_mean_m, y = log(individuals_per_m3), color = taxon)) +
  geom_point() +
  geom_smooth(method = "lm",se = FALSE) +
  scale_color_manual(values = species_colors, breaks = ordered_taxa_dvm,
                     labels = parse(text = taxon_labels[ordered_taxa_dvm])) +
  labs(title = "Taxa concentrations by mean depths",
       x = "Mean tow depth (m)", y = "Concentration (log(ind./m3)", color = "Taxon") +
  theme_classic()
ggsave("dayness_scatter_all.png", plot = d_n_scatter_all, path = here("output"),
         width = 7, height = 5, units = "in", dpi = 300)

taxa_panel_order <- c(
  "Bathylagus_ochotensis",
  "Tarletonbeania_crenularis",
  "Protomyctophum_spp",
  "Cyclopsettidae",
  "Glyptocephalus_zachirus",
  "Stenobrachius_leucopsarus",
  "Liparis_spp",
  "Parophrys_vetulus",
  "Sebastes_spp"
)
# Count positive tows in each model's input and preserve the taxon formatting.
positive_tow_labeller <- function(model_data) {
  positive_counts <- model_data %>%
    distinct(taxon, transect_station_rep_year_net, collection_date,
             positive_catch = individuals_in_tow > 0) %>%
    group_by(taxon) %>%
    summarize(n_positive = sum(positive_catch, na.rm = TRUE), .groups = "drop") %>%
    mutate(label = paste0(taxon_labels[as.character(taxon)], "~'(n = ", n_positive, ")'"))

  labeller(taxon = as_labeller(setNames(positive_counts$label, positive_counts$taxon),
                               default = label_parsed))
}

# Divide predictions and points by the same predicted peak within each taxon.
plot_relative_abundance_surface <- function(surface_predictions, observed_tows, taxon_labeller) {
  predicted_peaks <- surface_predictions %>%
    group_by(taxon) %>%
    summarize(peak_fit = max(fit, na.rm = TRUE), .groups = "drop")

  surface_predictions <- surface_predictions %>%
    left_join(predicted_peaks, by = "taxon") %>%
    mutate(normalized_fit = fit / peak_fit)

  # Back-transform observed log concentrations or log-scale partial residuals.
  # Subtract the log peak before exponentiating to get the linear ratio.
  observed_tows <- observed_tows %>%
    mutate(taxon = factor(taxon, levels = taxa_panel_order)) %>%
    left_join(predicted_peaks, by = "taxon") %>%
    mutate(normalized_observed = exp(log_observed - log(peak_fit))) %>%
    arrange(normalized_observed)

  ggplot(surface_predictions, aes(x = solar_dayness, y = depth_mean_m)) +
    geom_raster(aes(fill = normalized_fit), interpolate = TRUE) +
    geom_contour(aes(z = normalized_fit), color = "white", linewidth = 0.2, alpha = 0.6) +
    geom_point(data = observed_tows, aes(fill = normalized_observed),
               shape = 21, color = "white", size = 0.5, stroke = 0.35 / 3) +
    scale_y_reverse() +
    # Show points above the predicted peak at the top of the color scale.
    scale_fill_viridis_c(option = "magma", limits = c(0, 1),
                         oob = scales::oob_squish_any,
                         breaks = c(0, 0.25, 0.5, 0.75, 1),
                         labels = c("0", "0.25", "0.50", "0.75", "1+")) +
    facet_wrap(~ taxon, labeller = taxon_labeller,
               axes = "all", axis.labels = "margins") +
    labs(x = "Time of day",
         y = "Mean tow depth (m)",
         fill = "Relative concentration") +
    theme_classic()
}


# Subtract the predicted log peak within each taxon. This gives the log of
# concentration relative to the predicted peak. Use the same reference for
# the points, which can be observed log concentrations or partial residuals.
plot_log_relative_concentration <- function(surface_predictions, observed_tows, taxon_labeller,
                                            point_color_cap = Inf, filled_zero_catches = FALSE) {
  predicted_peaks <- surface_predictions %>%
    group_by(taxon) %>%
    summarize(peak_log_fit = max(log_fit, na.rm = TRUE), .groups = "drop")

  surface_predictions <- surface_predictions %>%
    left_join(predicted_peaks, by = "taxon") %>%
    mutate(log_relative_fit = log_fit - peak_log_fit)

  observed_tows <- observed_tows %>%
    mutate(taxon = factor(taxon, levels = taxa_panel_order)) %>%
    left_join(predicted_peaks, by = "taxon") %>%
    mutate(log_relative_observed = log_observed - peak_log_fit,
           point_fill = pmin(log_relative_observed, point_color_cap),
           zero_catch = individuals_in_tow == 0) %>%
    arrange(log_relative_observed)

  # Report the number of points whose colors were capped for the figure caption.
  if (is.finite(point_color_cap)) {
    n_capped <- sum(observed_tows$log_relative_observed > point_color_cap, na.rm = TRUE)
    message(n_capped, " of ", nrow(observed_tows), " point colors capped at ", point_color_cap, ".")
  }

  color_limits <- range(surface_predictions$log_relative_fit,
                        observed_tows$point_fill, finite = TRUE)

  if (filled_zero_catches) {
    zero_tows <- filter(observed_tows, zero_catch)
    # Shape 13 has no fill aesthetic. Back it with a filled circle on the
    # same color scale, then draw the white circled X over that fill.
    point_layers <- list(
      geom_point(data = filter(observed_tows, !zero_catch), aes(fill = point_fill),
                 shape = 21, colour = "white", size = 0.5, stroke = 0.35 / 3,
                 show.legend = FALSE),
      geom_point(data = zero_tows, aes(fill = point_fill, colour = point_fill),
                 shape = 21, size = 0.5, stroke = 0.35 / 3, show.legend = FALSE),
      geom_point(data = zero_tows, shape = 13, colour = "white", size = 0.5,
                 stroke = 0.35 / 3, show.legend = FALSE)
    )
  } else {
    observed_tows <- observed_tows %>%
      mutate(cross_color = if_else(zero_catch & is.finite(point_fill), point_fill, NA_real_))
    point_layers <- list(
      geom_point(data = observed_tows, aes(fill = point_fill, colour = cross_color, shape = zero_catch),
                 size = 0.5, stroke = 0.35 / 3, show.legend = FALSE),
      scale_shape_manual(values = c("FALSE" = 21, "TRUE" = 4))
    )
  }

  ggplot(surface_predictions, aes(x = solar_dayness, y = depth_mean_m)) +
    geom_raster(aes(fill = log_relative_fit), interpolate = TRUE) +
    geom_contour(aes(z = log_relative_fit), color = "white", linewidth = 0.2, alpha = 0.6) +
    point_layers +
    scale_y_reverse() +
    scale_fill_viridis_c(name = "log Relative concentration", option = "magma",
                         aesthetics = c("fill", "colour"), limits = color_limits,
                         na.value = "white") +
    facet_wrap(~ taxon, labeller = taxon_labeller, axes = "all", axis.labels = "margins") +
    labs(x = "Time of day", y = "Mean tow depth (m)") +
    theme_classic()
}


# Mark the solar dayness of the sampled tows. Darker ticks show positive catches.
glmm_sampling_rug <- function(observed_tows) {
  rug_data <- observed_tows %>%
    transmute(taxon = factor(taxon, levels = taxa_panel_order), solar_dayness,
              positive_catch = individuals_in_tow > 0) %>%
    arrange(positive_catch)

  list(
    geom_rug(data = rug_data, aes(x = solar_dayness, alpha = positive_catch),
             inherit.aes = FALSE, sides = "b", linewidth = 0.25, show.legend = FALSE),
    scale_alpha_manual(values = c("FALSE" = 0.18, "TRUE" = 0.8))
  )
}


# GLMM for the matched day and night tows ----------------------------------

glmm_dayness_grid <- seq(min(glmm_model_data$solar_dayness, na.rm = TRUE),
                        max(glmm_model_data$solar_dayness, na.rm = TRUE),
                        length.out = 100)
glmm_depth_grid <- seq(min(glmm_model_data$depth_mean_m, na.rm = TRUE),
                      max(glmm_model_data$depth_mean_m, na.rm = TRUE),
                      length.out = 100)

glmm_min_depth <- min(glmm_model_data$depth_mean_m, na.rm = TRUE)
glmm_max_depth <- max(glmm_model_data$depth_mean_m, na.rm = TRUE)

glmm_integration_depth_grid <- seq(glmm_min_depth, glmm_max_depth, length.out = 80)
glmm_integration_depth_step <- diff(range(glmm_integration_depth_grid)) /
  (length(glmm_integration_depth_grid) - 1)

glmm_integration_depth_weights <- tibble(depth_mean_m = glmm_integration_depth_grid,
                                        depth_weight = glmm_integration_depth_step) %>%
  mutate(depth_weight = if_else(row_number() %in% c(1, n()),
                                depth_weight / 2,
                                depth_weight))

glmm_taxa <- levels(glmm_model_data$taxon)
glmm_taxon_labeller <- positive_tow_labeller(glmm_model_data)

# The matched tows come from a single year. Use the filtered volume
# corresponding to the usable sides of each tow. Estimate dispersion by taxon.
glmm_dayness_depth_model <- glmmTMB(
  individuals_in_tow ~ taxon * solar_dayness_scaled * depth_mean_m_scaled +
    offset(log(volume_filtered_m3)) +
    (1 | transect_station_rep_year_net),
  family = nbinom2,
  dispformula = ~ taxon,
  data = glmm_model_data
)

summary(glmm_dayness_depth_model)

# Draw simulation-based residual diagnostics in the active RStudio device.
glmm_matched_residuals <- simulateResiduals(glmm_dayness_depth_model, n = 1000)
plot(glmm_matched_residuals, title = "Matched 2019 GLMM")

# Check residuals against the predictors in the taxon-specific interaction.
glmm_matched_diagnostic_data <- model.frame(glmm_dayness_depth_model)
plotResiduals(glmm_matched_residuals, form = glmm_matched_diagnostic_data$solar_dayness_scaled,
              xlab = "Solar dayness", main = "Matched 2019 GLMM")
plotResiduals(glmm_matched_residuals, form = glmm_matched_diagnostic_data$depth_mean_m_scaled,
              xlab = "Mean tow depth", main = "Matched 2019 GLMM")
plotResiduals(glmm_matched_residuals, form = glmm_matched_diagnostic_data$taxon,
              xlab = "Taxon", main = "Matched 2019 GLMM")
print(testZeroInflation(glmm_matched_residuals, plot = TRUE))

# Check concentration in the deepest matched stratum using the same tows and
# filtered volumes for the observed catches and each DHARMa simulation.
stopifnot(
  nrow(glmm_model_data) == nrow(glmm_matched_residuals$simulatedResponse),
  isTRUE(all.equal(glmm_model_data$individuals_in_tow,
                   glmm_matched_residuals$observedResponse, check.attributes = FALSE))
)

glmm_deep_diagnostics <- glmm_model_data %>%
  mutate(model_row = row_number()) %>%
  filter(depth_stratum == "75-100 m") %>%
  group_by(taxon) %>%
  group_modify(~ {
    night <- filter(.x, day_night == "Night")
    day <- filter(.x, day_night == "Day")
    night_volume <- sum(night$volume_filtered_m3)
    day_volume <- sum(day$volume_filtered_m3)
    stopifnot(night_volume > 0, day_volume > 0)

    observed_night <- sum(glmm_matched_residuals$observedResponse[night$model_row]) / night_volume
    observed_day <- sum(glmm_matched_residuals$observedResponse[day$model_row]) / day_volume
    simulated_night <- colSums(
      glmm_matched_residuals$simulatedResponse[night$model_row, , drop = FALSE]
    ) / night_volume
    simulated_day <- colSums(
      glmm_matched_residuals$simulatedResponse[day$model_row, , drop = FALSE]
    ) / day_volume

    # Subtract the observed statistic from each simulation. Calculate the
    # night-minus-day contrast within simulations before taking quantiles.
    night_error <- simulated_night - observed_night
    day_error <- simulated_day - observed_day
    simulation_error <- rbind(Night = night_error, Day = day_error,
                              `Night minus day` = night_error - day_error)

    tibble(comparison = rownames(simulation_error),
           lower = apply(simulation_error, 1, quantile, probs = 0.025),
           median = apply(simulation_error, 1, median),
           upper = apply(simulation_error, 1, quantile, probs = 0.975))
  }) %>%
  ungroup() %>%
  mutate(taxon = factor(taxon, levels = rev(taxa_panel_order)),
         comparison = factor(comparison, levels = c("Night", "Day", "Night minus day")))

# A symmetric pseudo-log scale keeps values near zero visible while showing
# the full simulation intervals for taxa with highly variable catches.
glmm_deep_diagnostic_plot <- ggplot(glmm_deep_diagnostics, aes(median, taxon)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.2, orientation = "y") +
  geom_point(size = 2, color = "#0072B2") +
  facet_wrap(~ comparison, nrow = 1) +
  scale_x_continuous(trans = scales::pseudo_log_trans(sigma = 0.002),
                     breaks = c(-0.1, -0.01, 0, 0.01, 0.1)) +
  scale_y_discrete(labels = function(x) parse(text = unname(taxon_labels[x]))) +
  labs(title = "Matched 2019 GLMM at 75–100 m",
       subtitle = "Points are simulation medians and bars are 95% simulation intervals",
       x = expression("Simulated minus observed concentration (ind. " * m^{-3} * ")"),
       y = NULL) +
  theme_classic()

print(glmm_deep_diagnostic_plot)

# The fixed-effect matrix uses a unit volume and zero random effects.
glmm_fixed_terms <- terms(~ taxon * solar_dayness_scaled * depth_mean_m_scaled)
glmm_coef <- fixef(glmm_dayness_depth_model)$cond
glmm_vcov <- vcov(glmm_dayness_depth_model)$cond


glmm_observed_tows <- glmm_model_data %>%
  mutate(log_observed = log(individuals_per_m3),
         taxon = factor(taxon, levels = taxa_panel_order))

# GLMM predicted abundance surfaces ---------------------------------------

glmm_surface_grid <- expand_grid(
  taxon = factor(glmm_taxa, levels = glmm_taxa),
  solar_dayness = glmm_dayness_grid,
  depth_mean_m = glmm_depth_grid
) %>%
  mutate(solar_dayness_scaled = (solar_dayness - glmm_solar_dayness_center) / glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - glmm_depth_mean_center) / glmm_depth_mean_scale)

glmm_surface_model_matrix <- model.matrix(glmm_fixed_terms, glmm_surface_grid)
glmm_surface_model_matrix <- glmm_surface_model_matrix[, names(glmm_coef), drop = FALSE]

glmm_surface_predictions <- glmm_surface_grid %>%
  mutate(link_fit = as.numeric(glmm_surface_model_matrix %*% glmm_coef),
         link_se = sqrt(rowSums((glmm_surface_model_matrix %*% glmm_vcov) *
                                  glmm_surface_model_matrix)),
         fit = exp(link_fit),
         lwr = exp(link_fit - 1.96 * link_se),
         upr = exp(link_fit + 1.96 * link_se)) %>%
  mutate(log_fit = log(fit),
         taxon = factor(as.character(taxon), levels = taxa_panel_order))

glmm_surface_plot <- plot_log_relative_concentration(
  glmm_surface_predictions,
  glmm_observed_tows,
  glmm_taxon_labeller
)

ggsave(here("output/glmm_matched_predicted_abundance_surfaces_all_taxa.png"),
       plot = glmm_surface_plot,
       width = 12,
       height = 8,
       dpi = 600)

glmm_surface_normalized_plot <- plot_relative_abundance_surface(
  glmm_surface_predictions, glmm_observed_tows, glmm_taxon_labeller
)

ggsave(here("output/glmm_matched_predicted_abundance_surfaces_normalized_all_taxa.png"),
       plot = glmm_surface_normalized_plot,
       width = 12,
       height = 8,
       dpi = 600)


# GLMM depth-slope function across solar dayness --------------------------

glmm_slope_predictions <- expand_grid(
  taxon = factor(glmm_taxa, levels = glmm_taxa),
  solar_dayness = glmm_dayness_grid
)

glmm_shallow_predictions <- glmm_slope_predictions %>%
  mutate(depth_mean_m = glmm_min_depth,
         solar_dayness_scaled = (solar_dayness - glmm_solar_dayness_center) / glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - glmm_depth_mean_center) / glmm_depth_mean_scale)

glmm_deep_predictions <- glmm_slope_predictions %>%
  mutate(depth_mean_m = glmm_max_depth,
         solar_dayness_scaled = (solar_dayness - glmm_solar_dayness_center) / glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - glmm_depth_mean_center) / glmm_depth_mean_scale)

glmm_shallow_model_matrix <- model.matrix(glmm_fixed_terms, glmm_shallow_predictions)
glmm_deep_model_matrix <- model.matrix(glmm_fixed_terms, glmm_deep_predictions)
glmm_shallow_model_matrix <- glmm_shallow_model_matrix[, names(glmm_coef), drop = FALSE]
glmm_deep_model_matrix <- glmm_deep_model_matrix[, names(glmm_coef), drop = FALSE]

glmm_slope_model_matrix <- (glmm_deep_model_matrix - glmm_shallow_model_matrix) /
  (glmm_max_depth - glmm_min_depth)

glmm_slope_predictions <- glmm_slope_predictions %>%
  mutate(fit = as.numeric(glmm_slope_model_matrix %*% glmm_coef),
         se = sqrt(rowSums((glmm_slope_model_matrix %*% glmm_vcov) *
                             glmm_slope_model_matrix)),
         lwr = fit - 1.96 * se,
         upr = fit + 1.96 * se,
         taxon = factor(as.character(taxon), levels = taxa_panel_order))

glmm_slope_plot <- ggplot(glmm_slope_predictions,
                          aes(x = solar_dayness, y = fit, ymin = lwr, ymax = upr)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3, linetype = "11") +
  geom_ribbon(alpha = 0.25, fill = "grey60") +
  geom_line(linewidth = 0.5) +
  glmm_sampling_rug(glmm_model_data) +
  facet_wrap(~ taxon, scales = "free_y", labeller = glmm_taxon_labeller,
             axes = "all", axis.labels = "all_y") +
  labs(x = "Time of day",
       y = expression(paste("Slope of effect of depth on abundance (", m^{-1}, ")"))) +
  theme_classic()

ggsave(here("output/glmm_matched_depth_slope_functions_all_taxa.png"),
       plot = glmm_slope_plot,
       width = 12,
       height = 8,
       dpi = 300)


# GLMM integrated predicted abundance across depth ------------------------
# Integrate on the response scale, then take logs. Apply the delta method
# to the log total to get finite pointwise confidence bounds.

glmm_integrated_predictions <- expand_grid(
  taxon = factor(glmm_taxa, levels = glmm_taxa),
  solar_dayness = glmm_dayness_grid,
  depth_mean_m = glmm_integration_depth_grid
) %>%
  left_join(glmm_integration_depth_weights, by = "depth_mean_m") %>%
  mutate(solar_dayness_scaled = (solar_dayness - glmm_solar_dayness_center) / glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - glmm_depth_mean_center) / glmm_depth_mean_scale)

glmm_integrated_summary <- tibble()

for (glmm_taxon in glmm_taxa) {
  glmm_integrated_taxon_predictions <- glmm_integrated_predictions %>%
    filter(taxon == glmm_taxon)
  
  glmm_integrated_model_matrix <- model.matrix(glmm_fixed_terms,
                                               glmm_integrated_taxon_predictions)
  glmm_integrated_model_matrix <- glmm_integrated_model_matrix[, names(glmm_coef), drop = FALSE]
  
  glmm_integrated_taxon_predictions <- glmm_integrated_taxon_predictions %>%
    mutate(point_fit = exp(as.numeric(glmm_integrated_model_matrix %*% glmm_coef)),
           integration_group = sprintf("%.8f", solar_dayness))
  
  glmm_integrated_fit <- rowsum(glmm_integrated_taxon_predictions$point_fit *
                                  glmm_integrated_taxon_predictions$depth_weight,
                                glmm_integrated_taxon_predictions$integration_group,
                                reorder = FALSE)
  
  glmm_integrated_gradient <- rowsum(glmm_integrated_model_matrix *
                                       (glmm_integrated_taxon_predictions$point_fit *
                                          glmm_integrated_taxon_predictions$depth_weight),
                                     glmm_integrated_taxon_predictions$integration_group,
                                     reorder = FALSE)
  
  glmm_integrated_se <- sqrt(rowSums((glmm_integrated_gradient %*% glmm_vcov) *
                                       glmm_integrated_gradient))
  
  glmm_integrated_taxon_summary <- glmm_integrated_taxon_predictions %>%
    distinct(integration_group, taxon, solar_dayness) %>%
    arrange(solar_dayness) %>%
    mutate(fit = as.numeric(glmm_integrated_fit),
           log_fit = log(fit),
           log_se = glmm_integrated_se / fit,
           lwr = log_fit - 1.96 * log_se,
           upr = log_fit + 1.96 * log_se)
  
  glmm_integrated_summary <- bind_rows(glmm_integrated_summary,
                                       glmm_integrated_taxon_summary)
}

glmm_integrated_summary <- glmm_integrated_summary %>%
  mutate(taxon = factor(as.character(taxon), levels = taxa_panel_order))

glmm_integrated_plot <- ggplot(glmm_integrated_summary,
                               aes(x = solar_dayness, y = log_fit, ymin = lwr, ymax = upr)) +
  geom_ribbon(alpha = 0.25, fill = "grey60") +
  geom_line(linewidth = 0.5) +
  glmm_sampling_rug(glmm_model_data) +
  facet_wrap(~ taxon, scales = "free_y", labeller = glmm_taxon_labeller,
             axes = "all", axis.labels = "all_y") +
  labs(x = "Time of day",
       y = expression("log Predicted abundance (ind." ~ m^{-2} * ")")) +
  theme_classic()

ggsave(here("output/glmm_matched_integrated_abundance_functions_all_taxa.png"),
       plot = glmm_integrated_plot,
       width = 12,
       height = 8,
       dpi = 300)


# GLMM for the full data set -----------------------------------------------

full_glmm_model_data <- model_data %>%
  drop_na(year, solar_dayness, solar_dayness_scaled, depth_mean_m, depth_mean_m_scaled,
          start_latitude_dd_scaled, seafloor_depth_m_scaled, mean_temperature_c_scaled,
          mean_salinity_psu_scaled, dissolved_oxygen_ml_l_scaled,
          mean_chl_0_100_m_mgm3_scaled, volume_filtered_m3) %>%
  mutate(taxon = droplevels(taxon),
         year = droplevels(year),
         transect_station_rep_year_net = factor(transect_station_rep_year_net))

full_glmm_taxa <- levels(full_glmm_model_data$taxon)
full_glmm_taxon_labeller <- positive_tow_labeller(full_glmm_model_data)
full_glmm_years <- levels(full_glmm_model_data$year)
full_glmm_solar_dayness_center <- mean(full_glmm_tows$solar_dayness, na.rm = TRUE)
full_glmm_solar_dayness_scale <- sd(full_glmm_tows$solar_dayness, na.rm = TRUE)
full_glmm_depth_mean_center <- mean(full_glmm_tows$depth_mean_m, na.rm = TRUE)
full_glmm_depth_mean_scale <- sd(full_glmm_tows$depth_mean_m, na.rm = TRUE)
full_glmm_dayness_grid <- seq(min(full_glmm_model_data$solar_dayness, na.rm = TRUE),
                        max(full_glmm_model_data$solar_dayness, na.rm = TRUE),
                        length.out = 100)
full_glmm_depth_grid <- seq(min(full_glmm_model_data$depth_mean_m, na.rm = TRUE),
                      max(full_glmm_model_data$depth_mean_m, na.rm = TRUE),
                      length.out = 100)

full_glmm_min_depth <- min(full_glmm_model_data$depth_mean_m, na.rm = TRUE)
full_glmm_max_depth <- max(full_glmm_model_data$depth_mean_m, na.rm = TRUE)

full_glmm_integration_depth_grid <- seq(full_glmm_min_depth, full_glmm_max_depth, length.out = 80)
full_glmm_integration_depth_step <- diff(range(full_glmm_integration_depth_grid)) /
  (length(full_glmm_integration_depth_grid) - 1)

full_glmm_integration_depth_weights <- tibble(depth_mean_m = full_glmm_integration_depth_grid,
                                        depth_weight = full_glmm_integration_depth_step) %>%
  mutate(depth_weight = if_else(row_number() %in% c(1, n()),
                                depth_weight / 2,
                                depth_weight))


# Retain the full set of mean-model covariates and estimate dispersion by taxon.
full_glmm_dayness_depth_model <- glmmTMB(
  individuals_in_tow ~ taxon * solar_dayness_scaled * depth_mean_m_scaled +
    year +
    start_latitude_dd_scaled + seafloor_depth_m_scaled +
    mean_temperature_c_scaled + mean_salinity_psu_scaled +
    dissolved_oxygen_ml_l_scaled + mean_chl_0_100_m_mgm3_scaled +
    offset(log(volume_filtered_m3)) +
    (1 | transect_station_rep_year_net),
  family = nbinom2,
  dispformula = ~ taxon,
  data = full_glmm_model_data
)

summary(full_glmm_dayness_depth_model)

# Draw simulation-based residual diagnostics in the active RStudio device.
glmm_full_residuals <- simulateResiduals(full_glmm_dayness_depth_model, n = 1000)
plot(glmm_full_residuals, title = "Full-data GLMM")

# Check residuals against the predictors in the taxon-specific interaction.
glmm_full_diagnostic_data <- model.frame(full_glmm_dayness_depth_model)
plotResiduals(glmm_full_residuals, form = glmm_full_diagnostic_data$solar_dayness_scaled,
              xlab = "Solar dayness", main = "Full-data GLMM")
plotResiduals(glmm_full_residuals, form = glmm_full_diagnostic_data$depth_mean_m_scaled,
              xlab = "Mean tow depth", main = "Full-data GLMM")
plotResiduals(glmm_full_residuals, form = glmm_full_diagnostic_data$taxon,
              xlab = "Taxon", main = "Full-data GLMM")
print(testZeroInflation(glmm_full_residuals, plot = TRUE))

full_glmm_fixed_terms <- terms(~ taxon * solar_dayness_scaled * depth_mean_m_scaled +
                            year +
                            start_latitude_dd_scaled + seafloor_depth_m_scaled +
                            mean_temperature_c_scaled + mean_salinity_psu_scaled +
                            dissolved_oxygen_ml_l_scaled + mean_chl_0_100_m_mgm3_scaled)
full_glmm_coef <- fixef(full_glmm_dayness_depth_model)$cond
full_glmm_vcov <- vcov(full_glmm_dayness_depth_model)$cond


# Adjust each observed tow to the same reference as the surface: one cubic
# meter, centered environmental covariates, zero tow effect, and equal year
# weights. Keep its original taxon, depth, and solar dayness.
full_glmm_observation_grid <- expand_grid(
  full_glmm_model_data %>%
    transmute(observation = row_number(), taxon, solar_dayness_scaled, depth_mean_m_scaled),
  year = factor(full_glmm_years, levels = full_glmm_years)
) %>%
  mutate(start_latitude_dd_scaled = 0, seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0, mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0, mean_chl_0_100_m_mgm3_scaled = 0)
full_glmm_observation_matrix <- model.matrix(full_glmm_fixed_terms, full_glmm_observation_grid)
full_glmm_observation_matrix <- full_glmm_observation_matrix[, names(full_glmm_coef), drop = FALSE]

full_glmm_reference_fit <- full_glmm_observation_grid %>%
  mutate(fit = exp(as.numeric(full_glmm_observation_matrix %*% full_glmm_coef))) %>%
  group_by(observation) %>%
  summarize(reference_log_fit = log(mean(fit)), .groups = "drop")

# Add conditional working residuals to the reference log concentration.
# For this log-link model, the working residual is (catch - fitted) / fitted,
# where the fitted catch includes the original year, covariates, and tow effect.
full_glmm_observed_tows <- full_glmm_model_data %>%
  mutate(observation = row_number(),
         working_residual = residuals(full_glmm_dayness_depth_model, type = "working")) %>%
  left_join(full_glmm_reference_fit, by = "observation") %>%
  mutate(log_observed = reference_log_fit + working_residual,
         taxon = factor(taxon, levels = taxa_panel_order))


# GLMM predicted abundance surfaces ---------------------------------------

full_glmm_surface_grid <- expand_grid(
  taxon = factor(full_glmm_taxa, levels = full_glmm_taxa),
  year = factor(full_glmm_years, levels = full_glmm_years),
  solar_dayness = full_glmm_dayness_grid,
  depth_mean_m = full_glmm_depth_grid
) %>%
  mutate(solar_dayness_scaled = (solar_dayness - full_glmm_solar_dayness_center) / full_glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - full_glmm_depth_mean_center) / full_glmm_depth_mean_scale,
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0)

full_glmm_surface_model_matrix <- model.matrix(full_glmm_fixed_terms, full_glmm_surface_grid)
full_glmm_surface_model_matrix <- full_glmm_surface_model_matrix[, names(full_glmm_coef), drop = FALSE]

full_glmm_surface_predictions <- full_glmm_surface_grid %>%
  mutate(link_fit = as.numeric(full_glmm_surface_model_matrix %*% full_glmm_coef),
         link_se = sqrt(rowSums((full_glmm_surface_model_matrix %*% full_glmm_vcov) *
                                  full_glmm_surface_model_matrix)),
         fit = exp(link_fit),
         lwr = exp(link_fit - 1.96 * link_se),
         upr = exp(link_fit + 1.96 * link_se)) %>%
  group_by(taxon, solar_dayness, depth_mean_m) %>%
  summarise(fit = mean(fit, na.rm = TRUE),
            lwr = mean(lwr, na.rm = TRUE),
            upr = mean(upr, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(log_fit = log(fit),
         taxon = factor(as.character(taxon), levels = taxa_panel_order))

full_glmm_surface_plot <- plot_log_relative_concentration(
  full_glmm_surface_predictions,
  full_glmm_observed_tows,
  full_glmm_taxon_labeller,
  point_color_cap = 5,
  filled_zero_catches = TRUE
)

ggsave(here("output/glmm_full_data_predicted_abundance_surfaces_all_taxa.png"),
       plot = full_glmm_surface_plot,
       width = 12,
       height = 8,
       dpi = 600)

full_glmm_surface_normalized_plot <- plot_relative_abundance_surface(
  full_glmm_surface_predictions, full_glmm_observed_tows, full_glmm_taxon_labeller
)

ggsave(here("output/glmm_full_data_predicted_abundance_surfaces_normalized_all_taxa.png"),
       plot = full_glmm_surface_normalized_plot,
       width = 12,
       height = 8,
       dpi = 600)


# GLMM depth-slope function across solar dayness --------------------------

full_glmm_slope_predictions <- expand_grid(
  taxon = factor(full_glmm_taxa, levels = full_glmm_taxa),
  solar_dayness = full_glmm_dayness_grid
)

full_glmm_shallow_predictions <- full_glmm_slope_predictions %>%
  mutate(depth_mean_m = full_glmm_min_depth,
         solar_dayness_scaled = (solar_dayness - full_glmm_solar_dayness_center) / full_glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - full_glmm_depth_mean_center) / full_glmm_depth_mean_scale,
         year = factor(first(full_glmm_years), levels = full_glmm_years),
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0)

full_glmm_deep_predictions <- full_glmm_slope_predictions %>%
  mutate(depth_mean_m = full_glmm_max_depth,
         solar_dayness_scaled = (solar_dayness - full_glmm_solar_dayness_center) / full_glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - full_glmm_depth_mean_center) / full_glmm_depth_mean_scale,
         year = factor(first(full_glmm_years), levels = full_glmm_years),
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0)

full_glmm_shallow_model_matrix <- model.matrix(full_glmm_fixed_terms, full_glmm_shallow_predictions)
full_glmm_deep_model_matrix <- model.matrix(full_glmm_fixed_terms, full_glmm_deep_predictions)
full_glmm_shallow_model_matrix <- full_glmm_shallow_model_matrix[, names(full_glmm_coef), drop = FALSE]
full_glmm_deep_model_matrix <- full_glmm_deep_model_matrix[, names(full_glmm_coef), drop = FALSE]

full_glmm_slope_model_matrix <- (full_glmm_deep_model_matrix - full_glmm_shallow_model_matrix) /
  (full_glmm_max_depth - full_glmm_min_depth)

full_glmm_slope_predictions <- full_glmm_slope_predictions %>%
  mutate(fit = as.numeric(full_glmm_slope_model_matrix %*% full_glmm_coef),
         se = sqrt(rowSums((full_glmm_slope_model_matrix %*% full_glmm_vcov) *
                             full_glmm_slope_model_matrix)),
         lwr = fit - 1.96 * se,
         upr = fit + 1.96 * se,
         taxon = factor(as.character(taxon), levels = taxa_panel_order))

full_glmm_slope_plot <- ggplot(full_glmm_slope_predictions,
                          aes(x = solar_dayness, y = fit, ymin = lwr, ymax = upr)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.3, linetype = "11") +
  geom_ribbon(alpha = 0.25, fill = "grey60") +
  geom_line(linewidth = 0.5) +
  glmm_sampling_rug(full_glmm_model_data) +
  facet_wrap(~ taxon, scales = "free_y", labeller = full_glmm_taxon_labeller,
             axes = "all", axis.labels = "all_y") +
  labs(x = "Time of day",
       y = expression(paste("Slope of effect of depth on abundance (", m^{-1}, ")"))) +
  theme_classic()

ggsave(here("output/glmm_full_data_depth_slope_functions_all_taxa.png"),
       plot = full_glmm_slope_plot,
       width = 12,
       height = 8,
       dpi = 300)


# GLMM integrated predicted abundance across depth ------------------------
# Integrate on the response scale, then take logs. Apply the delta method
# to the log total to get finite pointwise confidence bounds.

full_glmm_integrated_predictions <- expand_grid(
  taxon = factor(full_glmm_taxa, levels = full_glmm_taxa),
  year = factor(full_glmm_years, levels = full_glmm_years),
  solar_dayness = full_glmm_dayness_grid,
  depth_mean_m = full_glmm_integration_depth_grid
) %>%
  left_join(full_glmm_integration_depth_weights, by = "depth_mean_m") %>%
  mutate(solar_dayness_scaled = (solar_dayness - full_glmm_solar_dayness_center) / full_glmm_solar_dayness_scale,
         depth_mean_m_scaled = (depth_mean_m - full_glmm_depth_mean_center) / full_glmm_depth_mean_scale,
         start_latitude_dd_scaled = 0,
         seafloor_depth_m_scaled = 0,
         mean_temperature_c_scaled = 0,
         mean_salinity_psu_scaled = 0,
         dissolved_oxygen_ml_l_scaled = 0,
         mean_chl_0_100_m_mgm3_scaled = 0,
         depth_weight = depth_weight / length(full_glmm_years))

full_glmm_integrated_summary <- tibble()

for (full_glmm_taxon in full_glmm_taxa) {
  full_glmm_integrated_taxon_predictions <- full_glmm_integrated_predictions %>%
    filter(taxon == full_glmm_taxon)
  
  full_glmm_integrated_model_matrix <- model.matrix(full_glmm_fixed_terms,
                                               full_glmm_integrated_taxon_predictions)
  full_glmm_integrated_model_matrix <- full_glmm_integrated_model_matrix[, names(full_glmm_coef), drop = FALSE]
  
  full_glmm_integrated_taxon_predictions <- full_glmm_integrated_taxon_predictions %>%
    mutate(point_fit = exp(as.numeric(full_glmm_integrated_model_matrix %*% full_glmm_coef)),
           integration_group = sprintf("%.8f", solar_dayness))
  
  full_glmm_integrated_fit <- rowsum(full_glmm_integrated_taxon_predictions$point_fit *
                                  full_glmm_integrated_taxon_predictions$depth_weight,
                                full_glmm_integrated_taxon_predictions$integration_group,
                                reorder = FALSE)
  
  full_glmm_integrated_gradient <- rowsum(full_glmm_integrated_model_matrix *
                                       (full_glmm_integrated_taxon_predictions$point_fit *
                                          full_glmm_integrated_taxon_predictions$depth_weight),
                                     full_glmm_integrated_taxon_predictions$integration_group,
                                     reorder = FALSE)
  
  full_glmm_integrated_se <- sqrt(rowSums((full_glmm_integrated_gradient %*% full_glmm_vcov) *
                                       full_glmm_integrated_gradient))
  
  full_glmm_integrated_taxon_summary <- full_glmm_integrated_taxon_predictions %>%
    distinct(integration_group, taxon, solar_dayness) %>%
    arrange(solar_dayness) %>%
    mutate(fit = as.numeric(full_glmm_integrated_fit),
           log_fit = log(fit),
           log_se = full_glmm_integrated_se / fit,
           lwr = log_fit - 1.96 * log_se,
           upr = log_fit + 1.96 * log_se)
  
  full_glmm_integrated_summary <- bind_rows(full_glmm_integrated_summary,
                                       full_glmm_integrated_taxon_summary)
}

full_glmm_integrated_summary <- full_glmm_integrated_summary %>%
  mutate(taxon = factor(as.character(taxon), levels = taxa_panel_order))

full_glmm_integrated_plot <- ggplot(full_glmm_integrated_summary,
                               aes(x = solar_dayness, y = log_fit, ymin = lwr, ymax = upr)) +
  geom_ribbon(alpha = 0.25, fill = "grey60") +
  geom_line(linewidth = 0.5) +
  glmm_sampling_rug(full_glmm_model_data) +
  facet_wrap(~ taxon, scales = "free_y", labeller = full_glmm_taxon_labeller,
             axes = "all", axis.labels = "all_y") +
  labs(x = "Time of day",
       y = expression("log Predicted abundance (ind." ~ m^{-2} * ")")) +
  theme_classic()

ggsave(here("output/glmm_full_data_integrated_abundance_functions_all_taxa.png"),
       plot = full_glmm_integrated_plot,
       width = 12,
       height = 8,
       dpi = 300)
